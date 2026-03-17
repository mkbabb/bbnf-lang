//! Pass: Type inference for IR rules.
//!
//! Walks each rule's body and infers a `TypeDesc` describing the Rust/TS type
//! that the rule produces. Populates `GrammarIR::types` with `(RuleId, TypeDesc)`
//! pairs consumed by codegen backends.
//!
//! Also collects sub-variants for heterogeneous alternations and stores them
//! in `RuleMeta::sub_variants`.

use std::collections::HashMap;

use crate::{FnDescriptor, GrammarIR, IrNode, RuleId, SubVariant, TypeDesc};

/// Infer types for all rules and populate `ir.types`.
///
/// Rules are processed in topological order (the order they appear in `ir.rules`),
/// so a rule's dependencies are always inferred before the rule itself. Cyclic
/// rules are assigned `BoxedEnum` for any back-references.
///
/// Implements 5 fixes over the baseline:
/// - B.1: sp_method_rules Span override in Seq (all-Span guard)
/// - B.2: @pretty/@no_collapse consumable flag for tuple preservation
/// - B.3: Custom mapping return type from closure annotation
/// - B.4: Cyclic→acyclic type override (BoxedEnum for non-inlined acyclic refs in cyclic context)
/// - B.5: Sub-variant collection for heterogeneous alternations
pub fn infer_types(ir: &mut GrammarIR) {
    let mut cache: HashMap<RuleId, TypeDesc> = HashMap::new();

    // Collect metadata before inference (avoids borrow issues).
    let rule_meta: HashMap<RuleId, (bool, bool, bool)> = ir
        .rules
        .iter()
        .map(|r| {
            (
                r.id,
                (
                    r.meta.no_collapse,
                    r.meta.is_cyclic,
                    r.meta.span_eligible,
                ),
            )
        })
        .collect();

    // Collect which rules have @pretty or @no_collapse (for B.2).
    let pretty_preserve_rules: HashMap<RuleId, bool> = ir
        .rules
        .iter()
        .map(|r| {
            let has_pretty = r.meta.pretty.is_some();
            let has_no_collapse = r.meta.no_collapse;
            (r.id, has_pretty || has_no_collapse)
        })
        .collect();

    // Identify acyclic rules (for B.4 override).
    let acyclic_rules: std::collections::HashSet<RuleId> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();

    // Process rules in order (topological after pipeline passes).
    let rules_snapshot: Vec<(RuleId, IrNode, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.body.clone(), r.meta.is_transparent))
        .collect();

    for (id, body, _is_transparent) in &rules_snapshot {
        let (no_collapse, is_cyclic, _span_eligible) =
            rule_meta.get(id).copied().unwrap_or_default();

        // B.4: For cyclic rules, override acyclic Ref types to BoxedEnum.
        // This matches AST codegen's behavior where acyclic deps in cyclic context
        // get boxed_enum_type to enable (Vec<A>, A) → Vec<A> flattening.
        let cyclic_context = is_cyclic;

        // B.2: Set pretty_preserve for the top-level call only.
        let pretty_preserve = pretty_preserve_rules.get(id).copied().unwrap_or(false);

        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context,
            no_collapse,
            pretty_preserve,
        };

        let ty = infer_node(body, &ctx);
        cache.insert(*id, ty);
    }

    // B.5: Collect sub-variants for heterogeneous alternations.
    // Two-pass: first collect raw (String name, TypeDesc, branch_index), then intern names.
    let rule_names: HashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<RawSubVariant>> = HashMap::new();

    for (id, body, is_transparent) in &rules_snapshot {
        if *is_transparent {
            continue;
        }
        let (no_collapse, is_cyclic, _) = rule_meta.get(id).copied().unwrap_or_default();
        let ctx = InferCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            no_collapse,
            pretty_preserve: false,
        };
        let rule_name = rule_names.get(id).unwrap();
        let svs = collect_sub_variants_raw(rule_name, body, &ctx);
        if !svs.is_empty() {
            raw_sub_variants.insert(*id, svs);
        }
    }

    // Validate cross-rule sub-variant uniqueness (using raw names).
    validate_sub_variant_uniqueness_raw(&raw_sub_variants, &rule_names);

    // Intern sub-variant names and write to rules.
    for rule in &mut ir.rules {
        if let Some(raw_svs) = raw_sub_variants.remove(&rule.id) {
            rule.meta.sub_variants = raw_svs
                .into_iter()
                .map(|rsv| {
                    // Intern the variant name into the string table.
                    let name_id = if let Some(pos) =
                        ir.strings.iter().position(|s| s == &rsv.variant_name)
                    {
                        pos as u32
                    } else {
                        let id = ir.strings.len() as u32;
                        ir.strings.push(rsv.variant_name);
                        id
                    };
                    SubVariant {
                        variant_name: name_id,
                        ty: rsv.ty,
                        branch_index: rsv.branch_index,
                    }
                })
                .collect();
        }
    }

    ir.types = cache.into_iter().collect();
    ir.types.sort_by_key(|(id, _)| *id);

}

/// Post-inference pass: convert `Vec<BoxedEnum>` → `Vec<Enum>` in inferred types.
///
/// Context for type inference — avoids threading many parameters.
struct InferCtx<'a> {
    ir: &'a GrammarIR,
    cache: &'a HashMap<RuleId, TypeDesc>,
    acyclic_rules: &'a std::collections::HashSet<RuleId>,
    /// Whether the current rule being inferred is cyclic (for B.4).
    cyclic_context: bool,
    /// Whether @no_collapse is set for the current rule.
    no_collapse: bool,
    /// Consumable flag for @pretty/@no_collapse tuple preservation (B.2).
    /// Only applies to the first (top-level) Seq encountered.
    pretty_preserve: bool,
}

impl InferCtx<'_> {
    /// Return a copy with pretty_preserve consumed (set to false).
    fn consumed(&self) -> InferCtx<'_> {
        InferCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            no_collapse: self.no_collapse,
            pretty_preserve: false,
        }
    }
}

/// Infer the output type of a single IR node.
fn infer_node(node: &IrNode, ctx: &InferCtx<'_>) -> TypeDesc {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) => TypeDesc::Span,

        IrNode::Epsilon => TypeDesc::Tuple(vec![]),

        IrNode::Ref(_id) => {
            // BoxedEnum: emit_ref wraps non-transparent calls with Box::new.
            // Transparent refs also return Box<Enum>.
            // The insert_recursion_boxing post-pass converts Vec<BoxedEnum>
            // → Vec<Enum> where Vec provides sufficient heap indirection.
            TypeDesc::BoxedEnum
        }

        IrNode::Seq(children) => {
            // B.1 + B.2: Seq inference with sp_method_rules override and pretty_preserve.
            infer_seq(children, ctx)
        }

        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let consumed = ctx.consumed();
            let first = infer_node(&branches[0].node, &consumed);
            let all_same = branches[1..]
                .iter()
                .all(|b| infer_node(&b.node, &consumed) == first);
            if all_same {
                first
            } else {
                TypeDesc::BoxedEnum
            }
        }

        IrNode::Repeat { inner, lo, hi } => {
            // Nested: consume pretty_preserve.
            let consumed = ctx.consumed();

            if *lo == 0 && *hi == 1 {
                // Optional.
                let inner_ty = infer_node(inner, &consumed);
                if inner_ty == TypeDesc::Span && !ctx.no_collapse {
                    TypeDesc::Span
                } else {
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else {
                // Many / Many1: use in_vec inference for inner elements.
                // Vec provides heap indirection, so Box is unnecessary.
                let inner_ty = infer_node_in_vec(inner, &consumed);
                if inner_ty == TypeDesc::Span && !ctx.no_collapse {
                    TypeDesc::Span
                } else {
                    TypeDesc::Vec(Box::new(inner_ty))
                }
            }
        }

        IrNode::Skip(left, _) => {
            let consumed = ctx.consumed();
            infer_node(left, &consumed)
        }
        IrNode::Next(_, right) => {
            let consumed = ctx.consumed();
            infer_node(right, &consumed)
        }
        IrNode::Minus(left, _) => {
            let consumed = ctx.consumed();
            infer_node(left, &consumed)
        }

        IrNode::Negate(_) => TypeDesc::Tuple(vec![]),

        IrNode::OptionalWhitespace(inner) => infer_node(inner, ctx),

        IrNode::Map { inner: _, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                // B.3: Use parsed return type if available.
                FnDescriptor::Custom { return_type, source } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*source)
                    }
                }
            }
        }
    }
}

/// Infer the output type of a single IR node in a Vec context.
///
/// Identical to `infer_node` except `Ref` returns `Enum` for non-transparent rules
/// (since Vec provides heap indirection, Box is unnecessary). Transparent rules
/// still return `BoxedEnum` since they box internally.
///
/// The `in_vec` context propagates through Skip (left), Next (right), Minus (left),
/// Map, and OptionalWhitespace — the same nodes that propagate `in_vec` in codegen.
/// It does NOT propagate into Seq children (multi-element Seq produces a tuple),
/// Alt branches (they produce compound types), or Repeat (which starts its own context).
fn infer_node_in_vec(node: &IrNode, ctx: &InferCtx<'_>) -> TypeDesc {
    match node {
        // In Vec context, ALL refs return Enum (no boxing needed).
        // Non-transparent: codegen emits Self::rule() without Box.
        // Transparent: codegen emits Self::rule_unboxed() which returns Enum directly.
        IrNode::Ref(_) => TypeDesc::Enum,
        IrNode::Skip(left, _) => {
            let consumed = ctx.consumed();
            infer_node_in_vec(left, &consumed)
        }
        IrNode::Next(_, right) => {
            let consumed = ctx.consumed();
            infer_node_in_vec(right, &consumed)
        }
        IrNode::Minus(left, _) => {
            let consumed = ctx.consumed();
            infer_node_in_vec(left, &consumed)
        }
        IrNode::OptionalWhitespace(inner) => infer_node_in_vec(inner, ctx),
        IrNode::Map { inner: _, fn_id } => {
            // Map determines its own type from FnDescriptor, not from inner.
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                FnDescriptor::Custom { return_type, source } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*source)
                    }
                }
            }
        }
        // Alt: try in_vec inference. Only apply if branches are homogeneous
        // with in_vec (otherwise coercion produces BoxedEnum, defeating in_vec).
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let consumed = ctx.consumed();
            let first = infer_node_in_vec(&branches[0].node, &consumed);
            let all_same = branches[1..]
                .iter()
                .all(|b| infer_node_in_vec(&b.node, &consumed) == first);
            if all_same {
                first
            } else {
                // Heterogeneous even with in_vec — fall back to standard inference.
                infer_node(node, ctx)
            }
        }
        // For all other nodes (Seq, Repeat, Literal, Regex, Epsilon, Negate),
        // delegate to infer_node.
        _ => infer_node(node, ctx),
    }
}

/// Infer the output type of a Seq (concatenation) node.
///
/// Applies:
/// - B.1: sp_method_rules Span override (with all-Span guard)
/// - B.2: @pretty/@no_collapse tuple preservation (consume flag)
/// - Consecutive-Span compression
/// - `(T, Vec<T>)` flattening
fn infer_seq(children: &[IrNode], ctx: &InferCtx<'_>) -> TypeDesc {
    if children.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    if children.len() == 1 {
        return infer_node(&children[0], ctx);
    }

    // B.1: Override Ref to rules with _sp() methods with Span type.
    // Matches emit_seq's sp_method_rules override: refs to rules with _sp()
    // methods get their _sp() method called (producing Span) instead of the
    // normal parser (producing BoxedEnum). Transparent rules are excluded
    // because the codegen doesn't override them.
    let child_types: Vec<TypeDesc> = children
        .iter()
        .map(|c| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    return TypeDesc::Span;
                }
            }
            let consumed = ctx.consumed();
            infer_node(c, &consumed)
        })
        .collect();

    // B.1 guard: if ALL elements would become Span after override, don't apply.
    // Re-infer without the override to get true types.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    let effective_types = if all_span {
        children
            .iter()
            .map(|c| {
                let consumed = ctx.consumed();
                infer_node(c, &consumed)
            })
            .collect::<Vec<_>>()
    } else {
        child_types
    };

    // B.2: Consume pretty_preserve flag. Only the top-level Seq preserves all-Span tuples.
    let pretty_preserve = ctx.pretty_preserve && effective_types.iter().all(|t| *t == TypeDesc::Span);

    // Consecutive Span compression (skip if pretty_preserve).
    let compressed = if pretty_preserve {
        effective_types
    } else {
        let mut result: Vec<TypeDesc> = Vec::new();
        let mut in_span_run = false;
        for ty in &effective_types {
            if *ty == TypeDesc::Span {
                if !in_span_run {
                    result.push(TypeDesc::Span);
                    in_span_run = true;
                }
            } else {
                result.push(ty.clone());
                in_span_run = false;
            }
        }
        result
    };

    // Single-element unwrap.
    if compressed.len() == 1 {
        return compressed.into_iter().next().unwrap();
    }

    // (T, Vec<T>) → Vec<T> flattening.
    if compressed.len() == 2 {
        if let Some(flattened) = try_flatten_pair(&compressed[0], &compressed[1]) {
            return flattened;
        }
    }

    TypeDesc::Tuple(compressed)
}

/// Try to flatten a 2-element tuple where one is `T` and the other is `Vec<T>`.
fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    // (T, Vec<T>) → Vec<T>
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
        // (BoxedEnum, Vec<Enum>) → Vec<Enum>: unbox the first element to match.
        if **inner == TypeDesc::Enum && *a == TypeDesc::BoxedEnum {
            return Some(b.clone());
        }
    }
    // (Vec<T>, T) → Vec<T>
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
        // (Vec<Enum>, BoxedEnum) → Vec<Enum>: unbox the last element to match.
        if **inner == TypeDesc::Enum && *b == TypeDesc::BoxedEnum {
            return Some(a.clone());
        }
    }
    None
}

// ── B.5: Sub-variant collection ─────────────────────────────────────────────

/// Raw sub-variant data before string interning.
struct RawSubVariant {
    variant_name: String,
    ty: TypeDesc,
    branch_index: u32,
}

/// Collect raw sub-variants for all heterogeneous alternations in a rule's body.
///
/// Walks the IR tree recursively to find nested heterogeneous Alts (e.g., an Alt
/// inside a Seq child), not just top-level ones. Each non-BoxedEnum branch of a
/// heterogeneous Alt gets a sub-variant so `coerce_branches` in the codegen can
/// box every branch into `Box<Enum>`.
fn collect_sub_variants_raw(
    rule_name: &str,
    body: &IrNode,
    ctx: &InferCtx<'_>,
) -> Vec<RawSubVariant> {
    let mut variants = Vec::new();
    let mut counter: u32 = 0;
    collect_sub_variants_walk(rule_name, body, ctx, &mut variants, &mut counter);
    variants
}

/// Recursive walker: visits every node, collecting sub-variants from heterogeneous Alts.
fn collect_sub_variants_walk(
    rule_name: &str,
    node: &IrNode,
    ctx: &InferCtx<'_>,
    variants: &mut Vec<RawSubVariant>,
    counter: &mut u32,
) {
    match node {
        IrNode::Alt(branches, _) => {
            let consumed = ctx.consumed();
            let tys: Vec<TypeDesc> = branches
                .iter()
                .map(|b| infer_node(&b.node, &consumed))
                .collect();

            let is_heterogeneous = tys.len() >= 2
                && !tys.windows(2).all(|w| w[0] == w[1]);

            if is_heterogeneous {
                // Collect sub-variants for branches that need coercion.
                // Skip BoxedEnum and Enum (already the unified enum type).
                let mut seen_types: Vec<(TypeDesc, String)> = Vec::new();
                for (i, ty) in tys.iter().enumerate() {
                    if *ty == TypeDesc::BoxedEnum || *ty == TypeDesc::Enum {
                        continue;
                    }
                    let variant_name = if let Some((_, existing)) =
                        seen_types.iter().find(|(seen_ty, _)| seen_ty == ty)
                    {
                        existing.clone()
                    } else {
                        let name = format!("{}_{}", rule_name, counter);
                        *counter += 1;
                        seen_types.push((ty.clone(), name.clone()));
                        name
                    };
                    variants.push(RawSubVariant {
                        variant_name,
                        ty: ty.clone(),
                        branch_index: i as u32,
                    });
                }
            }

            // Recurse into branches.
            for b in branches {
                collect_sub_variants_walk(rule_name, &b.node, ctx, variants, counter);
            }
        }

        // Recurse into children of composite nodes.
        IrNode::Seq(children) => {
            for c in children {
                collect_sub_variants_walk(rule_name, c, ctx, variants, counter);
            }
        }
        IrNode::Repeat { inner, .. } => {
            collect_sub_variants_walk(rule_name, inner, ctx, variants, counter);
        }
        IrNode::Map { inner, .. } => {
            collect_sub_variants_walk(rule_name, inner, ctx, variants, counter);
        }
        IrNode::Skip(left, right)
        | IrNode::Next(left, right)
        | IrNode::Minus(left, right) => {
            collect_sub_variants_walk(rule_name, left, ctx, variants, counter);
            collect_sub_variants_walk(rule_name, right, ctx, variants, counter);
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Negate(inner) => {
            collect_sub_variants_walk(rule_name, inner, ctx, variants, counter);
        }

        // Leaf nodes — nothing to recurse into.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Validate that no two rules define sub-variants with structurally identical types.
///
/// Span-typed sub-variants are exempt: Span is the most common leaf type and
/// many nested heterogeneous alts contain Span branches. Since the coercion is
/// always `Box::new(Enum::Variant(x))` regardless of which Span sub-variant
/// name is chosen, cross-rule duplication is harmless.
fn validate_sub_variant_uniqueness_raw(
    all_sub_variants: &HashMap<RuleId, Vec<RawSubVariant>>,
    rule_names: &HashMap<RuleId, String>,
) {
    let mut type_to_origin: Vec<(&TypeDesc, &str, &str)> = Vec::new();

    for (rule_id, variants) in all_sub_variants {
        let rule_name = rule_names.get(rule_id).map(|s| s.as_str()).unwrap_or("?");
        let mut seen_in_rule: Vec<&TypeDesc> = Vec::new();
        for sv in variants {
            // Skip Span-typed sub-variants: cross-rule Span duplicates are harmless.
            if sv.ty == TypeDesc::Span {
                continue;
            }
            if seen_in_rule.iter().any(|seen| *seen == &sv.ty) {
                continue;
            }
            seen_in_rule.push(&sv.ty);

            if let Some((_, other_rule, other_variant)) = type_to_origin
                .iter()
                .find(|(seen_ty, seen_rule, _)| *seen_ty == &sv.ty && *seen_rule != rule_name)
            {
                panic!(
                    "Sub-variant coercion ambiguity: rule `{}` variant `{}` and rule `{}` \
                     variant `{}` both produce structurally identical type `{:?}`. \
                     Consider making the branch types distinct or using an explicit \
                     mapping function.",
                    rule_name, sv.variant_name, other_rule, other_variant, sv.ty,
                );
            }
            type_to_origin.push((&sv.ty, rule_name, &sv.variant_name));
        }
    }
}
