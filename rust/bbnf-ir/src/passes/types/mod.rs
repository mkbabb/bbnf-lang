//! Pass: Type projection for IR rules.
//!
//! Walks each rule's body and projects a `TypeDesc` describing the Rust/TS type
//! that the rule produces. Populates `GrammarIR::types` with `(RuleId, TypeDesc)`
//! pairs consumed by codegen backends.
//!
//! Also collects sub-variants for heterogeneous alternations and stores them
//! in `RuleMeta::sub_variants`.

mod project;
mod subvariants;
mod utils;

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId, SubVariant, TypeDesc};

use subvariants::{collect_sub_variants_raw, validate_sub_variant_uniqueness_raw};

// Re-export for codegen.
pub use project::{project_node, project_node_in_vec};
pub use utils::{ProjectionCtx, ProjectionRules, TypeMap, TypeRecorder, try_flatten_pair};

/// Project types for all rules and populate `ir.types`.
///
/// Rules are processed in topological order (the order they appear in `ir.rules`),
/// so a rule's dependencies are always projected before the rule itself. Cyclic
/// rules are assigned `BoxedEnum` for any back-references.
///
/// Implements 5 projection rules:
/// 1. Span-method override: Ref nodes to rules with `_sp()` methods project as
///    Span in Seq context, with a safety guard that reverts the override when any
///    child is Span through complex expressions (Repeat, Skip, etc.).
/// 2. Span preservation: When `preserve_spans` is set, the top-level Seq preserves
///    individual Span identity (skips consecutive-Span compression). Currently
///    sourced from `@pretty` directives; flag is consumed after the first Seq.
/// 3. Annotated return type: Map nodes with FnDescriptor::Custom use the parsed
///    return type annotation from the closure, falling back to Named.
/// 4. Cyclic-context boxing: (infrastructure only) Would override acyclic Ref
///    types to BoxedEnum when referenced from cyclic rules.
/// 5. Sub-variant collection: Heterogeneous alternation branches get generated
///    variant names for codegen coercion.
pub fn project_types(ir: &mut GrammarIR) {
    let mut cache: HashMap<RuleId, TypeDesc> = HashMap::new();

    // Collect metadata before projection (avoids borrow issues).
    let rule_meta: HashMap<RuleId, (bool, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.id, (r.meta.is_cyclic, r.meta.span_eligible)))
        .collect();

    // Collect which rules have @pretty (for span preservation).
    let pretty_preserve_rules: HashMap<RuleId, bool> = ir
        .rules
        .iter()
        .map(|r| {
            let has_pretty = r.meta.directives.pretty.is_some();
            (r.id, has_pretty)
        })
        .collect();

    // Identify acyclic rules (for cyclic-context boxing, not yet implemented).
    let acyclic_rules: std::collections::HashSet<RuleId> = ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();

    // Process rules in order (topological after pipeline passes).
    // Record sub-expression types for codegen via TypeRecorder.
    let recorder = TypeRecorder::new();

    // Collect rule IDs + metadata to avoid borrowing ir during projection.
    // Process rules in their array order (topological).
    // Use array index for body access, rule.id for cache key.
    let rule_meta_vec: Vec<(RuleId, usize, bool, bool)> = ir
        .rules
        .iter()
        .enumerate()
        .map(|(idx, r)| {
            let is_cyclic = rule_meta.get(&r.id).copied().unwrap_or_default().0;
            let preserve_spans = pretty_preserve_rules.get(&r.id).copied().unwrap_or(false);
            (r.id, idx, is_cyclic, preserve_spans)
        })
        .collect();

    for &(id, idx, is_cyclic, preserve_spans) in &rule_meta_vec {
        let ctx = ProjectionCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            rules: utils::ProjectionRules { preserve_spans },
            recorder: Some(&recorder),
        };

        // Project on the ORIGINAL body (not a clone) so pointers match codegen.
        let ty = project_node(&ir.rules[idx].body, &ctx);
        cache.insert(id, ty);
    }

    // Comprehensive recording pass: walk every node in every rule body and record
    // both project_node and project_node_in_vec results. The cache is fully populated,
    // so this is just lookups + recording. Ensures the TypeMap covers all nodes
    // that codegen might query (including span-method overridden Ref children and
    // nodes skipped by the main projection path).
    for &(_id, idx, is_cyclic, preserve_spans) in &rule_meta_vec {
        let ctx = ProjectionCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            rules: utils::ProjectionRules { preserve_spans },
            recorder: Some(&recorder),
        };
        record_all_node_types(&ir.rules[idx].body, &ctx);
    }

    // Sub-variant collection for heterogeneous alternations.
    // Two-pass: first collect raw (String name, TypeDesc, branch_index), then intern names.
    let rule_names: HashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    let mut raw_sub_variants: HashMap<RuleId, Vec<subvariants::RawSubVariant>> = HashMap::new();

    for &(id, idx, is_cyclic, _) in &rule_meta_vec {
        let rule = &ir.rules[idx];
        if rule.meta.is_transparent {
            continue;
        }
        let ctx = ProjectionCtx {
            ir,
            cache: &cache,
            acyclic_rules: &acyclic_rules,
            cyclic_context: is_cyclic,
            rules: utils::ProjectionRules::default(),
            recorder: None, // No recording needed for sub-variants
        };
        let rule_name = rule_names
            .get(&id)
            .expect("rule ID must exist in rule_names map");
        let svs = collect_sub_variants_raw(rule_name, &rule.body, &ctx);
        if !svs.is_empty() {
            raw_sub_variants.insert(id, svs);
        }
    }

    // Validate cross-rule sub-variant uniqueness (using raw names).
    validate_sub_variant_uniqueness_raw(&raw_sub_variants, &rule_names);

    // Clear all existing sub-variants before writing new ones.
    // This is critical when project_types runs multiple times (e.g., pipeline call
    // with collapse_simple_spans=false, then generate_all with collapse_simple_spans=true).
    // Without clearing, stale sub-variants from the first run persist and cause
    // type mismatches in codegen.
    for rule in &mut ir.rules {
        rule.meta.sub_variants.clear();
    }

    // Intern sub-variant names and write to rules.
    for rule in &mut ir.rules {
        if let Some(raw_svs) = raw_sub_variants.remove(&rule.id) {
            rule.meta.sub_variants = raw_svs
                .into_iter()
                .map(|rsv| {
                    // Intern the variant name into the string table.
                    let name_id =
                        if let Some(pos) = ir.strings.iter().position(|s| s == &rsv.variant_name) {
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

    // Store the precomputed sub-expression type map for codegen.
    let mut type_map = recorder.into_map();

    // Correction pass: align Repeat vec_elem_types with ir.types.
    let types_map: HashMap<RuleId, TypeDesc> =
        cache.into_iter().collect();
    for rule in &ir.rules {
        let rule_td = types_map.get(&rule.id);
        if let Some(td) = rule_td {
            correct_repeat_elem_types(&rule.body, td, &mut type_map);
        }
    }

    // Collect distinct scratch types for codegen Vec generation.
    //
    // Only collect from actual Repeat inners (hi > 1), not from all vec_elem_type
    // values in the TypeMap. The TypeMap records vec_elem_types for every node
    // (including non-Repeat nodes like Seq and Alt), and those "ghost" types would
    // create scratch fields that call alloc_slice_clone on BumpSlab with
    // incompatible element types.
    //
    // Two sources, both targeted at actual Vec elements:
    // (a) Walk rule bodies to find Repeat inners and collect their vec_elem_types
    // (b) Vec inners extracted from ir.types (handles nested Vecs in rule types)
    let mut scratch: Vec<TypeDesc> = Vec::new();

    fn collect_repeat_scratch(node: &IrNode, map: &utils::TypeMap, out: &mut Vec<TypeDesc>) {
        match node {
            IrNode::Repeat { inner, hi, .. } if *hi > 1 => {
                if let Some(ty) = map.vec_elem_type(inner) {
                    if *ty != TypeDesc::Span && !out.contains(ty) {
                        out.push(ty.clone());
                    }
                }
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::Seq(children) => {
                for c in children {
                    collect_repeat_scratch(c, map, out);
                }
            }
            IrNode::Alt(branches, _) => {
                for b in branches {
                    collect_repeat_scratch(&b.node, map, out);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                collect_repeat_scratch(l, map, out);
                collect_repeat_scratch(r, map, out);
            }
            IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. }
            | IrNode::Negate(inner) => {
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::Repeat { inner, .. } => {
                collect_repeat_scratch(inner, map, out);
            }
            IrNode::TokenDispatch { arms, fallback, .. } => {
                for arm in arms {
                    collect_repeat_scratch(&arm.continuation, map, out);
                }
                collect_repeat_scratch(fallback, map, out);
            }
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        }
    }
    for rule in &ir.rules {
        collect_repeat_scratch(&rule.body, &type_map, &mut scratch);
    }

    // Also collect Vec inners from rule types (handles nested Vecs like
    // Vec(Vec(Enum)) where the inner Vec is a valid scratch element type).
    fn collect_scratch_from_td(td: &TypeDesc, out: &mut Vec<TypeDesc>) {
        match td {
            TypeDesc::Vec(inner) => {
                if *inner.as_ref() != TypeDesc::Span && !out.contains(inner.as_ref()) {
                    out.push(inner.as_ref().clone());
                }
                collect_scratch_from_td(inner, out);
            }
            TypeDesc::Option(inner) => collect_scratch_from_td(inner, out),
            TypeDesc::Tuple(elems) => {
                for e in elems {
                    collect_scratch_from_td(e, out);
                }
            }
            _ => {}
        }
    }
    for (_, td) in &types_map {
        collect_scratch_from_td(td, &mut scratch);
    }
    type_map.set_scratch_types(scratch);

    ir.type_map = Some(type_map);
    ir.types = types_map.into_iter().collect();
    ir.types.sort_by_key(|(id, _)| *id);
}

/// Correct Repeat vec_elem_types to match the Vec inner from ir.types.
///
/// When a rule's type is `Vec(T)` (direct or via try_flatten_pair), the Repeat
/// inner's vec_elem_type should be `T`. The initial projection may have recorded
/// a different type (e.g., Tuple) due to Seq compression/flattening differences.
fn correct_repeat_elem_types(
    node: &IrNode,
    rule_type: &TypeDesc,
    map: &mut utils::TypeMap,
) {
    // Extract the Vec inner from the rule type, searching through Tuples
    // (for rules like `Tuple(Span, Vec(Enum))` where Seq didn't fully flatten).
    fn extract_all_vec_inners<'a>(td: &'a TypeDesc, out: &mut Vec<&'a TypeDesc>) {
        match td {
            TypeDesc::Vec(inner) => out.push(inner.as_ref()),
            TypeDesc::Tuple(elems) => {
                for e in elems {
                    extract_all_vec_inners(e, out);
                }
            }
            TypeDesc::Option(inner) => extract_all_vec_inners(inner, out),
            _ => {}
        }
    }

    let mut vec_inners = Vec::new();
    extract_all_vec_inners(rule_type, &mut vec_inners);
    // Only correct if there's exactly one Vec (unambiguous).
    let vec_inner = match vec_inners.as_slice() {
        [inner] => *inner,
        _ => return,
    };

    // Walk the body to find Repeat nodes (with hi > 1, i.e., Many/Many1).
    fn walk_and_correct(node: &IrNode, vec_inner: &TypeDesc, map: &mut utils::TypeMap) {
        match node {
            IrNode::Repeat { inner, lo: _, hi } if *hi > 1 => {
                // Override the Repeat inner's vec_elem_type with the authoritative inner.
                map.set_vec_elem_type(inner.as_ref(), vec_inner.clone());
            }
            IrNode::Seq(children) => {
                for c in children {
                    walk_and_correct(c, vec_inner, map);
                }
            }
            IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
                walk_and_correct(l, vec_inner, map);
                walk_and_correct(r, vec_inner, map);
            }
            IrNode::OptionalWhitespace(inner) => {
                walk_and_correct(inner, vec_inner, map);
            }
            _ => {}
        }
    }

    walk_and_correct(node, vec_inner, map);
}

/// Walk every node in the tree and record both `project_node` and `project_node_in_vec`
/// results. The cache must be fully populated before calling this.
fn record_all_node_types(node: &IrNode, ctx: &utils::ProjectionCtx<'_>) {
    // Record types only if not already recorded by the main projection pass.
    // This preserves span-method override types while filling in nodes that the main
    // pass didn't visit (e.g., discarded sides of Skip/Next).
    let node_ty = project::project_node(node, &ctx.with_no_recorder());
    let vec_ty = project::project_node_in_vec(node, &ctx.with_no_recorder());
    if let Some(rec) = ctx.recorder {
        rec.record_node_if_absent(node, &node_ty);
        rec.record_vec_elem_if_absent(node, &vec_ty);
    }

    // Recurse into children.
    match node {
        IrNode::Seq(children) => {
            // Record seq_child_types + seq_result for nested Seqs not covered
            // by the main projection pass (which only processes top-level rule bodies).
            if children.len() > 1 {
                if let Some(rec) = ctx.recorder {
                    if !rec.has_seq_children(children) {
                        // Re-project the Seq to record all its type data.
                        let _ = project::project_node(node, ctx);
                    }
                }
            }
            let consumed = ctx.consumed();
            for c in children {
                record_all_node_types(c, &consumed);
            }
        }
        IrNode::Alt(branches, _) => {
            let consumed = ctx.consumed();
            for b in branches {
                record_all_node_types(&b.node, &consumed);
            }
        }
        IrNode::Repeat { inner, .. } => {
            let consumed = ctx.consumed();
            record_all_node_types(inner, &consumed);
        }
        IrNode::Skip(l, r) | IrNode::Next(l, r) | IrNode::Minus(l, r) => {
            let consumed = ctx.consumed();
            record_all_node_types(l, &consumed);
            record_all_node_types(r, &consumed);
        }
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } | IrNode::Negate(inner) => {
            record_all_node_types(inner, ctx);
        }
        IrNode::TokenDispatch { arms, .. } => {
            let consumed = ctx.consumed();
            for arm in arms {
                record_all_node_types(&arm.continuation, &consumed);
            }
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}
