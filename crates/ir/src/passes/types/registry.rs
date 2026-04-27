//! Registry-population phase of `project_types`.
//!
//! Walks every `Named` rule in the IR and emits a [`StructLayout`] entry
//! into [`crate::registry::StructRegistry`]. The layout's
//! [`crate::registry::LayoutKind`] is derived from the rule body's
//! `IrNode` shape; its fields are projected from the per-rule
//! [`TypeDesc`] map produced by the upstream type-projection CSP.
//!
//! Rule-shape → layout-kind mapping (exhaustive; per
//! `feedback_no-silent-epsilon` the catch-all panics):
//!
//! - `Alt(branches, _)` → [`LayoutKind::TaggedEnum`] (or
//!   [`LayoutKind::UntaggedEnum`] when every branch projects to the
//!   same `TypeDesc`). One `BranchTag` field per branch; field name is
//!   the sub-variant name when `RuleMeta::sub_variants` carries one,
//!   else `branch_<idx>`.
//! - `Seq(children)` → [`LayoutKind::Struct`]. One `SeqPosition` field
//!   per child; field name is `field_<position>`.
//! - `Map { inner, .. }` of a single typed leaf — wrapping
//!   `Regex` / `Literal` / `Epsilon` — projects to
//!   [`LayoutKind::NewtypeWrapper`] when the rule's projected type is
//!   a primitive scalar (`F64` / `Bool` / `Span` / integer family).
//!   For composite return types the wrapped projection collapses to a
//!   `Struct` with a single TypedLeaf field carrying the projection.
//! - `Repeat { inner, lo, hi }` with `hi > 1` → `Struct` with one
//!   `RepeatElement` field carrying the inner's projected type. The
//!   rule's outermost type descriptor is `TypeDesc::Vec(_)`.
//! - `Ref(rule_id)` → `Struct` with one `RuleReference` field whose
//!   `target_rule` is the referenced rule's id and whose type is
//!   `TypeDesc::Named(_)` keyed by the referenced rule's name.
//! - `Skip` / `Next` / `Minus` / `Negate` / `OptionalWhitespace` /
//!   `TokenDispatch` → unwrap to the dominant child (the kept side
//!   for `Skip` / `Next`; the inner for `Negate` /
//!   `OptionalWhitespace`; the token + arms for `TokenDispatch` flatten
//!   into a Struct).
//! - Bare `Literal` / `Regex` / `Epsilon` → `Struct` with one
//!   `TypedLeaf` field carrying the rule's projected `TypeDesc::Span`.
//!
//! The closure runs after the upstream solver has populated `ir.types`
//! and `ir.type_map`, so per-rule and per-node types are available
//! without re-deriving them.

use rustc_hash::FxHashMap;

use crate::dag::GrammarDag;
use crate::registry::{FieldSource, LayoutKind, StructField, StructLayout, StructRegistry};
use crate::types::{FnDescriptor, GrammarIR, IrNode, IrRule, RuleId, SubVariant, TypeDesc};

use super::type_map::TypeMap;

/// Populate `ir.struct_registry` from the projected types and rule
/// bodies.
///
/// `rule_types` is the per-rule projected `TypeDesc` map produced by
/// the upstream type-projection CSP; `type_map` is the per-node type
/// map. Both must be populated before the registry-population phase
/// runs — `project_types`'s phase ordering invariants guarantee this.
///
/// The closure clears the registry and rebuilds it from scratch on
/// every invocation; the fixed-point loop in `project_types` runs the
/// upstream solver to convergence first, so a single populate pass
/// suffices. Idempotent: a second invocation against the same
/// projected types produces an identical registry.
pub fn populate_struct_registry(
    ir: &mut GrammarIR,
    rule_types: &FxHashMap<RuleId, TypeDesc>,
    type_map: &TypeMap,
) {
    let mut registry = StructRegistry::new();

    // Snapshot rule names to a borrow-stable map; the per-rule layout
    // builder needs both the rule's own name (for the layout's
    // `rule_name` field) and any rule-reference target name (resolved
    // when projecting `Ref(rule_id)` fields).
    let rule_names: FxHashMap<RuleId, String> = ir
        .rules
        .iter()
        .map(|r| (r.id, ir.get_string(r.name).to_string()))
        .collect();

    // The DAG is asserted non-None at `project_types` entry; in
    // direct-call test contexts where the DAG has not been built, the
    // per-node type lookup falls back to the rule's projected type
    // descriptor. The registry shape (layout kind + field count +
    // field source) is decided by IR shape alone, so DAG absence does
    // not change layout-shape correctness; it only loses per-node
    // type refinement (which the rule-type fallback restores).
    let dag = ir.dag.as_ref();
    let fns = ir.fns.as_slice();

    for rule in &ir.rules {
        let layout =
            build_layout_for_rule(rule, rule_types, type_map, &rule_names, dag, fns);
        registry.insert(layout);
    }

    ir.struct_registry = registry;
}

/// Build the [`StructLayout`] for a single rule.
///
/// The body shape decides [`LayoutKind`] and field provenance.
fn build_layout_for_rule(
    rule: &IrRule,
    rule_types: &FxHashMap<RuleId, TypeDesc>,
    type_map: &TypeMap,
    rule_names: &FxHashMap<RuleId, String>,
    dag: Option<&GrammarDag>,
    fns: &[FnDescriptor],
) -> StructLayout {
    let rule_name = rule_names
        .get(&rule.id)
        .cloned()
        .expect("rule.id present in rule_names map");
    let rule_type = rule_types
        .get(&rule.id)
        .cloned()
        .unwrap_or(TypeDesc::Span);

    let (kind, fields) = classify_body(
        &rule.body,
        rule_type.clone(),
        &rule.meta.sub_variants,
        type_map,
        rule_names,
        dag,
        fns,
        rule_types,
    );

    StructLayout {
        rule_id: rule.id,
        rule_name,
        kind,
        rule_type,
        fields,
    }
}

/// Classify a rule body to a `(LayoutKind, Vec<StructField>)`.
///
/// Per `feedback_no-silent-epsilon`: the match is exhaustive over
/// every `IrNode` variant. An added variant fails to compile here.
fn classify_body(
    body: &IrNode,
    rule_type: TypeDesc,
    sub_variants: &[SubVariant],
    type_map: &TypeMap,
    rule_names: &FxHashMap<RuleId, String>,
    dag: Option<&GrammarDag>,
    fns: &[FnDescriptor],
    rule_types: &FxHashMap<RuleId, TypeDesc>,
) -> (LayoutKind, Vec<StructField>) {
    match body {
        // ── Alternation → tagged enum (or untagged when all branches share type) ──
        IrNode::Alt(branches, _) => {
            let mut fields = Vec::with_capacity(branches.len());
            for (idx, branch) in branches.iter().enumerate() {
                let branch_idx = idx as u32;
                let branch_type = branch_projected_type(
                    &branch.node,
                    type_map,
                    dag,
                    fns,
                    rule_types,
                )
                .unwrap_or_else(|| rule_type.clone());
                let name = sub_variant_name_for_branch(sub_variants, branch_idx)
                    .unwrap_or_else(|| format!("branch_{}", branch_idx));
                fields.push(StructField {
                    name,
                    type_desc: branch_type,
                    source: FieldSource::BranchTag {
                        branch_index: branch_idx,
                    },
                });
            }
            let kind = if fields.is_empty() {
                LayoutKind::Struct
            } else if fields.iter().all(|f| f.type_desc == fields[0].type_desc) {
                LayoutKind::UntaggedEnum
            } else {
                LayoutKind::TaggedEnum
            };
            (kind, fields)
        }

        // ── Seq → struct with field-per-position ──
        IrNode::Seq(children) => {
            let fields: Vec<StructField> = children
                .iter()
                .enumerate()
                .map(|(pos, child)| {
                    let position = pos as u32;
                    let ty = branch_projected_type(child, type_map, dag, fns, rule_types)
                        .unwrap_or(TypeDesc::Span);
                    let name = field_name_for_seq_child(child, position, rule_names);
                    StructField {
                        name,
                        type_desc: ty,
                        source: FieldSource::SeqPosition { position },
                    }
                })
                .collect();
            (LayoutKind::Struct, fields)
        }

        // ── Map of a single typed leaf → newtype wrapper or struct ──
        IrNode::Map { inner, .. } => {
            // The Map's typed projection lives on the rule's outermost
            // type descriptor (rule_type); the inner is the leaf the
            // typed `->` annotation captures from. A Map wrapping a
            // single Regex / Literal / Epsilon body with a primitive
            // projection becomes a NewtypeWrapper. Map of a composite
            // body (Seq / Alt / Repeat) recurses into the inner so the
            // composite shape carries.
            if is_single_leaf_body(inner) && rule_type.is_scalar_payload() {
                let field = StructField {
                    name: "value".to_string(),
                    type_desc: rule_type.clone(),
                    source: FieldSource::TypedLeaf,
                };
                (LayoutKind::NewtypeWrapper, vec![field])
            } else {
                // Composite body: classify the inner and lift its
                // discriminator. The single typed annotation flows
                // through to the inner's outermost field.
                classify_body(
                    inner,
                    rule_type,
                    sub_variants,
                    type_map,
                    rule_names,
                    dag,
                    fns,
                    rule_types,
                )
            }
        }

        // ── Repeat → list-typed struct with single RepeatElement field ──
        IrNode::Repeat { inner, lo, hi } => {
            // Optional/single-shot Repeat (lo=0, hi=1) collapses to
            // its inner's shape.
            if *lo == 0 && *hi == 1 {
                return classify_body(
                    inner,
                    rule_type,
                    sub_variants,
                    type_map,
                    rule_names,
                    dag,
                    fns,
                    rule_types,
                );
            }
            let elem_type = branch_projected_type(inner, type_map, dag, fns, rule_types)
                .unwrap_or(TypeDesc::Span);
            let field = StructField {
                name: "element".to_string(),
                type_desc: elem_type,
                source: FieldSource::RepeatElement,
            };
            (LayoutKind::Struct, vec![field])
        }

        // ── Ref → single rule-reference field ──
        IrNode::Ref(target_rule) => {
            let target_name = rule_names
                .get(target_rule)
                .cloned()
                .unwrap_or_else(|| format!("rule_{}", target_rule));
            let field = StructField {
                name: target_name,
                type_desc: rule_type.clone(),
                source: FieldSource::RuleReference {
                    target_rule: *target_rule,
                },
            };
            (LayoutKind::Struct, vec![field])
        }

        // ── Skip / Next: keep the kept side, project as a single field ──
        IrNode::Skip(kept, _discard) => classify_body(
            kept,
            rule_type,
            sub_variants,
            type_map,
            rule_names,
            dag,
            fns,
            rule_types,
        ),
        IrNode::Next(_discard, kept) => classify_body(
            kept,
            rule_type,
            sub_variants,
            type_map,
            rule_names,
            dag,
            fns,
            rule_types,
        ),

        // ── Minus: dominant side is the lhs (the matched operand) ──
        IrNode::Minus(lhs, _rhs) => classify_body(
            lhs,
            rule_type,
            sub_variants,
            type_map,
            rule_names,
            dag,
            fns,
            rule_types,
        ),

        // ── Negate / OptionalWhitespace unwrap to inner ──
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => classify_body(
            inner,
            rule_type,
            sub_variants,
            type_map,
            rule_names,
            dag,
            fns,
            rule_types,
        ),

        // ── TokenDispatch: token + arm continuations + fallback flatten to struct ──
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            // The dispatched-token shape is a struct whose fields are:
            // - position 0: the token leaf
            // - per-arm continuation
            // - fallback continuation
            let mut fields: Vec<StructField> = Vec::with_capacity(arms.len() + 2);
            let token_ty = branch_projected_type(token, type_map, dag, fns, rule_types)
                .unwrap_or(TypeDesc::Span);
            fields.push(StructField {
                name: "token".to_string(),
                type_desc: token_ty,
                source: FieldSource::SeqPosition { position: 0 },
            });
            for (idx, arm) in arms.iter().enumerate() {
                let position = (idx + 1) as u32;
                let ty =
                    branch_projected_type(&arm.continuation, type_map, dag, fns, rule_types)
                        .unwrap_or(TypeDesc::Span);
                fields.push(StructField {
                    name: format!("arm_{}", idx),
                    type_desc: ty,
                    source: FieldSource::SeqPosition { position },
                });
            }
            let fallback_position = (arms.len() + 1) as u32;
            let fallback_ty =
                branch_projected_type(fallback, type_map, dag, fns, rule_types)
                    .unwrap_or(TypeDesc::Span);
            fields.push(StructField {
                name: "fallback".to_string(),
                type_desc: fallback_ty,
                source: FieldSource::SeqPosition {
                    position: fallback_position,
                },
            });
            (LayoutKind::Struct, fields)
        }

        // ── Bare leaves: single TypedLeaf field carrying the rule's projection ──
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
            let field = StructField {
                name: "value".to_string(),
                type_desc: rule_type.clone(),
                source: FieldSource::TypedLeaf,
            };
            // Primitive projection is a newtype wrapper; the
            // BoxedEnum / Enum / composite cases fall back to a single-
            // field struct so the consumer still has a stable
            // accessor surface.
            let kind = if rule_type.is_scalar_payload() {
                LayoutKind::NewtypeWrapper
            } else {
                LayoutKind::Struct
            };
            (kind, vec![field])
        }
    }
}

/// Resolve a child node's projected type.
///
/// Resolution order:
///
/// 1. DAG-keyed [`TypeMap`] lookup — the precise per-node type slot
///    populated by the upstream type-projection CSP. This is the
///    standard path inside `project_types`.
/// 2. Function-descriptor fallback — when the node is `Map(_, fn_id)`
///    and the descriptor carries an explicit return type, use that.
///    Covers typed-`->` annotations the grammar author wrote
///    explicitly, even before the DAG is wired.
/// 3. Reference fallback — when the node is `Ref(rule_id)` and the
///    referenced rule has a projected `TypeDesc` in `rule_types`,
///    use that.
/// 4. Recursive descent through structural wrappers
///    (`Skip` / `Next` / `Minus` / `Negate` / `OptionalWhitespace`)
///    until a typed leaf or terminal is found.
///
/// Returns `None` when no resolution succeeds; the caller defaults to
/// `TypeDesc::Span` per the existing sub-variant collector policy.
fn branch_projected_type(
    node: &IrNode,
    type_map: &TypeMap,
    dag: Option<&GrammarDag>,
    fns: &[FnDescriptor],
    rule_types: &FxHashMap<RuleId, TypeDesc>,
) -> Option<TypeDesc> {
    // 1. DAG-keyed precise per-node type.
    if let Some(dag) = dag {
        if let Some(id) = dag.node_for(node) {
            if let Some(ty) = type_map.node_type(id) {
                return Some(ty.clone());
            }
        }
    }

    // 2. & 3. & 4. — structure-driven fallbacks.
    match node {
        IrNode::Map { fn_id, inner } => {
            if let Some(fd) = fns.get(*fn_id as usize) {
                if let Some(ty) = fn_descriptor_return_type(fd) {
                    return Some(ty);
                }
            }
            // Map with no explicit return type — descend into inner.
            branch_projected_type(inner, type_map, dag, fns, rule_types)
        }
        IrNode::Ref(target) => rule_types.get(target).cloned(),
        IrNode::Skip(kept, _) | IrNode::Minus(kept, _) => {
            branch_projected_type(kept, type_map, dag, fns, rule_types)
        }
        IrNode::Next(_, kept) => {
            branch_projected_type(kept, type_map, dag, fns, rule_types)
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            branch_projected_type(inner, type_map, dag, fns, rule_types)
        }
        // Leaves with no inherent typed projection default to None;
        // the caller falls back to `Span`.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => None,
        // Composite shapes have no single representative type at this
        // boundary; the caller's per-shape default applies.
        IrNode::Seq(_)
        | IrNode::Alt(_, _)
        | IrNode::Repeat { .. }
        | IrNode::TokenDispatch { .. } => None,
    }
}

/// Project a [`FnDescriptor`] to the typed return its grammar
/// annotation produces, when one is available.
///
/// The compiler-internal specialisations (`NumberConvert`,
/// `HexConvert`, `SpanCapture`) carry a known fixed return type;
/// `Expr { return_type: Some(_), .. }` carries the grammar-author's
/// explicit annotation; `Expr { return_type: None, .. }`,
/// `EnumWrap`, and `BoxWrap` carry no fixed projection here and the
/// caller falls through.
fn fn_descriptor_return_type(fd: &FnDescriptor) -> Option<TypeDesc> {
    match fd {
        FnDescriptor::Expr {
            return_type: Some(t),
            ..
        } => Some(t.clone()),
        FnDescriptor::NumberConvert { .. } => Some(TypeDesc::F64),
        FnDescriptor::HexConvert { .. } => Some(TypeDesc::U32),
        FnDescriptor::SpanCapture => Some(TypeDesc::Span),
        FnDescriptor::Expr {
            return_type: None, ..
        }
        | FnDescriptor::EnumWrap { .. }
        | FnDescriptor::BoxWrap => None,
    }
}

/// True iff `body` is `Literal` / `Regex` / `Epsilon` — the leaf
/// primitives a `Map` newtype-wrapper must contain.
fn is_single_leaf_body(body: &IrNode) -> bool {
    matches!(
        body,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon
    )
}

/// Resolve a sub-variant name for `branch_idx` if `RuleMeta::sub_variants`
/// carries one; `None` if no entry maps the branch.
///
/// The sub-variant collector populates this map for heterogeneous
/// alternations only; homogeneous Alt branches return `None` and the
/// caller falls back to `branch_<idx>`.
fn sub_variant_name_for_branch(sub_variants: &[SubVariant], branch_idx: u32) -> Option<String> {
    sub_variants
        .iter()
        .find(|sv| sv.branch_index == branch_idx)
        .map(|sv| sv.variant_name.to_string())
}

/// Resolve a stable field name for a Seq child. `Ref` projects to the
/// referenced rule's name; non-Ref children get `field_<position>`.
fn field_name_for_seq_child(
    child: &IrNode,
    position: u32,
    rule_names: &FxHashMap<RuleId, String>,
) -> String {
    if let IrNode::Ref(target) = child {
        if let Some(name) = rule_names.get(target) {
            return name.clone();
        }
    }
    format!("field_{}", position)
}
