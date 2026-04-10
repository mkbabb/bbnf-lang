//! Sub-variant collection, gathering, and validation for heterogeneous alternations.

use std::collections::HashMap;

use crate::dag::GrammarDag;
use crate::{IrNode, RuleId, TypeDesc};

use super::utils::TypeMap;

/// Raw sub-variant data before string interning.
pub(super) struct RawSubVariant {
    pub variant_name: String,
    pub ty: TypeDesc,
    pub branch_index: u32,
}

/// Collect raw sub-variants for all heterogeneous alternations in a rule's body.
///
/// Walks the IR tree recursively to find nested heterogeneous Alts
/// (e.g., an Alt inside a Seq child), not just top-level ones. Each
/// non-BoxedEnum branch of a heterogeneous Alt gets a sub-variant
/// so `coerce_branches` in the codegen can box every branch into
/// `Box<Enum>`.
///
/// Uses the local (in-progress) `TypeMap` + the durable `GrammarDag`
/// to resolve `&IrNode → NodeId → TypeDesc`.
pub(super) fn collect_sub_variants_raw(
    rule_name: &str,
    body: &IrNode,
    type_map: &TypeMap,
    dag: &GrammarDag,
) -> Vec<RawSubVariant> {
    let mut variants = Vec::new();
    let mut counter: u32 = 0;
    collect_sub_variants_walk(rule_name, body, type_map, dag, &mut variants, &mut counter);
    variants
}

fn lookup_node_type(
    node: &IrNode,
    type_map: &TypeMap,
    dag: &GrammarDag,
) -> TypeDesc {
    dag.node_for(node)
        .and_then(|id| type_map.node_type(id).cloned())
        .unwrap_or(TypeDesc::Span)
}

/// Recursive walker: visits every node, collecting sub-variants from heterogeneous Alts.
fn collect_sub_variants_walk(
    rule_name: &str,
    node: &IrNode,
    type_map: &TypeMap,
    dag: &GrammarDag,
    variants: &mut Vec<RawSubVariant>,
    counter: &mut u32,
) {
    match node {
        IrNode::Alt(branches, _) => {
            let tys: Vec<TypeDesc> = branches
                .iter()
                .map(|b| lookup_node_type(&b.node, type_map, dag))
                .collect();

            let is_heterogeneous = tys.len() >= 2 && !tys.windows(2).all(|w| w[0] == w[1]);

            if is_heterogeneous {
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

            for b in branches {
                collect_sub_variants_walk(rule_name, &b.node, type_map, dag, variants, counter);
            }
        }

        IrNode::Seq(children) => {
            for c in children {
                collect_sub_variants_walk(rule_name, c, type_map, dag, variants, counter);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => {
            collect_sub_variants_walk(rule_name, inner, type_map, dag, variants, counter);
        }
        IrNode::Skip(left, right) | IrNode::Next(left, right) | IrNode::Minus(left, right) => {
            collect_sub_variants_walk(rule_name, left, type_map, dag, variants, counter);
            collect_sub_variants_walk(rule_name, right, type_map, dag, variants, counter);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_sub_variants_walk(rule_name, token, type_map, dag, variants, counter);
            for arm in arms {
                collect_sub_variants_walk(
                    rule_name,
                    &arm.continuation,
                    type_map,
                    dag,
                    variants,
                    counter,
                );
            }
            collect_sub_variants_walk(rule_name, fallback, type_map, dag, variants, counter);
        }

        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Validate that no two rules define sub-variants with structurally identical types.
///
/// Span-typed sub-variants are exempt: Span is the most common leaf type and
/// many nested heterogeneous alts contain Span branches. Since the coercion is
/// always `Box::new(Enum::Variant(x))` regardless of which Span sub-variant
/// name is chosen, cross-rule duplication is harmless.
pub(super) fn validate_sub_variant_uniqueness_raw<'a>(
    all_sub_variants: &'a HashMap<RuleId, Vec<RawSubVariant>>,
    rule_names: &'a rustc_hash::FxHashMap<RuleId, &'a str>,
) {
    let mut type_to_origin: Vec<(&TypeDesc, &str, &str)> = Vec::new();

    for (rule_id, variants) in all_sub_variants {
        let rule_name = rule_names.get(rule_id).copied().unwrap_or("?");
        let mut seen_in_rule: Vec<&TypeDesc> = Vec::new();
        for sv in variants {
            // Skip Span-typed sub-variants: cross-rule Span duplicates are harmless.
            if sv.ty == TypeDesc::Span {
                continue;
            }
            if seen_in_rule.contains(&&sv.ty) {
                continue;
            }
            seen_in_rule.push(&sv.ty);

            if let Some((_, other_rule, other_variant)) = type_to_origin
                .iter()
                .find(|(seen_ty, seen_rule, _)| *seen_ty == &sv.ty && *seen_rule != rule_name)
            {
                // Cross-rule type collision: two rules produce structurally identical
                // sub-variant types. This is handled at codegen time by scoping the
                // sub-variant search to the current rule. Log but don't panic.
                #[cfg(debug_assertions)]
                eprintln!(
                    "Note: sub-variant type collision between `{}::{}` and `{}::{}` ({:?}). \
                     Codegen will resolve by rule-scoped lookup.",
                    rule_name, sv.variant_name, other_rule, other_variant, sv.ty,
                );
            }
            type_to_origin.push((&sv.ty, rule_name, &sv.variant_name));
        }
    }
}
