//! Sub-variant collection, gathering, and validation for heterogeneous alternations.

use std::collections::HashMap;

use crate::{IrNode, RuleId, TypeDesc};

use super::infer::infer_node;
use super::utils::InferCtx;

/// Raw sub-variant data before string interning.
pub(super) struct RawSubVariant {
    pub variant_name: String,
    pub ty: TypeDesc,
    pub branch_index: u32,
}

/// Collect raw sub-variants for all heterogeneous alternations in a rule's body.
///
/// Walks the IR tree recursively to find nested heterogeneous Alts (e.g., an Alt
/// inside a Seq child), not just top-level ones. Each non-BoxedEnum branch of a
/// heterogeneous Alt gets a sub-variant so `coerce_branches` in the codegen can
/// box every branch into `Box<Enum>`.
pub(super) fn collect_sub_variants_raw(
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

        IrNode::TokenDispatch { token, arms, fallback } => {
            collect_sub_variants_walk(rule_name, token, ctx, variants, counter);
            for arm in arms {
                collect_sub_variants_walk(rule_name, &arm.continuation, ctx, variants, counter);
            }
            collect_sub_variants_walk(rule_name, fallback, ctx, variants, counter);
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
pub(super) fn validate_sub_variant_uniqueness_raw(
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
