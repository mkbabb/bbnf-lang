//! Pass: IR-level alias detection and transparent alternation detection.
//!
//! These passes compute `RuleMeta::is_alias` and `RuleMeta::is_transparent`
//! directly from the IR structure, replacing the AST-level analysis functions
//! `find_aliases` and `find_transparent_alternations`.

use crate::{FnDescriptor, GrammarIR, IrNode, RuleId, TypeDesc};

/// Predicate: does any reachable descendant of `body` carry an
/// `IrNode::Map { fn_id }` whose `FnDescriptor::Expr.return_type` is
/// `Some(TypeDesc::Named(_))`?
///
/// Defensive predicate landed by AY.W2.2 — guards alias/transparent
/// stamping (and any future pipeline pass that strips wrappers) so a
/// rule whose body grounds a Named return type is never silently
/// reclassified as an alias / transparent / scalar surface, which
/// would erase the annotation before `emit_direct_to_struct_projection`
/// consumes it.
///
/// Walks the body tree fully (Map / Seq / Alt / Repeat / Skip / Next /
/// Minus / Negate / OptionalWhitespace / TokenDispatch arms +
/// fallback). Returns true on the first Named-bearing Map seen.
pub fn has_named_return_type(body: &IrNode, ir: &GrammarIR) -> bool {
    match body {
        IrNode::Map { inner, fn_id } => {
            if let FnDescriptor::Expr {
                return_type: Some(TypeDesc::Named(_)),
                ..
            } = &ir.fns[*fn_id as usize]
            {
                return true;
            }
            has_named_return_type(inner, ir)
        }
        IrNode::Seq(children) => children.iter().any(|c| has_named_return_type(c, ir)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| has_named_return_type(&b.node, ir)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => has_named_return_type(inner, ir),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            has_named_return_type(a, ir) || has_named_return_type(b, ir)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            has_named_return_type(token, ir)
                || arms.iter().any(|arm| {
                    has_named_return_type(&arm.continuation, ir)
                        || arm.map_fn.is_some_and(|mf| {
                            matches!(
                                &ir.fns[mf as usize],
                                FnDescriptor::Expr {
                                    return_type: Some(TypeDesc::Named(_)),
                                    ..
                                }
                            )
                        })
                })
                || has_named_return_type(fallback, ir)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => false,
    }
}

/// Detect alias rules: rules whose body is a bare `Ref(target_id)`.
///
/// An alias is a rule of the form `A = B ;` where the body lowers to a
/// single `IrNode::Ref(target_id)`. Cyclic rules are excluded since alias
/// chains must terminate.
///
/// Sets `RuleMeta::is_alias = Some(target_id)` for each detected alias.
/// Must run BEFORE `canonicalize_aliases` (which resolves alias chains).
///
/// Defensively guards rules carrying a Named return type anywhere in
/// the body subtree — alias canonicalisation would erase the
/// annotation before `emit_direct_to_struct_projection` consumes it.
pub fn compute_aliases(ir: &mut GrammarIR) {
    // Pre-compute Named-bearing rule ids; the borrow split lets us
    // mutate `ir.rules` while reading `ir.fns` through `has_named_return_type`.
    let named_ids: Vec<RuleId> = ir
        .rules
        .iter()
        .filter_map(|r| (!r.meta.is_cyclic && has_named_return_type(&r.body, ir)).then_some(r.id))
        .collect();

    for rule in &mut ir.rules {
        if rule.meta.is_cyclic {
            continue;
        }
        if named_ids.contains(&rule.id) {
            rule.meta.is_alias = None;
            continue;
        }

        rule.meta.is_alias = extract_alias_target(&rule.body);
    }
}

/// Extract the alias target from an IR node.
///
/// Unwraps Map wrappers (enum/box wrapping is transparent for alias purposes)
/// and returns the target RuleId if the body is a bare Ref.
fn extract_alias_target(node: &IrNode) -> Option<RuleId> {
    match node {
        IrNode::Ref(id) => Some(*id),
        // Map wrappers (EnumWrap, BoxWrap) are transparent for alias detection.
        // However, value-changing maps (Expr, NumberConvert, etc.) change semantics,
        // so we only unwrap if the inner is a Ref. Conservative: `A = B` yes, `A = f(B)` no.
        _ => None,
    }
}

/// Detect transparent alternation rules: cyclic rules whose body is a
/// pure alternation of `Ref` nodes.
///
/// A transparent alternation is a rule like `value = object | array | string ;`
/// where every branch is a bare nonterminal reference. These rules generate
/// enum types with one variant per branch rather than a structural type.
///
/// Sets `RuleMeta::is_transparent = true` for each detected transparent rule.
///
/// Defensively guards rules carrying a Named return type anywhere in
/// the body subtree — transparent stamping would erase the annotation
/// before `emit_direct_to_struct_projection` consumes it.
pub fn compute_transparent(ir: &mut GrammarIR) {
    let named_ids: Vec<RuleId> = ir
        .rules
        .iter()
        .filter_map(|r| (r.meta.is_cyclic && has_named_return_type(&r.body, ir)).then_some(r.id))
        .collect();

    for rule in &mut ir.rules {
        if !rule.meta.is_cyclic {
            rule.meta.is_transparent = false;
            continue;
        }
        if named_ids.contains(&rule.id) {
            rule.meta.is_transparent = false;
            continue;
        }

        rule.meta.is_transparent = is_transparent_body(&rule.body);
    }
}

/// Check if an IR node body is a pure alternation of Ref nodes.
fn is_transparent_body(node: &IrNode) -> bool {
    match node {
        IrNode::Alt(branches, _) => branches.iter().all(|b| matches!(&b.node, IrNode::Ref(_))),
        // A Map wrapper around an Alt can occur from enum wrapping, but the
        // transparency detection should look through it if the Alt is all-Ref.
        // However, to keep the semantics identical to the AST-level pass, we
        // only detect the direct case.
        _ => false,
    }
}
