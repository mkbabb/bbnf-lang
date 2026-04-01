//! Pass: IR-level alias detection and transparent alternation detection.
//!
//! These passes compute `RuleMeta::is_alias` and `RuleMeta::is_transparent`
//! directly from the IR structure, replacing the AST-level analysis functions
//! `find_aliases` and `find_transparent_alternations`.

use crate::{GrammarIR, IrNode, RuleId};

/// Detect alias rules: rules whose body is a bare `Ref(target_id)`.
///
/// An alias is a rule of the form `A = B ;` where the body lowers to a
/// single `IrNode::Ref(target_id)`. Cyclic rules are excluded since alias
/// chains must terminate.
///
/// Sets `RuleMeta::is_alias = Some(target_id)` for each detected alias.
/// Must run BEFORE `canonicalize_aliases` (which resolves alias chains).
pub fn compute_aliases(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        if rule.meta.is_cyclic {
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
        // However, a Custom/NumberConvert/etc. map changes semantics, so we only
        // unwrap if the inner is a Ref. This is conservative: we detect `A = B`
        // but not `A = f(B)`.
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
pub fn compute_transparent(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        if !rule.meta.is_cyclic {
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
