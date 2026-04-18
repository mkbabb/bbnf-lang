//! Array-shape detector — `Wrap(open, Repeat(value << sep?), close)`.
//!
//! # Predicate
//!
//! A rule is Array-shaped when either
//!
//! 1. Its body unwraps to a canonical `Wrap(open_byte, middle, close_byte)`
//!    whose `middle` is a `Repeat` over a homogeneous element — a Ref,
//!    a single-leaf Alt, or a simple Seq whose first non-trivial child
//!    is the value and whose tail is at most a trailing-comma skip. The
//!    canonical source is JSON's
//!    `array = "[" >> ((value << comma?)*)?w << "]"`.
//! 2. It is the entry rule of a list grammar (`stylesheet = rule*`,
//!    `grammar = item*`) per the
//!    [`crate::passes::recognizers::list_rules::mine_list_rules`]
//!    output.
//!
//! Object-shape is excluded by construction — the Object detector
//! runs first and wins on pair-shaped inner bodies.
//!
//! # Projection
//!
//! Reads IR structure directly. No new mining. Defers entry-list
//! recognition to `mine_list_rules`.

use std::collections::HashSet;

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use crate::passes::recognizers::list_rules::mine_list_rules;
use crate::types::{GrammarIR, IrNode, RuleId};

use super::object::is_pair_seq;

/// Detect Array-shape: either a delimited homogeneous-repeat or an
/// entry-rule list.
pub fn detect_array(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);

    // Shape 1 — wrapped homogeneous repeat.
    if let Some((open, middle, close)) = unwrap_wrap(body) {
        if single_byte_literal(open, ir).is_some()
            && single_byte_literal(close, ir).is_some()
        {
            let mut visited = HashSet::new();
            if inner_is_homogeneous_repeat(middle, ir, &mut visited) {
                return true;
            }
        }
    }

    // Shape 2 — entry-rule top-level list (no wrap).
    list_rule_entry_matches(rule_id, ir)
}

/// Whether `rule_id` matches the entry-rule list shape per
/// [`mine_list_rules`].
pub(crate) fn list_rule_entry_matches(rule_id: RuleId, ir: &GrammarIR) -> bool {
    mine_list_rules(ir).iter().any(|&id| id == rule_id)
}

/// Walk the wrap's middle looking for a `Repeat` whose inner resolves
/// to a homogeneous element (Ref, single-leaf, or non-pair Seq).
/// Returns `false` if the inner is pair-shaped (that's Object's
/// territory).
fn inner_is_homogeneous_repeat(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> bool {
    match unwrap_map_ow(node) {
        IrNode::Repeat { inner, .. } => {
            // Reject the Object-pair shape; Array is the complement.
            if is_pair_seq(inner, ir, &mut visited.clone()) {
                return false;
            }
            is_homogeneous_element(inner, ir, visited)
        }
        IrNode::Skip(lhs, _) | IrNode::Next(_, lhs) => {
            inner_is_homogeneous_repeat(lhs, ir, visited)
        }
        _ => false,
    }
}

/// Returns `true` when the repeat's inner element is a single
/// homogeneous value — a Ref, a single-child Seq, or a simple value
/// class with at most a trailing separator. Follows Refs through the
/// visited cycle guard.
fn is_homogeneous_element(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> bool {
    match unwrap_map_ow(node) {
        // The canonical `value << comma?` — the trailing optional
        // separator keeps the element homogeneous.
        IrNode::Skip(lhs, _) => is_homogeneous_element(lhs, ir, visited),
        IrNode::Next(_, rhs) => is_homogeneous_element(rhs, ir, visited),
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return false;
            }
            let rule = &ir.rules[*rid as usize];
            let result = is_homogeneous_element(&rule.body, ir, visited);
            visited.remove(rid);
            result
        }
        // An Alt-over-value-refs is the dispatcher pattern JSON
        // uses — each branch is a Ref to a primitive value rule.
        IrNode::Alt(_, _) => true,
        // A single-leaf or Literal value.
        IrNode::Literal(_) | IrNode::Regex(_) => true,
        // A Seq — admit when it's not pair-shaped.
        IrNode::Seq(_) => !is_pair_seq(node, ir, &mut visited.clone()),
        _ => false,
    }
}
