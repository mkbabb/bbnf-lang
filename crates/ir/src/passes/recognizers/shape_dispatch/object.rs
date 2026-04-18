//! Object-shape detector — `Wrap(open, Repeat(Seq(key, sep, value)), close)`.
//!
//! # Predicate
//!
//! A rule is Object-shaped when
//!
//! 1. Its body unwraps to a canonical `Wrap(open_byte, middle, close_byte)`
//!    with single-byte open/close literals.
//! 2. The wrap's `middle` is a `Repeat` whose inner body resolves — after
//!    stripping trailing separators (`<< comma?`) — to a Seq-shape
//!    (either directly or through a Ref to a pair rule) whose first
//!    non-whitespace child is a key-class leaf and whose tail contains
//!    a structural pivot byte (e.g. `:` or `;`).
//!
//! The canonical source is JSON's
//! `object = "{" >> ((pair << comma?)*)?w << "}"` with
//! `pair = string, colon >> value`.
//!
//! # Projection
//!
//! Reads IR structure directly. Does not consult
//! `ir.delim_scan_configs` — that miner only records configs for
//! Wraps whose inner is an Alt-over-branches with a per-branch
//! pivot, which JSON object fails (the pair is a single Ref, not
//! a multi-branch Alt). The shape detector has to walk the wrap
//! shape itself.

use std::collections::HashSet;

use crate::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Object-shape on the rule body: a wrap over a `Repeat` whose
/// inner is a pair-shaped Seq (or Ref to one).
pub fn detect_object(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let Some((open, middle, close)) = unwrap_wrap(body) else {
        return false;
    };
    if single_byte_literal(open, ir).is_none() {
        return false;
    }
    if single_byte_literal(close, ir).is_none() {
        return false;
    }
    // Inspect the wrap's middle for a Repeat whose inner Seq is
    // pair-shaped.
    let mut visited = HashSet::new();
    inner_is_pair_shaped(middle, ir, &mut visited)
}

/// Walk the wrap's middle — stripping OptionalWhitespace / Map
/// wrappers and Next/Skip chains — looking for a `Repeat(lo: 0, ..)`
/// whose inner is a pair-shaped Seq.
fn inner_is_pair_shaped(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> bool {
    match unwrap_map_ow(node) {
        IrNode::Repeat { inner, .. } => is_pair_seq(inner, ir, visited),
        IrNode::Skip(lhs, _) | IrNode::Next(_, lhs) => {
            inner_is_pair_shaped(lhs, ir, visited)
        }
        _ => false,
    }
}

/// Returns `true` when `node` resolves to a Seq carrying at least
/// three structural positions — [key, pivot, value] in the canonical
/// JSON case. Follows Refs (with cycle protection) and strips
/// trailing `<< comma?` wrappers.
pub(super) fn is_pair_seq(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> bool {
    match unwrap_map_ow(node) {
        // `pair << comma?` — strip the trailing separator.
        IrNode::Skip(lhs, _) => is_pair_seq(lhs, ir, visited),
        // `a >> pair` — strip leading trivia.
        IrNode::Next(_, rhs) => is_pair_seq(rhs, ir, visited),
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return false;
            }
            let rule = &ir.rules[*rid as usize];
            let result = is_pair_seq(&rule.body, ir, visited);
            visited.remove(rid);
            result
        }
        IrNode::Seq(children) => seq_has_key_pivot_value(children, ir),
        // An Alt of pair-shaped branches (e.g. CSS ruleBlock where
        // every branch is a declaration Seq) counts.
        IrNode::Alt(branches, _) => branches
            .iter()
            .all(|b| is_pair_seq(&b.node, ir, visited)),
        _ => false,
    }
}

/// A Seq admits the Object-pair shape when it has at least three
/// non-trivial children: a key-class leaf, a pivot literal (one byte
/// such as `:` or `;`), and a value-class node. Non-structural
/// children (whitespace placeholders) don't count against the
/// position budget.
fn seq_has_key_pivot_value(children: &[IrNode], ir: &GrammarIR) -> bool {
    // Count structurally-meaningful children and look for a
    // single-byte literal pivot somewhere in the middle.
    let mut saw_key = false;
    let mut saw_pivot = false;
    let mut saw_value = false;
    for child in children {
        match unwrap_map_ow(child) {
            IrNode::Epsilon | IrNode::OptionalWhitespace(_) => continue,
            IrNode::Literal(_) if !saw_key => {
                // Leading literal without a key first — not pair-
                // shaped (that's a keyword head, not a key → value).
                return false;
            }
            IrNode::Literal(sid) => {
                let bytes = ir.get_string(*sid).as_bytes();
                if bytes.len() == 1 && !saw_pivot {
                    saw_pivot = true;
                }
            }
            other if !saw_key => {
                // First non-literal child is the key.
                let _ = other;
                saw_key = true;
            }
            _ if saw_pivot && !saw_value => {
                saw_value = true;
            }
            _ => {}
        }
    }
    saw_key && saw_pivot && saw_value
}
