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

/// A Seq admits the Object-pair shape when it has at least two
/// structural positions where somewhere in the Seq's descent there
/// is a single-byte pivot literal (e.g. `:` or `;`) AND at least two
/// distinct non-literal value positions (the key and the value).
///
/// The pivot may be:
///
/// - A direct Literal child of the Seq.
/// - A Ref child whose target rule resolves to a single-byte Literal
///   (JSON's `colon = ":" ?w` and CSS's `":"` before values).
/// - Wrapped in `Next` / `Skip` chains.
fn seq_has_key_pivot_value(children: &[IrNode], ir: &GrammarIR) -> bool {
    let positions: Vec<&IrNode> = children
        .iter()
        .map(unwrap_map_ow)
        .filter(|c| {
            !matches!(
                c,
                IrNode::Epsilon | IrNode::OptionalWhitespace(_)
            )
        })
        .collect();
    if positions.len() < 2 {
        return false;
    }
    // Leading single-byte literal = keyword head, not pair.
    if let IrNode::Literal(sid) = positions[0] {
        let bytes = ir.get_string(*sid).as_bytes();
        if bytes.len() == 1 {
            return false;
        }
    }
    // Count distinct value (Ref/Regex/compound) and pivot (single-
    // byte literal or single-byte ref) occurrences across the
    // Seq's flattened tree.
    let mut value_count = 0usize;
    let mut pivot_count = 0usize;
    let mut seen_refs: HashSet<RuleId> = HashSet::new();
    for pos in &positions {
        let (pivot, value) = scan_tree(pos, ir, &mut seen_refs);
        pivot_count += pivot;
        value_count += value;
    }
    // Object pair shape: ≥ 1 pivot byte and ≥ 2 value positions
    // (key + value).
    pivot_count >= 1 && value_count >= 2
}

/// Walk a tree and count (pivots, values). Pivots are single-byte
/// literals (either direct or resolved through a single Ref hop).
/// Values are Refs to non-single-byte rules, Regexes, Alts, or
/// Seqs.
fn scan_tree(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> (usize, usize) {
    match unwrap_map_ow(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            if bytes.len() == 1 {
                (1, 0)
            } else {
                (0, 0)
            }
        }
        IrNode::Regex(_) => (0, 1),
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return (0, 1); // cyclic — treat as value leaf
            }
            let rule = &ir.rules[*rid as usize];
            let resolved_body = unwrap_map_ow(&rule.body);
            // Check if this Ref resolves (through trivial wrappers)
            // to a single-byte literal — that makes it a pivot-
            // carrier. JSON's `colon = ":" ?w` lowers to
            // `OptionalWhitespace(Literal(":"))` or
            // `Skip(Literal(":"), ...)`; strip wrappers to check.
            let result = if is_single_byte_literal_rule(resolved_body, ir) {
                (1, 0)
            } else {
                (0, 1)
            };
            visited.remove(rid);
            result
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let (lp, lv) = scan_tree(lhs, ir, visited);
            let (rp, rv) = scan_tree(rhs, ir, visited);
            (lp + rp, lv + rv)
        }
        IrNode::Alt(_, _) | IrNode::Seq(_) => (0, 1),
        IrNode::Repeat { inner, .. } => scan_tree(inner, ir, visited),
        _ => (0, 0),
    }
}

/// Returns `true` when `body` ultimately resolves to a single-byte
/// literal, possibly wrapped in OptionalWhitespace / Map / Skip /
/// Next trivia or bridged through a single Ref.
fn is_single_byte_literal_rule(body: &IrNode, ir: &GrammarIR) -> bool {
    match unwrap_map_ow(body) {
        IrNode::Literal(sid) => {
            ir.get_string(*sid).as_bytes().len() == 1
        }
        IrNode::Skip(lhs, _) | IrNode::Next(lhs, _) => {
            is_single_byte_literal_rule(lhs, ir)
        }
        _ => false,
    }
}
