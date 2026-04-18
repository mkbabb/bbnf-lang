//! Unordered-shape detector — disjoint-FIRST Alt under a non-empty Repeat.
//!
//! # Predicate
//!
//! A rule is Unordered-shaped when its body resolves to a
//! `Repeat { lo: 1, .. }` over an `Alt` whose branches all have
//! mutually-disjoint FIRST byte sets. The FIRST-disjointness is the
//! authoritative [`DisjointFirstMiner`](
//! crate::passes::recognizers::disjoint_first::DisjointFirstMiner)
//! output — a mined
//! [`DisjointFirstTable`](
//! crate::passes::recognizers::disjoint_first::DisjointFirstTable)
//! exists on the Alt's [`crate::dag::NodeId`] when the alphabet
//! admits single-byte dispatch.
//!
//! The `lo: 1` bound excludes optional-Alt patterns (`( alt ) ?`)
//! which are either Wrap-shape (transparent dispatcher) or absorbed
//! into the enclosing shape. The Unordered emitter emits a sub-loop
//! that MUST iterate at least once.
//!
//! # Canonical sources
//!
//! - CSS `compoundSelector = (classSelector | idSelector |
//!   attrSelector | colonSelector | typeSelector) +` per
//!   `grammar/css/l4/selectors.bbnf:87-88` — 5 branches with disjoint
//!   FIRST bytes (`.`, `#`, `[`, `:`, `[a-z]`).
//!
//! # Projection
//!
//! Reads [`GrammarIR::disjoint_first_tables`] keyed by the inner
//! Alt's [`crate::dag::NodeId`]. No new mining.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Unordered-shape: body is `Repeat { lo >= 1 }` over an Alt
/// whose branches have mutually-disjoint FIRST byte sets (per the
/// existing [`DisjointFirstMiner`](
/// crate::passes::recognizers::disjoint_first::DisjointFirstMiner)
/// output).
pub fn detect_unordered(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    // Strip through Skip / Next of trivia ws wrappers. The canonical
    // `compoundSelector = (alt) +` body surfaces as the Repeat directly;
    // no additional unwrap required.
    let IrNode::Repeat { inner, lo, hi: _ } = body else {
        return false;
    };
    if *lo < 1 {
        return false;
    }
    let inner_body = unwrap_map_ow(inner);
    let IrNode::Alt(_, _) = inner_body else {
        return false;
    };
    // Must have a mined DisjointFirstTable on the Alt's NodeId.
    let Some(dag) = ir.dag.as_ref() else {
        return false;
    };
    let Some(alt_node_id) = dag.node_for(inner_body) else {
        return false;
    };
    ir.disjoint_first_tables
        .get(&alt_node_id)
        .is_some_and(|tbl| tbl.branch_count >= 2)
}
