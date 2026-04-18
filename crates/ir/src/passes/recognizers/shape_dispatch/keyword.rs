//! Keyword-shape detector — literal leaf or short Alt-of-literal.
//!
//! # Predicate
//!
//! A rule is Keyword-shaped when one of the following holds:
//!
//! 1. **Single literal body**: the body resolves to a
//!    [`IrNode::Literal`] (after stripping Map / OptionalWhitespace).
//!    Canonical: JSON's `null = "null" -> 0u8`; CSS's
//!    `displayKeyword = "block"` family; BBNF directive bodies.
//!
//! 2. **Alt of literal-led branches**: the body is an
//!    [`IrNode::Alt`] with 2..=[`KEYWORD_MAX_BRANCHES`] branches,
//!    every branch literal-led per the
//!    [`crate::passes::recognizers::keyword_stats::KeywordStatsMiner`]
//!    output, and the cumulative literal set has ≤
//!    `KEYWORD_MAX_BRANCHES` entries. Canonical: JSON's
//!    `bool = "true" -> true | "false" -> false`; CSS's
//!    `nthFunctionName = "nth-child" | "nth-last-child" | …`.
//!
//! The branch-count bound matches the K-S-L threshold used by the
//! existing keyword-dispatch code (PHF activates at a higher count;
//! the shape tag is separate from the PHF emission decision).
//!
//! # Projection
//!
//! Reads [`GrammarIR::keyword_branches`] — populated by
//! [`crate::passes::recognizers::keyword_stats::KeywordStatsMiner`]
//! during `mine_recognizers`. Supplements with direct IR inspection
//! for the single-literal case the Alt miner does not cover.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Maximum Alt branch count admitted to `Keyword` shape.
///
/// Matches the K-S-L (keyword-short-list) threshold the existing
/// keyword dispatch uses. Alts with more branches graduate to PHF
/// emission per `crate::passes::recognizers::keyword_stats` — the
/// Keyword shape's inline per-branch comparison is only viable
/// under this bound.
pub const KEYWORD_MAX_BRANCHES: usize = 16;

/// Detect Keyword-shape: a single literal body OR a bounded Alt
/// whose branches are all literal-led.
pub fn detect_keyword(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);

    // Case 1: single literal body (possibly Map-wrapped at the
    // outer level, already stripped by `unwrap_map_ow`).
    if matches!(body, IrNode::Literal(_)) {
        return true;
    }

    // Case 2: Alt of literal-led branches, read from the
    // KeywordStatsMiner output keyed by the Alt's NodeId.
    let IrNode::Alt(branches, _) = body else {
        return false;
    };
    if branches.is_empty() || branches.len() > KEYWORD_MAX_BRANCHES {
        return false;
    }
    let Some(dag) = ir.dag.as_ref() else {
        return false;
    };
    let Some(node_id) = dag.node_for(body) else {
        return false;
    };
    let Some(mined) = ir.keyword_branches.get(&node_id) else {
        return false;
    };
    // Every branch must be literal-led. The miner surfaces only the
    // literal-led subset — if its count equals the Alt's branch
    // count, the Alt is fully literal-led.
    mined.len() == branches.len()
}
