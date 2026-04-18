//! Scalar-shape detector — single typed Literal leaf not admitted by
//! the primary shape detectors.
//!
//! # Predicate
//!
//! A rule is Scalar-shaped when its body is a single [`IrNode::Literal`]
//! after stripping trivial wrappers — typically a single-byte
//! structural separator the surrounding shape consumes inline (JSON's
//! `comma = "," ?w` / `colon = ":" ?w`).
//!
//! Regex bodies that fail Number- and String-detectors and Ref bodies
//! to other rules are HRegex- / Wrap-shaped (W4 detectors); they stay
//! [`super::ShapeTag::None`] in W3 and route through the walker.
//! Admitting them here would route through the [`super::scalar`]
//! emitter's Literal-only path, which has no Regex / Ref lowering.
//!
//! # Projection
//!
//! Pure structural inspection of the rule body — no miner output.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Scalar-shape: a single [`IrNode::Literal`] leaf after
/// stripping trivial wrappers.
///
/// # Precedence
///
/// This detector runs AFTER the primary detectors (Object / Array /
/// String / Number / Keyword) per the dispatch order in
/// [`super::classify_rule`]; its job is to catch single-Literal rules
/// the Keyword detector did not admit (e.g. structural separators
/// without an Alt branch context).
pub fn detect_scalar(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    matches!(body, IrNode::Literal(_))
}
