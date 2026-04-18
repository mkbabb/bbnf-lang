//! Scalar-shape detector — single typed leaf not admitted by the
//! primary shape detectors.
//!
//! # Predicate
//!
//! A rule is Scalar-shaped when its body is a single typed leaf —
//! e.g. a single literal or regex wrapped in a `Map` producing a
//! primitive scalar (`u8`, `i64`, `bool`, `f64`-not-covered-by-
//! Number, etc.) — and it was NOT admitted by the more specific
//! Object / Array / String / Number / Keyword detectors.
//!
//! Canonical sources:
//!
//! - Single-char structural rules like JSON's `comma = "," ?w`
//!   (though these are typically caught by Keyword first).
//! - Typed regex leaves not matching QuotedString / Numeric:
//!   `Sheets boolean = "TRUE" -> true | "FALSE" -> false` — but
//!   that's Alt-of-Literal, Keyword-shaped.
//! - Rule bodies that reduce to a single non-Alt leaf after
//!   stripping trivial wrappers.
//!
//! # Projection
//!
//! The detector is structural — it inspects the rule body's IR shape
//! directly. No miner output is consulted because Scalar is the
//! catch-all for single-leaf rules the primary detectors don't
//! admit.

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Scalar-shape: a single-leaf body (Literal / Regex / Ref)
/// after stripping trivial wrappers.
///
/// # Precedence
///
/// This detector runs AFTER the primary detectors (Object / Array /
/// String / Number / Keyword) per the dispatch order in
/// [`super::classify_rule`]; its job is to catch the residual single-
/// leaf rules those did not admit.
pub fn detect_scalar(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    matches!(
        body,
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Ref(_)
    )
}
