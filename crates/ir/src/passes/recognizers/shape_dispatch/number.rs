//! Number-shape detector — numeric regex leaf.
//!
//! # Predicate
//!
//! A rule is Number-shaped when its body resolves to a single
//! [`IrNode::Regex`] whose cached [`bbnf_regex::RegexClass`] is
//! [`bbnf_regex::RegexClass::Numeric`]. `Numeric` subsumes signed /
//! unsigned variants, optional fractions, optional exponents, and
//! JSON's `reject_leading_zero` via parameter flags the classifier
//! records per pattern.
//!
//! The canonical sources:
//!
//! - JSON: `number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ -> f64`
//! - CSS: `number = /-?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/`
//! - Sheets: `number = /(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?/ -> f64`.
//!
//! # Projection
//!
//! Reads [`GrammarIR::regex_info`]. No new mining.

use bbnf_regex::RegexClass;

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect Number-shape: the rule's body is a regex leaf whose
/// `RegexClass` classification is `Numeric`.
pub fn detect_number(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Regex(sid) = body else {
        return false;
    };
    let Some(info) = ir.regex_info.get(sid) else {
        return false;
    };
    matches!(info.classification, RegexClass::Numeric { .. })
}
