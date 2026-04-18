//! String-shape detector — quoted-string regex leaf.
//!
//! # Predicate
//!
//! A rule is String-shaped when its body resolves to a single
//! [`IrNode::Regex`] (optionally wrapped in `Map` / `OptionalWhitespace`)
//! whose cached [`bbnf_regex::RegexClass`] classification is
//! [`bbnf_regex::RegexClass::QuotedString`].
//!
//! The canonical sources:
//!
//! - JSON: `string = /"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/ -> …`
//! - CSS: `string = /"(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'/`
//! - Sheets: `string = /"([^"]|"")*"/ -> input : Span`
//! - BBNF: `string_lit = /"…"/`.
//!
//! # Projection
//!
//! Reads [`GrammarIR::regex_info`], which the
//! [`crate::passes::regex_info::compute_regex_info`] pass populates
//! with each unique regex pattern's structural classification. The
//! `QuotedStringMiner` already surfaces the same fact via
//! `RecognizerShape::Regex` + `NodeFacts`; the String detector reads
//! `regex_info` directly so it doesn't have to re-derive the NodeId
//! indirection.

use bbnf_regex::RegexClass;

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect String-shape: the rule's body is a regex leaf whose
/// `RegexClass` classification is `QuotedString`.
pub fn detect_string(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Regex(sid) = body else {
        return false;
    };
    let Some(info) = ir.regex_info.get(sid) else {
        return false;
    };
    matches!(info.classification, RegexClass::QuotedString { .. })
}
