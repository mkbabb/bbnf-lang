//! HRegex-shape detector — regex leaf with host decode.
//!
//! # Predicate
//!
//! A rule is HRegex-shaped when its body resolves to a single
//! [`IrNode::Regex`] whose classification is NOT one of the two
//! narrow families claimed by earlier detectors:
//!
//! - [`bbnf_regex::RegexClass::QuotedString`] — claimed by
//!   [`super::string::detect_string`].
//! - [`bbnf_regex::RegexClass::Numeric`] — claimed by
//!   [`super::number::detect_number`].
//!
//! Every other regex leaf falls to HRegex: `Identifier`,
//! `HexDigits`, `PrefixThenClass`, `CharClassQuantified`,
//! `WhitespaceWithBlockComment`, `AccelDriven`, `Unknown`. The
//! emitter dispatches these through a regex-scan + optional host
//! decode function call per the rule's `-> host_fn(input) : type`
//! annotation.
//!
//! # Canonical sources
//!
//! - CSS `hex = "#" , /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) :
//!   u32` per `grammar/css/l4/color.bbnf:189-190`. Note this rule is
//!   a Seq (not a single Regex) — it wouldn't match a bare-regex
//!   detector; the CSS `ident` / `selectorIdent` / `dashIdent` /
//!   `propertyName` are the pure-regex HRegex cases.
//! - Sheets `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span` per
//!   `grammar/google-sheets/google-sheets.bbnf:60`.
//! - Sheets `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span`
//!   per `grammar/google-sheets/google-sheets.bbnf:87`.
//! - Sheets `sheet_prefix = /'(?:[^']|'')*'!/ -> 0u8 |
//!   /[A-Za-z_]\w*!/ -> 1u8` per
//!   `grammar/google-sheets/google-sheets.bbnf:48-49` — Alt body; not
//!   an HRegex single-regex leaf.
//! - BBNF `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span` per
//!   `grammar/bbnf/bbnf.bbnf:9`.
//!
//! # Projection
//!
//! Reads [`GrammarIR::regex_info`] — populated by
//! [`crate::passes::regex_info::compute_regex_info`] during the IR
//! construction pass. No new mining.

use bbnf_regex::RegexClass;

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{GrammarIR, IrNode, RuleId};

/// Detect HRegex-shape: a single regex leaf whose class is neither
/// QuotedString nor Numeric.
pub fn detect_hregex(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Regex(sid) = body else {
        return false;
    };
    let Some(info) = ir.regex_info.get(sid) else {
        // Pattern not classified — treat as HRegex with Unknown
        // lowering. The emitter's HRegex branch falls back to a
        // generic regex scan in that case.
        return true;
    };
    !matches!(
        info.classification,
        RegexClass::QuotedString { .. } | RegexClass::Numeric { .. }
    )
}
