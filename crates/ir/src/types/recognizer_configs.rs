//! Recognizer configuration payloads attached to IR decisions.
//!
//! These are the data payloads that the grammar-tier recognizer mining
//! pass populates on a per-`NodeId` basis for the backend to consume at
//! emission time. They live in `bbnf-ir` (not `bbnf-core`) because the
//! IR is authoritative: the backend reads these via sidecar maps on
//! `GrammarIR`, it does not recompute them.
//!
//! - [`DelimScanConfig`] — delimiter-scan wrap configuration (open/close
//!   bytes, pivot byte, optional trail byte, block/pivot rule refs).
//!   Populated by `passes::recognizers::delim_scan::collect`.
//! - [`KeyClass`], [`KeyDispatchConfig`], [`DetectedBranch`],
//!   [`KeyDispatchMatch`] — keyword-dispatch alternation configuration.
//!   Populated by `passes::recognizers::key_dispatch::collect`.

use bbnf_regex::RegexClass;
use serde::{Deserialize, Serialize};

use super::RuleId;

// ─── Delimiter Scan ──────────────────────────────────────────────────────

/// Grammar-agnostic delimiter-scan configuration.
///
/// Captures an `open >> body << close` wrap where the body is a
/// `Repeat(Alt(...))` and the alt branches are distinguishable by a
/// single-byte "pivot" in leading position. Sufficient data for the
/// backend's `emit_delim_scan` to generate a flat forward-scanning
/// loop without re-walking the IR.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DelimScanConfig {
    pub open_byte: u8,
    pub close_byte: u8,
    pub pivot_byte: u8,
    pub trail_byte: Option<u8>,
    pub block_rule: Option<(RuleId, String)>,
    pub pivot_rule: Option<(RuleId, String)>,
}

// ─── Key Dispatch ────────────────────────────────────────────────────────

/// Key class for key-dispatch optimization.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum KeyClass {
    /// Identifiers: `[a-zA-Z_][\w-]*`.
    Identifier,
    /// Quoted strings: `"..."` or `'...'`.
    QuotedString { quote_char: u8 },
}

/// The regex pattern string for a given key class.
///
/// Delegates to [`RegexClass::canonical_pattern`] so the canonical
/// regex literals live in exactly one place — the parameterized
/// classifier is the source of truth, this accessor just selects
/// the right variant for the requested key class.
pub fn key_class_regex_pattern(class: &KeyClass) -> &'static str {
    match class {
        KeyClass::Identifier => RegexClass::Identifier {
            allows_leading_dash: false,
            allows_double_dash_prefix: false,
        }
        .canonical_pattern()
        .expect("RegexClass::Identifier without flags has a canonical pattern"),
        KeyClass::QuotedString { quote_char } => RegexClass::QuotedString {
            quote_char: *quote_char,
            allows_escapes: false,
            allows_u_escapes: false,
        }
        .canonical_pattern()
        .expect("RegexClass::QuotedString has a canonical pattern"),
    }
}

/// Configuration for key-dispatch alternation.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KeyDispatchConfig {
    pub key_class: KeyClass,
    pub separator: Option<String>,
    /// Regex ID for the key scanner, assigned by the backend driver at
    /// emit time (the IR mining pass leaves this `None`).
    pub key_scanner_regex_id: Option<usize>,
}

/// Result of key dispatch detection for a single branch.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DetectedBranch {
    pub key_literals: Vec<String>,
    pub branch_idx: usize,
}

/// Pre-solved key-dispatch detection for an Alt node. Tuple form:
/// `(config, detected_branches, fallback_branch_indices)`.
///
/// `fallback_branch_indices` lists Alt branch indices that are NOT
/// key-dispatched. These are tried sequentially (checkpoint chain)
/// when no key matches. Typically includes a generic catch-all as
/// the last entry, but may also include non-literal-led branches
/// (e.g. regex-prefixed custom-property declarations) that cannot
/// participate in key lookup.
pub type KeyDispatchMatch = (KeyDispatchConfig, Vec<DetectedBranch>, Vec<usize>);
