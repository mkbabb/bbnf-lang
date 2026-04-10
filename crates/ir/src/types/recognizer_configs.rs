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
pub fn key_class_regex_pattern(class: &KeyClass) -> &'static str {
    match class {
        KeyClass::Identifier => r"[a-zA-Z_][\w-]*",
        KeyClass::QuotedString { quote_char } => match quote_char {
            b'\'' => r"'[^']*'",
            _ => r#""[^"]*""#,
        },
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

/// Pre-solved key-dispatch detection for an Alt node. Tuple form kept
/// for backward compatibility with the previous `backend/patterns/`
/// lookup map signature: `(config, detected_branches, fallback_idx)`.
pub type KeyDispatchMatch = (KeyDispatchConfig, Vec<DetectedBranch>, Option<usize>);
