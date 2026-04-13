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
            allows_escapes: true,
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
/// `(config, detected_branches, fallback_branch_indices, key_index)`.
///
/// `fallback_branch_indices` lists Alt branch indices that are NOT
/// key-dispatched. These are tried sequentially (checkpoint chain)
/// when no key matches. Typically includes a generic catch-all as
/// the last entry, but may also include non-literal-led branches
/// (e.g. regex-prefixed custom-property declarations) that cannot
/// participate in key lookup.
///
/// `key_index` is the AQ.7.3 length-bucketed lookup table covering
/// every detected key. Codegen emits a length-first match arm
/// followed by per-length probing (linear, first-byte hash, or
/// dense table) so the cost per dispatch falls from O(branches) to
/// O(1) under the perfect-hash buckets.
pub type KeyDispatchMatch = (
    KeyDispatchConfig,
    Vec<DetectedBranch>,
    Vec<usize>,
    KeyIndex,
);

/// Length-bucketed key index for AQ.7.3 perfect-hash dispatch.
///
/// Every detected key is bucketed by byte length. Within a bucket,
/// the lookup probe selects between three forms based on bucket
/// size and first-byte distribution:
///
/// - `Linear` — bucket has ≤ 4 entries, codegen emits a sequential
///   array-equality check with no extra probe overhead.
/// - `FirstByteTable` — bucket has > 4 entries with no first-byte
///   collisions, codegen emits a 256-entry table indexed by
///   `key[0]` returning the matching entry index (or sentinel for
///   no match), followed by a single equality check on the bucket
///   slot.
/// - `Linear` (collision fallback) — bucket has > 4 entries but
///   first-byte collisions force a linear scan over the bucket.
///   Reserved for the rare case where two keys of the same length
///   start with the same byte (e.g. `align`/`accent`).
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct KeyIndex {
    /// Buckets keyed by byte length, sorted ascending so the
    /// codegen match arm enumerates lengths predictably.
    pub buckets: Vec<LengthBucket>,
}

/// One length bucket inside [`KeyIndex`].
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LengthBucket {
    /// Byte length shared by every entry in this bucket.
    pub key_len: u8,
    /// Detected keys (and the dispatch branch each maps to). Each
    /// branch may have multiple keys (e.g. the CSS `border-color`
    /// branch covers `color`, `background-color`, …); each key gets
    /// one entry, so the same `branch_idx` can appear multiple
    /// times.
    pub entries: Vec<KeyEntry>,
    /// Probe form selected at codegen time.
    pub probe: BucketProbe,
}

/// Single key entry inside a length bucket.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KeyEntry {
    /// Key bytes (length matches the parent `LengthBucket::key_len`).
    pub key_bytes: Vec<u8>,
    /// Dispatch branch index inside the parent Alt.
    pub branch_idx: u8,
}

/// Codegen probe shape selected per bucket.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BucketProbe {
    /// Linear sweep — emit one comparison per entry. Best for ≤ 4
    /// entries, or for > 4 entries when first-byte collisions force
    /// a sequential scan.
    Linear,
    /// First-byte perfect hash — codegen emits a 256-entry table
    /// keyed by `key_bytes[0]`. Each cell stores the index of the
    /// matching entry inside `entries`, or 255 (sentinel) for "no
    /// candidate". Codegen looks up the cell, then performs a
    /// single equality check against the candidate. Sentinel `255`
    /// is safe because the length bucket caps at 64 entries.
    FirstByteTable {
        /// 256-byte lookup table; `cells[b] == 255` means no match.
        cells: Vec<u8>,
    },
}

impl KeyIndex {
    /// Total entry count across every bucket.
    pub fn total_entries(&self) -> usize {
        self.buckets.iter().map(|b| b.entries.len()).sum()
    }
}
