//! Byte-class pre-filter dispatcher emission.
//!
//! AY.W4.3 — for grammars whose `__regex_scan_<grammar>` adapter
//! collects multiple regex patterns with disjoint first-byte FIRST
//! sets, classify the input byte once at adapter entry and route to
//! the matching pattern's DFA body without walking the full
//! pointer-equality dispatch chain. Saves the cold-branch tax on
//! cross-pattern dispatch.
//!
//! # Mechanism
//!
//! Each pattern's [`bbnf_regex::regex_first_chars`] yields the set of
//! ASCII bytes that can begin a match. When the union of FIRST sets
//! across all adapter-dispatched patterns has a sparse mapping
//! (`byte → Vec<pattern_idx>`), we emit a `match input[pos]` ladder
//! before the existing pointer-equality cascade. Patterns whose FIRST
//! set the byte does NOT belong to are skipped at the head — the
//! cascade walks only the patterns that could conceivably match.
//!
//! # When this fires
//!
//! - At least 2 adapter-collected patterns whose FIRST sets are
//!   computable (not `None`/wildcards).
//! - Total distinct first bytes ≤ 32 — wider sets fall through to the
//!   plain cascade since the LUT becomes sparse.
//!
//! For wider/wildcard cases the dispatcher returns `None` and the
//! adapter emits the legacy pointer-equality cascade only.

use parse_that::regex::regex_first_chars;
use parse_that::regex::sets::charset::CharSet128;
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Maximum patterns the byte-class dispatcher will consider.
///
/// Above this count the LUT density and the per-byte arm
/// cardinality drown the pointer-equality cascade's overhead. The
/// adapter falls back to the cascade when a grammar exceeds this
/// count, but the limit is generous — every shipped grammar has
/// fewer than 32 unique adapter-dispatched patterns.
const MAX_DISPATCHED: usize = 32;

/// One adapter pattern's first-byte coverage.
pub struct PatternFirstBytes {
    /// Pattern index in the adapter's collected set (0-based).
    pub pattern_idx: usize,
    /// First-byte bitmap; `None` when the pattern admits any byte
    /// (wildcard-led) and cannot be filtered.
    pub first_bytes: Option<CharSet128>,
}

impl PatternFirstBytes {
    /// Mine the FIRST set from the regex pattern string.
    pub fn from_pattern(pattern_idx: usize, pattern: &str) -> Self {
        Self {
            pattern_idx,
            first_bytes: regex_first_chars(pattern),
        }
    }
}

/// Build a 256-byte byte → `&[u8]` mapping where each entry holds
/// the indices of patterns whose FIRST set includes that byte.
///
/// Returns `None` when fewer than 2 patterns admit a non-wildcard
/// FIRST set, or when any pattern is wildcard-led (since the
/// pre-filter would have to admit every pattern for those bytes,
/// negating the benefit).
fn build_byte_to_patterns(
    patterns: &[PatternFirstBytes],
) -> Option<[Vec<u8>; 256]> {
    if patterns.len() < 2 || patterns.len() > MAX_DISPATCHED {
        return None;
    }

    // Wildcards short-circuit — every byte routes to those patterns
    // anyway, so the pre-filter saves nothing.
    if patterns.iter().any(|p| p.first_bytes.is_none()) {
        return None;
    }

    let mut byte_to_pats: [Vec<u8>; 256] =
        std::array::from_fn(|_| Vec::new());

    for p in patterns {
        let bytes = p.first_bytes.as_ref().unwrap();
        for b in 0u8..=127 {
            if bytes.has(b) {
                byte_to_pats[b as usize].push(p.pattern_idx as u8);
            }
        }
    }

    // If any byte routes to ALL patterns, the dispatcher saves
    // nothing for that byte. Allow up to all-patterns coverage —
    // the dispatcher still benefits when bytes route to a
    // strict subset.
    Some(byte_to_pats)
}

/// Emit the per-byte LUT carrying, for each input byte, the bitmap
/// of admissible pattern indices. The bitmap is u32-wide; combined
/// with the [`MAX_DISPATCHED`] cap (32 patterns) this fits one
/// scalar load per dispatch.
///
/// Returns the LUT identifier and the const decl. The adapter
/// emits this at module scope and reads it at adapter entry.
pub fn emit_byte_class_lut(
    lut_ident: &proc_macro2::Ident,
    patterns: &[PatternFirstBytes],
) -> Option<TokenStream> {
    let byte_to_pats = build_byte_to_patterns(patterns)?;

    // Pack each byte's pattern set into a u32 bitmap.
    let mut bits: [u32; 256] = [0; 256];
    for b in 0..256 {
        for &pidx in &byte_to_pats[b] {
            bits[b] |= 1u32 << pidx;
        }
    }

    let bit_lits: Vec<Literal> =
        bits.iter().map(|w| Literal::u32_unsuffixed(*w)).collect();

    Some(quote! {
        /// AY.W4.3 — first-byte → admissible-pattern bitmap LUT.
        ///
        /// Each entry holds a u32 bitmap; bit `i` set means pattern
        /// `i` (in the adapter's collected order) admits this byte
        /// as a match-prefix. Read once at adapter entry; the
        /// dispatch cascade visits only patterns whose bit is set.
        #[allow(dead_code)]
        pub(crate) const #lut_ident: [u32; 256] = [#(#bit_lits),*];
    })
}

/// Whether the pattern set is dispatchable (≥ 2 patterns, all
/// FIRST-set computable). Used by the adapter to decide whether to
/// emit the pre-filter or skip directly to the cascade.
pub fn is_dispatchable(patterns: &[PatternFirstBytes]) -> bool {
    patterns.len() >= 2
        && patterns.len() <= MAX_DISPATCHED
        && patterns.iter().all(|p| p.first_bytes.is_some())
}
