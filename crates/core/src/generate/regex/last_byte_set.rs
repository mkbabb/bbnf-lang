//! BoundedRegex `last_byte_set` narrowing — fast-path skip before
//! engaging the DFA walker.
//!
//! AY.W4.3 (BoundedRegex absorbed from AX.W6 per A7).
//!
//! # Mechanism
//!
//! Each regex has a derivable LAST-byte set: the set of bytes the
//! pattern can end on. When the adapter is invoked at offset `pos`
//! and the byte at `pos` is NOT in the pattern's FIRST set (already
//! handled by [`crate::generate::regex::byte_class`]) — and ALSO
//! when the input length is exactly 0 or `pos >= input.len()` —
//! the DFA walk can be skipped entirely.
//!
//! A complementary narrowing fires for non-anchored patterns whose
//! match must include a fixed terminator: if the input slice from
//! `pos` does not contain any byte from the LAST set, the regex
//! cannot complete a match, and the adapter can short-circuit to
//! `None`.
//!
//! # Fit
//!
//! Patterns with regex literal suffixes (e.g. CSS L4's
//! `(\\.|[^\"\\\\])*"` ends on `"`) and selector regex chains have
//! computable LAST sets via the existing
//! [`bbnf_regex::RegexInfo::literal_suffix`] field.

use parse_that::regex::RegexInfo;
use parse_that::regex::sets::charset::CharSet128;
use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Mine the LAST-byte set from a regex pattern.
///
/// Strategy:
///
/// 1. If `RegexInfo::literal_suffix` resolves, the suffix's last
///    byte is the sole admissible LAST byte (deterministic).
/// 2. Otherwise, return `None` — no narrowing (the pattern's LAST
///    set spans too many bytes to discriminate).
///
/// Conservative — the narrowing fires only for patterns the
/// existing analysis can certify a deterministic suffix on.
pub fn mine_last_byte_set(pattern: &str) -> Option<CharSet128> {
    let info = RegexInfo::analyze(pattern)?;
    let suffix = info.literal_suffix?;
    let &last = suffix.last()?;
    let mut cs = CharSet128::new();
    cs.add(last);
    Some(cs)
}

/// Emit a per-pattern LAST-set bitmap (one `u128` literal) when
/// the suffix is deterministic. Used by the adapter to fast-path
/// skip the DFA walk when the input does not contain the suffix
/// byte beyond `pos`.
///
/// Returns `None` when no narrowing applies.
pub fn emit_last_byte_lit(pattern: &str) -> Option<TokenStream> {
    let cs = mine_last_byte_set(pattern)?;

    // CharSet128 is a 128-bit ASCII bitset; export the raw u128
    // literal so the consumer can match against it cheaply.
    // We use the public bits() API.
    let bits_lo = cs.bits_lo();
    let bits_hi = cs.bits_hi();
    let lo_lit = Literal::u64_unsuffixed(bits_lo);
    let hi_lit = Literal::u64_unsuffixed(bits_hi);

    Some(quote! {
        // (lo, hi) packed CharSet128 — the LAST-byte set this regex
        // can terminate on. Used by the adapter's narrowing
        // fast-path.
        (#lo_lit, #hi_lit)
    })
}

/// Emit a const slice carrying every adapter-collected pattern's
/// LAST-set tuple — `[(lo, hi); N]`. Patterns without a deterministic
/// suffix are emitted as `(0, 0)` (empty set → narrowing disabled
/// for that index).
pub fn emit_last_byte_set_table(
    table_ident: &proc_macro2::Ident,
    patterns: &[&str],
) -> TokenStream {
    let entries: Vec<TokenStream> = patterns
        .iter()
        .map(|pat| emit_last_byte_lit(pat).unwrap_or_else(|| quote! { (0, 0) }))
        .collect();

    let n = patterns.len();
    let n_lit = Literal::usize_unsuffixed(n);

    quote! {
        /// AY.W4.3 — per-pattern (LAST-byte-set lo, hi) packed
        /// `CharSet128` tuples. `(0, 0)` means narrowing is
        /// disabled for that pattern (suffix not deterministic).
        ///
        /// The adapter consults this when invoked: if the pattern's
        /// entry is non-zero AND the input slice from `pos` does not
        /// contain any byte in the LAST set, the regex cannot
        /// complete a match — skip the DFA walk entirely.
        #[allow(dead_code)]
        pub(crate) const #table_ident: [(u64, u64); #n_lit] = [#(#entries),*];
    }
}

/// Helper extension on `CharSet128` exposing the internal bit pair
/// for emission. The bbnf-regex crate stores the set as two u64s
/// (lo: bytes 0..64, hi: bytes 64..128); we expose them here to
/// avoid round-tripping through the public iterator.
trait CharSet128Bits {
    fn bits_lo(&self) -> u64;
    fn bits_hi(&self) -> u64;
}

impl CharSet128Bits for CharSet128 {
    fn bits_lo(&self) -> u64 {
        // CharSet128 doesn't expose raw bits; reconstruct by
        // iterating membership. ASCII range is 0..128 so we cover
        // the full set.
        let mut lo = 0u64;
        for b in 0u8..64 {
            if self.has(b) {
                lo |= 1u64 << b;
            }
        }
        lo
    }
    fn bits_hi(&self) -> u64 {
        let mut hi = 0u64;
        for b in 64u8..128 {
            if self.has(b) {
                hi |= 1u64 << (b - 64);
            }
        }
        hi
    }
}
