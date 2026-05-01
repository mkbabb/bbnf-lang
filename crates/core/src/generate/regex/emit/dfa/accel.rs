//! DFA acceleration primitives — byte-class predicates, shorthand
//! recognition, accel-emission shortcuts.
//!
//! - Byte-predicate construction from equivalence classes
//!   (`build_class_predicate`).
//! - Shorthand detection (`\d`, `\w`, `\s`, etc.) for accel-fast-path
//!   admission.
//! - Byte-to-range compression.
//! - `try_emit_accel_expr` / `try_emit_accel_scan` — emit a tight
//!   accel-only loop when the DFA collapses to a single classified
//!   step, bypassing the full state-table emission.
//! - `hash_dfa_structure` — content-hash for DFA-table interning.
//! - SIMD-accelerated self-loop scanning.
//! - Canonical DFA hashing for cross-rule deduplication.

use parse_that::regex::accel::{AccelStrategy, StateAccel};
use parse_that::regex::dfa::Dfa;
use proc_macro2::TokenStream;
use quote::quote;

/// Build a boolean predicate for a set of equivalence classes.
///
/// Expands the classes back to byte ranges and emits `__b` checks.
pub(super) fn build_class_predicate(dfa: &Dfa, classes: &[u8]) -> TokenStream {
    // Collect all bytes belonging to these classes.
    let mut bytes: Vec<u8> = Vec::new();
    for (b, &cls) in dfa.byte_classes.iter().enumerate() {
        if classes.contains(&cls) {
            bytes.push(b as u8);
        }
    }

    if bytes.is_empty() {
        return quote! { false };
    }

    // Try to emit efficient predicates.
    // Check for known shorthand patterns.
    if let Some(shorthand) = detect_shorthand(&bytes) {
        return shorthand;
    }

    // Build ranges for compact emission.
    let ranges = bytes_to_ranges(&bytes);

    if ranges.len() == 1 {
        let (lo, hi) = ranges[0];
        if lo == hi {
            let lit = proc_macro2::Literal::byte_character(lo);
            return quote! { __b == #lit };
        }
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return quote! { __b >= #lo_lit && __b <= #hi_lit };
    }

    let mut conditions: Vec<TokenStream> = Vec::new();
    for (lo, hi) in &ranges {
        if lo == hi {
            let lit = proc_macro2::Literal::byte_character(*lo);
            conditions.push(quote! { __b == #lit });
        } else {
            let lo_lit = proc_macro2::Literal::byte_character(*lo);
            let hi_lit = proc_macro2::Literal::byte_character(*hi);
            conditions.push(quote! { (__b >= #lo_lit && __b <= #hi_lit) });
        }
    }

    quote! { #(#conditions)||* }
}

/// Detect well-known shorthand predicates.
pub(super) fn detect_shorthand(bytes: &[u8]) -> Option<TokenStream> {
    let set: std::collections::HashSet<u8> = bytes.iter().copied().collect();

    // \d = [0-9]
    if set.len() == 10
        && (b'0'..=b'9').all(|b| set.contains(&b))
        && set.iter().all(|b| b.is_ascii_digit())
    {
        return Some(quote! { __b.is_ascii_digit() });
    }

    // \w = [0-9A-Za-z_]
    let word_chars: std::collections::HashSet<u8> = (b'0'..=b'9')
        .chain(b'A'..=b'Z')
        .chain(b'a'..=b'z')
        .chain(std::iter::once(b'_'))
        .collect();
    if set == word_chars {
        return Some(quote! { (__b.is_ascii_alphanumeric() || __b == b'_') });
    }

    // [a-zA-Z]
    let alpha: std::collections::HashSet<u8> = (b'A'..=b'Z').chain(b'a'..=b'z').collect();
    if set == alpha {
        return Some(quote! { __b.is_ascii_alphabetic() });
    }

    // \s = ASCII whitespace
    let ws: std::collections::HashSet<u8> = [b' ', b'\t', b'\n', b'\r', 0x0B, 0x0C]
        .iter()
        .copied()
        .collect();
    if set == ws {
        return Some(quote! { __b.is_ascii_whitespace() });
    }

    // [0-9a-fA-F]
    let hex: std::collections::HashSet<u8> = (b'0'..=b'9')
        .chain(b'A'..=b'F')
        .chain(b'a'..=b'f')
        .collect();
    if set == hex {
        return Some(quote! { __b.is_ascii_hexdigit() });
    }

    None
}

/// Convert a sorted list of bytes to inclusive ranges.
pub(super) fn bytes_to_ranges(bytes: &[u8]) -> Vec<(u8, u8)> {
    if bytes.is_empty() {
        return Vec::new();
    }
    let mut sorted = bytes.to_vec();
    sorted.sort_unstable();
    sorted.dedup();

    let mut ranges = Vec::new();
    let mut start = sorted[0];
    let mut end = sorted[0];

    for &b in &sorted[1..] {
        if b == end + 1 {
            end = b;
        } else {
            ranges.push((start, end));
            start = b;
            end = b;
        }
    }
    ranges.push((start, end));

    ranges
}

/// Hash the DFA's state machine structure (transitions + accept states).
pub(super) fn hash_dfa_structure(dfa: &Dfa) -> u64 {
    let mut hash: u64 = 0xcbf29ce484222325; // FNV-1a offset basis (64-bit)
    let prime: u64 = 0x100000001b3;

    // Hash state count.
    hash ^= dfa.state_count() as u64;
    hash = hash.wrapping_mul(prime);

    // Hash number of byte classes.
    hash ^= dfa.num_classes as u64;
    hash = hash.wrapping_mul(prime);

    // Hash byte class table.
    for &b in &dfa.byte_classes {
        hash ^= b as u64;
        hash = hash.wrapping_mul(prime);
    }

    // Hash each state's transitions and accept flag.
    for state in &dfa.states {
        hash ^= state.is_accept as u64;
        hash = hash.wrapping_mul(prime);
        for &t in &state.transitions {
            hash ^= t as u64;
            hash = hash.wrapping_mul(prime);
        }
    }

    hash
}

/// Emit an expression that scans `haystack` for the first exit byte,
/// returning `Option<usize>` (offset within the haystack slice).
///
/// Returns `None` if the strategy is not accelerable.
pub(super) fn try_emit_accel_expr(accel: &StateAccel) -> Option<TokenStream> {
    match &accel.strategy {
        AccelStrategy::Memchr1(b) => {
            let b_lit = proc_macro2::Literal::byte_character(*b);
            Some(quote! {
                ::parse_that::memchr::memchr(
                    #b_lit,
                    __accel_haystack,
                )
            })
        }
        AccelStrategy::Memchr2(b1, b2) => {
            let b1_lit = proc_macro2::Literal::byte_character(*b1);
            let b2_lit = proc_macro2::Literal::byte_character(*b2);
            Some(quote! {
                ::parse_that::memchr::memchr2(
                    #b1_lit, #b2_lit,
                    __accel_haystack,
                )
            })
        }
        AccelStrategy::Memchr3(b1, b2, b3) => {
            let b1_lit = proc_macro2::Literal::byte_character(*b1);
            let b2_lit = proc_macro2::Literal::byte_character(*b2);
            let b3_lit = proc_macro2::Literal::byte_character(*b3);
            Some(quote! {
                ::parse_that::memchr::memchr3(
                    #b1_lit, #b2_lit, #b3_lit,
                    __accel_haystack,
                )
            })
        }
        AccelStrategy::NibbleLut { lo_lut, hi_lut, .. } => {
            let lo_elems: Vec<proc_macro2::Literal> = lo_lut
                .iter()
                .map(|&b| proc_macro2::Literal::u8_unsuffixed(b))
                .collect();
            let hi_elems: Vec<proc_macro2::Literal> = hi_lut
                .iter()
                .map(|&b| proc_macro2::Literal::u8_unsuffixed(b))
                .collect();
            Some(quote! {
                {
                    static __LO_LUT: [u8; 16] = [#(#lo_elems),*];
                    static __HI_LUT: [u8; 16] = [#(#hi_elems),*];
                    ::parse_that::find_first_of_nibble_lut(
                        __accel_haystack,
                        &__LO_LUT,
                        &__HI_LUT,
                    ).map(|(pos, _)| pos)
                }
            })
        }
        _ => None,
    }
}

/// Emit a statement block that scans from `state.offset` using SIMD
/// acceleration, advancing `state.offset` to the first exit byte (or
/// to `state.src_bytes.len()` if no exit byte is found).
///
/// Used by the simple-loop (two-state) DFA path.
pub(super) fn try_emit_accel_scan(accel: &StateAccel) -> Option<TokenStream> {
    let expr = try_emit_accel_expr(accel)?;
    Some(quote! {
        {
            // AW-IV.W4.2.a — accel scan over the padded slice. memchr
            // / nibble-LUT needles are grammar literals (non-NUL), so
            // the NUL pad cannot produce a false positive; a hit at a
            // padded position is clamped by `__view.len()` below.
            let __view = state.padded();
            let __accel_haystack = &__view.bytes()[state.offset..];
            if let Some(__skip) = #expr {
                let __hit = state.offset + __skip;
                if __hit >= __view.len() {
                    state.offset = __view.len();
                } else {
                    state.offset = __hit;
                }
            } else {
                state.offset = __view.len();
            }
        }
    })
}
