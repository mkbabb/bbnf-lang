//! Shared SIMD emission helpers — memchr, nibble-LUT, find_first_of.
//!
//! Centralizes all SIMD-accelerated byte scanning codegen so that
//! multiple pattern emitters share the same emission primitives.

use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Emit a `memchr::memchr(needle, haystack)` call as a forward scan.
///
/// Returns a `TokenStream` that evaluates to `Option<usize>` — the offset
/// of the first occurrence of `needle` in `&state.src_bytes[start..]`.
pub fn emit_memchr1(needle: u8) -> TokenStream {
    let b0 = Literal::byte_character(needle);
    quote! {
        ::parse_that::memchr::memchr(#b0, &state.src_bytes[__start..])
    }
}

/// Emit a `memchr::memchr2(n1, n2, haystack)` call.
pub fn emit_memchr2(n1: u8, n2: u8) -> TokenStream {
    let b0 = Literal::byte_character(n1);
    let b1 = Literal::byte_character(n2);
    quote! {
        ::parse_that::memchr::memchr2(#b0, #b1, &state.src_bytes[__start..])
    }
}

/// Emit a `memchr::memchr3(n1, n2, n3, haystack)` call.
pub fn emit_memchr3(n1: u8, n2: u8, n3: u8) -> TokenStream {
    let b0 = Literal::byte_character(n1);
    let b1 = Literal::byte_character(n2);
    let b2 = Literal::byte_character(n3);
    quote! {
        ::parse_that::memchr::memchr3(#b0, #b1, #b2, &state.src_bytes[__start..])
    }
}

/// Emit a negated-class scan using memchr (1-3 needles, Plus quantifier).
///
/// Scans forward from `state.offset` until hitting any of the needle bytes.
/// Returns `Option<Span>` — `None` if the first byte matches (Plus requires >=1).
pub fn emit_negated_scan_plus(needles: &[u8]) -> Option<TokenStream> {
    let scan = match needles.len() {
        1 => emit_memchr1(needles[0]),
        2 => emit_memchr2(needles[0], needles[1]),
        3 => emit_memchr3(needles[0], needles[1], needles[2]),
        _ => return None,
    };
    Some(quote! {
        {
            let __start = state.offset;
            if __start >= state.src_bytes.len() { None } else {
                let __scan = (#scan).unwrap_or(state.src_bytes.len() - __start);
                if __scan == 0 { None } else {
                    state.offset = __start + __scan;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
            }
        }
    })
}

/// Emit a negated-class scan using memchr (1-3 needles, Star quantifier).
///
/// Scans forward from `state.offset` until hitting any of the needle bytes.
/// Always succeeds (Star allows zero-length match).
pub fn emit_negated_scan_star(needles: &[u8]) -> Option<TokenStream> {
    let scan = match needles.len() {
        1 => emit_memchr1(needles[0]),
        2 => emit_memchr2(needles[0], needles[1]),
        3 => emit_memchr3(needles[0], needles[1], needles[2]),
        _ => return None,
    };
    Some(quote! {
        {
            let __start = state.offset;
            let __scan = if __start >= state.src_bytes.len() { 0 } else {
                (#scan).unwrap_or(state.src_bytes.len() - __start)
            };
            state.offset = __start + __scan;
            Some(::parse_that::Span::new(__start, state.offset, state.src))
        }
    })
}

/// Compute the nibble-LUT tables for a set of target bytes.
///
/// Returns `(lo_lut, hi_lut)` where each is a `[u8; 16]` lookup table.
/// A byte `b` matches if `lo_lut[b & 0x0F] & hi_lut[b >> 4] != 0`.
pub fn build_nibble_luts(targets: &[u8]) -> ([u8; 16], [u8; 16]) {
    let mut lo_lut = [0u8; 16];
    let mut hi_lut = [0u8; 16];

    for (i, &b) in targets.iter().enumerate() {
        if i >= 8 {
            break; // nibble-LUT supports max 8 targets
        }
        let bit = 1u8 << i;
        lo_lut[(b & 0x0F) as usize] |= bit;
        hi_lut[(b >> 4) as usize] |= bit;
    }

    (lo_lut, hi_lut)
}

/// Emit a nibble-LUT scan for 4-8 target bytes.
///
/// Generates static LUT tables and calls `find_first_of_nibble_lut`.
/// Returns `Option<TokenStream>` — `None` if targets exceed 8.
pub fn emit_nibble_lut_scan(targets: &[u8]) -> Option<TokenStream> {
    if targets.len() > 8 || targets.is_empty() {
        return None;
    }

    let (lo, hi) = build_nibble_luts(targets);
    let lo_bytes: Vec<Literal> = lo.iter().map(|b| Literal::u8_unsuffixed(*b)).collect();
    let hi_bytes: Vec<Literal> = hi.iter().map(|b| Literal::u8_unsuffixed(*b)).collect();

    Some(quote! {
        {
            static __LO_LUT: [u8; 16] = [#(#lo_bytes),*];
            static __HI_LUT: [u8; 16] = [#(#hi_bytes),*];
            ::parse_that::find_first_of_nibble_lut(
                &state.src_bytes[__start..],
                &__LO_LUT,
                &__HI_LUT,
            )
        }
    })
}

/// Compute the set of ASCII bytes NOT in the given inclusive ranges.
///
/// Used to convert a positive character class into its excluded-byte set
/// for memchr/nibble-LUT scanning.
pub fn compute_excluded_bytes(included_ranges: &[(u8, u8)]) -> Vec<u8> {
    let mut included = [false; 128];
    for &(lo, hi) in included_ranges {
        for b in lo..=hi.min(127) {
            included[b as usize] = true;
        }
    }
    (0u8..128).filter(|b| !included[*b as usize]).collect()
}
