//! Char-class quantified kernel emission — Tranche W phase 5d.
//!
//! Maps a `CharSet128` + quantifier bounds onto the appropriate hoisted
//! helper from `parse_that::parsers::scan::digits`. Recognizes the
//! common shapes the cargo-expand audit flagged as duplicated 86 times
//! in the CSS L4 generated parser:
//!
//! - `[0-9]+` → `scan_digits_mut`
//! - `[0-9]*` → `scan_digits_star_mut`
//! - `[a-zA-Z0-9]+` → `scan_alnum_mut`
//! - `[0-9a-fA-F]+` → `scan_hex_mut`
//!
//! Other shapes return `None` so the existing `generalized/` emitter
//! continues to handle them inline.

use bbnf_ir::CharSet128;
use proc_macro2::TokenStream;
use quote::quote;

/// Try to map `(chars, negated, lo, hi)` onto a hoisted scanner.
/// Returns `Some(call)` if a hoisted helper applies; `None` otherwise.
pub fn emit_call_opt(
    chars: &CharSet128,
    negated: bool,
    lo: u32,
    hi: Option<u32>,
) -> Option<TokenStream> {
    if negated {
        return None;
    }
    let unbounded_or_one_plus = lo == 1 && hi.is_none();
    let zero_plus = lo == 0 && hi.is_none();

    if matches_set(chars, &DIGITS) {
        if unbounded_or_one_plus {
            return Some(quote! { ::parse_that::scan_digits_mut(state) });
        }
        if zero_plus {
            return Some(quote! { Some(::parse_that::scan_digits_star_mut(state)) });
        }
    }
    if matches_set(chars, &ALNUM) && unbounded_or_one_plus {
        return Some(quote! { ::parse_that::scan_alnum_mut(state) });
    }
    if matches_set(chars, &HEX) && unbounded_or_one_plus {
        return Some(quote! { ::parse_that::scan_hex_mut(state) });
    }
    None
}

/// Convenience wrapper kept for the V.7 stable surface used by the
/// V.10 consumer-invariant grep test. Falls back to `scan_digits_mut`
/// when the shape isn't recognized.
pub fn emit_call(chars: &CharSet128, negated: bool, lo: u32, hi: Option<u32>) -> TokenStream {
    emit_call_opt(chars, negated, lo, hi)
        .unwrap_or_else(|| quote! { ::parse_that::scan_digits_mut(state) })
}

const DIGITS: [u8; 10] = [b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9'];

const ALNUM: [u8; 62] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E',
    b'F', b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R', b'S', b'T',
    b'U', b'V', b'W', b'X', b'Y', b'Z', b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i',
    b'j', b'k', b'l', b'm', b'n', b'o', b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x',
    b'y', b'z',
];

const HEX: [u8; 22] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e',
    b'f', b'A', b'B', b'C', b'D', b'E', b'F',
];

fn matches_set(chars: &CharSet128, expected: &[u8]) -> bool {
    if chars.len() != expected.len() {
        return false;
    }
    for &b in expected {
        if !chars.has(b) {
            return false;
        }
    }
    true
}
