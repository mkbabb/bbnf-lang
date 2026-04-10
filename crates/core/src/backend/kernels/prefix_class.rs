//! Literal-prefix-then-class kernel emission — Tranche X.9a.
//!
//! Targets `memcmp` + a tail dispatch into another class-scan helper.
//! Used by `RegexClass::PrefixThenClass` (and `RegexClass::AccelDriven`
//! when the accel driver reduces to a prefix + tail-class shape).
//!
//! Three shapes are recognized, keyed by the tail `CharSet128`:
//!
//! - `alnum` tail (`[a-zA-Z0-9]+`)           → `scan_alnum_mut`
//! - `digits` tail (`[0-9]+`)                → `scan_digits_mut`
//! - `hex` tail (`[0-9a-fA-F]+`)             → `scan_hex_mut`
//!
//! Any other tail shape returns `None` so the generalized emitter keeps
//! handling it inline.

use bbnf_ir::CharSet128;
use proc_macro2::TokenStream;
use quote::quote;

/// Tail class shape for prefix-class routing.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TailShape {
    Alnum,
    Digits,
    Hex,
    Unrecognized,
}

/// Classify the tail `CharSet128` into a known shape. Must exactly match
/// the canonical byte set — heterogeneous / negated / irregular tails
/// fall through to `Unrecognized`, which the caller handles by bailing
/// to the inline emitter.
fn tail_class_shape(tail_class: &CharSet128) -> TailShape {
    if matches_set(tail_class, &DIGITS) {
        return TailShape::Digits;
    }
    if matches_set(tail_class, &ALNUM) {
        return TailShape::Alnum;
    }
    if matches_set(tail_class, &HEX) {
        return TailShape::Hex;
    }
    TailShape::Unrecognized
}

/// Emit a literal-prefix memcmp followed by a class-tail scan.
///
/// Returns `Some(expr)` iff the tail `CharSet128` matches one of the
/// three hoisted helper shapes; `None` otherwise. The returned token
/// stream is an **expression** of type `Option<Span<'a>>`:
///
/// ```ignore
/// {
///     let __start = state.offset;
///     if state.src_bytes.get(__start..__start + N) == Some(b"<prefix>" as &[u8]) {
///         state.offset = __start + N;
///         match ::parse_that::scan_<tail>_mut(state) {
///             Some(_) => Some(::parse_that::Span::new(__start, state.offset, state.src)),
///             None => { state.offset = __start; None }
///         }
///     } else {
///         None
///     }
/// }
/// ```
///
/// On tail failure the offset is rolled back so the caller observes a
/// clean `None` without partial advancement. On success the returned
/// `Span` covers the prefix plus the scanned tail.
pub fn emit_call_opt(prefix: &[u8], tail_class: &CharSet128) -> Option<TokenStream> {
    if prefix.is_empty() {
        return None;
    }
    let tail_scan = match tail_class_shape(tail_class) {
        TailShape::Alnum => quote! { ::parse_that::scan_alnum_mut(state) },
        TailShape::Digits => quote! { ::parse_that::scan_digits_mut(state) },
        TailShape::Hex => quote! { ::parse_that::scan_hex_mut(state) },
        TailShape::Unrecognized => return None,
    };

    let prefix_lit = proc_macro2::Literal::byte_string(prefix);
    let prefix_len = prefix.len();
    let prefix_len_lit = proc_macro2::Literal::usize_unsuffixed(prefix_len);

    Some(quote! {
        {
            let __start = state.offset;
            if state.src_bytes.get(__start..__start + #prefix_len_lit)
                == Some(#prefix_lit as &[u8])
            {
                state.offset = __start + #prefix_len_lit;
                match #tail_scan {
                    Some(_) => Some(::parse_that::Span::new(__start, state.offset, state.src)),
                    None => {
                        state.offset = __start;
                        None
                    }
                }
            } else {
                None
            }
        }
    })
}

/// Legacy stable-surface wrapper retained for the V.10 consumer-invariant
/// grep test. Falls back to the unconditional-`scan_ident` form when the
/// tail shape isn't recognized — callers that need actual routing should
/// use `emit_call_opt`.
pub fn emit_call(prefix: &[u8], tail_class: &CharSet128) -> TokenStream {
    emit_call_opt(prefix, tail_class)
        .unwrap_or_else(|| quote! { ::parse_that::scan_ident(state) })
}

// ── Canonical tail charsets ────────────────────────────────────────────

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
