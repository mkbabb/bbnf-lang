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
//!
//! Tranche AF.1 Wave 3E: the tail-shape classification helpers
//! (`matches_set` + canonical byte set constants) moved to the shared
//! `kernels::charset_shapes` module — the same classifier is used by
//! `kernels::charclass`, and keeping one copy prevents the two
//! kernels from drifting on the canonical shape definitions. The
//! legacy `emit_call` wrapper that fell back to `scan_ident` on
//! shape miss was deleted — zero production callers, unreachable
//! fallback.

use bbnf_ir::CharSet128;
use proc_macro2::TokenStream;
use quote::quote;

use super::charset_shapes::{ALNUM, DIGITS, HEX, matches_set};

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
