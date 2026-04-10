//! JSON / dictionary structural punctuation+whitespace kernel emission
//! (Tranche X.11b).
//!
//! Emits a fused scanner that consumes leading whitespace + a cluster
//! of structural punctuation bytes + trailing whitespace in a single
//! pass. Replaces the `ws >> p >> ws` shape with a SIMD-friendly
//! byte-scanning loop that avoids the combinator-call overhead of
//! `parse_ws_then_lit_then_ws`.
//!
//! The kernel handles single-byte punctuation clusters (the common
//! JSON `,`, `:`, `{`, `}`, `[`, `]` shape). Multi-byte clusters are
//! also supported via a sequential `all` check against the provided
//! byte slice.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a ws-padded punctuation scanner.
///
/// For a single-byte cluster (the common case), emits a fused scanner
/// of the form:
///
/// ```ignore
/// {
///     let __start = state.offset;
///     let __bytes = state.src_bytes;
///     let __end = __bytes.len();
///     let mut __pos = __start;
///     // Leading whitespace.
///     while __pos < __end
///         && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
///     { __pos += 1; }
///     // Punctuation check.
///     if __pos < __end && unsafe { *__bytes.get_unchecked(__pos) } == #p {
///         __pos += 1;
///         // Trailing whitespace.
///         while __pos < __end
///             && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
///         { __pos += 1; }
///         state.offset = __pos;
///         Some(::parse_that::Span::new(__start, __pos, state.src))
///     } else {
///         None
///     }
/// }
/// ```
///
/// Multi-byte clusters fall back to a sequential byte-by-byte match
/// that checks each expected punctuation in order.
pub fn emit_call(puncts: &[u8]) -> TokenStream {
    assert!(!puncts.is_empty(), "punct_ws_region requires at least one byte");

    if puncts.len() == 1 {
        let p = puncts[0];
        let p_lit = proc_macro2::Literal::byte_character(p);
        return quote! {
            {
                let __start = state.offset;
                let __bytes = state.src_bytes;
                let __end = __bytes.len();
                let mut __pos = __start;
                while __pos < __end
                    && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                {
                    __pos += 1;
                }
                if __pos < __end && unsafe { *__bytes.get_unchecked(__pos) } == #p_lit {
                    __pos += 1;
                    while __pos < __end
                        && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                    {
                        __pos += 1;
                    }
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        };
    }

    // Multi-byte cluster: sequential match against each byte with
    // intervening whitespace segments.
    let checks: Vec<TokenStream> = puncts
        .iter()
        .map(|&b| {
            let lit = proc_macro2::Literal::byte_character(b);
            quote! {
                while __pos < __end
                    && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                {
                    __pos += 1;
                }
                if __pos >= __end || unsafe { *__bytes.get_unchecked(__pos) } != #lit {
                    state.offset = __start;
                    return None;
                }
                __pos += 1;
            }
        })
        .collect();

    quote! {
        (|| {
            let __start = state.offset;
            let __bytes = state.src_bytes;
            let __end = __bytes.len();
            let mut __pos = __start;
            #(#checks)*
            while __pos < __end
                && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
            {
                __pos += 1;
            }
            state.offset = __pos;
            Some(::parse_that::Span::new(__start, __pos, state.src))
        })()
    }
}
