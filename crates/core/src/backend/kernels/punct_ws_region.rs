//! Structural punctuation+whitespace kernel emission
//! (Tranche X.11b, AP.ws universality fix).
//!
//! Emits a fused scanner that consumes leading whitespace + a cluster
//! of structural punctuation bytes + trailing whitespace in a single
//! pass. Replaces the `ws >> p >> ws` shape with a SIMD-friendly
//! byte-scanning loop that avoids the combinator-call overhead of
//! `parse_ws_then_lit_then_ws`.
//!
//! The kernel handles single-byte punctuation clusters (the common
//! `,`, `:`, `{`, `}`, `[`, `]` shapes seen in dictionary-shaped
//! grammars). Multi-byte clusters are also supported via a sequential
//! `all` check against the provided byte slice.
//!
//! When a custom `@ws` pattern is active and classifies as
//! `WhitespaceWithBlockComment`, whitespace segments call
//! `::parse_that::scan_ws_block_comments(state)` instead of bare
//! `is_ascii_whitespace` loops, ensuring CSS block comments are
//! consumed at every whitespace position.

use proc_macro2::TokenStream;
use quote::quote;

/// Whether the custom `@ws` pattern requires the comment-aware
/// whitespace kernel instead of bare `is_ascii_whitespace`.
fn is_comment_aware_ws(ws_pattern: Option<&str>) -> bool {
    use parse_that::regex::classify::{RegexClass, classify_regex};
    matches!(
        ws_pattern.map(classify_regex),
        Some(RegexClass::WhitespaceWithBlockComment),
    )
}

/// Emit a whitespace-skip segment. When `comment_aware` is true,
/// emits `scan_ws_block_comments` via state.offset; otherwise emits
/// the original `is_ascii_whitespace` byte loop via `__pos`.
fn emit_ws_skip(comment_aware: bool) -> TokenStream {
    if comment_aware {
        // Sync __pos → state.offset, call the kernel (which advances
        // state.offset past whitespace + block comments), then read
        // the new position back into __pos.
        quote! {
            state.offset = __pos;
            ::parse_that::scan_ws_block_comments(state);
            __pos = state.offset;
        }
    } else {
        quote! {
            while __pos < __end
                && unsafe { *__bytes.get_unchecked(__pos) }.is_ascii_whitespace()
            {
                __pos += 1;
            }
        }
    }
}

/// Emit a ws-padded punctuation scanner.
///
/// `ws_pattern` is the grammar's `@ws` regex (if any). When it
/// classifies as `WhitespaceWithBlockComment`, every whitespace
/// segment uses the comment-aware kernel; otherwise, falls back to
/// `is_ascii_whitespace` byte loops.
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
///     // Leading whitespace (comment-aware or bare).
///     <ws_skip>
///     // Punctuation check.
///     if __pos < __end && unsafe { *__bytes.get_unchecked(__pos) } == #p {
///         __pos += 1;
///         // Trailing whitespace (comment-aware or bare).
///         <ws_skip>
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
pub fn emit_call(puncts: &[u8], ws_pattern: Option<&str>) -> TokenStream {
    assert!(!puncts.is_empty(), "punct_ws_region requires at least one byte");

    let comment_aware = is_comment_aware_ws(ws_pattern);
    let ws_leading = emit_ws_skip(comment_aware);
    let ws_trailing = emit_ws_skip(comment_aware);

    if puncts.len() == 1 {
        let p = puncts[0];
        let p_lit = proc_macro2::Literal::byte_character(p);
        return quote! {
            {
                let __start = state.offset;
                let __bytes = state.src_bytes;
                let __end = __bytes.len();
                let mut __pos = __start;
                #ws_leading
                if __pos < __end && unsafe { *__bytes.get_unchecked(__pos) } == #p_lit {
                    __pos += 1;
                    #ws_trailing
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
            let ws_seg = emit_ws_skip(comment_aware);
            let lit = proc_macro2::Literal::byte_character(b);
            quote! {
                #ws_seg
                if __pos >= __end || unsafe { *__bytes.get_unchecked(__pos) } != #lit {
                    state.offset = __start;
                    return None;
                }
                __pos += 1;
            }
        })
        .collect();

    let ws_final = emit_ws_skip(comment_aware);

    quote! {
        (|| {
            let __start = state.offset;
            let __bytes = state.src_bytes;
            let __end = __bytes.len();
            let mut __pos = __start;
            #(#checks)*
            #ws_final
            state.offset = __pos;
            Some(::parse_that::Span::new(__start, __pos, state.src))
        })()
    }
}
