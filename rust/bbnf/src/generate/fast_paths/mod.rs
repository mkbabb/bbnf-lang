//! Shared regex/scanner pattern detection for IR codegen.
//!
//! Detects well-known regex patterns (JSON string/number, CSS ident/ws/string,
//! negated character classes) and emits optimized parser/span constructors.
//! Used by both `ir_codegen.rs` (Parser output) and `ir_span.rs` (SpanParser output).

mod detect;
mod generalized;
mod negated_class;

pub use negated_class::{NegCharClassQuantifier, is_negated_char_class_regex};

use generalized::emit_generalized_regex_direct;

use detect::*;

use proc_macro2::TokenStream;
use quote::quote;

// ---------------------------------------------------------------------------
// Regex fast-path emission
// ---------------------------------------------------------------------------

/// Emit a Parser<Span> expression for a regex pattern, using fast-paths where available.
pub fn emit_regex_parser(pattern: &str) -> TokenStream {
    if is_json_string_pattern(pattern) {
        return quote! { ::parse_that::sp_json_string_quoted().into_parser() };
    }
    if is_json_number_pattern(pattern) {
        return quote! { ::parse_that::sp_json_number().into_parser() };
    }
    if is_ws_block_comment_pattern(pattern) {
        return quote! { ::parse_that::sp_css_ws_comment().into_parser() };
    }
    if is_ident_pattern(pattern) {
        return quote! { ::parse_that::sp_css_ident().into_parser() };
    }
    if is_quoted_string_pattern(pattern) {
        return quote! { ::parse_that::sp_css_string().into_parser() };
    }
    // Try direct scanner (same strength reduction as monolithic path).
    // Wraps the raw state-manipulation code in a Parser closure.
    if let Some(direct) = emit_regex_direct_call(pattern) {
        return quote! {
            ::parse_that::Parser::new(|state: &mut ::parse_that::ParserState<'a>| #direct)
        };
    }
    if let Some((excluded, quantifier)) = is_negated_char_class_regex(pattern) {
        if quantifier == NegCharClassQuantifier::Plus {
            let excluded_bytes = proc_macro2::Literal::byte_string(excluded.as_bytes());
            return quote! { ::parse_that::take_until_any_span(#excluded_bytes) };
        }
    }
    quote! { ::parse_that::regex_span(#pattern) }
}

/// Emit a SpanParser expression for a regex pattern, using fast-paths where available.
pub fn emit_regex_span(pattern: &str) -> TokenStream {
    if is_json_string_pattern(pattern) {
        return quote! { ::parse_that::sp_json_string_quoted() };
    }
    if is_json_number_pattern(pattern) {
        return quote! { ::parse_that::sp_json_number() };
    }
    if is_ws_block_comment_pattern(pattern) {
        return quote! { ::parse_that::sp_css_ws_comment() };
    }
    if is_ident_pattern(pattern) {
        return quote! { ::parse_that::sp_css_ident() };
    }
    if is_quoted_string_pattern(pattern) {
        return quote! { ::parse_that::sp_css_string() };
    }
    if let Some((excluded, quantifier)) = is_negated_char_class_regex(pattern) {
        let excluded_bytes = proc_macro2::Literal::byte_string(excluded.as_bytes());
        if quantifier == NegCharClassQuantifier::Plus {
            return quote! { ::parse_that::sp_take_until_any(#excluded_bytes) };
        } else {
            return quote! { ::parse_that::sp_take_until_any(#excluded_bytes).opt_span() };
        }
    }
    quote! { ::parse_that::sp_regex(#pattern) }
}

/// Emit a direct scanner function call for known regex patterns, bypassing
/// the SpanParser dispatch stack. Returns `None` for unrecognized patterns.
///
/// Used by monolithic codegen to call `json_string_fast_quoted(state)` etc.
/// directly instead of going through `SpanParser::call → SpanKind match →
/// SpanScanner match → actual_scan_fn`.
/// Emit a direct scanner call, without fused number conversion.
/// Used by span-only codegen where f64 conversion is not needed.
pub fn emit_regex_direct_call(pattern: &str) -> Option<TokenStream> {
    emit_regex_direct_call_with_fuse(pattern, false)
}

/// Emit a direct scanner call with optional fused number conversion.
/// When `fuse_numbers` is true, JSON number regex returns `(Span, f64)`.
/// When false, returns `Span` only.
pub fn emit_regex_direct_call_with_fuse(pattern: &str, fuse_numbers: bool) -> Option<TokenStream> {
    if is_json_string_pattern(pattern) {
        return Some(quote! { ::parse_that::scan_json_string(state) });
    }
    if is_json_number_pattern(pattern) {
        if fuse_numbers {
            return Some(quote! { ::parse_that::scan_number_convert_json(state) });
        } else {
            return Some(quote! { ::parse_that::scan_number_span_json(state) });
        }
    }
    if is_ws_block_comment_pattern(pattern) {
        return Some(quote! { ::parse_that::scan_ws_block_comments(state) });
    }
    if is_ident_pattern(pattern) {
        return Some(quote! { ::parse_that::scan_ident(state) });
    }
    if is_quoted_string_pattern(pattern) {
        return Some(quote! { ::parse_that::scan_string_quoted(state) });
    }

    // Comma-or-whitespace separator: ,|\s+
    if pattern == r",|\s+" || pattern == r"\s+|," {
        return Some(quote! {
            {
                let __start = state.offset;
                if __start < state.src_bytes.len() {
                    if unsafe { *state.src_bytes.get_unchecked(__start) } == b',' {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        let mut __pos = __start;
                        while __pos < state.src_bytes.len()
                            && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                        {
                            __pos += 1;
                        }
                        if __pos > __start {
                            state.offset = __pos;
                            Some(::parse_that::Span::new(__start, __pos, state.src))
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            }
        });
    }

    // Structural classification: detect numeric/string/hex/identifier patterns
    // without requiring exact string matches against pattern lists.
    use super::regex_classify::{classify_regex, RegexClass};
    match classify_regex(pattern) {
        RegexClass::Numeric { allows_sign, .. } => {
            // Only use the number fast path for patterns WITHOUT sign
            // (unsigned integers). CSS number patterns with [-+]? have
            // edge cases with exponent-like suffixes (e.g., 0.375em where
            // 'e' is part of the unit 'em', not an exponent indicator).
            if !allows_sign {
                return Some(quote! { ::parse_that::scan_number_span_json(state) });
            }
        }
        RegexClass::Identifier => {
            return Some(quote! { ::parse_that::scan_ident(state) });
        }
        // QuotedString and HexDigits: handled by existing negated-class / char-class paths below.
        _ => {}
    }

    // Generalized regex patterns (char ranges, small char sets).
    if let Some(ts) = emit_generalized_regex_direct(pattern) {
        return Some(ts);
    }

    // Negated character class → direct memchr call, bypassing SpanParser dispatch.
    if let Some((excluded, quantifier)) = is_negated_char_class_regex(pattern) {
        let bytes = excluded.as_bytes();
        let result = match (bytes.len(), quantifier) {
            (1, NegCharClassQuantifier::Plus) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        if __start >= state.src_bytes.len() { None } else {
                            let __scan = ::parse_that::memchr::memchr(#b0, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start);
                            if __scan == 0 { None } else {
                                state.offset = __start + __scan;
                                Some(::parse_that::Span::new(__start, state.offset, state.src))
                            }
                        }
                    }
                })
            }
            (2, NegCharClassQuantifier::Plus) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                let b1 = proc_macro2::Literal::byte_character(bytes[1]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        if __start >= state.src_bytes.len() { None } else {
                            let __scan = ::parse_that::memchr::memchr2(#b0, #b1, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start);
                            if __scan == 0 { None } else {
                                state.offset = __start + __scan;
                                Some(::parse_that::Span::new(__start, state.offset, state.src))
                            }
                        }
                    }
                })
            }
            (3, NegCharClassQuantifier::Plus) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                let b1 = proc_macro2::Literal::byte_character(bytes[1]);
                let b2 = proc_macro2::Literal::byte_character(bytes[2]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        if __start >= state.src_bytes.len() { None } else {
                            let __scan = ::parse_that::memchr::memchr3(#b0, #b1, #b2, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start);
                            if __scan == 0 { None } else {
                                state.offset = __start + __scan;
                                Some(::parse_that::Span::new(__start, state.offset, state.src))
                            }
                        }
                    }
                })
            }
            (1, NegCharClassQuantifier::Star) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        let __scan = if __start >= state.src_bytes.len() { 0 } else {
                            ::parse_that::memchr::memchr(#b0, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start)
                        };
                        state.offset = __start + __scan;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                })
            }
            (2, NegCharClassQuantifier::Star) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                let b1 = proc_macro2::Literal::byte_character(bytes[1]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        let __scan = if __start >= state.src_bytes.len() { 0 } else {
                            ::parse_that::memchr::memchr2(#b0, #b1, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start)
                        };
                        state.offset = __start + __scan;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                })
            }
            (3, NegCharClassQuantifier::Star) => {
                let b0 = proc_macro2::Literal::byte_character(bytes[0]);
                let b1 = proc_macro2::Literal::byte_character(bytes[1]);
                let b2 = proc_macro2::Literal::byte_character(bytes[2]);
                Some(quote! {
                    {
                        let __start = state.offset;
                        let __scan = if __start >= state.src_bytes.len() { 0 } else {
                            ::parse_that::memchr::memchr3(#b0, #b1, #b2, &state.src_bytes[__start..])
                                .unwrap_or(state.src_bytes.len() - __start)
                        };
                        state.offset = __start + __scan;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                })
            }
            _ => None,
        };
        if result.is_some() {
            return result;
        }
    }

    None
}

/// Check if a regex pattern returns a fused `(Span, f64)` instead of plain `Span`.
/// Used by type inference to determine the correct enum variant type.
pub fn is_fused_number_regex(pattern: &str) -> bool {
    is_json_number_pattern(pattern)
}
