//! Shared regex/scanner pattern detection for IR codegen.
//!
//! Detects well-known regex patterns (JSON string/number, CSS ident/ws/string,
//! negated character classes) and emits optimized parser/span constructors.
//! Used by the codegen module for both arena and span output paths.

mod generalized;
mod inline_scanners;
mod negated_class;

pub use negated_class::{NegCharClassQuantifier, is_negated_char_class_regex};

use generalized::emit_generalized_regex_direct;

use crate::generate::regex_ir::classify::{classify_regex, RegexClass};

use proc_macro2::TokenStream;
use quote::quote;

// ---------------------------------------------------------------------------
// Regex fast-path emission
// ---------------------------------------------------------------------------

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
    match classify_regex(pattern) {
        RegexClass::JsonString => {
            return Some(quote! { ::parse_that::scan_json_string(state) });
        }
        RegexClass::JsonNumber => {
            if fuse_numbers {
                return Some(quote! { ::parse_that::scan_number_convert_json(state) });
            } else {
                return Some(quote! { ::parse_that::scan_number_span_json(state) });
            }
        }
        RegexClass::WsBlockComment => {
            return Some(inline_scanners::emit_inline_ws_comment_scanner());
        }
        RegexClass::CssIdent => {
            return Some(inline_scanners::emit_inline_ident_scanner());
        }
        RegexClass::CssQuotedString => {
            return Some(inline_scanners::emit_inline_string_scanner());
        }
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
            return Some(inline_scanners::emit_inline_ident_scanner());
        }
        // QuotedString and HexDigits: handled by existing negated-class / char-class paths below.
        _ => {}
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
    matches!(classify_regex(pattern), RegexClass::JsonNumber)
}
