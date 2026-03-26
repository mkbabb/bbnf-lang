//! Shared regex/scanner pattern detection for IR codegen.
//!
//! Detects well-known regex patterns (JSON string/number, CSS ident/ws/string,
//! negated character classes) and emits optimized parser/span constructors.
//! Used by both `ir_codegen.rs` (Parser output) and `ir_span.rs` (SpanParser output).

use proc_macro2::TokenStream;
use quote::quote;

// ---------------------------------------------------------------------------
// Regex fast-path emission
// ---------------------------------------------------------------------------

/// Emit a Parser<Span> expression for a regex pattern, using fast-paths where available.
pub fn emit_regex_parser(pattern: &str) -> TokenStream {
    if is_json_string_regex(pattern) {
        return quote! { ::parse_that::sp_json_string_quoted().into_parser() };
    }
    if is_json_number_regex(pattern) {
        return quote! { ::parse_that::sp_json_number().into_parser() };
    }
    if is_css_ws_comment_regex(pattern) {
        return quote! { ::parse_that::sp_css_ws_comment().into_parser() };
    }
    if is_css_ident_regex(pattern) {
        return quote! { ::parse_that::sp_css_ident().into_parser() };
    }
    if is_css_string_regex(pattern) {
        return quote! { ::parse_that::sp_css_string().into_parser() };
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
    if is_json_string_regex(pattern) {
        return quote! { ::parse_that::sp_json_string_quoted() };
    }
    if is_json_number_regex(pattern) {
        return quote! { ::parse_that::sp_json_number() };
    }
    if is_css_ws_comment_regex(pattern) {
        return quote! { ::parse_that::sp_css_ws_comment() };
    }
    if is_css_ident_regex(pattern) {
        return quote! { ::parse_that::sp_css_ident() };
    }
    if is_css_string_regex(pattern) {
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
pub fn emit_regex_direct_call(pattern: &str) -> Option<TokenStream> {
    if is_json_string_regex(pattern) {
        return Some(quote! { ::parse_that::json_string_fast_quoted(state) });
    }
    if is_json_number_regex(pattern) {
        return Some(quote! { ::parse_that::number_span_fast(state) });
    }
    if is_css_ws_comment_regex(pattern) {
        return Some(quote! { ::parse_that::css_ws_comment_fast(state) });
    }
    if is_css_ident_regex(pattern) {
        return Some(quote! { ::parse_that::css_ident_fast(state) });
    }
    if is_css_string_regex(pattern) {
        return Some(quote! { ::parse_that::css_string_fast(state) });
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

// ---------------------------------------------------------------------------
// Regex pattern detection
// ---------------------------------------------------------------------------

/// Known exact JSON string regex patterns that can be replaced with the
/// `sp_json_string_quoted()` SIMD fast-path.
const JSON_STRING_REGEX_PATTERNS: &[&str] = &[
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9A-Fa-f]{4}))*""#,
    r#""(?:[^"\\]|\\(?:["\\bfnrt]|u[0-9a-fA-F]{4}))*""#,
];

/// Known exact JSON number regex patterns that can be replaced with the
/// `sp_json_number()` monolithic byte-loop fast-path.
const JSON_NUMBER_REGEX_PATTERNS: &[&str] = &[
    r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
    r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?",
];

/// Detect the canonical JSON string regex pattern.
fn is_json_string_regex(pattern: &str) -> bool {
    JSON_STRING_REGEX_PATTERNS.contains(&pattern)
}

/// Detect the canonical JSON number regex.
fn is_json_number_regex(pattern: &str) -> bool {
    JSON_NUMBER_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS whitespace+comment regex patterns.
const CSS_WS_COMMENT_REGEX_PATTERNS: &[&str] =
    &[r"(?s)(?:\s|/\*.*?\*/)*", r"(?s)(?:\s|\/\*.*?\*\/)*"];

/// Detect the canonical CSS whitespace+comment regex.
fn is_css_ws_comment_regex(pattern: &str) -> bool {
    CSS_WS_COMMENT_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS identifier regex patterns.
const CSS_IDENT_REGEX_PATTERNS: &[&str] = &[
    r"[\-]?[a-zA-Z_][\w-]*|--[\w-]+",
    r"-?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[a-zA-Z_][\w-]*|--[\w-]+|-[a-zA-Z][\w-]*",
];

/// Detect a CSS identifier regex.
fn is_css_ident_regex(pattern: &str) -> bool {
    CSS_IDENT_REGEX_PATTERNS.contains(&pattern)
}

/// Known CSS string regex patterns.
const CSS_STRING_REGEX_PATTERNS: &[&str] = &[r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#];

/// Detect a CSS string regex.
fn is_css_string_regex(pattern: &str) -> bool {
    CSS_STRING_REGEX_PATTERNS.contains(&pattern)
}

/// Whether a negated character class uses `+` (one-or-more) or `*` (zero-or-more).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NegCharClassQuantifier {
    Plus,
    Star,
}

// ---------------------------------------------------------------------------
// Generalized regex strength reduction
// ---------------------------------------------------------------------------

/// Whether a pattern is a simple character range like `[a-z]` or `[0-9]`.
/// Returns `(lo, hi)` byte range if detected.
fn is_single_char_range_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must be exactly "X-Y" where X and Y are single ASCII characters.
    if inner.len() == 3 && inner.as_bytes()[1] == b'-' {
        let lo = inner.as_bytes()[0];
        let hi = inner.as_bytes()[2];
        if lo.is_ascii() && hi.is_ascii() && lo < hi {
            return Some((lo, hi));
        }
    }
    None
}

/// Whether a pattern is a simple character set like `[abc]` (no ranges, no escapes).
/// Returns the set of bytes if detected (max 8 bytes for practical emission).
fn is_small_char_set_regex(pattern: &str) -> Option<Vec<u8>> {
    let inner = pattern.strip_prefix('[')?.strip_suffix(']')?;
    // Must not contain ranges, escapes, or negation.
    if inner.starts_with('^') || inner.contains('-') || inner.contains('\\') {
        return None;
    }
    let bytes: Vec<u8> = inner.bytes().collect();
    if bytes.len() >= 2 && bytes.len() <= 8 && bytes.iter().all(|b| b.is_ascii()) {
        Some(bytes)
    } else {
        None
    }
}

/// Whether a pattern is a single-char range with `+` quantifier like `[a-z]+`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_plus_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('+')?;
    is_single_char_range_regex(inner)
}

/// Whether a pattern is a single-char range with `*` quantifier like `[a-z]*`.
/// Returns `(lo, hi)` byte range.
fn is_char_range_star_regex(pattern: &str) -> Option<(u8, u8)> {
    let inner = pattern.strip_suffix('*')?;
    is_single_char_range_regex(inner)
}

/// Emit a direct call for generalized regex patterns beyond JSON/CSS.
///
/// Covers:
/// - `[a-z]` → single byte range check
/// - `[abc]` → small character set match (2-8 chars)
/// - `[a-z]+` → byte range scan loop
/// - `[a-z]*` → byte range scan loop (zero-or-more)
pub fn emit_generalized_regex_direct(pattern: &str) -> Option<TokenStream> {
    // Single character range: [a-z]
    if let Some((lo, hi)) = is_single_char_range_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if __b >= #lo_lit && __b <= #hi_lit {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Small character set: [abc]
    if let Some(bytes) = is_small_char_set_regex(pattern) {
        let byte_lits: Vec<_> = bytes
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        return Some(quote! {
            {
                let __start = state.offset;
                if let Some(&__b) = state.src_bytes.get(__start) {
                    if matches!(__b, #(#byte_lits)|*) {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        });
    }

    // Character range with + quantifier: [a-z]+
    if let Some((lo, hi)) = is_char_range_plus_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                if __pos > __start {
                    state.offset = __pos;
                    Some(::parse_that::Span::new(__start, __pos, state.src))
                } else {
                    None
                }
            }
        });
    }

    // Character range with * quantifier: [a-z]*
    if let Some((lo, hi)) = is_char_range_star_regex(pattern) {
        let lo_lit = proc_macro2::Literal::byte_character(lo);
        let hi_lit = proc_macro2::Literal::byte_character(hi);
        return Some(quote! {
            {
                let __start = state.offset;
                let __end = state.src_bytes.len();
                let mut __pos = __start;
                while __pos < __end {
                    let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                    if __b >= #lo_lit && __b <= #hi_lit {
                        __pos += 1;
                    } else {
                        break;
                    }
                }
                state.offset = __pos;
                Some(::parse_that::Span::new(__start, __pos, state.src))
            }
        });
    }

    None
}

/// Detect a negated character class regex of the form `[^XYZ]+` or `[^XYZ]*`
/// and return the excluded bytes and quantifier. These patterns scan until any
/// excluded byte is found — suited for `take_until_any_span()` (256-byte LUT).
fn is_negated_char_class_regex(pattern: &str) -> Option<(String, NegCharClassQuantifier)> {
    let rest = pattern.strip_prefix("[^")?;

    let (inner, quantifier) = if let Some(inner) = rest.strip_suffix("]+") {
        (inner, NegCharClassQuantifier::Plus)
    } else if let Some(inner) = rest.strip_suffix("]*") {
        (inner, NegCharClassQuantifier::Star)
    } else {
        return None;
    };

    // Validate: only ASCII printable characters and simple backslash escapes.
    let mut chars = inner.chars().peekable();
    let mut excluded = String::new();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let esc = chars.next()?;
            match esc {
                '\\' | '/' | ']' | '[' | '^' | '-' | '.' | '*' | '+' | '?' | '(' | ')' | '{'
                | '}' | '|' | 'n' | 'r' | 't' => {
                    let actual = match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    excluded.push(actual);
                }
                _ => return None,
            }
        } else if c.is_ascii() && c != '[' && c != ']' {
            excluded.push(c);
        } else {
            return None;
        }
    }

    if excluded.is_empty() {
        return None;
    }

    Some((excluded, quantifier))
}
