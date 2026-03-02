//! Pattern detection helpers for code generation.
//!
//! Detects structural patterns in the AST (regex coalescing, sep-by, wrapped
//! expressions, JSON fast-paths, any-span, and span-eligible concatenation
//! elements) and emits optimized `TokenStream` codegen for each.

use crate::types::*;

use super::codegen::calculate_parser_from_expression;
use super::type_inference::{calculate_expression_type, type_is_span};
use super::types::*;

use proc_macro2::TokenStream;
use quote::quote;

/// Phase 1.4: Detect `literal >> many/many1(regex) << literal` and fuse into a single `sp_regex()`.
///
/// Handles both AST shapes:
///   Shape A: Next(Literal_L, Skip(Many/Many1(Regex), Literal_R))
///   Shape B: Skip(Next(Literal_L, Many/Many1(Regex)), Literal_R)
pub fn check_for_regex_coalesce<'a>(
    expr: &'a Expression<'a>,
) -> Option<TokenStream> {
    let (left_lit, pattern, quantifier, right_lit) = match expr {
        // Shape A: next(literal, skip(many(regex), literal))
        Expression::Next(left_token, right_token) => {
            let left = left_token.inner();
            let right = right_token.inner();
            match (left, right) {
                (Expression::Literal(l_tok), Expression::Skip(middle_token, end_token)) => {
                    let middle = middle_token.inner();
                    let end = end_token.inner();
                    match (middle, end) {
                        (Expression::Many(inner_token), Expression::Literal(r_tok)) => {
                            let inner = inner_token.inner();
                            if let Expression::Regex(re_tok) = inner {
                                (l_tok.value.as_ref(), re_tok.value.as_ref(), "*", r_tok.value.as_ref())
                            } else {
                                return None;
                            }
                        }
                        (Expression::Many1(inner_token), Expression::Literal(r_tok)) => {
                            let inner = inner_token.inner();
                            if let Expression::Regex(re_tok) = inner {
                                (l_tok.value.as_ref(), re_tok.value.as_ref(), "+", r_tok.value.as_ref())
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
                _ => return None,
            }
        }
        // Shape B: skip(next(literal, many(regex)), literal)
        Expression::Skip(left_token, right_token) => {
            let left = left_token.inner();
            let right = right_token.inner();
            match (left, right) {
                (Expression::Next(start_token, middle_token), Expression::Literal(r_tok)) => {
                    let start = start_token.inner();
                    let middle = middle_token.inner();
                    match (start, middle) {
                        (Expression::Literal(l_tok), Expression::Many(inner_token)) => {
                            let inner = inner_token.inner();
                            if let Expression::Regex(re_tok) = inner {
                                (l_tok.value.as_ref(), re_tok.value.as_ref(), "*", r_tok.value.as_ref())
                            } else {
                                return None;
                            }
                        }
                        (Expression::Literal(l_tok), Expression::Many1(inner_token)) => {
                            let inner = inner_token.inner();
                            if let Expression::Regex(re_tok) = inner {
                                (l_tok.value.as_ref(), re_tok.value.as_ref(), "+", r_tok.value.as_ref())
                            } else {
                                return None;
                            }
                        }
                        _ => return None,
                    }
                }
                _ => return None,
            }
        }
        _ => return None,
    };

    // Escape the literals for use in a regex
    let escaped_left = regex::escape(left_lit);
    let escaped_right = regex::escape(right_lit);
    let combined = format!("{}({}){}{}", escaped_left, pattern, quantifier, escaped_right);

    // Validate the combined regex compiles
    if regex::Regex::new(&combined).is_err() {
        return None;
    }

    Some(quote! {
        ::parse_that::sp_regex(#combined)
    })
}

pub fn check_for_sep_by<'a>(
    expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    max_depth: usize,
    depth: usize,
) -> Option<TokenStream> {
    match expr {
        Expression::Group(inner) => {
            let Token { value, .. } = inner.as_ref();
            check_for_sep_by(value, grammar_attrs, cache_bundle, max_depth, depth)
        }
        Expression::Skip(left_expr, inner)
            if matches!(inner.as_ref(), Token { value: Expression::Optional(_), .. }) =>
        {
            // Invariant: the match guard above already confirmed inner is Optional.
            let Token { value: Expression::Optional(right_expr), .. } = inner.as_ref() else {
                unreachable!("match guard confirmed Expression::Optional")
            };
            let left_expr = left_expr.inner();
            let mut right_expr = right_expr.inner();

            let left_parser = calculate_parser_from_expression(
                left_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );

            if let Some(Expression::MappedExpression((t_right_expr, _))) =
                cache_bundle.inline_cache.borrow().get(right_expr)
            {
                right_expr = t_right_expr.inner();
            }

            let right_parser = calculate_parser_from_expression(
                right_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );

            let left_type = calculate_expression_type(left_expr, grammar_attrs, cache_bundle);
            let right_type = calculate_expression_type(right_expr, grammar_attrs, cache_bundle);

            if type_is_span(&left_type) && type_is_span(&right_type) {
                Some(quote! {
                    #left_parser.sep_by_span(#right_parser, ..)
                })
            } else {
                Some(quote! {
                    #left_parser.sep_by(#right_parser, ..)
                })
            }
        }
        _ => None,
    }
}

pub fn check_for_wrapped<'a>(
    left_expr: &'a Expression<'a>,
    right_expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    max_depth: usize,
    depth: usize,
) -> Option<TokenStream> {
    match left_expr {
        Expression::Group(inner) => check_for_wrapped(
            &inner.as_ref().value,
            right_expr,
            grammar_attrs,
            cache_bundle,
            max_depth,
            depth,
        ),
        Expression::Next(left_expr, middle_expr) => {
            let left_expr = left_expr.inner();
            let middle_expr = middle_expr.inner();
            let left_parser = calculate_parser_from_expression(
                left_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let middle_parser = calculate_parser_from_expression(
                middle_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let right_parser = calculate_parser_from_expression(
                right_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let left_type = calculate_expression_type(left_expr, grammar_attrs, cache_bundle);
            let middle_type = calculate_expression_type(middle_expr, grammar_attrs, cache_bundle);
            let right_type = calculate_expression_type(right_expr, grammar_attrs, cache_bundle);
            if type_is_span(&left_type) && type_is_span(&middle_type) && type_is_span(&right_type) {
                Some(quote! {
                    #middle_parser.wrap_span(#left_parser, #right_parser)
                })
            } else {
                Some(quote! {
                    #middle_parser.wrap(#left_parser, #right_parser)
                })
            }
        }
        _ => None,
    }
}

/// Known exact JSON string regex patterns that can be replaced with the
/// `sp_json_string_quoted()` SIMD fast-path. Each entry is a canonical form
/// found in real JSON grammars. We match exactly rather than using substring
/// heuristics to avoid false positives on non-JSON patterns that happen to
/// contain similar substrings.
const JSON_STRING_REGEX_PATTERNS: &[&str] = &[
    // Standard JSON string: "..."  with escape sequences
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    // Variant with \uXXXX using [0-9A-Fa-f] (case-flexible hex)
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9A-Fa-f]{4}))*""#,
    // Variant without the \/ escape (some grammars omit forward-slash escape)
    r#""(?:[^"\\]|\\(?:["\\bfnrt]|u[0-9a-fA-F]{4}))*""#,
];

/// Known exact JSON number regex patterns that can be replaced with the
/// `sp_json_number()` monolithic byte-loop fast-path.
const JSON_NUMBER_REGEX_PATTERNS: &[&str] = &[
    // Standard JSON number with \d shorthand
    r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
    // Variant with explicit [0-9] instead of \d
    r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?",
];

/// Detect the canonical JSON string regex pattern and return true if it matches.
/// The JSON grammar uses `/"(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*"/`
/// which compiles to a general-purpose NFA via `sp_regex(...)`. The fast-path
/// `sp_json_string()` uses `memchr2(b'"', b'\\')` SIMD scanning instead.
///
/// Uses exact pattern matching against known canonical forms rather than
/// substring heuristics to avoid false positives.
pub fn is_json_string_regex(pattern: &str) -> bool {
    JSON_STRING_REGEX_PATTERNS.contains(&pattern)
}

/// Detect the canonical JSON number regex and return true if it matches.
/// The JSON grammar uses `/-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/` which
/// compiles to a general-purpose NFA. The fast-path `sp_json_number()` uses a
/// monolithic byte loop that is dramatically faster for number-heavy inputs.
///
/// Uses exact pattern matching against known canonical forms rather than
/// substring heuristics to avoid false positives.
pub fn is_json_number_regex(pattern: &str) -> bool {
    JSON_NUMBER_REGEX_PATTERNS.contains(&pattern)
}

/// Detect a negated character class regex of the form `[^XYZ]+` and return the
/// excluded bytes. These patterns scan until any excluded byte is found—perfectly
/// suited for `take_until_any_span()` which uses a 256-byte LUT instead of NFA.
///
/// Matches patterns like `/[^;{}!,]+/`, `/[^{};]+/`, `/[^"\\]+/` etc.
/// Returns `Some(excluded_bytes)` with the raw byte string content, or None.
pub fn is_negated_char_class_regex(pattern: &str) -> Option<String> {
    // Must be exactly `[^...]+` — no prefix, no suffix, no alternation
    let inner = pattern.strip_prefix("[^")?.strip_suffix("]+")?;

    // Validate: only ASCII printable characters and simple backslash escapes
    let mut chars = inner.chars().peekable();
    let mut excluded = String::new();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Backslash escape — take next char literally
            let esc = chars.next()?;
            match esc {
                '\\' | '/' | ']' | '[' | '^' | '-' | '.' | '*' | '+' | '?' | '(' | ')'
                | '{' | '}' | '|' | 'n' | 'r' | 't' => {
                    let actual = match esc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    excluded.push(actual);
                }
                _ => return None, // Complex escape (e.g. \d, \w) — bail
            }
        } else if c.is_ascii() && c != '[' && c != ']' {
            excluded.push(c);
        } else {
            return None; // Non-ASCII or nested bracket — bail
        }
    }

    if excluded.is_empty() {
        return None;
    }

    Some(excluded)
}

pub fn check_for_any_span(exprs: &[Expression]) -> Option<TokenStream> {
    let all_literals = exprs
        .iter()
        .all(|expr| matches!(expr, Expression::Literal(_)));
    if all_literals {
        let literal_arr: Vec<proc_macro2::Literal> = exprs
            .iter()
            .map(|expr| {
                if let Expression::Literal(token) = expr {
                    let unescaped = super::codegen::unescape_literal(&token.value);
                    proc_macro2::Literal::string(&unescaped)
                } else {
                    // Invariant: all_literals guard above ensures every expr is a Literal.
                    unreachable!("all_literals guard ensures Expression::Literal")
                }
            })
            .collect();
        Some(quote! {
            :: parse_that:: any_span(&[#(#literal_arr), *])
        })
    } else {
        None
    }
}

/// Phase E: Check if a concatenation element is a nonterminal in sp_method_rules.
/// Returns the nonterminal name if so.
pub fn concat_element_sp_name<'a>(
    expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> Option<&'a str> {
    if let Expression::Nonterminal(Token { value, .. }) = expr {
        if grammar_attrs.sp_method_rules.is_some_and(|set| set.contains(value.as_ref())) {
            return Some(value.as_ref());
        }
    }
    None
}
