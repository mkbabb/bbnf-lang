//! Parser code generation from grammar expressions.
//!
//! Translates each `Expression` into a `proc_macro2::TokenStream` representing
//! a Rust parser combinator invocation. Handles concatenation, alternation,
//! dispatch tables, regex fast-paths, and transparent/boxed enum wrapping.

use crate::types::*;

use super::alternation::calculate_alternation_expression;
use super::concatenation::calculate_concatenation_expression;
use super::patterns::*;
use super::type_inference::*;
use super::types::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

/// Unescape a BBNF literal string (e.g. `\n` → newline, `\t` → tab).
/// BBNF literals store escape sequences as raw characters (backslash + letter)
/// since they come from source text between quotes. We need to unescape them
/// before embedding into Rust string literals via `quote!`.
pub fn unescape_literal(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('f') => result.push('\x0C'),
                Some('b') => result.push('\x08'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

pub fn map_generated_parser<'a, 'b>(
    name: &str,
    expr: &Expression<'a>,
    enum_ident: &syn::Ident,
) -> Expression<'b>
where
    'a: 'b,
{
    let ident = format_ident!("{}", name);
    let expr_token = Token::new_without_span(expr.clone());
    let mapping_fn = format!("|x| {enum_ident}::{ident}( x ) ");
    let mapping_fn_token = Token::new_without_span(Expression::MappingFn(Token::new_without_span(
        mapping_fn.into(),
    )));

    Expression::MappedExpression((expr_token.into(), mapping_fn_token.into()))
}

pub fn box_generated_parser<'a, 'b>(
    name: &str,
    expr: &Expression<'a>,
    enum_ident: &syn::Ident,
) -> Expression<'b>
where
    'a: 'b,
{
    let ident = format_ident!("{}", name);
    let expr_token = Token::new_without_span(expr.clone());
    let mapping_fn = format!("|x| {enum_ident}::{ident}(Box::new(x)) ");
    let mapping_fn_token = Token::new_without_span(Expression::MappingFn(Token::new_without_span(
        mapping_fn.into(),
    )));

    Expression::MappedExpression((expr_token.into(), mapping_fn_token.into()))
}

pub fn box_generated_parser2<'a, 'b>(
    name: &str,
    expr: &Expression<'a>,
    enum_ident: &syn::Ident,
) -> Expression<'b>
where
    'a: 'b,
{
    let ident = format_ident!("{}", name);
    let expr_token = Token::new_without_span(expr.clone());
    let mapping_fn = format!("|x| Box::new({enum_ident}::{ident}(x)) ");
    let mapping_fn_token = Token::new_without_span(Expression::MappingFn(Token::new_without_span(
        mapping_fn.into(),
    )));

    Expression::MappedExpression((expr_token.into(), mapping_fn_token.into()))
}

pub fn format_parser<'a, 'b>(
    name: &str,
    expr: &'a Expression<'a>,
    parser_container_attrs: &ParserAttributes,
) -> Expression<'b>
where
    'a: 'b,
{
    let mut expr = expr.clone();
    if parser_container_attrs.ignore_whitespace {
        expr = Expression::OptionalWhitespace(Token::new_without_span(expr).into());
    }
    if parser_container_attrs.debug {
        expr = Expression::DebugExpression((Token::new_without_span(expr).into(), name.into()));
    }
    expr
}

pub fn map_span_if_needed<'a>(
    parser: TokenStream,
    is_span: bool,
    GeneratedGrammarAttributes {
        parser_container_attrs,
        ..
    }: &'a GeneratedGrammarAttributes<'a>,
) -> TokenStream {
    if parser_container_attrs.use_string && is_span {
        quote! {
            #parser.map(|x| x.as_str())
        }
    } else {
        parser
    }
}

pub fn calculate_parser_from_expression<'a>(
    expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    max_depth: usize,
    depth: usize,
) -> TokenStream {
    fn get_and_parse_default_parser<'a>(
        name: &str,
        args: Option<TokenStream>,
        grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    ) -> Option<TokenStream> {
        let GeneratedNonterminalParser {
            parser,
            ty,
            ..
        } = DEFAULT_PARSERS.get(name)?;
        let Ok(ty) = syn:: parse_str::< syn:: Type >(ty) else {
            return None;
        };
        let parser = syn::parse_str::<syn::Expr>(parser)
            .unwrap()
            .to_token_stream();
        let parser = if let Some(args) = args {
            quote! {
                #parser(#args)
            }
        } else {
            parser
        };
        Some(map_span_if_needed(parser, type_is_span(&ty), grammar_attrs))
    }

    if let Some(parser) = cache_bundle.parser_cache.borrow().get(expr) {
        return parser.clone();
    }
    if let Some(cached_expr) = cache_bundle.inline_cache.borrow().get(expr) {
        if depth <= max_depth {
            return calculate_parser_from_expression(
                cached_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth + 1,
            );
        }
    }
    let parser = match expr {
        Expression::Literal(Token { value, .. }) => {
            let unescaped = unescape_literal(value);
            let lit = proc_macro2::Literal::string(&unescaped);
            get_and_parse_default_parser(
                "LITERAL",
                Some(quote! {
                    #lit
                }),
                grammar_attrs,
            )
            .unwrap()
        }
        Expression::Regex(Token { value, .. }) => {
            // Phase 2.1: Detect JSON string regex and emit sp_json_string() fast path.
            // The JSON grammar's string regex uses memchr2-based SIMD scanning which is
            // dramatically faster than the general-purpose NFA for string-heavy workloads.
            if is_json_string_regex(value) {
                let parser = quote! { ::parse_that::sp_json_string_quoted() };
                map_span_if_needed(parser, true, grammar_attrs)
            } else if is_json_number_regex(value) {
                let parser = quote! { ::parse_that::sp_json_number() };
                map_span_if_needed(parser, true, grammar_attrs)
            } else if let Some(excluded) = is_negated_char_class_regex(value) {
                // Negated character class `[^XYZ]+` → LUT byte scan (10-15x faster than regex NFA)
                let excluded_bytes = proc_macro2::Literal::byte_string(excluded.as_bytes());
                let parser = quote! { ::parse_that::take_until_any_span(#excluded_bytes) };
                map_span_if_needed(parser, true, grammar_attrs)
            } else {
                get_and_parse_default_parser(
                    "REGEX",
                    Some(quote! {
                        #value
                    }),
                    grammar_attrs,
                )
                .unwrap()
            }
        }
        Expression::Nonterminal(Token { value, .. }) => {
            if let Some(parser) = get_and_parse_default_parser(value, None, grammar_attrs) {
                parser
            } else {
                // Phase 4.2: Resolve aliases — if this nonterminal aliases another,
                // emit the target's method instead to eliminate indirection.
                let resolved_name = if let Some(aliases) = grammar_attrs.aliases {
                    let canonical = aliases.iter().find(|(k, _)| {
                        matches!(k, Expression::Nonterminal(t) if t.value.as_ref() == value.as_ref())
                    });
                    if let Some((_, Expression::Nonterminal(t))) = canonical {
                        t.value.as_ref().to_string()
                    } else {
                        value.to_string()
                    }
                } else {
                    value.to_string()
                };
                let ident = format_ident!("{}", resolved_name);

                if is_transparent_rule(&resolved_name, grammar_attrs) {
                    // Phase B: Transparent rules already return Box<Enum>,
                    // so skip the extra .map(|x| Box::new(x)) wrapping.
                    quote! { Self::#ident() }
                } else {
                    quote! { Self::#ident().map(|x| Box::new(x)) }
                }
            }
        }
        Expression::Epsilon(_) => quote! {
            ::parse_that::epsilon()
        },
        Expression::MappedExpression((inner_expr, mapping_fn)) => {
            let inner_expr = inner_expr.inner();
            let mapping_fn = mapping_fn.inner();

            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );

            if let Expression::MappingFn(Token { value, .. }) = mapping_fn {
                let Ok(mapping_fn) = syn:: parse_str::< syn:: ExprClosure >(value) else {
                    panic!("Invalid mapper expression: {}", value);
                };

                quote! {
                    #parser.map(#mapping_fn)
                }
            } else {
                parser
            }
        }
        Expression::DebugExpression((inner_expr, name)) => {
            let inner_expr = inner_expr.inner();
            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );

            quote! {
                #parser.debug(#name)
            }
        }
        Expression::Group(inner_expr) => {
            let inner_expr = inner_expr.inner();
            calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            )
        }
        Expression::Optional(inner_expr) => {
            let inner_expr = inner_expr.inner();
            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let ty = calculate_expression_type(inner_expr, grammar_attrs, cache_bundle);
            if type_is_span(&ty) {
                return quote! {
                    #parser.opt_span()
                };
            }

            quote! {
                #parser.opt()
            }
        }
        Expression::OptionalWhitespace(inner_expr) => {
            let inner_expr = inner_expr.inner();
            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );

            quote! {
                #parser.trim_whitespace()
            }
        }
        Expression::Many(inner_expr) => {
            let inner_expr = inner_expr.inner();
            if let Some(parser) =
                check_for_sep_by(inner_expr, grammar_attrs, cache_bundle, max_depth, depth)
            {
                return parser;
            }
            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let ty = calculate_expression_type(inner_expr, grammar_attrs, cache_bundle);
            if type_is_span(&ty) {
                return quote! {
                    #parser.many_span(..)
                };
            }

            quote! {
                #parser.many(..)
            }
        }
        Expression::Many1(inner_expr) => {
            let inner_expr = inner_expr.inner();
            let parser = calculate_parser_from_expression(
                inner_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            let ty = calculate_expression_type(inner_expr, grammar_attrs, cache_bundle);
            if type_is_span(&ty) {
                return quote! {
                    #parser.many_span(1..)
                };
            }

            quote! {
                #parser.many(1..)
            }
        }
        Expression::Skip(left_expr, right_expr) => {
            // Phase 1.4: Try regex coalescing first
            if let Some(parser) = check_for_regex_coalesce(expr) {
                return map_span_if_needed(parser, true, grammar_attrs);
            }
            let left_expr = left_expr.inner();
            let right_expr = right_expr.inner();
            if let Some(parser) = check_for_wrapped(
                left_expr,
                right_expr,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            ) {
                return parser;
            }
            let left_parser = calculate_parser_from_expression(
                left_expr,
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

            quote! {
                #left_parser.skip(#right_parser)
            }
        }
        Expression::Next(left_expr, right_expr) => {
            // Phase 1.4: Try regex coalescing first
            if let Some(parser) = check_for_regex_coalesce(expr) {
                return map_span_if_needed(parser, true, grammar_attrs);
            }
            let mut left_expr = left_expr.inner();

            if let Some(Expression::MappedExpression((t_left_expr, _))) =
                cache_bundle.inline_cache.borrow().get(left_expr)
            {
                left_expr = t_left_expr.inner();
            }

            let right_expr = right_expr.inner();

            let left_parser = calculate_parser_from_expression(
                left_expr,
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

            quote! {
                #left_parser.next(#right_parser)
            }
        }
        Expression::Minus(left_expr, right_expr) => {
            let left_expr = left_expr.inner();
            let right_expr = right_expr.inner();
            let left_parser = calculate_parser_from_expression(
                left_expr,
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

            quote! {
                #left_parser.minus(#right_parser)
            }
        }
        Expression::Concatenation(inner_exprs) => {
            let inner_exprs = inner_exprs.inner();
            calculate_concatenation_expression(
                inner_exprs,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            )
        }
        Expression::Alternation(inner_exprs) => {
            let inner_exprs = inner_exprs.inner();
            calculate_alternation_expression(
                inner_exprs,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            )
        }
        Expression::Rule(rhs, mapping_fn) => {
            let parser = calculate_parser_from_expression(
                rhs,
                grammar_attrs,
                cache_bundle,
                max_depth,
                depth,
            );
            if let Some(inner) = mapping_fn {
                if let Expression::MappingFn(Token { value, .. }) = inner.as_ref() {
                    let Ok(mapping_fn) = syn:: parse_str::< syn:: ExprClosure >(value) else {
                        panic!("Invalid mapper expression: {}", value);
                    };

                    quote! {
                        #parser.map(#mapping_fn)
                    }
                } else {
                    parser
                }
            } else {
                parser
            }
        }
        _ => unimplemented!("Expression not implemented: {:?}", expr),
    };
    cache_bundle
        .parser_cache
        .borrow_mut()
        .insert(expr, parser.clone());
    parser
}
