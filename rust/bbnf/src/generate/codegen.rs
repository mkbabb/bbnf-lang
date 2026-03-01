//! Parser code generation from grammar expressions.
//!
//! Translates each `Expression` into a `proc_macro2::TokenStream` representing
//! a Rust parser combinator invocation. Handles concatenation, alternation,
//! dispatch tables, regex fast-paths, and transparent/boxed enum wrapping.

use crate::analysis::{build_dispatch_table, Dependencies};
use crate::types::*;

use super::patterns::*;
use super::type_inference::*;
use super::types::*;

use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse_quote;

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

pub fn calculate_acyclic_deps_degree<'a>(
    acyclic_deps: &'a Dependencies<'a>,
) -> HashMap<&'a Expression<'a>, usize> {
    fn recurse<'a, 'b>(
        expr: &'a Expression<'a>,
        acyclic_deps: &'a Dependencies<'a>,
        degree_map: &'b mut HashMap<&'a Expression<'a>, usize>,
    ) -> usize
    where
        'a: 'b,
    {
        if let Some(degree) = degree_map.get(expr) {
            return *degree;
        }
        let Some(deps) = acyclic_deps.get(expr) else {
            return 0;
        };
        let sum = deps
            .iter()
            .map(|dep| recurse(dep, acyclic_deps, degree_map))
            .sum::<usize>()
            + 1;
        degree_map.insert(expr, sum);
        sum
    }

    let mut degree_map = HashMap::new();
    acyclic_deps.keys().for_each(|expr| {
        recurse(expr, acyclic_deps, &mut degree_map);
    });
    degree_map
}

pub fn calculate_non_acyclic_deps_degree<'a, 'b>(
    non_acyclic_deps: &'a Dependencies<'a>,
    acyclic_deps_degree: &'b mut HashMap<&'a Expression<'a>, usize>,
) where
    'a: 'b,
{
    fn recurse<'a, 'b>(
        expr: &'a Expression<'a>,
        non_acyclic_deps: &'a Dependencies<'a>,
        acyclic_deps_degree: &'b mut HashMap<&'a Expression<'a>, usize>,
    ) -> usize
    where
        'a: 'b,
    {
        if let Some(degree) = acyclic_deps_degree.get(expr) {
            *degree
        } else {
            let Some(deps) = non_acyclic_deps.get(expr) else {
                return 0;
            };
            acyclic_deps_degree.insert(expr, 0);
            let max = deps
                .iter()
                .map(|dep| recurse(dep, non_acyclic_deps, acyclic_deps_degree))
                .max()
                .unwrap_or(0)
                + 1;
            acyclic_deps_degree.insert(expr, max);
            max
        }
    }

    non_acyclic_deps.keys().for_each(|expr| {
        recurse(expr, non_acyclic_deps, acyclic_deps_degree);
    });
}

pub fn calculate_concatenation_expression<'a>(
    inner_exprs: &'a [Expression<'a>],
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    max_depth: usize,
    depth: usize,
) -> TokenStream {
    // Phase E: For sp_method_rules nonterminals in concatenation, override type to Span
    // and parser to Self::rule_sp().into_parser(). This avoids Box allocation for
    // nonterminals that are just Span wrappers (e.g. `string` in `pair = string, colon >> value`).
    let tys: Vec<syn::Type> = inner_exprs
        .iter()
        .map(|expr| {
            if concat_element_sp_name(expr, grammar_attrs).is_some() {
                parse_quote!(::parse_that::Span<'a>)
            } else {
                calculate_expression_type(expr, grammar_attrs, cache_bundle)
            }
        })
        .collect();
    let mut chains: Vec<(bool, Vec<TokenStream>)> = Vec::new();
    for (parser, ty) in inner_exprs
        .iter()
        .map(|expr| {
            if let Some(name) = concat_element_sp_name(expr, grammar_attrs) {
                let sp_ident = format_ident!("{}_sp", name);
                quote! { Self::#sp_ident().into_parser() }
            } else {
                calculate_parser_from_expression(expr, grammar_attrs, cache_bundle, max_depth, depth)
            }
        })
        .zip(tys.iter())
    {
        let is_span = type_is_span(ty);
        if let Some((last_is_span, last_chain)) = chains.last_mut() {
            if is_span && *last_is_span {
                last_chain.push(parser);
                continue;
            }
        }
        chains.push((is_span, vec![parser]));
    }
    let mut acc = None;
    for (n, (_, chain)) in chains.iter().enumerate() {
        let chain_acc = chain.iter().fold(None, |acc, parser| match acc {
            None => Some(parser.clone()),
            Some(acc) => Some(quote! {
                #acc.then_span(#parser)
            }),
        });
        acc = match acc {
            None => chain_acc,
            Some(acc) => {
                if n > 1 {
                    Some(quote! {
                        #acc.then_flat(#chain_acc)
                    })
                } else {
                    Some(quote! {
                        #acc.then(#chain_acc)
                    })
                }
            }
        };
    }
    acc.unwrap()
}

/// Coerce alternation branch parsers to a uniform `Box<Enum>` output type.
/// - Branches already producing `Box<Enum>`: left as-is.
/// - Branches producing `Span`: wrapped with `.map(|x| Box::new(x))`.
/// - Branches producing other types (tuples): wrapped with a sub-variant
///   constructor: `.map(|x| Box::new(Enum::rule_N(x)))`.
fn coerce_alternation_branches<'a>(
    parsers: &[TokenStream],
    branch_tys: &[syn::Type],
    overall_is_boxed_enum: bool,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    _cache_bundle: &'a CacheBundle<'a, '_, '_>,
) -> Vec<TokenStream> {
    if !overall_is_boxed_enum {
        return parsers.to_vec();
    }

    let boxed_enum_str = grammar_attrs.boxed_enum_type.to_token_stream().to_string();
    let enum_ident = grammar_attrs.enum_ident;

    // Collect ALL sub-variants from all rules into a flat lookup by type string.
    // This handles the case where an alternation is encountered during deep inlining
    // and current_rule_name doesn't match the original rule that defined the sub-variants.
    let all_sub_variants: Vec<&(String, syn::Type)> = grammar_attrs
        .sub_variants
        .map(|sv| sv.values().flatten().collect())
        .unwrap_or_default();

    parsers
        .iter()
        .zip(branch_tys.iter())
        .map(|(parser, branch_ty)| {
            let ty_str = branch_ty.to_token_stream().to_string();

            if ty_str == boxed_enum_str {
                // Already Box<Enum> — no wrapping needed.
                parser.clone()
            } else if let Some((variant_name, _)) = all_sub_variants.iter().find(|(_, vty)| {
                vty.to_token_stream().to_string() == ty_str
            }) {
                // Found a sub-variant with matching type — wrap with it.
                // This must be checked BEFORE Span to handle heterogeneous alternations
                // where Span branches need sub-variant wrappers (e.g. term_0).
                let variant_ident = format_ident!("{}", variant_name);
                quote! { #parser.map(|x| Box::new(#enum_ident::#variant_ident(x))) }
            } else if type_is_span(branch_ty) {
                // Span branch with no sub-variant — box it directly.
                quote! { #parser.map(|x| Box::new(x)) }
            } else {
                // No matching sub-variant — leave as-is (this may cause a type error
                // if the branch type doesn't match the alternation's overall type).
                parser.clone()
            }
        })
        .collect()
}

pub fn calculate_alternation_expression<'a>(
    inner_exprs: &'a [Expression<'a>],
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    max_depth: usize,
    depth: usize,
) -> TokenStream {
    if let Some(parser) = check_for_any_span(inner_exprs) {
        return map_span_if_needed(parser, true, grammar_attrs);
    }

    let parsers: Vec<TokenStream> = inner_exprs
        .iter()
        .map(|expr| {
            calculate_parser_from_expression(expr, grammar_attrs, cache_bundle, max_depth, depth)
        })
        .collect();

    // Phase 1.3: Dispatch table generation — O(1) byte dispatch for alternations
    // with disjoint FIRST sets. Each branch parser is coerced to the alternation's
    // overall output type before being placed in the dispatch table.
    if let Some(first_sets) = grammar_attrs.first_sets {
        let alt_refs: Vec<&Expression<'a>> = inner_exprs.iter().collect();
        if let Some(dispatch) = build_dispatch_table(&alt_refs, first_sets, grammar_attrs.ast) {
            // Compute per-branch types to detect Span vs Box<Enum> mismatches.
            let branch_tys: Vec<syn::Type> = inner_exprs
                .iter()
                .map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle))
                .collect();
            let overall_ty = {
                let all_span = branch_tys.iter().all(type_is_span);
                let all_same = branch_tys.iter().all(|ty| {
                    ty.to_token_stream().to_string()
                        == branch_tys[0].to_token_stream().to_string()
                });
                if all_span || all_same {
                    branch_tys[0].clone()
                } else {
                    grammar_attrs.boxed_enum_type.clone()
                }
            };
            let overall_is_boxed_enum = !type_is_span(&overall_ty)
                && overall_ty.to_token_stream().to_string()
                    == grammar_attrs.boxed_enum_type.to_token_stream().to_string();

            // Coerce each branch parser to the overall type if needed.
            let coerced_parsers: Vec<TokenStream> = coerce_alternation_branches(
                &parsers, &branch_tys, overall_is_boxed_enum,
                grammar_attrs, cache_bundle,
            );

            // Phase C+D: Inline match dispatch with SpanParser fast-path.
            // For span-eligible branches, call Self::rule_sp() directly to avoid
            // vtable hops. For other branches, hoist parsers into let bindings.
            let mut branch_bindings: Vec<TokenStream> = Vec::new();
            let mut match_arms: Vec<TokenStream> = Vec::new();
            let mut used: Vec<bool> = vec![false; coerced_parsers.len()];

            // Detect which branches are span-eligible nonterminals with _sp methods.
            // Returns (name, sp_constructor, map_fn) so we can hoist the SpanParser
            // construction into a let binding and only call it in the match arm.
            let branch_sp_info: Vec<Option<(String, TokenStream, TokenStream)>> = inner_exprs
                .iter()
                .map(|expr| {
                    // First: check inline cache for MappedExpression (boxed2 path)
                    if let Some(Expression::MappedExpression((inner_token, mapping_token))) = cache_bundle.inline_cache.borrow().get(expr) {
                        let inner = inner_token.inner();
                        let mapping = mapping_token.inner();
                        if let Expression::Nonterminal(Token { value: nt_name, .. }) = inner {
                            if let Some(sp_rules) = grammar_attrs.sp_method_rules {
                                if sp_rules.contains(nt_name.as_ref()) {
                                    if let Expression::MappingFn(Token { value: map_fn, .. }) = mapping {
                                        let sp_ident = format_ident!("{}_sp", nt_name.as_ref());
                                        let map_closure: syn::ExprClosure = syn::parse_str(map_fn).ok()?;
                                        return Some((
                                            nt_name.as_ref().to_string(),
                                            quote! { Self::#sp_ident() },
                                            quote! { #map_closure },
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    // Phase E: Direct nonterminal check — sp_method_rules nonterminals
                    // skip the boxed2 inline cache, so detect them here directly.
                    if let Expression::Nonterminal(Token { value: nt_name, .. }) = expr {
                        if let Some(sp_rules) = grammar_attrs.sp_method_rules {
                            if sp_rules.contains(nt_name.as_ref())
                                && !is_transparent_rule(nt_name, grammar_attrs)
                            {
                                let sp_ident = format_ident!("{}_sp", nt_name.as_ref());
                                let enum_ident = grammar_attrs.enum_ident;
                                let variant_ident = format_ident!("{}", nt_name.as_ref());
                                return Some((
                                    nt_name.as_ref().to_string(),
                                    quote! { Self::#sp_ident() },
                                    quote! { |x| #enum_ident::#variant_ident(x) },
                                ));
                            }
                        }
                    }
                    None
                })
                .collect();

            for (idx, parser) in coerced_parsers.iter().enumerate() {
                if used[idx] { continue; }
                used[idx] = true;
                let bytes: Vec<u8> = (0u8..128)
                    .filter(|&c| dispatch.lookup(c) == Some(idx))
                    .collect();
                if bytes.is_empty() { continue; }

                // Build byte match patterns (e.g. b'{' | b'[' | b'"')
                let byte_patterns: Vec<proc_macro2::TokenStream> = bytes.iter()
                    .map(|&b| {
                        let b_lit = proc_macro2::Literal::byte_character(b);
                        quote! { #b_lit }
                    })
                    .collect();

                // Phase D: Use SpanParser fast-path if available.
                // Only use branch_sp_info when overall type is Box<Enum> — the sp_call
                // includes enum variant wrapping which is wrong for all-Span alternations.
                // Hoist SpanParser construction into a let binding to avoid reconstructing
                // on every match (critical for large-file performance).
                if overall_is_boxed_enum {
                    if let Some(Some((_, sp_constructor, map_fn))) = branch_sp_info.get(idx) {
                        let sp_binding_ident = format_ident!("_sp_{}", idx);
                        branch_bindings.push(quote! { let #sp_binding_ident = #sp_constructor; });
                        let call = quote! { #sp_binding_ident.call(state).map(#map_fn).map(Box::new) };
                        match_arms.push(quote! {
                            #(#byte_patterns)|* => { #call },
                        });
                        continue;
                    }
                }
                {
                    let branch_ident = format_ident!("_branch_{}", idx);
                    branch_bindings.push(quote! { let #branch_ident = #parser; });
                    match_arms.push(quote! {
                        #(#byte_patterns)|* => #branch_ident.call(state),
                    });
                }
            }

            match_arms.push(quote! { _ => None, });

            return quote! {
                {
                    #(#branch_bindings)*
                    ::parse_that::Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                        let byte = *state.src_bytes.get(state.offset)?;
                        match byte {
                            #(#match_arms)*
                        }
                    })
                }
            };
        }
    }

    // For 3+ non-span branches, emit one_of(vec![...]) for flat alternation
    if parsers.len() >= 3 {
        let tys: Vec<_> = inner_exprs
            .iter()
            .map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle))
            .collect();
        let all_span = tys.iter().all(type_is_span);
        if !all_span {
            // Check if branches are heterogeneous (need coercion).
            let all_same = tys.iter().all(|ty| {
                ty.to_token_stream().to_string() == tys[0].to_token_stream().to_string()
            });
            if !all_same {
                // Heterogeneous: coerce all branches to Box<Enum>.
                let coerced = coerce_alternation_branches(
                    &parsers, &tys, true,
                    grammar_attrs, cache_bundle,
                );
                return quote! {
                    ::parse_that::one_of(vec![#(#coerced),*])
                };
            }
            return quote! {
                ::parse_that::one_of(vec![#(#parsers),*])
            };
        }
    }

    let parser = parsers
        .into_iter()
        .fold(None, |acc, parser| match acc {
            None => Some(parser),
            Some(acc) => Some(quote! {
                #acc | #parser
            }),
        })
        .unwrap();
    if inner_exprs.len() > 1 {
        quote! {
            (#parser)
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
