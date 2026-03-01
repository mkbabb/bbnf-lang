use std::collections::HashSet;

use bbnf::get_nonterminal_name;
use bbnf::Expression;
use bbnf::GeneratedGrammarAttributes;
use bbnf::GeneratedParserCache;
use bbnf::ParserAttributes;
use bbnf::Token;
use bbnf::TypeCache;

use quote::{format_ident, quote};

pub(crate) fn generate_enum(
    grammar_attrs: &GeneratedGrammarAttributes,
    nonterminal_types: &TypeCache,
    sub_variants: &bbnf::SubVariantCache,
) -> proc_macro2::TokenStream {
    let enum_values = nonterminal_types.iter().filter_map(|(expr, ty)| {
        let Some(name) = get_nonterminal_name(expr) else {
            panic!("Expected nonterminal");
        };
        // Phase B: Skip transparent alternation rules — they don't get their own variant.
        if let Some(transparent) = grammar_attrs.transparent_rules {
            if transparent.contains(name) {
                return None;
            }
        }
        let name = format_ident!("{}", name);
        Some(quote! { #name(#ty) })
    });

    // Generate sub-variants for heterogeneous alternation branches.
    let mut seen_sub_variant_names = HashSet::new();
    let sub_variant_values = sub_variants.values().flatten().filter_map(|(variant_name, ty)| {
        // Deduplicate: multiple branches may share the same sub-variant name.
        if !seen_sub_variant_names.insert(variant_name.clone()) {
            return None;
        }
        let ident = format_ident!("{}", variant_name);
        Some(quote! { #ident(#ty) })
    });

    let enum_ident = &grammar_attrs.enum_ident;

    // Add Recovered variant if any @recover directives exist.
    let has_recovers = grammar_attrs.recovers
        .is_some_and(|r| !r.is_empty());

    let recovered_variant = if has_recovers {
        quote! { , Recovered }
    } else {
        quote! {}
    };

    // Skip Pretty derive when sub-variants exist — the Pretty derive's From<Enum>
    // for Doc impl can't handle tuple sub-variant types. Instead, generate_prettify
    // produces to_doc() which handles all variants including sub-variants.
    let has_sub_variants = !sub_variants.is_empty();
    if has_sub_variants {
        quote! {
            #[derive(Debug, Clone)]
            pub enum #enum_ident<'a> {
                #(#enum_values),*,
                #(#sub_variant_values),*
                #recovered_variant
            }
        }
    } else {
        quote! {
            #[derive(::pprint::Pretty, Debug, Clone)]
            pub enum #enum_ident<'a> {
                #(#enum_values),*
                #recovered_variant
            }
        }
    }
}

pub(crate) fn generate_grammar_arr(
    grammar_attrs: &GeneratedGrammarAttributes,
    parser_container_attrs: &ParserAttributes,
) -> proc_macro2::TokenStream {
    let grammar_arr_name = format_ident!("GRAMMAR_{}", grammar_attrs.ident);

    let len = parser_container_attrs.paths.len();
    let include_strs = parser_container_attrs.paths.iter().map(|path| {
        let path = path.to_str().unwrap();
        quote! { include_str!(#path) }
    });

    quote! {
        #[allow(non_upper_case_globals)]
        pub const #grammar_arr_name: [&'static str; #len] = [
            #(#include_strs),*
        ];
    }
}

/// Phase D: Try to generate a SpanParser expression for a span-eligible rule.
/// Returns `Some(TokenStream)` if the rule's RHS maps to a known SpanParser pattern.
pub(crate) fn try_generate_span_parser<'a>(
    name: &str,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> Option<proc_macro2::TokenStream> {
    let ast = grammar_attrs.ast;

    // Find the rule's RHS in the AST.
    let rhs = ast.iter().find_map(|(k, v)| {
        if let Expression::Nonterminal(t) = k {
            if t.value.as_ref() == name { Some(v) } else { None }
        } else {
            None
        }
    })?;

    try_generate_span_expr(rhs, grammar_attrs)
}

/// Recursively generate a SpanParser expression for an arbitrary Expression node.
fn try_generate_span_expr<'a>(
    expr: &Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> Option<proc_macro2::TokenStream> {
    match expr {
        // Unwrap Rule wrapper — recurse into the body, ignore mapping.
        Expression::Rule(inner, _) => try_generate_span_expr(inner, grammar_attrs),

        // Unwrap Group — just recurse.
        Expression::Group(token) => try_generate_span_expr(&token.value, grammar_attrs),

        // Leaves
        Expression::Regex(Token { value, .. }) => {
            if bbnf::generate::is_json_string_regex(value) {
                Some(quote! { ::parse_that::sp_json_string_quoted() })
            } else if bbnf::generate::is_json_number_regex(value) {
                Some(quote! { ::parse_that::sp_json_number() })
            } else if let Some(excluded) = bbnf::generate::is_negated_char_class_regex(value) {
                let excluded_bytes = proc_macro2::Literal::byte_string(excluded.as_bytes());
                Some(quote! { ::parse_that::sp_take_until_any(#excluded_bytes) })
            } else {
                let pattern = value.as_ref();
                Some(quote! { ::parse_that::sp_regex(#pattern) })
            }
        }
        Expression::Literal(Token { value, .. }) => {
            let unescaped = bbnf::generate::unescape_literal(value.as_ref());
            let lit = proc_macro2::Literal::string(&unescaped);
            Some(quote! { ::parse_that::sp_string(#lit) })
        }
        Expression::Epsilon(_) => {
            Some(quote! { ::parse_that::sp_epsilon() })
        }

        // Nonterminal reference — delegate to Self::name_sp() if it exists in sp_method_rules.
        Expression::Nonterminal(Token { value: name, .. }) => {
            let has_sp = grammar_attrs.sp_method_rules
                .is_some_and(|set| set.contains(name.as_ref()));
            if has_sp {
                let ident = grammar_attrs.ident;
                let sp_ident = format_ident!("{}_sp", name);
                Some(quote! { #ident::#sp_ident() })
            } else {
                None
            }
        }

        // Concatenation — chain with .then_span()
        Expression::Concatenation(token) => {
            let parts = &token.value;
            if parts.is_empty() {
                return Some(quote! { ::parse_that::sp_epsilon() });
            }
            let mut iter = parts.iter();
            let mut acc = try_generate_span_expr(iter.next().unwrap(), grammar_attrs)?;
            for part in iter {
                let next = try_generate_span_expr(part, grammar_attrs)?;
                acc = quote! { #acc.then_span(#next) };
            }
            Some(acc)
        }

        // Alternation — chain with .or()
        // Optimize: if all branches are literals, use sp_any() for Aho-Corasick dispatch.
        Expression::Alternation(token) => {
            let branches = &token.value;
            let all_lit = branches.iter().all(|b| matches!(b, Expression::Literal(_)));
            if all_lit {
                let lits: Vec<proc_macro2::Literal> = branches.iter().map(|b| {
                    if let Expression::Literal(Token { value, .. }) = b {
                        let unescaped = bbnf::generate::unescape_literal(value.as_ref());
                        proc_macro2::Literal::string(&unescaped)
                    } else {
                        unreachable!()
                    }
                }).collect();
                Some(quote! { ::parse_that::sp_any(&[#(#lits),*]) })
            } else {
                let mut iter = branches.iter();
                let mut acc = try_generate_span_expr(iter.next()?, grammar_attrs)?;
                for branch in iter {
                    let next = try_generate_span_expr(branch, grammar_attrs)?;
                    acc = quote! { #acc.or(#next) };
                }
                Some(acc)
            }
        }

        // Many (zero or more) → .many_span(..)
        Expression::Many(token) => {
            let inner = try_generate_span_expr(&token.value, grammar_attrs)?;
            Some(quote! { #inner.many_span(..) })
        }

        // Many1 (one or more) → .many_span(1..)
        Expression::Many1(token) => {
            let inner = try_generate_span_expr(&token.value, grammar_attrs)?;
            Some(quote! { #inner.many_span(1..) })
        }

        // Optional → .opt_span()
        Expression::Optional(token) => {
            let inner = try_generate_span_expr(&token.value, grammar_attrs)?;
            Some(quote! { #inner.opt_span() })
        }

        // OptionalWhitespace → .trim_whitespace()
        Expression::OptionalWhitespace(token) => {
            let inner = try_generate_span_expr(&token.value, grammar_attrs)?;
            Some(quote! { #inner.trim_whitespace() })
        }

        // Skip (left << right): parse left then right, return right's span.
        // SpanParser::skip_span semantics: self is consumed, next's span returned.
        Expression::Skip(left, right) => {
            let l = try_generate_span_expr(&left.value, grammar_attrs)?;
            let r = try_generate_span_expr(&right.value, grammar_attrs)?;
            Some(quote! { #r.skip_span(#l) })
        }

        // Next (left >> right): parse left then right, return left's span.
        // SpanParser::next_after semantics: self's span kept, next consumed.
        Expression::Next(left, right) => {
            let l = try_generate_span_expr(&left.value, grammar_attrs)?;
            let r = try_generate_span_expr(&right.value, grammar_attrs)?;
            Some(quote! { #l.next_after(#r) })
        }

        // Minus (main - excluded): match main only if excluded fails.
        Expression::Minus(main_expr, excluded_expr) => {
            let main = try_generate_span_expr(&main_expr.value, grammar_attrs)?;
            let excluded = try_generate_span_expr(&excluded_expr.value, grammar_attrs)?;
            Some(quote! { #main.minus_span(#excluded) })
        }

        // Expressions that can't be represented as SpanParser.
        Expression::MappingFn(_)
        | Expression::MappedExpression(_)
        | Expression::DebugExpression(_)
        | Expression::ProductionRule(_, _) => None,
    }
}

pub(crate) fn format_generated_parsers<'a, 'b>(
    generated_parsers: &'a GeneratedParserCache<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> proc_macro2::TokenStream
where
    'a: 'b,
{
    let mut methods: Vec<proc_macro2::TokenStream> = Vec::new();

    for (expr, parser) in generated_parsers.iter() {
        let Expression::Nonterminal(Token { value: name, ..}) = expr else {
            panic!("Expected nonterminal");
        };
        let ident = format_ident!("{}", name);

        // Phase B: Transparent rules return Box<Enum> directly.
        let is_transparent = grammar_attrs.transparent_rules
            .is_some_and(|set| set.contains(name.as_ref()));
        let ty = if is_transparent {
            grammar_attrs.boxed_enum_type
        } else {
            grammar_attrs.enum_type
        };

        // make the parser lazy if it's a non_acyclic dep:
        let mut parser = if grammar_attrs.non_acyclic_deps.contains_key(expr) {
            quote! { ::parse_that::lazy(|| #parser) }
        } else {
            parser.clone()
        };

        // Apply @recover wrapping if this rule has a recovery directive.
        if let Some(recovers) = grammar_attrs.recovers {
            if let Some(sync_expr) = recovers.get(name.as_ref()) {
                let sync_parser = bbnf::compile_sync_expression(sync_expr);
                let enum_ident = grammar_attrs.enum_ident;
                parser = if is_transparent {
                    // Transparent rules return Box<Enum> — sentinel needs Box wrapping.
                    quote! {
                        #parser.recover(#sync_parser.map(|_| ()), Box::new(#enum_ident::Recovered))
                    }
                } else {
                    // Normal rules return Enum directly.
                    quote! {
                        #parser.recover(#sync_parser.map(|_| ()), #enum_ident::Recovered)
                    }
                };
            }
        }

        methods.push(quote! {
            pub fn #ident<'a>() -> Parser<'a, #ty> {
                #parser
            }
        });

        // Phase D: Generate _sp() method for span-eligible rules.
        let is_span_eligible = grammar_attrs.span_eligible_rules
            .is_some_and(|set| set.contains(name.as_ref()));
        if is_span_eligible {
            if let Some(sp_parser) = try_generate_span_parser(name, grammar_attrs) {
                let sp_ident = format_ident!("{}_sp", name);
                methods.push(quote! {
                    #[inline(always)]
                    pub fn #sp_ident<'a>() -> ::parse_that::SpanParser<'a> {
                        #sp_parser
                    }
                });
            }
        }
    }

    quote! {
        #(#methods)*
    }
}
