//! Alternation expression codegen — dispatch tables, branch coercion, inline flat alternation.
//!
//! Extracted from `codegen.rs`. Contains `coerce_alternation_branches()` and
//! `calculate_alternation_expression()`.

use crate::analysis::build_dispatch_table;
use crate::types::*;

use super::codegen::{calculate_parser_from_expression, map_span_if_needed};
use super::patterns::check_for_any_span;
use super::type_inference::*;
use super::types::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

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

    // Inline flat alternation — emit a single closure with all branches.
    // This creates one Box<dyn ParserFn> instead of N-1 .or() boxes.
    if parsers.len() == 1 {
        return parsers.into_iter().next().unwrap();
    }
    if parsers.len() <= 8 {
        // Emit let bindings + inline match closure
        let bindings: Vec<TokenStream> = parsers.iter().enumerate().map(|(i, p)| {
            let ident = format_ident!("_alt_{}", i);
            quote! { let #ident = #p; }
        }).collect();
        let arms: Vec<TokenStream> = (0..parsers.len()).map(|i| {
            let ident = format_ident!("_alt_{}", i);
            if i < parsers.len() - 1 {
                quote! {
                    if let Some(v) = #ident.call(state) { return Some(v); }
                    state.furthest_offset = state.furthest_offset.max(state.offset);
                    state.offset = cp;
                }
            } else {
                // Last branch: no checkpoint restore needed
                quote! { #ident.call(state) }
            }
        }).collect();
        return quote! {
            {
                #(#bindings)*
                ::parse_that::Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                    let cp = state.offset;
                    #(#arms)*
                })
            }
        };
    }
    // More than 8 branches: fall back to .or() chain
    let parser = parsers
        .into_iter()
        .fold(None::<TokenStream>, |acc, parser| match acc {
            None => Some(parser),
            Some(acc) => Some(quote! { #acc | #parser }),
        })
        .unwrap();
    quote! { (#parser) }
}
