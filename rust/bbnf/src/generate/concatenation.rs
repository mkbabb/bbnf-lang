//! Concatenation expression codegen and dependency degree helpers.
//!
//! Extracted from `codegen.rs`. Contains `calculate_concatenation_expression()`,
//! `calculate_acyclic_deps_degree()`, and `calculate_non_acyclic_deps_degree()`.

use crate::analysis::Dependencies;
use crate::types::*;

use super::codegen::calculate_parser_from_expression;
use super::patterns::concat_element_sp_name;
use super::type_inference::*;
use super::types::*;

use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse_quote;

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
    //
    // Guard: if ALL elements would become Span after override, don't apply it — otherwise
    // the concatenation collapses to a single Span which contaminates rule-level types.
    let overridden_tys: Vec<syn::Type> = inner_exprs
        .iter()
        .map(|expr| {
            if concat_element_sp_name(expr, grammar_attrs).is_some() {
                parse_quote!(::parse_that::Span<'a>)
            } else {
                calculate_expression_type(expr, grammar_attrs, cache_bundle)
            }
        })
        .collect();
    let use_sp_override = !overridden_tys.iter().all(type_is_span);
    let tys: Vec<syn::Type> = if use_sp_override {
        overridden_tys
    } else {
        inner_exprs.iter().map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle)).collect()
    };
    let mut chains: Vec<(bool, Vec<TokenStream>)> = Vec::new();
    for (parser, ty) in inner_exprs
        .iter()
        .map(|expr| {
            if use_sp_override {
                if let Some(name) = concat_element_sp_name(expr, grammar_attrs) {
                    let sp_ident = format_ident!("{}_sp", name);
                    return quote! { Self::#sp_ident().into_parser() };
                }
            }
            calculate_parser_from_expression(expr, grammar_attrs, cache_bundle, max_depth, depth)
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
    // Invariant: concatenation expressions always have at least one element,
    // so the fold above always produces Some.
    acc.unwrap()
}
