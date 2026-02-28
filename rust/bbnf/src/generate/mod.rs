//! Rust parser code generation from BBNF grammars.
//!
//! This module translates a parsed and analysed BBNF grammar into
//! `proc_macro2::TokenStream` parser combinator code. The pipeline:
//!
//! 1. **Type inference** — map each expression to a `syn::Type`.
//! 2. **Pattern detection** — recognize sep-by, wrapped, regex coalescing, etc.
//! 3. **Codegen** — emit parser combinators, dispatch tables, and enum wrappers.
//! 4. **Orchestration** — `calculate_nonterminal_generated_parsers` ties it all together.

mod types;
mod type_inference;
mod patterns;
mod codegen;

// Re-export everything publicly so downstream code sees the same API.
pub use types::*;
pub use type_inference::*;
pub use patterns::*;
pub use codegen::*;

use crate::analysis::get_nonterminal_name;

use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};
use quote::quote;

pub fn calculate_nonterminal_generated_parsers<'a>(
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    type_cache: &'a TypeCache<'a>,
) -> GeneratedParserCache<'a> {
    let cache_bundle = CacheBundle {
        parser_cache: Rc::new(RefCell::new(GeneratedParserCache::new())),
        type_cache: Rc::new(RefCell::new(type_cache.clone())),
        inline_cache: Rc::new(RefCell::new(InlineCache::new())),
    };
    let mut acyclic_deps_degree = calculate_acyclic_deps_degree(grammar_attrs.acyclic_deps);
    calculate_non_acyclic_deps_degree(grammar_attrs.deps, &mut acyclic_deps_degree);

    let formatted = grammar_attrs
        .ast
        .iter()
        .map(|(lhs, rhs)| {
            let rhs = match get_nonterminal_name(lhs) {
                Some(name) => {
                    format_parser(name, rhs, grammar_attrs.parser_container_attrs)
                }
                None => rhs.clone(),
            };
            (lhs.clone(), rhs)
        })
        .collect::<HashMap<_, _>>();

    let mapped: HashMap<_, _> = grammar_attrs
        .ast
        .iter()
        .map(|(lhs, rhs)| {
            let rhs = match get_nonterminal_name(lhs) {
                Some(name) => {
                    let formatted_expr = formatted.get(lhs).unwrap_or(rhs);
                    // Phase B: Transparent rules skip the enum variant wrapper.
                    // The alternation branches already produce the correct inner type.
                    if is_transparent_rule(name, grammar_attrs) {
                        formatted_expr.clone()
                    } else {
                        map_generated_parser(name, formatted_expr, grammar_attrs.enum_ident)
                    }
                }
                None => rhs.clone(),
            };
            (lhs.clone(), rhs)
        })
        .collect();

    let _boxed: HashMap<_, _> = grammar_attrs
        .ast
        .iter()
        .map(|(lhs, rhs)| {
            let rhs = match get_nonterminal_name(lhs) {
                Some(name) => {
                    let formatted_expr = formatted.get(lhs).unwrap_or(rhs);
                    if is_transparent_rule(name, grammar_attrs) {
                        formatted_expr.clone()
                    } else {
                        box_generated_parser(name, formatted_expr, grammar_attrs.enum_ident)
                    }
                }
                None => rhs.clone(),
            };
            (lhs.clone(), rhs)
        })
        .collect();

    let boxed2: HashMap<_, _> = grammar_attrs
        .ast
        .iter()
        .map(|(lhs, rhs)| {
            let rhs = match get_nonterminal_name(lhs) {
                Some(name) => {
                    let formatted_expr = formatted.get(lhs).unwrap_or(rhs);
                    if is_transparent_rule(name, grammar_attrs) {
                        formatted_expr.clone()
                    } else {
                        box_generated_parser2(name, formatted_expr, grammar_attrs.enum_ident)
                    }
                }
                None => rhs.clone(),
            };
            (lhs.clone(), rhs)
        })
        .collect();

    formatted
        .iter()
        .filter(|(lhs, _)| grammar_attrs.acyclic_deps.contains_key(lhs))
        .for_each(|(lhs, rhs)| {
            cache_bundle.inline_cache.borrow_mut().insert(lhs, rhs);
        });

    let generate = |recursive_inline: bool| {
        grammar_attrs
            .ast
            .iter()
            .filter_map(|(lhs, rhs)| {
                let is_acyclic = grammar_attrs.acyclic_deps.contains_key(lhs);

                if !is_acyclic {
                    grammar_attrs
                        .deps
                        .get(lhs)
                        .unwrap()
                        .iter()
                        .filter(|dep| grammar_attrs.acyclic_deps.contains_key(dep))
                        .for_each(|dep| {
                            let rhs = boxed2.get(dep).unwrap_or(dep);
                            cache_bundle.inline_cache.borrow_mut().insert(dep, rhs);
                            cache_bundle
                                .type_cache
                                .borrow_mut()
                                .insert(dep, grammar_attrs.boxed_enum_type.clone());
                        });
                } else if recursive_inline {
                    return None;
                }

                let max_depth = *acyclic_deps_degree.get(lhs).unwrap_or(&1);

                let rhs = mapped.get(lhs).unwrap_or(rhs);

                let parser = calculate_parser_from_expression(
                    rhs,
                    grammar_attrs,
                    &cache_bundle,
                    max_depth,
                    0,
                );
                Some((lhs, parser))
            })
            .collect()
    };
    let mut acyclic_generated_parsers: HashMap<_, _> = generate(false);
    *cache_bundle.parser_cache.borrow_mut() = acyclic_generated_parsers.clone();

    grammar_attrs.ast.iter().for_each(|(lhs, rhs)| {
        let is_acyclic = grammar_attrs.acyclic_deps.contains_key(lhs);

        if !is_acyclic {
            let rhs = boxed2.get(lhs).unwrap_or(rhs);

            acyclic_generated_parsers.remove(lhs);
            cache_bundle.parser_cache.borrow_mut().remove(lhs);
            cache_bundle.inline_cache.borrow_mut().insert(lhs, rhs);
        } else {
            let tmp = cache_bundle.parser_cache.borrow().get(lhs).map(|parser| {
                quote! { #parser.map(Box::new) }
            });
            if let Some(parser) = tmp {
                cache_bundle.parser_cache.borrow_mut().insert(lhs, parser);
            }
        }
    });

    let mut generated_parsers = generate(true);
    generated_parsers.extend(acyclic_generated_parsers);

    generated_parsers
}
