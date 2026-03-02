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
mod alternation;
mod concatenation;
mod prettify;

// Re-export everything publicly so downstream code sees the same API.
pub use types::*;
pub use type_inference::*;
pub use patterns::*;
pub use codegen::*;
pub use alternation::*;
pub use concatenation::*;
pub use prettify::*;

use crate::analysis::get_nonterminal_name;
use crate::types::Expression;

use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};
use quote::{format_ident, quote};

pub fn calculate_nonterminal_generated_parsers<'a>(
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    type_cache: &'a TypeCache<'a>,
    _sub_variants: &'a SubVariantCache,
) -> GeneratedParserCache<'a> {
    let cache_bundle = CacheBundle {
        parser_cache: Rc::new(RefCell::new(GeneratedParserCache::new())),
        type_cache: Rc::new(RefCell::new(type_cache.clone())),
        inline_cache: Rc::new(RefCell::new(InlineCache::new())),
        current_rule_name: RefCell::new(None),
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
        // Skip inlining rules with sub-variants (heterogeneous alternations)
        .filter(|(lhs, _)| {
            let name = get_nonterminal_name(lhs).unwrap_or("");
            !grammar_attrs
                .sub_variants
                .is_some_and(|sv| sv.contains_key(name))
        })
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
                        // Skip inlining deps with sub-variants (heterogeneous alternations)
                        // — their branches have diverse types that can't be uniformly mapped.
                        .filter(|dep| {
                            let dep_name = get_nonterminal_name(dep).unwrap_or("");
                            !grammar_attrs
                                .sub_variants
                                .is_some_and(|sv| sv.contains_key(dep_name))
                        })
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

                // Set the current rule name for sub-variant lookup in alternation codegen.
                if let Some(name) = get_nonterminal_name(lhs) {
                    *cache_bundle.current_rule_name.borrow_mut() = Some(name.to_string());
                }

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
            // The boxed2 wrapper produces Box<Enum>, so update the type_cache to match.
            // Without this, calculate_expression_type returns the PRE-wrapper body type,
            // causing alternation coercion to apply incorrect sub-variant wrappers.
            cache_bundle
                .type_cache
                .borrow_mut()
                .insert(lhs, grammar_attrs.boxed_enum_type.clone());
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

/// Compile a simple BBNF expression (regex, literal, alternation, concatenation)
/// into a standalone parser token stream. Used for `@recover` sync expressions
/// which are always simple patterns, not referencing nonterminals.
pub fn compile_sync_expression(expr: &Expression<'_>) -> proc_macro2::TokenStream {
    use crate::types::Token;
    match expr {
        Expression::Regex(Token { value, .. }) => {
            let pattern = value.as_ref();
            quote! { ::parse_that::regex_span(#pattern) }
        }
        Expression::Literal(Token { value, .. }) => {
            let unescaped = codegen::unescape_literal(value.as_ref());
            let lit = proc_macro2::Literal::string(&unescaped);
            quote! { ::parse_that::string_span(#lit) }
        }
        Expression::Alternation(token) => {
            let branches: Vec<_> = token.value.iter().map(|e| compile_sync_expression(e)).collect();
            quote! { ( #(#branches)|* ) }
        }
        Expression::Concatenation(token) => {
            let parts: Vec<_> = token.value.iter().map(|e| compile_sync_expression(e)).collect();
            if parts.len() == 1 {
                parts.into_iter().next().unwrap()
            } else {
                let first = &parts[0];
                let rest = &parts[1..];
                quote! { #first #(.then_span(#rest))* }
            }
        }
        Expression::Many(inner) => {
            let p = compile_sync_expression(&inner.value);
            quote! { #p.many_span(..) }
        }
        Expression::Many1(inner) => {
            let p = compile_sync_expression(&inner.value);
            quote! { #p.many_span(1..) }
        }
        Expression::Optional(inner) => {
            let p = compile_sync_expression(&inner.value);
            quote! { #p.opt_span() }
        }
        Expression::Group(inner) => compile_sync_expression(&inner.value),
        Expression::Nonterminal(Token { value, .. }) => {
            let ident = format_ident!("{}", value.as_ref());
            quote! { Self::#ident() }
        }
        _ => {
            panic!("Unsupported expression type in @recover sync expression: {:?}", expr);
        }
    }
}
