//! Type inference for grammar expressions.
//!
//! Maps each `Expression` to its corresponding `syn::Type` (Span, Option, Vec,
//! tuple, or Box<Enum>). Used by codegen to determine parser output types.

use crate::types::*;

use super::patterns::concat_element_sp_name;
use super::types::*;

use quote::ToTokens;
use syn::{parse_quote, Type};

/// Structurally compare two `syn::Type` values for equality.
///
/// Serializing `TokenStream` to string and comparing is fragile because
/// whitespace and ordering differences can cause false negatives. This
/// function normalizes by round-tripping through the token stream: it
/// compares the token trees element-by-element, ignoring spacing.
pub fn types_eq(a: &Type, b: &Type) -> bool {
    // Compare via normalized token streams — each token tree is compared
    // by its Display representation (which is spacing-independent for
    // individual tokens). This is more reliable than comparing the raw
    // string output which includes arbitrary whitespace.
    let a_tokens: Vec<String> = a
        .to_token_stream()
        .into_iter()
        .map(|tt| tt.to_string())
        .collect();
    let b_tokens: Vec<String> = b
        .to_token_stream()
        .into_iter()
        .map(|tt| tt.to_string())
        .collect();
    a_tokens == b_tokens
}

pub fn is_transparent_rule(name: &str, grammar_attrs: &GeneratedGrammarAttributes) -> bool {
    grammar_attrs
        .transparent_rules
        .is_some_and(|set| set.contains(name))
}

/// Check whether a type is `parse_that::Span` (with or without leading `::`).
///
/// Uses structural `syn::Ident` comparison on path segments — no string
/// serialization involved. Matches both `parse_that::Span<'a>` and
/// `::parse_that::Span<'a>`.
pub fn type_is_span(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        let segments = &type_path.path.segments;
        if segments.len() != 2 {
            return false;
        }
        segments[0].ident == "parse_that" && segments[1].ident == "Span"
    } else {
        false
    }
}

pub fn calculate_expression_type<'a>(
    expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
) -> Type {
    fn get_and_parse_default_parser_ty<'a>(
        name: &str,
        grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    ) -> Option<Type> {
        let GeneratedNonterminalParser { ty, .. } = DEFAULT_PARSERS.get(name)?;
        let Ok(ty) = syn:: parse_str::< Type >(ty) else {
            return None;
        };
        if grammar_attrs.parser_container_attrs.use_string && type_is_span(&ty) {
            Some(parse_quote! {
                & 'a str
            })
        } else {
            Some(ty)
        }
    }

    if let Some(ty) = cache_bundle.type_cache.borrow_mut().get(expr) {
        return ty.clone();
    }
    if let Some(cached_expr) = cache_bundle.inline_cache.borrow().get(expr) {
        return calculate_expression_type(cached_expr, grammar_attrs, cache_bundle);
    }
    let ty = match expr {
        // Invariant: "LITERAL" and "REGEX" are hardcoded keys in DEFAULT_PARSERS
        // and always resolve to valid types.
        Expression::Literal(_) => {
            get_and_parse_default_parser_ty("LITERAL", grammar_attrs).unwrap()
        }
        Expression::Regex(_) => get_and_parse_default_parser_ty("REGEX", grammar_attrs).unwrap(),
        Expression::Epsilon(_) => parse_quote!(()),
        Expression::Nonterminal(Token { value, .. }) => {
            if let Some(ty) = get_and_parse_default_parser_ty(value, grammar_attrs) {
                ty
            } else {
                grammar_attrs.boxed_enum_type.clone()
            }
        }
        Expression::Group(inner_expr) => {
            let inner_expr = inner_expr.inner();
            calculate_expression_type(inner_expr, grammar_attrs, cache_bundle)
        }
        Expression::Optional(inner_expr) => {
            let inner_expr = inner_expr.inner();
            let inner_type = calculate_expression_type(inner_expr, grammar_attrs, cache_bundle);
            if type_is_span(&inner_type) {
                return inner_type;
            }
            parse_quote!(Option < #inner_type >)
        }
        Expression::OptionalWhitespace(inner_expr) => {
            let inner_expr = inner_expr.inner();
            calculate_expression_type(inner_expr, grammar_attrs, cache_bundle)
        }
        Expression::Many(inner_expr) | Expression::Many1(inner_expr) => {
            let inner_expr = inner_expr.inner();
            let inner_type = calculate_expression_type(inner_expr, grammar_attrs, cache_bundle);
            if type_is_span(&inner_type) {
                return inner_type;
            }
            parse_quote!(Vec < #inner_type >)
        }
        Expression::Skip(left_expr, _) => {
            let left_expr = left_expr.inner();
            let left_type = calculate_expression_type(left_expr, grammar_attrs, cache_bundle);
            return left_type;
        }
        Expression::Next(_, right_expr) => {
            let right_expr = right_expr.inner();
            let right_type = calculate_expression_type(right_expr, grammar_attrs, cache_bundle);
            return right_type;
        }
        Expression::Minus(left_expr, _) => {
            let left_expr = left_expr.inner();
            let left_type = calculate_expression_type(left_expr, grammar_attrs, cache_bundle);
            return left_type;
        }
        Expression::Concatenation(inner_exprs) => {
            let inner_exprs = inner_exprs.inner();
            // Phase E: Override sp_method_rules nonterminals to Span in concatenation context.
            // This must agree with concatenation codegen (concatenation.rs) which substitutes
            // Self::rule_sp().into_parser() for these elements.
            //
            // Guard: if ALL elements would become Span after override, DON'T apply the
            // override — otherwise the concatenation collapses to a single Span type,
            // which contaminates the TypeCache and causes Many/Optional handlers to
            // incorrectly emit .many_span()/.opt_span() on non-Span parsers.
            let overridden: Vec<Type> = inner_exprs
                .iter()
                .map(|expr| {
                    if concat_element_sp_name(expr, grammar_attrs).is_some() {
                        parse_quote!(::parse_that::Span<'a>)
                    } else {
                        calculate_expression_type(expr, grammar_attrs, cache_bundle)
                    }
                })
                .collect();
            let tys = if overridden.iter().all(type_is_span) {
                // All Span → don't apply override, use true types to avoid collapse.
                inner_exprs
                    .iter()
                    .map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle))
                    .collect::<Vec<_>>()
            } else {
                overridden
            };
            let mut span_counter = 0;
            let mut non_span_counter = 0;
            let mut new_tys = Vec::new();
            for ty in tys.iter() {
                if type_is_span(ty) {
                    span_counter += 1;
                    non_span_counter = 0;
                } else {
                    span_counter = 0;
                    non_span_counter += 1;
                }
                if span_counter == 1 {
                    new_tys.push(parse_quote!(::parse_that::Span<'a>));
                } else if non_span_counter > 0 {
                    new_tys.push(ty.clone());
                }
            }
            if new_tys.len() == 1 {
                return new_tys[0].clone();
            }
            parse_quote!((#(#new_tys), *))
        }
        Expression::Alternation(inner_exprs) => {
            let inner_exprs = inner_exprs.inner();
            let tys = inner_exprs
                .iter()
                .map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle))
                .collect::<Vec<_>>();
            let is_all_span = tys.iter().all(type_is_span);
            let is_all_same = tys.iter().all(|ty| types_eq(ty, &tys[0]));
            if is_all_span || is_all_same {
                tys[0].clone()
            } else {
                grammar_attrs.boxed_enum_type.clone()
            }
        }
        Expression::Rule(rhs, mapping_fn) => {
            if let Some(inner) = mapping_fn {
                if let Expression::MappingFn(Token { value, .. }) = inner.as_ref() {
                    let mapping_fn = syn::parse_str::<syn::ExprClosure>(value)
                        .unwrap_or_else(|e| panic!(
                            "Mapping function `{}` is not a valid Rust closure. \
                             Parse error: {}", value, e
                        ));
                    let syn::ReturnType::Type(_, ty) = &mapping_fn.output else {
                        panic!(
                            "Mapping function `{}` must have an explicit return type \
                             (e.g. `|x| -> MyType {{ ... }}`). The return type is needed \
                             for type inference during code generation.", value
                        );
                    };
                    ty.as_ref().clone()
                } else {
                    calculate_expression_type(rhs, grammar_attrs, cache_bundle)
                }
            } else {
                calculate_expression_type(rhs, grammar_attrs, cache_bundle)
            }
        }
        _ => panic!(
            "Unsupported expression type for type inference: {:?}. \
             This expression variant has no type inference handler. \
             Check that your grammar only uses supported BBNF constructs.",
            expr
        ),
    };
    cache_bundle
        .type_cache
        .borrow_mut()
        .insert(expr, ty.clone());
    ty
}

pub fn calculate_nonterminal_types<'a>(
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> (TypeCache<'a>, SubVariantCache) {
    let cache_bundle = CacheBundle {
        parser_cache: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        type_cache: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        inline_cache: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        current_rule_name: std::cell::RefCell::new(None),
    };
    let mut sub_variants = SubVariantCache::new();

    grammar_attrs
        .ast
        .iter()
        .filter(|(lhs, _)| grammar_attrs.acyclic_deps.contains_key(lhs))
        .for_each(|(lhs, rhs)| {
            cache_bundle.inline_cache.borrow_mut().insert(lhs, rhs);
        });
    let type_cache: TypeCache = grammar_attrs
        .ast
        .iter()
        .map(|(lhs, rhs)| {
            if !grammar_attrs.acyclic_deps.contains_key(lhs) {
                if let Some(deps) = grammar_attrs.deps.get(lhs) {
                    for dep in deps
                        .iter()
                        .filter(|dep| grammar_attrs.acyclic_deps.contains_key(dep))
                    {
                        cache_bundle
                            .type_cache
                            .borrow_mut()
                            .insert(dep, grammar_attrs.boxed_enum_type.clone());
                        cache_bundle.inline_cache.borrow_mut().remove(dep);
                    }
                }
            }
            let ty = calculate_expression_type(rhs, grammar_attrs, &cache_bundle);

            // Detect heterogeneous alternations that produce Box<Enum>.
            // For each branch with a non-Box<Enum> type, register a sub-variant.
            if let Some(name) = crate::analysis::get_nonterminal_name(lhs) {
                if !is_transparent_rule(name, grammar_attrs) {
                    collect_sub_variants(name, rhs, grammar_attrs, &cache_bundle, &mut sub_variants);
                }
            }

            (lhs, ty)
        })
        .collect();
    // Validate: no two sub-variants from DIFFERENT rules should produce
    // structurally identical types. The flat coercion lookup in alternation codegen
    // scans all sub-variants and picks the first match by type. If two rules
    // have sub-variants with the same type, the wrong variant could be selected.
    validate_sub_variant_uniqueness(&sub_variants);

    (type_cache, sub_variants)
}

/// Validate that no two rules define sub-variants with structurally identical types.
///
/// Within a single rule, identical types are expected (they reuse the same variant
/// name). Across rules, identical types would cause the flat coercion lookup in
/// `coerce_alternation_branches` to pick an arbitrary (potentially wrong) variant.
fn validate_sub_variant_uniqueness(sub_variants: &SubVariantCache) {
    // Collect (type, rule_name, variant_name) tuples from all rules.
    let mut type_to_origin: Vec<(&Type, &str, &str)> = Vec::new();

    for (rule_name, variants) in sub_variants.iter() {
        // Deduplicate within a rule — identical types within one rule share a variant.
        let mut seen_in_rule: Vec<&Type> = Vec::new();
        for (variant_name, vty) in variants {
            if seen_in_rule.iter().any(|seen| types_eq(seen, vty)) {
                continue; // Same type reused within same rule — expected, skip.
            }
            seen_in_rule.push(vty);

            // Check against sub-variants from other rules.
            if let Some((_, other_rule, other_variant)) = type_to_origin
                .iter()
                .find(|(seen_ty, seen_rule, _)| types_eq(seen_ty, vty) && *seen_rule != rule_name.as_str())
            {
                panic!(
                    "Sub-variant coercion ambiguity: rule `{}` variant `{}` and rule `{}` \
                     variant `{}` both produce structurally identical type `{}`. \
                     The flat coercion lookup would pick the wrong variant. \
                     Consider making the branch types distinct or using an explicit \
                     mapping function.",
                    rule_name,
                    variant_name,
                    other_rule,
                    other_variant,
                    vty.to_token_stream(),
                );
            }
            type_to_origin.push((vty, rule_name, variant_name));
        }
    }
}

/// If `rhs` is an alternation with heterogeneous branch types (i.e. the overall
/// type is Box<Enum>), create sub-variant entries for branches that produce
/// non-Box<Enum> types (e.g. tuples). This allows codegen to wrap those branches.
fn collect_sub_variants<'a>(
    rule_name: &str,
    rhs: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
    cache_bundle: &'a CacheBundle<'a, '_, '_>,
    sub_variants: &mut SubVariantCache,
) {
    // Unwrap Rule wrapper if present.
    let inner = match rhs {
        Expression::Rule(inner, _) => inner.as_ref(),
        other => other,
    };

    let Expression::Alternation(inner_exprs) = inner else {
        return;
    };
    let inner_exprs = inner_exprs.inner();
    let tys: Vec<Type> = inner_exprs
        .iter()
        .map(|expr| calculate_expression_type(expr, grammar_attrs, cache_bundle))
        .collect();

    let is_all_span = tys.iter().all(type_is_span);
    let is_all_same = tys.iter().all(|ty| types_eq(ty, &tys[0]));

    // Only needed when the alternation is heterogeneous (overall type = Box<Enum>).
    if is_all_span || is_all_same {
        return;
    }

    let mut variants = Vec::new();
    // Track seen types structurally: store (normalized_token_key, variant_name)
    let mut seen_types: Vec<(Type, String)> = Vec::new();

    for (i, ty) in tys.iter().enumerate() {
        // Skip branches that already produce Box<Enum> — they need no wrapping.
        if types_eq(ty, grammar_attrs.boxed_enum_type) {
            continue;
        }
        // Reuse the same sub-variant name for branches with identical types.
        let variant_name =
            if let Some((_, existing)) = seen_types.iter().find(|(seen_ty, _)| types_eq(seen_ty, ty)) {
                existing.clone()
            } else {
                let name = format!("{}_{}", rule_name, i);
                seen_types.push((ty.clone(), name.clone()));
                name
            };
        variants.push((variant_name, ty.clone()));
    }

    if !variants.is_empty() {
        sub_variants.insert(rule_name.to_string(), variants);
    }
}
