extern crate proc_macro;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use bbnf::calculate_ast_deps;
use bbnf::calculate_nonterminal_generated_parsers;
use bbnf::calculate_nonterminal_types;
use bbnf::generate_prettify;
use bbnf::get_nonterminal_name;

use bbnf::analysis::{
    tarjan_scc, topological_sort_scc, calculate_acyclic_deps_scc, calculate_non_acyclic_deps_scc,
    compute_first_sets, compute_ref_counts, find_aliases, find_transparent_alternations,
    find_span_eligible_rules,
};
use bbnf::BBNFGrammar;
use bbnf::Expression;
use bbnf::GeneratedGrammarAttributes;
use bbnf::GeneratedParserCache;
use bbnf::ParserAttributes;
use bbnf::Token;
use bbnf::TypeCache;
use bbnf::optimize::remove_direct_left_recursion;
use bbnf::imports::load_module_graph;
use indexmap::IndexMap;

use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::{
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Attribute, DeriveInput, Expr, ExprLit, Lit, Meta, Type,
};

use parse_that::utils::get_cargo_root_path;

fn parse_parser_attrs(attrs: &[Attribute]) -> ParserAttributes {
    let mut parser_attr = ParserAttributes::default();
    let root_path = get_cargo_root_path();

    for attr in attrs.iter().filter(|attr| attr.path().is_ident("parser")) {
        let Meta::List(meta_list) = &attr.meta else {
            continue;
        };

        let nested = meta_list
            .parse_args_with(Punctuated::<Meta, syn::Token![,]>::parse_terminated)
            .expect("failed to parse #[parser(...)] attribute");

        for meta in nested {
            match &meta {
                Meta::NameValue(nv) if nv.path.is_ident("path") => {
                    if let Expr::Lit(ExprLit { lit: Lit::Str(s), .. }) = &nv.value {
                        let path = PathBuf::from(s.value());
                        let path = if path.is_relative() {
                            root_path.join(path)
                        } else {
                            path
                        };
                        parser_attr.paths.push(path);
                    }
                }
                Meta::Path(p) if p.is_ident("ignore_whitespace") => {
                    parser_attr.ignore_whitespace = true;
                }
                Meta::Path(p) if p.is_ident("debug") => {
                    parser_attr.debug = true;
                }
                Meta::Path(p) if p.is_ident("use_string") => {
                    parser_attr.use_string = true;
                }
                Meta::Path(p) if p.is_ident("remove_left_recursion") => {
                    parser_attr.remove_left_recursion = true;
                }
                Meta::Path(p) if p.is_ident("prettify") => {
                    parser_attr.prettify = true;
                }
                _ => {}
            }
        }
    }
    parser_attr
}

fn generate_enum(
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
    let mut seen_sub_variant_names = std::collections::HashSet::new();
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

fn generate_grammar_arr(
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
fn try_generate_span_parser<'a>(
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

    // Unwrap Rule wrapper if present.
    let inner = match rhs {
        Expression::Rule(inner, _) => inner.as_ref(),
        other => other,
    };

    match inner {
        Expression::Regex(Token { value, .. }) => {
            if bbnf::generate::is_json_string_regex(value) {
                Some(quote! { ::parse_that::sp_json_string_quoted() })
            } else if bbnf::generate::is_json_number_regex(value) {
                Some(quote! { ::parse_that::sp_json_number() })
            } else {
                let pattern = value.as_ref();
                Some(quote! { ::parse_that::sp_regex(#pattern) })
            }
        }
        Expression::Literal(Token { value, .. }) => {
            let lit = value.as_ref();
            Some(quote! { ::parse_that::sp_string(#lit) })
        }
        Expression::Alternation(token) => {
            let branches = &token.value;
            // All-literal alternation → sp_any
            let all_lit = branches.iter().all(|b| matches!(b, Expression::Literal(_)));
            if all_lit {
                let lits: Vec<&str> = branches.iter().map(|b| {
                    if let Expression::Literal(Token { value, .. }) = b {
                        value.as_ref()
                    } else {
                        unreachable!()
                    }
                }).collect();
                Some(quote! { ::parse_that::sp_any(&[#(#lits),*]) })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn format_generated_parsers<'a, 'b>(
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

#[proc_macro_derive(Parser, attributes(parser))]
pub fn bbnf_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = &input.ident;
    let generics = &input.generics;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let enum_ident = format_ident!("{}Enum", ident);

    let enum_type: Type = parse_quote!(#enum_ident<'a>);
    let boxed_enum_type: Type = parse_quote!(Box<#enum_ident<'a>> );

    let parser_container_attrs = parse_parser_attrs(&input.attrs);

    // Try import-aware loading first: if the first file contains @import directives,
    // use load_module_graph() which handles DFS traversal, cycle detection, and
    // selective import resolution. Otherwise fall back to simple fold.
    let mut recover_map: HashMap<String, Expression<'static>> = HashMap::new();
    let mut pretty_map: HashMap<String, Vec<String>> = HashMap::new();

    let ast = if parser_container_attrs.paths.len() == 1 {
        let entry = &parser_container_attrs.paths[0];
        // Try parsing with import support to check for @import directives.
        let source = std::fs::read_to_string(entry)
            .unwrap_or_else(|_| panic!("Unable to read file: {}", entry.display()));
        let source_static: &'static str = Box::leak(source.clone().into_boxed_str());
        let parser = BBNFGrammar::grammar_with_imports();
        let (parsed, _) = parser.parse_return_state(source_static);

        if let Some(ref pg) = parsed {
            // Extract @recover directives.
            for rec in &pg.recovers {
                recover_map.insert(rec.rule_name.to_string(), rec.sync_expr.clone());
            }
            // Extract @pretty directives.
            for p in &pg.pretties {
                pretty_map.insert(
                    p.rule_name.to_string(),
                    p.hints.iter().map(|h| h.to_string()).collect(),
                );
            }

            if !pg.imports.is_empty() {
                // File has imports — use module graph loader.
                let registry = load_module_graph(entry)
                    .unwrap_or_else(|e| panic!("Import resolution failed: {}", e));
                if !registry.errors.is_empty() {
                    let msgs: Vec<String> = registry.errors.iter().map(|e| e.to_string()).collect();
                    panic!("Import errors:\n{}", msgs.join("\n"));
                }
                // Merge all modules in topological order (deps before dependents).
                let mut merged = IndexMap::new();
                for path in registry.paths() {
                    if let Some(module) = registry.get_module(path) {
                        // Also collect recovers from imported modules.
                        for rec in &module.grammar.recovers {
                            recover_map.entry(rec.rule_name.to_string())
                                .or_insert_with(|| rec.sync_expr.clone());
                        }
                        // Also collect pretties from imported modules.
                        for p in &module.grammar.pretties {
                            pretty_map.entry(p.rule_name.to_string())
                                .or_insert_with(|| p.hints.iter().map(|h| h.to_string()).collect());
                        }
                        for (name, expr) in &module.grammar.rules {
                            merged.insert(name.clone(), expr.clone());
                        }
                    }
                }
                merged
            } else {
                // No imports — use the already-parsed rules directly.
                pg.rules.clone()
            }
        } else {
            panic!("Unable to parse grammar: {}", entry.display());
        }
    } else {
        // Multiple explicit paths: simple fold (legacy behavior).
        // Leak strings to get 'static lifetime (proc-macro runs once at compile time).
        let file_strs: Vec<&'static str> = parser_container_attrs
            .paths
            .iter()
            .map(|path| {
                let s = std::fs::read_to_string(path)
                    .unwrap_or_else(|_| panic!("Unable to read file: {}", path.display()));
                &*Box::leak(s.into_boxed_str())
            })
            .collect();

        file_strs
            .iter()
            .map(|file_string| {
                BBNFGrammar::grammar()
                    .parse(file_string)
                    .expect("Unable to parse grammar")
            })
            .fold(IndexMap::new(), |mut acc, ast| {
                for (name, expr) in ast {
                    acc.insert(name, expr);
                }
                acc
            })
    };

    // Phase 2.2: Optionally remove direct left-recursion before analysis.
    let ast = if parser_container_attrs.remove_left_recursion {
        let transformed = remove_direct_left_recursion(&ast);
        // Convert back to IndexMap preserving insertion order
        transformed.into_iter().collect::<IndexMap<_, _>>()
    } else {
        ast
    };

    let deps = calculate_ast_deps(&ast);

    // Phase 1.1: Tarjan SCC — O(V+E) cycle detection + topological ordering
    let scc_result = tarjan_scc(&deps);
    let ast = topological_sort_scc(&ast, &scc_result, &deps);

    // O(V+E) acyclic/non-acyclic classification.
    // Nodes with cycles OR diamond dependencies are classified as non-acyclic.
    let acyclic_deps = calculate_acyclic_deps_scc(&deps, &scc_result);
    let non_acyclic_deps = calculate_non_acyclic_deps_scc(&deps, &acyclic_deps);

    // Phase 1.2: Compute FIRST sets for dispatch table generation
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);

    // Phase 1.6: Reference counting and alias detection
    let ref_counts = compute_ref_counts(&deps);
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);

    // Phase B: Transparent alternation detection
    let transparent_rules = find_transparent_alternations(&ast, &scc_result.cyclic_rules);

    // Phase D: Span-eligible rule detection
    let span_eligible_rules = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    // Phase E: Compute which span-eligible rules actually get _sp() methods
    // (only those whose RHS maps to a known SpanParser pattern).
    let sp_method_rules: HashSet<String> = span_eligible_rules
        .iter()
        .filter(|name| {
            // Temporarily build a minimal grammar_attrs just for the check.
            // try_generate_span_parser only needs the AST.
            let tmp_attrs = GeneratedGrammarAttributes {
                ast: &ast,
                deps: &deps,
                acyclic_deps: &acyclic_deps,
                non_acyclic_deps: &non_acyclic_deps,
                first_sets: None,
                ref_counts: None,
                aliases: None,
                transparent_rules: None,
                span_eligible_rules: None,
                sp_method_rules: None,
                recovers: None,
                pretties: None,
                sub_variants: None,
                ident,
                enum_ident: &enum_ident,
                enum_type: &enum_type,
                boxed_enum_type: &boxed_enum_type,
                parser_container_attrs: &parser_container_attrs,
            };
            try_generate_span_parser(name, &tmp_attrs).is_some()
        })
        .cloned()
        .collect();

    let recovers_ref = if recover_map.is_empty() { None } else { Some(&recover_map) };
    let pretties_ref = if pretty_map.is_empty() { None } else { Some(&pretty_map) };

    // First pass: type inference (sub_variants not yet known).
    let tmp_grammar_attrs = GeneratedGrammarAttributes {
        ast: &ast,
        deps: &deps,
        acyclic_deps: &acyclic_deps,
        non_acyclic_deps: &non_acyclic_deps,

        first_sets: Some(&first_sets),
        ref_counts: Some(&ref_counts),
        aliases: Some(&aliases),
        transparent_rules: Some(&transparent_rules),
        span_eligible_rules: Some(&span_eligible_rules),
        sp_method_rules: Some(&sp_method_rules),
        recovers: recovers_ref,
        pretties: pretties_ref,
        sub_variants: None,

        ident,
        enum_ident: &enum_ident,

        enum_type: &enum_type,
        boxed_enum_type: &boxed_enum_type,

        parser_container_attrs: &parser_container_attrs,
    };

    let (nonterminal_types, sub_variants) = calculate_nonterminal_types(&tmp_grammar_attrs);

    // Rebuild grammar_attrs with sub_variants for codegen.
    let grammar_attrs = GeneratedGrammarAttributes {
        ast: &ast,
        deps: &deps,
        acyclic_deps: &acyclic_deps,
        non_acyclic_deps: &non_acyclic_deps,

        first_sets: Some(&first_sets),
        ref_counts: Some(&ref_counts),
        aliases: Some(&aliases),
        transparent_rules: Some(&transparent_rules),
        span_eligible_rules: Some(&span_eligible_rules),
        sp_method_rules: Some(&sp_method_rules),
        recovers: recovers_ref,
        pretties: pretties_ref,
        sub_variants: Some(&sub_variants),

        ident,
        enum_ident: &enum_ident,

        enum_type: &enum_type,
        boxed_enum_type: &boxed_enum_type,

        parser_container_attrs: &parser_container_attrs,
    };

    let grammar_arr = generate_grammar_arr(&grammar_attrs, &parser_container_attrs);
    let grammar_enum = generate_enum(&grammar_attrs, &nonterminal_types, &sub_variants);

    let generated_parsers =
        calculate_nonterminal_generated_parsers(&grammar_attrs, &nonterminal_types, &sub_variants);

    let generated_parsers = format_generated_parsers(&generated_parsers, &grammar_attrs);

    // Optionally generate prettify (to_doc + source_range) methods.
    let prettify_impl = if parser_container_attrs.prettify {
        generate_prettify(&grammar_attrs, &nonterminal_types)
    } else {
        quote! {}
    };

    let expanded = quote! {
        // Re-export parse_that items so generated code can reference traits
        // (ParserSpan, ParserFlat) and functions (lazy, string_span, etc.).
        use ::parse_that::*;

        #grammar_arr

        #grammar_enum

         impl #impl_generics #ident #ty_generics #where_clause {
            #generated_parsers
        }

        #prettify_impl
    };

    expanded.into()
}
