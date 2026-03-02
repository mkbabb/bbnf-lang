extern crate proc_macro;

mod span_codegen;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use bbnf::calculate_ast_deps;
use bbnf::calculate_nonterminal_generated_parsers;
use bbnf::calculate_nonterminal_types;
use bbnf::generate_prettify;

use bbnf::analysis::{
    tarjan_scc, topological_sort_scc, calculate_acyclic_deps_scc, calculate_non_acyclic_deps_scc,
    compute_first_sets, find_aliases, find_transparent_alternations,
    find_span_eligible_rules,
};
use bbnf::BBNFGrammar;
use bbnf::Expression;
use bbnf::GeneratedGrammarAttributes;
use bbnf::ParserAttributes;
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

use span_codegen::{
    generate_enum, generate_grammar_arr, try_generate_span_parser, format_generated_parsers,
};

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
        // SAFETY: Leak the source string to get 'static lifetime for the AST.
        // Acceptable in a proc-macro context — the compiler process exits after expansion.
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
                // SAFETY: Leak to get 'static lifetime — acceptable in proc-macro context.
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

    // Phase 1.6: Alias detection
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);

    // Phase B: Transparent alternation detection
    let transparent_rules = find_transparent_alternations(&ast, &scc_result.cyclic_rules);

    // Phase D: Span-eligible rule detection
    let span_eligible_rules = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    // Phase E: Iterative fixed-point to compute which span-eligible rules get _sp() methods.
    // The recursive try_generate_span_parser needs sp_method_rules for nonterminal lookups,
    // but sp_method_rules depends on try_generate_span_parser succeeding. We iterate until
    // the set stabilizes (typically 2-3 iterations).
    let mut sp_method_rules: HashSet<String> = HashSet::new();
    loop {
        let next: HashSet<String> = span_eligible_rules
            .iter()
            .filter(|name| {
                let tmp_attrs = GeneratedGrammarAttributes {
                    ast: &ast,
                    deps: &deps,
                    acyclic_deps: &acyclic_deps,
                    non_acyclic_deps: &non_acyclic_deps,
                    first_sets: None,
                    aliases: None,
                    transparent_rules: None,
                    span_eligible_rules: Some(&span_eligible_rules),
                    sp_method_rules: Some(&sp_method_rules),
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
        if next == sp_method_rules {
            break;
        }
        sp_method_rules = next;
    }

    let recovers_ref = if recover_map.is_empty() { None } else { Some(&recover_map) };
    let pretties_ref = if pretty_map.is_empty() { None } else { Some(&pretty_map) };

    // First pass: type inference (sub_variants not yet known).
    let tmp_grammar_attrs = GeneratedGrammarAttributes {
        ast: &ast,
        deps: &deps,
        acyclic_deps: &acyclic_deps,
        non_acyclic_deps: &non_acyclic_deps,

        first_sets: Some(&first_sets),
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
