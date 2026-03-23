extern crate proc_macro;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use bbnf::calculate_ast_deps;

use bbnf::analysis::{
    compute_first_sets, find_aliases, find_span_eligible_rules, find_transparent_alternations,
    tarjan_scc, topological_sort_scc,
};
use bbnf::imports::load_module_graph;
use bbnf::lower::lower_to_ir;
use bbnf::optimize::remove_direct_left_recursion;
use bbnf::BBNFGrammar;
use bbnf::Expression;
use bbnf::ParserAttributes;
use indexmap::IndexMap;

use proc_macro::TokenStream;

use syn::{
    parse_macro_input, punctuated::Punctuated, Attribute, DeriveInput, Expr, ExprLit, Lit, Meta,
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
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(s), ..
                    }) = &nv.value
                    {
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
                Meta::Path(p) if p.is_ident("skip_recover") => {
                    parser_attr.skip_recover = true;
                }
                Meta::Path(p) if p.is_ident("arena") => {
                    parser_attr.arena = true;
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

    let (_impl_generics, _ty_generics, _where_clause) = generics.split_for_impl();

    let parser_container_attrs = parse_parser_attrs(&input.attrs);

    // Try import-aware loading first: if the first file contains @import directives,
    // use load_module_graph() which handles DFS traversal, cycle detection, and
    // selective import resolution. Otherwise fall back to simple fold.
    let mut recover_map: HashMap<String, Expression<'static>> = HashMap::new();
    let mut pretty_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut no_collapse_set: HashSet<String> = HashSet::new();
    let mut inline_set: HashSet<String> = HashSet::new();
    let mut ws_pattern: Option<String> = None;

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
            // Extract @no_collapse directives.
            for nc in &pg.no_collapses {
                no_collapse_set.insert(nc.rule_name.to_string());
            }
            // Extract @ws directive.
            if let Some(ref pat) = pg.ws_pattern {
                ws_pattern = Some(pat.to_string());
            }
            // Extract @inline directives.
            for name in &pg.inline_rules {
                inline_set.insert(name.to_string());
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
                            recover_map
                                .entry(rec.rule_name.to_string())
                                .or_insert_with(|| rec.sync_expr.clone());
                        }
                        // Also collect pretties from imported modules.
                        for p in &module.grammar.pretties {
                            pretty_map
                                .entry(p.rule_name.to_string())
                                .or_insert_with(|| p.hints.iter().map(|h| h.to_string()).collect());
                        }
                        // Also collect no_collapses from imported modules.
                        for nc in &module.grammar.no_collapses {
                            no_collapse_set.insert(nc.rule_name.to_string());
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

    // Phase 1.2: Compute FIRST sets for dispatch table generation
    let first_sets = compute_first_sets(&ast, &deps, &scc_result);

    // Phase 1.6: Alias detection
    let aliases = find_aliases(&ast, &scc_result.cyclic_rules);

    // Phase B: Transparent alternation detection
    let transparent_rules = find_transparent_alternations(&ast, &scc_result.cyclic_rules);

    // Phase D: Span-eligible rule detection
    let span_eligible_rules = find_span_eligible_rules(&ast, &scc_result.cyclic_rules);

    let recovers_ref = if recover_map.is_empty() {
        None
    } else {
        Some(&recover_map)
    };
    let pretties_ref = if pretty_map.is_empty() {
        None
    } else {
        Some(&pretty_map)
    };
    let no_collapse_ref = if no_collapse_set.is_empty() {
        None
    } else {
        Some(&no_collapse_set)
    };
    let inline_ref = if inline_set.is_empty() {
        None
    } else {
        Some(&inline_set)
    };

    // ── IR Lowering ──────────────────────────────────────────────────────────
    // Lower the parsed + analysed grammar to the canonical GrammarIR.
    let dispatch_tables_for_ir = std::collections::HashMap::new();
    let mut grammar_ir = lower_to_ir(
        &ast,
        &first_sets,
        &scc_result,
        &aliases,
        &transparent_rules,
        &span_eligible_rules,
        recovers_ref,
        pretties_ref,
        no_collapse_ref,
        &dispatch_tables_for_ir,
        ws_pattern.as_deref(),
        inline_ref,
    );

    // Run all IR optimization passes.
    bbnf_ir::passes::canonicalize_aliases(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::inline_acyclic(&mut grammar_ir);
    bbnf_ir::passes::force_inline(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::fuse_single_use(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::eliminate_epsilon(&mut grammar_ir);
    bbnf_ir::passes::merge_literals(&mut grammar_ir);
    bbnf_ir::passes::merge_regex_alts(&mut grammar_ir);
    bbnf_ir::passes::factor_common_prefixes(&mut grammar_ir);
    bbnf_ir::passes::refine_span_eligibility(&mut grammar_ir);
    grammar_ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&grammar_ir);
    bbnf_ir::passes::generate_dispatch_tables(&mut grammar_ir);
    bbnf_ir::passes::refine_memo_strategies(&mut grammar_ir);
    // NOTE: infer_types is called inside generate_all() AFTER sp_method_rules
    // computation, so that type inference uses the correct has_sp_method flags.

    // ── IR-based codegen (active) ──────────────────────────────────────
    let output = bbnf::generate::generate_all(&mut grammar_ir, &parser_container_attrs, ident);

    output.into()
}
