extern crate proc_macro;

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use bbnf::calculate_ast_deps;

use bbnf::analysis::{
    compute_first_sets, tarjan_scc, topological_sort_scc,
};
use bbnf::imports::load_module_graph;
use bbnf::lower::{DirectiveSet, lower_to_ir};
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

// ── Content-based codegen cache ──────────────────────────────────────────────
//
// The full pipeline (parse → lower → 17 IR passes → codegen) is expensive.
// We cache the generated TokenStream on disk, keyed by a hash of:
//   - All grammar file contents (entry + transitive imports)
//   - Parser attributes (arena, span, prettify, skip_recover, etc.)
//   - The struct ident name (determines generated type names)
//   - The bbnf crate version (invalidates on compiler changes)

/// Version tag baked into the cache key to invalidate on compiler changes.
///
/// Combines the crate version with a build-time ID emitted by `build.rs`.
/// Since Cargo recompiles bbnf-derive whenever any transitive dependency
/// (bbnf, bbnf-ir, parse_that) changes, the build ID changes on every
/// recompilation — invalidating stale caches even without a version bump.
const CACHE_VERSION: &str = concat!(
    env!("CARGO_PKG_VERSION"),
    "-",
    env!("BBNF_DERIVE_BUILD_ID"),
);

/// Recursively collect all grammar file contents for hashing.
///
/// Scans for `@import "path" ;` directives via simple string matching to avoid
/// running the full parser. This is conservative: any line containing `@import`
/// followed by a quoted path will be followed. False positives (e.g., inside
/// comments) are harmless — they just add extra files to the hash, which is
/// correct (more inputs → more cache invalidation, never less).
fn collect_grammar_contents(
    path: &Path,
    visited: &mut HashSet<PathBuf>,
    contents: &mut Vec<(PathBuf, String)>,
) {
    let canonical = match path.canonicalize() {
        Ok(p) => p,
        Err(_) => return,
    };
    if !visited.insert(canonical.clone()) {
        return;
    }
    let source = match std::fs::read_to_string(&canonical) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Scan for @import directives to find transitive dependencies.
    let parent = canonical.parent().unwrap_or(Path::new("."));
    for line in source.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("@import") {
            // Extract the quoted path: @import "path" ; or @import "path" { ... } ;
            let rest = rest.trim();
            if let Some(start) = rest.find('"') {
                if let Some(end) = rest[start + 1..].find('"') {
                    let import_path_str = &rest[start + 1..start + 1 + end];
                    let import_path = parent.join(import_path_str);
                    collect_grammar_contents(&import_path, visited, contents);
                }
            }
        }
    }

    contents.push((canonical, source));
}

/// Compute a deterministic hash of grammar contents + attributes + ident.
fn compute_cache_key(
    paths: &[PathBuf],
    attrs: &ParserAttributes,
    ident_name: &str,
) -> Option<u64> {
    let mut all_contents = Vec::new();
    let mut visited = HashSet::new();

    for path in paths {
        collect_grammar_contents(path, &mut visited, &mut all_contents);
    }

    if all_contents.is_empty() {
        return None;
    }

    // Sort by path for deterministic ordering.
    all_contents.sort_by(|a, b| a.0.cmp(&b.0));

    let mut hasher = std::hash::DefaultHasher::new();

    // Hash version tag.
    CACHE_VERSION.hash(&mut hasher);

    // Hash all grammar file contents.
    for (path, content) in &all_contents {
        path.hash(&mut hasher);
        content.hash(&mut hasher);
    }

    // Hash all parser attributes that affect codegen output.
    ident_name.hash(&mut hasher);
    attrs.ignore_whitespace.hash(&mut hasher);
    attrs.debug.hash(&mut hasher);
    attrs.use_string.hash(&mut hasher);
    attrs.remove_left_recursion.hash(&mut hasher);
    attrs.prettify.hash(&mut hasher);
    attrs.skip_recover.hash(&mut hasher);
    attrs.arena.hash(&mut hasher);
    attrs.span.hash(&mut hasher);
    for p in &attrs.paths {
        // Hash canonical paths so that the same file via different relative
        // paths produces the same key.
        if let Ok(c) = p.canonicalize() {
            c.hash(&mut hasher);
        } else {
            p.hash(&mut hasher);
        }
    }

    Some(hasher.finish())
}

/// Resolve the cache directory: `<target_dir>/.bbnf-cache/`.
fn cache_dir() -> Option<PathBuf> {
    // In proc-macro context, OUT_DIR is not set. CARGO_TARGET_DIR may be set
    // explicitly. Fall back to walking up from CARGO_MANIFEST_DIR to find
    // `target/`.
    if let Ok(target) = std::env::var("CARGO_TARGET_DIR") {
        let dir = PathBuf::from(target).join(".bbnf-cache");
        return Some(dir);
    }

    // Walk up from CARGO_MANIFEST_DIR looking for a `target/` directory.
    if let Ok(manifest) = std::env::var("CARGO_MANIFEST_DIR") {
        let mut dir = PathBuf::from(manifest);
        loop {
            let candidate = dir.join("target");
            if candidate.is_dir() {
                return Some(candidate.join(".bbnf-cache"));
            }
            if !dir.pop() {
                break;
            }
        }
    }

    None
}

/// Try to read a cached TokenStream for the given cache key.
fn read_cache(key: u64) -> Option<proc_macro2::TokenStream> {
    let dir = cache_dir()?;
    let path = dir.join(format!("{:016x}.rs", key));
    let cached = std::fs::read_to_string(&path).ok()?;
    cached.parse::<proc_macro2::TokenStream>().ok()
}

/// Write a generated TokenStream to the cache.
fn write_cache(key: u64, tokens: &proc_macro2::TokenStream) {
    let Some(dir) = cache_dir() else { return };
    if std::fs::create_dir_all(&dir).is_err() {
        return;
    }
    let path = dir.join(format!("{:016x}.rs", key));
    // Write to a temp file then rename for atomicity.
    let tmp = dir.join(format!("{:016x}.rs.tmp", key));
    if std::fs::write(&tmp, tokens.to_string()).is_ok() {
        let _ = std::fs::rename(&tmp, &path);
    }
}

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
                Meta::Path(p) if p.is_ident("span") => {
                    parser_attr.span = true;
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

    // ── Cache check ──────────────────────────────────────────────────────────
    // Compute a content-based hash of all grammar files + attributes. If the
    // cache has a valid entry, skip the entire pipeline.
    let cache_key = compute_cache_key(
        &parser_container_attrs.paths,
        &parser_container_attrs,
        &ident.to_string(),
    );

    if let Some(key) = cache_key {
        if let Some(cached) = read_cache(key) {
            return cached.into();
        }
    }

    // Try import-aware loading first: if the first file contains @import directives,
    // use load_module_graph() which handles DFS traversal, cycle detection, and
    // selective import resolution. Otherwise fall back to simple fold.
    let mut recover_map: HashMap<String, Expression<'static>> = HashMap::new();
    let mut pretty_map: HashMap<String, Vec<String>> = HashMap::new();
    let mut token_set: HashSet<String> = HashSet::new();
    let mut debug_set: HashSet<String> = HashSet::new();
    let mut debug_all = parser_container_attrs.debug;
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
            // Extract @ws directive.
            if let Some(ref pat) = pg.ws_pattern {
                ws_pattern = Some(pat.to_string());
            }
            // Extract @token directives.
            for name in &pg.token_rules {
                token_set.insert(name.to_string());
            }
            // Extract @debug directives.
            for name in &pg.debug_rules {
                if name.as_ref() == "*" {
                    debug_all = true;
                } else {
                    debug_set.insert(name.to_string());
                }
            }

            if !pg.imports.is_empty() {
                // File has imports — use module graph loader.
                let registry = load_module_graph(entry)
                    .unwrap_or_else(|e| panic!("Import resolution failed: {}", e));
                if !registry.errors.is_empty() {
                    let msgs: Vec<String> = registry.errors.iter().map(|e| e.to_string()).collect();
                    panic!("Import errors:\n{}", msgs.join("\n"));
                }
                // Merge all modules: imported modules first, entry module last.
                // The entry module's local rules override imported ones (e.g.,
                // css-stylesheet.bbnf can override calcFunction from css-func-body.bbnf).
                let entry_canonical = entry.canonicalize()
                    .unwrap_or_else(|_| entry.to_path_buf());
                let mut merged = IndexMap::new();
                // First pass: all non-entry modules.
                for path in registry.paths() {
                    if *path == entry_canonical {
                        continue; // Process entry last.
                    }
                    if let Some(module) = registry.get_module(path) {
                        for rec in &module.grammar.recovers {
                            recover_map
                                .entry(rec.rule_name.to_string())
                                .or_insert_with(|| rec.sync_expr.clone());
                        }
                        for p in &module.grammar.pretties {
                            pretty_map
                                .entry(p.rule_name.to_string())
                                .or_insert_with(|| p.hints.iter().map(|h| h.to_string()).collect());
                        }
                        for (name, expr) in &module.grammar.rules {
                            merged.insert(name.clone(), expr.clone());
                        }
                    }
                }
                // Second pass: entry module (its rules override imports).
                if let Some(module) = registry.get_module(&entry_canonical) {
                    for rec in &module.grammar.recovers {
                        recover_map
                            .entry(rec.rule_name.to_string())
                            .or_insert_with(|| rec.sync_expr.clone());
                    }
                    for p in &module.grammar.pretties {
                        pretty_map
                            .entry(p.rule_name.to_string())
                            .or_insert_with(|| p.hints.iter().map(|h| h.to_string()).collect());
                    }
                    for (name, expr) in &module.grammar.rules {
                        merged.insert(name.clone(), expr.clone());
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

    let recovers_ref = if recover_map.is_empty() { None } else { Some(&recover_map) };
    let pretties_ref = if pretty_map.is_empty() { None } else { Some(&pretty_map) };
    let token_ref = if token_set.is_empty() { None } else { Some(&token_set) };
    let debug_ref = if debug_set.is_empty() { None } else { Some(&debug_set) };

    let directives = DirectiveSet {
        recovers: recovers_ref,
        pretties: pretties_ref,
        ws_pattern: ws_pattern.as_deref(),
        token_rules: token_ref,
        debug_rules: debug_ref,
        debug_all,
    };

    // ── IR Lowering ──────────────────────────────────────────────────────────
    // Lower the parsed + analysed grammar to the canonical GrammarIR.
    let mut grammar_ir = lower_to_ir(
        &ast,
        &first_sets,
        &scc_result,
        &directives,
    );

    // Run IR metadata passes (alias + transparent detection from IR structure).
    bbnf_ir::passes::compute_aliases(&mut grammar_ir);
    bbnf_ir::passes::compute_transparent(&mut grammar_ir);

    // Run all IR optimization passes.
    bbnf_ir::passes::canonicalize_aliases(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::inline_acyclic(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::fuse_single_use(&mut grammar_ir);
    bbnf_ir::passes::prune_unreachable(&mut grammar_ir);
    bbnf_ir::passes::eliminate_epsilon(&mut grammar_ir);
    bbnf_ir::passes::merge_literals(&mut grammar_ir);
    bbnf_ir::passes::merge_regex_alts(&mut grammar_ir);
    bbnf_ir::passes::factor_common_prefixes(&mut grammar_ir);
    bbnf_ir::passes::sort_alt_branches(&mut grammar_ir);
    bbnf_ir::passes::refine_span_eligibility(&mut grammar_ir);
    grammar_ir.follow_sets = bbnf_ir::passes::compute_follow_sets(&grammar_ir);
    bbnf_ir::passes::factor_regex_with_lookahead(&mut grammar_ir);
    bbnf_ir::passes::fuse_token_dispatch(&mut grammar_ir);
    bbnf_ir::passes::generate_dispatch_tables(&mut grammar_ir);
    // NOTE: infer_types is called inside generate_all() AFTER sp_method_rules
    // computation, so that type inference uses the correct has_sp_method flags.

    // ── IR-based codegen (active) ──────────────────────────────────────
    let output = bbnf::generate::generate_all(&mut grammar_ir, &parser_container_attrs, ident);

    // ── Cache write ──────────────────────────────────────────────────────────
    if let Some(key) = cache_key {
        write_cache(key, &output);
    }

    output.into()
}
