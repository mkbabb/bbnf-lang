extern crate proc_macro;

use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use bbnf::ParserAttributes;
use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};

use proc_macro::TokenStream;

use syn::{
    Attribute, DeriveInput, Expr, ExprLit, Lit, Meta, parse_macro_input, punctuated::Punctuated,
};

use parse_that::utils::get_cargo_root_path;

// ── Content-based codegen cache ──────────────────────────────────────────────
//
// The full pipeline (parse → lower → 17 IR passes → codegen) is expensive.
// We cache the generated TokenStream on disk, keyed by a hash of:
//   - All grammar file contents (entry + transitive imports)
//   - Parser attributes (slab, span, prettify, skip_recover, etc.)
//   - The struct ident name (determines generated type names)
//   - The bbnf crate version (invalidates on compiler changes)

/// Version tag baked into the cache key to invalidate on compiler changes.
///
/// Combines the crate version with a source fingerprint emitted by `build.rs`
/// over the derive/runtime codegen crates. This invalidates stale proc-macro
/// cache entries when the shared pipeline or codegen implementation changes,
/// even if the on-disk `.bbnf-cache` survives between builds.
const CACHE_VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "-", env!("BBNF_DERIVE_BUILD_ID"),);

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
fn compute_cache_key(paths: &[PathBuf], attrs: &ParserAttributes, ident_name: &str) -> Option<u64> {
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
    attrs.debug.hash(&mut hasher);
    attrs.remove_left_recursion.hash(&mut hasher);
    attrs.prettify.hash(&mut hasher);
    attrs.skip_recover.hash(&mut hasher);
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
                Meta::Path(p) if p.is_ident("debug") => {
                    parser_attr.debug = true;
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

    let request = CompileRequest {
        options: PipelineOptions {
            remove_left_recursion: parser_container_attrs.remove_left_recursion,
            entry_rule: None,
        },
        target: CompileTarget::Aot {
            requested_prettify: parser_container_attrs.prettify,
        },
    };

    let prepared = match compile_paths_request(&parser_container_attrs.paths, &request) {
        Ok(CompileOutput::Aot(prepared)) => prepared,
        Ok(CompileOutput::Vm(_)) => unreachable!("AOT derive received VM pipeline output"),
        Err(err) => panic!("{err}"),
    };

    // ── IR-based codegen (active) ──────────────────────────────────────
    let output = bbnf::generate::generate_all(&prepared, &parser_container_attrs, ident);

    // ── Cache write ──────────────────────────────────────────────────────────
    if let Some(key) = cache_key {
        write_cache(key, &output);
    }

    output.into()
}
