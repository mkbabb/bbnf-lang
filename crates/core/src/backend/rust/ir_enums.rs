//! Grammar-string array generation from IR.
//!
//! Tranche AC.2 — `generate_enum` has been deleted. Under tape-
//! first the generated code never materializes a parse-tree enum;
//! type information lives in the per-rule view structs emitted by
//! [`super::view::generate_views`] and in the `variant_idx` byte
//! carried by every tape record.
//!
//! The `GRAMMAR_X` const array is still needed so consumers can
//! read the original grammar source (e.g. LSP, formatter, tests).

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::ParserAttributes;

/// Generate the `GRAMMAR_X` const array with `include_str!()` for each
/// grammar source path.
///
/// The emitter consumes `parser_attrs.grammar_rel_paths` — paths
/// relative to the manifest directory of the crate that compiles the
/// generated file (POSIX) — and wraps each one in a
/// `concat!(env!("CARGO_MANIFEST_DIR"), "/", <rel>)` token, so the
/// `include_str!` resolves inside that crate. For `bbnf` the path is
/// its in-crate grammar copy (`crates/core/grammar/…`), which
/// `cargo xtask regen` writes from the workspace `grammar/` tree and
/// `cargo xtask regen --check` holds byte-identical to it (sk-v27
/// LW2.TOPO-4b). A path that leaves the crate would not be in the
/// published package, so the crate would not build from crates.io.
///
/// Embedding the relative path keeps the generated file portable
/// across worktrees and developer checkouts; embedding an absolute
/// path (the pre-fix shape) silently bound the file to whichever
/// worktree last ran regen and surfaced as `os error 2` on every
/// other consumer.
pub fn generate_grammar_arr(parser_attrs: &ParserAttributes, ident: &syn::Ident) -> TokenStream {
    let grammar_arr_name = format_ident!("GRAMMAR_{}", ident);
    let len = parser_attrs.grammar_rel_paths.len();
    // B5.W0 — the lock-step `paths` / `grammar_rel_paths` invariant
    // is upheld at the type surface via [`ParserAttributes::with_paths`];
    // the prior runtime assert here papered over construction-site
    // gaps and has been retired.
    let include_strs = parser_attrs.grammar_rel_paths.iter().map(|rel| {
        // `concat!(env!("CARGO_MANIFEST_DIR"), "/", <rel>)` — `<rel>`
        // (e.g. `grammar/bbnf/bbnf.bbnf`) is inside the crate.
        let suffix = format!("/{rel}");
        quote! {
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), #suffix))
        }
    });

    quote! {
        pub const #grammar_arr_name: [&'static str; #len] = [
            #(#include_strs),*
        ];
    }
}
