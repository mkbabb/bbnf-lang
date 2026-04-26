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
/// The emitter consumes `parser_attrs.grammar_rel_paths` —
/// workspace-root-relative POSIX paths — and wraps each one in a
/// `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` token so
/// the resulting `include_str!` resolves at the consumer's compile
/// time relative to the `bbnf` crate's manifest directory
/// (`<workspace>/crates/core`). Two `..` levels lift to the workspace
/// root, then the relative path joins the actual grammar file.
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
        // `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` —
        // CARGO_MANIFEST_DIR for `bbnf` resolves to
        // `<workspace>/crates/core`; `../../` lifts to the workspace
        // root; `<rel>` (e.g. `grammar/bbnf/bbnf.bbnf`) joins the
        // grammar source file.
        let suffix = format!("/../../{rel}");
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
