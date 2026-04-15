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

/// Generate the `GRAMMAR_X` const array with `include_str!()` for each path.
pub fn generate_grammar_arr(parser_attrs: &ParserAttributes, ident: &syn::Ident) -> TokenStream {
    let grammar_arr_name = format_ident!("GRAMMAR_{}", ident);
    let len = parser_attrs.paths.len();
    let include_strs = parser_attrs.paths.iter().map(|path| {
        let path = path
            .to_str()
            .unwrap_or_else(|| panic!("non-UTF8 grammar path: {:?}", path));
        quote! { include_str!(#path) }
    });

    quote! {
        pub const #grammar_arr_name: [&'static str; #len] = [
            #(#include_strs),*
        ];
    }
}
