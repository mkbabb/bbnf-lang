//! Source range generation for prettify code generation.
//!
//! Generates `source_range()` match arms for compound types (tuples, Box<Enum>,
//! Option). Span and Vec source ranges are generated inline in the main
//! `generate_prettify` orchestrator.

use super::prettify_utils::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Generate a `source_range()` match arm for a compound-typed variant.
///
/// Handles Box<Enum> (delegate), Option (delegate via and_then), and tuples
/// (single-pass min/max fold over element ranges).
pub(crate) fn generate_compound_range(variant: &syn::Ident, ty: &syn::Type) -> TokenStream {
    if is_box_enum_type(ty) {
        return quote! {
            Self::#variant(val) => val.source_range(),
        };
    }

    if is_option_type(ty) {
        return quote! {
            Self::#variant(val) => val.as_ref().and_then(|v| v.source_range()),
        };
    }

    if let syn::Type::Tuple(tuple) = ty {
        let n = tuple.elems.len();
        let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
        let pattern = quote! { (#(#bindings),*) };

        let range_exprs: Vec<TokenStream> = bindings
            .iter()
            .enumerate()
            .map(|(i, binding)| {
                let elem_ty = &tuple.elems[i];
                range_for_binding(binding, elem_ty)
            })
            .collect();

        // Single-pass min/max without Vec allocation.
        let fold_stmts: Vec<TokenStream> = range_exprs.iter().map(|rp| {
            quote! {
                if let Some((_s, _e)) = #rp {
                    if _s < _min_s { _min_s = _s; }
                    if _e > _max_e { _max_e = _e; }
                    _found = true;
                }
            }
        }).collect();

        return quote! {
            Self::#variant(#pattern) => {
                let mut _min_s = usize::MAX;
                let mut _max_e = 0usize;
                let mut _found = false;
                #(#fold_stmts)*
                if _found { Some((_min_s, _max_e)) } else { None }
            }
        };
    }

    quote! {
        Self::#variant(_) => {
            panic!(
                "No @pretty source-range strategy registered for enum variant `{}`",
                stringify!(#variant)
            )
        },
    }
}
