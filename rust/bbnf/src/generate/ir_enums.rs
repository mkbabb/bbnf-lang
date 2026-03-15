//! Enum and grammar array generation from IR.
//!
//! Generates the `enum FooEnum<'a> { ... }` and `GRAMMAR_Foo` const array
//! from `GrammarIR` metadata.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::{type_desc_to_syn, IrCodegenCtx};
use super::types::ParserAttributes;

/// Generate the parser enum from IR rule types + sub-variants.
pub fn generate_enum(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;

    // Rule variants: one per non-transparent rule.
    let enum_values = ctx.ir.rules.iter().filter_map(|rule| {
        if rule.meta.is_transparent {
            return None;
        }
        let name = ctx.ir.get_string(rule.name);
        let ident = format_ident!("{}", name);
        let ty = ctx
            .rule_types
            .get(&rule.id)
            .map(|t| t.clone())
            .unwrap_or_else(|| ctx.boxed_enum_type.clone());
        Some(quote! { #ident(#ty) })
    });

    // Sub-variants from heterogeneous alternations.
    let mut seen_sub_variant_names = HashSet::new();
    let mut sub_variant_values: Vec<TokenStream> = Vec::new();
    for rule in &ctx.ir.rules {
        for sv in &rule.meta.sub_variants {
            let name = ctx.ir.get_string(sv.variant_name);
            if !seen_sub_variant_names.insert(name.to_string()) {
                continue;
            }
            let ident = format_ident!("{}", name);
            let ty = type_desc_to_syn(&sv.ty, ctx);
            sub_variant_values.push(quote! { #ident(#ty) });
        }
    }

    // Recovered variant if any @recover directives exist.
    let has_recovers = ctx.ir.rules.iter().any(|r| r.meta.recover.is_some())
        && !ctx.parser_attrs.skip_recover;
    let recovered_variant = if has_recovers {
        quote! { , Recovered }
    } else {
        quote! {}
    };

    let has_sub_variants = !sub_variant_values.is_empty();
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

/// Generate the `GRAMMAR_X` const array with `include_str!()` for each path.
pub fn generate_grammar_arr(
    parser_attrs: &ParserAttributes,
    ident: &syn::Ident,
) -> TokenStream {
    let grammar_arr_name = format_ident!("GRAMMAR_{}", ident);
    let len = parser_attrs.paths.len();
    let include_strs = parser_attrs.paths.iter().map(|path| {
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
