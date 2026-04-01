//! Enum and grammar array generation from IR.
//!
//! Generates the `enum FooEnum<'a> { ... }` and `GRAMMAR_Foo` const array
//! from `GrammarIR` metadata.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::{IrCodegenCtx, ParserAttributes, type_desc_to_syn};

/// Generate the parser enum from IR rule types + sub-variants.
pub fn generate_enum(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;

    // Rule variants: one per non-transparent rule.
    let debug_info: Vec<String> = ctx
        .ir
        .rules
        .iter()
        .map(|r| {
            format!(
                "{}: trans={}",
                ctx.ir.get_string(r.name),
                r.meta.is_transparent
            )
        })
        .collect();
    if ctx.ir.rules.iter().all(|r| r.meta.is_transparent) && !ctx.ir.rules.is_empty() {
        panic!(
            "All {} rules are transparent: {}",
            ctx.ir.rules.len(),
            debug_info.join(", ")
        );
    }
    let enum_values: Vec<_> = ctx
        .ir
        .rules
        .iter()
        .filter_map(|rule| {
            if rule.meta.is_transparent {
                return None;
            }
            let name = ctx.ir.get_string(rule.name);
            let ident = format_ident!("{}", name);
            let ty = if ctx.fused_number_rules.contains(&rule.id) {
                // Fused number: variant stores (Span<'a>, f64) instead of Span.
                syn::parse_quote!((::parse_that::Span<'a>, f64))
            } else {
                ctx.rule_types
                    .get(&rule.id)
                    .cloned()
                    .unwrap_or_else(|| ctx.boxed_enum_type.clone())
            };
            Some(quote! { #ident(#ty) })
        })
        .collect();

    let enum_values = enum_values.into_iter();

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
    let has_recovers =
        ctx.ir.rules.iter().any(|r| r.meta.directives.recover.is_some()) && !ctx.parser_attrs.skip_recover;
    let has_sub_variants = !sub_variant_values.is_empty();
    // Collect all variants into a single Vec for clean comma handling.
    let mut all_variants: Vec<TokenStream> = enum_values.collect();
    all_variants.extend(sub_variant_values.iter().cloned());
    if has_recovers {
        all_variants.push(quote! { Recovered });
    }
    // PhantomData ensures 'a is used even when all variants map to non-lifetime types.
    all_variants.push(quote! { #[doc(hidden)] __Phantom(::core::marker::PhantomData<&'a ()>) });

    // Skip Pretty derive when PhantomData is present (Pretty can't handle it).
    // The Pretty impl is generated manually for sub-variant enums anyway.
    if has_sub_variants {
        quote! {
            #[derive(Debug, Clone)]
            pub enum #enum_ident<'a> {
                #(#all_variants),*
            }
        }
    } else {
        // Generate manual From<Enum> for Doc instead of deriving Pretty,
        // to handle the __Phantom variant.
        quote! {
            #[derive(Debug, Clone)]
            pub enum #enum_ident<'a> {
                #(#all_variants),*
            }
        }
    }
}

/// Generate the `GRAMMAR_X` const array with `include_str!()` for each path.
pub fn generate_grammar_arr(parser_attrs: &ParserAttributes, ident: &syn::Ident) -> TokenStream {
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
