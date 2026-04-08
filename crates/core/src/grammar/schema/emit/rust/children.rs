//! `children()` debug helper: collects enum-typed children into a `Vec<&Enum>`.
//!
//! Allocates per call — callers in hot paths should prefer `walk_children`
//! which dispatches directly without the intermediate Vec.

use std::collections::HashSet;

use bbnf_ir::{RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::model::{CstSchema, VariantCategory};
use super::shared::type_has_enum_children;

pub(super) fn generate(
    schema: &CstSchema,
    enum_ident: &syn::Ident,
    fused_number_rules: &HashSet<RuleId>,
) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        let ident = format_ident!("{}", variant.name);
        match variant.category {
            VariantCategory::Phantom => {
                arms.push(quote! { #enum_ident::__Phantom(_) => ::std::vec::Vec::new() });
                continue;
            }
            VariantCategory::Recovered => {
                arms.push(quote! { #enum_ident::Recovered => ::std::vec::Vec::new() });
                continue;
            }
            _ => {}
        }

        // Rust override: fused number rules have `(Span, f64)` payload, no children.
        if let Some(rid) = variant.rule_id {
            if fused_number_rules.contains(&rid) {
                arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
                continue;
            }
        }

        let Some(td) = &variant.type_desc else {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        };

        if !type_has_enum_children(td) {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        }

        let extraction = generate_extraction(td);
        arms.push(quote! {
            #enum_ident::#ident(value) => { #extraction }
        });
    }

    quote! {
        match node {
            #(#arms),*
        }
    }
}

/// Build the body of a `children()` match arm: collect references into
/// a local `__children: Vec<&Enum>`.
fn generate_extraction(td: &TypeDesc) -> TokenStream {
    let mut collectors = Vec::new();
    extract_from_type(td, &quote! { value }, &mut collectors, 0);
    if collectors.is_empty() {
        quote! { ::std::vec::Vec::new() }
    } else {
        quote! {
            let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
            #(#collectors)*
            __children
        }
    }
}

/// Recursively walk a `TypeDesc` and emit code that pushes enum references
/// to a local `__children` accumulator.
fn extract_from_type(
    td: &TypeDesc,
    accessor: &TokenStream,
    collectors: &mut Vec<TokenStream>,
    depth: usize,
) {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            collectors.push(quote! { __children.push(#accessor); });
        }
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 | TypeDesc::Named(_) => {
            // Leaf — no enum content.
        }
        TypeDesc::Option(inner) if type_has_enum_children(inner) => {
            let inner_var = format_ident!("__opt_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        if let Some(#inner_var) = #accessor {
                            __children.push(#inner_var);
                        }
                    });
                }
                _ => {
                    let mut inner_collectors = Vec::new();
                    extract_from_type(
                        inner,
                        &quote! { #inner_var },
                        &mut inner_collectors,
                        depth + 1,
                    );
                    if !inner_collectors.is_empty() {
                        collectors.push(quote! {
                            if let Some(#inner_var) = #accessor {
                                #(#inner_collectors)*
                            }
                        });
                    }
                }
            }
        }
        TypeDesc::Option(_) => {}
        TypeDesc::Vec(inner) if type_has_enum_children(inner) => {
            let iter_var = format_ident!("__item_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __children.push(#iter_var);
                        }
                    });
                }
                TypeDesc::Tuple(inner_elems) => {
                    let mut inner_collectors = Vec::new();
                    for (i, elem) in inner_elems.iter().enumerate() {
                        if type_has_enum_children(elem) {
                            let idx = syn::Index::from(i);
                            let field_acc = quote! { #iter_var.#idx };
                            extract_from_type(
                                elem,
                                &field_acc,
                                &mut inner_collectors,
                                depth + 1,
                            );
                        }
                    }
                    if !inner_collectors.is_empty() {
                        collectors.push(quote! {
                            for #iter_var in (#accessor).iter() {
                                #(#inner_collectors)*
                            }
                        });
                    }
                }
                _ => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __children.push(#iter_var);
                        }
                    });
                }
            }
        }
        TypeDesc::Vec(_) => {}
        TypeDesc::Tuple(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                if type_has_enum_children(elem) {
                    let idx = syn::Index::from(i);
                    let child_accessor = quote! { (#accessor).#idx };
                    extract_from_type(elem, &child_accessor, collectors, depth + 1);
                }
            }
        }
    }
}
