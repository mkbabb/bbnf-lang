//! `span_text()`: recursively unwrap wrapper variants to return the terminal
//! text slice. Priority: PrimaryChild → IdentifierCarrier → first enum child
//! → first Span (for tuple variants); first element (for Vec variants);
//! direct slice (for plain Span variants).

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::model::{CstSchema, FieldRole, VariantCategory, VariantDescriptor};

pub(super) fn generate(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        if matches!(
            variant.category,
            VariantCategory::Phantom | VariantCategory::Recovered
        ) {
            continue;
        }
        let ident = format_ident!("{}", variant.name);
        let Some(td) = &variant.type_desc else {
            continue;
        };

        match td {
            TypeDesc::Span => {
                arms.push(quote! {
                    #enum_ident::#ident(s) => s.as_str()
                });
            }
            TypeDesc::BoxedEnum | TypeDesc::Enum => {
                arms.push(quote! {
                    #enum_ident::#ident(inner) => Self::span_text(inner)
                });
            }
            TypeDesc::Tuple(elems) => {
                if let Some(arm) = generate_tuple_arm(variant, elems, enum_ident, &ident) {
                    arms.push(arm);
                }
            }
            TypeDesc::Vec(inner) => {
                if let Some(arm) = generate_vec_arm(inner, enum_ident, &ident) {
                    arms.push(arm);
                }
            }
            _ => {}
        }
    }

    quote! {
        match node {
            #(#arms,)*
            _ => ""
        }
    }
}

/// For a Vec-payload variant (alternation/concatenation), recurse on the
/// first element. Handles `Vec<BoxedEnum>` and `Vec<Tuple([BoxedEnum, ...])>`.
fn generate_vec_arm(
    inner: &TypeDesc,
    enum_ident: &syn::Ident,
    variant_ident: &syn::Ident,
) -> Option<TokenStream> {
    match inner {
        TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
            #enum_ident::#variant_ident(items) if !items.is_empty() => Self::span_text(&items[0])
        }),
        TypeDesc::Tuple(elems) => {
            // Recurse on the first BoxedEnum field of the first element.
            let first_enum_idx = elems
                .iter()
                .position(|e| matches!(e, TypeDesc::BoxedEnum | TypeDesc::Enum))?;
            let idx = syn::Index::from(first_enum_idx);
            Some(quote! {
                #enum_ident::#variant_ident(items) if !items.is_empty() => Self::span_text(items[0].#idx)
            })
        }
        _ => None,
    }
}

/// For a tuple-payload variant, find the first field that should carry the
/// span and emit a match arm that recurses (or returns directly for Span).
fn generate_tuple_arm(
    variant: &VariantDescriptor,
    elems: &[TypeDesc],
    enum_ident: &syn::Ident,
    variant_ident: &syn::Ident,
) -> Option<TokenStream> {
    let pick = |role_filter: fn(&FieldRole) -> bool| -> Option<usize> {
        variant
            .fields
            .iter()
            .position(|f| role_filter(&f.role))
    };

    let idx = pick(|r| matches!(r, FieldRole::PrimaryChild))
        .or_else(|| pick(|r| matches!(r, FieldRole::IdentifierCarrier)))
        .or_else(|| {
            elems
                .iter()
                .position(|e| matches!(e, TypeDesc::BoxedEnum | TypeDesc::Enum))
        })
        .or_else(|| elems.iter().position(|e| matches!(e, TypeDesc::Span)))?;

    let elem = elems.get(idx)?;
    let tuple_idx = syn::Index::from(idx);

    match elem {
        TypeDesc::Span => Some(quote! {
            #enum_ident::#variant_ident(value) => (value).#tuple_idx.as_str()
        }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
            #enum_ident::#variant_ident(value) => Self::span_text((value).#tuple_idx)
        }),
        _ => None,
    }
}
