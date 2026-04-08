//! `identifier_text()` and `identifier_span()`: recursively extract the
//! identifier carried by a variant. Falls back to descending into the first
//! enum child when no `IdentifierCarrier` field is present.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::model::{CstSchema, FieldRole, VariantCategory, VariantDescriptor};

pub(super) fn generate_text(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    // Find all variants with an `IdentifierCarrier` field — those return
    // the carried identifier directly. Other variants recurse via children().
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

        // Identifier-carrier rule (`identifier`) — return the Span text directly.
        if matches!(td, TypeDesc::Span) && variant.name == "identifier" {
            arms.push(quote! {
                #enum_ident::#ident(s) => s.as_str()
            });
            continue;
        }

        // Variants whose first field is an IdentifierCarrier (e.g. `term_1((ident, _))`).
        if let Some(idx) = variant
            .fields
            .iter()
            .position(|f| f.role == FieldRole::IdentifierCarrier)
        {
            let extraction = text_extraction_from_field(td, idx, variant);
            if let Some(extr) = extraction {
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extr }
                });
                continue;
            }
        }

        // Otherwise, fall through to the recursive default (`children().first()`).
    }

    quote! {
        match node {
            #(#arms,)*
            _ => {
                // Fall back to descending into the first enum child.
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_text(first)
                } else {
                    ""
                }
            }
        }
    }
}

pub(super) fn generate_span(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        if matches!(
            variant.category,
            VariantCategory::Phantom | VariantCategory::Recovered
        ) {
            continue;
        }
        let ident = format_ident!("{}", variant.name);
        let Some(td) = &variant.type_desc else { continue };

        // The `identifier` rule itself is a Span carrier — return it directly.
        if matches!(td, TypeDesc::Span) && variant.name == "identifier" {
            arms.push(quote! {
                #enum_ident::#ident(s) => *s
            });
            continue;
        }

        // Variants whose first field is an IdentifierCarrier — recurse into it.
        if let Some(idx) = variant
            .fields
            .iter()
            .position(|f| f.role == FieldRole::IdentifierCarrier)
        {
            if let Some(extr) = span_extraction_from_field(td, idx) {
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extr }
                });
                continue;
            }
        }
    }

    quote! {
        match node {
            #(#arms,)*
            _ => {
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_span(first)
                } else {
                    ::parse_that::Span::default()
                }
            }
        }
    }
}

/// Generate the body of an identifier-text-extraction match arm for a variant.
///
/// Returns `None` if the field's type is not directly addressable (we fall
/// back to `Self::identifier_text(...)` recursion via children).
fn text_extraction_from_field(
    td: &TypeDesc,
    field_idx: usize,
    _variant: &VariantDescriptor,
) -> Option<TokenStream> {
    match td {
        TypeDesc::Tuple(elems) => {
            let elem = elems.get(field_idx)?;
            let idx = syn::Index::from(field_idx);
            match elem {
                TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
                    Self::identifier_text((value).#idx)
                }),
                TypeDesc::Span => Some(quote! {
                    (value).#idx.as_str()
                }),
                _ => None,
            }
        }
        TypeDesc::Span => Some(quote! { value.as_str() }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            Some(quote! { Self::identifier_text(value) })
        }
        _ => None,
    }
}

fn span_extraction_from_field(td: &TypeDesc, field_idx: usize) -> Option<TokenStream> {
    match td {
        TypeDesc::Tuple(elems) => {
            let elem = elems.get(field_idx)?;
            let idx = syn::Index::from(field_idx);
            match elem {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    Some(quote! { Self::identifier_span((value).#idx) })
                }
                // Span is Copy; tuple field access returns it by value.
                TypeDesc::Span => Some(quote! { (value).#idx }),
                _ => None,
            }
        }
        // Top-level Span variant: `value` is `&Span<'a>`, so deref.
        TypeDesc::Span => Some(quote! { *value }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            Some(quote! { Self::identifier_span(value) })
        }
        _ => None,
    }
}
