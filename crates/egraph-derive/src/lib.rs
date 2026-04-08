//! `#[derive(Language)]` — generate `Language` impl for an e-node enum.
//!
//! The derive targets a consumer-defined e-node enum whose recursive
//! positions have already been replaced with `Id` (or `Box<[Id]>`). It
//! generates `impl Language` by walking fields tagged with
//! `#[language(child)]` (a single `Id` field) or `#[language(children)]`
//! (a `Box<[Id]>` / `Vec<Id>` slice-like field).
//!
//! ```rust,ignore
//! use egraph::Id;
//! use egraph_derive::Language;
//!
//! #[derive(Clone, Eq, PartialEq, Hash, Language)]
//! pub enum IrNodeENode {
//!     Literal(u32),                               // leaf
//!     Seq(#[language(children)] Box<[Id]>),       // variadic
//!     Repeat {
//!         #[language(child)] inner: Id,           // single child
//!         lo: u32,
//!         hi: u32,
//!     },
//! }
//! ```
//!
//! Each variant contributes one arm to a generated `children(&self) -> &[Id]`
//! and `children_mut(&mut self) -> &mut [Id]` match. Variants with multiple
//! tagged fields require the fields to be adjacent `Id` fields that can be
//! returned as a contiguous slice via unsafe transmute — for portability
//! the derive instead refuses mixed shapes and directs the user to use a
//! single `Box<[Id]>` field.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

/// The `#[derive(Language)]` proc macro.
#[proc_macro_derive(Language, attributes(language))]
pub fn derive_language(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let Data::Enum(data_enum) = &input.data else {
        return syn::Error::new_spanned(&input, "#[derive(Language)] only supports enums")
            .to_compile_error()
            .into();
    };

    let mut children_arms = Vec::new();
    let mut children_mut_arms = Vec::new();

    for variant in &data_enum.variants {
        let variant_ident = &variant.ident;
        let arms_result: syn::Result<VariantArms> = match &variant.fields {
            Fields::Unit => Ok(VariantArms::unit(name, variant_ident)),
            Fields::Unnamed(fields_unnamed) => {
                variant_arms(name, variant_ident, &fields_unnamed.unnamed, false)
            }
            Fields::Named(fields_named) => {
                variant_arms(name, variant_ident, &fields_named.named, true)
            }
        };
        match arms_result {
            Ok(arms) => {
                children_arms.push(arms.children);
                children_mut_arms.push(arms.children_mut);
            }
            Err(err) => return err.to_compile_error().into(),
        }
    }

    let expanded = quote! {
        impl #impl_generics ::egraph::Language for #name #ty_generics #where_clause {
            fn children(&self) -> &[::egraph::Id] {
                match self {
                    #(#children_arms,)*
                }
            }

            fn children_mut(&mut self) -> &mut [::egraph::Id] {
                match self {
                    #(#children_mut_arms,)*
                }
            }
        }
    };

    expanded.into()
}

struct VariantArms {
    children: TokenStream2,
    children_mut: TokenStream2,
}

impl VariantArms {
    fn unit(name: &Ident, variant: &Ident) -> Self {
        Self {
            children: quote! { #name::#variant => &[] },
            children_mut: quote! { #name::#variant => &mut [] },
        }
    }
}

/// Build the match arms for a single struct-like or tuple-like variant.
fn variant_arms(
    name: &Ident,
    variant: &Ident,
    fields: &syn::punctuated::Punctuated<syn::Field, syn::Token![,]>,
    named: bool,
) -> syn::Result<VariantArms> {
    // Collect one (binding, kind) per field.
    let mut pairs: Vec<(Ident, Option<syn::Ident>, FieldKind)> = Vec::new();
    for (i, field) in fields.iter().enumerate() {
        let binding = format_ident!("__f{}", i);
        let field_name = field.ident.clone();
        let kind = classify_field(field)?;
        pairs.push((binding, field_name, kind));
    }

    let tagged: Vec<&(Ident, Option<syn::Ident>, FieldKind)> = pairs
        .iter()
        .filter(|(_, _, k)| !matches!(k, FieldKind::Leaf))
        .collect();

    // Destructuring pattern (named vs tuple).
    let pattern = if named {
        let binds: Vec<TokenStream2> = pairs
            .iter()
            .map(|(binding, field_name, _)| {
                let n = field_name.as_ref().expect("named field");
                quote! { #n: #binding }
            })
            .collect();
        quote! { #name::#variant { #(#binds),* } }
    } else {
        let binds: Vec<&Ident> = pairs.iter().map(|(b, _, _)| b).collect();
        quote! { #name::#variant(#(#binds),*) }
    };

    // No tagged fields: empty slice.
    if tagged.is_empty() {
        let ignore = pairs.iter().map(|(_, _, _)| quote! { _ });
        let ignore2 = ignore.clone();
        let pattern_ignore = if named {
            quote! { #name::#variant { .. } }
        } else {
            quote! { #name::#variant(#(#ignore),*) }
        };
        let pattern_ignore2 = if named {
            quote! { #name::#variant { .. } }
        } else {
            quote! { #name::#variant(#(#ignore2),*) }
        };
        return Ok(VariantArms {
            children: quote! { #pattern_ignore => &[] },
            children_mut: quote! { #pattern_ignore2 => &mut [] },
        });
    }

    // Single tagged field: direct slice projection.
    if tagged.len() == 1 {
        let (binding, _, kind) = tagged[0];
        let (child_expr, child_mut_expr) = match kind {
            FieldKind::Leaf => unreachable!(),
            FieldKind::SingleId => (
                quote! { ::std::slice::from_ref(#binding) },
                quote! { ::std::slice::from_mut(#binding) },
            ),
            FieldKind::SliceId => (
                quote! { &#binding[..] },
                quote! { &mut #binding[..] },
            ),
        };
        return Ok(VariantArms {
            children: quote! { #pattern => #child_expr },
            children_mut: quote! { #pattern => #child_mut_expr },
        });
    }

    // Multiple tagged fields. We require ALL tagged fields to be
    // `#[language(child)] Id` AND to occupy adjacent positions so we
    // can return them as a contiguous `&[Id]`. This holds for variants
    // like `Skip(#[language(child)] Id, #[language(child)] Id)` where
    // Rust guarantees tuple field layout.
    //
    // NOTE: relying on tuple layout for `&[Id]` projection would require
    // `#[repr(C)]`. Rather than impose that, we reject multi-child variants
    // and direct the user to use a `Box<[Id]>` field instead. This is the
    // same limitation as `egg` imposes via its Symbol/Node separation.
    let span = variant.span();
    Err(syn::Error::new(
        span,
        "Language derive: variants with multiple tagged fields must use a single \
         `Box<[Id]>` or `Vec<Id>` field annotated `#[language(children)]`. Split \
         the variant or rewrap children in a slice.",
    ))
}

enum FieldKind {
    Leaf,
    /// `#[language(child)]` on an `Id` field.
    SingleId,
    /// `#[language(children)]` on a `Box<[Id]>` / `Vec<Id>` field.
    SliceId,
}

fn classify_field(field: &syn::Field) -> syn::Result<FieldKind> {
    for attr in &field.attrs {
        if !attr.path().is_ident("language") {
            continue;
        }
        let mut kind: Option<FieldKind> = None;
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("child") {
                kind = Some(FieldKind::SingleId);
                Ok(())
            } else if meta.path.is_ident("children") {
                kind = Some(FieldKind::SliceId);
                Ok(())
            } else {
                Err(meta.error("unknown #[language(...)] attribute — expected `child` or `children`"))
            }
        })?;
        return Ok(kind.unwrap_or(FieldKind::Leaf));
    }
    Ok(FieldKind::Leaf)
}

