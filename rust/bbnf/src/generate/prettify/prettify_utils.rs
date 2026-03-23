//! Utility functions for prettify code generation.
//!
//! Contains type predicates, binding doc/range helpers,
//! and item-level doc/range generation used by both
//! the IR-based prettify generator and the prettify submodules.

use super::super::ir_types::type_is_span;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

// ---------------------------------------------------------------------------
// Type inspection helpers
// ---------------------------------------------------------------------------

pub fn is_vec_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(path) = ty {
        path.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Vec")
    } else {
        false
    }
}

pub fn is_option_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(path) = ty {
        path.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Option")
    } else {
        false
    }
}

/// Extract the inner type `T` from `Option<T>`.
pub fn extract_option_inner(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(path) = ty {
        let seg = path.path.segments.last()?;
        if seg.ident == "Option" {
            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner);
                }
            }
        }
    }
    None
}

pub fn is_box_enum_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(path) = ty {
        path.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Box")
    } else {
        false
    }
}

pub fn is_ref_enum_type(ty: &syn::Type) -> bool {
    if let syn::Type::Reference(reference) = ty {
        if let syn::Type::Path(path) = reference.elem.as_ref() {
            return path
                .path
                .segments
                .last()
                .is_some_and(|seg| seg.ident.to_string().ends_with("Enum"));
        }
    }
    false
}

pub fn is_recursive_enum_type(ty: &syn::Type) -> bool {
    is_box_enum_type(ty) || is_ref_enum_type(ty)
}

// ---------------------------------------------------------------------------
// Binding doc/range helpers for tuple elements
// ---------------------------------------------------------------------------

pub fn doc_for_binding(binding: &syn::Ident, ty: &syn::Type) -> TokenStream {
    if type_is_span(ty) {
        quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#binding.as_str())) }
    } else if is_vec_type(ty) {
        let item_doc = generate_item_to_doc(ty);
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = #binding.iter().map(|item| #item_doc).collect();
                ::pprint::Doc::Join(Box::new((::pprint::Doc::Softline, docs)))
            }
        }
    } else if is_option_type(ty) {
        // Extract Option<T> inner type and recursively generate doc for it.
        let inner_ty = extract_option_inner(ty);
        let inner_ident = format_ident!("inner");
        let inner_doc = if let Some(inner_t) = inner_ty {
            doc_for_binding(&inner_ident, inner_t)
        } else {
            quote! { inner.to_doc() }
        };
        quote! {
            match #binding {
                Some(#inner_ident) => { #inner_doc },
                None => ::pprint::Doc::Null,
            }
        }
    } else if is_recursive_enum_type(ty) {
        quote! { #binding.to_doc() }
    } else if let syn::Type::Tuple(tuple_ty) = ty {
        // Nested tuple — destructure and chain with `+` (no Null interleaving).
        // Chains into a single Concat(Vec) with one heap allocation.
        let n = tuple_ty.elems.len();
        let inner_bindings: Vec<_> = (0..n).map(|i| format_ident!("t{}", i)).collect();
        let pat = quote! { (#(#inner_bindings),*) };
        let doc_parts: Vec<TokenStream> = tuple_ty
            .elems
            .iter()
            .zip(inner_bindings.iter())
            .map(|(elem_ty, b)| doc_for_binding(b, elem_ty))
            .collect();
        let combined = if doc_parts.len() == 1 {
            doc_parts[0].clone()
        } else {
            let mut acc = doc_parts[0].clone();
            for part in &doc_parts[1..] {
                acc = quote! { (#acc) + (#part) };
            }
            acc
        };
        quote! {
            { let #pat = #binding; #combined }
        }
    } else {
        // Unknown type — emit Null.
        quote! { ::pprint::Doc::Null }
    }
}

pub fn range_for_binding(binding: &syn::Ident, ty: &syn::Type) -> TokenStream {
    if type_is_span(ty) {
        quote! { Some((#binding.start, #binding.end)) }
    } else if is_vec_type(ty) {
        let item_range = generate_item_source_range(ty);
        // Single-pass fold avoids collecting to a Vec.
        quote! {
            {
                let mut _min_s = usize::MAX;
                let mut _max_e = 0usize;
                let mut _found = false;
                for i in #binding.iter() {
                    if let Some((s, e)) = #item_range {
                        if s < _min_s { _min_s = s; }
                        if e > _max_e { _max_e = e; }
                        _found = true;
                    }
                }
                if _found { Some((_min_s, _max_e)) } else { None }
            }
        }
    } else if is_option_type(ty) {
        let inner_ty = extract_option_inner(ty);
        let inner_ident = format_ident!("v");
        let inner_range = if let Some(inner_t) = inner_ty {
            range_for_binding(&inner_ident, inner_t)
        } else {
            quote! { v.source_range() }
        };
        quote! { #binding.as_ref().and_then(|#inner_ident| #inner_range) }
    } else if is_recursive_enum_type(ty) {
        quote! { #binding.source_range() }
    } else if let syn::Type::Tuple(tuple_ty) = ty {
        // Nested tuple — single-pass min/max without Vec allocation.
        let n = tuple_ty.elems.len();
        let inner_bindings: Vec<_> = (0..n).map(|i| format_ident!("t{}", i)).collect();
        let pat = quote! { (#(#inner_bindings),*) };
        let range_parts: Vec<TokenStream> = tuple_ty
            .elems
            .iter()
            .zip(inner_bindings.iter())
            .map(|(elem_ty, b)| range_for_binding(b, elem_ty))
            .collect();
        // Generate a sequence of if-let checks that accumulate min/max.
        let fold_stmts: Vec<TokenStream> = range_parts
            .iter()
            .map(|rp| {
                quote! {
                    if let Some((_s, _e)) = #rp {
                        if _s < _min_s { _min_s = _s; }
                        if _e > _max_e { _max_e = _e; }
                        _found = true;
                    }
                }
            })
            .collect();
        quote! {
            {
                let #pat = #binding;
                let mut _min_s = usize::MAX;
                let mut _max_e = 0usize;
                let mut _found = false;
                #(#fold_stmts)*
                if _found { Some((_min_s, _max_e)) } else { None }
            }
        }
    } else {
        quote! { None }
    }
}

// ---------------------------------------------------------------------------
// Item-level doc/range generation for Vec elements
// ---------------------------------------------------------------------------

/// Generate the expression to convert a single item to Doc.
/// For `Vec<T>` types, extracts the element type `T`:
///   - If `T` is a tuple, destructures and concatenates element docs.
///   - Otherwise, calls `.to_doc()`.
pub fn generate_item_to_doc(vec_ty: &syn::Type) -> TokenStream {
    // Extract the inner type from Vec<T>.
    let inner_ty = if let syn::Type::Path(type_path) = vec_ty {
        type_path.path.segments.last().and_then(|seg| {
            if seg.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    args.args.first().and_then(|arg| {
                        if let syn::GenericArgument::Type(ty) = arg {
                            Some(ty)
                        } else {
                            None
                        }
                    })
                } else {
                    None
                }
            } else {
                None
            }
        })
    } else {
        None
    };

    if let Some(inner) = inner_ty {
        if type_is_span(inner) {
            // Span element — convert to Doc::String directly.
            return quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(item.as_str())) };
        }
    }
    if let Some(syn::Type::Tuple(tuple_ty)) = inner_ty {
        // Tuple element — destructure and chain with `+` (no Null interleaving).
        let n = tuple_ty.elems.len();
        let bindings: Vec<syn::Ident> = (0..n).map(|i| format_ident!("f{}", i)).collect();
        let pat = quote! { (#(#bindings),*) };
        let doc_parts: Vec<TokenStream> = tuple_ty
            .elems
            .iter()
            .zip(bindings.iter())
            .map(|(elem_ty, binding)| doc_for_binding(binding, elem_ty))
            .collect();
        let combined = if doc_parts.len() == 1 {
            doc_parts[0].clone()
        } else {
            let mut acc = doc_parts[0].clone();
            for part in &doc_parts[1..] {
                acc = quote! { (#acc) + (#part) };
            }
            acc
        };
        quote! {
            { let #pat = item; #combined }
        }
    } else {
        // Enum or other type — call to_doc.
        quote! { item.to_doc() }
    }
}

/// Generate the expression to get source_range from a single Vec item.
/// For tuple elements, extracts ranges from each tuple field.
pub fn generate_item_source_range(vec_ty: &syn::Type) -> TokenStream {
    // Extract the inner type from Vec<T>.
    let inner_ty = if let syn::Type::Path(type_path) = vec_ty {
        type_path.path.segments.last().and_then(|seg| {
            if seg.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    args.args.first().and_then(|arg| {
                        if let syn::GenericArgument::Type(ty) = arg {
                            Some(ty)
                        } else {
                            None
                        }
                    })
                } else {
                    None
                }
            } else {
                None
            }
        })
    } else {
        None
    };

    if let Some(inner) = inner_ty {
        if type_is_span(inner) {
            // Span element — use start/end directly.
            return quote! { Some((i.start, i.end)) };
        }
    }
    if let Some(syn::Type::Tuple(tuple_ty)) = inner_ty {
        let n = tuple_ty.elems.len();
        let bindings: Vec<syn::Ident> = (0..n).map(|j| format_ident!("f{}", j)).collect();
        let pat = quote! { (#(#bindings),*) };
        let range_parts: Vec<TokenStream> = tuple_ty
            .elems
            .iter()
            .zip(bindings.iter())
            .map(|(elem_ty, binding)| range_for_binding(binding, elem_ty))
            .collect();
        // Single-pass min/max without Vec allocation.
        let fold_stmts: Vec<TokenStream> = range_parts
            .iter()
            .map(|rp| {
                quote! {
                    if let Some((_s, _e)) = #rp {
                        if _s < _min_s { _min_s = _s; }
                        if _e > _max_e { _max_e = _e; }
                        _found = true;
                    }
                }
            })
            .collect();
        quote! {
            {
                let #pat = i;
                let mut _min_s = usize::MAX;
                let mut _max_e = 0usize;
                let mut _found = false;
                #(#fold_stmts)*
                if _found { Some((_min_s, _max_e)) } else { None }
            }
        }
    } else {
        quote! { i.source_range() }
    }
}
