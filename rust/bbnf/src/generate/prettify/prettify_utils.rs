//! Utility functions for prettify code generation.
//!
//! Contains type predicates, pattern detection, binding doc/range helpers,
//! and item-level doc/range generation extracted from the main prettify module.

use crate::types::*;

use super::super::type_inference::*;
use super::super::types::*;

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

// ---------------------------------------------------------------------------
// Pattern detection
// ---------------------------------------------------------------------------

/// Detect `"L" >> middle << "R"` (wrapped repetition).
///
/// In the BBNF AST this parses as: `Skip(Next(Literal(L), middle), Literal(R))`
/// where middle may be wrapped in OptionalWhitespace/Group/etc.
pub fn detect_wrapped_pattern(expr: &Expression) -> Option<(String, String)> {
    // Primary shape: Skip(Next(Literal(L), ...), Literal(R))
    if let Expression::Skip(left_token, right_token) = expr {
        let left = unwrap_transparent(&left_token.value);
        let right_delim = unwrap_transparent(&right_token.value);

        if let Expression::Next(next_left, _) = left {
            let next_left_inner = unwrap_transparent(&next_left.value);
            if let Expression::Literal(Token { value: l, .. }) = next_left_inner {
                if let Expression::Literal(Token { value: r, .. }) = right_delim {
                    return Some((l.to_string(), r.to_string()));
                }
            }
        }
    }
    None
}

/// Resolve a nonterminal reference and detect wrapped pattern in the referenced rule.
/// E.g. `lhs = nonterminal` where `nonterminal = "<" >> identifier << ">"`.
pub fn resolve_and_detect_wrapped<'a>(
    expr: &'a Expression<'a>,
    grammar_attrs: &'a GeneratedGrammarAttributes<'a>,
) -> Option<(String, String)> {
    if let Expression::Nonterminal(Token { value: ref_name, .. }) = expr {
        // Look up the referenced rule's RHS.
        let rhs = grammar_attrs.ast.iter().find_map(|(k, v)| {
            if let Expression::Nonterminal(t) = k {
                if t.value.as_ref() == ref_name.as_ref() { Some(v) } else { None }
            } else {
                None
            }
        })?;
        let inner = match rhs {
            Expression::Rule(inner, _) => inner.as_ref(),
            other => other,
        };
        detect_wrapped_pattern(inner)
    } else {
        None
    }
}

/// Detect `key, sep >> value` (key-value pair) pattern.
///
/// In the BBNF AST: `Concatenation([Nonterminal(key), Next(sep, value)])`
/// where sep can be a Nonterminal (like `colon`) or a Literal.
pub fn detect_key_value_pattern(expr: &Expression) -> Option<(String, String)> {
    if let Expression::Concatenation(token) = expr {
        let elems = &token.value;
        if elems.len() == 2 {
            // Second element should be Next(sep, value)
            if let Expression::Next(sep, _) = &elems[1] {
                let sep_inner = unwrap_transparent(&sep.value);
                // sep can be a Literal (e.g., ":")
                if let Expression::Literal(Token { value: sep_val, .. }) = sep_inner {
                    return Some(("key".to_string(), sep_val.to_string()));
                }
                // sep can be a Nonterminal (e.g., colon = ":" ?w)
                // In that case, look up what the nonterminal represents.
                if let Expression::Nonterminal(Token { value: sep_name, .. }) = sep_inner {
                    // Emit the separator name; the actual separator string
                    // will be inferred from the nonterminal's definition.
                    return Some(("key".to_string(), sep_name.to_string()));
                }
            }
        }
    }
    None
}

/// If `name` is a nonterminal rule name whose body is a simple literal (possibly
/// wrapped in OptionalWhitespace), return that literal string. Otherwise None.
pub fn resolve_separator_literal(name: &str, grammar_attrs: &GeneratedGrammarAttributes) -> Option<String> {
    for (k, v) in grammar_attrs.ast.iter() {
        if let Expression::Nonterminal(t) = k {
            if t.value.as_ref() == name {
                let inner = match v {
                    Expression::Rule(inner, _) => inner.as_ref(),
                    other => other,
                };
                let unwrapped = unwrap_transparent(inner);
                if let Expression::Literal(Token { value: lit, .. }) = unwrapped {
                    return Some(lit.to_string());
                }
            }
        }
    }
    None
}

/// Unwrap transparent expression wrappers (OptionalWhitespace, Group).
pub fn unwrap_transparent<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::OptionalWhitespace(inner) => unwrap_transparent(&inner.value),
        Expression::Group(inner) => unwrap_transparent(&inner.value),
        other => other,
    }
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
        quote! {
            match #binding {
                Some(inner) => inner.to_doc(),
                None => ::pprint::Doc::Null,
            }
        }
    } else if is_box_enum_type(ty) {
        quote! { #binding.to_doc() }
    } else if let syn::Type::Tuple(tuple_ty) = ty {
        // Nested tuple — destructure and concat.
        let n = tuple_ty.elems.len();
        let inner_bindings: Vec<_> = (0..n).map(|i| format_ident!("t{}", i)).collect();
        let pat = quote! { (#(#inner_bindings),*) };
        let doc_parts: Vec<TokenStream> = tuple_ty.elems.iter().zip(inner_bindings.iter()).map(|(elem_ty, b)| {
            doc_for_binding(b, elem_ty)
        }).collect();
        quote! {
            { let #pat = #binding; ::pprint::concat(vec![#(#doc_parts),*]) }
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
        quote! {
            {
                let ranges: Vec<_> = #binding.iter().filter_map(|i| #item_range).collect();
                if ranges.is_empty() {
                    None
                } else {
                    Some((
                        ranges.iter().map(|r| r.0).min().unwrap(),
                        ranges.iter().map(|r| r.1).max().unwrap(),
                    ))
                }
            }
        }
    } else if is_option_type(ty) {
        quote! { #binding.as_ref().and_then(|v| v.source_range()) }
    } else if is_box_enum_type(ty) {
        quote! { #binding.source_range() }
    } else if let syn::Type::Tuple(tuple_ty) = ty {
        // Nested tuple — get range from each element.
        let n = tuple_ty.elems.len();
        let inner_bindings: Vec<_> = (0..n).map(|i| format_ident!("t{}", i)).collect();
        let pat = quote! { (#(#inner_bindings),*) };
        let range_parts: Vec<TokenStream> = tuple_ty.elems.iter().zip(inner_bindings.iter()).map(|(elem_ty, b)| {
            range_for_binding(b, elem_ty)
        }).collect();
        quote! {
            {
                let #pat = #binding;
                let ranges: Vec<Option<(usize, usize)>> = vec![#(#range_parts),*];
                let valid: Vec<_> = ranges.into_iter().flatten().collect();
                if valid.is_empty() { None }
                else { Some((valid.iter().map(|r| r.0).min().unwrap(), valid.iter().map(|r| r.1).max().unwrap())) }
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

    if let Some(syn::Type::Tuple(tuple_ty)) = inner_ty {
        // Tuple element — destructure and concat using doc_for_binding for each element.
        let n = tuple_ty.elems.len();
        let bindings: Vec<syn::Ident> = (0..n).map(|i| format_ident!("f{}", i)).collect();
        let pat = quote! { (#(#bindings),*) };
        let doc_parts: Vec<TokenStream> = tuple_ty.elems.iter().zip(bindings.iter()).map(|(elem_ty, binding)| {
            doc_for_binding(binding, elem_ty)
        }).collect();
        quote! {
            { let #pat = item; ::pprint::concat(vec![#(#doc_parts),*]) }
        }
    } else {
        // Simple element — call to_doc directly.
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

    if let Some(syn::Type::Tuple(tuple_ty)) = inner_ty {
        let n = tuple_ty.elems.len();
        let bindings: Vec<syn::Ident> = (0..n).map(|j| format_ident!("f{}", j)).collect();
        let pat = quote! { (#(#bindings),*) };
        let range_parts: Vec<TokenStream> = tuple_ty.elems.iter().zip(bindings.iter()).map(|(elem_ty, binding)| {
            range_for_binding(binding, elem_ty)
        }).collect();
        quote! {
            { let #pat = i; let rs: Vec<_> = [#(#range_parts),*].iter().filter_map(|r| *r).collect();
              if rs.is_empty() { None } else { Some((rs.iter().map(|r| r.0).min().unwrap(), rs.iter().map(|r| r.1).max().unwrap())) } }
        }
    } else {
        quote! { i.source_range() }
    }
}
