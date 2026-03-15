//! Doc generation functions for prettify code generation.
//!
//! Contains `generate_*_doc` functions that convert AST/IR nodes to pprint Doc
//! types, plus `apply_hints` / `apply_outer_hints` for wrapping generated Doc
//! expressions in structural hint wrappers (Group, Indent, Dedent).

use super::prettify_utils::*;

use super::hints::{extract_sep_string, extract_split_delim};

use proc_macro2::TokenStream;
use quote::quote;

// ---------------------------------------------------------------------------
// Doc generation for Span-typed variants (leaf nodes)
// ---------------------------------------------------------------------------

pub(crate) fn generate_span_doc(variant: &syn::Ident, hints: &[String]) -> TokenStream {
    // Check for split("...") hint — split the span text at format time.
    let split_delim = hints.iter().find_map(|h| extract_split_delim(h));

    if let Some(delim_str) = split_delim {
        // The delimiter must be a single ASCII byte.
        let delim_byte = delim_str.as_bytes()[0];
        let delim_lit = proc_macro2::Literal::u8_suffixed(delim_byte);

        // Determine the separator Doc from a co-occurring sep("...") hint.
        let custom_sep = hints.iter().find_map(|h| extract_sep_string(h));
        let has_group = hints.contains(&"group".to_string());

        let sep_doc = if let Some(sep_str) = custom_sep {
            if has_group {
                let sep_lit = proc_macro2::Literal::string(sep_str);
                let break_sep = sep_str.trim_end();
                let break_lit = proc_macro2::Literal::string(break_sep);
                quote! {
                    ::pprint::Doc::IfBreak(
                        Box::new(
                            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                            + ::pprint::Doc::Hardline
                        ),
                        Box::new(
                            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit))
                        ),
                    )
                }
            } else {
                let sep_lit = proc_macro2::Literal::string(sep_str);
                quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit)) }
            }
        } else {
            // Default separator: IfBreak(Hardline, Space)
            quote! {
                ::pprint::Doc::IfBreak(
                    Box::new(::pprint::Doc::Hardline),
                    Box::new(::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")))
                )
            }
        };

        let base = quote! {
            {
                let text = s.as_str();
                // Fast path: skip split_balanced + Vec alloc when delimiter is absent.
                if ::parse_that::contains_delimiter(text, #delim_lit) {
                    let parts = ::parse_that::split_balanced(text, #delim_lit);
                    if parts.len() > 1 {
                        let sep = #sep_doc;
                        let docs: Vec<::pprint::Doc<'a>> = parts.iter()
                            .map(|s| ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.trim())))
                            .collect();
                        ::pprint::Doc::Join(Box::new((sep, docs)))
                    } else {
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(text))
                    }
                } else {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(text))
                }
            }
        };

        // Apply remaining hints (group, indent, etc.), filtering out split/sep
        // which are already consumed above.
        let filtered_hints: Vec<String> = hints
            .iter()
            .filter(|h| !super::hints::is_split_hint(h) && !super::hints::is_sep_hint(h))
            .cloned()
            .collect();
        let doc = apply_hints(base, &filtered_hints);
        quote! {
            Self::#variant(s) => { #doc }
        }
    } else {
        let base = quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str())) };
        let doc = apply_hints(base, hints);
        quote! {
            Self::#variant(s) => { #doc }
        }
    }
}

/// Wrapped Span: `"L" >> inner << "R"` where inner is a Span.
/// Re-emits the delimiters that Skip/Next stripped during parsing.
pub(crate) fn generate_wrapped_span_doc(
    variant: &syn::Ident,
    left: &str,
    right: &str,
    hints: &[String],
) -> TokenStream {
    let left_lit = proc_macro2::Literal::string(left);
    let right_lit = proc_macro2::Literal::string(right);
    let base = quote! {
        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#left_lit))
            + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str()))
            + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#right_lit))
    };
    let doc = apply_hints(base, hints);
    quote! {
        Self::#variant(s) => { #doc }
    }
}

// ---------------------------------------------------------------------------
// Wrapped pattern doc generation
// ---------------------------------------------------------------------------

pub(crate) fn generate_wrapped_doc(
    variant: &syn::Ident,
    left: &str,
    right: &str,
    ty: &syn::Type,
    hints: &[String],
) -> TokenStream {
    // The inner type determines how to destructure.
    let is_vec = is_vec_type(ty);

    if is_vec {
        let base = quote! {
            {
                let items_docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| item.to_doc()).collect();
                if items_docs.is_empty() {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(concat!(#left, #right)))
                } else {
                    // IfBreak separator: when Group breaks → ",\n" (one item per line),
                    // when it fits → ", " (inline).
                    let break_sep = ::pprint::Doc::IfBreak(
                        Box::new(
                            ::pprint::Doc::Char(b',')
                            + ::pprint::Doc::Hardline
                        ),
                        Box::new(
                            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(", "))
                        ),
                    );
                    // Build concat of items with IfBreak separators so break_mode
                    // propagates from the enclosing Group to each separator.
                    let mut body = items_docs[0].clone();
                    for item in &items_docs[1..] {
                        body = body + break_sep.clone() + item.clone();
                    }
                    let line_or_nothing = ::pprint::Doc::IfBreak(
                        Box::new(::pprint::Doc::Hardline),
                        Box::new(::pprint::Doc::Null),
                    );
                    ::pprint::Doc::Group(Box::new(
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#left))
                            + ::pprint::Doc::Indent(Box::new(
                                line_or_nothing.clone() + body
                            ))
                            + line_or_nothing
                            + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#right))
                    ))
                }
            }
        };
        // The base already contains a Group — filter it from outer hints to avoid double-wrapping.
        let outer_hints: Vec<String> = hints.iter()
            .filter(|h| h.as_str() != "group")
            .cloned()
            .collect();
        let doc = apply_outer_hints(base, &outer_hints);
        quote! {
            Self::#variant(items) => { #doc }
        }
    } else {
        // Single item wrapped.
        let base = quote! {
            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#left))
                + ::pprint::Doc::Group(Box::new(
                    ::pprint::Doc::Indent(Box::new(
                        ::pprint::Doc::Softline + val.to_doc()
                    ))
                    + ::pprint::Doc::Softline
                ))
                + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#right))
        };
        let doc = apply_outer_hints(base, hints);
        quote! {
            Self::#variant(val) => { #doc }
        }
    }
}

// ---------------------------------------------------------------------------
// @pretty hint application
// ---------------------------------------------------------------------------

/// Apply hints as wrappers around a Doc expression.
pub(crate) fn apply_hints(doc: TokenStream, hints: &[String]) -> TokenStream {
    let mut result = doc;
    for hint in hints {
        if super::hints::is_sep_hint(hint) || super::hints::is_split_hint(hint) {
            continue; // sep/split are handled at separator selection, not as wrappers.
        }
        result = match hint.as_str() {
            "group" => quote! { ::pprint::Doc::Group(Box::new(#result)) },
            "indent" => quote! { ::pprint::Doc::Indent(Box::new(#result)) },
            "dedent" => quote! { ::pprint::Doc::Dedent(Box::new(#result)) },
            "block" | "blankline" | "nobreak" | "softbreak" | "hardbreak" | "compact" | "fast" | "off" => result,
            other => panic!("Unknown @pretty hint `{}` in apply_hints()", other),
        };
    }
    result
}

/// Apply only outer structural hints (group, indent, dedent) without modifying join separators.
pub(crate) fn apply_outer_hints(doc: TokenStream, hints: &[String]) -> TokenStream {
    let mut result = doc;
    for hint in hints {
        if super::hints::is_sep_hint(hint) || super::hints::is_split_hint(hint) {
            continue; // sep/split are handled at separator selection, not as wrappers.
        }
        result = match hint.as_str() {
            "group" => quote! { ::pprint::Doc::Group(Box::new(#result)) },
            "indent" => quote! { ::pprint::Doc::Indent(Box::new(#result)) },
            "dedent" => quote! { ::pprint::Doc::Dedent(Box::new(#result)) },
            "block" | "blankline" | "nobreak" | "softbreak" | "hardbreak" | "compact" | "fast" | "off" => result,
            other => panic!("Unknown @pretty hint `{}` in apply_outer_hints()", other),
        };
    }
    result
}
