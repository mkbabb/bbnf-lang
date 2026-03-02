//! Prettify code generation from grammar expressions.
//!
//! Generates `to_doc()` and `source_range()` methods on the parser enum,
//! driven by structural inference from grammar expressions and optional
//! `@pretty` directive hints.

mod prettify_utils;
pub use prettify_utils::*;

use crate::analysis::get_nonterminal_name;
use crate::types::*;

use super::type_inference::*;
use super::types::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Recognised `@pretty` hint keywords.
const VALID_HINTS: &[&str] = &["group", "block", "indent", "blankline", "nobreak", "fast"];

/// Generate `to_doc()` and `source_range()` impl blocks for the parser enum.
///
/// Returns a `TokenStream` containing:
/// ```ignore
/// impl<'a> EnumName<'a> {
///     pub fn to_doc(&self) -> ::pprint::Doc<'a> { ... }
///     pub fn source_range(&self) -> Option<(usize, usize)> { ... }
/// }
/// ```
pub fn generate_prettify(
    grammar_attrs: &GeneratedGrammarAttributes,
    nonterminal_types: &TypeCache,
) -> TokenStream {
    let enum_ident = grammar_attrs.enum_ident;
    let has_recovers = grammar_attrs
        .recovers
        .is_some_and(|r| !r.is_empty());

    let mut to_doc_arms = Vec::new();
    let mut source_range_arms = Vec::new();

    for (expr, ty) in nonterminal_types.iter() {
        let Some(name) = get_nonterminal_name(expr) else {
            continue;
        };

        // Skip transparent rules — they don't have enum variants.
        if is_transparent_rule(name, grammar_attrs) {
            continue;
        }

        let variant = format_ident!("{}", name);

        // Find the rule's RHS expression.
        let rhs = grammar_attrs.ast.iter().find_map(|(k, v)| {
            if let Expression::Nonterminal(t) = k {
                if t.value.as_ref() == name {
                    Some(v)
                } else {
                    None
                }
            } else {
                None
            }
        });

        let Some(rhs) = rhs else { continue };

        // Unwrap Rule wrapper.
        let inner = match rhs {
            Expression::Rule(inner, _) => inner.as_ref(),
            other => other,
        };

        // Get @pretty hints for this rule (if any).
        let hints: Vec<String> = grammar_attrs
            .pretties
            .and_then(|p| p.get(name))
            .cloned()
            .unwrap_or_default();

        // Validate hints — unknown hints are user errors in the grammar file.
        for hint in &hints {
            if !VALID_HINTS.contains(&hint.as_str()) {
                panic!(
                    "@pretty directive for rule `{}` contains unknown hint `{}`. \
                     Valid hints are: {}",
                    name, hint, VALID_HINTS.join(", ")
                );
            }
        }

        // Determine the type shape to know how to destructure.
        let is_span = type_is_span(ty);
        let is_vec = is_vec_type(ty);

        // Check for wrapped pattern BEFORE type dispatch — wrapped patterns
        // like `"[" >> items* << "]"` produce Vec types but need bracket formatting.
        // Resolve through nonterminal references (e.g. `lhs = nonterminal`)
        // to find the underlying wrapped pattern.
        let wrapped = detect_wrapped_pattern(inner)
            .or_else(|| resolve_and_detect_wrapped(inner, grammar_attrs));

        // Generate the to_doc match arm.
        let doc_body = if let Some((ref left, ref right)) = wrapped {
            if is_span {
                // Wrapped pattern with Span output (e.g. "<" >> identifier << ">")
                // — re-emit the stripped delimiters around the span content.
                generate_wrapped_span_doc(&variant, left, right, &hints)
            } else {
                // Wrapped pattern: "L" >> middle << "R" → bracket-indent-join
                generate_wrapped_doc(&variant, left, right, ty, &hints)
            }
        } else if is_span {
            // Leaf: Span → Doc::from(span.as_str())
            generate_span_doc(&variant, &hints)
        } else if is_vec {
            // Vec<...> without wrapping → join items
            generate_vec_doc(&variant, inner, ty, &hints)
        } else {
            // Compound type — infer from expression shape.
            generate_compound_doc(&variant, inner, ty, &hints, grammar_attrs)
        };
        to_doc_arms.push(doc_body);

        // Generate source_range arm.
        let range_body = if is_span {
            quote! {
                Self::#variant(s) => Some((s.start, s.end)),
            }
        } else if is_vec {
            let item_source_range = generate_item_source_range(ty);
            quote! {
                Self::#variant(items) => {
                    let starts: Vec<_> = items.iter().filter_map(|i| #item_source_range).collect();
                    if starts.is_empty() {
                        None
                    } else {
                        Some((
                            starts.iter().map(|r| r.0).min().unwrap(),
                            starts.iter().map(|r| r.1).max().unwrap(),
                        ))
                    }
                }
            }
        } else {
            // For tuples and Box<Enum>, try to delegate.
            generate_compound_range(&variant, ty)
        };
        source_range_arms.push(range_body);
    }

    // Add sub-variant arms for heterogeneous alternation branches.
    if let Some(sub_variants) = grammar_attrs.sub_variants {
        let mut seen = std::collections::HashSet::new();
        for (variant_name, ty) in sub_variants.values().flatten() {
            if !seen.insert(variant_name.clone()) {
                continue;
            }
            let variant = format_ident!("{}", variant_name);

            // For tuples, destructure and concatenate element docs.
            if let syn::Type::Tuple(tuple_ty) = ty {
                let n = tuple_ty.elems.len();
                let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
                let pat = quote! { (#(#bindings),*) };

                // Each element — use doc_for_binding to handle all type shapes.
                let doc_parts: Vec<_> = tuple_ty.elems.iter().zip(bindings.iter()).map(|(elem_ty, binding)| {
                    doc_for_binding(binding, elem_ty)
                }).collect();

                to_doc_arms.push(quote! {
                    Self::#variant(#pat) => {
                        ::pprint::concat(vec![#(#doc_parts),*])
                    }
                });

                // source_range — use range_for_binding to handle all type shapes.
                let range_parts: Vec<_> = tuple_ty.elems.iter().zip(bindings.iter()).map(|(elem_ty, binding)| {
                    range_for_binding(binding, elem_ty)
                }).collect();

                source_range_arms.push(quote! {
                    Self::#variant(#pat) => {
                        let ranges: Vec<_> = [#(#range_parts),*].iter().filter_map(|r| *r).collect();
                        if ranges.is_empty() {
                            None
                        } else {
                            Some((
                                ranges.iter().map(|r| r.0).min().unwrap(),
                                ranges.iter().map(|r| r.1).max().unwrap(),
                            ))
                        }
                    }
                });
            } else if type_is_span(ty) {
                // Span sub-variant — emit as Doc::String.
                to_doc_arms.push(quote! {
                    Self::#variant(s) => ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str())),
                });
                source_range_arms.push(quote! {
                    Self::#variant(s) => Some((s.start, s.end)),
                });
            } else if is_box_enum_type(ty) {
                // Box<Enum> sub-variant — delegate.
                to_doc_arms.push(quote! {
                    Self::#variant(val) => val.to_doc(),
                });
                source_range_arms.push(quote! {
                    Self::#variant(val) => val.source_range(),
                });
            } else {
                // Unknown sub-variant type — emit Null / None.
                to_doc_arms.push(quote! {
                    Self::#variant(_) => ::pprint::Doc::Null,
                });
                source_range_arms.push(quote! {
                    Self::#variant(_) => None,
                });
            }
        }
    }

    // Add Recovered variant handling if recovers exist.
    if has_recovers {
        to_doc_arms.push(quote! {
            Self::Recovered => ::pprint::Doc::Null,
        });
        source_range_arms.push(quote! {
            Self::Recovered => None,
        });
    }

    quote! {
        impl<'a> #enum_ident<'a> {
            pub fn to_doc(&self) -> ::pprint::Doc<'a> {
                match self {
                    #(#to_doc_arms)*
                }
            }

            pub fn source_range(&self) -> Option<(usize, usize)> {
                match self {
                    #(#source_range_arms)*
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Doc generation for Span-typed variants (leaf nodes)
// ---------------------------------------------------------------------------

fn generate_span_doc(variant: &syn::Ident, hints: &[String]) -> TokenStream {
    let base = quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str())) };
    let doc = apply_hints(base, hints);
    quote! {
        Self::#variant(s) => { #doc }
    }
}

/// Wrapped Span: `"L" >> inner << "R"` where inner is a Span.
/// Re-emits the delimiters that Skip/Next stripped during parsing.
fn generate_wrapped_span_doc(
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
// Doc generation for Vec-typed variants (repetition)
// ---------------------------------------------------------------------------

fn generate_vec_doc(
    variant: &syn::Ident,
    _inner: &Expression,
    ty: &syn::Type,
    hints: &[String],
) -> TokenStream {
    // Default: softline-separated join of items.
    let sep = if hints.contains(&"blankline".to_string()) {
        quote! { ::pprint::Doc::Hardline + ::pprint::Doc::Hardline }
    } else if hints.contains(&"block".to_string()) || hints.contains(&"fast".to_string()) {
        quote! { ::pprint::Doc::Hardline }
    } else if hints.contains(&"nobreak".to_string()) {
        quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) }
    } else {
        quote! { ::pprint::Doc::Softline }
    };

    // Check if the Vec's element type is a tuple — if so, generate a
    // destructuring lambda that concatenates the element docs.
    let item_to_doc = generate_item_to_doc(ty);

    let base = quote! {
        {
            let docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| #item_to_doc).collect();
            if docs.is_empty() {
                ::pprint::Doc::Null
            } else {
                ::pprint::Doc::Join(Box::new((#sep, docs)))
            }
        }
    };

    let doc = apply_outer_hints(base, hints);
    quote! {
        Self::#variant(items) => { #doc }
    }
}

// ---------------------------------------------------------------------------
// Doc generation for compound types (tuples, Box<Enum>, Option)
// ---------------------------------------------------------------------------

fn generate_compound_doc(
    variant: &syn::Ident,
    inner: &Expression,
    ty: &syn::Type,
    hints: &[String],
    _grammar_attrs: &GeneratedGrammarAttributes,
) -> TokenStream {
    // Check for common patterns.

    // Pattern 1: Wrapped repetition — "L" >> (items)* << "R"
    if let Some((left_lit, right_lit)) = detect_wrapped_pattern(inner) {
        return generate_wrapped_doc(variant, &left_lit, &right_lit, ty, hints);
    }

    // Pattern 2: Key-value pair — key , sep >> value
    if let Some((key_name, sep_lit)) = detect_key_value_pattern(inner) {
        return generate_key_value_doc(variant, &key_name, &sep_lit, hints, _grammar_attrs);
    }

    // Default: generate based on type shape.
    if is_box_enum_type(ty) {
        // Box<Enum> — recurse.
        let doc = quote! { val.to_doc() };
        let doc = apply_hints(doc, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    if is_option_type(ty) {
        let base = quote! {
            match val {
                Some(inner) => inner.to_doc(),
                None => ::pprint::Doc::Null,
            }
        };
        let doc = apply_hints(base, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    // Tuple type — concatenation: space-separate children.
    if let syn::Type::Tuple(tuple) = ty {
        let n = tuple.elems.len();
        let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
        let pattern = quote! { (#(#bindings),*) };

        let parts: Vec<TokenStream> = bindings
            .iter()
            .enumerate()
            .map(|(i, binding)| {
                let elem_ty = &tuple.elems[i];
                doc_for_binding(binding, elem_ty)
            })
            .collect();

        let combined = if parts.len() == 1 {
            parts[0].clone()
        } else {
            // Interleave with spaces or softlines using concat() to dispatch to
            // DoubleDoc/TripleDoc for 2-3 elements, avoiding Vec heap allocation.
            let sep = if hints.contains(&"fast".to_string()) {
                quote! { ::pprint::Doc::Hardline }
            } else if hints.contains(&"nobreak".to_string()) {
                quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) }
            } else {
                quote! { ::pprint::Doc::Softline }
            };

            let mut interleaved: Vec<proc_macro2::TokenStream> = Vec::new();
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    interleaved.push(sep.clone());
                }
                interleaved.push(part.clone());
            }
            quote! { ::pprint::concat(vec![#(#interleaved),*]) }
        };

        let doc = apply_hints(combined, hints);
        return quote! {
            Self::#variant(#pattern) => { #doc }
        };
    }

    // Fallback: just try to_doc() on the value.
    let base = quote! { ::pprint::Doc::Null };
    let doc = apply_hints(base, hints);
    quote! {
        Self::#variant(_) => { #doc }
    }
}

fn generate_compound_range(variant: &syn::Ident, ty: &syn::Type) -> TokenStream {
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

        return quote! {
            Self::#variant(#pattern) => {
                let ranges: Vec<Option<(usize, usize)>> = vec![#(#range_exprs),*];
                let valid: Vec<_> = ranges.into_iter().flatten().collect();
                if valid.is_empty() {
                    None
                } else {
                    Some((
                        valid.iter().map(|r| r.0).min().unwrap(),
                        valid.iter().map(|r| r.1).max().unwrap(),
                    ))
                }
            }
        };
    }

    // Fallback.
    quote! {
        Self::#variant(_) => None,
    }
}

// ---------------------------------------------------------------------------
// Wrapped pattern doc generation
// ---------------------------------------------------------------------------

fn generate_wrapped_doc(
    variant: &syn::Ident,
    left: &str,
    right: &str,
    ty: &syn::Type,
    hints: &[String],
) -> TokenStream {
    // The inner type determines how to destructure.
    let is_vec = is_vec_type(ty);

    if is_vec {
        let join_expr = if hints.contains(&"fast".to_string()) {
            quote! {
                ::pprint::Doc::Join(
                    Box::new((
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(","))
                            + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")),
                        items_docs,
                    ))
                )
            }
        } else {
            quote! {
                ::pprint::Doc::SmartJoin(
                    Box::new((
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(","))
                            + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")),
                        items_docs,
                    ))
                )
            }
        };

        let base = quote! {
            {
                let items_docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| item.to_doc()).collect();
                if items_docs.is_empty() {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(concat!(#left, #right)))
                } else {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#left))
                        + #join_expr
                        + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#right))
                }
            }
        };
        let doc = apply_outer_hints(base, hints);
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
// Key-value pattern doc generation
// ---------------------------------------------------------------------------

fn generate_key_value_doc(
    variant: &syn::Ident,
    _key_name: &str,
    sep: &str,
    hints: &[String],
    grammar_attrs: &GeneratedGrammarAttributes,
) -> TokenStream {
    // Resolve separator: if sep is a rule name, look up the literal it represents.
    let sep_str = resolve_separator_literal(sep, grammar_attrs).unwrap_or_else(|| sep.to_string());
    let sep_with_space = format!("{} ", sep_str.trim());
    let base = quote! {
        {
            let (key, val) = inner;
            ::pprint::Doc::String(::std::borrow::Cow::Borrowed(key.as_str()))
                + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_with_space))
                + val.to_doc()
        }
    };
    let doc = apply_hints(base, hints);
    quote! {
        Self::#variant(inner) => { #doc }
    }
}

// ---------------------------------------------------------------------------
// @pretty hint application
// ---------------------------------------------------------------------------

/// Apply hints as wrappers around a Doc expression.
fn apply_hints(doc: TokenStream, hints: &[String]) -> TokenStream {
    let mut result = doc;
    for hint in hints {
        result = match hint.as_str() {
            "group" => quote! { ::pprint::Doc::Group(Box::new(#result)) },
            "indent" => quote! { ::pprint::Doc::Indent(Box::new(#result)) },
            _ => result, // block, blankline, nobreak, fast are handled in join generation
        };
    }
    result
}

/// Apply only outer structural hints (group, indent) without modifying join separators.
fn apply_outer_hints(doc: TokenStream, hints: &[String]) -> TokenStream {
    let mut result = doc;
    for hint in hints {
        result = match hint.as_str() {
            "group" => quote! { ::pprint::Doc::Group(Box::new(#result)) },
            "indent" => quote! { ::pprint::Doc::Indent(Box::new(#result)) },
            _ => result,
        };
    }
    result
}
