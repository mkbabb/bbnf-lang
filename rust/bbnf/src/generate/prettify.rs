//! Prettify code generation from grammar expressions.
//!
//! Generates `to_doc()` and `source_range()` methods on the parser enum,
//! driven by structural inference from grammar expressions and optional
//! `@pretty` directive hints.

use crate::analysis::get_nonterminal_name;
use crate::types::*;

use super::type_inference::*;
use super::types::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Recognised `@pretty` hint keywords.
const VALID_HINTS: &[&str] = &["group", "block", "indent", "blankline", "softbreak", "nobreak"];

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

        // Validate hints.
        for hint in &hints {
            if !VALID_HINTS.contains(&hint.as_str()) {
                panic!(
                    "@pretty directive for rule `{}` has unknown hint `{}`; valid: {:?}",
                    name, hint, VALID_HINTS
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
                        ::pprint::Doc::Concat(vec![#(#doc_parts),*])
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
    } else if hints.contains(&"block".to_string()) {
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
                ::pprint::Doc::Join(Box::new(#sep), docs)
            }
        }
    };

    let doc = apply_outer_hints(base, hints);
    quote! {
        Self::#variant(items) => { #doc }
    }
}

/// Generate the expression to convert a single item to Doc.
/// For `Vec<T>` types, extracts the element type `T`:
///   - If `T` is a tuple, destructures and concatenates element docs.
///   - Otherwise, calls `.to_doc()`.
fn generate_item_to_doc(vec_ty: &syn::Type) -> TokenStream {
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
            { let #pat = item; ::pprint::Doc::Concat(vec![#(#doc_parts),*]) }
        }
    } else {
        // Simple element — call to_doc directly.
        quote! { item.to_doc() }
    }
}

/// Generate the expression to get source_range from a single Vec item.
/// For tuple elements, extracts ranges from each tuple field.
fn generate_item_source_range(vec_ty: &syn::Type) -> TokenStream {
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
            // Interleave with spaces or softlines using Doc::Concat to avoid
            // nested `+` operator chains that produce invalid TokenStreams for 4+ elements.
            let sep = if hints.contains(&"nobreak".to_string()) {
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
            quote! { ::pprint::Doc::Concat(vec![#(#interleaved),*]) }
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
// Pattern detection
// ---------------------------------------------------------------------------

/// Detect `"L" >> middle << "R"` (wrapped repetition).
///
/// In the BBNF AST this parses as: `Skip(Next(Literal(L), middle), Literal(R))`
/// where middle may be wrapped in OptionalWhitespace/Group/etc.
fn detect_wrapped_pattern(expr: &Expression) -> Option<(String, String)> {
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
fn resolve_and_detect_wrapped<'a>(
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
fn detect_key_value_pattern(expr: &Expression) -> Option<(String, String)> {
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

/// Unwrap transparent expression wrappers (OptionalWhitespace, Group).
fn unwrap_transparent<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::OptionalWhitespace(inner) => unwrap_transparent(&inner.value),
        Expression::Group(inner) => unwrap_transparent(&inner.value),
        other => other,
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
        let base = quote! {
            {
                let items_docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| item.to_doc()).collect();
                if items_docs.is_empty() {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(concat!(#left, #right)))
                } else {
                    ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#left))
                        + ::pprint::Doc::SmartJoin(
                            Box::new(
                                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(","))
                                    + ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" "))
                            ),
                            items_docs,
                        )
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

/// If `name` is a nonterminal rule name whose body is a simple literal (possibly
/// wrapped in OptionalWhitespace), return that literal string. Otherwise None.
fn resolve_separator_literal(name: &str, grammar_attrs: &GeneratedGrammarAttributes) -> Option<String> {
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
            _ => result, // block, blankline, nobreak, softbreak are handled in join generation
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

// ---------------------------------------------------------------------------
// Type inspection helpers
// ---------------------------------------------------------------------------

fn is_vec_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(path) = ty {
        path.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Vec")
    } else {
        false
    }
}

fn is_option_type(ty: &syn::Type) -> bool {
    if let syn::Type::Path(path) = ty {
        path.path
            .segments
            .last()
            .is_some_and(|seg| seg.ident == "Option")
    } else {
        false
    }
}

fn is_box_enum_type(ty: &syn::Type) -> bool {
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
// Binding doc/range helpers for tuple elements
// ---------------------------------------------------------------------------

fn doc_for_binding(binding: &syn::Ident, ty: &syn::Type) -> TokenStream {
    if type_is_span(ty) {
        quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#binding.as_str())) }
    } else if is_vec_type(ty) {
        let item_doc = generate_item_to_doc(ty);
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = #binding.iter().map(|item| #item_doc).collect();
                ::pprint::Doc::Join(Box::new(::pprint::Doc::Softline), docs)
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
            { let #pat = #binding; ::pprint::Doc::Concat(vec![#(#doc_parts),*]) }
        }
    } else {
        // Unknown type — emit Null.
        quote! { ::pprint::Doc::Null }
    }
}

fn range_for_binding(binding: &syn::Ident, ty: &syn::Type) -> TokenStream {
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
