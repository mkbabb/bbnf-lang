//! Doc generation wrappers for IR pretty-printer codegen.
//!
//! Core codegen functions that emit `TokenStream` for prettify: Vec docs,
//! compound docs, key-value docs, and sub-variant arms.

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::patterns::{
    detect_key_value_pattern_ir, detect_wrapped_pattern_ir, resolve_separator_literal_ir,
};
use crate::generate::ir_types::{type_desc_to_syn, type_is_span, IrCodegenCtx};
use crate::generate::prettify::prettify_utils::*;
use crate::generate::prettify::to_doc::*;

/// Vec doc generation -- same as AST version but without Expression parameter.
pub(crate) fn generate_vec_doc_ir(
    variant: &syn::Ident,
    ty: &syn::Type,
    hints: &[String],
) -> TokenStream {
    // Reuse generate_vec_doc by passing a dummy Expression.
    // The Expression parameter `_inner` is unused in generate_vec_doc.
    // We duplicate the Vec logic here to avoid the unused dep.

    use crate::generate::prettify::hints::extract_sep_string;

    let custom_sep = hints.iter().find_map(|h| extract_sep_string(h));

    let sep = if let Some(sep_str) = custom_sep {
        let has_group = hints.contains(&"group".to_string());
        let has_hardbreak =
            hints.contains(&"hardbreak".to_string()) || hints.contains(&"block".to_string());
        if has_hardbreak {
            // Non-filling: trimmed separator + hardline (one item per line).
            let break_sep = sep_str.trim_end();
            let break_lit = proc_macro2::Literal::string(break_sep);
            quote! {
                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                + ::pprint::Doc::Hardline
            }
        } else if has_group {
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
    } else if hints.contains(&"blankline".to_string()) {
        quote! { ::pprint::Doc::Hardline + ::pprint::Doc::Hardline }
    } else if hints.contains(&"block".to_string())
        || hints.contains(&"fast".to_string())
        || hints.contains(&"hardbreak".to_string())
    {
        quote! { ::pprint::Doc::Hardline }
    } else if hints.contains(&"nobreak".to_string()) {
        quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) }
    } else if hints.contains(&"compact".to_string()) {
        // No separator -- matches VM's Doc::Concat behavior for compact.
        quote! { ::pprint::Doc::Null }
    } else if hints.contains(&"softbreak".to_string()) {
        quote! { ::pprint::Doc::Softline }
    } else if hints.contains(&"off".to_string()) {
        // No separator -- matches VM/tuple path behavior for off.
        quote! { ::pprint::Doc::Null }
    } else {
        quote! { ::pprint::Doc::Softline }
    };

    let item_to_doc = generate_item_to_doc(ty);

    let has_indent = hints.contains(&"indent".to_string());
    let has_hard_sep = hints.contains(&"block".to_string())
        || hints.contains(&"blankline".to_string())
        || hints.contains(&"hardbreak".to_string())
        || hints.contains(&"fast".to_string());

    let base = if has_indent && has_hard_sep {
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| #item_to_doc).collect();
                if docs.is_empty() {
                    ::pprint::Doc::Null
                } else {
                    ::pprint::Doc::Indent(Box::new(
                        ::pprint::Doc::Hardline
                            + ::pprint::Doc::Join(Box::new((#sep, docs)))
                    ))
                    + ::pprint::Doc::Hardline
                }
            }
        }
    } else {
        quote! {
            {
                let docs: Vec<::pprint::Doc<'a>> = items.iter().map(|item| #item_to_doc).collect();
                if docs.is_empty() {
                    ::pprint::Doc::Null
                } else {
                    ::pprint::Doc::Join(Box::new((#sep, docs)))
                }
            }
        }
    };

    let outer_hints: Vec<String> = if has_indent && has_hard_sep {
        hints
            .iter()
            .filter(|h| h.as_str() != "indent")
            .cloned()
            .collect()
    } else {
        hints.to_vec()
    };
    let doc = apply_outer_hints(base, &outer_hints);
    quote! {
        Self::#variant(items) => { #doc }
    }
}

/// Compound doc generation from IR -- replaces the AST-based version.
pub(crate) fn generate_compound_doc_ir(
    variant: &syn::Ident,
    inner: &IrNode,
    ty: &syn::Type,
    hints: &[String],
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    use crate::generate::prettify::hints::extract_sep_string;

    // Pattern 1: Wrapped repetition.
    if let Some((left_lit, right_lit)) = detect_wrapped_pattern_ir(inner, ctx.ir) {
        return generate_wrapped_doc(variant, &left_lit, &right_lit, ty, hints);
    }

    // Pattern 2: Key-value pair.
    if let Some((_key_name, sep_lit)) = detect_key_value_pattern_ir(inner, ctx.ir) {
        return generate_key_value_doc_ir(variant, &sep_lit, hints, ctx.ir);
    }

    // Box<Enum> or &'a ArenaEnum -- deref and recurse.
    if is_recursive_enum_type(ty) {
        let doc = quote! { val.to_doc() };
        let doc = apply_hints(doc, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    // Option -- unwrap.
    if is_option_type(ty) {
        let inner_doc = quote! { inner.to_doc() };
        let base = quote! {
            match val {
                Some(inner) => #inner_doc,
                None => ::pprint::Doc::Null,
            }
        };
        let doc = apply_hints(base, hints);
        return quote! {
            Self::#variant(val) => { #doc }
        };
    }

    // Tuple type -- concatenation.
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
            let custom_sep = hints.iter().find_map(|h| extract_sep_string(h));

            let sep: Option<TokenStream> = if let Some(sep_str) = custom_sep {
                let has_group = hints.contains(&"group".to_string());
                let has_hardbreak = hints.contains(&"hardbreak".to_string())
                    || hints.contains(&"block".to_string());
                if has_hardbreak {
                    // Non-filling: trimmed separator + hardline (one item per line).
                    let break_sep = sep_str.trim_end();
                    let break_lit = proc_macro2::Literal::string(break_sep);
                    Some(quote! {
                        ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                        + ::pprint::Doc::Hardline
                    })
                } else if has_group {
                    let sep_lit = proc_macro2::Literal::string(sep_str);
                    let break_sep = sep_str.trim_end();
                    let break_lit = proc_macro2::Literal::string(break_sep);
                    Some(quote! {
                        ::pprint::Doc::IfBreak(
                            Box::new(
                                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#break_lit))
                                + ::pprint::Doc::Hardline
                            ),
                            Box::new(
                                ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit))
                            ),
                        )
                    })
                } else {
                    let sep_lit = proc_macro2::Literal::string(sep_str);
                    Some(quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(#sep_lit)) })
                }
            } else if hints.contains(&"fast".to_string())
                || hints.contains(&"hardbreak".to_string())
                || hints.contains(&"block".to_string())
            {
                Some(quote! { ::pprint::Doc::Hardline })
            } else if hints.contains(&"blankline".to_string()) {
                Some(quote! { ::pprint::Doc::Hardline + ::pprint::Doc::Hardline })
            } else if hints.contains(&"nobreak".to_string()) {
                Some(quote! { ::pprint::Doc::String(::std::borrow::Cow::Borrowed(" ")) })
            } else if hints.contains(&"compact".to_string()) {
                // No separator -- matches VM's Doc::Concat behavior for compact.
                None
            } else if hints.contains(&"softbreak".to_string()) {
                Some(quote! { ::pprint::Doc::Softline })
            } else if hints.contains(&"off".to_string()) {
                None // Null -- skip interleaving entirely.
            } else {
                // No hints: raw concatenation -- no separator needed.
                None
            };

            if let Some(sep) = sep {
                // Non-Null separator -- interleave parts with separator.
                let mut interleaved: Vec<proc_macro2::TokenStream> = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        interleaved.push(sep.clone());
                    }
                    interleaved.push(part.clone());
                }
                quote! { ::pprint::Doc::Concat(vec![#(#interleaved),*]) }
            } else {
                // No separator -- direct `+` chaining (no Null interleaving).
                let mut acc = parts[0].clone();
                for part in &parts[1..] {
                    acc = quote! { (#acc) + (#part) };
                }
                acc
            }
        };

        let doc = apply_hints(combined, hints);
        return quote! {
            Self::#variant(#pattern) => { #doc }
        };
    }

    quote! {
        Self::#variant(_) => {
            panic!(
                "No @pretty doc-generation strategy registered for enum variant `{}`",
                stringify!(#variant)
            )
        }
    }
}

/// Key-value doc generation from IR.
fn generate_key_value_doc_ir(
    variant: &syn::Ident,
    sep: &str,
    hints: &[String],
    ir: &GrammarIR,
) -> TokenStream {
    let sep_str = resolve_separator_literal_ir(sep, ir).unwrap_or_else(|| sep.to_string());
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

/// Generate to_doc and source_range arms for heterogeneous alternation sub-variants.
pub(crate) fn generate_sub_variant_arms(
    ctx: &IrCodegenCtx<'_>,
    to_doc_arms: &mut Vec<TokenStream>,
    source_range_arms: &mut Vec<TokenStream>,
) {
    let mut seen = std::collections::HashSet::new();

    for rule in &ctx.ir.rules {
        for sv in &rule.meta.sub_variants {
            let variant_name = ctx.ir.get_string(sv.variant_name);
            if !seen.insert(variant_name.to_string()) {
                continue;
            }
            let variant = format_ident!("{}", variant_name);
            let ty = type_desc_to_syn(&sv.ty, ctx);

            if let syn::Type::Tuple(tuple_ty) = &ty {
                let n = tuple_ty.elems.len();
                let bindings: Vec<_> = (0..n).map(|i| format_ident!("f{}", i)).collect();
                let pat = quote! { (#(#bindings),*) };

                let doc_parts: Vec<_> = tuple_ty
                    .elems
                    .iter()
                    .zip(bindings.iter())
                    .map(|(elem_ty, binding)| doc_for_binding(binding, elem_ty))
                    .collect();

                to_doc_arms.push(quote! {
                    Self::#variant(#pat) => {
                        ::pprint::Doc::Concat(vec![#(#doc_parts),*])
                    }
                });

                let range_parts: Vec<_> = tuple_ty
                    .elems
                    .iter()
                    .zip(bindings.iter())
                    .map(|(elem_ty, binding)| range_for_binding(binding, elem_ty))
                    .collect();

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

                source_range_arms.push(quote! {
                    Self::#variant(#pat) => {
                        let mut _min_s = usize::MAX;
                        let mut _max_e = 0usize;
                        let mut _found = false;
                        #(#fold_stmts)*
                        if _found { Some((_min_s, _max_e)) } else { None }
                    }
                });
            } else if type_is_span(&ty) {
                to_doc_arms.push(quote! {
                    Self::#variant(s) => ::pprint::Doc::String(::std::borrow::Cow::Borrowed(s.as_str())),
                });
                source_range_arms.push(quote! {
                    Self::#variant(s) => Some((s.start, s.end)),
                });
            } else if is_recursive_enum_type(&ty) {
                to_doc_arms.push(quote! {
                    Self::#variant(val) => val.to_doc(),
                });
                source_range_arms.push(quote! {
                    Self::#variant(val) => val.source_range(),
                });
            } else {
                to_doc_arms.push(quote! {
                    Self::#variant(_) => ::pprint::Doc::Null,
                });
                source_range_arms.push(quote! {
                    Self::#variant(_) => None,
                });
            }
        }
    }
}
