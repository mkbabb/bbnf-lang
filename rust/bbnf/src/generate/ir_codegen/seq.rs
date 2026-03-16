//! Seq (concatenation) emission.
//!
//! Handles Span compression, sp_method_rules override, and (T, Vec<T>) flattening.

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::ir_types::IrCodegenCtx;
use super::infer::infer_node_type;
use super::ir_node_to_tokens;

/// Emit a Seq (concatenation) expression.
///
/// Handles:
/// - Span compression (consecutive Span children → `.then_span()`)
/// - sp_method_rules override (Ref to span-eligible → `Self::rule_sp().into_parser()`)
/// - `(T, Vec<T>)` → `Vec<T>` flattening
pub fn emit_seq(children: &[IrNode], ctx: &IrCodegenCtx<'_>) -> TokenStream {
    if children.is_empty() {
        return quote! { ::parse_that::epsilon() };
    }
    if children.len() == 1 {
        return ir_node_to_tokens(&children[0], ctx);
    }

    // Compute per-child types and determine sp_method_rules overrides.
    let child_info: Vec<(TokenStream, bool)> = children
        .iter()
        .map(|c| {
            // Check for sp_method_rules override.
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                let name = ctx.ir.get_string(rule.name);
                if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                    let sp_ident = format_ident!("{}_sp", name);
                    return (quote! { Self::#sp_ident().into_parser() }, true);
                }
            }
            (ir_node_to_tokens(c, ctx), false)
        })
        .collect();

    // Determine types for each child.
    let child_types: Vec<TypeDesc> = children
        .iter()
        .enumerate()
        .map(|(i, c)| {
            if child_info[i].1 {
                // sp_method_rules override → Span
                TypeDesc::Span
            } else {
                infer_node_type(c, ctx)
            }
        })
        .collect();

    // All-Span guard: if all children would be Span after override, don't use override.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    let (parsers, types): (Vec<TokenStream>, Vec<TypeDesc>) = if all_span {
        children
            .iter()
            .map(|c| (ir_node_to_tokens(c, ctx), infer_node_type(c, ctx)))
            .unzip()
    } else {
        child_info
            .into_iter()
            .zip(child_types.into_iter())
            .map(|((ts, _), ty)| (ts, ty))
            .unzip()
    };

    // Build chains: group consecutive Span children for then_span().
    let mut chains: Vec<(bool, Vec<TokenStream>)> = Vec::new();
    for (parser, ty) in parsers.iter().zip(types.iter()) {
        let is_span = *ty == TypeDesc::Span;
        if let Some((last_is_span, last_chain)) = chains.last_mut() {
            if is_span && *last_is_span {
                last_chain.push(parser.clone());
                continue;
            }
        }
        chains.push((is_span, vec![parser.clone()]));
    }

    // Fold chains into a single expression.
    let mut acc: Option<TokenStream> = None;
    for (n, (_, chain)) in chains.iter().enumerate() {
        let chain_acc = chain.iter().fold(None::<TokenStream>, |acc, parser| match acc {
            None => Some(parser.clone()),
            Some(acc) => Some(quote! { #acc.then_span(#parser) }),
        });
        acc = match acc {
            None => chain_acc,
            Some(prev) => {
                if n > 1 {
                    Some(quote! { #prev.then_flat(#chain_acc) })
                } else {
                    Some(quote! { #prev.then(#chain_acc) })
                }
            }
        };
    }
    let parser = acc.unwrap();

    // Flattening: (A, Vec<A>) → Vec<A> and (Vec<A>, A) → Vec<A>.
    // Compute effective types after Span compression.
    let effective_types = {
        let mut result: Vec<TypeDesc> = Vec::new();
        let mut in_span_run = false;
        for ty in &types {
            if *ty == TypeDesc::Span {
                if !in_span_run {
                    result.push(TypeDesc::Span);
                    in_span_run = true;
                }
            } else {
                result.push(ty.clone());
                in_span_run = false;
            }
        }
        result
    };

    if effective_types.len() == 2 {
        // (A, Vec<A>) → prepend first element
        if let TypeDesc::Vec(inner) = &effective_types[1] {
            if **inner == effective_types[0] {
                return quote! {
                    #parser.map(|(first, rest)| {
                        let mut v = Vec::with_capacity(1 + rest.len());
                        v.push(first);
                        v.extend(rest);
                        v
                    })
                };
            }
        }
        // (Vec<A>, A) → append last element
        if let TypeDesc::Vec(inner) = &effective_types[0] {
            if **inner == effective_types[1] {
                return quote! {
                    #parser.map(|(mut v, last)| {
                        v.push(last);
                        v
                    })
                };
            }
        }
    }

    parser
}
