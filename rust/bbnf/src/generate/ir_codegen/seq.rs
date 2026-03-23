//! Seq (concatenation) emission.
//!
//! Handles Span compression, sp_method_rules override, and (T, Vec<T>) flattening.
//! Both combinator mode (`emit_seq`) and inline mode (`emit_seq_inline`).

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::ir_types::IrCodegenCtx;
use super::infer::infer_node_type;
use super::inline::{ir_node_to_inline, ir_node_to_inline_vec, InlineCtx};
use super::{ir_node_to_tokens, ir_node_to_tokens_elide};

/// Emit a Seq (concatenation) expression.
///
/// Handles:
/// - Span compression (consecutive Span children → `.then_span()`)
/// - sp_method_rules override (Ref to span-eligible → `Self::rule_sp().into_parser()`)
/// - `(T, Vec<T>)` → `Vec<T>` flattening
pub fn emit_seq(children: &[IrNode], ctx: &IrCodegenCtx<'_>, elide_box: bool) -> TokenStream {
    if children.is_empty() {
        return quote! { ::parse_that::epsilon() };
    }
    if children.len() == 1 {
        return ir_node_to_tokens_elide(&children[0], ctx, elide_box);
    }

    // Multi-element seq produces a tuple, not a Vec element directly.
    // Reset elide_box for individual children.

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

    // All-Span guard: keep B.1 only when every child is a simple Span leaf
    // or a B.1-overridden Ref, and !no_collapse.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    let all_simple_span = all_span
        && ctx.ir.b1_span_collapse
        && !ctx.no_collapse.get()
        && children.iter().zip(child_types.iter()).all(|(c, ty)| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    return true;
                }
            }
            *ty == TypeDesc::Span
        });
    let (parsers, types): (Vec<TokenStream>, Vec<TypeDesc>) = if all_span && !all_simple_span {
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
    // When no_collapse is set and all types are Span, don't group — preserve the tuple.
    // Consume after first use (only top-level Seq of the rule preserves tuples).
    let no_collapse = ctx.no_collapse.get() && types.iter().all(|t| *t == TypeDesc::Span);
    // When no_collapse, clear it for nested emit_seq calls (only top-level preserves).
    if no_collapse {
        ctx.no_collapse.set(false);
    }
    let mut chains: Vec<(bool, Vec<TokenStream>)> = Vec::new();
    for (parser, ty) in parsers.iter().zip(types.iter()) {
        let is_span = *ty == TypeDesc::Span && !no_collapse;
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
        let chain_acc = chain
            .iter()
            .fold(None::<TokenStream>, |acc, parser| match acc {
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
        // (A, Vec<A>) → prepend first element (same-type only)
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
        // (Vec<A>, A) → append last element (same-type only)
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

/// Emit a Seq (concatenation) as inline code.
///
/// Produces sequential `let` bindings with `?` propagation inside the enclosing
/// `Parser::new(move |state| { ... })` closure. Handles Span compression by
/// recording start offsets and constructing combined Spans, and applies the same
/// sp_method override and (T, Vec<T>) flattening as `emit_seq`.
pub(super) fn emit_seq_inline(
    children: &[IrNode],
    ctx: &IrCodegenCtx<'_>,
    ictx: &mut InlineCtx,
    elide_box: bool,
) -> TokenStream {
    if children.is_empty() {
        return quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) };
    }
    if children.len() == 1 {
        return ir_node_to_inline_vec(&children[0], ctx, ictx, elide_box);
    }
    // Multi-element seq produces a tuple — reset elide_box for children.

    // ── Step 1: Determine per-child types and sp_method overrides ────────

    let mut child_types: Vec<TypeDesc> = Vec::new();
    let mut sp_override: Vec<bool> = Vec::new();

    for child in children {
        if let IrNode::Ref(id) = child {
            let rule = &ctx.ir.rules[*id as usize];
            let name = ctx.ir.get_string(rule.name);
            if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                child_types.push(TypeDesc::Span);
                sp_override.push(true);
                continue;
            }
        }
        child_types.push(infer_node_type(child, ctx));
        sp_override.push(false);
    }

    // All-Span guard: keep B.1 only when every child is a simple Span leaf
    // or a B.1-overridden Ref, and !no_collapse.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    let all_simple_span = all_span
        && ctx.ir.b1_span_collapse
        && !ctx.no_collapse.get()
        && children.iter().zip(child_types.iter()).all(|(c, ty)| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    return true;
                }
            }
            *ty == TypeDesc::Span
        });
    if all_span && !all_simple_span {
        child_types = children.iter().map(|c| infer_node_type(c, ctx)).collect();
        sp_override = vec![false; children.len()];
    }

    // If still all-Span, emit combined Span.
    // Exception: when no_collapse is set (@pretty/@no_collapse), keep the tuple.
    let still_all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    if still_all_span {
        if ctx.no_collapse.get() {
            // @pretty/@no_collapse: emit each child as Span, produce a tuple.
            // Clear no_collapse for nested Seqs (only top-level preserves tuples).
            ctx.no_collapse.set(false);
            let mut stmts: Vec<TokenStream> = Vec::new();
            let mut vars: Vec<TokenStream> = Vec::new();
            for child in children {
                let child_expr = ir_node_to_inline(child, ctx, ictx);
                let var = ictx.fresh_ident("sp");
                stmts.push(quote! { let #var = #child_expr?; });
                vars.push(quote! { #var });
            }
            return quote! {
                {
                    #(#stmts)*
                    Some((#(#vars),*))
                }
            };
        } else {
            // All-Span: emit combined SpanParser chain via combinator path.
            let parser = emit_seq(children, ctx, false);
            let name = ictx.hoist(parser);
            return quote! { #name.call(state) };
        }
    }

    // ── Step 2: Group consecutive Span children (span compression) ───────

    struct Group {
        is_span: bool,
        indices: Vec<usize>,
    }
    let mut groups: Vec<Group> = Vec::new();
    for (i, ty) in child_types.iter().enumerate() {
        let is_span = *ty == TypeDesc::Span;
        if let Some(last) = groups.last_mut() {
            if is_span && last.is_span {
                last.indices.push(i);
                continue;
            }
        }
        groups.push(Group {
            is_span,
            indices: vec![i],
        });
    }

    // ── Step 3: Emit inline code for each group ──────────────────────────

    let mut stmts: Vec<TokenStream> = Vec::new();
    let mut result_vars: Vec<TokenStream> = Vec::new();
    let mut effective_types: Vec<TypeDesc> = Vec::new();

    for group in &groups {
        if group.is_span {
            // Consecutive Span group: record start offset, call each child,
            // construct a combined Span from start to current offset.
            let start_var = ictx.fresh_ident("sp_start");
            stmts.push(quote! { let #start_var = state.offset; });

            for &idx in &group.indices {
                let child = &children[idx];
                if sp_override[idx] {
                    // Use SpanParser _sp method directly (no Box, no into_parser).
                    if let IrNode::Ref(id) = child {
                        let rule = &ctx.ir.rules[*id as usize];
                        let name = ctx.ir.get_string(rule.name);
                        let sp_ident = format_ident!("{}_sp", name);
                        let h = ictx.hoist(quote! { Self::#sp_ident() });
                        stmts.push(quote! { #h.call(state)?; });
                    }
                } else {
                    // Inline the child (produces Span — discard value, keep offset).
                    let child_expr = ir_node_to_inline(child, ctx, ictx);
                    stmts.push(quote! { #child_expr?; });
                }
            }

            let result_var = ictx.fresh_ident("span");
            stmts.push(quote! {
                let #result_var = ::parse_that::Span::new(#start_var, state.offset, state.src);
            });
            result_vars.push(quote! { #result_var });
            effective_types.push(TypeDesc::Span);
        } else {
            // Non-Span group: single element — capture value in a let binding.
            debug_assert_eq!(group.indices.len(), 1);
            let idx = group.indices[0];
            let child = &children[idx];

            let child_expr = ir_node_to_inline(child, ctx, ictx);
            let result_var = ictx.fresh_ident("val");
            stmts.push(quote! { let #result_var = #child_expr?; });
            result_vars.push(quote! { #result_var });
            effective_types.push(child_types[idx].clone());
        }
    }

    // ── Step 4: (T, Vec<T>) / (Vec<T>, T) flattening ────────────────────

    if effective_types.len() == 2 {
        // (A, Vec<A>) → Vec<A>: prepend first element (same-type only).
        if let TypeDesc::Vec(inner) = &effective_types[1] {
            if **inner == effective_types[0] {
                let first = &result_vars[0];
                let rest = &result_vars[1];
                return quote! {
                    {
                        #(#stmts)*
                        let mut __v = Vec::with_capacity(1 + #rest.len());
                        __v.push(#first);
                        __v.extend(#rest);
                        Some(__v)
                    }
                };
            }
        }
        // (Vec<A>, A) → Vec<A>: append last element (same-type only).
        if let TypeDesc::Vec(inner) = &effective_types[0] {
            if **inner == effective_types[1] {
                let vec_var = &result_vars[0];
                let last = &result_vars[1];
                return quote! {
                    {
                        #(#stmts)*
                        let mut __v = #vec_var;
                        __v.push(#last);
                        Some(__v)
                    }
                };
            }
        }
    }

    // ── Step 5: Construct result ─────────────────────────────────────────

    if result_vars.len() == 1 {
        let var = &result_vars[0];
        quote! {
            {
                #(#stmts)*
                Some(#var)
            }
        }
    } else {
        quote! {
            {
                #(#stmts)*
                Some((#(#result_vars),*))
            }
        }
    }
}
