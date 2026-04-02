//! Monolithic Seq emission: sequential let bindings with span compression.
//!
//! Mirrors the inline seq pattern from codegen
//! but emits direct function calls for Refs instead of hoisted Parser objects.

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::ir_types::IrCodegenCtx;
use super::{MonoCtx, emit_mono_expr};

/// Emit a monolithic Seq — sequential let bindings with span compression.
///
/// 4-step process:
/// 1. Classify children with span-method override
/// 2. All-Span guard + still-all-Span handling
/// 3. Group consecutive Span children for compression
/// 4. (T, Vec<T>) flattening
pub(super) fn emit_mono_seq(
    children: &[IrNode],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    _elide_box: bool,
) -> TokenStream {
    if children.is_empty() {
        return quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) };
    }
    if children.len() == 1 {
        return emit_mono_expr(&children[0], ctx, mctx, _elide_box);
    }

    // ── Step 1: Get per-child types from TypeMap (authoritative) ──────────
    //
    // The TypeMap records the per-child effective types after span-method override +
    // all-Span guard, as computed by project_seq during the IR pass. This
    // guarantees exact agreement between the types used here and the types
    // stored in ir.types (which determine enum variant types).
    let child_types: Vec<TypeDesc> = ctx
        .seq_child_types(children)
        .unwrap_or_else(|| {
            children.iter().map(|c| ctx.node_type(c)).collect()
        });

    // Determine sp_override from the child types: if a Ref child was given
    // type Span by the span-method override, it's sp-overridden.
    let sp_override: Vec<bool> = children
        .iter()
        .zip(child_types.iter())
        .map(|(c, ty)| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                rule.meta.has_sp_method && !rule.meta.is_transparent && *ty == TypeDesc::Span
            } else {
                false
            }
        })
        .collect();

    // ── Step 2: Still-all-Span handling ───────────────────────────────────

    let still_all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    if still_all_span {
        // All-Span: monolithic span compression.
        // Span-method overridden Refs use _sp() (no enum wrapping, no arena alloc).
        let start_var = mctx.fresh("sp_start");
        let mut stmts: Vec<TokenStream> = Vec::new();
        stmts.push(quote! { let #start_var = state.offset; });
        for (idx, child) in children.iter().enumerate() {
            let expr = emit_span_child(child, sp_override[idx], ctx, mctx);
            stmts.push(quote! { #expr?; });
        }
        return quote! {
            {
                #(#stmts)*
                Some(::parse_that::Span::new(#start_var, state.offset, state.src))
            }
        };
    }

    // ── Step 3: Group consecutive Span children for compression ──────────
    //
    // For prettify grammars, skip grouping: each Span child keeps its own
    // identity so whitespace-only Spans (ws rules) can be individually
    // nullified by the Doc generator.  Merging them with adjacent non-ws
    // Spans (e.g., ";") produces mixed-content Spans that bypass the
    // whitespace check and break idempotency.

    struct Group {
        is_span: bool,
        indices: Vec<usize>,
    }
    let mut groups: Vec<Group> = Vec::new();

    let skip_span_grouping = ctx.seq_preserve_spans(children);

    for (i, ty) in child_types.iter().enumerate() {
        let is_span = *ty == TypeDesc::Span;
        if !skip_span_grouping {
            if let Some(last) = groups.last_mut() {
                if is_span && last.is_span {
                    last.indices.push(i);
                    continue;
                }
            }
        }
        groups.push(Group {
            is_span,
            indices: vec![i],
        });
    }

    // ── Step 3b: Emit bindings ───────────────────────────────────────────
    //
    // Produce only stmts + result_vars. Type decisions are deferred to Step 4
    // which reads the projected seq_result_type from the TypeMap.

    let mut stmts: Vec<TokenStream> = Vec::new();
    let mut result_vars: Vec<syn::Ident> = Vec::new();
    // Minimal per-group flag: true if this group's child_type is Vec.
    // Used solely for Vec flattening order detection in Step 4.
    let mut result_is_vec: Vec<bool> = Vec::new();

    for group in &groups {
        if group.is_span {
            if group.indices.len() > 1 {
                // Consecutive Span group: record start, parse all, combined Span.
                let start_var = mctx.fresh("sp_start");
                stmts.push(quote! { let #start_var = state.offset; });
                for &idx in &group.indices {
                    let child = &children[idx];
                    let expr = emit_span_child(child, sp_override[idx], ctx, mctx);
                    stmts.push(quote! { #expr?; });
                }
                let span_var = mctx.fresh("span");
                stmts.push(quote! {
                    let #span_var = ::parse_that::Span::new(#start_var, state.offset, state.src);
                });
                result_vars.push(span_var);
                result_is_vec.push(false);
            } else {
                // Single Span child.
                let idx = group.indices[0];
                let child = &children[idx];
                let expr = emit_span_child(child, sp_override[idx], ctx, mctx);
                let var = mctx.fresh("v");
                stmts.push(quote! { let #var = #expr?; });
                result_vars.push(var);
                result_is_vec.push(false);
            }
        } else {
            // Non-Span group: single element (non-Span children don't merge).
            debug_assert_eq!(group.indices.len(), 1);
            let idx = group.indices[0];
            let child = &children[idx];
            // Non-Span children in Seq use standard (non-elide) projection,
            // so Refs produce BoxedEnum. Match by emitting with elide_box=false.
            let expr = emit_mono_expr(child, ctx, mctx, false);
            let var = mctx.fresh("v");
            stmts.push(quote! { let #var = #expr?; });
            result_vars.push(var);
            result_is_vec.push(matches!(&child_types[idx], TypeDesc::Vec(_)));
        }
    }

    // ── Step 4: Assembly via projected seq_result_type ───────────────────
    //
    // The TypeMap's seq_result_type is the single source of truth for the
    // Seq's return type. This eliminates the prior approach of re-deriving
    // the type from codegen's Span grouping (which could disagree with the
    // projection pass's Span compression).
    let projected_vec_inner: Option<TypeDesc> =
        ctx.seq_result_type(children).and_then(|td| match td {
            TypeDesc::Vec(inner) => Some(inner.as_ref().clone()),
            _ => None,
        });

    if let Some(vec_inner) = projected_vec_inner {
        // Projection says this Seq produces Vec(inner) via (T, Vec<T>) flattening.
        debug_assert_eq!(
            result_vars.len(), 2,
            "Vec-flattened Seq must have exactly 2 groups after Span compression"
        );
        if result_is_vec[1] {
            // (T, Vec<T>) → Vec<T>: first is head, second is rest.
            let first = &result_vars[0];
            let rest = &result_vars[1];
            let depth_var = quote::format_ident!("__flat_depth");
            let init = ctx.emit_scratch_init(&vec_inner, &depth_var);
            let push_first = ctx.emit_scratch_push(&vec_inner, &quote! { #first });
            let extend = ctx.emit_scratch_extend_slice(&vec_inner, &quote! { #rest });
            let collect = ctx.emit_scratch_collect(&vec_inner, &depth_var);
            return quote! {
                {
                    #(#stmts)*
                    #init
                    #push_first;
                    #extend;
                    Some(#collect)
                }
            };
        } else {
            // (Vec<T>, T) → Vec<T>: first is rest, second is last.
            debug_assert!(result_is_vec[0], "one of the two groups must be Vec");
            let vec_var = &result_vars[0];
            let last = &result_vars[1];
            let depth_var = quote::format_ident!("__flat_depth");
            let init = ctx.emit_scratch_init(&vec_inner, &depth_var);
            let extend = ctx.emit_scratch_extend_slice(&vec_inner, &quote! { #vec_var });
            let push_last = ctx.emit_scratch_push(&vec_inner, &quote! { #last });
            let collect = ctx.emit_scratch_collect(&vec_inner, &depth_var);
            return quote! {
                {
                    #(#stmts)*
                    #init
                    #extend;
                    #push_last;
                    Some(#collect)
                }
            };
        }
    }

    // ── Assemble result ──────────────────────────────────────────────────
    //
    // For non-Vec results: single-element unwrap or tuple assembly.
    // The debug_assert verifies that Span grouping produces the same element
    // count as the projection's consecutive-Span compression.
    if let Some(projected) = ctx.seq_result_type(children) {
        if let TypeDesc::Tuple(elems) = projected {
            debug_assert_eq!(
                result_vars.len(),
                elems.len(),
                "Span grouping element count must match projected Tuple arity"
            );
        }
    }

    if result_vars.len() == 1 {
        let v = &result_vars[0];
        quote! {
            {
                #(#stmts)*
                Some(#v)
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

/// Emit a Span child in a Span group.
///
/// For fusion-eligible Refs, inline the body directly (returns Span without
/// SpanParser dispatch overhead). Otherwise, use the SpanParser `_sp()` path.
/// For non-Ref nodes, emit the monolithic expression.
fn emit_span_child(
    child: &IrNode,
    is_sp_override: bool,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if is_sp_override {
        if let IrNode::Ref(id) = child {
            // Span-method override: must produce Span, not Enum.
            // Use direct monolithic call + offset-delta Span construction.
            // Avoids SpanParser combinator overhead entirely.
            let fn_ident = super::mono_fn_ident(ctx.resolve_rule_name(*id));
            return quote! {
                {
                    let __sp_b1 = state.offset;
                    Self::#fn_ident(state).map(|_| {
                        ::parse_that::Span::new(__sp_b1, state.offset, state.src)
                    })
                }
            };
        }
    }
    emit_mono_expr(child, ctx, mctx, false)
}
