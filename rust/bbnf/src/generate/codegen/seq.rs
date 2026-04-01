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
/// Follows the same 4-step process as the combinator `emit_seq_inline`:
/// 1. Classify children with B.1 sp_method override
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

    // ── Step 1: Get per-child types from InferMap (authoritative) ──────────
    //
    // The InferMap records the per-child effective types after B.1 override +
    // all-Span guard, as computed by infer_seq during the IR pass. This
    // guarantees exact agreement between the types used here and the types
    // stored in ir.types (which determine enum variant types).
    let child_types: Vec<TypeDesc> = ctx
        .infer_seq_child_types(children)
        .unwrap_or_else(|| {
            // Fallback for nodes not in InferMap (shouldn't happen after
            // comprehensive recording, but handles edge cases gracefully).
            children.iter().map(|c| ctx.infer_node_type(c)).collect()
        });

    // Determine sp_override from the child types: if a Ref child was given
    // type Span by B.1, it's sp-overridden.
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
        // B.1-overridden Refs use _sp() (no enum wrapping, no arena alloc).
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

    let skip_span_grouping = false;

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

    let mut stmts: Vec<TokenStream> = Vec::new();
    let mut result_vars: Vec<syn::Ident> = Vec::new();
    let mut effective_types: Vec<TypeDesc> = Vec::new();

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
                effective_types.push(TypeDesc::Span);
            } else {
                // Single Span child.
                let idx = group.indices[0];
                let child = &children[idx];
                let expr = emit_span_child(child, sp_override[idx], ctx, mctx);
                let var = mctx.fresh("v");
                stmts.push(quote! { let #var = #expr?; });
                result_vars.push(var);
                effective_types.push(TypeDesc::Span);
            }
        } else {
            // Non-Span group: single element (non-Span children don't merge).
            debug_assert_eq!(group.indices.len(), 1);
            let idx = group.indices[0];
            let child = &children[idx];
            // Non-Span children in Seq use standard (non-elide) inference,
            // so Refs produce BoxedEnum. Match by emitting with elide_box=false.
            let expr = emit_mono_expr(child, ctx, mctx, false);
            let var = mctx.fresh("v");
            stmts.push(quote! { let #var = #expr?; });
            result_vars.push(var);
            effective_types.push(child_types[idx].clone());
        }
    }

    // ── Step 4: (T, Vec<T>) / (Vec<T>, T) flattening ────────────────────
    //
    // Use ir.types (authoritative) to determine if the current rule's Seq was
    // flattened to Vec. This avoids relying on effective_types child matching
    // which can disagree with ir.types for prettify grammars due to
    // pretty_preserve affecting Span compression.
    let rule_vec_inner = ctx.current_rule_vec_inner(mctx.current_rule_id);

    if let Some(vec_inner) = rule_vec_inner.filter(|_| !ctx.parser_attrs.prettify) {
        // ir.types says this rule produces Vec(inner). The Seq was flattened.
        // Determine which child is the "head" (T) and which is the "rest" (Vec<T>).
        if effective_types.len() == 2 {
            let scratch_inner = vec_inner;
            if matches!(&effective_types[1], TypeDesc::Vec(_)) {
                // (T, Vec<T>) → Vec<T): first is head, second is rest.
                let first = &result_vars[0];
                let rest = &result_vars[1];
                let depth_var = quote::format_ident!("__flat_depth");
                let init = ctx.emit_scratch_init(scratch_inner, &depth_var);
                let push_first = ctx.emit_scratch_push(scratch_inner, &quote! { #first });
                let extend = ctx.emit_scratch_extend_slice(scratch_inner, &quote! { #rest });
                let collect = ctx.emit_scratch_collect(scratch_inner, &depth_var);
                return quote! {
                    {
                        #(#stmts)*
                        #init
                        #push_first;
                        #extend;
                        Some(#collect)
                    }
                };
            } else if matches!(&effective_types[0], TypeDesc::Vec(_)) {
                // (Vec<T>, T) → Vec<T>: first is rest, second is last.
                let vec_var = &result_vars[0];
                let last = &result_vars[1];
                let depth_var = quote::format_ident!("__flat_depth");
                let init = ctx.emit_scratch_init(scratch_inner, &depth_var);
                let extend = ctx.emit_scratch_extend_slice(scratch_inner, &quote! { #vec_var });
                let push_last = ctx.emit_scratch_push(scratch_inner, &quote! { #last });
                let collect = ctx.emit_scratch_collect(scratch_inner, &depth_var);
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
    } else if effective_types.len() == 2 && !ctx.parser_attrs.prettify {
        // Non-Vec rule: check effective_types for local flattening.
        if let TypeDesc::Vec(inner) = &effective_types[1] {
            if **inner == effective_types[0] {
                let first = &result_vars[0];
                let rest = &result_vars[1];
                let scratch_inner = inner.as_ref();
                let depth_var = quote::format_ident!("__flat_depth");
                let init = ctx.emit_scratch_init(scratch_inner, &depth_var);
                let push_first = ctx.emit_scratch_push(scratch_inner, &quote! { #first });
                let extend = ctx.emit_scratch_extend_slice(scratch_inner, &quote! { #rest });
                let collect = ctx.emit_scratch_collect(scratch_inner, &depth_var);
                return quote! {
                    {
                        #(#stmts)*
                        #init
                        #push_first;
                        #extend;
                        Some(#collect)
                    }
                };
            }
        }
        if let TypeDesc::Vec(inner) = &effective_types[0] {
            if **inner == effective_types[1] {
                let vec_var = &result_vars[0];
                let last = &result_vars[1];
                let scratch_inner = inner.as_ref();
                let depth_var = quote::format_ident!("__flat_depth");
                let init = ctx.emit_scratch_init(scratch_inner, &depth_var);
                let extend = ctx.emit_scratch_extend_slice(scratch_inner, &quote! { #vec_var });
                let push_last = ctx.emit_scratch_push(scratch_inner, &quote! { #last });
                let collect = ctx.emit_scratch_collect(scratch_inner, &depth_var);
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
    }

    // ── Assemble result ──────────────────────────────────────────────────

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
            // B.1 span override: must produce Span, not ArenaEnum.
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
