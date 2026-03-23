//! Monolithic Seq emission: sequential let bindings with span compression.
//!
//! Mirrors the combinator inline seq (`ir_codegen/seq.rs:emit_seq_inline`)
//! but emits direct function calls for Refs instead of hoisted Parser objects.

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::ir_types::IrCodegenCtx;
use super::super::infer::infer_node_type;
use super::{emit_mono_expr, MonoCtx};

/// Emit a monolithic Seq — sequential let bindings with span compression.
///
/// Follows the same 4-step process as the combinator `emit_seq_inline`:
/// 1. Classify children with B.1 sp_method override
/// 2. All-Span guard + still-all-Span / no_collapse handling
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

    // ── Step 1: Determine per-child types with B.1 sp_method override ────

    let mut child_types: Vec<TypeDesc> = Vec::new();
    let mut sp_override: Vec<bool> = Vec::new();

    for child in children {
        if let IrNode::Ref(id) = child {
            let rule = &ctx.ir.rules[*id as usize];
            if rule.meta.has_sp_method && !rule.meta.is_transparent {
                child_types.push(TypeDesc::Span);
                sp_override.push(true);
                continue;
            }
        }
        child_types.push(infer_node_type(child, ctx));
        sp_override.push(false);
    }

    // All-Span guard: keep B.1 only when every child is a simple Span leaf
    // or a B.1-overridden Ref, and !no_collapse. Complex children (Repeat,
    // Skip, etc.) that infer to Span through collapse may compress differently.
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

    // ── Step 2: Still-all-Span + no_collapse handling ────────────────────

    let still_all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    if still_all_span {
        if ctx.no_collapse.get() {
            // @pretty/@no_collapse: emit each child individually as Span tuple.
            // Consume the flag for nested Seqs (only top-level preserves tuples).
            ctx.no_collapse.set(false);
            let mut stmts: Vec<TokenStream> = Vec::new();
            let mut vars: Vec<syn::Ident> = Vec::new();
            for child in children {
                let expr = emit_mono_expr(child, ctx, mctx, false);
                let var = mctx.fresh("sp");
                stmts.push(quote! { let #var = #expr?; });
                vars.push(var);
            }
            return quote! {
                {
                    #(#stmts)*
                    Some((#(#vars),*))
                }
            };
        } else {
            // All-Span without no_collapse: monolithic span compression.
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
    }

    // ── Step 3: Group consecutive Span children for compression ──────────
    // (no_collapse does NOT affect this — only still-all-Span is gated above)

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

    if effective_types.len() == 2 {
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
            // Fusion: inline the rule body for direct Span result.
            if mctx.fusion_eligible.get(*id as usize).copied() == Some(true) {
                let rule = &ctx.ir.rules[*id as usize];
                let saved_no_collapse = ctx.no_collapse.get();
                ctx.no_collapse.set(false);
                let result = emit_mono_expr(&rule.body, ctx, mctx, false);
                ctx.no_collapse.set(saved_no_collapse);
                return result;
            }
            // Standard: use SpanParser _sp() path.
            let rule = &ctx.ir.rules[*id as usize];
            let name = ctx.ir.get_string(rule.name);
            let sp_ident = format_ident!("{}_sp", name);
            let hname = mctx.hoist(quote! { Self::#sp_ident() });
            return quote! { #hname.call(state) };
        }
    }
    emit_mono_expr(child, ctx, mctx, false)
}
