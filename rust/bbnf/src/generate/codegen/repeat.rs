//! Monolithic Repeat emission: quantifiers, optional, many.
//!
//! Sep_by variants are in the sibling `sep_by` module.

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::regex;
use super::helpers::try_sep_by;
use super::ir_types::IrCodegenCtx;
use super::loop_emit::{RestoringLoop, emit_restoring_loop};
use super::sep_by::emit_mono_sep_by;
use super::unescape_literal;
use super::{MonoCtx, emit_mono_expr, is_simple_expr, mono_fn_ident};

/// Emit a monolithic Repeat expression.
pub(super) fn emit_mono_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // sep_by detection.
    if !(lo == 0 && hi == 1) {
        if let Some((element, separator)) = try_sep_by(inner) {
            return emit_mono_sep_by(element, separator, lo, ctx, mctx);
        }
    }

    if lo == 0 && hi == 1 {
        emit_mono_optional(inner, ctx, mctx, elide_box)
    } else {
        emit_mono_many(inner, lo, ctx, mctx)
    }
}

/// Emit a monolithic Optional (Repeat 0..1).
fn emit_mono_optional(
    inner: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    let inner_ty = ctx.node_type(inner);

    // Ref nodes: skip Box in Optional context.
    if let IrNode::Ref(rule_id) = inner {
        let rule = &ctx.ir.rules[*rule_id as usize];
        let fn_ident = mono_fn_ident(ctx.resolve_rule_name(*rule_id));
        let cp_var = mctx.fresh("opt_cp");

        if rule.meta.is_transparent || elide_box {
            return quote! {
                {
                    let #cp_var = state.offset;
                    if let Some(__v) = Self::#fn_ident(state) {
                        Some(Some(__v))
                    } else {
                        state.offset = #cp_var;
                        Some(None)
                    }
                }
            };
        } else {
            let val_expr = quote! { __v };
            let alloc_expr = ctx.emit_alloc(&val_expr);
            return quote! {
                {
                    let #cp_var = state.offset;
                    if let Some(__v) = Self::#fn_ident(state) {
                        Some(Some(#alloc_expr))
                    } else {
                        state.offset = #cp_var;
                        Some(None)
                    }
                }
            };
        }
    }

    // Span case: emit inline for Literal/Regex, fall back to combinator otherwise.
    if inner_ty == TypeDesc::Span {
        // Optional single-byte literal: inline byte check, no SpanParser construction.
        if let IrNode::Literal(sid) = inner {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            if bytes.len() == 1 {
                let byte_lit = proc_macro2::Literal::byte_character(bytes[0]);
                return quote! {
                    {
                        let __start = state.offset;
                        if state.src_bytes.get(state.offset).copied() == Some(#byte_lit) {
                            state.offset += 1;
                        }
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                };
            }
            // Multi-byte optional literal: inline slice check.
            let len = bytes.len();
            let byte_lits: Vec<proc_macro2::Literal> = bytes
                .iter()
                .map(|b| proc_macro2::Literal::byte_character(*b))
                .collect();
            return quote! {
                {
                    let __start = state.offset;
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                        state.offset = __end;
                    }
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
            };
        }

        // Optional regex: emit inline via direct call if available.
        if let IrNode::Regex(sid) = inner {
            let pattern = ctx.ir.get_string(*sid);
            if let Some(direct) = regex::emit_regex_direct_call(pattern) {
                return quote! {
                    {
                        let __start = state.offset;
                        let _ = #direct;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                };
            }
        }

        // General Span optional: try to match, emit zero-width Span on failure.
        // Optional(Span) collapses to Span in the type system.
        let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
        let cp_var = mctx.fresh("opt_cp");
        let inner_call = if is_simple_expr(inner, mctx) {
            quote! { #inner_expr }
        } else {
            quote! { (|| #inner_expr)() }
        };
        return quote! {
            {
                let #cp_var = state.offset;
                if #inner_call.is_none() {
                    state.offset = #cp_var;
                }
                Some(::parse_that::Span::new(#cp_var, state.offset, state.src))
            }
        };
    }

    // General case: wrap in IIFE to scope `?` unless the expr is simple.
    let inner_expr = emit_mono_expr(inner, ctx, mctx, elide_box);
    let cp_var = mctx.fresh("opt_cp");
    // Phase 6: elide IIFE for simple expressions.
    let inner_call = if is_simple_expr(inner, mctx) {
        quote! { #inner_expr }
    } else {
        quote! { (|| #inner_expr)() }
    };
    quote! {
        {
            let #cp_var = state.offset;
            if let Some(__v) = #inner_call {
                Some(Some(__v))
            } else {
                state.offset = #cp_var;
                Some(None)
            }
        }
    }
}

/// Emit a monolithic many (Repeat 1+).
fn emit_mono_many(
    inner: &IrNode,
    lo: u32,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    // Repeat(Span) collapses to Span: loop consuming, produce combined Span.
    let inner_ty = ctx.node_type(inner);
    if inner_ty == TypeDesc::Span {
        let elem_expr = emit_mono_expr(inner, ctx, mctx, false);
        let start_var = mctx.fresh("sp_start");
        let prev_var = mctx.fresh("prev");
        let count_var = mctx.fresh("cnt");
        let elem_call = if is_simple_expr(inner, mctx) {
            quote! { #elem_expr }
        } else {
            quote! { (|| #elem_expr)() }
        };
        let lo_usize = lo as usize;
        let check = if lo == 0 {
            quote! { Some(::parse_that::Span::new(#start_var, state.offset, state.src)) }
        } else {
            quote! {
                if #count_var >= #lo_usize {
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                } else {
                    None
                }
            }
        };
        return quote! {
            {
                let #start_var = state.offset;
                let mut #count_var = 0usize;
                loop {
                    let #prev_var = state.offset;
                    if #elem_call.is_none() {
                        state.offset = #prev_var;
                        break;
                    }
                    #count_var += 1;
                    if state.offset == #prev_var { break; }
                }
                #check
            }
        };
    }

    let elem_ty = ctx.vec_elem_type(inner);
    let elem_expr = emit_mono_expr(inner, ctx, mctx, true);
    let lo_usize = lo as usize;
    let prev_var = mctx.fresh("prev");

    // Phase 6: elide IIFE for simple expressions (no `?` operator).
    let elem_call = if is_simple_expr(inner, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };

    // ── Scratch-based collection ────────────────────────────────────────────
    let depth_var = mctx.fresh("depth");
    let init_code = ctx.emit_scratch_init(&elem_ty, &depth_var);
    let push_code = ctx.emit_scratch_push(&elem_ty, &quote! { __value });
    let count_expr = ctx.emit_scratch_count(&elem_ty, &depth_var);
    let collect_expr = ctx.emit_scratch_collect(&elem_ty, &depth_var);
    let truncate_expr = ctx.emit_scratch_truncate(&elem_ty, &depth_var);

    let check = if lo == 0 {
        quote! { Some(#collect_expr) }
    } else {
        quote! {
            if #count_expr >= #lo_usize {
                Some(#collect_expr)
            } else {
                #truncate_expr
                None
            }
        }
    };

    emit_restoring_loop(RestoringLoop {
        init: init_code,
        prev_var: &prev_var,
        step: quote! { #elem_call },
        on_success: quote! {
            #push_code;
            if state.offset == #prev_var { break; }
        },
        on_failure: quote! { break; },
        finish: check,
    })
}
