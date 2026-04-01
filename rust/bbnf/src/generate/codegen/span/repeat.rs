//! Span-only repetition emission: many, sep_by, optional.
//! No Vec collection — just track start/end offsets.

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::super::ir_types::IrCodegenCtx;
use super::{emit_span_discarded, emit_span_expr};

/// Emit a span-only Repeat (many).
pub(super) fn emit_span_repeat(
    inner: &IrNode,
    lo: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if let Some((element, separator)) = super::super::helpers::try_sep_by(inner) {
        return emit_span_sep_by(element, separator, lo, ir, ctx, mctx);
    }

    let elem_expr = emit_span_expr(inner, ir, ctx, mctx);
    let lo_usize = lo as usize;
    let start_var = mctx.fresh("rep_start");
    let prev_var = mctx.fresh("rep_prev");
    let count_var = mctx.fresh("rep_count");

    let elem_call = if super::super::is_simple_expr(inner, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };

    quote! {
        {
            let #start_var = state.offset;
            let mut #count_var: usize = 0;
            loop {
                let #prev_var = state.offset;
                let __elem = #elem_call;
                match __elem {
                    Some(_) => {
                        #count_var += 1;
                        if state.offset == #prev_var { break; }
                    }
                    None => {
                        state.offset = #prev_var;
                        break;
                    }
                }
            }
            if #count_var >= #lo_usize {
                Some(::parse_that::Span::new(#start_var, state.offset, state.src))
            } else {
                None
            }
        }
    }
}

/// Span-only sep_by: parse (element, separator)* without Vec.
fn emit_span_sep_by(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let elem_expr = emit_span_expr(element, ir, ctx, mctx);
    let sep_expr = emit_span_discarded(separator, ir, ctx, mctx);
    let lo_usize = lo as usize;
    let start_var = mctx.fresh("sb_start");
    let cp_var = mctx.fresh("sb_cp");
    let count_var = mctx.fresh("sb_count");

    let elem_call = if super::super::is_simple_expr(element, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };
    let first_call = elem_call.clone();

    quote! {
        {
            let #start_var = state.offset;
            let mut #count_var: usize = 0;
            let __first = #first_call;
            if let Some(_) = __first {
                #count_var += 1;
                loop {
                    let #cp_var = state.offset;
                    if (#sep_expr).is_none() {
                        state.offset = #cp_var;
                        break;
                    }
                    let __elem = #elem_call;
                    if let Some(_) = __elem {
                        #count_var += 1;
                    } else {
                        state.offset = #cp_var;
                        break;
                    }
                }
            }
            if #count_var >= #lo_usize {
                Some(::parse_that::Span::new(#start_var, state.offset, state.src))
            } else {
                None
            }
        }
    }
}

/// Span-only Optional (Repeat 0..1).
pub(super) fn emit_span_optional(
    inner: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let start_var = mctx.fresh("opt_start");

    // Inline optional single-byte literal: direct byte check, no IIFE.
    if let IrNode::Literal(sid) = inner {
        let raw = ir.get_string(*sid);
        let unescaped = super::super::unescape_literal(raw);
        let bytes = unescaped.as_bytes();
        if bytes.len() == 1 {
            let byte_lit = proc_macro2::Literal::byte_character(bytes[0]);
            return quote! {
                {
                    let #start_var = state.offset;
                    if state.src_bytes.get(state.offset).copied() == Some(#byte_lit) {
                        state.offset += 1;
                    }
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                }
            };
        }
    }

    // Inline optional regex with direct fast-path call or regex_emit.
    if let IrNode::Regex(sid) = inner {
        let pattern = ir.get_string(*sid);
        // 1. Try known fast paths.
        if let Some(direct) =
            super::super::super::regex_ir::fast_paths::emit_regex_direct_call(pattern)
        {
            return quote! {
                {
                    let #start_var = state.offset;
                    let _ = #direct;
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                }
            };
        }
        // 2. Try HIR-based inline compilation.
        if let Some(inline) = super::super::super::regex_ir::try_emit_regex_inline(pattern) {
            return quote! {
                {
                    let #start_var = state.offset;
                    let _ = #inline;
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                }
            };
        }
        // 3. Try DFA-based inline compilation.
        if let Some(dfa_code) = super::super::super::regex_ir::try_emit_dfa_inline(pattern) {
            return quote! {
                {
                    let #start_var = state.offset;
                    let _ = #dfa_code;
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                }
            };
        }
        // 4. Unsupported pattern — compile-time error.
        let err = super::super::super::regex_ir::emit_regex_unsupported(pattern);
        return quote! {
            {
                let #start_var = state.offset;
                let _ = #err;
                Some(::parse_that::Span::new(#start_var, state.offset, state.src))
            }
        };
    }

    // General case: IIFE wrapper.
    let inner_expr = emit_span_expr(inner, ir, ctx, mctx);
    quote! {
        {
            let #start_var = state.offset;
            let _ = (|| #inner_expr)();
            Some(::parse_that::Span::new(#start_var, state.offset, state.src))
        }
    }
}
