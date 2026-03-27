//! Span-only expression helpers: Ref, Seq, Skip/Next/Wrap, OW, sep_by_ws variants.

use bbnf_ir::{IrNode, GrammarIR};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::super::ir_types::IrCodegenCtx;
use super::super::MonoCtx;
use super::{span_fn_ident, emit_span_expr, emit_span_discarded, emit_ws_trim};

// ── Ref ──────────────────────────────────────────────────────────────────────

pub(super) fn emit_span_ref(
    rule_id: bbnf_ir::RuleId,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let can_inline = mctx.fusion_eligible.get(rule_id as usize).copied() == Some(true)
        || mctx.single_site_inline.get(rule_id as usize).copied() == Some(true);
    if can_inline {
        let rule = &ir.rules[rule_id as usize];
        return emit_span_expr(&rule.body, ir, ctx, mctx);
    }
    let rule = &ir.rules[rule_id as usize];
    let name = ir.get_string(rule.name);
    let fn_ident = span_fn_ident(name);
    quote! { Self::#fn_ident(state) }
}

// ── Seq ──────────────────────────────────────────────────────────────────────

pub(super) fn emit_span_seq(
    children: &[IrNode],
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if children.is_empty() {
        return quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) };
    }
    if children.len() == 1 {
        return emit_span_expr(&children[0], ir, ctx, mctx);
    }
    let start_var = mctx.fresh("seq_start");
    let mut stmts: Vec<TokenStream> = Vec::new();
    stmts.push(quote! { let #start_var = state.offset; });
    for child in children {
        let expr = emit_span_expr(child, ir, ctx, mctx);
        stmts.push(quote! { #expr?; });
    }
    quote! {
        {
            #(#stmts)*
            Some(::parse_that::Span::new(#start_var, state.offset, state.src))
        }
    }
}

// ── Skip / Next ──────────────────────────────────────────────────────────────

pub(super) fn emit_span_skip(
    left: &IrNode,
    right: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if let IrNode::Next(open, middle) = left {
        return emit_span_wrap(open, middle, right, ir, ctx, mctx);
    }
    let left_expr = emit_span_expr(left, ir, ctx, mctx);
    let right_expr = emit_span_discarded(right, ir, ctx, mctx);
    let var = mctx.fresh("skip");
    quote! {
        {
            let #var = #left_expr?;
            #right_expr?;
            Some(#var)
        }
    }
}

pub(super) fn emit_span_next(
    left: &IrNode,
    right: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if let IrNode::Skip(middle, close) = right {
        return emit_span_wrap(left, middle, close, ir, ctx, mctx);
    }
    let left_expr = emit_span_discarded(left, ir, ctx, mctx);
    let right_expr = emit_span_expr(right, ir, ctx, mctx);
    quote! {
        {
            #left_expr?;
            #right_expr
        }
    }
}

// ── Minus / Negate ───────────────────────────────────────────────────────────

pub(super) fn emit_span_minus(
    main: &IrNode,
    excluded: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let main_expr = emit_span_expr(main, ir, ctx, mctx);
    let excl_expr = emit_span_expr(excluded, ir, ctx, mctx);
    let cp = mctx.fresh("minus_cp");
    quote! {
        {
            let #cp = state.offset;
            if (#excl_expr).is_some() { state.offset = #cp; None }
            else { state.offset = #cp; #main_expr }
        }
    }
}

pub(super) fn emit_span_negate(
    inner: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let inner_expr = emit_span_expr(inner, ir, ctx, mctx);
    let cp = mctx.fresh("neg_cp");
    quote! {
        {
            let #cp = state.offset;
            let __r = #inner_expr;
            state.offset = #cp;
            if __r.is_some() { None }
            else { Some(::parse_that::Span::new(#cp, #cp, state.src)) }
        }
    }
}

// ── Wrap (open >> middle << close) ───────────────────────────────────────────

pub(super) fn emit_span_wrap(
    open: &IrNode,
    middle: &IrNode,
    close: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    // sep_by_ws_until optimization.
    if let IrNode::Literal(_) = close {
        if let IrNode::OptionalWhitespace(ow_inner) = middle {
            if let IrNode::Repeat { inner: rep_inner, lo, hi } = ow_inner.as_ref() {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = super::super::super::repeat::try_sep_by(rep_inner) {
                        let open_expr = emit_span_discarded(open, ir, ctx, mctx);
                        let close_expr = emit_span_discarded(close, ir, ctx, mctx);
                        return emit_span_sep_by_ws_until(
                            element, separator, *lo, &open_expr, &close_expr, ir, ctx, mctx,
                        );
                    }
                }
            }
        }
    }

    // Delimiter-scan optimization.
    {
        let rule_name = mctx.current_rule_name.clone();
        if let Some(ts) = super::super::delim_scan::try_emit_span_wrap(
            open, middle, close, rule_name.as_deref(), ir, ctx, mctx,
        ) {
            return ts;
        }
    }

    // General wrap.
    let start_var = mctx.fresh("wrap_start");
    let open_expr = emit_span_discarded(open, ir, ctx, mctx);
    let middle_expr = emit_span_expr(middle, ir, ctx, mctx);
    let close_expr = emit_span_discarded(close, ir, ctx, mctx);

    quote! {
        {
            let #start_var = state.offset;
            #open_expr?;
            #middle_expr?;
            #close_expr?;
            Some(::parse_that::Span::new(#start_var, state.offset, state.src))
        }
    }
}

fn emit_span_sep_by_ws_until(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    open_expr: &TokenStream,
    close_expr: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let elem_expr = emit_span_expr(element, ir, ctx, mctx);
    let sep_expr = emit_span_discarded(separator, ir, ctx, mctx);
    let ws_trim = emit_ws_trim(ctx, mctx);
    let lo_usize = lo as usize;
    let start_var = mctx.fresh("sbwu_start");
    let cp_var = mctx.fresh("sbwu_cp");
    let count_var = mctx.fresh("sbwu_count");

    let elem_call = if super::super::is_simple_expr(element, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };
    let first_call = elem_call.clone();
    let ws2 = ws_trim.clone();
    let ws3 = ws_trim.clone();

    quote! {
        {
            let #start_var = state.offset;
            #open_expr?;
            let mut #count_var: usize = 0;
            #ws_trim
            let __first = #first_call;
            if let Some(_) = __first {
                #count_var += 1;
                loop {
                    let #cp_var = state.offset;
                    if (#sep_expr).is_none() {
                        state.offset = #cp_var;
                        break;
                    }
                    #ws2
                    let __elem = #elem_call;
                    if let Some(_) = __elem {
                        #count_var += 1;
                    } else {
                        state.offset = #cp_var;
                        break;
                    }
                }
            }
            #ws3
            if #count_var >= #lo_usize {
                #close_expr?;
                Some(::parse_that::Span::new(#start_var, state.offset, state.src))
            } else {
                None
            }
        }
    }
}

// ── OptionalWhitespace ───────────────────────────────────────────────────────

pub(super) fn emit_span_ow(
    inner: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    // sep_by_ws detection.
    if let IrNode::Repeat { inner: rep_inner, lo, hi } = inner {
        if !(*lo == 0 && *hi == 1) {
            if let Some((element, separator)) = super::super::super::repeat::try_sep_by(rep_inner) {
                return emit_span_sep_by_ws(element, separator, *lo, ir, ctx, mctx);
            }
        }
    }

    let ws_trim = emit_ws_trim(ctx, mctx);
    let inner_expr = emit_span_expr(inner, ir, ctx, mctx);
    let start_var = mctx.fresh("ow_start");

    // Loop invariant hoisting: if the inner expression already ends with a
    // whitespace trim, skip the redundant trailing trim.
    if super::super::expr::ends_with_ow(inner) {
        quote! {
            {
                let #start_var = state.offset;
                #ws_trim
                let __r = #inner_expr;
                if __r.is_some() {
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                } else {
                    __r
                }
            }
        }
    } else {
        let ws2 = ws_trim.clone();
        quote! {
            {
                let #start_var = state.offset;
                #ws_trim
                let __r = #inner_expr;
                if __r.is_some() { #ws2 }
                if __r.is_some() {
                    Some(::parse_that::Span::new(#start_var, state.offset, state.src))
                } else {
                    __r
                }
            }
        }
    }
}

fn emit_span_sep_by_ws(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let elem_expr = emit_span_expr(element, ir, ctx, mctx);
    let sep_expr = emit_span_discarded(separator, ir, ctx, mctx);
    let ws_trim = emit_ws_trim(ctx, mctx);
    let lo_usize = lo as usize;
    let start_var = mctx.fresh("sbws_start");
    let cp_var = mctx.fresh("sbws_cp");
    let count_var = mctx.fresh("sbws_count");

    let elem_call = if super::super::is_simple_expr(element, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };
    let first_call = elem_call.clone();
    let ws2 = ws_trim.clone();
    let ws3 = ws_trim.clone();

    quote! {
        {
            let #start_var = state.offset;
            let mut #count_var: usize = 0;
            #ws_trim
            let __first = #first_call;
            if let Some(_) = __first {
                #count_var += 1;
                loop {
                    let #cp_var = state.offset;
                    #ws2
                    if (#sep_expr).is_none() {
                        state.offset = #cp_var;
                        break;
                    }
                    #ws3
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
