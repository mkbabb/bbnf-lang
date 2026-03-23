//! Monolithic Repeat emission: quantifiers, optional, many, sep_by, sep_by_ws.

use bbnf_ir::{IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::ir_types::IrCodegenCtx;
use super::super::infer::infer_node_type;
use super::super::repeat as combinator_repeat;
use super::super::unescape_literal;
use super::{
    emit_literal_inline_unchecked, emit_mono_expr, emit_mono_fallback, is_simple_expr,
    mono_fn_ident, MonoCtx,
};

// ── Unified sep_by configuration ─────────────────────────────────────────────

/// Configuration for the unified sep_by loop emitter.
///
/// Captures the 6 dimensions that vary across the three sep_by variants:
/// bare sep_by, ws-aware sep_by_ws, and delimited sep_by_ws_until.
pub(super) struct SepByConfig {
    /// Trim whitespace around elements and separator.
    pub ws: bool,
    /// Open delimiter expression (emitted before the loop with `?`).
    pub open_expr: Option<TokenStream>,
    /// Close delimiter expression (emitted after the loop with `?`).
    pub close_expr: Option<TokenStream>,
    /// Terminator bytes for early-exit check after separator + ws trim.
    pub terminator_bytes: Option<Vec<u8>>,
    /// Phase 11: unchecked separator expression for loop iterations
    /// (after successful element parse, offset < end is guaranteed in delimited contexts).
    pub unchecked_sep: Option<TokenStream>,
}

/// Try to extract an unchecked single-byte separator expression.
///
/// Returns `Some(TokenStream)` if the separator is a single-byte literal
/// (possibly wrapped in OW), suitable for `get_unchecked` in loop body.
pub(super) fn try_unchecked_sep(separator: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<TokenStream> {
    let check_literal = |sid: bbnf_ir::StringId| -> Option<TokenStream> {
        let raw = ctx.ir.get_string(sid);
        let unesc = unescape_literal(raw);
        let bytes = unesc.as_bytes();
        if bytes.len() == 1 {
            Some(emit_literal_inline_unchecked(bytes[0]))
        } else {
            None
        }
    };
    match separator {
        IrNode::Literal(sid) => check_literal(*sid),
        IrNode::OptionalWhitespace(inner) => {
            if let IrNode::Literal(sid) = inner.as_ref() {
                check_literal(*sid)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Unified sep_by loop emitter. All three sep_by variants delegate here.
pub(super) fn emit_mono_sep_by_core(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    config: &SepByConfig,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let elem_expr = emit_mono_expr(element, ctx, mctx, true);
    let lo_usize = lo as usize;
    let vals_var = mctx.fresh("vals");
    let cp_var = mctx.fresh("cp");

    // Phase 6: IIFE elision for simple expressions.
    let elem_call = if is_simple_expr(element, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };
    let first_call = elem_call.clone();

    // Separator: strip_ow when ws is handled explicitly.
    let sep_expr = super::emit_mono_discarded(separator, config.ws, ctx, mctx);
    let loop_sep = config.unchecked_sep.as_ref().unwrap_or(&sep_expr);

    // No pre-allocation: Rust's Vec grows 0→4→8→16→... which handles both
    // small containers (1-5 elements, 1 alloc) and large ones (amortized O(1)).
    // Pre-allocating caused pathological over-allocation for nested containers
    // (canada.json: 2-element arrays allocated 16K capacity each).
    let capacity_code = quote! { let mut #vals_var = Vec::new(); };

    // ── Whitespace fragments (use custom @ws pattern if configured) ──
    let ws_trim = super::emit_ws_trim(ctx, mctx);

    let pre_ws = if config.ws {
        ws_trim.clone()
    } else {
        quote! {}
    };

    // ── Open delimiter ──
    let open_code = if let Some(open) = &config.open_expr {
        quote! { #open?; }
    } else {
        quote! {}
    };

    // ── Post-separator: ws trim + optional terminator check ──
    let post_sep_in_loop = if config.ws {
        if let Some(ref term_bytes) = config.terminator_bytes {
            let term_check = if term_bytes.len() == 1 {
                let b = proc_macro2::Literal::byte_character(term_bytes[0]);
                quote! { __b == #b }
            } else {
                let byte_lits: Vec<proc_macro2::Literal> =
                    term_bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                quote! { [#(#byte_lits),*].contains(&__b) }
            };
            let ws = &ws_trim;
            quote! {
                #ws
                if let Some(&__b) = state.src_bytes.get(state.offset) {
                    if #term_check { break; }
                }
            }
        } else {
            ws_trim.clone()
        }
    } else {
        quote! {}
    };

    // ── Post-loop: ws trim placement differs for delimited vs non-delimited ──
    let final_check = if let Some(close) = &config.close_expr {
        // Delimited: ws trim before close delimiter, outside the len check.
        let ws = &ws_trim;
        quote! {
            #ws
            if #vals_var.len() >= #lo_usize {
                #close?;
                Some(#vals_var)
            } else {
                None
            }
        }
    } else if config.ws {
        // Non-delimited ws: trailing trim inside the success branch.
        let ws = &ws_trim;
        quote! {
            if #vals_var.len() >= #lo_usize {
                #ws
                Some(#vals_var)
            } else {
                None
            }
        }
    } else {
        // Bare: no ws.
        quote! {
            if #vals_var.len() >= #lo_usize {
                Some(#vals_var)
            } else {
                None
            }
        }
    };

    // Pre-separator ws trim in the loop body.
    // Phase 7: in delimited contexts (terminator_bytes set), ws before the
    // separator is skipped — the separator byte immediately follows the element.
    // In non-delimited contexts, ws before the separator IS needed.
    let pre_sep_ws = if config.ws && config.terminator_bytes.is_none() {
        ws_trim.clone()
    } else {
        quote! {}
    };

    quote! {
        {
            #open_code
            #capacity_code
            #pre_ws
            let __first = #first_call;
            if let Some(__value) = __first {
                #vals_var.push(__value);
                loop {
                    let #cp_var = state.offset;
                    #pre_sep_ws
                    if (#loop_sep).is_none() {
                        state.offset = #cp_var;
                        break;
                    }
                    #post_sep_in_loop
                    let __elem = #elem_call;
                    if let Some(__value) = __elem {
                        #vals_var.push(__value);
                    } else {
                        state.offset = #cp_var;
                        break;
                    }
                }
            }
            #final_check
        }
    }
}

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
        if let Some((element, separator)) = combinator_repeat::try_sep_by(inner) {
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
    let inner_ty = infer_node_type(inner, ctx);

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
            let helper = ctx.arena_helper_ident();
            return quote! {
                {
                    let #cp_var = state.offset;
                    if let Some(__v) = Self::#fn_ident(state) {
                        Some(Some(&*#helper(state).alloc(__v)))
                    } else {
                        state.offset = #cp_var;
                        Some(None)
                    }
                }
            };
        }
    }

    // Span case: use combinator opt/opt_span.
    if inner_ty == TypeDesc::Span {
        return emit_mono_fallback(
            &IrNode::Repeat {
                inner: Box::new(inner.clone()),
                lo: 0,
                hi: 1,
            },
            ctx,
            mctx,
            elide_box,
        );
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
    let elem_expr = emit_mono_expr(inner, ctx, mctx, true);
    let lo_usize = lo as usize;

    let vals_var = mctx.fresh("vals");
    let prev_var = mctx.fresh("prev");

    // Phase 6: elide IIFE for simple expressions (no `?` operator).
    let elem_call = if is_simple_expr(inner, mctx) {
        quote! { #elem_expr }
    } else {
        quote! { (|| #elem_expr)() }
    };

    quote! {
        {
            let mut #vals_var = Vec::new();
            loop {
                let #prev_var = state.offset;
                let __elem = #elem_call;
                match __elem {
                    Some(__value) => {
                        #vals_var.push(__value);
                        if state.offset == #prev_var { break; }
                    }
                    None => {
                        state.offset = #prev_var;
                        break;
                    }
                }
            }
            if #vals_var.len() >= #lo_usize {
                Some(#vals_var)
            } else {
                None
            }
        }
    }
}

/// Emit a monolithic sep_by (bare — no whitespace, no delimiters).
fn emit_mono_sep_by(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    emit_mono_sep_by_core(
        element,
        separator,
        lo,
        &SepByConfig {
            ws: false,
            open_expr: None,
            close_expr: None,
            terminator_bytes: None,
            unchecked_sep: None,
        },
        ctx,
        mctx,
    )
}

/// Emit a monolithic sep_by_ws (whitespace-aware, no delimiters).
pub(super) fn emit_mono_sep_by_ws(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    emit_mono_sep_by_core(
        element,
        separator,
        lo,
        &SepByConfig {
            ws: true,
            open_expr: None,
            close_expr: None,
            terminator_bytes: None,
            unchecked_sep: None,
        },
        ctx,
        mctx,
    )
}
