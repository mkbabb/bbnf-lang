//! Unified sep_by loop emission: bare sep_by, ws-aware sep_by_ws, delimited sep_by_ws_until.
//!
//! Extracted from `repeat.rs` — these three sep_by variants share a common core
//! (`emit_mono_sep_by_core`) parameterized by `SepByConfig`.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::ir_types::IrCodegenCtx;
use super::unescape_literal;
use super::{MonoCtx, emit_literal_inline_unchecked, emit_mono_expr, is_simple_expr};

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
    let elem_ty = ctx.vec_elem_type(element);
    let lo_usize = lo as usize;
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
                let byte_lits: Vec<proc_macro2::Literal> = term_bytes
                    .iter()
                    .map(|b| proc_macro2::Literal::byte_character(*b))
                    .collect();
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

    // Pre-separator ws trim in the loop body.
    let pre_sep_ws = if config.ws && config.terminator_bytes.is_none() {
        ws_trim.clone()
    } else {
        quote! {}
    };

    // ── Scratch-based collection ────────────────────────────────────────────
    let depth_var = mctx.fresh("depth");
    let init_code = ctx.emit_scratch_init(&elem_ty, &depth_var);
    let push_first = ctx.emit_scratch_push(&elem_ty, &quote! { __value });
    let push_elem = ctx.emit_scratch_push(&elem_ty, &quote! { __value });
    let count_expr = ctx.emit_scratch_count(&elem_ty, &depth_var);
    let collect_expr = ctx.emit_scratch_collect(&elem_ty, &depth_var);
    let truncate_expr = ctx.emit_scratch_truncate(&elem_ty, &depth_var);

    let final_check = if let Some(close) = &config.close_expr {
        let ws = &ws_trim;
        quote! {
            #ws
            if #count_expr >= #lo_usize {
                #close?;
                Some(#collect_expr)
            } else {
                #truncate_expr
                None
            }
        }
    } else if config.ws {
        let ws = &ws_trim;
        quote! {
            if #count_expr >= #lo_usize {
                #ws
                Some(#collect_expr)
            } else {
                #truncate_expr
                None
            }
        }
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

    quote! {
        {
            #open_code
            #init_code
            #pre_ws
            let __first = #first_call;
            if let Some(__value) = __first {
                #push_first;
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
                        #push_elem;
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

/// Emit a monolithic sep_by (bare — no whitespace, no delimiters).
pub(super) fn emit_mono_sep_by(
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
