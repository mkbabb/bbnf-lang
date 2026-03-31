//! Prettify Repeat emission — repetition with hint-driven separator formatting.

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::super::ir_types::IrCodegenCtx;
use super::super::MonoCtx;
use super::emit_prettify_expr;

/// Emit a Repeat for prettify.
pub(super) fn emit_prettify_repeat(
    inner: &IrNode,
    lo: usize,
    hi: usize,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if lo == 0 && hi == 1 {
        // Optional: try once, succeed either way.
        return emit_prettify_optional(inner, ir, ctx, mctx);
    }

    // Many / Many1: loop with hint-driven separator.
    let inner_expr = emit_prettify_expr(inner, ir, ctx, mctx);
    let count_var = mctx.fresh("rep_count");
    let cp_var = mctx.fresh("rep_cp");

    // Determine separator from @pretty hints on the current rule.
    let sep_expr = emit_separator(mctx);

    let lo_check = if lo > 0 {
        let lo_lit = proc_macro2::Literal::usize_unsuffixed(lo);
        quote! {
            if #count_var < #lo_lit {
                return false;
            }
        }
    } else {
        quote! {}
    };

    let hi_check = if hi < usize::MAX {
        let hi_lit = proc_macro2::Literal::usize_unsuffixed(hi);
        quote! { #count_var < #hi_lit }
    } else {
        quote! { true }
    };

    quote! { {
        let mut #count_var = 0usize;
        while #hi_check {
            let #cp_var = state.offset;
            let __bcp = __builder.checkpoint();
            // Emit separator between items (not before the first).
            if #count_var > 0 {
                #sep_expr
            }
            let __ok = (|| -> bool { #inner_expr; true })();
            if !__ok {
                state.offset = #cp_var;
                __builder.restore(__bcp);
                break;
            }
            // Guard against zero-length match to prevent infinite loops.
            if state.offset == #cp_var {
                break;
            }
            #count_var += 1;
        }
        #lo_check
    } }
}

/// Emit the separator expression based on @pretty hints.
fn emit_separator(mctx: &MonoCtx) -> TokenStream {
    let hints = match &mctx.current_pretty_hints {
        Some(h) => h,
        // No @pretty hint on this rule — no separator between repeat items.
        None => return quote! {},
    };

    // Custom separator: sep("str") → IfBreak(hardline, "str")
    if let Some(ref sep_str) = hints.sep {
        let sep_lit = proc_macro2::Literal::string(sep_str);
        return quote! {
            __builder.if_break(
                |b| { b.hardline(); },
                |b| { b.text(#sep_lit); },
            );
        };
    }

    if hints.blankline {
        // Double hardline (blank line between items).
        quote! { __builder.hardline(); __builder.hardline(); }
    } else if hints.block || hints.hardbreak || hints.fast {
        // Single hardline.
        quote! { __builder.hardline(); }
    } else if hints.nobreak {
        // Space separator (never break).
        quote! { __builder.text(" "); }
    } else if hints.compact || hints.off {
        // No separator.
        quote! {}
    } else if hints.softbreak {
        // Explicit softline.
        quote! { __builder.softline(); }
    } else {
        // No explicit separator hint — no separator.
        // @pretty rules with specific separator needs should use explicit hints.
        quote! {}
    }
}

fn emit_prettify_optional(
    inner: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let inner_expr = emit_prettify_expr(inner, ir, ctx, mctx);
    let cp_var = mctx.fresh("opt_cp");
    quote! { {
        let #cp_var = state.offset;
        let __bcp = __builder.checkpoint();
        let __ok = (|| -> bool { #inner_expr; true })();
        if !__ok {
            state.offset = #cp_var;
            __builder.restore(__bcp);
        }
    } }
}
