//! Prettify expression helpers: Ref, Skip, Next, Wrap.

use bbnf_ir::{GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::super::ir_types::IrCodegenCtx;
use super::{emit_prettify_expr, prettify_fn_ident};

// ── Ref ──────────────────────────────────────────────────────────────────────

pub(super) fn emit_prettify_ref(
    rule_id: bbnf_ir::RuleId,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    // Always inline — the prettify path doesn't need separate functions
    // per rule. Each Ref is expanded at its call site. Cyclic rules are
    // handled by the surrounding `lazy` wrapper in the monolithic path,
    // or by the function-level recursion in the monolithic path. For prettify,
    // we just inline the body — Rust's own function recursion handles cycles
    // since each non-transparent rule gets its own `__rule_prettify` function.
    let rule = &ir.rules[rule_id as usize];

    // Detect whitespace rules: body is a Regex matching the @ws pattern.
    // Emit inline spaces (no newlines) to preserve source formatting.
    // Newlines/indentation are suppressed — the @pretty hints handle those.
    let is_ws_rule = if let (IrNode::Regex(body_sid), Some(ws_sid)) = (&rule.body, ir.ws_pattern) {
        *body_sid == ws_sid
    } else {
        false
    };
    if is_ws_rule {
        let start_var = mctx.fresh("ws_start");
        let body_expr = emit_prettify_expr(&rule.body, ir, ctx, mctx);
        return quote! { {
            let #start_var = state.offset;
            let __wcp = __builder.checkpoint();
            #body_expr;
            __builder.restore(__wcp);
            let __ws = &state.src[#start_var..state.offset];
            if !__ws.is_empty() && !__ws.contains('\n') {
                __builder.text(__ws);
            }
        } };
    }

    let can_inline = mctx.fusion_eligible.get(rule_id as usize).copied() == Some(true)
        || mctx.single_site_inline.get(rule_id as usize).copied() == Some(true);
    if can_inline {
        // Save parent hints, switch to the inlined rule's hints so nested
        // Repeats use the correct separator (not the parent's @pretty hint).
        let saved_hints = mctx.current_pretty_hints.take();
        mctx.current_pretty_hints = rule.meta.directives.pretty.clone();
        let result = emit_prettify_expr(&rule.body, ir, ctx, mctx);
        mctx.current_pretty_hints = saved_hints;
        return result;
    }
    // Non-inlineable: call the rule's prettify function.
    let name = ir.get_string(rule.name);
    let fn_ident = prettify_fn_ident(name);
    quote! {
        if !Self::#fn_ident(state, __builder) {
            return false;
        }
    }
}

// ── Skip (keep left, discard right) ─────────────────────────────────────────
//
// For prettify, Skip/Next are TYPE operations (which value to return), not
// formatting operations. Both sides' text must be emitted. The "discard" refers
// to the returned value, not the formatted output.

pub(super) fn emit_prettify_skip(
    left: &IrNode,
    right: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let left_expr = emit_prettify_expr(left, ir, ctx, mctx);
    let right_expr = emit_prettify_expr(right, ir, ctx, mctx);
    quote! { {
        #left_expr;
        #right_expr;
    } }
}

// ── Next (discard left, keep right) ─────────────────────────────────────────

pub(super) fn emit_prettify_next(
    left: &IrNode,
    right: &IrNode,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let left_expr = emit_prettify_expr(left, ir, ctx, mctx);
    let right_expr = emit_prettify_expr(right, ir, ctx, mctx);
    quote! { {
        #left_expr;
        #right_expr;
    } }
}
