//! Prettify expression helpers: Ref, Skip, Next, Wrap.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::{emit_prettify_expr, prettify_fn_ident};
use super::policy::PrettifyCtx;

// ── Ref ──────────────────────────────────────────────────────────────────────

pub(super) fn emit_prettify_ref(
    rule_id: bbnf_ir::RuleId,
    pctx: &PrettifyCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let rule = &pctx.ir.rules[rule_id as usize];
    let plan = pctx.plan(rule_id);

    // Whitespace rules: inline the scan + text_inline_ws directly at the
    // call site, avoiding the function call + checkpoint + discard + re-emit
    // pattern of the generated __ws_prettify function.
    if plan.policy.is_ws_rule {
        let ws_start = mctx.fresh("ws_start");
        let body_expr = emit_prettify_expr(&rule.body, pctx, rule_id, mctx);
        return quote! {{
            let #ws_start = state.offset;
            let __wcp = __builder.light_checkpoint();
            #body_expr;
            __builder.light_restore(__wcp);
            __builder.text_inline_ws(&state.src[#ws_start..state.offset]);
        }};
    }

    if plan.inline {
        return emit_prettify_expr(&rule.body, pctx, rule_id, mctx);
    }

    let name = pctx.ir.get_string(rule.name);
    let fn_ident = prettify_fn_ident(name);
    quote! {
        if !Self::#fn_ident(state, __builder) {
            return false;
        }
    }
}

// ── Skip (keep left, discard right) ─────────────────────────────────────────

pub(super) fn emit_prettify_skip(
    left: &IrNode,
    right: &IrNode,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let left_expr = emit_prettify_expr(left, pctx, current_rule, mctx);
    let right_expr = emit_prettify_expr(right, pctx, current_rule, mctx);
    quote! { {
        #left_expr;
        #right_expr;
    } }
}

// ── Next (discard left, keep right) ─────────────────────────────────────────

pub(super) fn emit_prettify_next(
    left: &IrNode,
    right: &IrNode,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let left_expr = emit_prettify_expr(left, pctx, current_rule, mctx);
    let right_expr = emit_prettify_expr(right, pctx, current_rule, mctx);
    quote! { {
        #left_expr;
        #right_expr;
    } }
}
