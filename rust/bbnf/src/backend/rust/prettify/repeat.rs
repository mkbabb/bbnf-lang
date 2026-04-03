//! Prettify Repeat emission - repetition with policy-driven separator formatting.

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::attempt::{emit_prettify_attempt, emits_only_on_success};
use super::policy::{emit_separator, PrettifyCtx, SeparatorPolicy};
use super::emit_prettify_expr;

/// Emit a Repeat for prettify.
pub(super) fn emit_prettify_repeat(
    inner: &IrNode,
    lo: usize,
    hi: usize,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if lo == 0 && hi == 1 {
        return emit_prettify_optional(inner, pctx, current_rule, mctx);
    }

    let plan = pctx.plan(current_rule);
    let sep_expr = emit_separator(&plan.policy);
    let has_separator = !matches!(plan.policy.separator, SeparatorPolicy::None);

    let inner_expr = emit_prettify_expr(inner, pctx, current_rule, mctx);
    let inner_try = if has_separator {
        emit_prettify_attempt(inner_expr, false, Some((inner, pctx)), mctx)
    } else {
        emit_prettify_attempt(inner_expr, !emits_only_on_success(inner, pctx), Some((inner, pctx)), mctx)
    };

    let count_var = mctx.fresh("rep_count");
    let cp_var = mctx.fresh("rep_cp");
    let loop_start_state = if lo > 0 {
        Some(mctx.fresh("rep_start"))
    } else {
        None
    };
    let loop_start_builder = if lo > 0 {
        Some(mctx.fresh("rep_bcp"))
    } else {
        None
    };

    let lo_check = if lo > 0 {
        let lo_lit = proc_macro2::Literal::usize_unsuffixed(lo);
        if let (Some(start_state), Some(start_builder)) = (&loop_start_state, &loop_start_builder) {
            quote! {
                if #count_var < #lo_lit {
                    state.offset = #start_state;
                    __builder.restore(#start_builder);
                    return false;
                }
            }
        } else {
            quote! {
                if #count_var < #lo_lit {
                    return false;
                }
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

    let loop_cp = if let (Some(start_state), Some(start_builder)) = (&loop_start_state, &loop_start_builder) {
        quote! {
            let #start_state = state.offset;
            let #start_builder = __builder.checkpoint();
        }
    } else {
        quote! {}
    };

    if has_separator {
        // With separator: checkpoint covers sep + inner so we can undo the
        // separator if the inner expression fails on the next iteration.
        quote! { {
            #loop_cp
            let mut #count_var = 0usize;
            while #hi_check {
                let #cp_var = state.offset;
                let __iter_cp = if #count_var > 0 {
                    Some(__builder.checkpoint())
                } else {
                    None
                };
                if #count_var > 0 {
                    #sep_expr
                };
                if !#inner_try {
                    state.offset = #cp_var;
                    if let Some(__bcp) = __iter_cp {
                        __builder.restore(__bcp);
                    }
                    break;
                }
                if state.offset == #cp_var {
                    break;
                }
                #count_var += 1;
            }
            #lo_check
        } }
    } else {
        // No separator: simplified loop with just state checkpoint.
        quote! { {
            #loop_cp
            let mut #count_var = 0usize;
            while #hi_check {
                let #cp_var = state.offset;
                if !#inner_try {
                    state.offset = #cp_var;
                    break;
                }
                if state.offset == #cp_var {
                    break;
                }
                #count_var += 1;
            }
            #lo_check
        } }
    }
}

fn emit_prettify_optional(
    inner: &IrNode,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let inner_expr = emit_prettify_expr(inner, pctx, current_rule, mctx);
    let inner_try = emit_prettify_attempt(inner_expr, !emits_only_on_success(inner, pctx), Some((inner, pctx)), mctx);
    quote! { {
        let _ = #inner_try;
        true
    } }
}
