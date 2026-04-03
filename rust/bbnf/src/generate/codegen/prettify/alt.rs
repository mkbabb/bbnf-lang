//! Prettify Alt emission - dispatch tables and sequential trial for alternation.

use bbnf_ir::{AltBranch, AltDispatch, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::MonoCtx;
use super::attempt::{emit_prettify_attempt, emits_only_on_success};
use super::emit_prettify_expr;
use super::policy::PrettifyCtx;

/// Emit an Alt for prettify: dispatch table or sequential trial.
pub(super) fn emit_prettify_alt(
    branches: &[AltBranch],
    dispatch: Option<&AltDispatch>,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if branches.len() == 1 {
        return emit_prettify_expr(&branches[0].node, pctx, current_rule, mctx);
    }

    // Try dispatch table first.
    if let Some(disp) = dispatch {
        return emit_prettify_dispatch(branches, disp, pctx, current_rule, mctx);
    }

    // Fallback: sequential trial with checkpoint/restore.
    emit_prettify_sequential(branches, pctx, current_rule, mctx)
}

fn emit_prettify_dispatch(
    branches: &[AltBranch],
    disp: &AltDispatch,
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let mut match_arms: Vec<TokenStream> = Vec::new();
    let mut used = vec![false; branches.len()];

    for (idx, branch) in branches.iter().enumerate() {
        if used[idx] {
            continue;
        }
        used[idx] = true;
        let bytes: Vec<u8> = (0u8..128)
            .filter(|&c| disp.table.get(c as usize).copied() == Some(idx as u8))
            .collect();
        if bytes.is_empty() {
            continue;
        }
        let byte_patterns: Vec<TokenStream> = bytes
            .iter()
            .map(|&b| {
                let b_lit = proc_macro2::Literal::byte_character(b);
                quote! { #b_lit }
            })
            .collect();

        let branch_expr = emit_prettify_expr(&branch.node, pctx, current_rule, mctx);
        match_arms.push(quote! { #(#byte_patterns)|* => { #branch_expr; } });
    }

    // Find nullable branch for default + EOF handling.
    let nullable_idx = disp.fallback_idx.or_else(|| {
        branches
            .iter()
            .position(|b| {
                matches!(b.node, IrNode::Epsilon)
                    || matches!(b.node, IrNode::Repeat { lo: 0, .. })
                    || b.first_set.is_none()
            })
            .map(|i| i as u8)
    });

    let default_arm = if let Some(nul_idx) = nullable_idx {
        let nul_expr = emit_prettify_expr(&branches[nul_idx as usize].node, pctx, current_rule, mctx);
        quote! { _ => { #nul_expr; } }
    } else {
        quote! { _ => { return false; } }
    };
    match_arms.push(default_arm);

    let eof_handler = if let Some(nul_idx) = nullable_idx {
        let nul_expr = emit_prettify_expr(&branches[nul_idx as usize].node, pctx, current_rule, mctx);
        quote! {
            let Some(&__byte) = state.src_bytes.get(state.offset) else {
                #nul_expr;
                return true;
            };
        }
    } else {
        quote! {
            let __byte = match state.src_bytes.get(state.offset) {
                Some(&b) => b,
                None => return false,
            };
        }
    };

    quote! { {
        #eof_handler
        match __byte {
            #(#match_arms)*
        }
    } }
}

fn emit_prettify_sequential(
    branches: &[AltBranch],
    pctx: &PrettifyCtx<'_>,
    current_rule: bbnf_ir::RuleId,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if branches.len() == 2 {
        let first_expr = emit_prettify_expr(&branches[0].node, pctx, current_rule, mctx);
        let first_try = emit_prettify_attempt(
            first_expr,
            !emits_only_on_success(&branches[0].node, pctx),
            Some((&branches[0].node, pctx)),
            mctx,
        );
        let second_expr = emit_prettify_expr(&branches[1].node, pctx, current_rule, mctx);
        return quote! { {
            if !#first_try {
                #second_expr;
            }
        } };
    }

    // General case: try each branch in order, restore builder on failure.
    let mut result = quote! { return false; };
    for branch in branches.iter().rev() {
        let branch_expr = emit_prettify_expr(&branch.node, pctx, current_rule, mctx);
        let branch_try = emit_prettify_attempt(
            branch_expr,
            !emits_only_on_success(&branch.node, pctx),
            Some((&branch.node, pctx)),
            mctx,
        );
        result = quote! {
            {
                if !#branch_try {
                    #result
                }
            }
        };
    }
    result
}
