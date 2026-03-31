//! Prettify Alt emission — dispatch tables and sequential trial for alternation.

use bbnf_ir::{AltBranch, AltDispatch, GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::ir_types::IrCodegenCtx;
use super::super::MonoCtx;
use super::emit_prettify_expr;

/// Emit an Alt for prettify: dispatch table or sequential trial.
pub(super) fn emit_prettify_alt(
    branches: &[AltBranch],
    dispatch: Option<&AltDispatch>,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    if branches.len() == 1 {
        return emit_prettify_expr(&branches[0].node, ir, ctx, mctx);
    }

    // Try dispatch table first.
    if let Some(disp) = dispatch {
        return emit_prettify_dispatch(branches, disp, ir, ctx, mctx);
    }

    // Fallback: sequential trial with checkpoint/restore.
    emit_prettify_sequential(branches, ir, ctx, mctx)
}

fn emit_prettify_dispatch(
    branches: &[AltBranch],
    disp: &AltDispatch,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
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

        let branch_expr = emit_prettify_expr(&branch.node, ir, ctx, mctx);
        match_arms.push(quote! { #(#byte_patterns)|* => { #branch_expr; } });
    }

    // Find nullable branch for default + EOF handling.
    let nullable_idx = disp.fallback_idx.or_else(|| {
        branches.iter().position(|b| {
            matches!(b.node, IrNode::Epsilon)
                || matches!(b.node, IrNode::Repeat { lo: 0, .. })
                || b.first_set.is_none()
        }).map(|i| i as u8)
    });

    let default_arm = if let Some(nul_idx) = nullable_idx {
        let nul_expr = emit_prettify_expr(&branches[nul_idx as usize].node, ir, ctx, mctx);
        quote! { _ => { #nul_expr; } }
    } else {
        quote! { _ => { return false; } }
    };
    match_arms.push(default_arm);

    let eof_handler = if let Some(nul_idx) = nullable_idx {
        let nul_expr = emit_prettify_expr(&branches[nul_idx as usize].node, ir, ctx, mctx);
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
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    let cp_var = mctx.fresh("alt_cp");
    let ops_var = mctx.fresh("alt_ops");
    let mut arms: Vec<TokenStream> = Vec::new();

    for (i, branch) in branches.iter().enumerate() {
        let branch_expr = emit_prettify_expr(&branch.node, ir, ctx, mctx);
        if i < branches.len() - 1 {
            arms.push(quote! {
                {
                    let #ops_var = __builder.ops().len();
                    state.offset = #cp_var;
                    let mut __ok = true;
                    #branch_expr;
                    if __ok {
                        // Success — keep these ops.
                    } else {
                        // Failure — would need to truncate builder ops.
                        // For now, we rely on the branch not emitting ops on failure.
                        state.furthest_offset = state.furthest_offset.max(state.offset);
                        state.offset = #cp_var;
                    }
                }
            });
        } else {
            // Last branch: no checkpoint needed.
            arms.push(quote! { { #branch_expr; } });
        }
    }

    // Simplified sequential trial — branches that fail should not emit ops.
    // This is enforced by the `return false` pattern in leaf expressions.
    let first_expr = emit_prettify_expr(&branches[0].node, ir, ctx, mctx);
    if branches.len() == 2 {
        let second_expr = emit_prettify_expr(&branches[1].node, ir, ctx, mctx);
        return quote! { {
            let #cp_var = state.offset;
            let __bcp = __builder.checkpoint();
            let __ok = (|| -> bool { #first_expr; true })();
            if !__ok {
                state.offset = #cp_var;
                __builder.restore(__bcp);
                #second_expr;
            }
        } };
    }

    // General case: try each branch in order, restore builder on failure.
    let mut result = quote! { return false; };
    for branch in branches.iter().rev() {
        let branch_expr = emit_prettify_expr(&branch.node, ir, ctx, mctx);
        result = quote! {
            {
                let __saved = state.offset;
                let __bcp = __builder.checkpoint();
                let __ok = (|| -> bool { #branch_expr; true })();
                if !__ok {
                    state.offset = __saved;
                    __builder.restore(__bcp);
                    #result
                }
            }
        };
    }
    result
}
