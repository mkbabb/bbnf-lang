//! Monolithic codegen for TokenDispatch — lexer-parser fusion.
//!
//! Emits: parse the token once (e.g., `scan_ident`), then match the consumed
//! bytes against arm patterns via byte comparison. Apply guard_byte check for
//! function detection (`(`). Fall back to original Alt when no pattern matches.

use bbnf_ir::{IrNode, TokenDispatchArm};

use proc_macro2::TokenStream;
use quote::quote;

use super::ir_types::IrCodegenCtx;
use super::unescape_literal;
use super::{emit_mono_expr, MonoCtx};

/// Emit monolithic code for a TokenDispatch node.
///
/// Parses the token once, then dispatches on the consumed string value.
/// Each arm has a set of string patterns and an optional guard byte.
/// Falls back to the original Alt when no pattern matches.
pub(super) fn emit_token_dispatch(
    token: &IrNode,
    arms: &[TokenDispatchArm],
    fallback: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // If no arms, just emit the fallback.
    if arms.is_empty() {
        return emit_mono_expr(fallback, ctx, mctx, elide_box);
    }

    // Emit the token scanner. For identifier tokens, this is scan_ident.
    let token_expr = emit_mono_expr(token, ctx, mctx, elide_box);
    let cp_var = mctx.fresh("td_cp");

    // Build if-else chain for each arm's patterns.
    let mut arm_checks: Vec<TokenStream> = Vec::new();

    for arm in arms {
        let continuation_expr = emit_mono_expr(&arm.continuation, ctx, mctx, elide_box);

        // Generate byte comparisons for each pattern string.
        let comparisons: Vec<TokenStream> = arm.patterns.iter().map(|&sid| {
            let pat = unescape_literal(ctx.ir.get_string(sid));
            let bytes = pat.as_bytes();
            let byte_lits: Vec<proc_macro2::Literal> =
                bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
            let len = bytes.len();
            quote! { (__td_len == #len && __td_bytes == &[#(#byte_lits),*]) }
        }).collect();

        // If there's a guard byte, check it after the pattern match.
        let body = if let Some(guard) = arm.guard_byte {
            let guard_lit = proc_macro2::Literal::byte_character(guard);
            quote! {
                if state.src_bytes.get(state.offset).copied() == Some(#guard_lit) {
                    return #continuation_expr;
                }
            }
        } else {
            quote! {
                return #continuation_expr;
            }
        };

        arm_checks.push(quote! {
            if #(#comparisons)||* {
                #body
            }
        });
    }

    // Fallback: restore offset and try the original Alt.
    let fallback_expr = emit_mono_expr(fallback, ctx, mctx, elide_box);

    quote! {
        {
            let #cp_var = state.offset;
            if let Some(ref __td_s) = (|| { #token_expr })() {
                let __td_bytes = &state.src_bytes[__td_s.start..__td_s.end];
                let __td_len = __td_bytes.len();
                #(#arm_checks)*
            }
            // No pattern matched — restore and try fallback.
            state.offset = #cp_var;
            #fallback_expr
        }
    }
}
