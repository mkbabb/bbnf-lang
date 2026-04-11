//! Token dispatch and delimiter scan emission for the Rust backend.
//!
//! Tranche AC.2 tape-first.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::{DelimScanConfig, TokenDispatchArmCompiled};

use super::RustEmitCtx;
use super::RustEmitter;

impl RustEmitter {
    pub(super) fn emit_token_dispatch_impl(
        &mut self,
        token: TokenStream,
        arms: Vec<TokenDispatchArmCompiled<TokenStream>>,
        fallback: TokenStream,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let token_var = ctx.fresh("tok");
        let mut arm_checks = Vec::new();
        for arm in &arms {
            let patterns: Vec<TokenStream> = arm
                .patterns
                .iter()
                .map(|pat| {
                    let byte_lits: Vec<proc_macro2::Literal> =
                        pat.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                    let len = pat.len();
                    quote! { (__td_len == #len && __td_bytes == &[#(#byte_lits),*]) }
                })
                .collect();
            let cont = &arm.continuation;
            if let Some(guard) = arm.guard_byte {
                arm_checks.push(quote! {
                    if (#(#patterns)||*) && state.offset < state.src_bytes.len()
                        && state.src_bytes[state.offset] == #guard
                    {
                        break 'td_blk #cont;
                    }
                });
            } else {
                arm_checks.push(quote! {
                    if #(#patterns)||* {
                        break 'td_blk #cont;
                    }
                });
            }
        }
        quote! {
            'td_blk: {
                if let Some(#token_var) = #token {
                    let __td_bytes = &state.src_bytes[#token_var.start..#token_var.end];
                    let __td_len = __td_bytes.len();
                    #(#arm_checks)*
                }
                #fallback
            }
        }
    }

    pub(super) fn emit_delim_scan_impl(
        &mut self,
        config: &DelimScanConfig,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        // Delim-scan kernel body lives in
        // `backend::kernels::balanced_wrap`. Under tape-first the
        // inner rule calls pass `(state, tape)`; we wrap them in
        // lambdas that discard the offset to satisfy the kernel's
        // scanner contract.
        let block_call = if let Some((_, ref name)) = config.block_rule {
            let fn_ident = format_ident!("__{}", name);
            quote! { Self::#fn_ident(state, tape) }
        } else {
            quote! { None::<::bbnf::runtime::tape::TapeOffset> }
        };

        let pivot_call = if let Some((_, ref name)) = config.pivot_rule {
            let fn_ident = format_ident!("__{}", name);
            quote! { Self::#fn_ident(state, tape) }
        } else {
            quote! { None::<::bbnf::runtime::tape::TapeOffset> }
        };

        let trail_consume = if let Some(tb) = config.trail_byte {
            quote! {
                if state.offset < state.src_bytes.len()
                    && state.src_bytes[state.offset] == #tb
                {
                    state.offset += 1;
                }
            }
        } else {
            quote! {}
        };

        Some(crate::backend::kernels::balanced_wrap::emit_call(
            config.open_byte,
            config.close_byte,
            config.pivot_byte,
            &block_call,
            &pivot_call,
            &trail_consume,
        ))
    }
}
