//! Token dispatch and delimiter scan emission for the shared-driver Rust emitter.

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
            let patterns: Vec<TokenStream> = arm.patterns.iter().map(|pat| {
                let byte_lits: Vec<proc_macro2::Literal> =
                    pat.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                let len = pat.len();
                quote! { (__td_len == #len && __td_bytes == &[#(#byte_lits),*]) }
            }).collect();
            let cont = &arm.continuation;
            if let Some(guard) = arm.guard_byte {
                arm_checks.push(quote! {
                    if (#(#patterns)||*) && state.offset < state.src.len()
                        && state.src.as_bytes()[state.offset] == #guard
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
        // Tranche AA.8 — labeled block for arm_checks.
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
        // Tranche W phase 3d: emission body lives in
        // `backend::kernels::balanced_wrap`. The emitter builds the
        // per-grammar dispatch token streams (block call / pivot call
        // / trail consume) and delegates to the kernel.
        let block_call = if let Some((_, ref name)) = config.block_rule {
            let fn_ident = format_ident!("__{}", name);
            quote! { Self::#fn_ident(state) }
        } else {
            quote! { None::<::parse_that::Span<'_>> }
        };

        let pivot_call = if let Some((_, ref name)) = config.pivot_rule {
            let fn_ident = format_ident!("__{}", name);
            quote! { Self::#fn_ident(state) }
        } else {
            quote! { None::<::parse_that::Span<'_>> }
        };

        let trail_consume = if let Some(tb) = config.trail_byte {
            quote! {
                if state.offset < state.src.len()
                    && state.src.as_bytes()[state.offset] == #tb
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
