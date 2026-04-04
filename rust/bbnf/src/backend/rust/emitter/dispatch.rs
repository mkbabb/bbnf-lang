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
                        return #cont;
                    }
                });
            } else {
                arm_checks.push(quote! {
                    if #(#patterns)||* {
                        return #cont;
                    }
                });
            }
        }
        quote! {
            (|| {
                if let Some(#token_var) = #token {
                    let __td_bytes = &state.src_bytes[#token_var.start..#token_var.end];
                    let __td_len = __td_bytes.len();
                    #(#arm_checks)*
                }
                #fallback
            })()
        }
    }

    pub(super) fn emit_delim_scan_impl(
        &mut self,
        config: &DelimScanConfig,
        _ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        let open = config.open_byte;
        let close = config.close_byte;
        let pivot = config.pivot_byte;

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

        Some(quote! {
            (|| {
                let __ds_start = state.offset;
                if state.offset >= state.src.len()
                    || state.src.as_bytes()[state.offset] != #open
                {
                    return None;
                }
                state.offset += 1;
                loop {
                    if state.offset >= state.src.len() {
                        state.offset = __ds_start;
                        return None;
                    }
                    let __b = state.src.as_bytes()[state.offset];
                    if __b == #close {
                        state.offset += 1;
                        return Some(::parse_that::Span::new(
                            __ds_start,
                            state.offset,
                            state.src,
                        ));
                    }
                    if __b == #open {
                        match #block_call {
                            Some(_) => continue,
                            None => {
                                state.offset = __ds_start;
                                return None;
                            }
                        }
                    }
                    // Scan for pivot byte.
                    loop {
                        if state.offset >= state.src.len() {
                            break;
                        }
                        let __pb = state.src.as_bytes()[state.offset];
                        if __pb == #pivot {
                            state.offset += 1;
                            #trail_consume
                            match #pivot_call {
                                Some(_) => break, // Back to outer loop.
                                None => {
                                    state.offset = __ds_start;
                                    return None;
                                }
                            }
                        }
                        if __pb == #close || __pb == #open {
                            break; // Let outer loop handle delimiter.
                        }
                        state.offset += 1;
                    }
                }
            })()
        })
    }
}
