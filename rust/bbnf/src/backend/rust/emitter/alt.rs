//! Alternation emission for the shared-driver Rust emitter.

use bbnf_ir::AltDispatch;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::key_dispatch::{KeyClass, KeyDispatchConfig};
use crate::backend::{AllocStrategy, AltBranchInfo, KeyDispatchBranch};

use super::RustEmitter;
use super::RustEmitCtx;

impl RustEmitter {
    pub(super) fn emit_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Build match arms from dispatch table.
        let mut arms = Vec::new();

        for (branch_idx, (_info, body)) in branches.iter().enumerate() {
            // Collect all bytes that map to this branch.
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(byte_val, _)| byte_val as u8)
                .collect();

            if byte_patterns.is_empty() {
                continue;
            }

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            arms.push(quote! {
                #( #patterns )|* => { #body }
            });
        }

        // Fallback arm.
        let fallback_expr = if let Some((_info, fb_body)) = fallback {
            quote! { _ => { #fb_body } }
        } else {
            quote! { _ => None }
        };
        arms.push(fallback_expr);

        quote! {
            if state.offset < state.src.len() {
                match state.src.as_bytes()[state.offset] {
                    #( #arms ),*
                }
            } else {
                None
            }
        }
    }

    pub(super) fn emit_alt_checkpoint_impl(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if branches.len() == 1 {
            let (_, body) = &branches[0];
            return body.clone();
        }

        let mut chain = Vec::new();
        for (_info, body) in &branches {
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    let __result = #body;
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            (|| {
                #( #chain )*
                None
            })()
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Checkpoint-free sequential literal matching.
        // Literal `starts_with` is non-destructive — no save/restore needed.
        let mut chain = Vec::new();
        for (_value, body) in &literals {
            chain.push(quote! {
                let __r = #body;
                if __r.is_some() { return __r; }
            });
        }
        quote! {
            (|| {
                #( #chain )*
                None
            })()
        }
    }

    pub(super) fn emit_key_dispatch_impl(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let cp = ctx.fresh("kd_cp");
        let scanner = match config.key_class {
            KeyClass::Identifier => quote! { ::parse_that::scan_ident(state) },
            KeyClass::QuotedString { .. } => quote! { ::parse_that::scan_string_quoted(state) },
        };
        let arm_checks: Vec<TokenStream> = branches
            .into_iter()
            .map(|kd| {
                let comparisons: Vec<TokenStream> = kd
                    .key_bytes
                    .iter()
                    .map(|key| {
                        let byte_lits: Vec<proc_macro2::Literal> =
                            key.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                        let len = key.len();
                        quote! { (__kd_len == #len && __kd_bytes == &[#(#byte_lits),*]) }
                    })
                    .collect();
                let body = kd.body;
                quote! {
                    if #(#comparisons)||* {
                        state.offset = #cp;
                        return #body;
                    }
                }
            })
            .collect();
        let fallback_expr = if let Some((_, fb)) = fallback {
            fb
        } else {
            quote! { None }
        };
        quote! {
            {
                let #cp = state.offset;
                if let Some(ref __kd_s) = #scanner {
                    let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                    let __kd_len = __kd_bytes.len();
                    #(#arm_checks)*
                }
                state.offset = #cp;
                #fallback_expr
            }
        }
    }
}
