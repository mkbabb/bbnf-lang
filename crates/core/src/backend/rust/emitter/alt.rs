//! Alternation emission for the Rust backend.
//!
//! Tranche AC.2 tape-first. An Alt returns `Option<()>` — the
//! chosen branch is a side-effecting sub-parse; the owning rule's
//! epilogue carries the variant discriminator if the Alt is the
//! rule head. When a nested Alt is inside a bigger compound, the
//! caller composes it like any other sub-parse.
//!
//! All three emission shapes (`dispatch`, `checkpoint`,
//! `all_literal`, `key_dispatch`) follow the same contract: each
//! branch body is `Option<()>` (or composable thereof), and the
//! Alt expression returns `Option<()>` via a labeled block.
//! Heterogeneous type coercion is moot because all branch bodies
//! share the same return shape under tape-first.

use bbnf_ir::AltDispatch;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{AltBranchInfo, KeyClass, KeyDispatchBranch, KeyDispatchConfig, ValuePlacement};

use super::RustEmitter;
use super::RustEmitCtx;

impl RustEmitter {
    pub(super) fn emit_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut arms = Vec::new();

        for (branch_idx, (_info, body)) in branches.iter().enumerate() {
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

            // AK.1: when branch_idx_ident is set, prepend the branch
            // index assignment so the rule epilogue can use it as the
            // variant discriminator.
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                let idx = branch_idx as u8;
                quote! { #ident = #idx; }
            } else {
                quote! {}
            };

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            arms.push(quote! {
                #( #patterns )|* => { #branch_assign #body }
            });
        }

        let (fallback_arm, eof_expr) = if let Some((_info, fb_body)) = fallback {
            (
                quote! { _ => { #fb_body } },
                quote! { { #fb_body } },
            )
        } else {
            (quote! { _ => None }, quote! { None })
        };
        arms.push(fallback_arm);

        quote! {
            {
                if state.offset < state.src_bytes.len() {
                    match state.src_bytes[state.offset] {
                        #( #arms ),*
                    }
                } else {
                    #eof_expr
                }
            }
        }
    }

    pub(super) fn emit_alt_checkpoint_impl(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if branches.len() == 1 {
            let (_, body) = &branches[0];
            // AK.1: single branch still needs the idx assignment.
            if let Some(ref ident) = ctx.branch_idx_ident {
                return quote! { { #ident = 0u8; #body } };
            }
            return body.clone();
        }

        let mut chain = Vec::new();
        for (i, (_info, body)) in branches.iter().enumerate() {
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                let idx = i as u8;
                quote! { #ident = #idx; }
            } else {
                quote! {}
            };
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    #branch_assign
                    let __result = #body;
                    if __result.is_some() {
                        break 'alt_blk __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            'alt_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut chain = Vec::new();
        for (_value, body) in &literals {
            chain.push(quote! {
                {
                    let __r = #body;
                    if __r.is_some() { break 'alt_lit_blk __r; }
                }
            });
        }
        quote! {
            'alt_lit_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_key_dispatch_impl(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
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
                        let byte_lits: Vec<proc_macro2::Literal> = key
                            .iter()
                            .map(|b| proc_macro2::Literal::byte_character(*b))
                            .collect();
                        let len = key.len();
                        quote! { (__kd_len == #len && __kd_bytes == &[#(#byte_lits),*]) }
                    })
                    .collect();
                let body = kd.body;
                quote! {
                    if #(#comparisons)||* {
                        state.offset = #cp;
                        break 'kd_blk #body;
                    }
                }
            })
            .collect();
        let fallback_expr = if let Some((_info, fb)) = fallback {
            fb
        } else {
            quote! { None }
        };
        quote! {
            'kd_blk: {
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
