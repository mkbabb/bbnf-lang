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
//!
//! ## AM.3 per-branch `mark_children`
//!
//! When `ctx.tape_surgery` is active (Alt-bodied `MustTape` rules),
//! compound branches prepend `__children = mark_children(tape);
//! __has_children = true;` before their body. Leaf branches skip
//! `mark_children`. The shared rule epilogue in `grammar.rs` checks
//! `__has_children` to choose `push_compound` vs `push_leaf`.
//!
//! The surgery context is `take()`-ed by the emitter so nested Alts
//! within branch bodies don't re-apply the mark — they compile as
//! normal `Option<()>` sub-expressions.

use bbnf_ir::AltDispatch;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{AltBranchInfo, KeyClass, KeyDispatchBranch, KeyDispatchConfig, ValuePlacement};

use super::RustEmitter;
use super::RustEmitCtx;

impl RustEmitter {
    /// AM.3: emit `mark_children` + `__has_children = true` for
    /// compound branches. Returns empty tokens for leaf branches.
    fn emit_compound_mark(pushes_children: bool) -> TokenStream {
        if pushes_children {
            quote! {
                __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
                __has_children = true;
            }
        } else {
            quote! {}
        }
    }

    pub(super) fn emit_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // AM.3: take the surgery context so nested Alts within branch
        // bodies don't re-apply the per-branch mark_children.
        let surgery = ctx.tape_surgery.take();
        let mut arms = Vec::new();

        for (branch_idx, (info, body)) in branches.iter().enumerate() {
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

            // AM.3: compound branches prepend mark_children.
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            arms.push(quote! {
                #( #patterns )|* => { #branch_assign #compound_mark #body }
            });
        }

        let (fallback_arm, eof_expr) = if let Some((info, fb_body)) = fallback {
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            (
                quote! { _ => { #compound_mark #fb_body } },
                quote! { { #compound_mark #fb_body } },
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
        // AM.3: take the surgery context so nested Alts don't re-apply.
        let surgery = ctx.tape_surgery.take();
        let alt_blk = ctx.fresh_lifetime("alt_blk");

        if branches.len() == 1 {
            let (ref info, ref body) = branches[0];
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                quote! { #ident = 0u8; }
            } else {
                quote! {}
            };
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            if !branch_assign.is_empty() || !compound_mark.is_empty() {
                return quote! { { #branch_assign #compound_mark #body } };
            }
            return body.clone();
        }

        let mut chain = Vec::new();
        for (i, (info, body)) in branches.iter().enumerate() {
            let branch_assign = if let Some(ref ident) = ctx.branch_idx_ident {
                let idx = i as u8;
                quote! { #ident = #idx; }
            } else {
                quote! {}
            };
            let compound_mark = if surgery.is_some() {
                Self::emit_compound_mark(info.pushes_children)
            } else {
                quote! {}
            };
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    #branch_assign
                    #compound_mark
                    let __result = #body;
                    if __result.is_some() {
                        break #alt_blk __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            #alt_blk: {
                #( #chain )*
                None
            }
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let alt_lit_blk = ctx.fresh_lifetime("alt_lit_blk");
        let mut chain = Vec::new();
        for (_value, body) in &literals {
            chain.push(quote! {
                {
                    let __r = #body;
                    if __r.is_some() { break #alt_lit_blk __r; }
                }
            });
        }
        quote! {
            #alt_lit_blk: {
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
            KeyClass::Identifier => quote! {
                ::parse_that::scan_ident(state, &::parse_that::DEFAULT_IDENT_CONFIG)
            },
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
