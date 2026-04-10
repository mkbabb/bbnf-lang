//! Sequence emission for the Rust backend.
//!
//! `emit_seq_grouped_impl` consumes per-child groups (single value or
//! span-compressed run) and produces a Rust IIFE expression that returns
//! the assembled tuple. Vec-flattening (for `(T, Vec<T>)` patterns) is
//! handled here as well via the scratch-allocator helpers on
//! `RustEmitCtx::ir_ctx()`.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::quote;

use crate::backend::{FlattenStrategy, SeqChildGroup};

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_seq_grouped_impl(
        &mut self,
        groups: Vec<SeqChildGroup<TokenStream>>,
        result_type: &TypeDesc,
        flatten: Option<FlattenStrategy>,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut stmts = Vec::new();
        let mut result_vars = Vec::new();
        let mut result_types = Vec::new();

        // Tranche AA.8 — use `match` instead of let-else because child
        // outputs can be block-shaped (`{ ... }`) and Rust's let-else
        // syntax forbids a brace-terminated RHS. The match arm comma
        // makes the ambiguity disappear.
        for group in groups {
            match group {
                SeqChildGroup::Single { output, ty } => {
                    let var = ctx.fresh("v");
                    stmts.push(quote! {
                        let #var = match #output {
                            Some(__v) => __v,
                            None => break 'seq_blk None,
                        };
                    });
                    result_vars.push(var);
                    result_types.push(ty);
                }
                SeqChildGroup::SpanCompressed { outputs } => {
                    let var = ctx.fresh("sp");
                    let output_checks: Vec<TokenStream> = outputs
                        .into_iter()
                        .map(|o| quote! { if #o.is_none() { break 'seq_blk None; } })
                        .collect();
                    stmts.push(quote! {
                        let __sp_start = state.offset;
                        #( #output_checks )*
                        let #var = ::parse_that::Span::new(__sp_start, state.offset, state.src);
                    });
                    result_vars.push(var);
                    result_types.push(TypeDesc::Span);
                }
            }
        }

        // Handle Vec flattening: (T, Vec<T>) → Vec<T> via scratch.
        if let Some(flatten_strat) = flatten {
            if let TypeDesc::Vec(elem_td) = result_type {
                let ir_ctx = ctx.ir_ctx();
                let depth_var = ctx.fresh("depth");
                let init = ir_ctx.emit_scratch_init(elem_td, &depth_var);
                let collect = ir_ctx.emit_scratch_collect(elem_td, &depth_var);

                match flatten_strat {
                    FlattenStrategy::HeadThenVec => {
                        // (head, &[T]) → push head, extend from slice
                        if result_vars.len() == 2 {
                            let head = &result_vars[0];
                            let tail = &result_vars[1];
                            let push = ir_ctx.emit_scratch_push(elem_td, &quote! { #head });
                            let extend = ir_ctx.emit_scratch_extend_slice(elem_td, &quote! { #tail });
                            // Tranche AA.8 — labeled block instead of IIFE.
                            return quote! {
                                'seq_blk: {
                                    #init
                                    #( #stmts )*
                                    #push;
                                    #extend;
                                    Some(#collect)
                                }
                            };
                        }
                    }
                    FlattenStrategy::VecThenTail => {
                        // (&[T], tail) → extend from slice, push tail
                        if result_vars.len() == 2 {
                            let vec_part = &result_vars[0];
                            let tail = &result_vars[1];
                            let extend = ir_ctx.emit_scratch_extend_slice(elem_td, &quote! { #vec_part });
                            let push = ir_ctx.emit_scratch_push(elem_td, &quote! { #tail });
                            // Tranche AA.8 — labeled block instead of IIFE.
                            return quote! {
                                'seq_blk: {
                                    #init
                                    #( #stmts )*
                                    #extend;
                                    #push;
                                    Some(#collect)
                                }
                            };
                        }
                    }
                }
            }
        }

        // Assemble result.
        let result_expr = if result_vars.len() == 1 {
            let v = &result_vars[0];
            quote! { #v }
        } else {
            quote! { ( #( #result_vars ),* ) }
        };

        // Tranche AA.8 — labeled block instead of IIFE.
        quote! {
            'seq_blk: {
                #( #stmts )*
                Some(#result_expr)
            }
        }
    }
}
