//! Operator-chain emission for the Rust backend.
//!
//! Generates the inline `head + (op rhs)*` loop with scratch-allocated
//! `Vec<(Op, Rhs)>` for the link list. Returns `None` for all-Span heads
//! (those degenerate to plain Seq emission).

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::quote;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_operator_chain_impl(
        &mut self,
        head: TokenStream,
        op: TokenStream,
        rhs: TokenStream,
        head_type: &TypeDesc,
        link_elem_type: &TypeDesc,
        ctx: &mut RustEmitCtx,
    ) -> Option<TokenStream> {
        let ir_ctx = ctx.ir_ctx();

        // Only handle typed chains (not all-Span).
        if *head_type == TypeDesc::Span {
            return None;
        }

        let depth_var = ctx.fresh("chain_depth");
        let head_var = ctx.fresh("chain_head");
        let prev_var = ctx.fresh("chain_prev");
        let op_var = ctx.fresh("chain_op");
        let rhs_var = ctx.fresh("chain_rhs");

        let init_code = ir_ctx.emit_scratch_init(link_elem_type, &depth_var);
        let push_code = ir_ctx.emit_scratch_push(link_elem_type, &quote! { (#op_var, #rhs_var) });
        let collect_code = ir_ctx.emit_scratch_collect(link_elem_type, &depth_var);

        Some(quote! {
            {
                let #head_var = #head?;
                #init_code
                loop {
                    let #prev_var = state.offset;
                    match (|| {
                        let #op_var = #op?;
                        let #rhs_var = #rhs?;
                        Some((#op_var, #rhs_var))
                    })() {
                        Some(__value) => {
                            let (#op_var, #rhs_var) = __value;
                            #push_code;
                            if state.offset == #prev_var {
                                break;
                            }
                        }
                        None => {
                            state.offset = #prev_var;
                            break;
                        }
                    }
                }
                Some((#head_var, #collect_code))
            }
        })
    }
}
