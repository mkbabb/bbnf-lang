//! Binary-op + reference emission for the Rust backend:
//! `emit_call`, `emit_inline_wrap`, `emit_skip`, `emit_next`, `emit_minus`,
//! `emit_negate`.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::ValuePlacement;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_call_impl(
        &mut self,
        rule_name: &str,
        alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let fn_ident = format_ident!("__{}", rule_name);
        if alloc == ValuePlacement::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let val = quote! { __v };
            let alloc_expr = ir_ctx.emit_alloc(&val);
            quote! { Self::#fn_ident(state).map(|__v| #alloc_expr) }
        } else {
            quote! { Self::#fn_ident(state) }
        }
    }

    pub(super) fn emit_inline_wrap_impl(
        &mut self,
        body: TokenStream,
        variant_name: Option<&str>,
        alloc: ValuePlacement,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if let Some(name) = variant_name {
            let enum_ident = &self.enum_ident;
            let variant = format_ident!("{}", name);
            if alloc == ValuePlacement::Alloc {
                let ir_ctx = ctx.ir_ctx();
                let val = quote! { __v };
                let alloc_expr = ir_ctx.emit_alloc(&val);
                quote! {
                    #body.map(|__inner| {
                        let __v = #enum_ident::#variant(__inner);
                        #alloc_expr
                    })
                }
            } else {
                quote! {
                    #body.map(|__v| #enum_ident::#variant(__v))
                }
            }
        } else if alloc == ValuePlacement::Alloc {
            let ir_ctx = ctx.ir_ctx();
            let val = quote! { __v };
            let alloc_expr = ir_ctx.emit_alloc(&val);
            quote! { #body.map(|__v| #alloc_expr) }
        } else {
            body
        }
    }

    pub(super) fn emit_skip_impl(
        &mut self,
        kept: TokenStream,
        discarded: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Tranche AA.8 — `match` instead of let-else because child
        // outputs can be block-shaped (Rust's let-else forbids a
        // brace-terminated RHS).
        quote! {
            'skip_blk: {
                let __kept = match #kept {
                    Some(__v) => __v,
                    None => break 'skip_blk None,
                };
                if #discarded.is_none() { break 'skip_blk None; }
                Some(__kept)
            }
        }
    }


    pub(super) fn emit_next_impl(
        &mut self,
        discarded: TokenStream,
        kept: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Tranche AA.8 — labeled block for `?` short-circuit.
        quote! {
            'next_blk: {
                if #discarded.is_none() { break 'next_blk None; }
                #kept
            }
        }
    }

    pub(super) fn emit_minus_impl(
        &mut self,
        lhs: TokenStream,
        rhs: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        quote! {
            {
                let __save_minus = state.offset;
                let __excluded = #rhs;
                state.offset = __save_minus;
                if __excluded.is_some() {
                    None
                } else {
                    #lhs
                }
            }
        }
    }

    pub(super) fn emit_negate_impl(
        &mut self,
        inner: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        quote! {
            {
                let __save_neg = state.offset;
                let __inner = #inner;
                state.offset = __save_neg;
                if __inner.is_some() {
                    None
                } else {
                    Some(())
                }
            }
        }
    }
}
