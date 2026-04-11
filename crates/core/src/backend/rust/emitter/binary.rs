//! Binary-op + reference emission for the Rust backend under
//! Tranche AC.2 tape-first: `emit_call`, `emit_inline_wrap`,
//! `emit_skip`, `emit_next`, `emit_minus`, `emit_negate`.
//!
//! Rule calls (`emit_call` / `emit_inline_wrap`) evaluate to
//! `Option<TapeOffset>` — they dispatch to the target rule's
//! `__rule(state, tape)` function which pushes its own tape
//! record. Side-effecting ops (`emit_skip`, `emit_next`,
//! `emit_minus`, `emit_negate`) return `Option<()>`. The
//! uniform composition pattern `match (#sub) { Some(_) => (), ... }`
//! accepts either.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::ValuePlacement;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_call_impl(
        &mut self,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // `alloc` is ignored — tape-first has no slab allocation
        // step. The rule function is `__rule(state, tape) ->
        // Option<TapeOffset>`; we discard the returned offset
        // because the rule already pushed its own record into the
        // parent's children run via `mark_children`. Normalizing
        // to `Option<()>` keeps every sub-expression's type
        // uniform so the match-arm short-circuit pattern in Seq /
        // Alt / Repeat composes cleanly.
        let fn_ident = format_ident!("__{}", rule_name);
        quote! { Self::#fn_ident(state, tape).map(|_| ()) }
    }

    pub(super) fn emit_inline_wrap_impl(
        &mut self,
        body: TokenStream,
        _variant_name: Option<&str>,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Variant wrapping used to produce `Enum::Variant(inner)`.
        // Under tape-first the variant discriminator travels in the
        // tape record's `variant_idx` byte, written by the owning
        // rule's epilogue. The inline wrap is a pass-through.
        body
    }

    pub(super) fn emit_skip_impl(
        &mut self,
        kept: TokenStream,
        discarded: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Skip: `kept << discarded` — evaluate kept (must succeed),
        // then evaluate discarded (must succeed), return ().
        quote! {
            'skip_blk: {
                match (#kept) {
                    Some(_) => (),
                    None => break 'skip_blk None,
                }
                match (#discarded) {
                    Some(_) => (),
                    None => break 'skip_blk None,
                }
                Some(())
            }
        }
    }

    pub(super) fn emit_next_impl(
        &mut self,
        discarded: TokenStream,
        kept: TokenStream,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Next: `discarded >> kept` — evaluate discarded (must
        // succeed), then evaluate kept. Return whatever kept
        // returned (which under tape-first may be either () or a
        // TapeOffset).
        quote! {
            'next_blk: {
                match (#discarded) {
                    Some(_) => (),
                    None => break 'next_blk None,
                }
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
        // Minus: `lhs - rhs`. Try rhs at the current position
        // (with rollback); if it succeeds, fail. Otherwise commit
        // to lhs.
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
        // Negative lookahead: try inner at the current position
        // (with rollback); succeed iff inner fails. No tape side
        // effect on the success path.
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
