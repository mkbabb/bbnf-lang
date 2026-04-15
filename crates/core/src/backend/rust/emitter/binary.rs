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

use bbnf_ir::RuleId;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::ValuePlacement;

use super::{RustEmitCtx, RustEmitter};

impl RustEmitter {
    pub(super) fn emit_call_impl(
        &mut self,
        _rule_id: RuleId,
        rule_name: &str,
        _alloc: ValuePlacement,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Tape call. The rule function is
        // `__rule(state, tape) -> Option<TapeOffset>`; the offset
        // composes into every enclosing Seq / Alt / rule-body match
        // site via `Some(_)` — so AU.6.5 no-value-discard drops the
        // historical `.map(|_| ())` wrap and emits the natural
        // `Option<TapeOffset>` directly. Alt emitters unify
        // heterogeneous arm types via an `is_some()` probe at the
        // label break, not at each call site.
        let fn_ident = format_ident!("__{}", rule_name);
        quote! { Self::#fn_ident(state, tape) }
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
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Skip: `kept << discarded` — evaluate kept (must succeed),
        // then evaluate discarded (must succeed), return ().
        //
        // `fresh_lifetime` threads a per-rule counter through the
        // emitter so nested Skip / Next / Seq blocks get unique
        // block labels — avoids the `derive(Parser)` `label name
        // shadows a label name that is already in scope` warning and
        // keeps LLVM's block-level optimisations unambiguous.
        let skip_blk = ctx.fresh_lifetime("skip_blk");
        quote! {
            #skip_blk: {
                match (#kept) {
                    Some(_) => (),
                    None => break #skip_blk None,
                }
                match (#discarded) {
                    Some(_) => (),
                    None => break #skip_blk None,
                }
                Some(())
            }
        }
    }

    pub(super) fn emit_next_impl(
        &mut self,
        discarded: TokenStream,
        kept: TokenStream,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        // Next: `discarded >> kept` — evaluate discarded (must
        // succeed), then evaluate kept.
        //
        // AU.6.5 no-value-discard: `#kept` can legitimately be
        // `Option<TapeOffset>` / `Option<()>` / `Option<Span>`. The
        // labeled block yields uniform `Option<()>` via an
        // `is_some()` probe so callers in heterogeneous Alt / Seq
        // contexts compose without a `.map(|_| ())` wrap at each
        // nested site.
        let next_blk = ctx.fresh_lifetime("next_blk");
        quote! {
            #next_blk: {
                match (#discarded) {
                    Some(_) => (),
                    None => break #next_blk None,
                }
                if #kept.is_some() { Some(()) } else { None }
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
