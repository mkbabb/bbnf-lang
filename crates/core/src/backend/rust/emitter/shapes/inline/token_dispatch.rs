//! `TokenDispatch { token, arms, fallback }` emission — tape and
//! visitor paths.
//!
//! Emits a `TapeKind::TokenDispatch` compound with the token's records
//! followed by the winning arm's continuation (or the fallback on no
//! match).
//!
//! Dispatch semantics follow the VM interpreter:
//!
//! 1. Parse `token` — capture the span via `save_p .. *p`.
//! 2. For each arm, test whether the span's bytes match any of the
//!    arm's `patterns` (each a `StringId` keyword). If `guard_byte`
//!    is set, also require `input[*p] == guard`.
//! 3. On match, emit the arm's continuation.
//! 4. On no arm match, emit the `fallback` continuation.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::guard::{emit_primary_tape, emit_primary_visitor};

/// Emit `TokenDispatch { token, arms, fallback }` as a
/// `TapeKind::TokenDispatch` compound with the token's records
/// followed by the winning arm's continuation (or the fallback on no
/// match).
pub(super) fn emit_token_dispatch_tape(
    token: &IrNode,
    arms: &[bbnf_ir::TokenDispatchArm],
    fallback: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let token_emit = emit_primary_tape(token, variant_idx, support_mod, grammar_suffix, ir);

    let mut per_arm: Vec<TokenStream> = Vec::with_capacity(arms.len());
    for arm in arms {
        let cont = emit_primary_tape(
            &arm.continuation,
            variant_idx,
            support_mod,
            grammar_suffix,
            ir,
        );
        let pattern_literals: Vec<TokenStream> = arm
            .patterns
            .iter()
            .map(|sid| {
                let bytes = ir.get_string(*sid).as_bytes();
                let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
                quote! { &[#(#byte_lits),*][..] }
            })
            .collect();
        let guard_check = if let Some(g) = arm.guard_byte {
            quote! { && input.get(*p).copied() == ::core::option::Option::Some(#g) }
        } else {
            quote! {}
        };
        per_arm.push(quote! {
            if !td_match
                && (#(token_span == #pattern_literals)||*)
                #guard_check
            {
                #cont
                td_match = true;
            }
        });
    }

    let fallback_emit = emit_primary_tape(fallback, variant_idx, support_mod, grammar_suffix, ir);
    let variant_lit = variant_idx;

    quote! {
        {
            let td_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order TokenDispatch
            // compound. Capture first-child index pre-emission;
            // allocate the compound row post-children; override
            // child_off to point back at first-child.
            //
            // B5.W6 — bracket the post-order children scope so child
            // records stamp `frame_depth` at the correct
            // (parent + 1) depth at push time.
            //
            // B5.W6b — IIFE-wrap the token + per-arm + fallback
            // emissions so `?`-propagation from primary-tape sub-emitters
            // (regex `?`, ref calls) cannot bypass
            // `end_compound_post_order`. The Err-arm rolls back partial
            // pushes BEFORE exit (Order B): the rollback restores
            // `current_depth` to the bracket-bumped depth (via
            // `frame_depth[__td_save]`), then `exit_post_order_children`
            // decrements once to the outer frame.
            let __td_save = builder.position();
            let td_child = builder.enter_post_order_children();
            let __post_body: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                let token_lo = *p;
                #token_emit
                let token_span: &[u8] = &input[token_lo..*p];
                let mut td_match = false;
                #(#per_arm)*
                if !td_match {
                    #fallback_emit
                }
                Ok(())
            })();
            if let ::core::result::Result::Err(__err) = __post_body {
                builder.rollback_to(__td_save);
                builder.exit_post_order_children();
                return ::core::result::Result::Err(__err);
            }
            let td_hi = *p as u32;
            let __td_off = builder.begin_compound_post(
                ::tape::TapeKind::TokenDispatch,
                td_lo,
                #variant_lit,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __td_off,
                td_hi,
                ::tape::TapeOffset(td_child),
            );
        }
    }
}

pub(super) fn emit_token_dispatch_visitor(
    token: &IrNode,
    arms: &[bbnf_ir::TokenDispatchArm],
    fallback: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let token_emit = emit_primary_visitor(token, support_mod, grammar_suffix, ir);
    let mut per_arm: Vec<TokenStream> = Vec::with_capacity(arms.len());
    for arm in arms {
        let cont = emit_primary_visitor(&arm.continuation, support_mod, grammar_suffix, ir);
        let pattern_literals: Vec<TokenStream> = arm
            .patterns
            .iter()
            .map(|sid| {
                let bytes = ir.get_string(*sid).as_bytes();
                let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
                quote! { &[#(#byte_lits),*][..] }
            })
            .collect();
        let guard_check = if let Some(g) = arm.guard_byte {
            quote! { && input.get(*p).copied() == ::core::option::Option::Some(#g) }
        } else {
            quote! {}
        };
        per_arm.push(quote! {
            if !td_match
                && (#(token_span == #pattern_literals)||*)
                #guard_check
            {
                #cont
                td_match = true;
            }
        });
    }
    let fallback_emit = emit_primary_visitor(fallback, support_mod, grammar_suffix, ir);
    quote! {
        {
            let token_lo = *p;
            #token_emit
            let token_span: &[u8] = &input[token_lo..*p];
            let mut td_match = false;
            #(#per_arm)*
            if !td_match {
                #fallback_emit
            }
        }
    }
}
