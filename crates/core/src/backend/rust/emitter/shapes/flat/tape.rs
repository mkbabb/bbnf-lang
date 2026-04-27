//! Tape-path per-position emission for the Flat shape.
//!
//! [`emit_tape_positions`] threads each position's
//! leading/trailing-whitespace skip around the record-producing core
//! [`emit_tape_position_core`], which dispatches per `IrNode`
//! variant: Literal / Ref / Alt / Regex / Negate / Minus /
//! TokenDispatch / Repeat / Seq / Next / Skip / Map /
//! OptionalWhitespace / Epsilon. The core delegates inline-position
//! emission ([`super::super::inline::emit_inline_position_tape`]) for
//! Alt / Regex / Negate / Minus / TokenDispatch and the
//! [`super::map_regex_host`] specialiser for `Map { Regex, host-fn }`.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::dispatcher::emit_ref_call_tape;
use super::map_regex_host::emit_map_regex_host_fn;
use super::typed_payload::{
    alt_branches_carry_typed_payloads, emit_alt_typed_payload_tape,
};
use super::PositionedNode;

/// Emit the full tape-path body for all positions.
pub(super) fn emit_tape_positions(
    positions: &[PositionedNode],
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut emissions = Vec::with_capacity(positions.len());
    for pos in positions {
        let leading = if pos.leading_ws {
            quote! { let _ = #support_mod::skip_space(input, p, state); }
        } else {
            quote! {}
        };
        let trailing = if pos.trailing_ws {
            quote! { let _ = #support_mod::skip_space(input, p, state); }
        } else {
            quote! {}
        };
        let core = emit_tape_position_core(
            pos.node,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        );
        emissions.push(quote! {
            {
                #leading
                #core
                #trailing
            }
        });
    }
    quote! { #(#emissions)* }
}

/// Emit the record-producing core for one position (ws-handling lives
/// on the caller).
pub(super) fn emit_tape_position_core(
    node: &IrNode,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AW-V.W5.2 — per-Ref routing. Extract grammar_suffix from the
    // support_mod ident ("__shape_support_<grammar>").
    let grammar_suffix = support_mod
        .to_string()
        .strip_prefix("__shape_support_")
        .unwrap_or("")
        .to_string();
    match node {
        IrNode::Literal(sid) => emit_literal_match(*sid, variant_idx, ir),
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing. Resolve the target's
            // shape at codegen time and emit a direct call.
            if let Some(call) = emit_ref_call_tape(&grammar_suffix, *rid, ir) {
                quote! { let _ = (#call)?; }
            } else {
                // Unclassified target — fall back to the dispatcher (which
                // will resolve to __value's Alt dispatch if Alt-rooted, or
                // trip the admission gate otherwise).
                quote! {
                    let _ = #dispatcher_ident(input, p, state, builder)?;
                }
            }
        }
        IrNode::Alt(branches, _)
            if alt_branches_carry_typed_payloads(branches, ir) =>
        {
            // AX.W0a.2.p — Alt position whose branches each carry a
            // `Map { inner: Literal|Seq-literal-chain, IntLit|BoolLit }`
            // annotation. The default inline emission (inline.rs
            // `emit_alt_branch_body_tape`) strips Map wrappers and
            // pushes Literal leaves with `push_leaf(Literal, ..., 0, 0)`
            // — payload LOST. Emit here instead with
            // `push_leaf_with_arena_payload` so the per-branch byte
            // discriminant reaches the tape.
            //
            // Canonical source: Sheets `error_literal` post-factoring
            // (`Seq(Literal("#"), Alt(Map{"N/A",0u8}, Map{"VALUE!",1u8},
            // …))`), and structurally-analogous CSS `keyframeStop`.
            let _ = dispatcher_ident;
            emit_alt_typed_payload_tape(branches, support_mod, &grammar_suffix, ir)
        }
        IrNode::Alt(_, _) | IrNode::Regex(_)
        | IrNode::Negate(_) | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            // AX.W0a.2.e — inline-position emission. Walker-parity
            // byte-dispatch for inline Alt, regex-scan adapter for
            // inline Regex, guard-only for Negate / Minus, dedicated
            // compound for TokenDispatch. No recursion into
            // `#dispatcher_ident` — for non-Alt-rooted grammars the
            // dispatcher IS the root shape fn and would loop.
            let _ = dispatcher_ident;
            super::super::inline::emit_inline_position_tape(
                node, variant_idx, support_mod, &grammar_suffix, ir,
            )
        }
        IrNode::Repeat { inner, lo, hi } => emit_tape_repeat(
            inner,
            *lo,
            *hi,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        IrNode::Seq(children) => {
            let inner = emit_tape_seq_children(
                children,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let seq_lo = *p as u32;
                // B5.W6 — bracket the post-order children scope so
                // child records stamp `frame_depth` at the correct
                // (parent + 1) depth at push time. The matching
                // `end_compound_post_order` absorbs the bracket bump.
                //
                // B5.W6b — IIFE-wrap so `?`-propagation from nested
                // ref / regex / dispatcher calls cannot bypass
                // `end_compound_post_order`. The Err-arm rolls back
                // partial pushes BEFORE exit (Order B): the rollback
                // restores `current_depth` to the bracket-bumped depth
                // (via `frame_depth[__seq_save]`), then
                // `exit_post_order_children` decrements once to the
                // outer frame.
                let __seq_save = builder.position();
                let seq_child = builder.enter_post_order_children();
                let __seq_body: ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
                > = (|| {
                    #inner
                    Ok(())
                })();
                if let ::core::result::Result::Err(__err) = __seq_body {
                    builder.rollback_to(__seq_save);
                    builder.exit_post_order_children();
                    return ::core::result::Result::Err(__err);
                }
                let seq_hi = *p as u32;
                let __seq_off = builder.begin_compound_post(
                    crate::runtime::tape::TapeKind::Seq,
                    seq_lo,
                    0u8,
                    0u8,
                    0u16,
                );
                builder.end_compound_post_order(
                    __seq_off,
                    seq_hi,
                    crate::runtime::tape::TapeOffset(seq_child),
                );
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_tape_position_core(
                lhs,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let r = emit_tape_position_core(
                rhs,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! { #l #r }
        }
        IrNode::Map { inner, fn_id } => {
            // AX.W0a.2.p — detect `Map { Regex, Expr { expr: FnCall,
            // return_type: U32 } }` (CSS `hex` host-fn pattern) and
            // emit a regex-scan + host fn call + arena-payload push
            // so the u32 value reaches the tape via
            // `push_leaf_with_arena_payload`. Other Map shapes retain
            // the transparent unwrap for structural emission.
            if let Some(emission) = emit_map_regex_host_fn(
                inner,
                *fn_id,
                variant_idx,
                rule_id,
                &grammar_suffix,
                ir,
            ) {
                emission
            } else {
                emit_tape_position_core(
                    inner,
                    variant_idx,
                    rule_id,
                    support_mod,
                    dispatcher_ident,
                    ir,
                )
            }
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_tape_position_core(
                inner,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner_emit
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Epsilon => quote! {},
    }
}

/// Emit a Seq's children inline (no outer compound).
fn emit_tape_seq_children(
    children: &[IrNode],
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut out = Vec::with_capacity(children.len());
    for child in children {
        out.push(emit_tape_position_core(
            child,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ));
    }
    quote! { #(#out)* }
}

/// Emit a Repeat position. Two canonical sub-shapes per H1 audit:
///
/// - `Repeat { lo = 0, hi = 1, inner }` — optional. Route through the
///   dispatcher; rollback handled at the dispatcher's boundary by the
///   walker fallback.
/// - Any other repeat — iterate by dispatching the inner in a loop;
///   bail when the dispatcher returns an error.
///
/// Walker-parity outer compound: a `Rule` compound bracketing the
/// Repeat span.
fn emit_tape_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AX.W0a.2.g — walker-parity iter-Seq flattening. Walker's
    // `IrState::Repeat { inner: Seq }` lowering transitions directly
    // to the Seq's inner states after pushing the Repeat frame (Rule),
    // and the Seq state's compound push IS the iter Seq — no extra
    // wrapper. When the Repeat's inner is itself a Seq, emit the
    // children directly inside the iter-Seq push, not a nested Seq
    // wrapping another Seq. For non-Seq inners (Ref, Alt, Literal,
    // Regex, etc.) the iter-Seq is the only walker-pushed Seq, so
    // emission is unchanged.
    let inner_emit = match inner {
        IrNode::Seq(children) => emit_tape_seq_children(
            children,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        _ => emit_tape_position_core(
            inner,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
    };
    let lo_lit = lo as usize;

    if hi == 1 && lo == 0 {
        // Optional. AX.W0a.2.h — wrap the inner in an attempt so
        // failure silently rolls back `*p` + tape columns instead
        // of propagating to the enclosing shape fn. Matches walker's
        // `lo==0` optional semantics.
        //
        // AY-II.W0.b — truncate→rollback_to on iter failure; per-iter
        // Seq and outer Repeat compounds use begin_compound/end_compound.
        //
        // B5.W6 — outer Repeat opens its post-order bracket UNCONDITIONALLY
        // around the iteration; the per-iter Seq opens its own bracket
        // inside the attempt and either closes it via
        // `end_compound_post_order` on success, or `exit_post_order_children`
        // (paired with `rollback_to`) on failure. The depth columns
        // become correct at push time without any retroactive cascade.
        let _ = lo_lit;
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let iter_save_p = *p;
            let iter_save_cols = builder.position();
            let iter_lo = *p as u32;
            let iter_child = builder.enter_post_order_children();
            let opt_attempt: ::core::result::Result<(), crate::runtime::tape::DtaError> =
                (|| {
                    #inner_emit
                    Ok(())
                })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.rollback_to(iter_save_cols);
                builder.exit_post_order_children();
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder.begin_compound_post(
                    crate::runtime::tape::TapeKind::Seq,
                    iter_lo,
                    0u8,
                    0u8,
                    0u16,
                );
                builder.end_compound_post_order(
                    __iter_off,
                    iter_hi,
                    crate::runtime::tape::TapeOffset(iter_child),
                );
            }
            let repeat_hi = *p as u32;
            // AX.W0a.2.j — `TapeKind::Repeat` (not `Rule`) so
            // downstream IR-lowering's `iter_rep_children` peels it.
            let __repeat_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Repeat,
                repeat_lo,
                0u8,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __repeat_off,
                repeat_hi,
                crate::runtime::tape::TapeOffset(repeat_child),
            );
        }
    } else {
        // Generic repeat. Iterate greedily.
        //
        // AX.W0a.2.g — column rollback on iter failure + zero-width
        // break. Walker's `handle_repeat_failure_bounded` rolls back
        // `columns` to the iter's savepoint on Err or zero-width
        // success.
        //
        // AY-II.W0.b — truncate→rollback_to across the 2 rollback sites
        // + per-iter Seq + outer Repeat compounds on begin/end_compound.
        //
        // B5.W6 — outer Repeat opens its post-order bracket once at
        // the loop entry; each iteration opens a per-iter Seq
        // bracket; on iter failure the bracket closes via
        // `exit_post_order_children` paired with `rollback_to` so the
        // depth counter mirrors the structural rewind.
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.enter_post_order_children();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.position();
                let iter_lo = *p as u32;
                let iter_child = builder.enter_post_order_children();
                let attempt = (|| -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
                    #inner_emit
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                // Protect against non-progressing iterations.
                if *p == save_p {
                    builder.rollback_to(save_cols);
                    builder.exit_post_order_children();
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder.begin_compound_post(
                    crate::runtime::tape::TapeKind::Seq,
                    iter_lo,
                    0u8,
                    0u8,
                    0u16,
                );
                builder.end_compound_post_order(
                    __iter_off,
                    iter_hi,
                    crate::runtime::tape::TapeOffset(iter_child),
                );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (#lo_lit as u32) {
                // B5.W6 — close the outer Repeat bracket before
                // returning the error so `current_depth` reflects the
                // outer frame.
                builder.exit_post_order_children();
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Repeat,
                repeat_lo,
                0u8,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                __repeat_off,
                repeat_hi,
                crate::runtime::tape::TapeOffset(repeat_child),
            );
        }
    }
}

/// Emit a byte-sequence literal match + Literal leaf push.
fn emit_literal_match(
    sid: u32,
    variant_idx: u8,
    ir: &GrammarIR,
) -> TokenStream {
    let bytes = ir.get_string(sid).as_bytes();
    let len = bytes.len();
    let byte_lits: Vec<TokenStream> =
        bytes.iter().map(|b| quote! { #b }).collect();
    quote! {
        let at = *p;
        let end = at + #len;
        if input.len() < end || input[at..end] != [#(#byte_lits),*] {
            return Err(crate::runtime::tape::DtaError::Syntax {
                offset: at as u32,
                failing_state: crate::runtime::tape::DtaStateId::NONE,
                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        *p = end;
        let _ = builder.push_leaf_with(
            crate::runtime::tape::TapeKind::Literal,
            at as u32,
            end as u32,
            #variant_idx,
            0,
            crate::runtime::tape::PayloadData::None,
        );
    }
}
