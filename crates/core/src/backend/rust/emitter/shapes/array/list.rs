//! Shape-2 emitter — entry-rule list with no bracket wrap.
//!
//! Admitted bodies (after inlining) carry one of two structural shapes:
//!
//! 1. **Direct Repeat** — `rule.body = Repeat { inner, lo, hi }`.
//!    Walker pushes one Rule compound (the Repeat frame); emission
//!    matches.
//! 2. **OW-wrapped Repeat** — `rule.body = OptionalWhitespace(Repeat)`.
//!    Walker lowers OW to `Seq[WsTrim, Repeat state, WsTrim]` pushing
//!    an outer Seq compound whose single meaningful child is the Rule
//!    compound (WsTrim emits no record). Emission matches: outer Seq
//!    compound with leading/trailing ws skips and the Repeat Rule
//!    inside.
//!
//! Per iteration: if the Repeat's inner is itself an
//! `OptionalWhitespace(value)` the walker pushes a per-iter Seq
//! compound (from the inner OW's Seq lowering) with a single
//! meaningful child from the value dispatch. Emission matches. If the
//! Repeat's inner is a bare value with no OW wrapper, no per-iter
//! compound is pushed — the iteration produces the value records
//! directly as children of the Rule compound.
//!
//! Termination: the loop exits when `input.get(*p)` is out of the
//! Repeat-inner's first-set. The value's dispatcher fn rejects at
//! its own byte-dispatch (the same hook the Shape 1 inner uses),
//! which rolls back the iter's savepoint and closes the Rule
//! compound.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{dispatcher_fn_ident, shape_fn_ident};
use super::super::root_rule_name;
use super::element::emit_element_position_tape;

pub(super) fn emit_parse_array_list(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Non-root dispatcher fallback — used when the value-position Ref
    // target can't be classified at emit time. Shape 2 entry rules
    // always have a classified target under the predicate that admits
    // them; the fallback closes the emission contract for completeness.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Pattern-match the body structure. The rule body is either a
    // `Repeat` directly (BBNF grammar) or an `OptionalWhitespace`
    // wrapping a `Repeat` (CSS stylesheet post-inline).
    let (has_outer_ow, repeat_inner) = match &rule.body {
        IrNode::OptionalWhitespace(inner) => match inner.as_ref() {
            IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
            _ => return quote! {},
        },
        IrNode::Repeat { inner, .. } => (false, inner.as_ref()),
        IrNode::Map { inner, .. } => match inner.as_ref() {
            IrNode::OptionalWhitespace(ow_inner) => match ow_inner.as_ref() {
                IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
                _ => return quote! {},
            },
            IrNode::Repeat { inner: r_inner, .. } => (false, r_inner.as_ref()),
            _ => return quote! {},
        },
        _ => return quote! {},
    };

    // Does the Repeat's inner element lower to an `IrState::Seq` (and
    // therefore push a per-iter Seq compound in the walker)? Walker's
    // Seq arm fires for `IrNode::Seq`, `IrNode::OptionalWhitespace`
    // (lowers to `Seq[WsTrim, inner, WsTrim]`), and `IrNode::Next` /
    // `IrNode::Skip` (lower to 2-child Seq). Bare `Ref` / `Literal` /
    // `Regex` inners don't lower to Seq — walker pushes only the
    // bare element's own records.
    //
    // BBNF / CSS use `OW(Ref)` inside the Repeat (→ per-iter Seq);
    // EBNF uses `Seq[Regex, Ref, Regex]` inside the Repeat (→ per-iter
    // Seq); pure `Repeat(Ref)` grammars (if any) would skip the
    // per-iter compound.
    let has_iter_ow = matches!(
        repeat_inner,
        IrNode::OptionalWhitespace(_)
            | IrNode::Seq(_)
            | IrNode::Next(_, _)
            | IrNode::Skip(_, _),
    );

    // AX.W0a.2.f — structural element emission. Walk `repeat_inner`
    // (or its OW-unwrapped inner when `has_iter_ow`) and emit position-
    // specific code: Ref → `emit_ref_call_tape`; Alt/Regex/Negate/
    // Minus/TokenDispatch → `inline::emit_inline_position_tape`; Seq /
    // Next / Skip → recurse; Literal → byte-match. Prior implementation
    // `extract_element_ref` collapsed the element to its first Ref and
    // fell back to `#dispatcher_ident` for Alts (BBNF
    // `Repeat(OW(Alt[Ref,…]))`) — the fallback called the root's
    // `__value` dispatcher, which on non-Alt-rooted grammars IS the
    // root shape fn and loops indefinitely. Direct structural emission
    // eliminates the recursive edge.
    let inner_to_emit = if has_iter_ow {
        match repeat_inner {
            IrNode::OptionalWhitespace(inner) => inner.as_ref(),
            _ => repeat_inner,
        }
    } else {
        repeat_inner
    };
    let _ = dispatcher_ident;
    let value_call = emit_element_position_tape(
        inner_to_emit,
        variant_idx,
        &support_mod,
        grammar_suffix,
        ir,
    );

    // First-set byte check for termination. When the dispatcher's byte
    // dispatch would reject the current `*p`, the Repeat closes. Today
    // this is a structural guard on end-of-input; the per-Ref
    // dispatcher carries its own byte check that rolls back the iter
    // savepoint and surfaces the failure. The outer loop intercepts
    // that failure via a try-iter pattern below.
    //
    // For robustness under walker parity: capture `*p` before each
    // iter, call the value, and on iter-body failure roll back `*p`
    // and exit the loop. The walker's `handle_repeat_failure` is
    // functionally identical.

    // Per-iter body. Two structural variants depending on whether the
    // Repeat's inner carries an OW wrapper:
    //
    // - `has_iter_ow == true` — push per-iter Seq compound; leading
    //   WsTrim (silent skip_space); value dispatch; trailing WsTrim;
    //   close Seq.
    // - `has_iter_ow == false` — no per-iter compound; just the value
    //   dispatch. The walker emits no Seq for a bare Ref inside
    //   Repeat.
    let iter_body = if has_iter_ow {
        quote! {
            // AY-II.W0.b — pre-order open for per-iter Seq. The enclosing
            // outer loop wraps this body in a retry closure; on failure
            // it calls `builder.rollback_to(__iter_save_cols)` to
            // discard any records this iter partially emitted (atomic
            // unwind across tape + value substrates per B4.W1).
            let iter_open = *p as u32;
            let iter_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                iter_open,
                0,
                0u8,
                0u16,
            );

            // Leading WsTrim (silent — emits no record).
            let _ = #support_mod::skip_space(input, p, state);

            // Value dispatch. Failure surfaces through `?` and unwinds
            // to the retry closure; the outer loop calls
            // `builder.rollback_to(...)` (B4.W1 fused-substrate path)
            // so the partially-opened Seq compound leaves no trace.
            #value_call

            // Trailing WsTrim.
            let _ = #support_mod::skip_space(input, p, state);

            let iter_close = *p as u32;
            builder.end_compound(iter_off, iter_close);
        }
    } else {
        quote! {
            #value_call
        }
    };

    // Outer-OW wrap emission. When `rule.body` is `OW(Repeat)`, the
    // walker pushes an outer Seq compound (from OW's
    // `Seq[WsTrim, Repeat, WsTrim]` lowering) wrapping the Rule
    // compound. Emission matches: outer Seq open; leading ws skip;
    // Rule compound with the iterations; trailing ws skip; close
    // outer Seq.
    //
    // Variant_idx on the outer compound comes from the rule id — the
    // rule's variant stamp propagates through the OW's Seq (walker
    // parity).
    if has_outer_ow {
        quote! {
            /// AX.W0a.2.a — per-grammar Array-shape parse function
            /// (Shape 2 — OW-wrapped entry-rule list,
            /// **walker-tape-identical**).
            ///
            /// Emits `Seq[Rule]` matching the walker's lowering of
            /// `OptionalWhitespace(Repeat(...))` where the outer Seq
            /// carries the rule's variant stamp and the inner Rule
            /// compound carries the per-iteration children.
            ///
            /// AX.W0a.2.f — compound; plain `#[inline]`.
            #[inline]
            #[allow(non_snake_case, clippy::too_many_arguments)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut crate::runtime::tape::Tape<()>,
            ) -> ::core::result::Result<
                crate::runtime::tape::TapeOffset,
                crate::runtime::tape::DtaError,
            > {
                let span_lo = *p as u32;

                // AY-II.W0.b — outer Seq compound (OW lowering:
                // Seq[WsTrim, Repeat, WsTrim]); pre-order open.
                let outer_off = builder.begin_compound(
                    crate::runtime::tape::TapeKind::Seq,
                    span_lo,
                    // AX.W0a.2.g — entry-rule OW-Seq stamps variant=0.
                    0u8,
                    0u8,
                    0u16,
                );

                // Leading WsTrim (silent).
                let _ = #support_mod::skip_space(input, p, state);

                // Rule compound (the Repeat frame) — pre-order open.
                let repeat_open = *p as u32;
                let repeat_off = builder.begin_compound(
                    crate::runtime::tape::TapeKind::Rule,
                    repeat_open,
                    0,
                    0u8,
                    0u16,
                );

                // Iterate until the value dispatcher rejects or EOF.
                loop {
                    let iter_save_p = *p;
                    let __iter_save_cols = builder.position();
                    // Peek the current byte; EOF or a byte outside
                    // the dispatcher's first set terminates the loop.
                    // The dispatcher's own byte-dispatch performs the
                    // first-set check; on reject it returns Err which
                    // we intercept below.
                    if input.get(*p).is_none() {
                        break;
                    }
                    // Attempt one iteration. On success, continue;
                    // on failure, roll back `*p` + the fused builder
                    // (tape + value substrates) atomically via
                    // `builder.rollback_to`.
                    let iter_result: ::core::result::Result<(), crate::runtime::tape::DtaError>
                        = (|| {
                            #iter_body
                            Ok(())
                        })();
                    match iter_result {
                        Ok(()) => {
                            // Guard against zero-width iteration
                            // (infinite loop protection): if `*p` did
                            // not advance, treat as terminator.
                            if *p == iter_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = iter_save_p;
                            builder.rollback_to(__iter_save_cols);
                            break;
                        }
                    }
                }

                let repeat_close = *p as u32;
                builder.end_compound(repeat_off, repeat_close);

                // Trailing WsTrim (silent).
                let _ = #support_mod::skip_space(input, p, state);

                let outer_close = *p as u32;
                builder.end_compound(outer_off, outer_close);
                Ok(crate::runtime::tape::TapeOffset(outer_off))
            }
        }
    } else {
        quote! {
            /// AX.W0a.2.a — per-grammar Array-shape parse function
            /// (Shape 2 — direct-Repeat entry-rule list,
            /// **walker-tape-identical**).
            ///
            /// Emits a single Rule compound (the Repeat frame)
            /// carrying the per-iteration children. Matches the
            /// walker's direct lowering of a `Repeat { .. }` body
            /// where the rule's variant stamp lands on the Rule
            /// compound itself (no outer Seq wrapper).
            ///
            /// AX.W0a.2.f — compound; plain `#[inline]`.
            #[inline]
            #[allow(non_snake_case, clippy::too_many_arguments)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut crate::runtime::tape::Tape<()>,
            ) -> ::core::result::Result<
                crate::runtime::tape::TapeOffset,
                crate::runtime::tape::DtaError,
            > {
                let repeat_open = *p as u32;
                // AY-II.W0.b — pre-order Rule compound. Shape 2 direct-
                // Repeat stamps variant=0 per AX.W0a.2.g (entry-only).
                let repeat_off = builder.begin_compound(
                    crate::runtime::tape::TapeKind::Rule,
                    repeat_open,
                    0u8,
                    0u8,
                    0u16,
                );

                loop {
                    let iter_save_p = *p;
                    let __iter_save_cols = builder.position();
                    if input.get(*p).is_none() {
                        break;
                    }
                    let iter_result: ::core::result::Result<(), crate::runtime::tape::DtaError>
                        = (|| {
                            #iter_body
                            Ok(())
                        })();
                    match iter_result {
                        Ok(()) => {
                            if *p == iter_save_p {
                                break;
                            }
                        }
                        Err(_) => {
                            *p = iter_save_p;
                            builder.rollback_to(__iter_save_cols);
                            break;
                        }
                    }
                }

                let repeat_close = *p as u32;
                builder.end_compound(repeat_off, repeat_close);
                Ok(crate::runtime::tape::TapeOffset(repeat_off))
            }
        }
    }
}
