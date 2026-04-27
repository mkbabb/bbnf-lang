//! Shape-1 emitter — wrapped homogeneous repeat (canonical JSON `array`).
//!
//! Body shape: `"[" >> ((value << comma?)*)?w << "]"` with concrete
//! single-byte open/close literals. Emits the nested Seq/Seq/Repeat/Seq
//! compound tree the walker produces, with bracket literals as Literal
//! leaves and per-iteration Seq compounds containing the value records
//! and the comma-Repeat sub-compound.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{dispatcher_fn_ident, emit_ref_call_tape, shape_fn_ident};
use super::super::root_rule_name;
use super::element::extract_array_value_ref;

pub(super) fn emit_parse_array_wrapped(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Non-root dispatcher — the shape fn the value position recurses
    // into. Both root and non-root call sites share the same
    // dispatcher in the walker-parity emission (the walker's value
    // ByteDispatch is identical at root and nested positions).
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve the value-position Ref from the array body
    // (`"[" >> ((value << comma?)*)?w << "]"` → the `value` Ref).
    let value_ref = extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _value_off = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _value_off = #dispatcher_ident(input, p, state, builder)?;
            }
        });

    quote! {
        /// AW-V.W3.2 — per-grammar Array-shape parse function,
        /// **walker-tape-identical**.
        ///
        /// Emits the same nested Seq/Seq/Repeat/Seq compound tree the
        /// walker produces for the canonical JSON array rule. The
        /// record tree is navigated by every downstream view derive
        /// and the `tape_parity` golden fixtures; only dispatch is
        /// inlined relative to the walker, not the record stream.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale (the array ↔ value cycle).
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
            if input.get(*p).copied() != Some(b'[') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }

            // AY.W5.2 — pre-order emission via open_compound. Every
            // compound this shape emits lands write-time with
            // `close_compound` back-patching span_hi / child_off /
            // HAS_CHILDREN on the parent row and stamping each direct
            // child's SIB_SKIP_STAMPED_BIT. Value calls inside the loop
            // either emit leaves or nested pre-order subtrees; both
            // participate in the open-frame's push hook automatically.
            //
            // Outer compound: Skip(Next("[", rest), "]") — the array
            // rule itself. Variant_idx comes from the Ref's pending
            // stamp = rule.id & 0xFF.
            let outer_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                #variant_idx,
                0u8,
                0u16,
            );

            // Inner Next compound: Next("[", OptionalWhitespace(Repeat(...))).
            let lbracket_open = *p as u32;
            let next_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                lbracket_open,
                0,
                0u8,
                0u16,
            );

            // Leaf: "[" Literal (payload-less; TapeKind::Literal per
            // walker). Walker stamps variant_idx from the enclosing
            // rule's `pending_variant_idx` (set by the Ref landing in
            // this rule). Downstream `Leaf::Other` tests assert that
            // every structural literal inside a rule inherits the
            // owning rule's variant.
            *p += 1;
            let bracket_close = *p as u32;
            let _ = builder.push_leaf_with(
                crate::runtime::tape::TapeKind::Literal,
                lbracket_open,
                bracket_close,
                #variant_idx,
                0,
                crate::runtime::tape::PayloadData::None,
            );

            // OptionalWhitespace Seq compound — contains the Repeat.
            let opt_ws_open = *p as u32;
            let opt_ws_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Seq,
                opt_ws_open,
                0,
                0u8,
                0u16,
            );

            // Leading whitespace trim.
            let _ = #support_mod::skip_space(input, p, state);
            let repeat_open = *p as u32;

            // Repeat compound — one per rule invocation, regardless of iterations.
            let repeat_off = builder.begin_compound(
                crate::runtime::tape::TapeKind::Rule,
                repeat_open,
                0,
                0u8,
                0u16,
            );

            // Fast-empty check: `]` immediately closes everything.
            let maybe_close = input.get(*p).copied();
            if maybe_close == Some(b']') {
                // Close Repeat (no iterations), OptionalWhitespace, Next, outer.
                let repeat_close = *p as u32;
                builder.end_compound(repeat_off, repeat_close);
                let opt_ws_close = *p as u32;
                builder.end_compound(opt_ws_off, opt_ws_close);
                // Consume the ].
                *p += 1;
                let rbracket_lo = opt_ws_close;
                let rbracket_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    rbracket_lo,
                    rbracket_hi,
                    #variant_idx,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
                let next_close = rbracket_hi;
                builder.end_compound(next_off, next_close);
                let outer_close = *p as u32;
                builder.end_compound(outer_off, outer_close);
                return Ok(crate::runtime::tape::TapeOffset(outer_off));
            }

            // Non-empty loop: parse iterations. Each iteration emits:
            //   Seq (iter) { value_records; Rule (comma_repeat) { maybe: Seq { Literal "," } } }
            //
            // Walker-parity decomposition of `(value << comma?) *`:
            //  - Iter body: `Seq[value, Repeat(comma, 0..1)]`. No OW
            //    between `value` and `Repeat(comma)`.
            //  - `comma = "," ?w` lowers to `OptionalWhitespace(Literal(","))`,
            //    which lowers to DTA `Seq[WsTrim, ",", WsTrim]` (leading
            //    + trailing ws absorbed into the Seq's span).
            //  - Repeat(comma, 0..1) fires at most once per iter; when the
            //    "," is absent, the comma iter Seq rolls back via
            //    `handle_repeat_failure` with `*pos = sp.pos` = iter entry.
            //  - Outer Repeat closes when the value Ref's ByteDispatch
            //    rejects `*pos` (e.g. `*pos` at `]` or at ws-before-`]`);
            //    on that absorb, `*pos = sp.pos` = position at which the
            //    last iter body ended (BEFORE the OW-Seq's trailing WsTrim).
            //  - Trailing ws between the last iter and `]` lives in the
            //    OW-Seq (outer OptionalWhitespace(Repeat)) trailing WsTrim,
            //    NOT in the Repeat compound span, NOT in any iter Seq span.
            loop {
                let iter_open = *p as u32;
                let iter_off = builder.begin_compound(
                    crate::runtime::tape::TapeKind::Seq,
                    iter_open,
                    0,
                    0u8,
                    0u16,
                );

                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call

                // comma_repeat Rule — span_lo captured AT `*p` immediately
                // after the value parse, BEFORE any leading ws the
                // comma rule's OW wrapper would consume. The walker's
                // Repeat arm captures `*pos` via `push_compound_fused`
                // at arm entry; there is no OW between `value` and
                // `Repeat(comma)` in the iter-body IR.
                let comma_repeat_open = *p as u32;
                let comma_repeat_off = builder.begin_compound(
                    crate::runtime::tape::TapeKind::Rule,
                    comma_repeat_open,
                    0,
                    0u8,
                    0u16,
                );

                // Attempt one iter of comma_repeat. Iter body is
                // `Seq[leading WsTrim, Literal(","), trailing WsTrim]`.
                // On failure (no `,` after leading ws), handle_repeat_failure
                // rolls back `*pos` to the iter's saved entry.
                let comma_iter_save_p = *p;
                let _ = #support_mod::skip_space(input, p, state);
                let has_comma = input.get(*p).copied() == Some(b',');
                if has_comma {
                    // Comma iter Seq — span_lo at the iter entry (BEFORE
                    // the leading WsTrim), span_hi after the trailing
                    // WsTrim (same shape as the walker's Seq-wrapper for
                    // `OptionalWhitespace(Literal(","))`).
                    let comma_iter_open = comma_iter_save_p as u32;
                    let comma_iter_off = builder.begin_compound(
                        crate::runtime::tape::TapeKind::Seq,
                        comma_iter_open,
                        0,
                        0u8,
                        0u16,
                    );
                    let comma_lo = *p as u32;
                    *p += 1;
                    let comma_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        crate::runtime::tape::TapeKind::Literal,
                        comma_lo,
                        comma_hi,
                        #variant_idx,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
                    // Trailing WsTrim inside the comma Seq.
                    let _ = #support_mod::skip_space(input, p, state);
                    let comma_iter_close = *p as u32;
                    builder.end_compound(comma_iter_off, comma_iter_close);
                } else {
                    // Walker-parity rollback: on comma-iter failure,
                    // handle_repeat_failure restores `*pos = sp.pos` = the
                    // iter's saved entry position. No records have been
                    // pushed — just reset `*p`.
                    *p = comma_iter_save_p;
                }
                let comma_repeat_close = *p as u32;
                builder.end_compound(comma_repeat_off, comma_repeat_close);

                let iter_close = *p as u32;
                builder.end_compound(iter_off, iter_close);

                // Decide continue vs close without consuming ws. The walker's
                // outer Repeat re-enters the value Ref; its ByteDispatch fails
                // when `*pos` is NOT a JSON value-start byte ({ / [ / " / -
                // / 0–9 / t / f / n), triggering handle_repeat_failure which
                // closes Repeat at `*pos = sp.pos` (= `iter_close` captured
                // above). Any trailing ws between the last iter and `]`
                // belongs to the OW-Seq's trailing WsTrim, NOT the Repeat.
                // Direct boolean form rather than `matches!`: nightly's
                // `matches!` expansion decorates the inner `match` with
                // `#[allow(non_exhaustive_omitted_patterns)]` — an
                // attribute on an expression (unstable, E0658) —
                // surfaced by the bootstrap's `cargo expand` step.
                let is_value_start = match input.get(*p).copied() {
                    Some(b'{') | Some(b'[') | Some(b'"') | Some(b'-')
                    | Some(b'0'..=b'9') | Some(b't') | Some(b'f') | Some(b'n') => true,
                    _ => false,
                };
                if !is_value_start {
                    // Close Repeat at current *p (BEFORE OW-Seq trailing ws).
                    let repeat_close = *p as u32;
                    builder.end_compound(repeat_off, repeat_close);
                    // OW-Seq trailing WsTrim: advance past ws.
                    let _ = #support_mod::skip_space(input, p, state);
                    let opt_ws_close = *p as u32;
                    builder.end_compound(opt_ws_off, opt_ws_close);
                    // Expect "]"; anything else (EOF, garbage) is a
                    // well-formed error identical to the walker's path
                    // (Skip's RHS literal mismatches).
                    if input.get(*p).copied() != Some(b']') {
                        return Err(match input.get(*p).copied() {
                            None => crate::runtime::tape::DtaError::UnexpectedEnd {
                                offset: *p as u32,
                            },
                            _ => crate::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state: crate::runtime::tape::DtaStateId::NONE,
                                failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        });
                    }
                    // Consume "]".
                    *p += 1;
                    let rbracket_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        crate::runtime::tape::TapeKind::Literal,
                        opt_ws_close,
                        rbracket_hi,
                        #variant_idx,
                        0,
                        crate::runtime::tape::PayloadData::None,
                    );
                    let next_close = rbracket_hi;
                    builder.end_compound(next_off, next_close);
                    let outer_close = *p as u32;
                    builder.end_compound(outer_off, outer_close);
                    return Ok(crate::runtime::tape::TapeOffset(outer_off));
                }
                // Continue: next iter's dispatcher call handles its own
                // leading ws-skip before byte-dispatching the value.
            }
        }
    }
}
