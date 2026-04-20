//! Pratt-shape emitter — `parse_pratt_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Pratt-shape parse functions for operator-chain
//! head rules. The emitted body mirrors the walker's existing
//! `DtaState::ShuntingYard` arm at
//! `tape::driver.rs` — operand dispatch, then a loop that
//! consults [`PRECEDENCE_LUT`](
//! crate::backend::rust::emitter::precedence) to pack / pop / reduce
//! operators until EOF-operator or lower precedence is reached.
//!
//! Unlike the walker's indirected `dispatch_one` arm, the emitted
//! code is a monolithic inline body:
//!
//! - Operand dispatch calls through the per-grammar value-position
//!   dispatcher — the same dispatcher Object / Array shape emitters
//!   re-enter for nested values.
//! - Operator reduction uses the per-grammar `PRECEDENCE_LUT` const
//!   the [`crate::backend::rust::emitter::precedence`] emitter
//!   already lowers.
//! - Compound emission uses [`TapeBuilder::push_compound`] for the
//!   outer Pratt compound and raw column writes for the reducer
//!   inner compounds — mirroring the walker's `emit_reducer_compound`
//!   byte layout so downstream tape consumers see identical records.
//!
//! # Tape shape (for `a + b * c`)
//!
//! ```text
//! [ 0] Rule    (Pratt outer)         span=0..N  child=1  has_children=true
//! [ 1] ...operand 'a' records...
//! [ 2] Span    variant=0 payload='+' span=p..p+1
//! [ 3] ...operand 'b' records...
//! [ 4] Span    variant=0 payload='*' span=q..q+1
//! [ 5] ...operand 'c' records...
//! [ 6] Rule    (reducer b*c)         child=pointer-to-'b'  variant='*'_disc
//! [ 7] Rule    (reducer a+(b*c))     child=pointer-to-'a'  variant='+'_disc
//! ```
//!
//! After reduction the outer Rule compound's `child_off` is patched
//! to point at record `[7]` (the root reducer) — the same layout the
//! walker's `advance_or_pop_with` ShuntingYard arm produces.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Emit `pub fn parse_pratt_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
///
/// The emitted body invokes the grammar's value-position dispatcher
/// for the leftmost operand, then runs a byte-load + bit-unpack over
/// the per-grammar `PRECEDENCE_LUT` — the same table
/// `emit_precedence_lut` lowers — and reduces operators in
/// shunting-yard order. Reducer compounds are post-order column
/// writes mirroring [`bbnf::runtime::tape::emit_reducer_compound`];
/// the outer Pratt compound is patched to point at the final
/// reduced operand on close, matching the walker's byte-for-byte
/// tape layout.
pub fn emit_parse_pratt(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("pratt", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    // AX.W0a.2.l — per-rule Pratt LUT. Each rule's Pratt body
    // consults its own `PRECEDENCE_LUT_<rule>` so cross-rule byte
    // collisions (BBNF: `||` in `value_or` vs `<<` in `binary_factor`)
    // don't leak into one another's dispatch.
    let rule_lut_ident = format_ident!("PRECEDENCE_LUT_{}", rule_name);
    let rule_entries_ident = format_ident!("PRECEDENCE_ENTRIES_{}", rule_name);

    // The per-grammar value-position dispatcher — the operand
    // parses recurse through this so nested calls, parens, numbers,
    // identifiers, function calls all resolve via the existing
    // shape dispatch. The dispatcher exists for any grammar whose
    // root rule passes `has_full_shape_coverage`; for grammars
    // that don't (Sheets, CSS under W4.1 substrate) the emitter
    // returns an empty stream so the gate in `mod.rs` can continue
    // to skip this shape until the consumer wiring (W4.2 / W4.3)
    // lands.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve the operand Ref from the Pratt body.
    // Canonical body shape: `operand (op operand)*` — the first Ref in
    // the body is the operand rule. Used for per-Ref direct calls.
    let operand_ref = extract_first_ref(&rule.body);
    let operand_call = operand_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _operand_off = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _operand_off = #dispatcher_ident(input, p, state, builder)?;
            }
        });
    let rhs_call = operand_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _rhs_off = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _rhs_off = #dispatcher_ident(input, p, state, builder)?;
            }
        });

    quote! {
        /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
        ///
        /// Runs the operand-led shunting-yard reducer bounded by the
        /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
        /// the walker's `DtaState::ShuntingYard` arm — `TapeKind::Rule`
        /// outer compound + per-op reduced binary compounds via
        /// `emit_reducer_compound`.
        ///
        /// # Emitted algorithm
        ///
        /// 1. Reserve an outer Rule compound via
        ///    [`::bbnf::runtime::tape::TapeBuilder::mark_children`] +
        ///    record the parse-open position.
        /// 2. Dispatch the leftmost operand through the grammar's
        ///    value-position dispatcher; the operand's records land
        ///    inside the outer compound's child run.
        /// 3. Loop: peek the next byte; consult `PRECEDENCE_LUT`; when
        ///    zero, break; when nonzero:
        ///    a. Reduce every top-of-op-stack entry whose precedence
        ///       exceeds the new byte's (or ties + left-assoc); each
        ///       reduce emits a `TapeKind::Rule` reducer compound via
        ///       [`::bbnf::runtime::tape::emit_reducer_compound`].
        ///    b. Emit a `TapeKind::Span` op leaf carrying the operator
        ///       byte's u8 discriminant into `pay_narrow` directly via
        ///       `push_leaf_with(InlineScalar)` (AY.W1.4 Pratt Option C
        ///       inline; bypasses the `arena_mut().push` round-trip
        ///       AX.W0a.2.l routed through).
        ///    c. Push the operator onto the local op stack with its
        ///       `(precedence, associativity, lhs_idx, lhs_span_lo)`.
        ///    d. Advance past the op bytes (1 or 2 for two-byte ops).
        ///    e. Re-dispatch the RHS operand.
        /// 4. On EOF-operator: drain the op stack — every remaining
        ///    entry reduces into a terminal compound. The final
        ///    `this_operand_root` is stamped onto the outer Rule
        ///    compound's `child_off` (overriding the default
        ///    `mark_children` index) so the cursor's pre-order walk
        ///    surfaces the reduced tree root as the compound's first
        ///    child.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_assignments, unused_mut, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            // Local op-stack entry — mirrors
            // `::bbnf::runtime::tape::OpStackEntry` but lives on the
            // CPU stack inside this fn so the reducer's LLVM loop fuses
            // the push / pop without a cross-crate call boundary.
            //
            // `precedence` and `associativity_is_left` pack the sparse
            // `PRECEDENCE_ENTRIES` row the LUT byte's bit 7 admits; one
            // indexed load + three shifts per op boundary (the
            // consumer-side unpack lives inline below).
            //
            // AY.W1.4 — initialised via `::core::array::from_fn` for
            // the fixed-size stack array (the bootstrap postprocessor
            // strips inner `#[derive(...)]` attributes that cargo
            // expand emits into unstable internals; `from_fn` is the
            // Copy-free idiom). Layout (after padding): 16 bytes —
            // three 1-byte fields + two `u32`s + 5 padding. 16 entries
            // × 16 bytes = 256 byte stack frame, well below the
            // 8 KiB thread-stack budget the rayon work-stealer honours.
            struct LocalOpEntry {
                op_discriminant: u8,
                precedence: u8,
                associativity_is_left: bool,
                lhs_idx: u32,
                lhs_span_lo: u32,
            }

            // ── Outer Pratt compound ────────────────────────────────
            let _ = #support_mod::skip_space(input, p, state);
            let outer_span_lo = *p as u32;
            let outer_child_mark = builder.mark_children();
            let outer_child_mark_idx: u32 = outer_child_mark.0;

            // ── Leftmost operand ────────────────────────────────────
            // The operand root is the first record the dispatcher
            // emits; track it so the reducer's first reduce points
            // back at the correct LHS. Initial value = child_mark
            // (= index of the first child after the outer compound's
            // reserved slot — the operand's root record lands here).
            let mut this_operand_root: u32 = outer_child_mark_idx;
            // AW-V.W5.2 — per-Ref operand call.
            #operand_call
            // The dispatcher returns the operand's root TapeOffset;
            // the walker's SY arm uses the child_mark directly (the
            // first-child position) rather than the dispatcher's
            // return value because the DTA driver's frame marker sits
            // at child_mark. Preserve that invariant here.
            let _ = _operand_off;

            // ── Op stack ────────────────────────────────────────────
            //
            // AY.W1.4 — fixed-size stack array (`[LocalOpEntry; 16]` +
            // `op_stack_len: usize`). The pre-AY heap `Vec::with_capacity(4)`
            // burned an `alloc::alloc` per Pratt parse; the bench-pressure
            // delta on Sheets `parse_stress` shows up as the
            // `_platform_memset` self-time the W1.4 sub-gate targets.
            //
            // `OP_STACK_CAP = 16` matches the IR-mined max chain depth
            // ceiling across production grammars (Sheets formula: 6
            // rungs / `||` + `<>` + `<` + `+` + `*` + `^`; CSS calc:
            // single-rung Pratt; BBNF `binary_factor` / `value_or`:
            // single-rung). The runtime `debug_assert!` below traps
            // any future grammar that exceeds this ceiling — the
            // emitter caller verifies the bound at codegen via the
            // mined operator-chain facts.
            const OP_STACK_CAP: usize = 16;
            // The `op_stack_len` cursor is the only valid-data witness;
            // reads only touch indices `< op_stack_len`. Zero-init each
            // entry so subsequent loads from
            // unwritten slots remain deterministic in any future debug
            // tooling that ignores the cursor.
            let mut op_stack: [LocalOpEntry; OP_STACK_CAP] =
                ::core::array::from_fn(|_| LocalOpEntry {
                    op_discriminant: 0,
                    precedence: 0,
                    associativity_is_left: false,
                    lhs_idx: 0,
                    lhs_span_lo: 0,
                });
            let mut op_stack_len: usize = 0;

            // ── Reducer loop ────────────────────────────────────────
            //
            // AX.W0a.2.l — per-rule LUT references; reducer-compound
            // emission preserved (`push_compound(TapeKind::Rule, ...,
            // top_op.op_discriminant, 0)` remains inside the reduce
            // step so downstream consumers see the walker-compatible
            // reduced tree).
            //
            // AX.W0a.2.n — Whitespace-aware operator peek. If the
            // byte at `*p` is already a valid operator (LUT byte
            // nonzero), dispatch on it directly — whitespace may
            // BE the operator (CSS combinators `/\s*>\s*/` etc.
            // carry leading whitespace as part of the operator
            // alphabet). If the byte is not an operator, attempt
            // skip_space + re-peek once; operator-chain mining
            // ensures the rule's LUT indexes a disjoint first-byte
            // alphabet so a skipped whitespace byte cannot collide
            // with a non-whitespace operator. This restores
            // json.bbnf's `"[" >> ... << "]"` Pratt chain (the
            // W0a.2.n target) without breaking whitespace-carrying
            // combinator operators.
            loop {
                // Peek next byte; consult the per-rule PRECEDENCE_LUT_<rule>.
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = #rule_lut_ident[op_byte as usize];
                if lut_byte == 0 {
                    let _ = #support_mod::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = #rule_lut_ident[op_byte as usize];
                }
                let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(lut_byte & 0x0Fu8)
                };

                // Reduce op-stack entries whose precedence > new_prec
                // (or ties + left-assoc). On EOF-op (new_prec = None)
                // reduce all remaining entries.
                loop {
                    if op_stack_len == 0 {
                        break;
                    }
                    let top_op = &op_stack[op_stack_len - 1];
                    let should_reduce = match new_prec {
                        ::core::option::Option::None => true,
                        ::core::option::Option::Some(p_new) => {
                            top_op.precedence > p_new
                                || (top_op.precedence == p_new
                                    && top_op.associativity_is_left)
                        }
                    };
                    if !should_reduce {
                        break;
                    }
                    let lhs_idx = top_op.lhs_idx;
                    let lhs_span_lo = top_op.lhs_span_lo;
                    let op_discriminant = top_op.op_discriminant;
                    op_stack_len -= 1;
                    let compound_idx = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        ::bbnf::runtime::tape::TapeOffset(lhs_idx),
                        lhs_span_lo,
                        *p as u32,
                        op_discriminant,
                        0,
                    );
                    this_operand_root = compound_idx.0;
                }

                // If no new op: outer compound closes.
                if lut_byte == 0 {
                    break;
                }

                // Push-op path: unpack LUT byte's (prec, assoc,
                // two_byte) + resolve (op_rule, op_discriminant) from
                // the per-rule PRECEDENCE_ENTRIES_<rule> slice.
                let precedence: u8 = lut_byte & 0x0Fu8;
                let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
                let associativity_is_left: bool = assoc_bit == 0;
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;

                let second_byte: ::core::option::Option<u8> =
                    input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    // Two-byte bit was set, but the specific
                    // (byte, second_byte) pair wasn't in the entries
                    // — fall back to a single-byte entry on the same
                    // first byte.
                    if !matched_two_byte {
                        for e in #rule_entries_ident.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };

                // AX.W0a.2.n — if the LUT byte was nonzero but no
                // concrete entry matched (first byte of a two-byte op
                // alone, e.g. `|` where only `||` is a real operator),
                // the current position isn't actually an operator —
                // break the loop instead of consuming a phantom byte.
                // Pre-W0a.2.n the same code consumed the byte (found_disc
                // = 0, width = 1) and emitted a zero-discriminant op
                // leaf, corrupting the reducer chain. Surface exposed
                // once skip_space lifted the operator peek past trailing
                // whitespace on `value_or`'s single `|` alternation
                // separator.
                if !op_matched {
                    break;
                }

                // Advance past the op bytes + emit a payload-bearing
                // Span leaf carrying the 1-byte op_discriminant.
                //
                // AY.W1.4 (Pratt Option C inline) — direct
                // `push_leaf_with(PayloadData::InlineScalar(disc as u32))`
                // routes the op_discriminant straight into the
                // `pay_narrow` column. Pre-AY.W1.4 the code took a
                // `arena_mut().push(disc)` + `push_leaf_with_arena_payload`
                // detour: one extra arena store + one extra pointer
                // load on the read side, plus the `PAYLOAD_IN_ARENA_BIT`
                // routing branch in `payload_inline<T>`. Inline-scalar
                // reads short-circuit that branch (the bit stays
                // clear) and resolve to a single `pay_narrow[rank]`
                // load. The reducer-compound emission downstream is
                // unchanged — `top_op.op_discriminant` is captured in
                // the LocalOpEntry above and stamped into the
                // `push_compound`'s `variant_idx`, preserving the
                // walker-compatible reduced-tree byte layout that
                // W0a.2.k regressed CSS+Sheets parity by breaking.
                let op_lo: u32 = *p as u32;
                *p = (*p).saturating_add(op_width as usize);
                let op_hi: u32 = *p as u32;
                let _op_rec = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    ::bbnf::runtime::tape::PayloadData::InlineScalar(
                        op_discriminant as u32,
                    ),
                );

                // Capture LHS span_lo for the reducer compound. The
                // walker reads the LHS row's `span_lo` directly via
                // the AoS column accessor (AY.W1.1).
                let lhs_span_lo: u32 = if (this_operand_root as usize)
                    < builder.columns().len()
                {
                    builder.columns().span_lo_at(this_operand_root)
                } else {
                    op_hi
                };

                // AY.W1.4 — overflow guard. Production grammars peak
                // at 6 rungs (Sheets formula); reaching 16 indicates
                // a new grammar whose precedence chain blew past the
                // ceiling — codegen needs to widen `OP_STACK_CAP`.
                debug_assert!(
                    op_stack_len < OP_STACK_CAP,
                    "Pratt op_stack overflow at depth {} (cap {})",
                    op_stack_len,
                    OP_STACK_CAP,
                );
                op_stack[op_stack_len] = LocalOpEntry {
                    op_discriminant,
                    precedence,
                    associativity_is_left,
                    lhs_idx: this_operand_root,
                    lhs_span_lo,
                };
                op_stack_len += 1;

                // ── RHS operand ─────────────────────────────────────
                let _ = #support_mod::skip_space(input, p, state);
                // AW-V.W5.2 — per-Ref RHS call.
                #rhs_call
                // Re-point `this_operand_root` at the RHS root (first
                // record the RHS emitted). The `push_leaf_with` above
                // sits at this_operand_root + 1 when the operand was
                // a single leaf; for compound operands the root sits
                // at the position right after the op leaf.
                //
                // Mirrors the walker's SY reducer: after the op leaf
                // fires, the next operand's first record becomes the
                // new `this_operand_root`.
                // The op leaf incremented columns.len() by 1; the RHS
                // dispatcher then emitted its root starting at that
                // position. Tracking via the builder's post-op record
                // index (arena offset proxy) isn't safe across
                // payload alignment; instead read back the index of
                // the first record AFTER the op leaf.
                this_operand_root = _op_rec.0 + 1;
            }

            // ── Close outer compound ────────────────────────────────
            // Patch the outer Rule compound's `child_off` to point at
            // the final reduced operand root; stamp span_hi + close.
            let outer_span_hi = *p as u32;
            let outer_off = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                ::bbnf::runtime::tape::TapeOffset(this_operand_root),
                outer_span_lo,
                outer_span_hi,
                #variant_idx,
                0,
            );
            Ok(outer_off)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4.1 — visitor-path Pratt emitter.
//
// The visitor-path Pratt emitter invokes the visitor's per-shape
// operator / operand methods in place of tape record pushes.
// `V: PrattVisitor + <operand-shape trait bounds>` composes the
// delegates statically at the call site. The operand dispatch rides
// the grammar's value-position visitor dispatcher (emitted by
// `dispatcher::emit_visitor_dispatcher`), so nested expressions,
// numbers, strings, function calls resolve through the same
// monomorphic trait surface.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_pratt_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_pratt_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("pratt", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    // AX.W0a.2.l — per-rule visitor-path Pratt LUT (mirrors tape-
    // path emitter above). Each visitor body consults its own
    // rule-scoped LUT + sparse entries slice.
    let rule_lut_ident = format_ident!("PRECEDENCE_LUT_{}", rule_name);
    let rule_entries_ident = format_ident!("PRECEDENCE_ENTRIES_{}", rule_name);

    // Visitor-path dispatcher — wires the RHS operand parse into the
    // grammar's per-shape visitor family. When the grammar has no
    // root dispatcher (Sheets' `formula` root is a Seq not an Alt),
    // the visitor emitter returns an empty stream so the compile
    // gate stays on the walker fallback; the tape-path emitter does
    // the same above.
    let visitor_dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — per-Ref operand calls for visitor path.
    let operand_ref = extract_first_ref(&rule.body);
    let operand_call = operand_ref
        .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #visitor_dispatcher_ident(input, p, state, visitor)?;
            }
        });
    let rhs_call = operand_call.clone();

    quote! {
        /// AW-V.W4.1 — visitor-path Pratt-shape parse function.
        ///
        /// Dispatches visitor method calls in place of tape pushes.
        /// Per-grammar `V: PrattVisitor` composes at the call site —
        /// the generic bound statically resolves every method to the
        /// chosen visitor impl.
        ///
        /// # Emitted algorithm (mirrors tape path; see `emit_parse_pratt`)
        ///
        /// 1. `visitor.begin_pratt()` — outer compound marker.
        /// 2. Dispatch leftmost operand through the grammar's
        ///    visitor dispatcher.
        /// 3. `visitor.operand_end()` — per operand boundary.
        /// 4. Loop: peek byte / LUT / reduce / push op / advance / RHS.
        ///    Each `operator()` visitor call fires at the
        ///    reducer-compound stamp; `operand_end` fires after each
        ///    operand parse.
        /// 5. `visitor.end_pratt()` — outer compound close.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_assignments, unused_mut, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::PrattVisitor
                + ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            // Local op stack — same layout as the tape-path emitter's
            // `LocalOpEntry` but carries only the data the visitor
            // reducer thread needs. No `lhs_idx` / `lhs_span_lo` —
            // the visitor receives the operator via its `operator`
            // method and synthesises AST nodes on its own side.
            //
            // AY.W1.4 — initialised via `::core::array::from_fn` for
            // the fixed-size stack array (the bootstrap postprocessor
            // strips inner derive attributes; `from_fn` is the
            // Copy-free idiom).
            struct LocalOpEntry {
                op_discriminant: u8,
                precedence: u8,
                associativity_is_left: bool,
            }

            let _ = #support_mod::skip_space(input, p, state);
            visitor.begin_pratt().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;

            // Leftmost operand — AW-V.W5.2 per-Ref direct call.
            #operand_call
            visitor.operand_end().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;

            // AY.W1.4 — fixed-size stack array (mirrors tape-path
            // emitter; 16 entries × 4 bytes = 64 byte stack frame).
            // The pre-AY heap `Vec::with_capacity(4)` allocated per
            // Pratt parse and freed at function exit; the bench delta
            // is measurable on Sheets `parse_stress` whose
            // `parse_pratt_visitor_*` frames recur every formula.
            const OP_STACK_CAP: usize = 16;
            let mut op_stack: [LocalOpEntry; OP_STACK_CAP] =
                ::core::array::from_fn(|_| LocalOpEntry {
                    op_discriminant: 0,
                    precedence: 0,
                    associativity_is_left: false,
                });
            let mut op_stack_len: usize = 0;

            // AX.W0a.2.n — Whitespace-aware operator peek (mirrors
            // tape-path emitter). Dispatch on the current byte
            // directly when it is already an operator; fall back to
            // skip_space + re-peek only when the current byte is
            // not in the rule's LUT alphabet. Preserves whitespace-
            // carrying combinator operators while fixing the
            // trailing-whitespace premature-break.
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = #rule_lut_ident[op_byte as usize];
                if lut_byte == 0 {
                    let _ = #support_mod::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = #rule_lut_ident[op_byte as usize];
                }
                let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(lut_byte & 0x0Fu8)
                };

                // Reduce: fire `operator` for each op we pop.
                loop {
                    if op_stack_len == 0 {
                        break;
                    }
                    let top_op = &op_stack[op_stack_len - 1];
                    let should_reduce = match new_prec {
                        ::core::option::Option::None => true,
                        ::core::option::Option::Some(p_new) => {
                            top_op.precedence > p_new
                                || (top_op.precedence == p_new
                                    && top_op.associativity_is_left)
                        }
                    };
                    if !should_reduce {
                        break;
                    }
                    let op_disc = top_op.op_discriminant;
                    let op_prec = top_op.precedence;
                    op_stack_len -= 1;
                    visitor
                        .operator(op_disc, op_prec)
                        .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        })?;
                }

                if lut_byte == 0 {
                    break;
                }

                let precedence: u8 = lut_byte & 0x0Fu8;
                let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
                let associativity_is_left: bool = assoc_bit == 0;
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;

                let second_byte: ::core::option::Option<u8> =
                    input.get(*p + 1).copied();
                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in #rule_entries_ident.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };

                // AX.W0a.2.n — phantom-op guard (mirrors tape-path).
                if !op_matched {
                    break;
                }

                *p = (*p).saturating_add(op_width as usize);

                debug_assert!(
                    op_stack_len < OP_STACK_CAP,
                    "Pratt visitor op_stack overflow at depth {} (cap {})",
                    op_stack_len,
                    OP_STACK_CAP,
                );
                op_stack[op_stack_len] = LocalOpEntry {
                    op_discriminant,
                    precedence,
                    associativity_is_left,
                };
                op_stack_len += 1;

                let _ = #support_mod::skip_space(input, p, state);
                // AW-V.W5.2 — per-Ref RHS operand call.
                #rhs_call
                visitor.operand_end().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            }

            visitor.end_pratt().map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
            Ok(())
        }
    }
}

/// Extract the first value-position Ref from a Pratt rule body.
///
/// AW-V.W5.2 — canonical Pratt body is `operand (op operand)*` which
/// lowers through Seq / Next / Repeat to place the operand Ref at
/// the head of the body. Walks left-most to find the first Ref.
fn extract_first_ref(node: &bbnf_ir::IrNode) -> Option<bbnf_ir::RuleId> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Ref(rid) => Some(*rid),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_first_ref(inner)
        }
        IrNode::Seq(children) => children.iter().find_map(extract_first_ref),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            extract_first_ref(lhs).or_else(|| extract_first_ref(rhs))
        }
        IrNode::Repeat { inner, .. } => extract_first_ref(inner),
        _ => None,
    }
}
