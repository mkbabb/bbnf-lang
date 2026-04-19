//! Pratt-shape emitter — `parse_pratt_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Pratt-shape parse functions for operator-chain
//! head rules. The emitted body mirrors the walker's existing
//! `DtaState::ShuntingYard` arm at
//! `bbnf-tape::driver.rs` — operand dispatch, then a loop that
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
        ///       byte's u8 discriminant into `pay_agg`.
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
            // Inline `Vec` bounded by the grammar's precedence-chain
            // depth. Typical Pratt towers have ≤ 6 rungs (Sheets, CSS
            // math); a small-capacity heap Vec avoids the bench
            // pressure of a SmallVec dep while keeping allocation
            // amortised across parses.
            let mut op_stack: ::std::vec::Vec<LocalOpEntry> =
                ::std::vec::Vec::with_capacity(4);

            // ── Reducer loop ────────────────────────────────────────
            loop {
                // Peek next byte; consult PRECEDENCE_LUT.
                let op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let lut_byte: u8 = PRECEDENCE_LUT[op_byte as usize];
                let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(lut_byte & 0x0Fu8)
                };

                // Reduce op-stack entries whose precedence > new_prec
                // (or ties + left-assoc). On EOF-op (new_prec = None)
                // reduce all remaining entries.
                loop {
                    let top_op = match op_stack.last() {
                        ::core::option::Option::Some(e) => e,
                        ::core::option::Option::None => break,
                    };
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
                    let top_op = op_stack.pop().unwrap();
                    let compound_idx = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Rule,
                        ::bbnf::runtime::tape::TapeOffset(top_op.lhs_idx),
                        top_op.lhs_span_lo,
                        *p as u32,
                        top_op.op_discriminant,
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
                // PRECEDENCE_ENTRIES.
                let precedence: u8 = lut_byte & 0x0Fu8;
                let assoc_bit: u8 = (lut_byte >> 4) & 0x01u8;
                let associativity_is_left: bool = assoc_bit == 0;
                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;

                let second_byte: ::core::option::Option<u8> =
                    input.get(*p + 1).copied();
                let (op_width, op_discriminant) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    for e in PRECEDENCE_ENTRIES.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            break;
                        }
                    }
                    (1u32, found_disc)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    for e in PRECEDENCE_ENTRIES.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc)
                };

                // Advance past the op bytes + emit a payload-bearing
                // Span leaf carrying the discriminant (walker parity;
                // downstream typed-payload walkers surface every op).
                let op_lo: u32 = *p as u32;
                *p = (*p).saturating_add(op_width as usize);
                let op_hi: u32 = *p as u32;
                let arena_off: u32 = builder.arena_mut().len() as u32;
                builder.arena_mut().push(op_discriminant);
                let _op_rec = builder.push_leaf_with_arena_frame(
                    ::bbnf::runtime::tape::TapeKind::Span,
                    op_lo,
                    op_hi,
                    0,
                    0,
                    arena_off,
                );

                // Capture LHS span_lo for the reducer compound. The
                // walker reads `columns.span_lo[this_operand_root]`
                // — this emitter mirrors via the builder's column
                // accessor.
                let lhs_span_lo: u32 = builder
                    .columns()
                    .span_lo
                    .get(this_operand_root as usize)
                    .copied()
                    .unwrap_or(op_hi);

                op_stack.push(LocalOpEntry {
                    op_discriminant,
                    precedence,
                    associativity_is_left,
                    lhs_idx: this_operand_root,
                    lhs_span_lo,
                });

                // ── RHS operand ─────────────────────────────────────
                let _ = #support_mod::skip_space(input, p, state);
                // AW-V.W5.2 — per-Ref RHS call.
                #rhs_call
                // Re-point `this_operand_root` at the RHS root (first
                // record the RHS emitted). The `push_leaf_with_arena_frame`
                // above sits at this_operand_root + 1 when the operand
                // was a single leaf; for compound operands the root
                // sits at the position right after the op leaf.
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

            let mut op_stack: ::std::vec::Vec<LocalOpEntry> =
                ::std::vec::Vec::with_capacity(4);

            loop {
                let op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let lut_byte: u8 = PRECEDENCE_LUT[op_byte as usize];
                let new_prec: ::core::option::Option<u8> = if lut_byte == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(lut_byte & 0x0Fu8)
                };

                // Reduce: fire `operator` for each op we pop.
                loop {
                    let top_op = match op_stack.last() {
                        ::core::option::Option::Some(e) => e,
                        ::core::option::Option::None => break,
                    };
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
                    let top_op = op_stack.pop().unwrap();
                    visitor
                        .operator(top_op.op_discriminant, top_op.precedence)
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
                let (op_width, op_discriminant) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    for e in PRECEDENCE_ENTRIES.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            break;
                        }
                    }
                    (1u32, found_disc)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    for e in PRECEDENCE_ENTRIES.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc)
                };

                *p = (*p).saturating_add(op_width as usize);

                op_stack.push(LocalOpEntry {
                    op_discriminant,
                    precedence,
                    associativity_is_left,
                });

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
