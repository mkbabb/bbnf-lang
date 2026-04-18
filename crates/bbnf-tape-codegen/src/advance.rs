//! Body fragment for `bbnf_tape::driver::advance_or_pop_with`.
//!
//! This helper is the Seq / Alt / Repeat / ShuntingYard advance
//! dispatcher the walker calls from every arm tail. In AW-IV it
//! remained out-of-line as a cross-crate function-call boundary; the
//! per-shape emitter splices this body into its generated per-shape
//! parse function so the boundary collapses on the hot path.
//!
//! The splicer captures the runtime helper's parameters at its
//! splice site (`_table`, `_input`, `columns`, `frame_depth`, `psi`,
//! `stack`, `pos`, `slot`) before emitting `quote! { #fragment }`.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim source for the body of
/// `bbnf_tape::driver::advance_or_pop_with` — the text between the
/// outer `fn ... { ... }` braces.
///
/// The runtime helper at `crates/bbnf-tape/src/driver.rs:2536`
/// survives unchanged. This constant is the splice source for the
/// per-shape emitter's hot path; divergence between the two is
/// detected by the pair of tests in `tests/parse_fragments.rs`
/// (fragment parses as `syn::Block`) + the wire-contract integration
/// tests landed by W3.
pub const SOURCE: &str = r#"{
    // AW-III.W5.c — `slot` participates in iter-savepoint capture so a
    // body absorption restores the structural cursor atomically. The
    // helper itself does not consult the structural index — the
    // dual-cursor's per-arm shortcuts live in `dispatch_one`'s
    // ConsumeToNextStructural / WsTrim arms.
    loop {
        let Some(top) = stack.top_mut() else {
            return Ok(StepResult::Done);
        };
        match top.kind {
            DtaFrameKind::Seq => {
                top.cursor += 1;
                if (top.cursor as usize) < top.children.len() {
                    return Ok(StepResult::Next(top.children[top.cursor as usize]));
                }
            }
            DtaFrameKind::Alt => {
                // Alt frames resolve on the first successful branch —
                // the AltLinear arm already stamped `cursor` with the
                // branch index, and the branch's subtree sits inside
                // the Alt compound's child run. Close and pop.
            }
            DtaFrameKind::Repeat => {
                // One iteration completed. Consult lo/hi + position-
                // stagnation to decide whether to re-enter or close.
                // Copy-out the `top` fields first to release the
                // mutable borrow on `stack`.
                let counter_idx = top.counter_idx as usize;
                let iter_start_pos = top.last_pos;
                let counter_optional_flag = top.counter_optional_flag;
                let hi = top.hi;
                let inner = top.repeat_inner;
                let _ = top;

                let counter_val = stack.counters[counter_idx] + 1;
                stack.counters[counter_idx] = counter_val;

                let stagnant = *pos == iter_start_pos;
                let should_close = counter_val as u32 >= hi as u32
                    || (stagnant && counter_optional_flag == 0);

                if should_close {
                    // Fall through to close+pop.
                } else {
                    // Re-enter the body. Refresh the iteration
                    // savepoint + `last_pos` for the next round.
                    //
                    // AW-I.W4δ: refresh psi_len alongside cols/fd.
                    // Pre-W4δ the code preserved the iter-1 psi_len
                    // ("prior_psi_len"), which meant a later-
                    // iteration body-failure would truncate psi back
                    // past already-committed iterations' payload
                    // writes. Use the CURRENT psi.len() so absorbed
                    // failures restore to "end of successful
                    // iterations", not "before the loop".
                    let new_sp_cols = columns.len() as u32;
                    let new_sp_fd = frame_depth.len() as u32;
                    let new_sp_psi = psi.len() as u32;
                    let new_sp_pay_agg = columns.pay_agg.len() as u32;
                    let pos_val = *pos;
                    // AW-III.W5.c — capture the structural cursor slot
                    // alongside the stack lengths. A later body failure
                    // restores both atomically via `handle_repeat_failure`.
                    let new_stack_sp = stack.savepoint(*slot);
                    stack.iter_savepoints[counter_idx] = IterSavepoint {
                        cols_len: new_sp_cols,
                        fd_len: new_sp_fd,
                        psi_len: new_sp_psi,
                        pay_agg_len: new_sp_pay_agg,
                        pos: pos_val,
                        stack: new_stack_sp,
                    };
                    if let Some(top2) = stack.top_mut() {
                        top2.last_pos = pos_val;
                    }
                    return Ok(StepResult::Next(inner));
                }
            }
            DtaFrameKind::ShuntingYard => {
                // Operand complete. Consult the precedence table to
                // decide: reduce-and-pop (no more ops), or push a new
                // operator (emitting a reduced compound first if the
                // stack top's precedence demands it).
                let sy_state_id = top.repeat_inner;
                let sy_parent_depth_marker = top.child_mark;
                let sy_parent_rec = top.parent_rec;
                // The operand just finished at span [top.last_pos ..
                // pos]. Track the operand's tape root — the record at
                // `sy_parent_depth_marker` is the first operand; each
                // subsequent operand starts at the length-marker
                // from the prior op-push.
                let mut this_operand_root = top.cursor as u32;
                if this_operand_root == 0 {
                    // First operand: its root sits at the child_mark
                    // (the first record after the outer SY parent).
                    this_operand_root = sy_parent_depth_marker;
                }

                let (head_state, precedence_slice, input_ref) = match (_table, _input) {
                    (Some(t), Some(i)) => {
                        let st = t.states[sy_state_id.0 as usize];
                        match st {
                            DtaState::ShuntingYard { head, precedence } => {
                                (head, precedence, i)
                            }
                            _ => return Err(DtaError::InvalidState { state: sy_state_id }),
                        }
                    }
                    _ => {
                        // Context unavailable — the walker always
                        // supplies table + input for ShuntingYard
                        // dispatch. This arm is unreachable in
                        // practice; failing loud beats silent
                        // misbehaviour.
                        return Err(DtaError::InvalidState { state: sy_state_id });
                    }
                };

                let b = input_ref.get(*pos as usize).copied().unwrap_or(0);
                let b2 = input_ref.get(*pos as usize + 1).copied();
                let entry_opt = lookup_precedence(precedence_slice, b, b2);

                // Reduce top-of-op-stack entries whose precedence
                // exceeds (or ties with, for left-assoc) the new
                // op's precedence; reducing emits a binary compound.
                // If no new op, reduce all pending ops.
                let new_prec = entry_opt.map(|e| e.precedence);
                while let Some(top_op) = stack.op_stack.last().copied() {
                    let should_reduce = match new_prec {
                        None => true, // no new op — reduce all remaining
                        Some(p) => {
                            top_op.precedence > p
                                || (top_op.precedence == p
                                    && matches!(
                                        top_op.associativity,
                                        crate::dta::DtaAssociativity::Left
                                    ))
                        }
                    };
                    if !should_reduce {
                        break;
                    }
                    stack.op_stack.pop();
                    let compound_idx = emit_reducer_compound(
                        columns,
                        frame_depth,
                        stack.depth(),
                        top_op.lhs_idx,
                        top_op.op_discriminant,
                        top_op.lhs_span_lo,
                        *pos,
                    );
                    this_operand_root = compound_idx;
                    let _ = top_op.op_rule;
                }

                if let Some(entry) = entry_opt {
                    // Push the new op onto the stack. Advance past
                    // its bytes (1 or 2). Re-enter `head` to parse
                    // the RHS operand.
                    let op_width = if entry.second_byte.is_some() { 2 } else { 1 };
                    let op_lo = *pos;
                    *pos = pos.saturating_add(op_width);
                    // AW-III.W1: emit a payload-bearing Span leaf
                    // carrying the op's u8 discriminant so downstream
                    // walkers (`typed_u8_payloads`,
                    // `find_named_color_payload`-style readers) can
                    // surface every operator the SY chain consumed.
                    // Without this the SY collapse intercepted the
                    // per-branch Map { Literal "+", IntLit(0) } shape
                    // before its U8 payload had a chance to land — the
                    // walker advanced past `+`/`-`/`*`/`/` opcodes
                    // without writing anything to the tape.
                    let op_arena_off = columns.pay_agg.len() as u32;
                    columns.pay_agg.push(entry.op_discriminant);
                    let _op_rec = emit_leaf_with_payload(
                        columns,
                        frame_depth,
                        stack,
                        TapeKind::Span,
                        op_lo,
                        *pos,
                        TapeOffset(op_arena_off),
                    );
                    let lhs_span_lo = columns
                        .span_lo
                        .get(this_operand_root as usize)
                        .copied()
                        .unwrap_or(*pos);
                    stack.op_stack.push(OpStackEntry {
                        op_rule: entry.op_rule,
                        op_discriminant: entry.op_discriminant,
                        precedence: entry.precedence,
                        associativity: entry.associativity,
                        lhs_idx: this_operand_root,
                        lhs_span_lo,
                    });
                    let pos_val = *pos;
                    if let Some(top) = stack.top_mut() {
                        top.cursor = 0;
                        top.last_pos = pos_val;
                    }
                    return Ok(StepResult::Next(head_state));
                } else {
                    // No operator — the outermost SY frame closes.
                    // The parent compound's child_off points at the
                    // final reduced operand (this_operand_root).
                    // Patch it instead of letting close_compound
                    // default to the frame's `child_mark`.
                    let sy_parent = sy_parent_rec as usize;
                    columns.child_off[sy_parent] = TapeOffset(this_operand_root);
                    columns.extra[sy_parent] |= crate::tape::TapeRec::HAS_CHILDREN_BIT;
                    columns.span_hi[sy_parent] = *pos;
                    // Suppress the default close_compound path for
                    // this frame by popping manually and continuing
                    // the outer loop.
                    pop_and_release(stack);
                    continue;
                }
            }
        }
        // Close the compound and pop.
        close_compound(columns, frame_depth, stack, *pos);
        pop_and_release(stack);
    }
}"#;

/// Parse [`SOURCE`] as a [`syn::Block`] and return the
/// [`TokenStream`] the per-shape emitter splices inline.
///
/// # Panics
///
/// Panics if [`SOURCE`] fails to parse — a regression in the fragment
/// source or a syntax-breaking edit on the runtime helper this crate
/// mirrors. The `tests/parse_fragments.rs` suite catches this at
/// `cargo test -p bbnf-tape-codegen` time.
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect(
            "bbnf-tape-codegen: advance_or_pop_with body fragment must \
             parse as syn::Block — the runtime helper at \
             crates/bbnf-tape/src/driver.rs has likely diverged from \
             this crate's SOURCE constant",
        )
        .to_token_stream()
}
