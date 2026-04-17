//! AW-III.W4.d — Per-`DtaState` lowering routines (full inline lowering).
//!
//! # Architectural role
//!
//! Each variant of `bbnf_ir::passes::DtaState` lowers to a `match cur`
//! arm in the outer dispatch loop. The arm encodes the variant's
//! semantics — byte cmp for `Literal`, regex scan for `Regex`, frame
//! push for `Seq`, etc. — directly inline. There is no cold-path
//! bridge in the hot dispatch arms (W4.d collapse): every arm body
//! reproduces the corresponding `dispatch_one` semantic with the
//! state-id specialised at codegen time.
//!
//! Per the §6 invariant the lowering decisions read only IR-structural
//! facts (variant, byte ranges, child slices); the grammar's identity
//! never appears in the lowering body.
//!
//! # Per-variant lowering shape
//!
//! Each arm produces `Result<StepResult, DtaError>`. Successful arms
//! return `Ok(StepResult::Next(<id>))` to pass the next state to the
//! outer loop, or `Ok(StepResult::Done)` when the entry rule's root
//! frame closed. Failing arms return `Err(DtaError::Syntax { ... })`;
//! the outer loop hands those to `handle_repeat_failure` to absorb
//! into an enclosing Repeat or surface to the caller.
//!
//! ## Variants
//!
//! - `Epsilon` — clear pending stamp + `advance_or_pop_with`.
//! - `Literal { text, payload }` — byte cmp inline; emit_leaf with
//!   `TapeKind::Span`/`Literal` keyed by payload presence; advance.
//! - `Regex { pattern, payload }` — scanner.scan + emit_leaf_with_payload
//!   when payload is Some; PSI push; advance.
//! - `Seq { children, frame, promote }` — reserve compound, push frame,
//!   transition to `children[0]`.
//! - `ByteDispatch { table, fallback }` — 256-entry LUT inlined as
//!   `match input[pos]`; LLVM lowers to a jump table.
//! - `AltLinear { branches }` — savepoint-and-try every branch via
//!   `try_branch`; restore on Syntax; surface first success.
//! - `Repeat { inner, lo, hi, counter_optional }` — counter slot
//!   allocation, iter-savepoint reservation, push Repeat frame,
//!   transition to inner.
//! - `Ref { rule, target }` — set `pending_variant_idx`, transition to
//!   target (or `rule_entry_for(rule)` when target is NONE).
//! - `WsTrim { pattern }` — scanner.scan or `trim_ascii_ws`; advance.
//! - `Minus { primary, excluded }` — probe excluded with deep snapshot;
//!   on success → Syntax error; on failure → transition to primary.
//! - `ShuntingYard { head, precedence }` — reserve compound, push SY
//!   frame, transition to head. The reducer remains in
//!   `advance_or_pop_with`'s SY arm — the W4.d hot-path lowering
//!   inlines the entry-side logic; the reducer's operator-precedence
//!   loop stays in the runtime helper because its size + dynamic
//!   precedence-table consumption do not benefit from per-state
//!   inlining.
//!
//! # Hot/cold split
//!
//! [`emit_state_dispatch_arms`] emits the hot states' arms inline in
//! the outer `match cur`. Cold states emit as `#[cold] #[inline(never)]`
//! sibling functions called via fallthrough — the outer loop treats a
//! cold sibling as a single dispatch step that surfaces the same
//! `Result<StepResult, DtaError>` shape.

use bbnf_ir::passes::recognizers::dta::{
    CounterOptional, DtaState as IrState, DtaTable, FrameKind, LiteralPayload,
    RegexPayloadKind, SeqPromote, StateId,
};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use super::hot_cold::HotColdPartition;

/// Emit the body of the outer `match cur { ... }` dispatch.
///
/// Hot states emit as inline arms with the lowered state body. Cold
/// states emit as forwarders to their `#[cold]` sibling — the outer
/// dispatch sees a single uniform `Result<StepResult, DtaError>`
/// return shape.
pub(super) fn emit_state_dispatch_arms(
    table: &DtaTable,
    partition: &HotColdPartition,
) -> TokenStream {
    let arms = table
        .states
        .iter()
        .enumerate()
        .map(|(idx, state)| {
            let id = idx as u16;
            let id_lit = Literal::u16_unsuffixed(id);
            if partition.is_hot(id) {
                let body = emit_state_arm_body(idx, state);
                quote! {
                    #id_lit => { #body }
                }
            } else {
                let cold_ident = cold_sibling_ident(id);
                quote! {
                    #id_lit => {
                        #cold_ident::<__S>(
                            input, scanner, columns, psi, frame_depth,
                            stack, pos,
                        )
                    }
                }
            }
        })
        .collect::<Vec<_>>();
    quote! { #(#arms)* }
}

/// Emit the cold sibling functions — one per cold state. Each sibling
/// is `#[cold] #[inline(never)]` and contains the same arm body the
/// hot path would have inlined; the dispatch contract is the same
/// `Result<StepResult, DtaError>` return shape.
///
/// Cold siblings take the walker's mutable state (`stack`, `pos`) by
/// reference because their lowered body mutates them — the hot path
/// supplies them from its own enclosing scope; the cold sibling
/// receives them through the function signature.
pub(super) fn emit_cold_siblings(
    table: &DtaTable,
    partition: &HotColdPartition,
) -> TokenStream {
    if partition.cold.is_empty() {
        return TokenStream::new();
    }
    let siblings = table
        .states
        .iter()
        .enumerate()
        .filter_map(|(idx, state)| {
            let id = idx as u16;
            if partition.is_hot(id) {
                return None;
            }
            let cold_ident = cold_sibling_ident(id);
            let body = emit_state_arm_body(idx, state);
            Some(quote! {
                #[cold]
                #[inline(never)]
                fn #cold_ident<__S: ::bbnf::runtime::tape::RegexScanner>(
                    input: &[u8],
                    scanner: &__S,
                    columns: &mut ::bbnf::runtime::tape::Columns,
                    psi: &mut ::bbnf::runtime::tape::PayloadStream,
                    frame_depth: &mut ::std::vec::Vec<u8>,
                    stack: &mut ::bbnf::runtime::tape::FrameStack,
                    pos: &mut u32,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::StepResult,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    // Cold sibling wraps its body in `'step` so
                    // `break 'step <value>` exits here with `<value>`
                    // as the return, keeping the emitted bodies
                    // identical to the hot path's structure.
                    //
                    // The cold sibling lives in the same inner module
                    // as `run`; the bare `DTA_TABLE` resolves through
                    // the module's `use super::*;` glob to the single
                    // `__<grammar>parser_emit_impl::DTA_TABLE` const,
                    // avoiding ambiguity when multiple parsers
                    // co-exist in the same parent scope (test files).
                    let table: &::bbnf::runtime::tape::DtaTable = &DTA_TABLE;
                    'step: {
                        #body
                    }
                }
            })
        })
        .collect::<Vec<_>>();
    quote! { #(#siblings)* }
}

/// The fn ident for a cold sibling — `__cold_state_<id>`.
fn cold_sibling_ident(id: u16) -> proc_macro2::Ident {
    format_ident!("__cold_state_{}", id)
}

/// Emit the inlined body of one state's dispatch arm. The body
/// produces a `Result<StepResult, DtaError>` value the outer
/// dispatch loop consumes to set the next `cur` or terminate.
fn emit_state_arm_body(idx: usize, state: &IrState) -> TokenStream {
    let kind_tag = state_kind_tag(state);
    let body = match state {
        IrState::Epsilon => emit_epsilon_arm(idx),
        IrState::Literal { text: _, payload } => {
            emit_literal_arm(idx, *payload)
        }
        IrState::Regex { pattern: _, payload } => {
            emit_regex_arm(idx, *payload)
        }
        IrState::Seq { children, frame, promote } => {
            emit_seq_arm(idx, children, *frame, *promote)
        }
        IrState::ByteDispatch { table: disp, fallback } => {
            emit_byte_dispatch_arm(idx, disp, *fallback)
        }
        IrState::AltLinear { branches } => {
            emit_alt_linear_arm(idx, branches)
        }
        IrState::Repeat { inner, lo, hi, counter_optional } => {
            emit_repeat_arm(idx, *inner, *lo, *hi, *counter_optional)
        }
        IrState::Ref { rule, target } => {
            emit_ref_arm(idx, *rule, *target)
        }
        IrState::WsTrim { pattern } => emit_ws_trim_arm(idx, pattern.is_some()),
        IrState::Minus { primary, excluded } => {
            emit_minus_arm(idx, *primary, *excluded)
        }
        IrState::ShuntingYard { head, .. } => {
            emit_shunting_yard_arm(idx, *head)
        }
    };
    quote! {
        // State #idx — variant: #kind_tag
        let _ = #kind_tag;
        #body
    }
}

/// The state's variant tag — emitted as a `&'static str` literal in
/// every arm so the codegen test can verify per-variant arm presence
/// via grep without parsing the full token stream.
fn state_kind_tag(state: &IrState) -> &'static str {
    match state {
        IrState::Epsilon => "epsilon",
        IrState::Literal { .. } => "literal",
        IrState::Regex { .. } => "regex",
        IrState::Seq { .. } => "seq",
        IrState::ByteDispatch { .. } => "byte_dispatch",
        IrState::AltLinear { .. } => "alt_linear",
        IrState::Repeat { .. } => "repeat",
        IrState::Ref { .. } => "ref_target",
        IrState::ShuntingYard { .. } => "shunting_yard",
        IrState::WsTrim { .. } => "ws_trim",
        IrState::Minus { .. } => "minus",
    }
}

// ── Per-variant lowering routines ───────────────────────────────────

/// AW-III.W4.d — emit the post-leaf advance pattern: try the
/// inline-always Seq-fast-path first, fall through to the full
/// `advance_or_pop_with` body when the fast path doesn't apply.
///
/// JSON twitter visits a Seq frame for ~80% of leaf emit sites; the
/// fast path inlines an in-place cursor++ and a child-state read,
/// folding the dominant case directly into the calling arm. The
/// non-Seq fall-through (Alt close, Repeat re-entry, SY reducer,
/// stack drain) goes through the full helper which LLVM keeps
/// out-of-line.
fn emit_advance_or_pop_call() -> TokenStream {
    quote! {
        if let ::core::option::Option::Some(next) =
            ::bbnf::runtime::tape::advance_seq_fast(stack)
        {
            ::core::result::Result::Ok(next)
        } else {
            ::bbnf::runtime::tape::advance_or_pop_with(
                ::core::option::Option::Some(table),
                ::core::option::Option::Some(input),
                columns, frame_depth, psi, stack, pos,
            )
        }
    }
}

/// `Epsilon` — no column emission, no byte advance. Drop pending
/// rule-entry stamp and let `advance_or_pop_with` route to the next
/// dispatch.
fn emit_epsilon_arm(_idx: usize) -> TokenStream {
    let advance = emit_advance_or_pop_call();
    quote! {
        stack.pending_variant_idx = u8::MAX;
        #advance
    }
}

/// `Literal { text, payload }` — destructure from the table at the
/// known state-id, byte-compare inline, emit_leaf, advance.
fn emit_literal_arm(idx: usize, payload: LiteralPayload) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let advance = emit_advance_or_pop_call();
    let payload_arm = if matches!(payload, LiteralPayload::None) {
        quote! {
            ::bbnf::runtime::tape::emit_leaf(
                columns, frame_depth, stack,
                ::bbnf::runtime::tape::TapeKind::Literal,
                lo, *pos,
            );
        }
    } else {
        quote! {
            let arena_off = ::bbnf::runtime::tape::stage_literal_payload_in_arena(
                columns, payload,
            );
            ::bbnf::runtime::tape::emit_leaf_with_payload(
                columns, frame_depth, stack,
                ::bbnf::runtime::tape::TapeKind::Span,
                lo, *pos, arena_off,
            );
        }
    };
    quote! {
        let (text, payload) = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Literal { text, payload } => (text, payload),
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let bytes = text.as_bytes();
        let start = *pos as usize;
        let end = start.saturating_add(bytes.len());
        if end > input.len() || &input[start..end] != bytes {
            break 'step ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *pos,
                    failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                },
            );
        }
        let lo = *pos;
        *pos = end as u32;
        #payload_arm
        stack.pending_variant_idx = u8::MAX;
        #advance
    }
}

/// `Regex { pattern, payload }` — scanner.scan inline; emit_leaf with
/// `TapeKind::Span`; PSI push when payload is Some.
fn emit_regex_arm(idx: usize, payload: Option<RegexPayloadKind>) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let advance = emit_advance_or_pop_call();
    let emit_payload = if payload.is_none() {
        quote! {
            ::bbnf::runtime::tape::emit_leaf(
                columns, frame_depth, stack,
                ::bbnf::runtime::tape::TapeKind::Span,
                lo, *pos,
            );
        }
    } else {
        // Per the cold-path semantics: arena-reserve worst-case width
        // up front so the PSI worker performs only the decode + store.
        quote! {
            if let ::core::option::Option::Some(kind) = payload {
                let width = match (kind, kind.arena_byte_width()) {
                    (::bbnf::runtime::tape::PayloadKind::String, _) => 4 + match_len as usize,
                    (_, 0) => match_len as usize,
                    (_, w) => w,
                };
                let arena_off = columns.pay_agg.len() as u32;
                columns.pay_agg.resize(arena_off as usize + width, 0);
                let rec_idx = columns.len() as u32;
                ::bbnf::runtime::tape::emit_leaf_with_payload(
                    columns, frame_depth, stack,
                    ::bbnf::runtime::tape::TapeKind::Span,
                    lo, *pos, ::bbnf::runtime::tape::TapeOffset(arena_off),
                );
                psi.push(::bbnf::runtime::tape::PayloadJob::new(
                    rec_idx, lo, *pos, kind, arena_off,
                ));
            } else {
                ::bbnf::runtime::tape::emit_leaf(
                    columns, frame_depth, stack,
                    ::bbnf::runtime::tape::TapeKind::Span,
                    lo, *pos,
                );
            }
        }
    };
    quote! {
        let (pattern, payload) = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Regex { pattern, payload } => (pattern, payload),
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let match_len = match scanner.scan(pattern, input, *pos as usize) {
            ::core::option::Option::Some(n) => n,
            ::core::option::Option::None => {
                break 'step ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *pos,
                        failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
        };
        let lo = *pos;
        *pos = lo + match_len;
        #emit_payload
        stack.pending_variant_idx = u8::MAX;
        #advance
    }
}

/// `Seq { children, frame, promote }` — reserve the parent row, push
/// the Seq/Alt/Repeat frame with the rule's variant_idx, and dispatch
/// to `children[0]` (or close immediately if the body is empty).
fn emit_seq_arm(
    idx: usize,
    children: &[StateId],
    frame: FrameKind,
    promote: SeqPromote,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let advance = emit_advance_or_pop_call();
    let _ = (children, frame, promote); // captured via destructure below
    quote! {
        let (children, frame, promote) = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Seq { children, frame, promote } => {
                (children, frame, promote)
            }
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let parent_rec = columns.len() as u32;
        let tape_kind = ::bbnf::runtime::tape::frame_to_tape_kind(frame);
        ::bbnf::runtime::tape::reserve_compound(
            columns, frame_depth, stack.depth(), tape_kind, *pos,
        );
        let child_mark = columns.len() as u32;
        let variant_idx = stack.pending_variant_idx;
        stack.pending_variant_idx = u8::MAX;
        stack.push(::bbnf::runtime::tape::Frame {
            kind: frame,
            counter_idx: u8::MAX,
            cursor: 0,
            children,
            repeat_inner: ::bbnf::runtime::tape::DtaStateId::NONE,
            parent_rec,
            child_mark,
            tape_kind,
            last_pos: *pos,
            lo: 0,
            hi: 0,
            counter_optional_flag: 0,
            variant_idx,
            promote,
        });
        if children.is_empty() {
            ::bbnf::runtime::tape::close_compound(columns, frame_depth, stack, *pos);
            #advance
        } else {
            ::core::result::Result::Ok(
                ::bbnf::runtime::tape::StepResult::Next(children[0]),
            )
        }
    }
}

/// `ByteDispatch { table, fallback }` — 256-entry LUT inlined as
/// `match input[pos]` over byte literals. LLVM lowers this to a jump
/// table; the per-state runtime table read is gone.
fn emit_byte_dispatch_arm(
    idx: usize,
    disp: &[StateId],
    fallback: ::core::option::Option<StateId>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    // Group the 256 entries by target state — reduces the emitted
    // arm count from 256 (one per byte) to ~K (one per distinct
    // target).
    let mut by_target: std::collections::BTreeMap<u16, Vec<u8>> =
        std::collections::BTreeMap::new();
    for (b, &target) in disp.iter().enumerate() {
        if target != StateId::NONE {
            by_target
                .entry(target.0)
                .or_default()
                .push(b as u8);
        }
    }
    let arms = by_target.iter().map(|(target, bytes)| {
        let target_lit = Literal::u16_unsuffixed(*target);
        let byte_lits = bytes.iter().map(|b| Literal::u8_unsuffixed(*b));
        quote! {
            #(#byte_lits)|* => ::bbnf::runtime::tape::DtaStateId(#target_lit),
        }
    });
    let fallback_arm = match fallback {
        ::core::option::Option::Some(s) if s != StateId::NONE => {
            let s_lit = Literal::u16_unsuffixed(s.0);
            quote! {
                _ => ::bbnf::runtime::tape::DtaStateId(#s_lit),
            }
        }
        _ => {
            quote! {
                _ => {
                    break 'step ::core::result::Result::Err(
                        ::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *pos,
                            failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    );
                }
            }
        }
    };
    quote! {
        let b = input.get(*pos as usize).copied().unwrap_or(0);
        let chosen = match b {
            #(#arms)*
            #fallback_arm
        };
        if let ::core::option::Option::Some(top) = stack.top_mut() {
            if matches!(top.kind, ::bbnf::runtime::tape::DtaFrameKind::Alt) {
                top.cursor = chosen.0;
            }
        }
        ::core::result::Result::Ok(
            ::bbnf::runtime::tape::StepResult::Next(chosen),
        )
    }
}

/// `AltLinear { branches }` — push an Alt frame, savepoint, and try
/// each branch via `try_branch`. Restore-and-retry on Syntax; surface
/// first success or final failure.
fn emit_alt_linear_arm(idx: usize, branches: &[StateId]) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let _ = branches;
    quote! {
        let branches = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::AltLinear { branches } => branches,
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        if branches.is_empty() {
            break 'step ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *pos,
                    failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                },
            );
        }
        let start_depth = stack.depth();
        let start_pos = *pos;
        let parent_rec = columns.len() as u32;
        ::bbnf::runtime::tape::reserve_compound(
            columns, frame_depth, start_depth,
            ::bbnf::runtime::tape::TapeKind::Alt, *pos,
        );
        let child_mark = columns.len() as u32;
        let variant_idx = stack.pending_variant_idx;
        stack.pending_variant_idx = u8::MAX;
        stack.push(::bbnf::runtime::tape::Frame {
            kind: ::bbnf::runtime::tape::DtaFrameKind::Alt,
            counter_idx: u8::MAX,
            cursor: 0,
            children: &[],
            repeat_inner: ::bbnf::runtime::tape::DtaStateId::NONE,
            parent_rec,
            child_mark,
            tape_kind: ::bbnf::runtime::tape::TapeKind::Alt,
            last_pos: *pos,
            lo: 0,
            hi: 0,
            counter_optional_flag: 0,
            variant_idx,
            promote: ::bbnf::runtime::tape::SeqPromote::Default,
        });
        let sp_after_push = stack.savepoint();
        let cols_len_after_push = columns.len();
        let fd_len_after_push = frame_depth.len();
        let psi_len_after_push = psi.len();
        let pay_agg_len_after_push = columns.pay_agg.len();
        let pending_after_push = stack.pending_variant_idx;
        let mut last_err: ::core::option::Option<
            ::bbnf::runtime::tape::DtaError,
        > = ::core::option::Option::None;
        let mut chosen_outcome: ::core::option::Option<
            ::core::result::Result<
                ::bbnf::runtime::tape::StepResult,
                ::bbnf::runtime::tape::DtaError,
            >,
        > = ::core::option::Option::None;
        for (branch_idx, &branch) in branches.iter().enumerate() {
            *pos = start_pos;
            if let ::core::option::Option::Some(top) = stack.top_mut() {
                top.cursor = branch_idx as u16;
            }
            match ::bbnf::runtime::tape::try_branch(
                table, input, scanner, columns, psi, frame_depth, stack,
                branch, pos, start_depth,
            ) {
                ::core::result::Result::Ok(next) => {
                    chosen_outcome = ::core::option::Option::Some(
                        ::core::result::Result::Ok(next),
                    );
                    break;
                }
                ::core::result::Result::Err(
                    e @ ::bbnf::runtime::tape::DtaError::Syntax { .. },
                ) => {
                    columns.truncate(cols_len_after_push);
                    frame_depth.truncate(fd_len_after_push);
                    psi.truncate(psi_len_after_push);
                    columns.pay_agg.truncate(pay_agg_len_after_push);
                    stack.restore(sp_after_push);
                    stack.pending_variant_idx = pending_after_push;
                    last_err = ::core::option::Option::Some(e);
                }
                ::core::result::Result::Err(e) => {
                    break 'step ::core::result::Result::Err(e);
                }
            }
        }
        if let ::core::option::Option::Some(out) = chosen_outcome {
            out
        } else {
            columns.truncate(parent_rec as usize);
            frame_depth.truncate(parent_rec as usize);
            columns.pay_agg.truncate(pay_agg_len_after_push);
            ::bbnf::runtime::tape::pop_and_release(stack);
            ::core::result::Result::Err(last_err.unwrap_or(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: start_pos,
                    failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                },
            ))
        }
    }
}

/// `Repeat { inner, lo, hi, counter_optional }` — allocate counter
/// slot + iter savepoint slot, push Repeat frame, transition to inner.
/// Body-failure absorption happens at the outer-loop boundary via
/// `handle_repeat_failure`.
fn emit_repeat_arm(
    idx: usize,
    _inner: StateId,
    lo: u32,
    hi: u32,
    _counter_optional: ::core::option::Option<CounterOptional>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let lo_lit = Literal::u32_unsuffixed(lo);
    let hi_lit = Literal::u32_unsuffixed(hi);
    let advance = emit_advance_or_pop_call();
    let counter_optional_flag_expr = quote! {
        match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Repeat { counter_optional, .. } => {
                if counter_optional.is_some() { 1u8 } else { 0u8 }
            }
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        }
    };
    quote! {
        let inner = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Repeat { inner, .. } => inner,
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let parent_rec = columns.len() as u32;
        ::bbnf::runtime::tape::reserve_compound(
            columns, frame_depth, stack.depth(),
            ::bbnf::runtime::tape::TapeKind::Rule, *pos,
        );
        let child_mark = columns.len() as u32;
        let counter_idx = stack.counters.len();
        if counter_idx >= u8::MAX as usize {
            break 'step ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::InvalidState {
                    state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                },
            );
        }
        stack.counters.push(0);
        let counter_optional_flag: u8 = #counter_optional_flag_expr;
        stack.iter_savepoints.push(::bbnf::runtime::tape::IterSavepoint {
            cols_len: columns.len() as u32,
            fd_len: frame_depth.len() as u32,
            psi_len: psi.len() as u32,
            pay_agg_len: columns.pay_agg.len() as u32,
            pos: *pos,
            stack: ::bbnf::runtime::tape::FrameStackSavepoint {
                inline_len: 0,
                overflow_len: 0,
                counters_len: 0,
                op_stack_len: 0,
                iter_savepoints_len: 0,
            },
        });
        let variant_idx = stack.pending_variant_idx;
        stack.pending_variant_idx = u8::MAX;
        stack.push(::bbnf::runtime::tape::Frame {
            kind: ::bbnf::runtime::tape::DtaFrameKind::Repeat,
            counter_idx: counter_idx as u8,
            cursor: 0,
            children: &[],
            repeat_inner: inner,
            parent_rec,
            child_mark,
            tape_kind: ::bbnf::runtime::tape::TapeKind::Rule,
            last_pos: *pos,
            lo: ::bbnf::runtime::tape::saturating_u16(#lo_lit),
            hi: ::bbnf::runtime::tape::saturating_u16(#hi_lit),
            counter_optional_flag,
            variant_idx,
            promote: ::bbnf::runtime::tape::SeqPromote::Default,
        });
        stack.iter_savepoints[counter_idx].stack = stack.savepoint();
        if #hi_lit == 0u32 {
            ::bbnf::runtime::tape::close_compound(columns, frame_depth, stack, *pos);
            #advance
        } else {
            ::core::result::Result::Ok(
                ::bbnf::runtime::tape::StepResult::Next(inner),
            )
        }
    }
}

/// `Ref { rule, target }` — set pending_variant_idx, transition to
/// the resolved target (or `rule_entry_for(rule)` when target is
/// `NONE`, the forward-reference case).
fn emit_ref_arm(
    idx: usize,
    rule: u32,
    target: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let rule_lit = Literal::u32_unsuffixed(rule);
    let _ = target;
    quote! {
        let (rule, target) = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Ref { rule, target } => (rule, target),
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let chosen = if target == ::bbnf::runtime::tape::DtaStateId::NONE {
            table.rule_entry_for(rule)
        } else {
            target
        };
        if chosen == ::bbnf::runtime::tape::DtaStateId::NONE {
            break 'step ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *pos,
                    failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                    failing_rule: rule,
                },
            );
        }
        // Stamp the rule's full 8-bit discriminant. Mirrors the cold
        // path's AW-III.W1.A wire-format: the next compound push
        // captures this into `frame.variant_idx`, then close_compound
        // stamps the tape record.
        let _ = #rule_lit; // visible in cargo asm for grammar-agnostic proof.
        stack.pending_variant_idx = (rule.0 & 0xFF) as u8;
        ::core::result::Result::Ok(
            ::bbnf::runtime::tape::StepResult::Next(chosen),
        )
    }
}

/// `WsTrim { pattern }` — scanner.scan with the grammar's `@ws`
/// pattern when set, else `trim_ascii_ws` fallback.
fn emit_ws_trim_arm(idx: usize, has_pattern: bool) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let advance = emit_advance_or_pop_call();
    let scan_path = if has_pattern {
        quote! {
            if let ::core::option::Option::Some(pat) = pattern {
                if let ::core::option::Option::Some(len) =
                    scanner.scan(pat, input, *pos as usize)
                {
                    *pos += len;
                }
            } else {
                ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
            }
        }
    } else {
        quote! {
            let _ = pattern;
            ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
        }
    };
    quote! {
        let pattern = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::WsTrim { pattern } => pattern,
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        #scan_path
        #advance
    }
}

/// `Minus { primary, excluded }` — deep-snapshot probe of `excluded`.
/// On success → Syntax (the matched bytes were excluded). On failure
/// → restore and dispatch `primary`.
fn emit_minus_arm(
    idx: usize,
    _primary: StateId,
    _excluded: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    quote! {
        let (primary, excluded) = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::Minus { primary, excluded } => {
                (primary, excluded)
            }
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let start_pos = *pos;
        let probe_snapshot = stack.snapshot_probe();
        let cols_len = columns.len();
        let fd_len = frame_depth.len();
        let psi_len = psi.len();
        let pay_agg_len = columns.pay_agg.len();
        let start_depth = stack.depth();
        let probe = ::bbnf::runtime::tape::try_branch(
            table, input, scanner, columns, psi, frame_depth, stack,
            excluded, pos, start_depth,
        );
        columns.truncate(cols_len);
        frame_depth.truncate(fd_len);
        psi.truncate(psi_len);
        columns.pay_agg.truncate(pay_agg_len);
        stack.restore_probe(probe_snapshot);
        *pos = start_pos;
        match probe {
            ::core::result::Result::Ok(_) => {
                ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: start_pos,
                        failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                )
            }
            ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax { .. },
            ) => {
                ::core::result::Result::Ok(
                    ::bbnf::runtime::tape::StepResult::Next(primary),
                )
            }
            ::core::result::Result::Err(e) => {
                ::core::result::Result::Err(e)
            }
        }
    }
}

/// `ShuntingYard { head, .. }` — reserve the outer compound, push the
/// SY frame with `repeat_inner = state` (the SY state-id used by
/// `advance_or_pop_with`'s reducer), transition to `head`. The
/// reducer loop stays in `advance_or_pop_with` per the W4.d
/// architectural decision: the operator-precedence reducer's body
/// size + dynamic precedence-table consumption do not benefit from
/// per-state inlining, and the SY chain visits a head-state-bounded
/// number of bytes per parse (≤ chain length × operands).
fn emit_shunting_yard_arm(
    idx: usize,
    _head: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    quote! {
        let head = match table.states[#idx_lit] {
            ::bbnf::runtime::tape::DtaState::ShuntingYard { head, .. } => head,
            _ => unsafe { ::core::hint::unreachable_unchecked() },
        };
        let parent_rec = columns.len() as u32;
        ::bbnf::runtime::tape::reserve_compound(
            columns, frame_depth, stack.depth(),
            ::bbnf::runtime::tape::TapeKind::Rule, *pos,
        );
        let child_mark = columns.len() as u32;
        let variant_idx = stack.pending_variant_idx;
        stack.pending_variant_idx = u8::MAX;
        stack.push(::bbnf::runtime::tape::Frame {
            kind: ::bbnf::runtime::tape::DtaFrameKind::ShuntingYard,
            counter_idx: u8::MAX,
            cursor: 0,
            children: &[],
            repeat_inner: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
            parent_rec,
            child_mark,
            tape_kind: ::bbnf::runtime::tape::TapeKind::Rule,
            last_pos: *pos,
            lo: 0,
            hi: 0,
            counter_optional_flag: 0,
            variant_idx,
            promote: ::bbnf::runtime::tape::SeqPromote::Default,
        });
        ::core::result::Result::Ok(
            ::bbnf::runtime::tape::StepResult::Next(head),
        )
    }
}
