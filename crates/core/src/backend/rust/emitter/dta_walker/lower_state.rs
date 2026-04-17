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
//! - `Regex { pattern, payload }` — hoisted pattern binding + scanner.scan
//!   (to be replaced with direct `__dfa_match_<grammar>_<idx>` call in
//!   the second AW-IV.W1.α landing); emit_leaf_with_payload when
//!   payload is Some; PSI push; advance.
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
                            input, scanner, idx, columns, psi, frame_depth,
                            stack, pos, slot,
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
                #[allow(clippy::too_many_arguments)]
                fn #cold_ident<__S: ::bbnf::runtime::tape::RegexScanner>(
                    input: &[u8],
                    scanner: &__S,
                    idx: &::bbnf::runtime::tape::stage1::StructuralIndex,
                    columns: &mut ::bbnf::runtime::tape::Columns,
                    psi: &mut ::bbnf::runtime::tape::PayloadStream,
                    frame_depth: &mut ::std::vec::Vec<u8>,
                    stack: &mut ::bbnf::runtime::tape::FrameStack,
                    pos: &mut u32,
                    slot: &mut u32,
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
                    //
                    // AW-III.W5.c — cold siblings now carry the dual
                    // cursor (`slot`) + structural index (`idx`)
                    // through their parameter list so the W5 helpers
                    // (try_branch, handle_repeat_failure_bounded)
                    // receive uniform arguments matching the hot
                    // path's invocation shape.
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
///
/// AW-IV.W1.α — every arm body opens with literal `let` bindings
/// computed from the codegen-time `DtaState` value; the runtime
/// `match table.states[N] { Variant { fields } => (fields), _ =>
/// unreachable_unchecked() }` unpack is abrogated.
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
        IrState::ClassifyByte { table: disp, fallback } => {
            // AW-III.W6.3 — Lower via the same per-byte match skeleton
            // as ByteDispatch. The distinct IR variant preserves
            // mining provenance; the emitter body is identical because
            // the dispatch shape is identical (one indexed load + NONE-
            // fallback branch). Separating the emitters lets a future
            // tranche specialize ClassifyByte further (e.g. a proper
            // LLVM jumptable hint) without disturbing ByteDispatch.
            super::super::classify_byte::emit_classify_byte_arm(idx, disp, *fallback)
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
        IrState::ConsumeToNextStructural { pattern: _ } => {
            emit_consume_to_next_structural_arm(idx)
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
        IrState::ClassifyByte { .. } => "classify_byte",
        IrState::AltLinear { .. } => "alt_linear",
        IrState::Repeat { .. } => "repeat",
        IrState::Ref { .. } => "ref_target",
        IrState::ShuntingYard { .. } => "shunting_yard",
        IrState::WsTrim { .. } => "ws_trim",
        IrState::Minus { .. } => "minus",
        IrState::ConsumeToNextStructural { .. } => "consume_to_next_structural",
    }
}

/// AW-III.W5-carry — Emit the CTNS arm body. Collapses the regex
/// scan to a single cursor jump via the stage-1 structural index
/// (when populated) or degrades to ASCII-whitespace trim when the
/// index is empty.
fn emit_consume_to_next_structural_arm(_idx: usize) -> TokenStream {
    let advance = emit_advance_or_pop_call();
    quote! {
        if !idx.positions.is_empty() {
            let slot_idx = *slot as usize;
            if slot_idx < idx.positions.len() {
                *pos = idx.positions[slot_idx];
                *slot = (slot_idx + 1) as u32;
            } else {
                *pos = input.len() as u32;
            }
        } else {
            ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
        }
        stack.pending_variant_idx = u8::MAX;
        #advance
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
            // AW-III.W5.c — slot threaded through alongside pos so
            // the iter-savepoint capture inside the Repeat re-entry
            // path snapshots the dual cursor atomically.
            ::bbnf::runtime::tape::advance_or_pop_with(
                ::core::option::Option::Some(table),
                ::core::option::Option::Some(input),
                columns, frame_depth, psi, stack, pos, slot,
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

/// `Literal { text, payload }` — hoist the runtime destructure into
/// literal `let` bindings that reference the `__DTA_LITERAL_<idx>`
/// static (emitted by `dta.rs`) and the payload's codegen-time value.
///
/// AW-IV.W1.α — the
/// `match table.states[N] { Literal { text, payload } => (text,
/// payload), _ => unreachable_unchecked() }` indirection is replaced
/// with literal bindings. The emitter knows at codegen time that
/// state `idx` IS a `Literal` with the given payload; LLVM sees the
/// payload variant directly in the arm body and elides every branch
/// whose payload class is known never to take.
fn emit_literal_arm(idx: usize, payload: LiteralPayload) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let text_ident = format_ident!("__DTA_LITERAL_{}", idx);
    let payload_tok = literal_payload_token(payload);
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
        let text: &'static str = #text_ident;
        let payload: ::bbnf::runtime::tape::LiteralPayload = #payload_tok;
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
///
/// AW-IV.W1.α — the `match table.states[N]` destructure is hoisted
/// into literal bindings that reference the `__DTA_REGEX_<idx>`
/// static (emitted by `dta.rs`) + the payload's codegen-time value.
/// The scanner.scan indirection stays in this wave; AW-IV.W1.α's
/// second landing replaces it with a direct
/// `__dfa_match_<grammar>_<idx>` call.
fn emit_regex_arm(idx: usize, payload: Option<RegexPayloadKind>) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let pat_ident = format_ident!("__DTA_REGEX_{}", idx);
    let payload_tok = regex_payload_token(payload);
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
        let pattern: &'static str = #pat_ident;
        let payload: ::core::option::Option<::bbnf::runtime::tape::PayloadKind> = #payload_tok;
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

/// `Seq { children, frame, promote }` — hoist the destructure to
/// literal bindings referencing the emitted `__DTA_SEQ_<idx>_CHILDREN`
/// static array + the codegen-time frame + promote variants. Reserve
/// the parent row, push the Seq/Alt/Repeat frame with the rule's
/// variant_idx, and dispatch to `children[0]` (or close immediately
/// if the body is empty).
///
/// AW-IV.W1.α — `match table.states[#idx_lit] { Seq { … } =>
/// (children, frame, promote), _ => unreachable_unchecked() }`
/// replaced with three literal bindings computed at codegen time.
fn emit_seq_arm(
    idx: usize,
    children: &[StateId],
    frame: FrameKind,
    promote: SeqPromote,
) -> TokenStream {
    let _ = idx; // retained for parity with other arms' idx-derived symbols
    let children_ident = format_ident!("__DTA_SEQ_{}_CHILDREN", idx);
    let frame_tok = frame_kind_token(frame);
    let promote_tok = seq_promote_token(promote);
    let advance = emit_advance_or_pop_call();
    let _ = children; // referenced via the emitted static array
    quote! {
        let children: &'static [::bbnf::runtime::tape::DtaStateId] = &#children_ident;
        let frame: ::bbnf::runtime::tape::DtaFrameKind = #frame_tok;
        let promote: ::bbnf::runtime::tape::SeqPromote = #promote_tok;
        let tape_kind = ::bbnf::runtime::tape::frame_to_tape_kind(frame);
        // AW-III.W5.c — fused compound write replaces reserve_compound's
        // 7-Vec::push tax with one bounds-check + 7 unchecked stores.
        let parent_rec = columns.push_compound_fused(tape_kind, *pos);
        frame_depth.push(stack.depth());
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
///
/// AW-IV.W1.α — `branches` binds to the emitted
/// `__DTA_ALT_LIN_<idx>` static array (the same symbol `dta.rs`
/// declares at module scope); the runtime `match table.states[N]`
/// destructure is abrogated.
fn emit_alt_linear_arm(idx: usize, branches: &[StateId]) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let branches_ident = format_ident!("__DTA_ALT_LIN_{}", idx);
    let _ = branches; // referenced via the emitted static array
    quote! {
        let branches: &'static [::bbnf::runtime::tape::DtaStateId] = &#branches_ident;
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
        // AW-III.W5.c — capture dual-cursor's structural slot at Alt
        // entry so failed branches rewind both pos + slot atomically.
        let start_slot = *slot;
        // AW-III.W5.c — fused compound write.
        let parent_rec = columns.push_compound_fused(
            ::bbnf::runtime::tape::TapeKind::Alt, *pos,
        );
        frame_depth.push(start_depth);
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
        // AW-III.W5.c — savepoint captures the dual-cursor slot.
        let sp_after_push = stack.savepoint(*slot);
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
            // AW-III.W5.c — restore slot to pre-Alt-entry so each
            // branch attempt starts with a clean dual cursor.
            *slot = start_slot;
            if let ::core::option::Option::Some(top) = stack.top_mut() {
                top.cursor = branch_idx as u16;
            }
            match ::bbnf::runtime::tape::try_branch(
                table, input, scanner, idx, columns, psi, frame_depth, stack,
                branch, pos, slot, start_depth,
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
///
/// AW-IV.W1.α — `match table.states[N]` for the `inner` state +
/// `counter_optional` presence flag are abrogated; the emitter emits
/// both as literal bindings computed from the codegen-time IR node.
fn emit_repeat_arm(
    idx: usize,
    inner: StateId,
    lo: u32,
    hi: u32,
    counter_optional: ::core::option::Option<CounterOptional>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let inner_lit = Literal::u16_unsuffixed(inner.0);
    let lo_lit = Literal::u32_unsuffixed(lo);
    let hi_lit = Literal::u32_unsuffixed(hi);
    let counter_optional_flag_u8 = if counter_optional.is_some() { 1u8 } else { 0u8 };
    let counter_optional_flag_lit = Literal::u8_unsuffixed(counter_optional_flag_u8);
    let advance = emit_advance_or_pop_call();
    quote! {
        let inner: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#inner_lit);
        // AW-III.W5.c — fused compound write.
        let parent_rec = columns.push_compound_fused(
            ::bbnf::runtime::tape::TapeKind::Rule, *pos,
        );
        frame_depth.push(stack.depth());
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
        let counter_optional_flag: u8 = #counter_optional_flag_lit;
        stack.iter_savepoints.push(::bbnf::runtime::tape::IterSavepoint {
            cols_len: columns.len() as u32,
            fd_len: frame_depth.len() as u32,
            psi_len: psi.len() as u32,
            pay_agg_len: columns.pay_agg.len() as u32,
            pos: *pos,
            // AW-III.W5.c — placeholder; the in-place fill below
            // captures the real slot via `stack.savepoint(*slot)`.
            stack: ::bbnf::runtime::tape::FrameStackSavepoint {
                inline_len: 0,
                overflow_len: 0,
                counters_len: 0,
                op_stack_len: 0,
                iter_savepoints_len: 0,
                slot: 0,
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
        // AW-III.W5.c — captures the dual-cursor slot.
        stack.iter_savepoints[counter_idx].stack = stack.savepoint(*slot);
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
    let target_lit = Literal::u16_unsuffixed(target.0);
    quote! {
        let rule: ::bbnf::runtime::tape::DtaRuleId =
            ::bbnf::runtime::tape::DtaRuleId(#rule_lit);
        let target: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#target_lit);
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
        stack.pending_variant_idx = (rule.0 & 0xFF) as u8;
        ::core::result::Result::Ok(
            ::bbnf::runtime::tape::StepResult::Next(chosen),
        )
    }
}

/// `WsTrim { pattern }` — scanner.scan with the grammar's `@ws`
/// pattern when set, else `trim_ascii_ws` fallback.
///
/// AW-IV.W1.α — the runtime `match table.states[N]` destructure for
/// the pattern field is abrogated. When `has_pattern` is true the
/// emitter emits a literal binding referencing the `__DTA_WS_<idx>`
/// static (the same static `dta.rs` emits for pattern-bearing WsTrim
/// states); when false the emitter emits no pattern binding.
fn emit_ws_trim_arm(idx: usize, has_pattern: bool) -> TokenStream {
    let advance = emit_advance_or_pop_call();
    let (pattern_binding, scan_path) = if has_pattern {
        let pat_ident = format_ident!("__DTA_WS_{}", idx);
        (
            quote! {
                let pattern: ::core::option::Option<&'static str> =
                    ::core::option::Option::Some(#pat_ident);
            },
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
            },
        )
    } else {
        (
            TokenStream::new(),
            quote! {
                ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
            },
        )
    };
    quote! {
        #pattern_binding
        #scan_path
        #advance
    }
}

/// `Minus { primary, excluded }` — deep-snapshot probe of `excluded`.
/// On success → Syntax (the matched bytes were excluded). On failure
/// → restore and dispatch `primary`.
///
/// AW-IV.W1.α — `primary` / `excluded` hoisted to literal bindings
/// computed from the codegen-time IR node.
fn emit_minus_arm(
    idx: usize,
    primary: StateId,
    excluded: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let primary_lit = Literal::u16_unsuffixed(primary.0);
    let excluded_lit = Literal::u16_unsuffixed(excluded.0);
    quote! {
        let primary: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#primary_lit);
        let excluded: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#excluded_lit);
        let start_pos = *pos;
        // AW-III.W5.c — capture dual-cursor slot for probe restore.
        let probe_snapshot = stack.snapshot_probe(*slot);
        let cols_len = columns.len();
        let fd_len = frame_depth.len();
        let psi_len = psi.len();
        let pay_agg_len = columns.pay_agg.len();
        let start_depth = stack.depth();
        let probe = ::bbnf::runtime::tape::try_branch(
            table, input, scanner, idx, columns, psi, frame_depth, stack,
            excluded, pos, slot, start_depth,
        );
        columns.truncate(cols_len);
        frame_depth.truncate(fd_len);
        psi.truncate(psi_len);
        columns.pay_agg.truncate(pay_agg_len);
        // AW-III.W5.c — restore slot from probe snapshot before
        // restoring the stack's deeper state.
        *slot = probe_snapshot.base.slot;
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
    head: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let head_lit = Literal::u16_unsuffixed(head.0);
    quote! {
        let head: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#head_lit);
        // AW-III.W5.c — fused compound write.
        let parent_rec = columns.push_compound_fused(
            ::bbnf::runtime::tape::TapeKind::Rule, *pos,
        );
        frame_depth.push(stack.depth());
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

// ── Literal-binding tokenisers ──────────────────────────────────────
//
// AW-IV.W1.α — the per-arm hoisting emits literal `let` bindings
// computed from the codegen-time `DtaState` value. These free
// functions project the IR variants onto tape-side constant
// expressions — the same projection the dta.rs emitter performs for
// the `DTA_TABLE` literal, reproduced here to avoid cross-emitter
// coupling (lower_state owns the walker-local projection; dta.rs
// owns the const-table projection; both consume the same IR
// variants).

/// Emit the tape-side `LiteralPayload` constructor expression for
/// the given IR payload variant.
fn literal_payload_token(p: LiteralPayload) -> TokenStream {
    match p {
        LiteralPayload::None => {
            quote! { ::bbnf::runtime::tape::LiteralPayload::None }
        }
        LiteralPayload::U8(v) => {
            let lit = Literal::u8_unsuffixed(v);
            quote! { ::bbnf::runtime::tape::LiteralPayload::U8(#lit) }
        }
        LiteralPayload::Bool(b) => {
            let lit = if b { quote!(true) } else { quote!(false) };
            quote! { ::bbnf::runtime::tape::LiteralPayload::Bool(#lit) }
        }
        LiteralPayload::U32(v) => {
            let lit = Literal::u32_unsuffixed(v);
            quote! { ::bbnf::runtime::tape::LiteralPayload::U32(#lit) }
        }
        LiteralPayload::U64(v) => {
            let lit = Literal::u64_unsuffixed(v);
            quote! { ::bbnf::runtime::tape::LiteralPayload::U64(#lit) }
        }
        LiteralPayload::F64(v) => {
            // Mirror dta.rs: finite floats round-trip via
            // `Literal::f64_unsuffixed`; non-finite (NaN, ±Inf)
            // reconstitute via `f64::from_bits` so bit identity is
            // preserved across the codegen boundary.
            if v.is_finite() {
                let lit = Literal::f64_unsuffixed(v);
                quote! { ::bbnf::runtime::tape::LiteralPayload::F64(#lit) }
            } else {
                let bits = Literal::u64_unsuffixed(v.to_bits());
                quote! {
                    ::bbnf::runtime::tape::LiteralPayload::F64(
                        f64::from_bits(#bits),
                    )
                }
            }
        }
    }
}

/// Emit the tape-side `Option<PayloadKind>` constructor expression
/// for the IR-side `Option<RegexPayloadKind>` selector.
fn regex_payload_token(p: Option<RegexPayloadKind>) -> TokenStream {
    let Some(kind) = p else {
        return quote! { ::core::option::Option::None };
    };
    let variant = match kind {
        RegexPayloadKind::F64 => quote! { ::bbnf::runtime::tape::PayloadKind::F64 },
        RegexPayloadKind::U8 => quote! { ::bbnf::runtime::tape::PayloadKind::U8 },
        RegexPayloadKind::Bool => quote! { ::bbnf::runtime::tape::PayloadKind::Bool },
        RegexPayloadKind::HexU32 => {
            quote! { ::bbnf::runtime::tape::PayloadKind::HexU32 }
        }
        RegexPayloadKind::I64 => quote! { ::bbnf::runtime::tape::PayloadKind::I64 },
        RegexPayloadKind::String => {
            quote! { ::bbnf::runtime::tape::PayloadKind::String }
        }
        RegexPayloadKind::AggregateLarge => {
            quote! { ::bbnf::runtime::tape::PayloadKind::AggregateLarge }
        }
    };
    quote! { ::core::option::Option::Some(#variant) }
}

/// Emit the tape-side `DtaFrameKind` variant expression for the IR
/// `FrameKind`.
fn frame_kind_token(f: FrameKind) -> TokenStream {
    match f {
        FrameKind::Seq => quote! { ::bbnf::runtime::tape::DtaFrameKind::Seq },
        FrameKind::Alt => quote! { ::bbnf::runtime::tape::DtaFrameKind::Alt },
        FrameKind::Repeat => quote! { ::bbnf::runtime::tape::DtaFrameKind::Repeat },
        FrameKind::ShuntingYard => {
            quote! { ::bbnf::runtime::tape::DtaFrameKind::ShuntingYard }
        }
    }
}

/// Emit the tape-side `SeqPromote` variant expression for the IR
/// `SeqPromote`.
fn seq_promote_token(p: SeqPromote) -> TokenStream {
    match p {
        SeqPromote::Default => {
            quote! { ::bbnf::runtime::tape::SeqPromote::Default }
        }
        SeqPromote::KvPair => {
            quote! { ::bbnf::runtime::tape::SeqPromote::KvPair }
        }
    }
}
