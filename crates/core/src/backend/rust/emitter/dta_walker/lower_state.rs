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
//! - `Regex { pattern, payload }` — inline DFA body splice (labelled
//!   `'__dfa:` block yielding `Option<u32>`) + emit_leaf_with_payload
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
//! - `WsTrim { pattern }` — codegen-time inline DFA body splice when
//!   pattern is Some; `trim_ascii_ws` otherwise; advance.
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
use bbnf_ir::{GrammarIR, StringId};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

use super::super::dfa_codegen;
use super::decoders::emit_eisel_lemire_inline_body;
use super::helpers::{
    emit_advance_or_pop_inline, emit_close_compound_inline, emit_emit_leaf_inline,
    emit_emit_leaf_with_payload_inline, emit_psi_push_inline,
};
use super::hot_cold::HotColdPartition;
use super::regex_scan_adapter_ident;

/// Emit the body of the outer `match cur { ... }` dispatch.
///
/// Hot states emit as inline arms with the lowered state body. Cold
/// states emit as forwarders to their `#[cold]` sibling — the outer
/// dispatch sees a single uniform `Result<StepResult, DtaError>`
/// return shape.
///
/// AW-IV.W1.4-aggro — the `ir` + `table` arguments thread through to
/// the per-state Regex / WsTrim arms so those arms can splice the DFA's
/// `loop { match state { ... } }` body directly at the call site via
/// [`dfa_codegen::emit_dfa_inline_body`]. There is no separately-emitted
/// `__dfa_match_*` fn; the hot path's walker arm IS the DFA match loop,
/// visible to LLVM as a straight-line match basic block (no function
/// call boundary anywhere on the hot regex / WsTrim path).
pub(super) fn emit_state_dispatch_arms(
    grammar: &str,
    ir: &GrammarIR,
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
                let body = emit_state_arm_body(grammar, ir, table, idx, state);
                quote! {
                    #id_lit => { #body }
                }
            } else {
                let cold_ident = cold_sibling_ident(id);
                quote! {
                    #id_lit => {
                        #cold_ident(
                            input, idx, columns, psi, frame_depth,
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
///
/// AW-IV.W1.4-aggro — cold siblings consume the same `emit_state_arm_body`
/// that the hot path does, so their Regex / WsTrim arms also splice the
/// DFA body inline (no fn-call boundary inside the cold sibling
/// either). The per-grammar `__regex_scan_<grammar>` adapter — emitted
/// by `dfa_codegen::emit_regex_scan_adapter` at grammar scope — is used
/// only by the replay-surface helpers (`try_branch`,
/// `handle_repeat_failure_bounded`) the cold sibling dispatches into
/// via fn-pointer, not for the DFA match itself.
pub(super) fn emit_cold_siblings(
    grammar: &str,
    ir: &GrammarIR,
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
            let body = emit_state_arm_body(grammar, ir, table, idx, state);
            Some(quote! {
                #[cold]
                #[inline(never)]
                #[allow(clippy::too_many_arguments)]
                fn #cold_ident(
                    input: &[u8],
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
                    // AW-III.W5.c — cold siblings carry the dual
                    // cursor (`slot`) + structural index (`idx`)
                    // through their parameter list so the W5 helpers
                    // (try_branch, handle_repeat_failure_bounded)
                    // receive uniform arguments matching the hot
                    // path's invocation shape.
                    //
                    // AW-IV.W1.4-aggro — the Regex / WsTrim arms splice
                    // the DFA's `loop { match state { ... } }` body
                    // directly from `dfa_codegen::emit_dfa_inline_body`;
                    // no fn-call boundary sits on the cold path's DFA
                    // walk either. Replay-surface helpers consume the
                    // per-grammar `__regex_scan_<grammar>` fn-pointer
                    // adapter as their regex-scan argument.
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
/// AW-IV.W1.4-aggro — every arm body opens with literal `let` bindings
/// computed from the codegen-time `DtaState` value; the runtime
/// `match table.states[N] { Variant { fields } => (fields), _ =>
/// unreachable_unchecked() }` unpack is abrogated. `ir` + `table`
/// thread through so the Regex and WsTrim arms can splice the DFA's
/// loop body inline via [`dfa_codegen::emit_dfa_inline_body`]. No
/// function call separates the walker arm from the DFA match loop; the
/// arm body contains the DFA state machine as a labelled block.
fn emit_state_arm_body(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
    state: &IrState,
) -> TokenStream {
    let kind_tag = state_kind_tag(state);
    let body = match state {
        IrState::Epsilon => emit_epsilon_arm(idx),
        IrState::Literal { text: _, payload } => {
            emit_literal_arm(idx, *payload)
        }
        IrState::Regex { pattern, payload } => {
            emit_regex_arm(grammar, ir, table, idx, *pattern, *payload)
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
            emit_alt_linear_arm(grammar, idx, branches)
        }
        IrState::Repeat { inner, lo, hi, counter_optional } => {
            emit_repeat_arm(idx, *inner, *lo, *hi, *counter_optional)
        }
        IrState::Ref { rule, target } => {
            emit_ref_arm(idx, *rule, *target)
        }
        IrState::WsTrim { pattern } => {
            emit_ws_trim_arm(grammar, ir, table, idx, pattern.is_some())
        }
        IrState::Minus { primary, excluded } => {
            emit_minus_arm(grammar, idx, *primary, *excluded)
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

/// AW-IV.W2.1 — emit the post-leaf advance pattern with the Seq-fast-
/// path splice inline.
///
/// The fast path (peek top + Seq cursor advance) was a cross-crate
/// call to `bbnf_tape::advance_seq_fast` before W2.1; it now splices
/// its body verbatim into each arm via
/// [`emit_advance_or_pop_inline`]. The non-Seq fall-through
/// (`advance_or_pop_with` — Alt close, Repeat re-entry, SY reducer,
/// stack drain) remains behind a single call boundary reached only
/// from the ≤20% minority path; its ~250-line body covers the SY
/// reducer's precedence loop and would explode per-arm code size if
/// inlined at every leaf-emit site.
///
/// JSON twitter visits a Seq frame for ~80% of leaf emit sites per
/// the post-AW-III samply; the spliced fast path folds the dominant
/// case directly into the calling arm.
fn emit_advance_or_pop_call() -> TokenStream {
    emit_advance_or_pop_inline()
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
    let rec_ident = format_ident!("_rec");
    // AW-IV.W2.1 — splice `emit_leaf` / `emit_leaf_with_payload`
    // bodies inline. The cross-crate fn call was the per-leaf
    // dispatch boundary post-W1; inlining the variant-resolution +
    // column push at the arm level flattens the hot path.
    let payload_arm = if matches!(payload, LiteralPayload::None) {
        let emit = emit_emit_leaf_inline(
            &rec_ident,
            quote! { ::bbnf::runtime::tape::TapeKind::Literal },
            quote! { lo },
            quote! { *pos },
        );
        quote! { #emit }
    } else {
        let emit = emit_emit_leaf_with_payload_inline(
            &rec_ident,
            quote! { ::bbnf::runtime::tape::TapeKind::Span },
            quote! { lo },
            quote! { *pos },
            quote! { arena_off },
        );
        quote! {
            let arena_off = ::bbnf::runtime::tape::stage_literal_payload_in_arena(
                columns, payload,
            );
            #emit
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

/// `Regex { pattern, payload }` — inline DFA (or NEON string-scan)
/// body splice; emit_leaf with `TapeKind::Span`; inline f64 decode
/// or PSI push for other payload kinds.
///
/// AW-IV.W1.4-aggro — the `match table.states[N]` destructure is
/// hoisted into a literal `payload` binding; the `pattern` field is
/// encoded into the DFA's byte-classes / state-arms whose body is
/// spliced directly into this arm by
/// [`dfa_codegen::emit_dfa_inline_body`]. There is no fn call; the arm
/// contains the DFA's `loop { match state { ... } }` as a labelled
/// block returning `Option<u32>`. LLVM sees the walker arm, the DFA
/// state machine, and the leaf-emission body as one straight-line basic
/// block — no function-call boundary anywhere on the regex hot path.
///
/// AW-IV.W2.3.a — two inline-decoder substitutions driven by IR
/// classification:
///
/// - If the pattern's [`RegexInfo`](parse_that::regex::RegexInfo)
///   classification is [`RegexClass::QuotedString`], the DFA body is
///   replaced by the [`emit_neon_string_scan_inline_body`] splice — the
///   portable-SIMD backslash-parity scanner mirroring
///   `parse_that::parsers::scan::quoted_simd::scan_quoted_string_simd`.
///   On Apple-M / aarch64 NEON + AVX2 targets the SIMD path outstrips
///   the general DFA on quoted-string scanning by ~10× per the
///   parse-that benchmarks.
/// - If the payload is [`RegexPayloadKind::F64`], the PSI scheduling
///   call is replaced by the [`emit_eisel_lemire_inline_body`] splice
///   + a direct 8-byte write into `columns.pay_agg[arena_off..
///   arena_off + 8]`. No `PayloadJob` enters the stream for this arm;
///   the Stage-B worker never visits this record. The existing
///   `pay_agg.resize(..)` pre-reservation guarantees the write is
///   in-bounds (W2.3.b will later elide that via capacity
///   pre-allocation in `Columns::with_capacity_for`).
///
/// Other payload kinds (U8, Bool, HexU32, I64, String, AggregateLarge)
/// retain the existing PSI-scheduled path — W2.3.c handles the
/// remaining inline-decodable scalars.
fn emit_regex_arm(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
    pattern: StringId,
    payload: Option<RegexPayloadKind>,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let payload_tok = regex_payload_token(payload);
    let advance = emit_advance_or_pop_call();

    // AW-IV.W2.3.a — detect QuotedString classification; splice the
    // NEON / portable-SIMD scan body instead of the generic DFA body.
    // The NEON path manually consumes the opening quote byte and then
    // runs the 16-byte SIMD backslash-parity scan over the body.
    let scan_body = emit_regex_scan_body(grammar, ir, table, idx, pattern);

    // AW-IV.W2.3.a — splice emit_leaf / emit_leaf_with_payload; for
    // F64 payload, inline Eisel-Lemire decode + direct column write
    // instead of psi.push scheduling.
    let rec_ident = format_ident!("_rec_none");
    let emit_leaf_none = emit_emit_leaf_inline(
        &rec_ident,
        quote! { ::bbnf::runtime::tape::TapeKind::Span },
        quote! { lo },
        quote! { *pos },
    );

    let emit_payload = match payload {
        None => quote! { #emit_leaf_none },
        Some(RegexPayloadKind::F64) => {
            // AW-IV.W2.3.a inline-decode for F64 payload. Eisel-Lemire
            // body decodes the matched byte range to an f64; the
            // result is written directly into the arena at `arena_off`
            // as 8 little-endian bytes. No psi.push scheduling — the
            // Stage-B worker does not visit this record.
            let rec_ident_some = format_ident!("_rec_some");
            let emit_leaf_some = emit_emit_leaf_with_payload_inline(
                &rec_ident_some,
                quote! { ::bbnf::runtime::tape::TapeKind::Span },
                quote! { lo },
                quote! { *pos },
                quote! { ::bbnf::runtime::tape::TapeOffset(arena_off) },
            );
            let eisel_lemire_splice = emit_eisel_lemire_inline_body();
            quote! {
                // Reserve the arena slot for the decoded f64.
                let arena_off = columns.pay_agg.len() as u32;
                columns.pay_agg.resize(arena_off as usize + 8, 0);
                let _rec_idx = columns.len() as u32;
                #emit_leaf_some
                // AW-IV.W2.3.a inline-decode — Eisel-Lemire body
                // spliced verbatim. The decoder returns Some(f) on
                // clean decode, None on the ambiguous-rounding case;
                // the fallback uses fast_float2::parse on the matched
                // string (the ~0.01% slow path).
                #eisel_lemire_splice
                let __f64_value: f64 = match __decoded_f64 {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => {
                        // Cold-path ambiguous-rounding fallback
                        // (~0.01% of inputs per the `compute_f64`
                        // docs). Routes through
                        // `parse_that::parse_number_f64` — the public
                        // fn that wraps `fast_float2::parse`. Kept as a
                        // single out-of-line call boundary because the
                        // full fast-float2 decoder (~1000 lines) would
                        // explode code size when inlined per arm
                        // without measurable benefit at 0.01%
                        // incidence.
                        let __slice = unsafe {
                            input.get_unchecked(
                                lo as usize
                                ..(lo as usize).wrapping_add(match_len as usize),
                            )
                        };
                        let __s = match ::core::str::from_utf8(__slice) {
                            ::core::result::Result::Ok(s) => s,
                            ::core::result::Result::Err(_) => "0",
                        };
                        ::parse_that::parse_number_f64(__s)
                    }
                };
                // Direct column write — the arena slot was reserved
                // above; the 8-byte little-endian write goes through
                // the unchecked pointer path so the bounds check
                // collapses at codegen time. W2.3.b will later elide
                // the reservation as well via input-length-driven
                // pre-allocation.
                let __bits = __f64_value.to_bits().to_le_bytes();
                unsafe {
                    ::core::ptr::copy_nonoverlapping(
                        __bits.as_ptr(),
                        columns.pay_agg.as_mut_ptr().add(arena_off as usize),
                        8,
                    );
                }
            }
        }
        Some(_) => {
            // Non-F64 payload kinds retain the PSI-scheduled path —
            // W2.3.c covers the remaining inline-decodable scalars
            // (U8, Bool, HexU32) and shape-aware scalar emissions.
            let rec_ident_some = format_ident!("_rec_some");
            let emit_leaf_some = emit_emit_leaf_with_payload_inline(
                &rec_ident_some,
                quote! { ::bbnf::runtime::tape::TapeKind::Span },
                quote! { lo },
                quote! { *pos },
                quote! { ::bbnf::runtime::tape::TapeOffset(arena_off) },
            );
            let psi_push = emit_psi_push_inline(
                quote! { rec_idx },
                quote! { lo },
                quote! { *pos },
                quote! { kind },
                quote! { arena_off },
            );
            quote! {
                if let ::core::option::Option::Some(kind) = payload {
                    let width = match (kind, kind.arena_byte_width()) {
                        (::bbnf::runtime::tape::PayloadKind::String, _) => {
                            4 + match_len as usize
                        }
                        (_, 0) => match_len as usize,
                        (_, w) => w,
                    };
                    let arena_off = columns.pay_agg.len() as u32;
                    columns.pay_agg.resize(arena_off as usize + width, 0);
                    let rec_idx = columns.len() as u32;
                    #emit_leaf_some
                    #psi_push
                } else {
                    #emit_leaf_none
                }
            }
        }
    };

    quote! {
        let payload: ::core::option::Option<::bbnf::runtime::tape::PayloadKind> = #payload_tok;
        // AW-IV.W1.4-aggro / W2.3.a — splice the scan body inline. The
        // inner block shadows `pos` with a local `usize` so the scan
        // body's `pos: usize` initialisation binds against a concrete
        // type; the block's value is `Option<u32>` (the matched prefix
        // length). LLVM sees the walker arm's dispatch, the scan state
        // machine, and the leaf-emission sequence as one straight-line
        // basic block — zero function-call boundary on the regex hot
        // path.
        let dfa_result: ::core::option::Option<u32> = {
            let pos: usize = *pos as usize;
            #scan_body
        };
        let match_len = match dfa_result {
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

/// AW-IV.W2.3.a — Emit the scan body for a Regex arm.
///
/// Reads the pattern's classification from `ir.regex_info`; when the
/// classification is [`RegexClass::QuotedString`], splices the
/// portable-SIMD scan body from
/// [`emit_neon_string_scan_inline_body`]; otherwise falls back to the
/// generic DFA inline body from
/// [`dfa_codegen::emit_dfa_inline_body`].
///
/// The NEON scan body expects `input: &[u8]` and `start: usize`
/// bindings from the surrounding scope; the wrapping arm binds these
/// after consuming the opening quote byte. The block's value is
/// `Option<usize>` (the closing-quote byte offset); the wrapper
/// converts it to `Option<u32>` match-length for uniformity with the
/// DFA path.
///
/// Falls through to the DFA path for every non-QuotedString pattern,
/// for patterns with missing `regex_info` entries, and for quote bytes
/// outside the ASCII printable range (the NEON splice's compile-time
/// literal cannot accommodate non-representable quote bytes).
fn emit_regex_scan_body(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
    pattern: StringId,
) -> TokenStream {
    // AW-IV.W2.3.a (intermediate) — QuotedString dispatch lands in the
    // follow-on commit that wires `emit_neon_string_scan_inline_body`.
    // This intermediate hop accepts the `pattern: StringId` argument
    // so the walker's Regex arm can pivot on classification without
    // the QuotedString branch firing yet.
    let _ = (ir, pattern);
    dfa_codegen::emit_dfa_inline_body(grammar, ir, table, idx)
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
    let close = emit_close_compound_inline(quote! { *pos });
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
            // AW-IV.W2.1 — close_compound body spliced inline.
            #close
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
/// destructure is abrogated. The `try_branch` helper gains the
/// per-grammar `__regex_scan_<grammar>` adapter as its regex-scan
/// argument instead of the removed `scanner: &dyn RegexScanner`.
fn emit_alt_linear_arm(
    grammar: &str,
    idx: usize,
    branches: &[StateId],
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let branches_ident = format_ident!("__DTA_ALT_LIN_{}", idx);
    let regex_scan_ident = regex_scan_adapter_ident(grammar);
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
                table, input, #regex_scan_ident, idx, columns, psi, frame_depth, stack,
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
    let close = emit_close_compound_inline(quote! { *pos });
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
            // AW-IV.W2.1 — close_compound body spliced inline.
            #close
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

/// `WsTrim { pattern }` — codegen-time inline DFA body splice when
/// the state carries a pattern, `trim_ascii_ws` otherwise.
///
/// AW-IV.W1.4-aggro — the pattern's presence is known at codegen time;
/// pattern-bearing WsTrim states splice the DFA's loop body inline via
/// [`dfa_codegen::emit_dfa_inline_body`] inside a fixed-point `loop {
/// ... }` that drains every consecutive whitespace run. Pattern-less
/// states fall to `trim_ascii_ws`. No fn-call boundary appears on the
/// WsTrim hot path — the DFA state machine is spliced as a labelled
/// block returning `Option<u32>` per iteration.
fn emit_ws_trim_arm(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
    has_pattern: bool,
) -> TokenStream {
    let advance = emit_advance_or_pop_call();
    let scan_path = if has_pattern {
        let dfa_inline_body = dfa_codegen::emit_dfa_inline_body(grammar, ir, table, idx);
        // AW-IV.W1.4-aggro — drive the DFA body inline in a fixed-
        // point loop. Each iteration shadows `pos` locally with the
        // current `*pos as usize` so the DFA body's `__dfa_p: usize =
        // pos` + `pos as u32` subtraction bind against a concrete
        // `usize`. On a non-zero match, advance the walker's `*pos` by
        // the matched length; on `Some(0)` / `None` the loop breaks.
        // The `*pos` mutation happens outside the shadowed scope so
        // the walker's `&mut u32` is not aliased.
        quote! {
            loop {
                let dfa_result: ::core::option::Option<u32> = {
                    let pos: usize = *pos as usize;
                    #dfa_inline_body
                };
                match dfa_result {
                    ::core::option::Option::Some(0)
                    | ::core::option::Option::None => break,
                    ::core::option::Option::Some(len) => {
                        *pos += len;
                    }
                }
            }
        }
    } else {
        quote! {
            ::bbnf::runtime::tape::trim_ascii_ws(input, pos);
        }
    };
    quote! {
        #scan_path
        #advance
    }
}

/// `Minus { primary, excluded }` — deep-snapshot probe of `excluded`.
/// On success → Syntax (the matched bytes were excluded). On failure
/// → restore and dispatch `primary`.
///
/// AW-IV.W1.α — `primary` / `excluded` hoisted to literal bindings
/// computed from the codegen-time IR node; `try_branch` takes the
/// per-grammar `__regex_scan_<grammar>` fn pointer instead of the
/// deleted `scanner: &dyn RegexScanner`.
fn emit_minus_arm(
    grammar: &str,
    idx: usize,
    primary: StateId,
    excluded: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let primary_lit = Literal::u16_unsuffixed(primary.0);
    let excluded_lit = Literal::u16_unsuffixed(excluded.0);
    let regex_scan_ident = regex_scan_adapter_ident(grammar);
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
            table, input, #regex_scan_ident, idx, columns, psi, frame_depth, stack,
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
