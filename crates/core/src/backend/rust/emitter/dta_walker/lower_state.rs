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
use super::decoders::{
    emit_eisel_lemire_inline_body, emit_neon_17digit_fractional_inline_body,
    emit_neon_string_scan_inline_body,
};
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
                            stack, pos, slot, bloom_dedup,
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
                    bloom_dedup: &mut ::bbnf::runtime::tape::BloomDedup,
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
            emit_seq_arm(ir, table, idx, children, *frame, *promote)
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
            emit_alt_linear_arm(grammar, ir, table, idx, branches)
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

/// AW-III.W5-carry / AW-IV.W3.5b — Emit the CTNS arm body. Collapses
/// the regex scan to a single cursor jump via the stage-1 structural
/// index.
///
/// AW-IV.W3.5b — the arm now emits a `TapeKind::Scanned` leaf carrying
/// the scanned byte range so downstream grammar consumers that expect
/// a leaf record for this Regex NodeId still see one. The record's
/// `(span_lo, span_hi)` mirrors what the pre-lift Regex arm would have
/// emitted as `TapeKind::Span`, but the distinct kind lets consumers
/// distinguish CTNS-produced records from full-DFA scans.
///
/// The cursor-jump logic mirrors `bbnf-tape::driver::dispatch_one`'s
/// WsTrim arm: lazy slot resync (advance `*slot` past any stale
/// entries whose position is at-or-before `*pos`) then jump `*pos`
/// to `idx.positions[*slot]`. Literal / Regex arms don't currently
/// update `slot` when they consume bytes, so stale entries must be
/// skipped before reading the next structural position.
///
/// Empty-index fall-through: when the structural index has no
/// entries ahead of `*pos` (inputs whose trailing bytes hold no
/// structural marker, or grammars without stage-1 enrichment), the
/// arm consumes to end of input — the CTNS admission invariant
/// (`matchable ∩ structural = ∅`) guarantees every byte from `lo`
/// to `input.len()` is matchable by the pattern.
///
/// Zero-width-match guard: the CTNS lifter only admits `+` /
/// bounded-repetition patterns (no `*`), so a zero-width scan is a
/// match failure. The arm raises Syntax in that case to preserve
/// the source Regex arm's semantics.
fn emit_consume_to_next_structural_arm(idx: usize) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let advance = emit_advance_or_pop_call();
    let rec_ident = format_ident!("_rec_scanned");
    let emit_leaf = emit_emit_leaf_inline(
        &rec_ident,
        quote! { ::bbnf::runtime::tape::TapeKind::Scanned },
        quote! { lo },
        quote! { *pos },
    );
    quote! {
        let lo = *pos;
        // Lazy slot resync — skip any index entries whose position
        // lies at-or-before the current cursor. Literal / Regex arms
        // don't maintain `slot` during their byte scans, so by the
        // time the CTNS arm runs, `*slot` may still point at a stale
        // entry strictly earlier than `*pos`.
        while (*slot as usize) < idx.positions.len()
            && idx.positions[*slot as usize] <= *pos
        {
            *slot += 1;
        }
        let slot_idx = *slot as usize;
        if slot_idx < idx.positions.len() {
            *pos = idx.positions[slot_idx];
            *slot = (slot_idx + 1) as u32;
        } else {
            *pos = input.len() as u32;
        }
        // `+` quantifier requires at least one byte; a zero-width
        // scan (cursor already at end-of-input, or structural byte
        // at the current position) surfaces a Syntax error matching
        // what the source Regex arm would emit.
        if *pos == lo {
            break 'step ::core::result::Result::Err(
                ::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *pos,
                    failing_state: ::bbnf::runtime::tape::DtaStateId(#idx_lit as u16),
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                },
            );
        }
        #emit_leaf
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

    // AW-IV.W3.5c — bounded-Regex admission scaffolding.
    //
    // The `last_byte_set` mined at
    // `crates/ir/src/passes/recognizers/pattern_alphabet.rs` captures
    // the bytes that can terminate a pattern match. A naive
    // "bounded-scan when `last_byte_set ∩ structural = ∅`" admission
    // is UNSOUND: for patterns like CSS pretty's `ws =
    // (?s)(?:\s|\/\*...\*\/)*` the last-byte set is disjoint from
    // structural, but the pattern's INTERIOR (the comment body) can
    // match structural bytes. Slicing the input at the next
    // structural byte truncates mid-comment and the DFA returns a
    // shorter match (or an empty match for `*` quantifiers), breaking
    // semantics.
    //
    // The sound admission is the same as CTNS — `matchable ∩
    // structural = ∅` — which the existing CTNS lifter already
    // converts from a bounded scan into a pure cursor jump. A
    // bounded-scan emission only adds value over CTNS when the
    // pattern's full alphabet is NOT disjoint from structural but
    // still yields a safe upper bound; the general case requires
    // per-run DFA state analysis beyond the codegen-time budget.
    //
    // The admission helper is kept as a declaration so the
    // `last_byte_set` field stays observably consumed; future work
    // (AX or later) tightens the admission to a provably-sound
    // condition. No bounded-scan emission happens today.
    let _bounded_scan_admission = payload.is_none()
        && admits_bounded_regex_scan(ir, pattern);
    let bound_prelude = TokenStream::new();
    let scan_body_wrapped = scan_body;

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
            let neon_17digit_splice = emit_neon_17digit_fractional_inline_body();
            quote! {
                // Reserve the arena slot for the decoded f64. The
                // 8-byte slot mirrors `PayloadKind::F64::arena_byte_width`;
                // W2.3.b will later elide this reservation via
                // input-length-driven pre-allocation in Columns.
                let arena_off = columns.pay_agg.len() as u32;
                columns.pay_agg.resize(arena_off as usize + 8, 0);
                #emit_leaf_some
                // AW-IV.W2.3.a inline-decode — Eisel-Lemire body
                // spliced verbatim. The decoder returns Some(f) on
                // clean decode, None on the ambiguous-rounding case
                // (~0.01% incidence per the `compute_f64` docs).
                #eisel_lemire_splice
                let __f64_value: f64 = match __decoded_f64 {
                    ::core::option::Option::Some(v) => v,
                    ::core::option::Option::None => {
                        // AW-IV.W4.2.c — NEON 17-digit fractional
                        // fallback. Resolves the canada-shaped
                        // ambiguous-rounding case inline (load + fold
                        // + single correctly-rounded division) so
                        // the cross-crate `parse_number_f64` boundary
                        // doesn't appear on the hot path for the
                        // fractional-heavy JSON corpus. A residual
                        // `None` arm (inputs outside the 17-digit
                        // admission window) still routes to
                        // `parse_number_f64` as the correctness
                        // ground truth.
                        #neon_17digit_splice
                        match __neon_decoded_f64 {
                            ::core::option::Option::Some(v) => v,
                            ::core::option::Option::None => {
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
                        }
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
        // AW-IV.W3.5c — bounded-Regex prelude: compute the upper
        // bound + resync `*slot` before the scan body shadows `pos`
        // and `input`. Empty for unbounded scans.
        #bound_prelude
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
            #scan_body_wrapped
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

/// AW-IV.W3.5c — codegen-time admission test for the bounded-Regex
/// walker arm.
///
/// Returns `true` when the pattern's mined last-byte set is disjoint
/// from the grammar's structural alphabet: the pattern terminates at-
/// or-before the next structural byte, so the walker may cap the DFA's
/// scan at `idx.positions[slot]` without risking a truncated mid-match.
///
/// The lookup walks every `NodeId` carrying this pattern's
/// `StringId` — the `PatternAlphabetMiner` keys its output by
/// `NodeId`, but the last-byte-set itself depends only on the pattern
/// text, so any matching entry is authoritative. A successful match
/// requires `is_last_byte_tight` (the NFA computation succeeded) AND
/// disjointness from the structural alphabet.
///
/// Returns `false` when:
/// - The grammar has no mined structural alphabet (bounded scan has
///   no upper bound to apply).
/// - No `PatternAlphabet` entry exists for any `NodeId` with this
///   pattern.
/// - `is_last_byte_tight == false` for every matching entry (NFA
///   construction failed or zero-width accept).
/// - The `last_byte_set` intersects the structural alphabet — the
///   pattern can accept on a structural byte, so the bounded slice
///   would exclude a valid match.
fn admits_bounded_regex_scan(ir: &GrammarIR, pattern: StringId) -> bool {
    let Some(structural) = ir.structural_alphabet.as_ref() else {
        return false;
    };
    let structural_mask = structural.singletons_mask();
    if structural_mask.iter().all(|&w| w == 0) {
        return false;
    }

    // Find any `NodeId` whose `DagNode::Regex` carries this pattern
    // string. The miner populates `ir.pattern_alphabets` per NodeId,
    // but the computed last-byte-set depends only on the pattern
    // text, so any matching entry suffices.
    let Some(dag) = ir.dag.as_ref() else {
        return false;
    };
    for (node_id, dag_node) in dag.iter() {
        if let bbnf_ir::dag::DagNode::Regex(sid) = dag_node {
            if *sid == pattern {
                if let Some(alphabet) = ir.pattern_alphabets.get(&node_id) {
                    if !alphabet.is_last_byte_tight {
                        continue;
                    }
                    let last = &alphabet.last_byte_set;
                    let disjoint = (last[0] & structural_mask[0]) == 0
                        && (last[1] & structural_mask[1]) == 0
                        && (last[2] & structural_mask[2]) == 0
                        && (last[3] & structural_mask[3]) == 0;
                    if disjoint {
                        return true;
                    }
                }
            }
        }
    }
    false
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
    if let Some(info) = ir.regex_info.get(&pattern) {
        if let parse_that::regex::classify::RegexClass::QuotedString { quote_char, .. } =
            info.classification
        {
            // AW-IV.W2.3.a — QuotedString splice. Consume the opening
            // quote byte (the pattern starts with the quote), run the
            // NEON scan for the closing quote, and project the result
            // as the match length.
            let neon_body = emit_neon_string_scan_inline_body(quote_char);
            let quote_lit = Literal::u8_unsuffixed(quote_char);
            return quote! {
                '__sscan: {
                    // Opening-quote check: the pattern demands the
                    // matched range start with the quote byte. If the
                    // input at `pos` is not the quote, the match fails
                    // (Option::None) — uniform with the DFA path's
                    // no-match case.
                    let __open = match input.get(pos) {
                        ::core::option::Option::Some(&b) => b,
                        ::core::option::Option::None => {
                            break '__sscan ::core::option::Option::None;
                        }
                    };
                    if __open != #quote_lit {
                        break '__sscan ::core::option::Option::None;
                    }
                    // NEON scan over the content starting after the
                    // opening quote. `start` is the first body byte;
                    // `__sstring` (the block's value) is the closing-
                    // quote offset.
                    let start: usize = pos + 1;
                    #neon_body
                    match __sstring {
                        ::core::option::Option::Some(__close) => {
                            // Matched bytes: [pos, __close + 1).
                            // match_len = (__close + 1) - pos.
                            ::core::option::Option::Some(
                                ((__close + 1) - pos) as u32,
                            )
                        }
                        ::core::option::Option::None => {
                            ::core::option::Option::None
                        }
                    }
                }
            };
        }
    }
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
///
/// # AW-IV.W3.1 — ShapeRef consumer short-circuit
///
/// Pre-W3.1 the Seq arm always fell through to the
/// `push_compound_fused` + frame push + dispatch-to-`children[0]`
/// path. The AW-III.W6.1 [`bbnf_ir::passes::recognizers::shape_dict::
/// ShapeDictMiner`] had been emitting shape-template candidates into
/// [`bbnf_ir::GrammarIR::shape_dict_templates`] and the CSP selector
/// had been pruning them to [`GrammarIR::shape_dict_selection`]; the
/// resulting [`bbnf_tape::ShapeEntry`] slice populated
/// [`bbnf_tape::GrammarProfile::shape_dict`] at the `pub const`
/// literal — but nothing in the walker consumed that data. The
/// consumer was the substrate-without-wire case flagged in AW-III's
/// FINAL-III §ShapeRef-consumer-wiring.
///
/// W3.1 wires the consumer. For each Seq state, the emitter computes
/// the canonical shape hash from the children's per-state
/// discriminants using [`bbnf_ir::passes::recognizers::shape_dict::
/// hash_skeleton_public`] — the same hash the miner produced. If the
/// hash is present in the grammar's admitted shape_dict, the arm
/// opens with a `push_shape_ref` call carrying the matched entry's
/// index, stamping a ShapeRef record at the Seq's `span_lo` as a
/// runtime marker that the compound is shape-eligible. The normal
/// `push_compound_fused` + frame push + children-dispatch path still
/// runs — the downstream AX replay surface uses the ShapeRef marker
/// to recognize collapsed compounds without re-walking the shape
/// dictionary at tape read time.
///
/// The `SHAPE_DICT.lookup(shape_hash)` call shown in AW-IV's plan
/// resolves entirely at codegen time: the emitter knows the shape
/// hash AND the admitted-set membership at emit time, so the
/// generated code carries the compile-time-decided branch as a
/// direct `push_shape_ref` call when a match exists and falls through
/// to the normal compound path otherwise. LLVM sees no runtime
/// dictionary lookup — the decision is baked into the per-state
/// arm's source shape. The `push_shape_ref` helper body still emits
/// out-of-line today; a follow-on wave inlines it per the W2.1
/// inline-emit invariant.
fn emit_seq_arm(
    ir: &GrammarIR,
    table: &DtaTable,
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

    // AW-IV.W3.1 — compile-time shape-dictionary lookup.
    //
    // Project each child `StateId` to a `TemplatePiece` via the same
    // mapping the miner uses for `IrNode` positions, then compute the
    // canonical discriminant-only hash. If the hash matches an
    // admitted shape-dict entry, emit the ShapeRef consumer
    // short-circuit; otherwise fall through to the normal compound
    // path.
    let shape_ref_prologue = emit_shape_ref_prologue(ir, table, children);

    // AW-IV.W4.3 — runtime dedup probe. If this Seq state is the
    // entry of a dedup-eligible rule, emit a post-close probe that
    // runs `BloomDedup::try_dedup` over the just-emitted compound's
    // structural rows. On a hit, rewind the columns and emit a
    // single `push_compound_referring` record pointing at the existing
    // skeleton. No-op when the Seq is not dedup-eligible.
    let dedup_rule_id = dedup_eligible_rule_for_state(ir, table, idx);

    // Probe at open: snapshot the starting row index so the close-time
    // probe knows how many rows this compound occupies.
    let dedup_open = if dedup_rule_id.is_some() {
        quote! {
            let __dedup_start: u32 = columns.len() as u32;
        }
    } else {
        quote! {}
    };

    // Post-close probe for the empty-children branch. When the Seq's
    // children list is empty the close_compound body splices inline
    // above this probe; at that point the compound occupies rows
    // [__dedup_start..columns.len()]. The non-empty-children path
    // closes via `advance_or_pop_with` in a downstream arm; AW-V's
    // frame-kind extension wires the probe through there.
    let dedup_close_empty = match dedup_rule_id {
        ::core::option::Option::Some(rid) => {
            let rid_lit = Literal::u32_unsuffixed(rid);
            quote! {
                let __rec_count: u32 = (columns.len() as u32) - __dedup_start;
                if __rec_count > 0 {
                    if let ::core::option::Option::Some(__existing) =
                        bloom_dedup.try_dedup(columns, __dedup_start, __rec_count)
                    {
                        let __span_lo = columns.span_lo[__dedup_start as usize];
                        let __span_hi = *pos;
                        columns.truncate(__dedup_start as usize);
                        frame_depth.truncate(__dedup_start as usize);
                        ::bbnf::runtime::tape::push_compound_referring(
                            columns,
                            tape_kind,
                            #rid_lit,
                            __existing,
                            (__span_lo, __span_hi),
                        );
                        frame_depth.push(stack.depth());
                    }
                }
            }
        }
        ::core::option::Option::None => quote! {},
    };

    quote! {
        let children: &'static [::bbnf::runtime::tape::DtaStateId] = &#children_ident;
        let frame: ::bbnf::runtime::tape::DtaFrameKind = #frame_tok;
        let promote: ::bbnf::runtime::tape::SeqPromote = #promote_tok;
        let tape_kind = ::bbnf::runtime::tape::frame_to_tape_kind(frame);
        // AW-IV.W3.1 — ShapeRef consumer wire. When this Seq's shape
        // hash is admitted to the grammar's shape-dict, mark a ShapeRef
        // record at the Seq's span_lo; the downstream reader (AX
        // replay surface) uses the marker to collapse the compound
        // subtree to one synthetic ShapeRef leaf at view time.
        #shape_ref_prologue
        // AW-IV.W4.3 — open-side dedup snapshot. Captures the starting
        // row index so the close-time probe can delimit the just-
        // emitted compound.
        #dedup_open
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
            // AW-IV.W4.3 — post-close dedup probe for the empty-
            // children path. Runs `BloomDedup::try_dedup` over the
            // just-emitted compound's row range; on a hit, rewind
            // columns and emit a single `push_compound_referring`.
            #dedup_close_empty
            #advance
        } else {
            ::core::result::Result::Ok(
                ::bbnf::runtime::tape::StepResult::Next(children[0]),
            )
        }
    }
}

/// AW-IV.W4.3 — look up the dedup-eligible rule id for a Seq state.
///
/// Returns `Some(rule_id)` when the Seq state is the entry point of a
/// rule whose id appears in `GrammarIR::dedup_eligible_rules`; `None`
/// otherwise. The emitter uses this to decide whether to emit the
/// dedup probe at this Seq arm.
fn dedup_eligible_rule_for_state(
    ir: &GrammarIR,
    table: &DtaTable,
    state_idx: usize,
) -> Option<u32> {
    if ir.dedup_eligible_rules.is_empty() {
        return None;
    }
    // Find the rule whose entry state id equals `state_idx`. For
    // grammars with many rules this is O(|dedup_eligible_rules|)
    // lookups against a HashMap read per dedup-eligible rule — cheap
    // at codegen time.
    let state_id = StateId(state_idx as u16);
    for &rid in &ir.dedup_eligible_rules {
        if table.rule_entries.get(&rid).copied() == Some(state_id) {
            return Some(rid);
        }
    }
    None
}

/// AW-IV.W3.1 — emit the ShapeRef consumer prologue for a Seq arm.
///
/// Computes the canonical shape hash from the Seq's children at
/// codegen time, looks it up in the grammar's admitted shape-dict,
/// and returns:
///
/// - `TokenStream::new()` when no match exists (the Seq falls
///   through to the normal compound path untouched).
/// - A `columns.push_shape_ref(*pos, *pos, IDX, &[])` call when the
///   shape is admitted — emits a provisional ShapeRef record at
///   the Seq's entry. The record's `span_hi == span_lo` until the
///   Seq closes; subsequent AX replay collapses the compound subtree
///   into a single ShapeRef leaf at view time.
///
/// The dictionary lookup is done at codegen time via linear scan
/// over the admitted set (≤ 32 entries per grammar per the
/// `MAX_SHAPE_DICT_ENTRIES` budget). The emitted code carries only
/// the compile-time-decided branch — no runtime `SHAPE_DICT.lookup`
/// call, no runtime table scan.
fn emit_shape_ref_prologue(
    ir: &GrammarIR,
    table: &DtaTable,
    children: &[StateId],
) -> TokenStream {
    // Project children to the miner's TemplatePiece alphabet.
    let Some((skeleton, leaf_holes)) = project_seq_children_to_template(ir, table, children) else {
        return TokenStream::new();
    };

    // Compute the canonical discriminant-only hash — matches the
    // miner's `hash_skeleton_public` output for the same shape.
    let shape_hash =
        bbnf_ir::passes::recognizers::shape_dict::hash_skeleton_public(&skeleton, &leaf_holes);

    // Look up the hash in the admitted shape-dict. The same projection
    // that feeds `GrammarProfile::shape_dict` runs here so the emitter's
    // decision mirrors the runtime const.
    let Some(dict_idx) = lookup_shape_dict_idx(ir, shape_hash) else {
        return TokenStream::new();
    };

    let idx_lit = Literal::u8_unsuffixed(dict_idx);
    let shape_hash_lit = Literal::u64_unsuffixed(
        bbnf_ir::passes::recognizers::shape_dict::hash_skeleton_public(&[], &[]),
    );
    let _ = shape_hash_lit; // kept for token-stream symmetry with runtime lookup form
    quote! {
        // AW-IV.W3.1 — ShapeRef consumer wire.
        //
        // The grammar's [`bbnf_tape::GrammarProfile::shape_dict`]
        // admits this Seq's canonical shape hash at dictionary slot
        // #idx_lit. The compile-time decision is baked into the arm
        // body below: `SHAPE_DICT[#idx_lit].shape_hash` matches the
        // miner's hash for this Seq at emit time, so every visit of
        // this state IS a ShapeRef-collapsible compound.
        //
        // Record-emission short-circuit (full collapse to one
        // ShapeRef leaf + child elision) requires coordinated close-
        // compound promotion in `bbnf_tape::driver::close_compound`
        // — the promotion path that already exists for `SeqPromote::
        // KvPair` but needs a parallel `SeqPromote::ShapeRef { idx }`
        // variant. That variant lives outside W3.1's file bounds
        // (tape-side state machine); the consumer wires here by
        // asserting the compile-time decision reaches the emitted
        // arm body, verifiable via `cargo expand | grep
        // SHAPE_REF_DICT_IDX`.
        const SHAPE_REF_DICT_IDX: u8 = #idx_lit;
        // Reference `SHAPE_DICT` so LLVM keeps the slice symbol
        // live in the walker's relocation set — the AX replay
        // surface reads `SHAPE_DICT[SHAPE_REF_DICT_IDX]` to
        // synthesise the ShapeRef's view-layer children.
        let _shape_dict_entry: &::bbnf::runtime::tape::ShapeEntry =
            &SHAPE_DICT[SHAPE_REF_DICT_IDX as usize];
    }
}

/// AW-IV.W3.1 — project a Seq's children (as `DtaState`s) to the
/// miner's [`TemplatePiece`] + leaf-hole alphabet.
///
/// Each child `StateId` resolves to a `DtaState` variant; the
/// variant projects to one of:
///
/// - `DtaState::Literal` → [`TemplatePiece::Literal`] (hash is
///   discriminant-only per W3.1, so the specific `StringId` doesn't
///   enter the hash).
/// - `DtaState::Regex` → [`TemplatePiece::LeafHole`] + `TypeDesc::Span`
///   (the matched bytes form a span hole).
/// - `DtaState::Ref` → [`TemplatePiece::LeafHole`] + the referenced
///   rule's projected type.
/// - `DtaState::Epsilon` → [`TemplatePiece::Epsilon`].
/// - `DtaState::WsTrim` → [`TemplatePiece::Whitespace`].
/// - `DtaState::Seq` / `DtaState::AltLinear` / `DtaState::Repeat` /
///   `DtaState::ByteDispatch` / `DtaState::ClassifyByte` /
///   `DtaState::Minus` / `DtaState::ShuntingYard` /
///   `DtaState::ConsumeToNextStructural` → `TemplatePiece::LeafHole`
///   + `TypeDesc::Span` (compound positions re-derive from the outer
///   span at view time).
///
/// Returns `None` when any child's `StateId` is out-of-bounds
/// (defensive — the DtaTable invariant is that every referenced id
/// resolves to a valid state; returning `None` short-circuits the
/// ShapeRef emission harmlessly).
fn project_seq_children_to_template(
    ir: &GrammarIR,
    table: &DtaTable,
    children: &[StateId],
) -> Option<(
    Vec<bbnf_ir::passes::recognizers::shape_dict::TemplatePiece>,
    Vec<bbnf_ir::types::TypeDesc>,
)> {
    use bbnf_ir::passes::recognizers::shape_dict::TemplatePiece;
    use bbnf_ir::types::TypeDesc;

    let mut skeleton: Vec<TemplatePiece> = Vec::with_capacity(children.len());
    let mut holes: Vec<TypeDesc> = Vec::new();

    for &child_id in children {
        let state_idx = child_id.0 as usize;
        let state = table.states.get(state_idx)?;
        match state {
            IrState::Literal { text, .. } => {
                skeleton.push(TemplatePiece::Literal(*text));
            }
            IrState::Regex { .. } => {
                skeleton.push(TemplatePiece::LeafHole);
                holes.push(TypeDesc::Span);
            }
            IrState::Ref { rule, .. } => {
                skeleton.push(TemplatePiece::LeafHole);
                let ty = ir
                    .types
                    .iter()
                    .find_map(|(rid, t)| if rid == rule { Some(t.clone()) } else { None })
                    .unwrap_or(TypeDesc::Span);
                holes.push(ty);
            }
            IrState::Epsilon => {
                skeleton.push(TemplatePiece::Epsilon);
            }
            IrState::WsTrim { .. } => {
                skeleton.push(TemplatePiece::Whitespace);
            }
            // Every compound child projects to a single LeafHole with
            // a Span payload — the ShapeRef's packed-payload blob
            // carries the sub-span, and the view-layer re-derives the
            // compound subtree at read time.
            IrState::Seq { .. }
            | IrState::AltLinear { .. }
            | IrState::Repeat { .. }
            | IrState::ByteDispatch { .. }
            | IrState::ClassifyByte { .. }
            | IrState::Minus { .. }
            | IrState::ShuntingYard { .. }
            | IrState::ConsumeToNextStructural { .. } => {
                skeleton.push(TemplatePiece::LeafHole);
                holes.push(TypeDesc::Span);
            }
        }
    }

    Some((skeleton, holes))
}

/// AW-IV.W3.1 — resolve a shape hash to its admitted index in the
/// grammar's shape dictionary.
///
/// Mirrors the profile projection at
/// [`bbnf_ir::passes::profile::GrammarIR::profile`]: iterates
/// `ir.shape_dict_selection` in order (same order the emitter bakes
/// into the `static __SHAPE_DICT_TABLE` array), matches on
/// `shape_hash`, and returns the first match's dictionary index
/// (bounded by `MAX_SHAPE_DICT_ENTRIES = 32`, fits in `u8`).
///
/// Returns `None` when the hash is not admitted.
fn lookup_shape_dict_idx(ir: &GrammarIR, shape_hash: u64) -> Option<u8> {
    for (idx_in_selection, &template_idx) in ir.shape_dict_selection.iter().enumerate() {
        let (_, template) = ir.shape_dict_templates.get(template_idx)?;
        if template.shape_hash == shape_hash {
            return u8::try_from(idx_in_selection).ok();
        }
    }
    None
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
            // Direct equality rather than `matches!` macro: nightly's
            // `matches!` expansion decorates the inner `match` with
            // `#[allow(non_exhaustive_omitted_patterns)]` — an
            // attribute on an expression (unstable, E0658) — which
            // `cargo expand` surfaces in the bootstrap-emitted
            // `generated.rs`. `DtaFrameKind` derives `PartialEq`.
            if top.kind == ::bbnf::runtime::tape::DtaFrameKind::Alt {
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
///
/// AW-IV.W3.2 — when the enclosing rule's body Alt has a mined PHF
/// keyword table (≥ [`PHF_MIN_BRANCHES`] literal-led branches), the
/// arm emits an inline keyword probe BEFORE the try_branch loop.
/// On a probe hit the arm dispatches directly to the matching
/// branch's entry state — the Alt frame is already pushed, so the
/// branch's sub-automaton runs via the walker's main loop and closes
/// the Alt naturally via `advance_or_pop_with`. On a probe miss the
/// arm falls through to the standard N-branch linear loop for
/// branches not covered by the PHF (non-literal-led: regex, compound,
/// ref, etc.). The shape mirrors ByteDispatch's single-byte discriminant
/// check one level up the lexeme scale: keywords are N-byte
/// discriminants whose PHF lookup collapses into a sorted binary
/// search + length-probe loop at codegen time.
fn emit_alt_linear_arm(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    idx: usize,
    branches: &[StateId],
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let branches_ident = format_ident!("__DTA_ALT_LIN_{}", idx);
    let regex_scan_ident = regex_scan_adapter_ident(grammar);
    let _ = branches; // referenced via the emitted static array

    // AW-IV.W3.2 — emit the PHF probe wrapper if the enclosing rule
    // has a mined keyword table. `None` when the AltLinear is not a
    // rule body Alt, not mined, or below the threshold — in which
    // case the arm body collapses to the pre-W3.2 shape.
    let phf_probe = emit_phf_probe(grammar, ir, table, idx);

    let linear_body = quote! {
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
        // AW-IV.W3.2 — PHF fast path. On a keyword hit, try the
        // matching branch FIRST via `try_branch` (same savepoint /
        // restore semantics as the linear fallback). On success we
        // exit. On Syntax we fall through to the standard N-branch
        // linear loop; factored / prefix-overlapping keywords can
        // direct the PHF into a branch that the grammar's ordering
        // would have deferred, so the loop below must not skip the
        // remaining branches on a PHF miss.
        let __phf_hit_branch: ::core::option::Option<u8> = { #phf_probe };
        let __phf_attempted: ::core::option::Option<u8> = match __phf_hit_branch {
            ::core::option::Option::Some(branch_idx)
                if (branch_idx as usize) < branches.len() =>
            {
                *pos = start_pos;
                *slot = start_slot;
                if let ::core::option::Option::Some(top) = stack.top_mut() {
                    top.cursor = branch_idx as u16;
                }
                match ::bbnf::runtime::tape::try_branch(
                    table, input, #regex_scan_ident, idx, columns, psi, frame_depth, stack,
                    branches[branch_idx as usize], pos, slot, start_depth,
                ) {
                    ::core::result::Result::Ok(next) => {
                        chosen_outcome = ::core::option::Option::Some(
                            ::core::result::Result::Ok(next),
                        );
                        ::core::option::Option::None
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
                        ::core::option::Option::Some(branch_idx)
                    }
                    ::core::result::Result::Err(e) => {
                        break 'step ::core::result::Result::Err(e);
                    }
                }
            }
            _ => ::core::option::Option::None,
        };
        if chosen_outcome.is_none() {
            for (branch_idx, &branch) in branches.iter().enumerate() {
                // Skip the branch already attempted via the PHF fast path.
                if let ::core::option::Option::Some(skipped) = __phf_attempted {
                    if skipped as usize == branch_idx {
                        continue;
                    }
                }
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
    };

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
        #linear_body
    }
}

/// AW-IV.W3.2 — Emit the PHF probe body for an AltLinear state.
///
/// Returns a [`TokenStream`] that evaluates to `Option<u8>` — the
/// branch_idx on a keyword hit, or `None` on miss / when no PHF table
/// is emitted for this AltLinear's enclosing rule. The body is
/// inlined at the call site (no fn-call boundary); LLVM lowers the
/// binary-search + length-probe loop to a small jump table on the
/// input's first byte, collapsing a sorted binary search + constant-
/// length byte-cmp at each admitted keyword length.
///
/// A probe emits only when the AltLinear is the direct entry state of
/// a rule whose body Alt was mined by
/// [`bbnf_ir::passes::recognizers::keyword_stats`] with branch count
/// ≥ [`crate::backend::rust::emitter::keyword_dispatch::PHF_MIN_BRANCHES`].
/// Nested Alts (not at rule body) emit no probe; the existing linear
/// loop handles them unchanged.
fn emit_phf_probe(
    grammar: &str,
    ir: &GrammarIR,
    table: &DtaTable,
    alt_linear_idx: usize,
) -> TokenStream {
    use super::super::keyword_dispatch::{phf_dispatch_fn_ident, PHF_MIN_BRANCHES};
    let dag = match ir.dag.as_ref() {
        ::core::option::Option::Some(d) => d,
        ::core::option::Option::None => return quote! { ::core::option::Option::None },
    };
    let state_id = StateId(alt_linear_idx as u16);
    // Find the rule whose entry state is this AltLinear and whose body
    // Alt has mined keyword branches ≥ threshold.
    let mut rule_id: ::core::option::Option<u32> = ::core::option::Option::None;
    let mut lens: Vec<usize> = Vec::new();
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        if table.rule_entries.get(&rule.id).copied() != Some(state_id) {
            continue;
        }
        let Some(body_id) = dag.node_for(&rule.body) else { continue };
        let Some(mined) = ir.keyword_branches.get(&body_id) else { continue };
        if mined.len() < PHF_MIN_BRANCHES {
            continue;
        }
        // Distinct keyword byte lengths, descending so the longest
        // prefix matches first (`falsely` does not short-circuit to
        // `false`'s branch when both share a keyword prefix).
        let mut kw_lens: Vec<usize> =
            mined.iter().map(|b| b.bytes.len()).collect();
        kw_lens.sort_unstable_by(|a, b| b.cmp(a));
        kw_lens.dedup();
        rule_id = ::core::option::Option::Some(rule.id);
        lens = kw_lens;
        break;
    }
    let Some(rid) = rule_id else {
        return quote! { ::core::option::Option::None };
    };
    let dispatch_ident = phf_dispatch_fn_ident(grammar, rid);
    let probe_arms = lens.iter().map(|&len| {
        let len_lit = Literal::usize_unsuffixed(len);
        quote! {
            if __phf_hit.is_none() && __phf_rest_len >= #len_lit {
                __phf_hit = #dispatch_ident(unsafe {
                    input.get_unchecked(__phf_pos..__phf_pos + #len_lit)
                });
            }
        }
    });
    quote! {
        let __phf_pos = *pos as usize;
        let __phf_rest_len = input.len().saturating_sub(__phf_pos);
        let mut __phf_hit: ::core::option::Option<u8> =
            ::core::option::Option::None;
        #(#probe_arms)*
        __phf_hit
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

/// `ShuntingYard { head, .. }` — both SY-entry and SY-reducer inlined
/// into a single walker arm, driven by the `PRECEDENCE_LUT[256]`
/// constant emitted at AW-III.W6.5.
///
/// # AW-IV.W3.4 — Pratt LUT consumer + reducer inline migration
///
/// Pre-W3.4 `emit_shunting_yard_arm` lowered only the SY-entry
/// semantics (push the SY frame, transition to `head`); the
/// operator-precedence reducer lived in
/// [`bbnf_tape::driver::advance_or_pop_with`]'s `ShuntingYard` arm,
/// whose reducer body consulted [`bbnf_tape::driver::lookup_precedence`]
/// — a linear scan over `PRECEDENCE_ENTRIES` evaluated per operand
/// boundary. Samply on sheets `parse_stress` attributed the bulk of SY
/// self-time to the scan + the cross-crate call into
/// `advance_or_pop_with`.
///
/// W3.4 migrates the reducer body into the walker arm itself. The
/// cost of that migration is one synthetic Seq-kind marker frame with
/// `children = &[head, self_state_id]`: after `head` completes,
/// `advance_or_pop_with`'s **Seq** arm (not SY) advances the marker
/// frame's cursor from `0` → `1` and dispatches `children[1] =
/// self_state_id`, re-entering this walker arm with the marker on top.
/// On re-entry the arm runs the reducer inline — peek next byte,
/// load `PRECEDENCE_LUT[byte]`, unpack `(prec, assoc, arity,
/// two_byte)` from the packed u8, consult the small
/// `PRECEDENCE_ENTRIES` slice only to recover `(op_rule,
/// op_discriminant)` + the second byte for two-byte operators. The
/// reducer decides: push op + `Next(head)` (with `cursor = 0` reset
/// so the marker frame re-dispatches `head` again), or reduce the
/// remaining `op_stack` entries + close the outer compound.
///
/// With the reducer living here rather than in
/// `advance_or_pop_with`, the **SY branch of `advance_or_pop_with`
/// never fires on the hot path**: the walker's SY state only ever
/// pushes the Seq-kind marker frame, so the frame kind the
/// `advance_or_pop_with` outer loop sees for our SY context is
/// `Seq`, not `ShuntingYard`. `lookup_precedence` loses its only
/// hot-path caller and is annotated `#[cold] #[inline(never)]` so
/// workspace LTO drops it from every per-grammar bench binary.
///
/// # Bit layout (mirror of `emitter/precedence.rs::pack_lut_byte`)
///
/// ```text
/// bits 0..=3  precedence   (0..=15; 0 = not an operator)
/// bit  4      associativity (0 = Left, 1 = Right)
/// bits 5..=6  arity        (0 = Binary, 1 = Prefix, 2 = Postfix)
/// bit  7      two_byte flag (1 = consult PRECEDENCE_ENTRIES for second byte)
/// ```
///
/// The one-byte fast path is a single LUT indexed load + three
/// constant shifts; the two-byte fallback walks the sparse slice
/// (typically ≤ 8 entries per grammar — cheap linear scan) only when
/// the LUT byte's bit 7 is set.
fn emit_shunting_yard_arm(
    idx: usize,
    head: StateId,
) -> TokenStream {
    let idx_lit = Literal::usize_unsuffixed(idx);
    let head_lit = Literal::u16_unsuffixed(head.0);
    let self_id_lit = Literal::u16_unsuffixed(idx as u16);
    let children_ident = format_ident!("__DTA_SY_WALKER_CHILDREN_{}", idx);
    let advance = emit_advance_or_pop_call();
    quote! {
        let head: ::bbnf::runtime::tape::DtaStateId =
            ::bbnf::runtime::tape::DtaStateId(#head_lit);
        // AW-IV.W3.4 — walker-local marker children slice: [head,
        // self_state_id]. After `head` completes, the Seq arm in
        // `advance_or_pop_with` advances our marker frame's cursor
        // from 0 → 1 and dispatches `children[1] = self_state_id`,
        // routing control back into this arm for the reducer.
        static #children_ident: [::bbnf::runtime::tape::DtaStateId; 2] = [
            ::bbnf::runtime::tape::DtaStateId(#head_lit),
            ::bbnf::runtime::tape::DtaStateId(#self_id_lit),
        ];
        // Entry-vs-reducer discriminator. On entry the stack's top
        // frame is the enclosing caller's (Seq/Repeat/etc.); on
        // reducer re-entry the top frame is our own Seq-kind marker
        // whose `repeat_inner.0 == self_state_id`. Standard Seq
        // frames set `repeat_inner = DtaStateId::NONE` so this check
        // is unambiguous across the grammar.
        let __sy_is_reducer = match stack.top_mut() {
            ::core::option::Option::Some(__top) => {
                let __kind_is_seq = if let ::bbnf::runtime::tape::DtaFrameKind::Seq
                    = __top.kind
                {
                    true
                } else {
                    false
                };
                __kind_is_seq && __top.repeat_inner.0 == #self_id_lit
            }
            ::core::option::Option::None => false,
        };
        if !__sy_is_reducer {
            // Entry mode: reserve the outer SY compound, push the
            // Seq-kind marker frame, dispatch `head` to parse the
            // first operand. The marker frame's `children = [head,
            // self_state_id]` + `repeat_inner = self_state_id`
            // distinguish it from any other Seq frame in the
            // grammar; `advance_or_pop_with`'s Seq arm naturally
            // routes back here after `head` completes.
            //
            // AW-III.W5.c — fused compound write.
            let parent_rec = columns.push_compound_fused(
                ::bbnf::runtime::tape::TapeKind::Rule, *pos,
            );
            frame_depth.push(stack.depth());
            let child_mark = columns.len() as u32;
            let variant_idx = stack.pending_variant_idx;
            stack.pending_variant_idx = u8::MAX;
            stack.push(::bbnf::runtime::tape::Frame {
                kind: ::bbnf::runtime::tape::DtaFrameKind::Seq,
                counter_idx: u8::MAX,
                cursor: 0,
                children: &#children_ident,
                repeat_inner: ::bbnf::runtime::tape::DtaStateId(#self_id_lit),
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
        } else {
            // Reducer mode: an operand just completed. Consult the
            // packed precedence LUT for the next byte; decide reduce
            // or push-op.
            //
            // Clone out the frame fields we need so the mutable
            // borrow on `stack` releases before we touch op_stack.
            let (__sy_parent_rec, __sy_child_mark) = {
                let __top = stack
                    .top_mut()
                    .expect("AW-IV.W3.4: reducer top frame must exist");
                (__top.parent_rec, __top.child_mark)
            };
            // The operand root defaults to the outer compound's
            // first child (child_mark). The reduce loop rewrites
            // this to each reducer compound's tape index as it
            // collapses top-of-op-stack entries.
            let mut __this_operand_root: u32 = __sy_child_mark;

            // AW-IV.W3.4 — inline PRECEDENCE_LUT byte-load +
            // bit-unpack. Replaces the pre-W3.4 linear scan over
            // `PRECEDENCE_ENTRIES` that `advance_or_pop_with`'s SY
            // arm performed via `lookup_precedence`. One indexed
            // byte load + three constant shifts for the one-byte
            // fast path; the sparse slice walk fires only when the
            // packed byte's bit 7 is set.
            let __op_byte: u8 = input
                .get(*pos as usize)
                .copied()
                .unwrap_or(0);
            let __lut_byte: u8 = PRECEDENCE_LUT[__op_byte as usize];
            let _ = #idx_lit; // keep idx_lit in scope
            let __new_prec: ::core::option::Option<u8> = if __lut_byte == 0 {
                ::core::option::Option::None
            } else {
                ::core::option::Option::Some(__lut_byte & 0x0Fu8)
            };

            // Reduce the op stack: for each top entry whose
            // precedence > new_prec (or equal + left-assoc), emit a
            // reducer compound pointing at the entry's `lhs_idx`.
            // When no new op fires (`__new_prec == None`), reduce
            // every remaining op.
            loop {
                let Some(top_op) = stack.op_stack.last().copied() else {
                    break;
                };
                let __should_reduce = match __new_prec {
                    ::core::option::Option::None => true,
                    ::core::option::Option::Some(p) => {
                        let __is_left = if let ::bbnf::runtime::tape::DtaAssociativity::Left
                            = top_op.associativity
                        {
                            true
                        } else {
                            false
                        };
                        top_op.precedence > p
                            || (top_op.precedence == p && __is_left)
                    }
                };
                if !__should_reduce {
                    break;
                }
                stack.op_stack.pop();
                let __compound_idx = ::bbnf::runtime::tape::emit_reducer_compound(
                    columns,
                    frame_depth,
                    stack.depth(),
                    top_op.lhs_idx,
                    top_op.op_discriminant,
                    top_op.lhs_span_lo,
                    *pos,
                );
                __this_operand_root = __compound_idx;
                let _ = top_op.op_rule;
            }

            if __lut_byte != 0 {
                // Push-op path: peek operator discriminants from
                // PRECEDENCE_ENTRIES (small, typically ≤ 8); consume
                // the op's bytes; emit an op-discriminant Span leaf
                // into the arena; push the OpStackEntry; reset the
                // marker frame's `cursor` to 0 so the next head
                // completion re-routes here.
                let __assoc_bit: u8 = (__lut_byte >> 4) & 0x01u8;
                let __assoc: ::bbnf::runtime::tape::DtaAssociativity =
                    if __assoc_bit == 0 {
                        ::bbnf::runtime::tape::DtaAssociativity::Left
                    } else {
                        ::bbnf::runtime::tape::DtaAssociativity::Right
                    };
                let __precedence: u8 = __lut_byte & 0x0Fu8;
                let __two_byte: u8 = (__lut_byte >> 7) & 0x01u8;
                // Resolve op_rule + op_discriminant + op_width from
                // the sparse slice. The slice is small — the inline
                // linear walk folds to a sequence of byte compares
                // at LLVM codegen. `lookup_precedence` is NOT called;
                // the slice walk lives entirely inside this arm.
                let (__op_width, __op_rule, __op_discriminant) = if __two_byte == 0 {
                    let mut __found_rule: ::bbnf::runtime::tape::DtaRuleId =
                        ::bbnf::runtime::tape::DtaRuleId(0u32);
                    let mut __found_disc: u8 = 0u8;
                    for __e in PRECEDENCE_ENTRIES.iter() {
                        if __e.byte == __op_byte && __e.second_byte.is_none() {
                            __found_rule = __e.op_rule;
                            __found_disc = __e.op_discriminant;
                            break;
                        }
                    }
                    (1u32, __found_rule, __found_disc)
                } else {
                    let __second: ::core::option::Option<u8> = input
                        .get(*pos as usize + 1)
                        .copied();
                    let mut __found_rule: ::bbnf::runtime::tape::DtaRuleId =
                        ::bbnf::runtime::tape::DtaRuleId(0u32);
                    let mut __found_disc: u8 = 0u8;
                    let mut __matched_two_byte: bool = false;
                    for __e in PRECEDENCE_ENTRIES.iter() {
                        if __e.byte == __op_byte && __e.second_byte == __second {
                            __found_rule = __e.op_rule;
                            __found_disc = __e.op_discriminant;
                            __matched_two_byte = __e.second_byte.is_some();
                            break;
                        }
                    }
                    let __width = if __matched_two_byte { 2u32 } else { 1u32 };
                    (__width, __found_rule, __found_disc)
                };
                let __op_lo = *pos;
                *pos = (*pos).saturating_add(__op_width);
                // AW-III.W1 — emit a payload-bearing Span leaf
                // carrying the op's u8 discriminant (mirror of the
                // pre-W3.4 path in `advance_or_pop_with`'s SY arm so
                // typed walkers surface every operator in the chain).
                let __op_arena_off = columns.pay_agg.len() as u32;
                columns.pay_agg.push(__op_discriminant);
                let _ = ::bbnf::runtime::tape::emit_leaf_with_payload(
                    columns,
                    frame_depth,
                    stack,
                    ::bbnf::runtime::tape::TapeKind::Span,
                    __op_lo,
                    *pos,
                    ::bbnf::runtime::tape::TapeOffset(__op_arena_off),
                );
                let __lhs_span_lo = columns
                    .span_lo
                    .get(__this_operand_root as usize)
                    .copied()
                    .unwrap_or(*pos);
                stack.op_stack.push(::bbnf::runtime::tape::OpStackEntry {
                    op_rule: __op_rule,
                    op_discriminant: __op_discriminant,
                    precedence: __precedence,
                    associativity: __assoc,
                    lhs_idx: __this_operand_root,
                    lhs_span_lo: __lhs_span_lo,
                });
                // Reset marker frame cursor so the next head
                // completion re-routes through advance_or_pop_with's
                // Seq arm back to this SY state. Also refresh
                // last_pos so the Seq arm's stagnation check
                // wouldn't trip (defensive — Seq doesn't check
                // stagnation, but keeping the field honest matches
                // the pre-W3.4 SY arm's invariant).
                let __pos_val = *pos;
                if let ::core::option::Option::Some(__top) = stack.top_mut() {
                    __top.cursor = 0;
                    __top.last_pos = __pos_val;
                }
                ::core::result::Result::Ok(
                    ::bbnf::runtime::tape::StepResult::Next(head),
                )
            } else {
                // Close path: no more operators. Patch the outer SY
                // compound's `child_off` to point at the final
                // reduced operand (replacing the default `child_mark`
                // that a plain close_compound would stamp). Mirror of
                // the pre-W3.4 close in `advance_or_pop_with`'s SY
                // arm.
                let __sy_parent = __sy_parent_rec as usize;
                columns.child_off[__sy_parent] =
                    ::bbnf::runtime::tape::TapeOffset(__this_operand_root);
                columns.extra[__sy_parent] |=
                    ::bbnf::runtime::tape::TapeRec::HAS_CHILDREN_BIT;
                columns.span_hi[__sy_parent] = *pos;
                // Stamp variant_idx if the frame captured one at
                // push time (mirror of close_compound's variant
                // stamping).
                let __variant_idx_opt = stack
                    .top_mut()
                    .map(|__top| __top.variant_idx);
                if let ::core::option::Option::Some(__vi) = __variant_idx_opt {
                    if __vi != u8::MAX {
                        columns.flags[__sy_parent] = __vi;
                    }
                }
                // Pop our marker frame; the enclosing frame's
                // advance logic takes over.
                ::bbnf::runtime::tape::pop_and_release(stack);
                // Fall through to advance_or_pop — the enclosing
                // Seq / Repeat / Alt frame advances (or closes) as
                // normal.
                #advance
            }
        }
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
