//! `bbnf-tape` — leaf crate for the parser tape representation.
//!
//! # Architectural role
//!
//! The tape is bbnf-lang's replacement for the eager AST. Instead
//! of materialising a typed enum tree per parse (one `slab().alloc()`
//! per heterogeneous Alt element, one enum variant per branch,
//! recursive `&'a Node` indirection through every compound shape),
//! the parser appends records to a columnar [`Columns`] substrate.
//! The typed view is generated lazily as an `impl` over the tape:
//! each accessor call materialises a [`TapeRec`] by offset and
//! returns a view over its children.
//!
//! ```text
//!   input bytes ─► parser ─► Tape (Columns SoA substrate) ─► View<'tape>
//!                     │                                           │
//!                     └─ TapeBuilder::push_compound(kind, start..end)
//!                                                                 │
//!                                                                 ▼
//!                                                      accessor.key() / .value()
//! ```
//!
//! # Design
//!
//! - **Columnar substrate** (Tranche AV.2.1): [`Columns`] holds the
//!   six structural columns (`kinds`, `flags`, `extra`, `span_lo`,
//!   `span_hi`, `sib_skip`) plus `child_off` and the typed-payload
//!   columns (`pay_narrow`, `pay_wide`, `pay_agg`). Per-record
//!   metadata lives in SoA layout so bulk typed visitors — the
//!   4-lane reordered-unrolling kernels landing in V2.5 — see dense
//!   `Vec<u64>` / `Vec<u32>` blocks rather than 16-byte records.
//! - **16-byte [`TapeRec`]** is the read-side materialised view.
//!   External consumers bind `TapeRec` by value; the struct is
//!   reconstructed from the six structural columns on demand.
//! - **Sibling-skip traversal** (Tranche AV.2.2): forward sibling
//!   walks consume one indexed `sib_skip` column read per step.
//!   The backward-walk child enumeration is gone.
//! - **Column-rank payload routing** (Tranche AV.2.3):
//!   `PayloadData::InlineScalar` lands in `pay_narrow`, `WideScalar`
//!   lands in `pay_wide`; the record's `child_off` holds the column
//!   rank. The `u32::MAX` collision with `TapeOffset::NONE` is
//!   resolved because column ranks are push-ordered counters.
//! - **Zero inter-crate dependencies**: leaf crate with no external
//!   deps beyond std.

#![warn(missing_docs)]

pub mod builder;
pub mod columns;
pub mod cursor;
pub mod decoders;
pub mod dedup;
pub mod driver;
pub mod dta;
pub mod finaliser;
pub mod kind;
pub mod profile;
pub mod psi;
pub mod shape_dict;
pub mod stage1;
pub mod tape;
pub mod visitor;

pub use builder::{PayloadData, TapeBuildError, TapeBuilder};
pub use columns::{
    ColumnTag, Columns, Count, MaxF64, MinF64, PayAggU8, PayNarrowU32, PayWideF64, PayWideU64,
    Reducer, SumF64, SumU32, SumU64,
};
pub use cursor::{ChildIter, ColumnRank, ShapeRefChildIter, ShapeRefSyntheticChild, TapeCursor};
pub use dedup::{columns_range_eq, push_compound_referring, BloomDedup, N_WORDS};
// AW-III.W4.c — driver hot/cold split.
//
// Hot path: per-grammar `dta_run_<grammar>` emitted by W4.b's
// `emit_specialised_walker` pass; the per-grammar `parse()` calls
// the emitted function directly.
//
// Cold path: [`dta_run_cold`] is the dispatch_one + walk-table loop
// preserved verbatim for the AX replay subsystem and walker-arm
// regression tests. Helpers ([`emit_leaf`], [`emit_leaf_with_payload`],
// [`close_compound`], [`advance_or_pop_with`], [`frame_to_tape_kind`],
// etc.) are re-exported so the emitted walker can call them as
// inlined helpers. AW-III.W5.c: `reserve_compound` helper deleted;
// compounds emit via [`Columns::push_compound_fused`] directly.
pub use driver::{
    advance_or_pop_with, advance_seq_fast, close_compound, dispatch_one,
    dta_run_cold, emit_leaf, emit_leaf_with_payload, emit_reducer_compound,
    first_ws_pattern, frame_at, frame_to_tape_kind, handle_repeat_failure,
    handle_repeat_failure_bounded, lookup_precedence, pop_and_release,
    saturating_u16, stack_top, stage_literal_payload_in_arena,
    trim_ascii_ws, trim_with_pattern, try_branch, Cursor, DtaError, Frame,
    FrameStack, FrameStackProbeSnapshot, FrameStackSavepoint, IterSavepoint,
    OpStackEntry, RepeatAbsorbResult, StepResult,
    COUNTER_INLINE_SLOTS, STACK_INLINE_DEPTH,
};

// AW-IV.W4.4 — document-parallel fork substrate.
//
// `WalkerFn` + `dta_run_parallel` + `rayon_num_threads` are always
// exported — `dta_run_parallel` has a rayon-feature-gated body and a
// single-thread fallback when rayon is disabled. This keeps the
// emitted `parse()` entry compilable regardless of the tape crate's
// feature-flag state; when rayon is off, the fallback forwards to
// the single-thread walker so the parallel branch collapses to the
// sequential path. The emitter's break-even gate + worker-count
// clamp do the same thing at runtime when `rayon_num_threads()`
// returns 1.
pub use driver::{dta_run_parallel, WalkerFn};

/// AW-IV.W4.4 — rayon worker-count accessor for the emitted parse()
/// entry.
///
/// The emitted `parse()` caps the parallel fork at 4 workers per the
/// canonical AW-IV.W4.4 bench gate (tailwind.css 4c sub-linear-to-
/// linear scaling); this accessor returns `rayon::current_num_threads()`
/// when the feature is enabled and `1` when it isn't (the emitter's
/// break-even gate + worker-count clamp both collapse to the single-
/// thread path on `1`).
#[inline]
pub fn rayon_num_threads() -> usize {
    #[cfg(feature = "rayon")]
    {
        ::rayon::current_num_threads()
    }
    #[cfg(not(feature = "rayon"))]
    {
        1
    }
}
#[cfg(feature = "dta-replay")]
pub use driver::{dta_run_with_replay, DtaSnapshot};
pub use dta::{
    DtaAssociativity, DtaCounterOptional, DtaDiagnostic, DtaFrameKind, DtaPrecedenceEntry,
    DtaRuleEntry, DtaRuleId, DtaState, DtaStateId, DtaTable, LiteralPayload, SeqPromote,
};
pub use finaliser::{derive_frame_depth, finalise, STACK_DEPTH_HINT};
pub use kind::TapeKind;
pub use profile::{
    BranchPrior, ColumnId, GrammarProfile, KeywordTable, RuleId, ShapeEntry, VisitorId,
};
pub use psi::{PayloadJob, PayloadKind, PayloadStream};
pub use shape_dict::{BbnfShapeEntry, BbnfShapeKind};
pub use stage1::StructuralIndex;
pub use tape::{Tape, TapeIter, TapeOffset, TapeRec};
pub use visitor::{
    ArrayVisitor, GrammarVisitor, KeywordVisitor, NumberVisitor, ObjectVisitor, StringVisitor,
    TapeVisitor, TapeVisitorError, Value, ValueVisitor, ValueVisitorError,
};

/// Inline-aggregate payload budget (in bytes).
///
/// Aggregate payloads `≤ MAX_INLINE_AGGREGATE_BYTES` route through
/// [`PayloadData::Aggregate`] and occupy at most two 8-byte arena
/// slots. Aggregates that exceed this budget — CSS `colorFunction`
/// and friends at 33+ B — route through [`PayloadData::LargeAggregate`]
/// instead; the on-arena layout is identical (bytes verbatim into an
/// 8-aligned slot, no length prefix) and the read path recovers the
/// byte count from the grammar's payload-layout table keyed by
/// `(kind, variant_idx)`.
///
/// 16 bytes matches the size of a single [`TapeRec`] — the bound is
/// chosen so small aggregates occupy exactly one or two arena slots
/// while larger ones pay a per-slot accounting cost only when they
/// genuinely exceed the stack-buffer budget used by the emitter.
pub const MAX_INLINE_AGGREGATE_BYTES: usize = 16;
