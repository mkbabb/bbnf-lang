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
pub mod driver;
pub mod dta;
pub mod finaliser;
pub mod kind;
pub mod profile;
pub mod psi;
pub mod shape_dict;
pub mod tape;

pub use builder::{PayloadData, TapeBuildError, TapeBuilder};
pub use columns::Columns;
pub use cursor::{ChildIter, ColumnRank, ShapeRefChildIter, ShapeRefSyntheticChild, TapeCursor};
pub use driver::{dta_run, DtaError, Frame, FrameStack, RegexScanner, STACK_INLINE_DEPTH};
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
pub use tape::{Tape, TapeIter, TapeOffset, TapeRec};

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
