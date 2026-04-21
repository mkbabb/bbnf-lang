//! `tape` — leaf crate for the parser tape representation.
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
pub mod packed;
pub mod profile;
pub mod psi;
pub mod stage1;
pub mod structural_scan;
pub mod tape;
pub mod visitor;

pub use builder::{OpenFrame, PayloadData, TapeBuildError, TapeBuilder};
pub use columns::{
    ColumnTag, Columns, Count, MaxF64, MinF64, PayAggU8, PayNarrowU32, PayWideF64, PayWideU64,
    Reducer, SumF64, SumU32, SumU64,
};
pub use cursor::{ChildIter, ColumnRank, TapeCursor};
pub use dedup::{columns_range_eq, push_compound_referring, BloomDedup, N_WORDS};
// AX.W0b.A — tape-emission helper re-exports. Post-W0b the walker
// interpreter retires; shape emitters are the sole consumers of
// these helpers (leaf emission, compound close, payload staging,
// whitespace skip, Pratt-operator lookup).
pub use driver::{
    close_compound, emit_leaf, emit_leaf_with_payload, emit_reducer_compound, first_ws_pattern,
    lookup_precedence, saturating_u16, trim_ascii_ws, trim_with_pattern, DtaError,
};
pub use dta::{DtaAssociativity, DtaPrecedenceEntry, DtaRuleId, DtaStateId};
pub use finaliser::{derive_frame_depth, finalise, STACK_DEPTH_HINT};
pub use kind::TapeKind;
pub use packed::PackedRecord;
pub use profile::{GrammarProfile, RuleId};
pub use psi::{PayloadJob, PayloadKind, PayloadStream};
pub use stage1::StructuralIndex;
pub use structural_scan::{next_structural_at_or_after, scan_structural};
pub use tape::{Tape, TapeIter, TapeOffset, TapeRec};
pub use visitor::{
    ArrayVisitor, GrammarVisitor, KeywordVisitor, NumberVisitor, ObjectVisitor, PrattVisitor,
    StringVisitor, TapeVisitor, TapeVisitorError, Value, ValueVisitor, ValueVisitorError,
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
