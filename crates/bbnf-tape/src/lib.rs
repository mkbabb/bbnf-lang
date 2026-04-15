//! `bbnf-tape` — leaf crate for the parser tape representation.
//!
//! # Architectural role
//!
//! The tape is bbnf-lang's replacement for the eager AST. Instead of
//! materialising a typed enum tree per parse (one `slab().alloc()` per
//! heterogeneous Alt element, one enum variant per branch, recursive
//! `&'a Node` indirection through every compound shape), the parser
//! appends fixed-size records to a flat `Vec<TapeRec>`. The typed view
//! is generated lazily as an `impl` over the tape: each accessor call
//! reads a [`TapeRec`] by offset and returns a view over its children.
//!
//! ```text
//!   input bytes ─► parser ─► Tape (flat Vec<TapeRec>) ─► View<'tape>
//!                     │                                       │
//!                     └─ TapeBuilder::push_compound(kind, start..end)
//!                                                             │
//!                                                             ▼
//!                                                  accessor.key() / .value()
//! ```
//!
//! # Design
//!
//! - **Fixed-size records** ([`TapeRec`] is 16 bytes, one-quarter of a
//!   64-byte cache line): `kind`, `flags`, `span_lo`, `span_hi`,
//!   `child_off` fields. Compound records (Seq, Alt, Repeat, rule
//!   entry) point to their children via `child_off`; the children are
//!   a contiguous run from `child_off` to the next compound's
//!   `child_off`.
//! - **Flat Vec storage** (Tranche AK.0): single `Vec<TapeRec>` with
//!   one pre-allocation via `with_capacity`. Zero indirection per
//!   push — just bounds check + write + len increment. Replaces the
//!   ChunkedArena `Vec<Vec<TapeRec>>` that had 2 pointer dereferences
//!   per push.
//! - **Zero inter-crate dependencies**: leaf crate with no external
//!   deps beyond std.

#![warn(missing_docs)]

pub mod builder;
pub mod cursor;
pub mod kind;
pub mod tape;

pub use builder::{PayloadData, TapeBuildError, TapeBuilder};
pub use cursor::{ChildIter, TapeCursor};
pub use kind::TapeKind;
pub use tape::{Tape, TapeOffset, TapeRec};

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
