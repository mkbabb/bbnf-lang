//! `bbnf-tape` — leaf crate for the parser tape representation.
//!
//! # Architectural role
//!
//! The tape is bbnf-lang's replacement for the eager AST. Instead of
//! materialising a typed enum tree per parse (one `slab().alloc()` per
//! heterogeneous Alt element, one enum variant per branch, recursive
//! `&'a Node` indirection through every compound shape), the parser
//! appends fixed-size records to a chunked arena. The typed view is
//! generated lazily as an `impl` over the tape: each accessor call
//! reads a [`TapeRec`] by offset and returns a view over its children.
//!
//! ```text
//!   input bytes ─► parser ─► Tape (chunked arena of TapeRec) ─► View<'tape>
//!                     │                                             │
//!                     └─ TapeBuilder::push_compound(kind, start..end)
//!                                                                   │
//!                                                                   ▼
//!                                                        accessor.key() / .value()
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
//! - **Chunked arena storage** ([`ChunkedArena`]): 64 KB chunks
//!   allocated from a `Vec<Vec<TapeRec>>`. No realloc-copy on growth —
//!   each chunk is allocated once and lives until the tape is dropped.
//!   Indexing is `(chunk_idx, within_chunk)` via right-shift + mask
//!   with the per-chunk capacity.
//! - **Zero inter-crate dependencies**: leaf crate, `bumpalo` only if
//!   we need a shared arena for the string spans. Starting point is
//!   `Vec<Vec<TapeRec>>` — simpler, audits clean in Miri.
//!
//! # Non-goals
//!
//! - No per-record compression. The fixed size is the contract —
//!   consumers read by offset + 16 bytes, branchless.
//! - No serialisation across the WASM boundary in this tranche. The
//!   tape lives for the lifetime of one parse; persistence is future
//!   work.

#![warn(missing_docs)]

pub mod arena;
pub mod builder;
pub mod cursor;
pub mod kind;
pub mod tape;

pub use arena::ChunkedArena;
pub use builder::{TapeBuildError, TapeBuilder};
pub use cursor::TapeCursor;
pub use kind::TapeKind;
pub use tape::{Tape, TapeOffset, TapeRec};
