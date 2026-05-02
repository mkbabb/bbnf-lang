//! AZ-II.cutover.A — BBNF struct-direct runtime.
//!
//! `crates/core/src/runtime/bbnf/` is the typed-struct runtime for
//! the BBNF self-hosted grammar — peer of
//! [`crate::runtime::json`] / [`crate::runtime::css_l4`] /
//! [`crate::runtime::google_sheets`]. The regenerated parser (landing
//! at AZ-II.cutover.B) writes directly into a [`BbnfDocument`] graph
//! via the [`BbnfStructBuilder`] consumer of the
//! [`crate::runtime::builder::StructBuilder`] trait — the tape
//! substrate severs on the BBNF parse path; no `TapeBuilder` /
//! `TapeRec` / `TapeCursor` symbol appears in this module's
//! transitive code.
//!
//! cutover.A landed the substrate (this module + the resolver-arm
//! extension at `crates/ir/src/registry/strategy.rs`); cutover.B
//! regens BBNF onto the struct-direct path; cutover.C deletes the
//! tape crate.
//!
//! # Module layout
//!
//! - [`value`]    — typed [`BbnfValue`] sum and its companion
//!   compound-id newtype.
//! - [`arena`]    — the [`BbnfArena`] owning slab for compound
//!   children and the [`BbnfCompoundKind`] discriminator alphabet.
//! - [`document`] — the [`BbnfDocument`] root, the [`BbnfView`]
//!   newtype, [`BbnfKind`] discriminator, and [`BbnfPathQuery`]
//!   trait.
//! - [`builder`]  — the [`BbnfStructBuilder`] concrete `StructBuilder`
//!   impl that the regenerated parse function targets.
//! - [`view`]     — the [`crate::runtime::RuntimeView`] impl on
//!   [`BbnfView`] (kind / span / input / children traversal).
//!
//! # Wire contract
//!
//! Once cutover.B regen lands, `bbnf::grammar::generated::bbnf::BbnfBootstrap::parse(src)`
//! returns a [`BbnfDocument<'_>`] borrowing from `src`'s lifetime.

pub mod arena;
pub mod builder;
pub mod document;
pub mod parse_with;
pub mod serialize;
pub mod value;
pub mod view;

pub use arena::{BbnfArena, BbnfCompound, BbnfCompoundId, BbnfCompoundKind};
pub use builder::BbnfStructBuilder;
pub use document::{BbnfDocument, BbnfKind, BbnfPathQuery, BbnfView};
pub use parse_with::parse_with;
pub use serialize::serialize_compact_doc;
pub use value::BbnfValue;
