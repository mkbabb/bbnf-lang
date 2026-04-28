//! AZ-II.cutover.E (Phase 2) — CSV struct-direct runtime.
//!
//! `crates/core/src/runtime/csv/` is the typed-struct runtime for the
//! CSV grammar (`grammar/misc/csv.bbnf`) — peer of
//! [`crate::runtime::json`] / [`crate::runtime::css_l4`] /
//! [`crate::runtime::google_sheets`] / [`crate::runtime::bbnf`]. The
//! post-Phase 2 regenerated parser writes directly into a
//! [`CsvDocument`] graph via the [`CsvStructBuilder`] consumer of the
//! [`crate::runtime::builder::StructBuilder`] trait — the tape
//! substrate severs on the CSV parse path; no `TapeBuilder` /
//! `TapeRec` / `TapeCursor` symbol appears in this module's
//! transitive code post-regen.
//!
//! cutover.E lands the substrate (this module + the resolver-arm
//! extension at `crates/ir/src/registry/strategy.rs`); cutover.F
//! regens CSV onto the struct-direct path and then deletes the tape
//! crate (the latter requires every other tape-direct grammar — math,
//! ebnf, bnf, css_pretty — to migrate as well; cutover.E lands
//! csv + math substrate; bnf / ebnf / css_pretty await cutover.F).
//!
//! # Module layout
//!
//! - [`value`]    — typed [`CsvValue`] sum.
//! - [`arena`]    — the [`CsvArena`] owning slab for compound children
//!   and the [`CsvCompoundKind`] discriminator alphabet.
//! - [`document`] — the [`CsvDocument`] root, the [`CsvView`] newtype,
//!   [`CsvKind`] discriminator, and [`CsvPathQuery`] trait.
//! - [`builder`]  — the [`CsvStructBuilder`] concrete `StructBuilder`
//!   impl that the post-regen parse function targets.
//! - [`view`]     — the [`crate::runtime::RuntimeView`] impl on
//!   [`CsvView`] (kind / span / input / children traversal).
//!
//! # Wire contract
//!
//! Once cutover.F regen lands, `bbnf::grammar::generated::csv::CsvParser::parse(src)`
//! returns a [`CsvDocument<'_>`] borrowing from `src`'s lifetime.

pub mod arena;
pub mod builder;
pub mod document;
pub mod value;
pub mod view;

pub use arena::{CsvArena, CsvCompound, CsvCompoundId, CsvCompoundKind};
pub use builder::CsvStructBuilder;
pub use document::{CsvDocument, CsvKind, CsvPathQuery, CsvView};
pub use value::CsvValue;
