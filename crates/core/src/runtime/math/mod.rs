//! AZ-II.cutover.E (Phase 2) — Math struct-direct runtime.
//!
//! Peer of `csv` / `bbnf` / `json` runtimes. Substrate-only landing
//! at cutover.E; cutover.F runs `cargo xtask regen --grammar math`
//! to flip `MathParser::parse` onto this substrate.
//!
//! # Module layout
//!
//! - [`value`]    — typed [`MathValue`] sum.
//! - [`arena`]    — [`MathArena`] slab + [`MathCompoundKind`] alphabet.
//! - [`document`] — [`MathDocument`] root + [`MathView`] newtype.
//! - [`builder`]  — [`MathStructBuilder`] concrete `StructBuilder`.
//! - [`view`]     — [`crate::runtime::RuntimeView`] impl on [`MathView`].

pub mod arena;
pub mod builder;
pub mod document;
pub mod value;
pub mod view;

pub use arena::{MathArena, MathCompound, MathCompoundId, MathCompoundKind};
pub use builder::MathStructBuilder;
pub use document::{MathDocument, MathKind, MathPathQuery, MathView};
pub use value::MathValue;
