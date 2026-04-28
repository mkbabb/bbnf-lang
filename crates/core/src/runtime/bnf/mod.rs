//! AZ-II.cutover.E (Phase 2) — Bnf struct-direct runtime.
//!
//! Peer of csv / math / bbnf / json runtimes. Substrate-only landing
//! at cutover.E; cutover.F runs `cargo xtask regen --grammar bnf`
//! to flip `BnfParser::parse` onto this substrate.

pub mod arena;
pub mod builder;
pub mod document;
pub mod value;
pub mod view;

pub use arena::{BnfArena, BnfCompound, BnfCompoundId, BnfCompoundKind};
pub use builder::BnfStructBuilder;
pub use document::{BnfDocument, BnfKind, BnfPathQuery, BnfView};
pub use value::BnfValue;
