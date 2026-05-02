//! AZ-II.cutover.E (Phase 2) — Ebnf struct-direct runtime.
//!
//! Peer of csv / math / bbnf / json runtimes. Substrate-only landing
//! at cutover.E; cutover.F runs `cargo xtask regen --grammar ebnf`
//! to flip `EbnfParser::parse` onto this substrate.

pub mod arena;
pub mod builder;
pub mod document;
pub mod kind;
pub mod value;
pub mod view;

pub use arena::{EbnfArena, EbnfCompoundId};
pub use builder::EbnfStructBuilder;
pub use document::{EbnfDocument, EbnfKind, EbnfPathQuery, EbnfView};
pub use kind::{EbnfCompound, EbnfCompoundKind};
pub use value::EbnfValue;
