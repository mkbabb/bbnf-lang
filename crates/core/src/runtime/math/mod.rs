//! AZ-II.cutover.E (Phase 2) — Math struct-direct runtime.
//!
//! Peer of `csv` / `bbnf` / `json` runtimes. Substrate-only landing
//! at cutover.E; cutover.F runs `cargo xtask regen --grammar math`
//! to flip `MathParser::parse` onto this substrate.

pub mod arena;
pub mod builder;
pub mod document;
pub mod kind;
pub mod value;
pub mod view;

pub use arena::{MathArena, MathCompoundId};
pub use builder::MathStructBuilder;
pub use document::{MathDocument, MathKind, MathPathQuery, MathView};
pub use kind::{MathCompound, MathCompoundKind};
pub use value::MathValue;
