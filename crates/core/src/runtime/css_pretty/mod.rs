//! AZ-II.cutover.E (Phase 2) — CssPretty struct-direct runtime.
//!
//! Peer of csv / math / bbnf / json runtimes. Substrate-only landing
//! at cutover.E; cutover.F runs `cargo xtask regen --grammar css_pretty`
//! to flip `CssPrettyParser::parse` onto this substrate.

pub mod arena;
pub mod builder;
pub mod document;
pub mod kind;
pub mod value;
pub mod view;

pub use arena::{CssPrettyArena, CssPrettyCompoundId};
pub use builder::CssPrettyStructBuilder;
pub use document::{CssPrettyDocument, CssPrettyKind, CssPrettyPathQuery, CssPrettyView};
pub use kind::{CssPrettyCompound, CssPrettyCompoundKind};
pub use value::CssPrettyValue;
