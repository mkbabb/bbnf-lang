//! AX.W1.A — first-class JSON `Value` type.
//!
//! Exposes a runtime JSON value tree isomorphic to `sonic_rs::Value`,
//! built from a BBNF-parsed tape through a shape-emission-authoritative
//! walker (invariant 20). The module sits under
//! `backend::rust::view::` alongside the per-rule codegen helpers
//! because the consumer-facing projection layer is Rust-specific (TS /
//! WASM backends will carry their own sibling `view/json/` modules
//! when those projections land in later waves).
//!
//! Submodule layout:
//!
//! - [`number`] — `Number` + private `N` enum, isomorphic to
//!   `sonic_rs::Number`'s integer-preserving split (PosInt / NegInt /
//!   Float).
//! - [`value`] — `Value<'input>` six-variant enum, `JsonRuleIds`
//!   tape-walker context, `From<sonic_rs::Value>` +
//!   `PartialEq<sonic_rs::Value>` bridges.

pub mod number;
pub mod value;

pub use number::{N, Number};
pub use value::{JsonRuleIds, Value};
