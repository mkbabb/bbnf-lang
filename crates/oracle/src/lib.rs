//! `bbnf-oracle`: the independent oracles of the G1 reading (sk-v27 LW3).
//!
//! - [`obs`]: the declared observation function and the failure contract
//!   (LW3.COMP-4a).
//! - [`sample`]: the deterministic generators the oracle's own laws use.
//!
//! The crate is `publish = false` and is never a dependency of a published
//! crate (`cargo tree -i bbnf-oracle` lists dev edges only).

pub mod sample;
pub mod obs;
