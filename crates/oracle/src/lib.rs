//! `bbnf-oracle`: the independent oracles of the G1 reading (sk-v27 LW3).
//!
//! - [`obs`]: the declared observation function and the failure contract
//!   (LW3.COMP-4a).
//! - [`arm`], [`arms`]: the implementations G1 runs, each reporting an
//!   outcome per input; [`oracles`]: the independent judges; [`harness`]: one
//!   harness, grammar × input × arm → `obs()` plus the failure contract, with
//!   its self-check (LW3.COMP-3a).
//! - [`registry`]: the 9 grammars and the held-out set with their corpora,
//!   read through custody (R-c); [`npm`]: the pinned JavaScript oracles.
//! - [`sample`]: the deterministic generators the oracle's own laws use.
//!
//! The crate is `publish = false` and is never a dependency of a published
//! crate (`cargo tree -i bbnf-oracle` lists dev edges only).

pub mod arm;
pub mod arms;
pub mod harness;
pub mod npm;
pub mod obs;
pub mod oracles;
pub mod registry;
pub mod sample;
