//! `materialization` — per-`NodeId` class + classification pass.
//!
//! Tranche AB.0. The materialization substrate — a three-variant
//! tape-safe lattice, a bottom-up classifier, and a consumer-pin
//! fix-up sweep. Produces the `ir.materialization` sidecar consumed
//! by the CSP joint solve (AB.1) and the tape-first emitter (AB.2).
//!
//! See `docs/tranches/AB.md` for the full architectural design.

pub mod classify;
pub mod lattice;
pub mod pin_sweep;

pub use classify::classify_materialization;
pub use lattice::{MaterializationClass, mat_join};
