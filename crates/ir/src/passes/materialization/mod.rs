//! `materialization` — per-`NodeId` class + classification pass.
//!
//! Tranche AB.0. The materialization substrate — a three-variant
//! tape-safe lattice, a bottom-up classifier, and a consumer-pin
//! fix-up sweep. Produces the `ir.materialization` sidecar consumed
//! by the CSP joint solve (AB.1) and the tape-first emitter (AB.2).
//!
//! Tranche AF.1 — the classifier activates the dormant
//! [`EClassFacts`](crate::egraph::analysis::EClassFacts) lattice as
//! a pre-seed for `TransparentElide` eligibility. `classify.rs`
//! computes per-`NodeId` facts bottom-up (mirroring
//! `GrammarAnalysis::make`) with a fixed-point iteration for
//! cross-rule `Ref` propagation, then gates every `TransparentElide`
//! return on `is_fixed_shape && elision_safe && closure_free &&
//! all_descendants_elidable`. A node whose facts disagree demotes
//! to `TapeSpanOnly`.
//!
//! Tranche AF.4 — [`EmissionTier`] lives alongside
//! `MaterializationClass` as an orthogonal axis. Where the class
//! decides what record shape a rule pushes, the tier decides what
//! return type the rule's parse function projects to: tape-only
//! (AE default), direct-to-struct (Tier B), or lazy view-layer
//! hybrid (Tier C). The per-rule tier is a CSP variable owned by
//! the AF.3 cross-rule solve and decoded into `ir.emission_tier`
//! by AF.5.
//!
//! See `docs/tranches/AB.md` and `docs/tranches/AF.md` for the full
//! architectural design.

pub mod classify;
pub mod emission_tier;
pub mod lattice;
pub mod pin_sweep;

pub use classify::{
    EClassFactsMap, classify_materialization, classify_materialization_with_facts,
    compute_eclass_facts,
};
pub use emission_tier::{EmissionTier, tier_join, tier_meet};
pub use lattice::{MaterializationClass, mat_join};
