//! Tranche AF.4 — `EmissionTier` lattice.
//!
//! The emission tier is an axis orthogonal to
//! [`MaterializationClass`]. Where `MaterializationClass` decides
//! **what record shape** a rule pushes (compound / span only /
//! elided), `EmissionTier` decides **what return type** the rule's
//! parse function projects to — tape-only, direct-to-struct, or a
//! lazy view-layer hybrid.
//!
//! ```text
//!                         ┌───────────┐
//!                    top →│   Tape    │  universal, always legal
//!                         └─────┬─────┘
//!                               │ join
//!                         ┌─────▼─────┐
//!                         │   Lazy    │  view-layer hybrid
//!                         └─────┬─────┘  (Rust-only; TS/WASM degrade to Tape)
//!                               │ join
//!                         ┌─────▼─────┐
//!                   bot → │  Direct   │  direct-to-struct projection
//!                         └───────────┘  (Tier B; requires FixedShape)
//! ```
//!
//! # Tiers
//!
//! - **`Tape`** — the AE default. Every rule emits a
//!   `fn __<rule>(state, tape) -> Option<TapeOffset>` that pushes
//!   a tape record. Universal — every grammar can always fall back
//!   to Tape. The top of the lattice.
//!
//! - **`Direct`** — Tier B. Rules whose body is a pure-conversion
//!   leaf (e.g., `FnDescriptor::NumberConvert` /
//!   `HexConvert` / `Constant`) with a `FixedShape` materialization
//!   class emit a second function
//!   `fn __<rule>_direct(state) -> Option<T>` alongside the tape
//!   shim. Both share the same prelude parsing logic via a private
//!   `__<rule>_inner` helper. Requires `MaterializationClass::
//!   TransparentElide` as the pre-seed (AF.1 gate), plus
//!   `FixedShape` and a closure-free body.
//!
//! - **`Lazy`** — Tier C. A view-layer hybrid: the rule emits the
//!   tape function, and the generated `<Rule>View<'p>` carries a
//!   `DirectSlot<'p>` field that dispatches between tape walk and
//!   direct value on access. Universal above Tape; Rust-only (TS
//!   and WASM backends degrade to Tape by default via the
//!   `Emitter::tier()` accessor added in AF.6).
//!
//! # Semantics
//!
//! `EmissionTier` is a CSP variable owned by the AF.3 cross-rule
//! strategy solve. The solver picks a tier per rule constrained by
//! `TierFollowsMaterialization` (a rule's tier is bounded above by
//! its materialization class) and `ParentCompatibility` (a parent
//! Tape rule calling a Direct child pays a `cross_module_coercion`
//! cost at the call site). After AF.5 the per-rule decision is
//! written to `ir.emission_tier` and read by the backend emitter in
//! AF.6.
//!
//! # Ordering
//!
//! The lattice is totally ordered `Direct < Lazy < Tape`:
//!
//! - `top()` = `Tape` — the widest, always-legal choice.
//! - `bottom()` = `Direct` — the narrowest, tightest-constraint
//!   choice.
//! - `tier_join(a, b)` = the weakest tier that covers both `a` and
//!   `b`. Monotone and commutative.
//! - `tier_meet(a, b)` = the strongest tier that is covered by both.
//!
//! Joins are used when combining constraints from multiple
//! consumers (e.g., a rule called by both Tape and Direct parents
//! joins to Tape, because Tape is universal). Meets are used when
//! reconciling upper and lower bounds during CSP propagation.

use serde::{Deserialize, Serialize};

/// The per-rule emission tier selected by the AF.3 cross-rule CSP
/// and read by the AF.6 backend emitter.
///
/// The variants are ordered top-to-bottom as `Tape > Lazy > Direct`;
/// `Tape` is universal and always legal, `Direct` is the tightest
/// specialization. See the module-level docs for the full lattice
/// definition and the AF.3–AF.6 tranche sequencing.
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, Hash, Serialize, Deserialize,
)]
pub enum EmissionTier {
    /// Tier A — tape-only emission. Universal default. Every rule
    /// emits a `fn __<rule>(state, tape) -> Option<TapeOffset>`.
    #[default]
    Tape,

    /// Tier C — view-layer lazy projection. The tape function is
    /// emitted AND the generated view carries a `DirectSlot<'p>`
    /// that dispatches between tape walk and direct value on
    /// access. Rust-only; non-Rust backends fall back to `Tape`.
    Lazy,

    /// Tier B — direct-to-struct projection. Emits
    /// `fn __<rule>_direct(state) -> Option<T>` alongside the tape
    /// shim. Both share a private `__<rule>_inner` prelude helper.
    /// Requires `FixedShape` materialization, a closure-free body,
    /// and a single-site consumer model.
    Direct,
}

impl EmissionTier {
    /// The top of the lattice — the widest, always-legal tier.
    #[inline]
    pub const fn top() -> Self {
        Self::Tape
    }

    /// The bottom of the lattice — the tightest-constraint tier.
    #[inline]
    pub const fn bottom() -> Self {
        Self::Direct
    }

    /// The variant's rank in the total order. `Direct = 0 <
    /// Lazy = 1 < Tape = 2`. Used by [`tier_join`] and
    /// [`tier_meet`].
    #[inline]
    pub const fn rank(self) -> u8 {
        match self {
            Self::Direct => 0,
            Self::Lazy => 1,
            Self::Tape => 2,
        }
    }

    /// Human-readable label used in diagnostics and CSP reports.
    #[inline]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Tape => "tape",
            Self::Lazy => "lazy",
            Self::Direct => "direct",
        }
    }

    /// True when emission at this tier produces a tape record. Only
    /// `Direct` does NOT push a tape record (it returns the typed
    /// value directly). `Lazy` still emits the tape function for the
    /// walk-on-demand path.
    #[inline]
    pub const fn emits_tape(self) -> bool {
        !matches!(self, Self::Direct)
    }

    /// True when emission at this tier produces a direct-to-struct
    /// shim function (`__<rule>_direct`). `Direct` and `Lazy` both
    /// emit the shim; only `Tape` skips it.
    #[inline]
    pub const fn emits_direct_shim(self) -> bool {
        !matches!(self, Self::Tape)
    }
}

/// Widen two tiers to the weakest tier that covers both — the
/// lattice join.
///
/// Monotone: `tier_join(a, b) >= a` and `tier_join(a, b) >= b`.
/// Commutative: `tier_join(a, b) == tier_join(b, a)`. Idempotent:
/// `tier_join(a, a) == a`. Absorbs toward `Tape` (the top):
/// `tier_join(_, Tape) == Tape`.
///
/// Used by the `ParentCompatibility` constraint when combining the
/// tiers of a rule's callers.
#[inline]
pub fn tier_join(a: EmissionTier, b: EmissionTier) -> EmissionTier {
    if a.rank() >= b.rank() { a } else { b }
}

/// Narrow two tiers to the strongest tier covered by both — the
/// lattice meet.
///
/// Monotone: `tier_meet(a, b) <= a` and `tier_meet(a, b) <= b`.
/// Commutative. Idempotent. Absorbs toward `Direct` (the bottom):
/// `tier_meet(_, Direct) == Direct`.
///
/// Used by `TierFollowsMaterialization` when reconciling the
/// upstream materialization class's upper bound (`MustTape` forces
/// `Tape`; `TapeSpanOnly` permits `Lazy` or `Tape`; `TransparentElide`
/// permits `Direct`, `Lazy`, or `Tape`) with the CSP solver's
/// cost-driven preference.
#[inline]
pub fn tier_meet(a: EmissionTier, b: EmissionTier) -> EmissionTier {
    if a.rank() <= b.rank() { a } else { b }
}
