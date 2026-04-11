//! `MaterializationClass` — the 3-variant tape-safe lattice.
//!
//! Tranche AB.0. Every grammar node in a tape-emitting backend falls
//! into one of three classes, ordered by a monotone lattice where
//! disagreement widens toward the safe top:
//!
//! ```text
//!                MustTape           ← top (safest default, always legal)
//!                   |
//!              TapeSpanOnly
//!                   |
//!             TransparentElide      ← bottom (most aggressive elision)
//! ```
//!
//! - **`MustTape`** — full tape record with `push_compound(...)`. The
//!   universal legal default.
//! - **`TapeSpanOnly`** — single `push_leaf(Span, ...)` record, no
//!   children, no compound header.
//! - **`TransparentElide`** — no record, no function. Inlined at every
//!   call site; the inlined body's own class decides what (if any)
//!   record gets pushed.
//!
//! Dropped from Tranche AB: `DirectProjection` (scalar/aggregate
//! return) and `TapeCompact` (bulk-encoded repeats). Both break the
//! one-ABI commitment — they live in a post-AB tranche over the
//! stable tape.

use serde::{Deserialize, Serialize};

/// A grammar node's materialization commitment.
///
/// Produced by [`classify_materialization`] and refined by the CSP
/// solver (Tranche AB.1). Consumed by the emitter (Tranche AB.2) to
/// decide per-rule prelude / epilogue shape and per-ref inlining.
///
/// The variants are ordered: `TransparentElide < TapeSpanOnly <
/// MustTape`. A monotone lattice join always widens toward
/// `MustTape`.
///
/// [`classify_materialization`]: super::classify::classify_materialization
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum MaterializationClass {
    /// Full tape record with children. Universal legal default.
    ///
    /// Pinned by `@pretty`, `@debug`, `preserve_identity`, closure-
    /// typed `Map`, multi-site consumer reuse, or any node whose
    /// descendants aren't uniformly elision-safe. Emitter shape:
    /// `fn __rule(state, tape) -> Option<TapeOffset>` with a
    /// `mark_children` prelude and a `push_compound` epilogue.
    #[default]
    MustTape,

    /// Single `push_leaf(TapeKind::Span, ...)` record. No children,
    /// no compound header.
    ///
    /// Emitted for leaves whose entire observable output is a
    /// contiguous span: punctuation tokens, `@token`-annotated rules,
    /// `Negate`, `OptionalWhitespace` left-sides, `Skip` left-sides.
    /// Still returns `Option<TapeOffset>` — this is a tape record,
    /// just a smaller one.
    TapeSpanOnly,

    /// No record, no function. Inlined at every call site.
    ///
    /// Legal only for rules whose body is itself elision-compatible
    /// (transparent aliases, single-child wrappers). The caller's
    /// control flow is unchanged; the inlined body's own class
    /// decides what record (if any) gets pushed.
    TransparentElide,
}

impl MaterializationClass {
    /// Human-readable name for diagnostics.
    pub fn name(self) -> &'static str {
        match self {
            MaterializationClass::MustTape => "must_tape",
            MaterializationClass::TapeSpanOnly => "tape_span_only",
            MaterializationClass::TransparentElide => "transparent_elide",
        }
    }

    /// True iff this class emits a tape record (leaf or compound).
    #[inline]
    pub fn emits_record(self) -> bool {
        matches!(self, Self::MustTape | Self::TapeSpanOnly)
    }

    /// The numeric order of this class in the lattice — lower is
    /// more aggressive elision, higher is safer. Used to implement
    /// [`mat_join`] without pattern matching.
    #[inline]
    fn rank(self) -> u8 {
        match self {
            Self::TransparentElide => 0,
            Self::TapeSpanOnly => 1,
            Self::MustTape => 2,
        }
    }

    /// Top of the lattice — the safest default. Every node that the
    /// classifier can't positively demote falls through here.
    #[inline]
    pub const fn top() -> Self {
        Self::MustTape
    }

    /// Bottom of the lattice — the most aggressive elision.
    #[inline]
    pub const fn bottom() -> Self {
        Self::TransparentElide
    }
}

/// Monotone lattice join — widen toward `MustTape`.
///
/// When two siblings disagree the result is the safer (higher)
/// class. This is the primary monotone operator used by
/// [`classify_materialization`] when combining child classes into a
/// parent decision.
///
/// # Properties
///
/// - **Idempotent**: `mat_join(x, x) == x` for every `x`.
/// - **Commutative**: `mat_join(a, b) == mat_join(b, a)`.
/// - **Monotone**: `mat_join(a, b) >= a` and `>= b`.
///
/// These properties are exercised by the unit tests in
/// `crates/ir/tests/materialization.rs`.
///
/// [`classify_materialization`]: super::classify::classify_materialization
#[inline]
pub fn mat_join(a: MaterializationClass, b: MaterializationClass) -> MaterializationClass {
    if a.rank() >= b.rank() { a } else { b }
}
