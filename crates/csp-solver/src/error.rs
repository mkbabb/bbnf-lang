//! Unified error family for the solver's public API surface.
//!
//! Reconciled from grand-audit Pass 2 prototype 14 (`api-error-taxonomy`)
//! onto the Pass-3 composed tree (Pass 3 `py-module-reconciliation`, T9).
//!
//! Before this, the crate had two independent, incomplete error shapes:
//! `Unsatisfiable` (a bare unit struct, `lib.rs`) and `AssignmentError`
//! (`builder/assignment.rs`, five variants scoped to the assignment-problem
//! builder only, and — per the Pass-1 `rust-cop-builder` finding —
//! conflating budget-exhaustion with genuine infeasibility). Every
//! downstream binding (`py/`, wasm's `isomorphic.rs`) re-derived its own ad
//! hoc mapping of "something went wrong" onto a bare
//! `PyRuntimeError`/`JsError` string, which is exactly the "silent/conflated
//! handling" the audit's fail-explicit precept bans (Pass-1 ledger R15, B0).
//!
//! `CspError` is the one family every layer should map 1:1 rather than
//! reinvent: PyO3 via `create_exception!` (`py/errors.rs`), wasm via a typed
//! `WasmCspError` that stamps a `.code` onto a genuine `Error` instance
//! (`wasm/src/errors.rs`, not reconciled in this pass — see report), and the
//! FastAPI JSON error envelope (`web/api/src/app/core/errors.py`, likewise
//! not reconciled here) via the *Python* exception classes that `py/`
//! raises. `code()` is the one string all downstream layers agree on.
//!
//! Tests: `tests/error.rs`.

use std::fmt;

/// Unified error family for `Csp`/Sudoku/Futoshiki operations exposed
/// across the PyO3 and wasm boundaries.
///
/// Deliberately flat (no nested `Box<dyn Error>` source chaining) — every
/// downstream binding needs to pattern-match this by value to pick an
/// exception class / HTTP status, and a flat enum is the cheapest thing
/// that supports that without reflection.
#[derive(Debug, Clone, PartialEq)]
pub enum CspError {
    /// No solution exists under the current constraints/propagation.
    /// Maps from the crate's existing [`crate::Unsatisfiable`] marker (kept
    /// as the internal propagation-layer type; this is the binding-facing
    /// supertype).
    Unsatisfiable,
    /// The search aborted after [`crate::SolveConfig::node_budget`] nodes;
    /// any solutions returned are best-so-far, not verified optimal or
    /// exhaustive. Distinct from `Unsatisfiable` — conflating the two was
    /// Pass-1 finding R8/F12 (`AssignmentBuilder` and `SudokuCSP` both did
    /// this).
    BudgetExceeded,
    /// A caller-supplied argument was structurally invalid: an
    /// out-of-range position, a malformed numeric key, an unknown enum
    /// value, mismatched dimensions, etc. Carries a human-readable detail
    /// string (never leaked Rust-internal panic/debug detail — callers
    /// construct this variant explicitly, it is never built from a caught
    /// panic).
    InvalidInput {
        /// What was wrong, safe to surface to an API client verbatim.
        detail: String,
    },
    /// A wall-clock deadline elapsed before the search produced a result.
    /// Distinct from `BudgetExceeded` (a node-count budget, checked at the
    /// same cadence but triggered by *count* not *time*). Forward-declared
    /// for the wall-clock `time_budget` deferred item (synthesis-pass1 §3.1
    /// N11) — the cooperative cancellation flag a caller's HTTP-layer
    /// timeout should set, rather than the FastAPI `asyncio.wait_for`
    /// theater that today cancels only the awaiting coroutine, never the
    /// search (Pass-1 `fastapi-service` F3 / `pyo3-boundary`).
    Timeout,
}

impl fmt::Display for CspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unsatisfiable => write!(f, "no solution exists under the given constraints"),
            Self::BudgetExceeded => {
                write!(
                    f,
                    "search exceeded its node budget before finding a solution"
                )
            }
            Self::InvalidInput { detail } => write!(f, "invalid input: {detail}"),
            Self::Timeout => write!(f, "search exceeded its wall-clock deadline"),
        }
    }
}

impl std::error::Error for CspError {}

impl CspError {
    /// Stable machine-readable discriminant. This exact string is what
    /// `py/errors.rs`'s exception class *names* are derived from, what a
    /// wasm `CspJsError.code` would carry across that boundary, and what a
    /// FastAPI JSON error envelope's `error.code` field would be — the
    /// single vocabulary every layer of the taxonomy shares.
    pub const fn code(&self) -> &'static str {
        match self {
            Self::Unsatisfiable => "UNSATISFIABLE",
            Self::BudgetExceeded => "BUDGET_EXCEEDED",
            Self::InvalidInput { .. } => "INVALID_INPUT",
            Self::Timeout => "TIMEOUT",
        }
    }

    /// Convenience constructor — the one place `format!` call sites
    /// collapse into, instead of each binding hand-rolling its own
    /// `PyValueError::new_err(format!(...))` / `JsError::new(&format!(...))`.
    pub fn invalid_input(detail: impl Into<String>) -> Self {
        Self::InvalidInput {
            detail: detail.into(),
        }
    }
}

/// The propagation layer's existing marker type converts losslessly — every
/// current `?`-using call site (`Csp::propagate`, `Csp::propagate_with`)
/// keeps compiling unchanged; only the bindings need to switch their
/// `map_err` target.
impl From<crate::Unsatisfiable> for CspError {
    fn from(_: crate::Unsatisfiable) -> Self {
        CspError::Unsatisfiable
    }
}

/// `AssignmentBuilder`'s error family converts too, so `py/`/wasm call
/// sites that touch the assignment/COP path get the same typed exceptions
/// as the Sudoku path for free. Note this conversion is itself evidence of
/// Pass-1 finding R8: `Infeasible` collapses onto `Unsatisfiable` here
/// because `AssignmentError` has no distinct budget-exhaustion variant yet
/// — fixing that is a separate, already ledgered item, not silently patched
/// over by this mapping.
impl From<crate::AssignmentError> for CspError {
    fn from(e: crate::AssignmentError) -> Self {
        match e {
            crate::AssignmentError::Infeasible => CspError::Unsatisfiable,
            other => CspError::invalid_input(other.to_string()),
        }
    }
}
