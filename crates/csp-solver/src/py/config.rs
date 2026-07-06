//! `SolveConfig` / `SolveStats` PyO3 bindings + the `CancelToken` cooperative
//! cancellation handle.

use pyo3::prelude::*;

use super::enums::{Ordering, Pruning};
use crate::{CancelToken as RustCancelToken, SolveConfig as RustSolveConfig};

// ═══════════════════════════════════════════════════════════════════════════
// Cooperative cancellation
// ═══════════════════════════════════════════════════════════════════════════

/// A cancellation handle for a running (or not-yet-started) solve.
///
/// Construct one, pass it into `SolveConfig(cancel=token)`, keep the same
/// `token` on the Python side, and call `token.cancel()` from *any* thread —
/// including the asyncio event-loop thread — to stop a search that's
/// currently running on a `Python::allow_threads`-released worker thread.
/// Checked at the same cadence as `node_budget` inside the search loop, so
/// cancellation typically takes effect within a handful of search nodes.
#[pyclass]
#[derive(Clone, Default)]
pub struct CancelToken {
    pub(super) inner: RustCancelToken,
}

#[pymethods]
impl CancelToken {
    #[new]
    fn new() -> Self {
        Self {
            inner: RustCancelToken::new(),
        }
    }

    /// Request cancellation. Safe to call while a search holding this
    /// token is running on another thread.
    fn cancel(&self) {
        self.inner.cancel();
    }

    #[getter]
    fn is_cancelled(&self) -> bool {
        self.inner.is_cancelled()
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SolveConfig + SolveStats
// ═══════════════════════════════════════════════════════════════════════════

/// The subset of the Rust-core [`RustSolveConfig`] surfaced to Python.
///
/// **R15 — `optimization_mode` and `restarts` are deliberately kept off the py
/// wire** (recorded alongside GF-7's restarts/CHS off-the-wire decision):
///
/// - `restarts` is accepted-but-inert in the Rust core today (its Luby driver is
///   not wired onto the unified kernel — see the core `SolveConfig` doc-comment),
///   so exposing it would advertise a knob that does nothing.
/// - `optimization_mode` only changes behavior for **cost-aware** search
///   (`solve_optimized()` on a `CostDomain`, or `solve()` over a domain carrying
///   real domain-costs / soft-constraint penalties). The py [`Csp`] wraps
///   `Csp<BitsetDomain>` and its builder surface adds only
///   not-equal / all-different / equals / less-than / greater-than constraints —
///   **no cost or soft constraints**. Under that surface `MinimizeCost` /
///   `MaximizeCost` run branch-and-bound with a `ZeroCost` evaluator (every
///   solution costs 0), collapsing to `Feasibility` in result while doing
///   strictly more work. Surfacing it would put an inert-or-pessimizing knob on
///   the wire — the same reasoning that keeps `restarts` off it.
///
/// Both stay at their Rust-core defaults (`restarts: false`,
/// `optimization_mode: Feasibility`) via the `..Default::default()` below.
/// Revisit if/when the py wire ever exposes cost/soft constraints.
#[pyclass]
#[derive(Clone)]
pub struct SolveConfig {
    #[pyo3(get, set)]
    pub pruning: Pruning,
    #[pyo3(get, set)]
    pub ordering: Ordering,
    #[pyo3(get, set)]
    pub max_solutions: usize,
    /// Maximum number of search nodes before aborting early.
    /// `None` disables the budget. Defaults to 1_000_000.
    #[pyo3(get, set)]
    pub node_budget: Option<u64>,
    /// Cooperative cancellation handle. `None` (the default) means the
    /// search cannot be cancelled externally. See [`CancelToken`].
    #[pyo3(get, set)]
    pub cancel: Option<CancelToken>,
}

#[pymethods]
impl SolveConfig {
    #[new]
    #[pyo3(signature = (pruning=Pruning::FORWARD_CHECKING, ordering=Ordering::CHRONOLOGICAL, max_solutions=1, node_budget=Some(1_000_000), cancel=None))]
    fn new(
        pruning: Pruning,
        ordering: Ordering,
        max_solutions: usize,
        node_budget: Option<u64>,
        cancel: Option<CancelToken>,
    ) -> Self {
        Self {
            pruning,
            ordering,
            max_solutions,
            node_budget,
            cancel,
        }
    }
}

impl From<&SolveConfig> for RustSolveConfig {
    fn from(c: &SolveConfig) -> Self {
        RustSolveConfig {
            pruning: c.pruning.into(),
            ordering: c.ordering.into(),
            max_solutions: c.max_solutions,
            node_budget: c.node_budget,
            cancel: c.cancel.as_ref().map(|t| t.inner.clone()),
            ..Default::default()
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct SolveStats {
    #[pyo3(get)]
    pub backtracks: u64,
    #[pyo3(get)]
    pub nodes_explored: u64,
    #[pyo3(get)]
    pub propagations: u64,
    /// `True` when the last search hit its `node_budget` and returned
    /// best-so-far rather than optimal results.
    #[pyo3(get)]
    pub budget_exceeded: bool,
    /// `True` when the last search was stopped early via a `CancelToken`
    /// rather than hitting `node_budget`. Distinct from `budget_exceeded`.
    #[pyo3(get)]
    pub cancelled: bool,
}
