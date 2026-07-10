//! Solver configuration vocabulary and the [`Csp<D>`] problem container.
//!
//! The pruning / propagation / optimization enums, the [`SolveConfig`] bundle
//! (+ its `Default`), the [`SolveStats`] counters, and the `Csp<D>` struct
//! definition. The builder surface lives in [`crate::csp`]; the search
//! dispatch in [`crate::csp::solve`].
//!
//! Tests: `tests/solver.rs` (config matrix across every pruning × ordering).

use crate::adjacency::Adjacency;
use crate::cancel::CancelToken;
use crate::constraint::ConstraintEnum;
use crate::domain::Domain;
use crate::ordering::Ordering;
use crate::variable::Variable;

/// Pruning strategy for backtracking search.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pruning {
    /// No pruning — pure backtracking.
    None,
    /// Forward checking: prune neighbors of the assigned variable.
    ForwardChecking,
    /// MAC: Maintaining Arc Consistency (AC-3 after each assignment).
    Ac3,
    /// Hybrid: forward checking + singleton propagation.
    AcFc,
}

/// Propagation strategy for `propagate_with()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropagationStrategy {
    /// Auto-select: AC-3 if finalize() was called, sweep otherwise.
    Auto,
    /// AC-3 worklist with adjacency graph. Requires finalize().
    Ac3,
    /// Fixed-point sweep over all constraints. No adjacency needed.
    Sweep,
}

/// Optimization mode for the solver.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationMode {
    /// Find any feasible solution (existing behavior).
    Feasibility,
    /// Find the solution minimizing total cost (domain costs + soft penalties).
    MinimizeCost,
    /// Find the solution maximizing total cost.
    MaximizeCost,
}

/// Solve configuration, isomorphic to Python's CSP constructor arguments.
#[derive(Debug, Clone)]
pub struct SolveConfig {
    pub pruning: Pruning,
    pub ordering: Ordering,
    /// Cap on solutions returned. `1` is a satisfiability probe.
    ///
    /// On a problem with more than one solution, *which* first solution comes
    /// back under `Pruning::Ac3` is trajectory-dependent — different
    /// pruning/ordering combinations may return different valid members of the
    /// solution set. Each is a genuine member (see `kernel-soundness-closure.md`
    /// §7.2), but callers must not depend on the specific choice. Only
    /// enumerate-all (`usize::MAX`) has a defined, pruning-invariant set.
    pub max_solutions: usize,
    /// Optimization mode. Defaults to `Feasibility` (pure constraint satisfaction).
    pub optimization_mode: OptimizationMode,
    /// Maximum number of search nodes (backtrack / branch-and-bound
    /// recursions) before the solver aborts early and returns whatever
    /// solutions it has found so far. `None` disables the budget.
    ///
    /// Defaults to `Some(1_000_000)` so an unbounded pathological
    /// search cannot hang a caller. When the budget is hit,
    /// [`SolveStats::budget_exceeded`] is set to `true` on the
    /// returning `Csp::stats()`. Callers that care about optimality
    /// should branch on this flag and either accept the best-so-far
    /// solution or fall back to a trivial per-variable pick.
    pub node_budget: Option<u64>,
    /// Cooperative cancellation flag, checked at the same cadence as
    /// `node_budget`. `None` (the default) means the search cannot be
    /// cancelled externally. Set this to a [`CancelToken`] clone and keep
    /// another clone on the calling side to request an early stop — e.g.
    /// from Python, released via `Python::allow_threads`, when an
    /// `asyncio.wait_for` timeout elapses. See [`SolveStats::cancelled`].
    pub cancel: Option<CancelToken>,
}

impl Default for SolveConfig {
    /// Ac3 + FailFirst (Pass-2 D6, ratified; ships in 0.2.0): the production
    /// posture every measured surface converged on. Coordination-gated with
    /// bbnf-lang's two live `finalize()+solve_optimized()` consumers via the
    /// enforced-compile sync gate (`sync-csp-solver-vendor.sh --verify`).
    fn default() -> Self {
        Self {
            pruning: Pruning::Ac3,
            ordering: Ordering::FailFirst,
            max_solutions: 1,
            optimization_mode: OptimizationMode::Feasibility,
            node_budget: Some(1_000_000),
            cancel: None,
        }
    }
}

/// Solver statistics.
#[derive(Debug, Clone, Default)]
pub struct SolveStats {
    pub backtracks: u64,
    pub nodes_explored: u64,
    pub propagations: u64,
    /// Set to `true` when the last search hit its
    /// [`SolveConfig::node_budget`] and aborted early.
    /// Solutions returned alongside this flag are best-so-far, not
    /// necessarily optimal.
    pub budget_exceeded: bool,
    /// Set to `true` when the last search was stopped early via
    /// [`SolveConfig::cancel`] rather than hitting `node_budget`.
    /// Distinct from `budget_exceeded`: this is an externally requested
    /// stop, not a self-imposed cap.
    pub cancelled: bool,
}

/// The main CSP solver struct.
///
/// Generic over the domain type `D`. Build a problem by adding variables and
/// constraints, call `finalize()` to build the adjacency graph, then `solve()`.
///
/// The builder methods (`add_*`, `finalize`) live in [`crate::csp`]; the
/// solve/propagate dispatch in [`crate::csp::solve`].
pub struct Csp<D: Domain> {
    pub variables: Vec<Variable<D>>,
    pub(crate) constraints: Vec<ConstraintEnum<D>>,
    pub(crate) adjacency: Option<Adjacency>,
    pub(crate) stats: SolveStats,
    /// Per-constraint weights for the Mrv weighted-degree scan.
    pub(crate) constraint_weights: Vec<f64>,
    /// For each variable, the indices of constraints involving it.
    pub(crate) var_constraint_ids: Vec<Vec<usize>>,
}
