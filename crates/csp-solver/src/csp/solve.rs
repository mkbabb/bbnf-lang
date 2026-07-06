//! Solve / propagate dispatch — routes a finalized [`Csp<D>`] into the
//! propagation engines (AC-3, monotonic sweep) and the unified search kernel
//! (feasibility / branch-and-bound). Also home to the [`Unsatisfiable`]
//! control-flow marker, whose sole consumers are `propagate` / `propagate_with`.
//!
//! Tests: `tests/solver.rs` (general solve correctness),
//! `tests/solution_set_invariance.rs` (dispatch parity property test).

use crate::config::{Csp, OptimizationMode, PropagationStrategy, Pruning, SolveConfig, SolveStats};
use crate::constraint::VarId;
use crate::domain::{self, Domain};
use crate::solver::search::{self, SearchParams};
use crate::solver::{self, Solution, optimize};

impl<D: Domain> Csp<D> {
    /// Propagate constraints to a fixed point (auto-select strategy).
    pub fn propagate(&mut self) -> Result<(), Unsatisfiable>
    where
        D::Value: PartialEq + 'static,
    {
        self.propagate_with(PropagationStrategy::Auto)
    }

    /// Propagate constraints with an explicit strategy.
    pub fn propagate_with(&mut self, strategy: PropagationStrategy) -> Result<(), Unsatisfiable>
    where
        D::Value: PartialEq + 'static,
    {
        match strategy {
            PropagationStrategy::Auto => {
                if self.adjacency.is_some() {
                    self.propagate_with(PropagationStrategy::Ac3)
                } else {
                    self.propagate_with(PropagationStrategy::Sweep)
                }
            }
            PropagationStrategy::Ac3 => {
                // Disjoint field borrows — no adjacency clone (kernel S5).
                let adjacency = self.adjacency.as_ref().ok_or(Unsatisfiable)?;
                // Caller-owned reusable AC-3 worklist scratch (zero-alloc P2-2).
                let mut worklist = solver::ac3::BitsetWorklist::new(self.constraints.len());
                solver::ac3::ac3_full(
                    &mut self.variables,
                    &self.constraints,
                    adjacency,
                    &mut self.stats,
                    &mut worklist,
                    search::PERMANENT_DEPTH,
                )
            }
            PropagationStrategy::Sweep => solver::monotonic::propagate_monotonic(
                &mut self.variables,
                &self.constraints,
                &mut self.stats,
            ),
        }
    }

    /// Run backtracking search with the given configuration.
    ///
    /// Returns up to `config.max_solutions` solutions.
    /// When `optimization_mode` is `MinimizeCost` or `MaximizeCost`, uses
    /// branch-and-bound search and returns solutions sorted by cost (best first).
    pub fn solve(&mut self, config: &SolveConfig) -> Vec<Solution<D>>
    where
        D::Value: PartialEq + 'static,
    {
        assert!(self.adjacency.is_some(), "call finalize() before solve()");

        self.stats = SolveStats::default();

        // Reset all variables to their original domains.
        for v in &mut self.variables {
            v.reset();
        }

        // Root propagation, symmetric with `solve_with_given`'s initial AC-3.
        // Only the MAC (`Ac3`) strategy establishes global arc-consistency, so
        // it is the one whose contract calls for propagating the root before
        // search; the weaker forward-checking strategies prune relative to an
        // assignment and do no root work (`forward_check` on an empty
        // assignment is a no-op). Runs at `PERMANENT_DEPTH` so search's
        // depth-keyed undo never reverts it.
        if config.pruning == Pruning::Ac3 {
            let adjacency = self.adjacency.as_ref().unwrap();
            let mut worklist = solver::ac3::BitsetWorklist::new(self.constraints.len());
            let _ = solver::ac3::ac3_full(
                &mut self.variables,
                &self.constraints,
                adjacency,
                &mut self.stats,
                &mut worklist,
                search::PERMANENT_DEPTH,
            );
        }

        let params = SearchParams {
            pruning: config.pruning,
            ordering: config.ordering,
            max_solutions: config.max_solutions,
            node_budget: config.node_budget,
            cancel: config.cancel.clone(),
        };
        let adjacency = self.adjacency.as_ref().unwrap();

        match config.optimization_mode {
            OptimizationMode::Feasibility => search::feasibility_search(
                &mut self.variables,
                &self.constraints,
                adjacency,
                &mut self.constraint_weights,
                &self.var_constraint_ids,
                &params,
                &mut self.stats,
                None,
            ),
            mode @ (OptimizationMode::MinimizeCost | OptimizationMode::MaximizeCost) => {
                // ZeroCost evaluator — domain costs are 0. For CostDomain-aware
                // optimization, use `solve_optimized()`.
                search::branch_and_bound(
                    &mut self.variables,
                    &self.constraints,
                    adjacency,
                    &mut self.constraint_weights,
                    &self.var_constraint_ids,
                    &params,
                    &mut self.stats,
                    mode == OptimizationMode::MaximizeCost,
                    &optimize::ZeroCost,
                )
            }
        }
    }

    /// Solve with initial propagation for pre-assigned ("given") values.
    ///
    /// Analogous to Python's `solve_with_initial_propagation`.
    /// Pre-assigns the given values, propagates constraints, then searches.
    pub fn solve_with_given(
        &mut self,
        config: &SolveConfig,
        given: &[(VarId, D::Value)],
    ) -> Vec<Solution<D>>
    where
        D::Value: PartialEq + 'static,
    {
        assert!(
            self.adjacency.is_some(),
            "call finalize() before solve_with_given()"
        );

        self.stats = SolveStats::default();

        // Reset all variables to their original domains.
        for v in &mut self.variables {
            v.reset();
        }

        // Apply given values: restrict domain to singleton. Uses
        // `Domain::restrict_to` directly (not `Variable::restrict_to`) —
        // these reductions are permanent, not undo-logged (zero-alloc: O(1)
        // bitmask restrict for `BitsetDomain`, not a collect-then-remove loop).
        for (var, val) in given {
            let _ = self.variables[*var as usize].domain.restrict_to(val);
        }

        // One-hop propagation: remove each given value from its non-given
        // neighbors. Also permanent (direct `remove`).
        {
            let adjacency = self.adjacency.as_ref().unwrap();
            for (var, val) in given {
                for &neighbor in adjacency.neighbors_of_var(*var) {
                    let is_given = given.iter().any(|(gv, _)| *gv == neighbor);
                    if !is_given {
                        self.variables[neighbor as usize].domain.remove(val);
                    }
                }
            }
        }

        // Initial AC-3 propagation from the given cells, at `PERMANENT_DEPTH`.
        // The kernel searches from a strictly deeper frame, so its depth-keyed
        // undo can never revert these reductions — closing the depth-0 seam
        // where the first failed root candidate un-pruned this AC-3 via
        // `restore(0)`. Worklist is caller-owned reusable scratch (zero-alloc).
        {
            let adjacency = self.adjacency.as_ref().unwrap();
            let mut worklist = solver::ac3::BitsetWorklist::new(self.constraints.len());
            let _ = solver::ac3::ac3_full(
                &mut self.variables,
                &self.constraints,
                adjacency,
                &mut self.stats,
                &mut worklist,
                search::PERMANENT_DEPTH,
            );
        }

        let params = SearchParams {
            pruning: config.pruning,
            ordering: config.ordering,
            max_solutions: config.max_solutions,
            node_budget: config.node_budget,
            cancel: config.cancel.clone(),
        };
        let adjacency = self.adjacency.as_ref().unwrap();

        search::feasibility_search(
            &mut self.variables,
            &self.constraints,
            adjacency,
            &mut self.constraint_weights,
            &self.var_constraint_ids,
            &params,
            &mut self.stats,
            Some(given),
        )
    }

    /// Run optimization search with a custom cost evaluator.
    ///
    /// This is the most flexible entry point: you supply a `DomainCostEval`
    /// that defines how domain values are costed. Use `solve()` with
    /// `OptimizationMode::MinimizeCost` if you only need soft constraint
    /// penalties (zero domain cost), or `solve_optimized()` if your domain
    /// implements `CostDomain`.
    pub fn solve_with_cost_eval(
        &mut self,
        config: &SolveConfig,
        cost_eval: &dyn optimize::DomainCostEval<D>,
    ) -> Vec<Solution<D>>
    where
        D::Value: PartialEq + 'static,
    {
        assert!(
            self.adjacency.is_some(),
            "call finalize() before solve_with_cost_eval()"
        );

        self.stats = SolveStats::default();
        for v in &mut self.variables {
            v.reset();
        }

        let params = SearchParams {
            pruning: config.pruning,
            ordering: config.ordering,
            max_solutions: config.max_solutions,
            node_budget: config.node_budget,
            cancel: config.cancel.clone(),
        };
        let adjacency = self.adjacency.as_ref().unwrap();
        search::branch_and_bound(
            &mut self.variables,
            &self.constraints,
            adjacency,
            &mut self.constraint_weights,
            &self.var_constraint_ids,
            &params,
            &mut self.stats,
            config.optimization_mode == OptimizationMode::MaximizeCost,
            cost_eval,
        )
    }

    /// Get solver statistics from the last run.
    pub fn stats(&self) -> &SolveStats {
        &self.stats
    }

    /// Get a reference to the adjacency graph (available after `finalize()`).
    pub fn adjacency(&self) -> Option<&crate::adjacency::Adjacency> {
        self.adjacency.as_ref()
    }
}

impl<D: Domain> Default for Csp<D> {
    fn default() -> Self {
        Self::new()
    }
}

impl<D: domain::CostDomain> Csp<D> {
    /// Run optimization search using the domain's `CostDomain` implementation
    /// for value costing. This is the ergonomic entry point when your domain
    /// type implements `CostDomain`.
    pub fn solve_optimized(&mut self, config: &SolveConfig) -> Vec<Solution<D>>
    where
        D::Value: PartialEq + 'static,
    {
        self.solve_with_cost_eval(config, &optimize::CostDomainEval)
    }
}

/// Error type for unsatisfiable constraints.
#[derive(Debug, Clone)]
pub struct Unsatisfiable;

impl std::fmt::Display for Unsatisfiable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CSP is unsatisfiable")
    }
}

impl std::error::Error for Unsatisfiable {}
