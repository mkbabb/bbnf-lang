//! Branch-and-bound optimization search.
//!
//! Extends backtracking with cost tracking: at each search node, computes a
//! lower bound on the total cost of any completion. Prunes when the bound
//! exceeds the incumbent solution's cost.
//!
//! # Deferred extensions (Phase 2 design decisions)
//!
//! **`TieredCostEval` / lazy cost evaluation** — deferred because the
//! [`CostFiniteDomain::min_cost`] incremental `Cell` cache (landed in
//! Phase 2A) covers the immediate bottleneck: `optimistic_bound` was
//! calling `min_cost()` O(domain) per node, and the cache amortizes that
//! to O(1). A tiered evaluator (cheap proxy lower bound + expensive actual
//! cost with memoization) only pays its API cost when a second consumer
//! with a genuinely non-trivial cost function appears. The existing
//! [`DomainCostEval`] trait is the extension point — implement a new
//! evaluator type, no solver-core changes needed.
//!
//! **`solve_with_warm_start`** — deferred because no consumer currently
//! demands incremental re-solving (accepting a previous solution as the
//! initial incumbent for tighter B&B pruning from node zero). The
//! implementation is ~20 LOC: add a `warm_start: Option<&Solution>` field
//! to [`OptimizeConfig`] and seed `best_cost` / `best_solution` before
//! the first `bb_recurse` call. Land it when the first incremental
//! consumer (e.g., a playground animation scrubber) appears.
//!
//! **Unified `Constraint` trait** — the current `ConstraintEnum` dispatch
//! and separate `SoftConstraint` trait are adequate for the constraint
//! vocabulary in use. Collapsing them into a single trait with a default
//! `penalty() -> f64::INFINITY` method is a readability improvement, not a
//! performance one (the enum dispatch is already inlined by the compiler).
//! Land when the constraint vocabulary grows enough to justify the
//! refactor.
//!
//! **`tracing` instrumentation** — adding `#[instrument]` spans on
//! `branch_and_bound`, `propagate`, `ac3_from_variable` would enable
//! production diagnostics (measure propagation time, identify bottleneck
//! constraints). Deferred as polish — the `SolveStats` struct already
//! tracks `nodes_explored`, `backtracks`, `propagations`, and
//! `budget_exceeded`, which covers the coarse-grained profiling needs.

use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::ordering::{self, Ordering};
use crate::solver::ac3;
use crate::solver::backtrack::Solution;
use crate::solver::propagate;
use crate::solver::SearchContext;
use crate::variable::Variable;
use crate::Pruning;

/// Configuration for branch-and-bound optimization.
pub struct OptimizeConfig {
    pub pruning: Pruning,
    pub ordering: Ordering,
    pub max_solutions: usize,
    pub constraint_weights: Vec<f64>,
    pub var_constraint_ids: Vec<Vec<usize>>,
    /// If true, maximize cost instead of minimize.
    pub maximize: bool,
    /// Maximum number of search nodes before aborting early.
    /// See [`crate::SolveConfig::node_budget`].
    pub node_budget: Option<u64>,
}

/// Cost evaluator for domains. Passed into the optimizer so that the same
/// search code works for both `CostDomain` and plain `Domain` (zero cost).
pub trait DomainCostEval<D: Domain> {
    /// Cost of assigning `val` to the variable whose current domain is `domain`.
    fn cost(&self, domain: &D, val: &D::Value) -> f64;
    /// Lower bound on the minimum cost achievable from `domain`.
    fn min_cost(&self, domain: &D) -> f64;
    /// Upper bound on the maximum cost achievable from `domain`.
    fn max_cost(&self, domain: &D) -> f64;
}

/// No-op evaluator: all costs are zero. Used when D doesn't implement CostDomain.
pub struct ZeroCost;

impl<D: Domain> DomainCostEval<D> for ZeroCost {
    #[inline]
    fn cost(&self, _domain: &D, _val: &D::Value) -> f64 {
        0.0
    }
    #[inline]
    fn min_cost(&self, _domain: &D) -> f64 {
        0.0
    }
    #[inline]
    fn max_cost(&self, _domain: &D) -> f64 {
        0.0
    }
}

/// Evaluator that delegates to CostDomain methods.
pub struct CostDomainEval;

impl<D: crate::domain::CostDomain> DomainCostEval<D> for CostDomainEval {
    #[inline]
    fn cost(&self, domain: &D, val: &D::Value) -> f64 {
        domain.cost(val)
    }
    #[inline]
    fn min_cost(&self, domain: &D) -> f64 {
        domain.min_cost()
    }
    #[inline]
    fn max_cost(&self, domain: &D) -> f64 {
        domain
            .values()
            .into_iter()
            .map(|v| domain.cost(&v))
            .fold(f64::NEG_INFINITY, f64::max)
    }
}

/// A scored solution: the assignment together with its total cost.
struct ScoredSolution<D: Domain> {
    solution: Solution<D>,
    cost: f64,
}

/// Mutable accumulator for branch-and-bound: scored solutions found so far
/// and the best (effective) cost seen, threaded through the recursive search
/// to avoid per-function argument bloat.
struct BranchBoundState<D: Domain> {
    scored: Vec<ScoredSolution<D>>,
    best_cost: f64,
}

/// Run branch-and-bound search. Returns up to `max_solutions` solutions,
/// sorted by cost (best first according to the optimization direction).
pub fn branch_and_bound<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &crate::adjacency::Adjacency,
    config: &OptimizeConfig,
    stats: &mut crate::SolveStats,
    cost_eval: &dyn DomainCostEval<D>,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];
    let mut stack: Vec<VarId> = (0..num_vars as u32).collect();
    let mut ctx = SearchContext { variables, constraints, adjacency, stats };
    let mut bb = BranchBoundState {
        scored: Vec::new(),
        best_cost: f64::INFINITY,
    };

    bb_recurse(
        &mut ctx,
        config,
        cost_eval,
        &mut assignment,
        &mut stack,
        &mut bb,
        0,
    );

    // Sort by cost: best first (lowest for minimize, highest for maximize).
    if config.maximize {
        bb.scored.sort_by(|a, b| b.cost.partial_cmp(&a.cost).unwrap_or(std::cmp::Ordering::Equal));
    } else {
        bb.scored.sort_by(|a, b| a.cost.partial_cmp(&b.cost).unwrap_or(std::cmp::Ordering::Equal));
    }

    // Keep only the best `max_solutions`.
    bb.scored.truncate(config.max_solutions);

    bb.scored.into_iter().map(|s| s.solution).collect()
}

/// Compute the cost of a complete assignment.
fn assignment_cost<D: Domain>(
    assignment: &[Option<D::Value>],
    variables: &[Variable<D>],
    constraints: &[ConstraintEnum<D>],
    cost_eval: &dyn DomainCostEval<D>,
) -> f64
where
    D::Value: PartialEq,
{
    let mut cost = 0.0;

    // Domain costs.
    for (i, val) in assignment.iter().enumerate() {
        if let Some(v) = val {
            cost += cost_eval.cost(&variables[i].domain, v);
        }
    }

    // Soft constraint penalties.
    for c in constraints {
        cost += c.soft_penalty(assignment);
    }

    cost
}

/// Compute the optimistic bound on the cost of any completion.
///
/// For minimize: returns a lower bound (assigned vars use actual cost,
/// unassigned use min_cost).
/// For maximize: returns an upper bound (assigned vars use actual cost,
/// unassigned use max_cost). This is then negated by the caller to
/// compare against the negated incumbent.
fn optimistic_bound<D: Domain>(
    assignment: &[Option<D::Value>],
    variables: &[Variable<D>],
    constraints: &[ConstraintEnum<D>],
    cost_eval: &dyn DomainCostEval<D>,
    maximize: bool,
) -> f64
where
    D::Value: PartialEq,
{
    let mut bound = 0.0;

    for (i, val) in assignment.iter().enumerate() {
        match val {
            Some(v) => bound += cost_eval.cost(&variables[i].domain, v),
            None => {
                if maximize {
                    bound += cost_eval.max_cost(&variables[i].domain);
                } else {
                    bound += cost_eval.min_cost(&variables[i].domain);
                }
            }
        }
    }

    // Soft constraint penalties for fully-assigned scopes.
    // (Partially-assigned scopes contribute 0 optimistically.)
    for c in constraints {
        let scope = c.scope();
        if scope.iter().all(|&v| assignment[v as usize].is_some()) {
            bound += c.soft_penalty(assignment);
        }
    }

    bound
}

fn bb_recurse<D: Domain>(
    ctx: &mut SearchContext<'_, D>,
    config: &OptimizeConfig,
    cost_eval: &dyn DomainCostEval<D>,
    assignment: &mut Vec<Option<D::Value>>,
    stack: &mut Vec<VarId>,
    bb: &mut BranchBoundState<D>,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    // Complete assignment — record solution.
    if stack.is_empty() {
        let cost = assignment_cost(assignment, ctx.variables, ctx.constraints, cost_eval);
        let effective_cost = if config.maximize { -cost } else { cost };

        if effective_cost < bb.best_cost {
            bb.best_cost = effective_cost;
        }

        let sol: Solution<D> = assignment
            .iter()
            .map(|v| v.as_ref().unwrap().clone())
            .collect();
        bb.scored.push(ScoredSolution { solution: sol, cost });

        // For optimization, keep searching for better solutions.
        return false;
    }

    // Budget guard: abort early if the search has exceeded its node
    // budget. Return `true` so the recursion unwinds cleanly; whatever
    // scored solutions have been found so far remain in `scored` and
    // are surfaced at the end of `branch_and_bound`. `budget_exceeded`
    // is set so callers can distinguish best-so-far from optimal.
    // Checked before `nodes_explored += 1` so the post-budget node is
    // never counted and the flag is set exactly once per search.
    if let Some(budget) = config.node_budget
        && ctx.stats.nodes_explored >= budget
    {
        ctx.stats.budget_exceeded = true;
        return true;
    }

    ctx.stats.nodes_explored += 1;

    // Bound check: prune if the optimistic bound can't beat the incumbent.
    let ob = optimistic_bound(
        assignment, ctx.variables, ctx.constraints, cost_eval, config.maximize,
    );
    let effective_ob = if config.maximize { -ob } else { ob };
    if effective_ob >= bb.best_cost {
        return false;
    }

    let idx = ordering::select_variable(
        stack,
        ctx.variables,
        config.ordering,
        &config.constraint_weights,
        &config.var_constraint_ids,
    )
    .unwrap();

    let var = stack.swap_remove(idx);

    // Value ordering: sort by cost (lowest first for minimize, highest for maximize).
    let mut values: Vec<_> = ctx.variables[var as usize].domain.iter().collect();
    {
        let domain = &ctx.variables[var as usize].domain;
        if config.maximize {
            values.sort_by(|a, b| {
                let ca = cost_eval.cost(domain, b);
                let cb = cost_eval.cost(domain, a);
                ca.partial_cmp(&cb).unwrap_or(std::cmp::Ordering::Equal)
            });
        } else {
            values.sort_by(|a, b| {
                let ca = cost_eval.cost(domain, a);
                let cb = cost_eval.cost(domain, b);
                ca.partial_cmp(&cb).unwrap_or(std::cmp::Ordering::Equal)
            });
        }
    }

    for val in values {
        assignment[var as usize] = Some(val.clone());
        ctx.variables[var as usize].restrict_to(&val, depth);

        let mut valid = true;
        for &ci in ctx.adjacency.constraints_for(var) {
            let ci = ci as usize;
            let scope = ctx.constraints[ci].scope();
            if scope.iter().all(|&v| assignment[v as usize].is_some())
                && !ctx.constraints[ci].check(assignment)
            {
                valid = false;
                break;
            }
        }

        if valid {
            let dwo = match config.pruning {
                Pruning::None => false,
                Pruning::ForwardChecking => propagate::forward_check(
                    var,
                    ctx.variables,
                    ctx.constraints,
                    ctx.adjacency,
                    assignment.as_mut_slice(),
                    ctx.stats,
                    depth,
                ),
                Pruning::Ac3 => ac3::ac3_from_variable(
                    var, ctx.variables, ctx.constraints, ctx.adjacency, assignment, ctx.stats, depth,
                ),
                Pruning::AcFc => propagate::ac_fc(
                    var,
                    ctx.variables,
                    ctx.constraints,
                    ctx.adjacency,
                    assignment.as_mut_slice(),
                    ctx.stats,
                    depth,
                ),
            };

            if !dwo
                && bb_recurse(
                    ctx,
                    config,
                    cost_eval,
                    assignment,
                    stack,
                    bb,
                    depth + 1,
                )
            {
                return true;
            }
        }

        ctx.stats.backtracks += 1;
        assignment[var as usize] = None;
        for v in ctx.variables.iter_mut() {
            v.restore(depth);
        }
    }

    stack.push(var);
    false
}
