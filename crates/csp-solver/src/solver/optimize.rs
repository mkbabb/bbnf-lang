//! Branch-and-bound optimization search.
//!
//! Extends backtracking with cost tracking: at each search node, computes a
//! lower bound on the total cost of any completion. Prunes when the bound
//! exceeds the incumbent solution's cost.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::ordering::{self, Ordering};
use crate::solver::ac3;
use crate::solver::backtrack::Solution;
use crate::solver::propagate;
use crate::variable::Variable;
use crate::{Pruning, SolveStats};

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

/// Run branch-and-bound search. Returns up to `max_solutions` solutions,
/// sorted by cost (best first according to the optimization direction).
pub fn branch_and_bound<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &OptimizeConfig,
    stats: &mut SolveStats,
    cost_eval: &dyn DomainCostEval<D>,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];
    let mut stack: Vec<VarId> = (0..num_vars as u32).collect();
    let mut scored: Vec<ScoredSolution<D>> = Vec::new();
    let mut best_cost = f64::INFINITY;

    // Pre-collect indices of soft constraints so the hot-path cost
    // functions (`optimistic_bound`, `assignment_cost`) only iterate
    // the soft subset instead of scanning all N constraints. On real
    // grammars the soft fraction is <10% of the total constraint
    // count, turning an O(N_constraints) per-node scan into O(N_soft).
    let soft_indices: Vec<usize> = constraints
        .iter()
        .enumerate()
        .filter_map(|(i, c)| if c.is_soft() { Some(i) } else { None })
        .collect();

    // Incremental bound: start with the optimistic bound of the empty
    // assignment (every variable contributes its min/max cost). The
    // recursive search maintains this incrementally as variables are
    // assigned/unassigned, avoiding the O(N_vars) full recomputation
    // at every node.
    let initial_bound = if config.maximize {
        variables
            .iter()
            .map(|v| cost_eval.max_cost(&v.domain))
            .sum::<f64>()
    } else {
        variables
            .iter()
            .map(|v| cost_eval.min_cost(&v.domain))
            .sum::<f64>()
    };

    bb_recurse(
        variables,
        constraints,
        adjacency,
        config,
        stats,
        cost_eval,
        &mut assignment,
        &mut stack,
        &mut scored,
        &mut best_cost,
        &soft_indices,
        initial_bound,
        0,
    );

    // Sort by cost: best first (lowest for minimize, highest for maximize).
    if config.maximize {
        scored.sort_by(|a, b| {
            b.cost
                .partial_cmp(&a.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    } else {
        scored.sort_by(|a, b| {
            a.cost
                .partial_cmp(&b.cost)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    }

    // Keep only the best `max_solutions`.
    scored.truncate(config.max_solutions);

    scored.into_iter().map(|s| s.solution).collect()
}

/// Compute the cost of a complete assignment.
fn assignment_cost<D: Domain>(
    assignment: &[Option<D::Value>],
    variables: &[Variable<D>],
    constraints: &[ConstraintEnum<D>],
    soft_indices: &[usize],
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

    // Soft constraint penalties — only iterate the pre-indexed soft
    // subset. Hard constraints always return 0.0 for soft_penalty,
    // so skipping them is correct and avoids an O(N_constraints) scan.
    for &idx in soft_indices {
        cost += constraints[idx].soft_penalty(assignment);
    }

    cost
}

fn bb_recurse<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &OptimizeConfig,
    stats: &mut SolveStats,
    cost_eval: &dyn DomainCostEval<D>,
    assignment: &mut Vec<Option<D::Value>>,
    stack: &mut Vec<VarId>,
    scored: &mut Vec<ScoredSolution<D>>,
    best_cost: &mut f64,
    soft_indices: &[usize],
    domain_bound: f64,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    // Complete assignment — record solution.
    if stack.is_empty() {
        let cost = assignment_cost(assignment, variables, constraints, soft_indices, cost_eval);
        let effective_cost = if config.maximize { -cost } else { cost };

        if effective_cost < *best_cost {
            *best_cost = effective_cost;
        }

        let sol: Solution<D> = assignment
            .iter()
            .map(|v| v.as_ref().unwrap().clone())
            .collect();
        scored.push(ScoredSolution {
            solution: sol,
            cost,
        });

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
    if let Some(budget) = config.node_budget {
        if stats.nodes_explored >= budget {
            stats.budget_exceeded = true;
            return true;
        }
    }

    stats.nodes_explored += 1;

    // Bound check: use the incrementally-maintained domain bound
    // plus soft constraint penalties. The domain_bound tracks the
    // sum of (actual cost for assigned vars, min/max cost for
    // unassigned vars) without recomputing from scratch.
    let soft_penalty: f64 = soft_indices
        .iter()
        .filter_map(|&idx| {
            let c = &constraints[idx];
            let scope = c.scope();
            if scope.iter().all(|&v| assignment[v as usize].is_some()) {
                Some(c.soft_penalty(assignment))
            } else {
                None
            }
        })
        .sum();
    let ob = domain_bound + soft_penalty;
    let effective_ob = if config.maximize { -ob } else { ob };
    if effective_ob >= *best_cost {
        return false;
    }

    let idx = ordering::select_variable(
        stack,
        variables,
        config.ordering,
        &config.constraint_weights,
        &config.var_constraint_ids,
    )
    .unwrap();

    let var = stack.swap_remove(idx);

    // The optimistic contribution of this variable before assignment.
    let var_optimistic = if config.maximize {
        cost_eval.max_cost(&variables[var as usize].domain)
    } else {
        cost_eval.min_cost(&variables[var as usize].domain)
    };

    // Value ordering: sort by cost (lowest first for minimize, highest for maximize).
    let mut values: Vec<_> = variables[var as usize].domain.iter().collect();
    {
        let domain = &variables[var as usize].domain;
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
        // Update the incremental bound: replace the optimistic
        // contribution of this variable with the actual cost.
        let actual_cost = cost_eval.cost(&variables[var as usize].domain, &val);
        let new_bound = domain_bound - var_optimistic + actual_cost;

        assignment[var as usize] = Some(val.clone());
        variables[var as usize].restrict_to(&val, depth);

        let mut valid = true;
        for &ci in adjacency.constraints_for(var) {
            let ci = ci as usize;
            let scope = constraints[ci].scope();
            if scope.iter().all(|&v| assignment[v as usize].is_some()) {
                if !constraints[ci].check(assignment) {
                    valid = false;
                    break;
                }
            }
        }

        if valid {
            let dwo = match config.pruning {
                Pruning::None => false,
                Pruning::ForwardChecking => propagate::forward_check(
                    var,
                    variables,
                    constraints,
                    adjacency,
                    assignment.as_mut_slice(),
                    stats,
                    depth,
                ),
                Pruning::Ac3 => ac3::ac3_from_variable(
                    var,
                    variables,
                    constraints,
                    adjacency,
                    assignment,
                    stats,
                    depth,
                ),
                Pruning::AcFc => propagate::ac_fc(
                    var,
                    variables,
                    constraints,
                    adjacency,
                    assignment.as_mut_slice(),
                    stats,
                    depth,
                ),
            };

            if !dwo {
                if bb_recurse(
                    variables,
                    constraints,
                    adjacency,
                    config,
                    stats,
                    cost_eval,
                    assignment,
                    stack,
                    scored,
                    best_cost,
                    soft_indices,
                    new_bound,
                    depth + 1,
                ) {
                    return true;
                }
            }
        }

        stats.backtracks += 1;
        assignment[var as usize] = None;
        for v in variables.iter_mut() {
            v.restore(depth);
        }
    }

    stack.push(var);
    false
}
