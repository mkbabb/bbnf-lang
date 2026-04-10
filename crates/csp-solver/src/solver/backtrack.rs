//! Chronological backtracking search.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::ordering::{self, Ordering};
use crate::solver::ac3;
use crate::solver::propagate;
use crate::variable::Variable;
use crate::{Pruning, SolveStats};

/// Solution type: a vector of values indexed by variable ID.
pub type Solution<D> = Vec<<D as Domain>::Value>;

/// Run backtracking search. Returns up to `max_solutions` solutions.
pub fn backtrack_search<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &BacktrackConfig,
    stats: &mut SolveStats,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];
    let mut stack: Vec<VarId> = (0..num_vars as u32).collect();
    let mut solutions = Vec::new();

    backtrack_recurse(
        variables, constraints, adjacency, config, stats,
        &mut assignment, &mut stack, &mut solutions, 0,
    );

    solutions
}

/// Run backtracking search with pre-assigned variables.
pub fn backtrack_search_with_given<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &BacktrackConfig,
    stats: &mut SolveStats,
    given: &[(VarId, D::Value)],
) -> Vec<Solution<D>>
where
    D::Value: PartialEq,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];

    for (var, val) in given {
        assignment[*var as usize] = Some(val.clone());
    }

    let mut stack: Vec<VarId> = (0..num_vars as u32)
        .filter(|v| assignment[*v as usize].is_none())
        .collect();

    let mut solutions = Vec::new();

    backtrack_recurse(
        variables, constraints, adjacency, config, stats,
        &mut assignment, &mut stack, &mut solutions, 0,
    );

    solutions
}

/// Configuration for backtracking search.
pub struct BacktrackConfig {
    pub pruning: Pruning,
    pub ordering: Ordering,
    pub max_solutions: usize,
    pub constraint_weights: Vec<f64>,
    pub var_constraint_ids: Vec<Vec<usize>>,
    /// Maximum number of search nodes before aborting early.
    /// See [`crate::SolveConfig::node_budget`].
    pub node_budget: Option<u64>,
}

fn backtrack_recurse<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &BacktrackConfig,
    stats: &mut SolveStats,
    assignment: &mut Vec<Option<D::Value>>,
    stack: &mut Vec<VarId>,
    solutions: &mut Vec<Solution<D>>,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    if stack.is_empty() {
        let sol: Solution<D> = assignment
            .iter()
            .map(|v| v.as_ref().unwrap().clone())
            .collect();
        solutions.push(sol);
        return solutions.len() >= config.max_solutions;
    }

    // Budget guard: abort early if the search has exceeded its node
    // budget. Return the `done` signal so the recursion unwinds
    // cleanly, preserving whatever solutions have been found so far.
    // Checked before `nodes_explored += 1` so the post-budget node is
    // never counted and the flag is set exactly once per search.
    if let Some(budget) = config.node_budget {
        if stats.nodes_explored >= budget {
            stats.budget_exceeded = true;
            return true;
        }
    }

    stats.nodes_explored += 1;

    let idx = ordering::select_variable(
        stack, variables, config.ordering,
        &config.constraint_weights, &config.var_constraint_ids,
    )
    .unwrap();

    let var = stack.swap_remove(idx);
    let values: Vec<_> = variables[var as usize].domain.iter().collect();

    for val in values {
        assignment[var as usize] = Some(val.clone());

        // Restrict domain to singleton {val} so AC-3 revise() sees it.
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
                    var, variables, constraints, adjacency,
                    assignment.as_mut_slice(), stats, depth,
                ),
                Pruning::Ac3 => ac3::ac3_from_variable(
                    var, variables, constraints, adjacency,
                    assignment, stats, depth,
                ),
                Pruning::AcFc => propagate::ac_fc(
                    var, variables, constraints, adjacency,
                    assignment.as_mut_slice(), stats, depth,
                ),
            };

            if !dwo {
                if backtrack_recurse(
                    variables, constraints, adjacency, config, stats,
                    assignment, stack, solutions, depth + 1,
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
