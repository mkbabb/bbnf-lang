//! Conflict-directed backjumping with bitset conflict tracking.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::ordering::{self, Ordering};
use crate::solver::ac3;
use crate::solver::propagate;
use crate::variable::Variable;
use crate::{Pruning, SolveStats};

use super::backtrack::Solution;

/// Run backjumping search. Returns up to `max_solutions` solutions.
pub fn backjump_search<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &BackjumpConfig,
    stats: &mut SolveStats,
) -> Vec<Solution<D>>
where
    D::Value: PartialEq,
{
    let num_vars = variables.len();
    let mut assignment: Vec<Option<D::Value>> = vec![None; num_vars];
    let mut stack: Vec<VarId> = (0..num_vars as u32).collect();
    let mut solutions = Vec::new();
    let mut assigned_order: Vec<VarId> = Vec::new();
    let mut conflict_membership = vec![false; num_vars];

    backjump_recurse(
        variables, constraints, adjacency, config, stats,
        &mut assignment, &mut stack, &mut solutions,
        &mut assigned_order, &mut conflict_membership, 0,
    );

    solutions
}

enum BackjumpResult {
    Continue,
    Done,
    JumpTo(usize),
}

/// Configuration for backjumping search.
pub struct BackjumpConfig {
    pub pruning: Pruning,
    pub ordering: Ordering,
    pub max_solutions: usize,
    pub constraint_weights: Vec<f64>,
    pub var_constraint_ids: Vec<Vec<usize>>,
    /// Maximum number of search nodes before aborting early.
    /// See [`crate::SolveConfig::node_budget`].
    pub node_budget: Option<u64>,
}

fn backjump_recurse<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    config: &BackjumpConfig,
    stats: &mut SolveStats,
    assignment: &mut Vec<Option<D::Value>>,
    stack: &mut Vec<VarId>,
    solutions: &mut Vec<Solution<D>>,
    assigned_order: &mut Vec<VarId>,
    conflict_membership: &mut [bool],
    depth: usize,
) -> BackjumpResult
where
    D::Value: PartialEq,
{
    if stack.is_empty() {
        let sol: Solution<D> = assignment
            .iter()
            .map(|v| v.as_ref().unwrap().clone())
            .collect();
        solutions.push(sol);
        if solutions.len() >= config.max_solutions {
            return BackjumpResult::Done;
        }
        return BackjumpResult::Continue;
    }

    // Budget guard — see `backtrack.rs::backtrack_recurse` for details.
    if let Some(budget) = config.node_budget {
        if stats.nodes_explored >= budget {
            stats.budget_exceeded = true;
            return BackjumpResult::Done;
        }
    }

    stats.nodes_explored += 1;

    let idx = ordering::select_variable(
        stack, variables, config.ordering,
        &config.constraint_weights, &config.var_constraint_ids,
    )
    .unwrap();

    let var = stack.swap_remove(idx);
    assigned_order.push(var);

    // Bitset-backed conflict set
    let mut conflict_vars: Vec<VarId> = Vec::new();

    let values: Vec<_> = variables[var as usize].domain.iter().collect();
    let mut exhausted = true;

    for val in values {
        assignment[var as usize] = Some(val.clone());

        // Restrict domain to singleton {val} for AC-3.
        variables[var as usize].restrict_to(&val, depth);

        let mut valid = true;
        for &ci in adjacency.constraints_for(var) {
            let ci = ci as usize;
            let scope = constraints[ci].scope();
            if scope.iter().all(|&v| assignment[v as usize].is_some()) {
                if !constraints[ci].check(assignment) {
                    valid = false;
                    for &v in scope {
                        if v != var
                            && assignment[v as usize].is_some()
                            && !conflict_membership[v as usize]
                        {
                            conflict_membership[v as usize] = true;
                            conflict_vars.push(v);
                        }
                    }
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

            if dwo {
                for &neighbor in adjacency.neighbors_of_var(var) {
                    if assignment[neighbor as usize].is_some()
                        && !conflict_membership[neighbor as usize]
                    {
                        conflict_membership[neighbor as usize] = true;
                        conflict_vars.push(neighbor);
                    }
                }
            } else {
                match backjump_recurse(
                    variables, constraints, adjacency, config, stats,
                    assignment, stack, solutions, assigned_order,
                    conflict_membership, depth + 1,
                ) {
                    BackjumpResult::Done => return BackjumpResult::Done,
                    BackjumpResult::Continue => {
                        exhausted = false;
                    }
                    BackjumpResult::JumpTo(target_depth) => {
                        stats.backtracks += 1;
                        assignment[var as usize] = None;
                        for v in variables.iter_mut() {
                            v.restore(depth);
                        }
                        assigned_order.pop();
                        stack.push(var);

                        if target_depth < depth {
                            for &cv in &conflict_vars {
                                conflict_membership[cv as usize] = false;
                            }
                            return BackjumpResult::JumpTo(target_depth);
                        }
                        continue;
                    }
                }
            }
        }

        stats.backtracks += 1;
        assignment[var as usize] = None;
        for v in variables.iter_mut() {
            v.restore(depth);
        }
    }

    assigned_order.pop();
    stack.push(var);

    let result = if exhausted && !conflict_vars.is_empty() {
        // Single-pass scan for most recent conflict variable
        let mut max_depth = 0;
        let mut found = false;
        for (pos, &av) in assigned_order.iter().enumerate() {
            if conflict_membership[av as usize] && pos >= max_depth {
                max_depth = pos;
                found = true;
            }
        }
        if found && max_depth < depth {
            BackjumpResult::JumpTo(max_depth)
        } else {
            BackjumpResult::Continue
        }
    } else {
        BackjumpResult::Continue
    };

    for &cv in &conflict_vars {
        conflict_membership[cv as usize] = false;
    }

    result
}
