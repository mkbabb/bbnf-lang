//! Forward checking and AC-FC hybrid propagation.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::variable::Variable;
use crate::SolveStats;

/// Forward checking using assign-check-unassign.
///
/// Reuses a value buffer across neighbors to avoid per-neighbor heap allocation.
pub fn forward_check<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &mut [Option<D::Value>],
    stats: &mut SolveStats,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    let mut val_buf: Vec<D::Value> = Vec::new();

    for &neighbor in adjacency.neighbors_of_var(var) {
        if assignment[neighbor as usize].is_some() {
            continue;
        }

        // Reuse buffer: clear + extend avoids reallocation after first neighbor.
        val_buf.clear();
        val_buf.extend(variables[neighbor as usize].domain.iter());

        for val in &val_buf {
            assignment[neighbor as usize] = Some(val.clone());

            let mut valid = true;
            for &ci in adjacency.constraints_for(neighbor) {
                let ci = ci as usize;
                let scope = constraints[ci].scope();
                if scope.iter().all(|&v| assignment[v as usize].is_some()) {
                    if !constraints[ci].check(assignment) {
                        valid = false;
                        break;
                    }
                }
            }

            assignment[neighbor as usize] = None;

            if !valid {
                variables[neighbor as usize].prune(val, depth);
                stats.propagations += 1;
            }
        }

        if variables[neighbor as usize].domain.is_empty() {
            return true;
        }
    }

    false
}

/// AC-FC hybrid: forward check + singleton propagation.
pub fn ac_fc<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &mut [Option<D::Value>],
    stats: &mut SolveStats,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    if forward_check(var, variables, constraints, adjacency, assignment, stats, depth) {
        return true;
    }

    let mut worklist: Vec<VarId> = Vec::new();
    for &neighbor in adjacency.neighbors_of_var(var) {
        if assignment[neighbor as usize].is_none()
            && variables[neighbor as usize].domain.is_singleton()
        {
            worklist.push(neighbor);
        }
    }

    let mut visited = vec![false; variables.len()];
    visited[var as usize] = true;

    while let Some(v) = worklist.pop() {
        if visited[v as usize] {
            continue;
        }
        visited[v as usize] = true;

        let singleton_val = variables[v as usize].domain.singleton_value().unwrap();
        assignment[v as usize] = Some(singleton_val);

        if forward_check(v, variables, constraints, adjacency, assignment, stats, depth) {
            assignment[v as usize] = None;
            return true;
        }

        assignment[v as usize] = None;

        for &neighbor in adjacency.neighbors_of_var(v) {
            if assignment[neighbor as usize].is_none()
                && !visited[neighbor as usize]
                && variables[neighbor as usize].domain.is_singleton()
            {
                worklist.push(neighbor);
            }
        }
    }

    false
}
