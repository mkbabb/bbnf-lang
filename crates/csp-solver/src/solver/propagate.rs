//! Forward checking and AC-FC hybrid propagation.
//!
//! On a domain wipe-out these return `Some(constraint_id)` naming the
//! constraint that emptied a domain — a "blame" signal. `None` means
//! propagation completed without a wipe-out. Every pruned neighbor is still
//! recorded on `trail` for O(touched) undo (the kernel's touched-VarId trail).

use crate::SolveStats;
use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::solver::Trail;
use crate::variable::Variable;

/// Outcome of a propagation step: `Some(ci)` on wipe-out (constraint `ci` is
/// to blame), `None` on success.
pub type PropResult = Option<usize>;

/// Forward checking using assign-check-unassign.
///
/// Reuses a value buffer across neighbors to avoid per-neighbor heap allocation.
/// Every pruned neighbor is recorded on `trail` so backtrack undoes only what
/// was touched. Returns the blame signal (see module docs).
#[allow(clippy::too_many_arguments)]
pub(crate) fn forward_check<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &mut [Option<D::Value>],
    stats: &mut SolveStats,
    trail: &mut Trail,
    depth: usize,
) -> PropResult
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

        // Blame the constraint that pruned the value which emptied the domain.
        let mut culprit: usize = 0;

        for val in &val_buf {
            assignment[neighbor as usize] = Some(val.clone());

            let mut failing: Option<usize> = None;
            for &ci in adjacency.constraints_for(neighbor) {
                let ci = ci as usize;
                let scope = constraints[ci].scope();
                if scope.iter().all(|&v| assignment[v as usize].is_some())
                    && !constraints[ci].check(assignment)
                {
                    failing = Some(ci);
                    break;
                }
            }

            assignment[neighbor as usize] = None;

            if let Some(ci) = failing {
                if variables[neighbor as usize].prune(val, depth) {
                    trail.push(neighbor);
                }
                culprit = ci;
                stats.propagations += 1;
            }
        }

        if variables[neighbor as usize].domain.is_empty() {
            return Some(culprit);
        }
    }

    None
}

/// AC-FC hybrid: forward check + singleton propagation.
#[allow(clippy::too_many_arguments)]
pub(crate) fn ac_fc<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &mut [Option<D::Value>],
    stats: &mut SolveStats,
    trail: &mut Trail,
    depth: usize,
) -> PropResult
where
    D::Value: PartialEq,
{
    if let Some(ci) = forward_check(
        var,
        variables,
        constraints,
        adjacency,
        assignment,
        stats,
        trail,
        depth,
    ) {
        return Some(ci);
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

        if let Some(ci) = forward_check(
            v,
            variables,
            constraints,
            adjacency,
            assignment,
            stats,
            trail,
            depth,
        ) {
            assignment[v as usize] = None;
            return Some(ci);
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

    None
}
