//! Monotonic propagation for lattice domains.
//!
//! Specialized fixed-point loop that skips adjacency graph construction
//! and undo-log overhead. Domains only grow (join/union), so no backtracking
//! is needed — simple iteration until convergence.

use crate::constraint::{ConstraintEnum, Revision};
use crate::domain::Domain;
use crate::variable::Variable;
use crate::SolveStats;

/// Propagate constraints over lattice domains to a fixed point.
///
/// Unlike AC-3, this doesn't build an adjacency graph or maintain a worklist
/// with per-constraint membership tracking. Instead, it repeatedly sweeps all
/// constraints until none produce changes — appropriate when domains are
/// monotonic (only grow via join) and the constraint graph is small.
///
/// Returns `Err(())` if a constraint reports unsatisfiable (shouldn't happen
/// for well-formed lattice CSPs).
pub fn propagate_monotonic<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    stats: &mut SolveStats,
) -> Result<(), ()>
where
    D::Value: PartialEq,
{
    loop {
        let mut changed = false;
        for c in constraints {
            match c.revise(variables, 0) {
                Revision::Unchanged => {}
                Revision::Changed => {
                    changed = true;
                    stats.propagations += 1;
                }
                Revision::Unsatisfiable => return Err(()),
            }
        }
        if !changed {
            break;
        }
    }
    Ok(())
}

/// SCC-stratified propagation: process acyclic constraints in one pass,
/// iterate only on cyclic constraints.
///
/// `scc_order[i]` is the constraint index to process.
/// `scc_ids[constraint_idx]` is the SCC id for that constraint.
/// `cyclic_sccs` is the set of SCC ids that have >1 member (are cyclic).
///
/// Acyclic constraints (SCC size 1, no self-loop) are guaranteed to converge
/// in a single pass when processed in topological order. Only cyclic SCCs
/// need iterative fixed-point.
pub fn propagate_stratified<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    scc_order: &[usize],
    scc_ids: &[usize],
    cyclic_sccs: &[bool],
    stats: &mut SolveStats,
) -> Result<(), ()>
where
    D::Value: PartialEq,
{
    let mut i = 0;
    while i < scc_order.len() {
        let ci = scc_order[i];
        let scc_id = scc_ids[ci];

        if !cyclic_sccs[scc_id] {
            // Acyclic: single pass
            match constraints[ci].revise(variables, 0) {
                Revision::Unchanged | Revision::Changed => {
                    stats.propagations += 1;
                }
                Revision::Unsatisfiable => return Err(()),
            }
            i += 1;
        } else {
            // Cyclic SCC: collect all constraints in this SCC, iterate to fixed point
            let scc_start = i;
            while i < scc_order.len() && scc_ids[scc_order[i]] == scc_id {
                i += 1;
            }
            let scc_constraints = &scc_order[scc_start..i];

            loop {
                let mut changed = false;
                for &ci in scc_constraints {
                    match constraints[ci].revise(variables, 0) {
                        Revision::Unchanged => {}
                        Revision::Changed => {
                            changed = true;
                            stats.propagations += 1;
                        }
                        Revision::Unsatisfiable => return Err(()),
                    }
                }
                if !changed {
                    break;
                }
            }
        }
    }
    Ok(())
}
