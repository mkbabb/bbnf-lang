//! Monotonic propagation for lattice domains.
//!
//! Specialized fixed-point loop that skips adjacency graph construction
//! and undo-log overhead. Domains only grow (join/union), so no backtracking
//! is needed — simple iteration until convergence.

use crate::constraint::{ConstraintEnum, Revision};
use crate::domain::Domain;
use crate::variable::Variable;
use crate::{SolveStats, Unsatisfiable};

/// Propagate constraints over lattice domains to a fixed point.
///
/// Unlike AC-3, this doesn't build an adjacency graph or maintain a worklist
/// with per-constraint membership tracking. Instead, it repeatedly sweeps all
/// constraints until none produce changes — appropriate when domains are
/// monotonic (only grow via join) and the constraint graph is small.
///
/// Returns `Err(Unsatisfiable)` if a constraint reports unsatisfiable
/// (shouldn't happen for well-formed lattice CSPs).
pub fn propagate_monotonic<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    stats: &mut SolveStats,
) -> Result<(), Unsatisfiable>
where
    D::Value: PartialEq + 'static,
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
                Revision::Unsatisfiable => return Err(Unsatisfiable),
            }
        }
        if !changed {
            break;
        }
    }
    Ok(())
}
