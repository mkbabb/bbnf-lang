//! Core constraint trait and supporting types.

use std::fmt::Debug;

use crate::domain::Domain;
use crate::variable::Variable;

/// Unique variable identifier (index into the variable array).
pub type VarId = u32;

/// Result of running `revise` on a constraint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Revision {
    Unchanged,
    Changed,
    Unsatisfiable,
}

/// A constraint over one or more CSP variables.
pub trait Constraint<D: Domain>: Debug {
    /// The variables this constraint involves (its "scope").
    fn scope(&self) -> &[VarId];

    /// Check whether a full or partial assignment satisfies this constraint.
    fn check(&self, assignment: &[Option<D::Value>]) -> bool;

    /// AC-3 style revision: prune unsupported values from domains.
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        let scope = self.scope();
        if scope.len() != 2 {
            return Revision::Unchanged;
        }

        let xi = scope[0] as usize;
        let xj = scope[1] as usize;
        let mut changed = false;

        let mut assignment: Vec<Option<D::Value>> = vec![None; vars.len()];

        let vals_i = vars[xi].domain.values();
        let vals_j = vars[xj].domain.values();

        for vi in &vals_i {
            let mut supported = false;
            assignment[xi] = Some(vi.clone());
            for vj in &vals_j {
                assignment[xj] = Some(vj.clone());
                if self.check(&assignment) {
                    supported = true;
                    break;
                }
            }
            if !supported {
                vars[xi].prune(vi, depth);
                changed = true;
            }
        }
        assignment[xi] = None;
        assignment[xj] = None;

        if vars[xi].domain.is_empty() {
            return Revision::Unsatisfiable;
        }

        let vals_j = vars[xj].domain.values();
        let vals_i = vars[xi].domain.values();

        for vj in &vals_j {
            let mut supported = false;
            assignment[xj] = Some(vj.clone());
            for vi in &vals_i {
                assignment[xi] = Some(vi.clone());
                if self.check(&assignment) {
                    supported = true;
                    break;
                }
            }
            if !supported {
                vars[xj].prune(vj, depth);
                changed = true;
            }
        }

        if vars[xj].domain.is_empty() {
            return Revision::Unsatisfiable;
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// A constraint with a penalty for violation.
///
/// Hard constraints prune domains; soft constraints contribute cost to the
/// objective when violated. During optimization, the solver accumulates
/// penalties from violated soft constraints alongside domain costs.
pub trait SoftConstraint<D: Domain>: Constraint<D> {
    /// Penalty added to the objective when this constraint is violated.
    fn penalty(&self) -> f64;
}
