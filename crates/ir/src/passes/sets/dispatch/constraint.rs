//! `DisjointConstraint` — the per-Alt CSP constraint that records the
//! pairwise-disjoint check result and assigns the corresponding
//! `Dispatchable` / `NonDispatchable` decision via AC-3.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use super::domain::{DispatchDecision, DispatchDomain};

/// Constraint that resolves an Alt's dispatch eligibility from its branch FIRST
/// sets. Checks pairwise disjointness and immediately assigns the result.
#[derive(Debug)]
pub struct DisjointConstraint {
    var: VarId,
    dispatchable: bool,
}

impl DisjointConstraint {
    pub fn new(var: VarId, dispatchable: bool) -> Self {
        Self { var, dispatchable }
    }
}

impl Constraint<DispatchDomain> for DisjointConstraint {
    fn scope(&self) -> &[VarId] {
        std::slice::from_ref(&self.var)
    }

    fn check(&self, assignment: &[Option<DispatchDecision>]) -> bool {
        match &assignment[self.var as usize] {
            Some(d) => {
                if self.dispatchable {
                    *d == DispatchDecision::Dispatchable
                } else {
                    *d == DispatchDecision::NonDispatchable
                }
            }
            None => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<DispatchDomain>], _depth: usize) -> Revision {
        let target = if self.dispatchable {
            DispatchDecision::Dispatchable
        } else {
            DispatchDecision::NonDispatchable
        };
        let slot = &mut vars[self.var as usize].domain.decision;
        if *slot == DispatchDecision::Unknown {
            *slot = target;
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
