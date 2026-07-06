//! Binary not-equal constraint: `x != y`.
//!
//! Tests: `tests/solver.rs`.

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

#[derive(Debug)]
pub struct NotEqual {
    pub(crate) scope: [VarId; 2],
}

impl NotEqual {
    pub fn new(x: VarId, y: VarId) -> Self {
        Self { scope: [x, y] }
    }

    pub(crate) fn check_impl<V: PartialEq>(&self, assignment: &[Option<V>]) -> bool {
        let xi = self.scope[0] as usize;
        let xj = self.scope[1] as usize;
        match (&assignment[xi], &assignment[xj]) {
            (Some(a), Some(b)) => a != b,
            _ => true,
        }
    }

    pub(crate) fn revise_impl<D: Domain>(&self, vars: &mut [Variable<D>], depth: usize) -> Revision
    where
        D::Value: PartialEq,
    {
        let xi = self.scope[0] as usize;
        let xj = self.scope[1] as usize;
        let mut changed = false;

        if let Some(v) = vars[xi].domain.singleton_value()
            && vars[xj].prune(&v, depth)
        {
            changed = true;
        }
        if let Some(v) = vars[xj].domain.singleton_value()
            && vars[xi].prune(&v, depth)
        {
            changed = true;
        }

        if vars[xi].domain.is_empty() || vars[xj].domain.is_empty() {
            return Revision::Unsatisfiable;
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

impl<D: Domain> Constraint<D> for NotEqual
where
    D::Value: PartialEq,
{
    fn scope(&self) -> &[VarId] {
        &self.scope
    }
    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        self.check_impl(assignment)
    }
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        self.revise_impl(vars, depth)
    }
}
