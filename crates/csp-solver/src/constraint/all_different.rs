//! N-ary all-different constraint.

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

#[derive(Debug)]
pub struct AllDifferent {
    pub(crate) scope: Vec<VarId>,
}

impl AllDifferent {
    pub fn new(vars: Vec<VarId>) -> Self {
        Self { scope: vars }
    }

    pub(crate) fn check_impl<V: PartialEq>(&self, assignment: &[Option<V>]) -> bool {
        let assigned: Vec<&V> = self
            .scope
            .iter()
            .filter_map(|&v| assignment[v as usize].as_ref())
            .collect();
        for i in 0..assigned.len() {
            for j in (i + 1)..assigned.len() {
                if assigned[i] == assigned[j] {
                    return false;
                }
            }
        }
        true
    }

    /// Singleton removal: prune assigned values from peers.
    ///
    /// GAC (Régin's algorithm) is available separately via
    /// `solver::gac_alldiff::propagate_gac_alldiff` for one-shot use.
    pub(crate) fn revise_impl<D: Domain>(&self, vars: &mut [Variable<D>], depth: usize) -> Revision
    where
        D::Value: PartialEq,
    {
        let mut changed = false;
        let singletons: Vec<(VarId, D::Value)> = self
            .scope
            .iter()
            .filter_map(|&v| {
                vars[v as usize]
                    .domain
                    .singleton_value()
                    .map(|val| (v, val))
            })
            .collect();

        for (sv, sval) in &singletons {
            for &other in &self.scope {
                if other == *sv {
                    continue;
                }
                if vars[other as usize].prune(sval, depth) {
                    changed = true;
                }
                if vars[other as usize].domain.is_empty() {
                    return Revision::Unsatisfiable;
                }
            }
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

impl<D: Domain> Constraint<D> for AllDifferent
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
