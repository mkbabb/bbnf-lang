//! `AltConstraint` — Alt branch join (LUB).
//! `AltInVecConstraint` — Alt in Vec context with same-type fast path.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::helpers::{assign, join_types};

/// Alt constraint: join (least upper bound) of alternation branch types.
#[derive(Debug)]
pub struct AltConstraint {
    scope: Vec<VarId>,
    pub var: TypeVarId,
    pub branches: Vec<TypeVarId>,
}

impl AltConstraint {
    pub fn new(var: TypeVarId, branches: Vec<TypeVarId>) -> Self {
        let mut scope = vec![var];
        scope.extend_from_slice(&branches);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            var,
            branches,
        }
    }
}

impl Constraint<TypeDomain> for AltConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let branch_types: Vec<TypeDesc> = self
            .branches
            .iter()
            .filter_map(|&b| vars[b as usize].domain.solved.clone())
            .collect();

        if branch_types.len() != self.branches.len() {
            return Revision::Unchanged;
        }

        let result = join_types(&branch_types);
        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// AltInVec constraint: alternation in Vec context.
/// Uses in-vec projection for branch types; falls back to standard Alt
/// projection when branches are heterogeneous.
#[derive(Debug)]
pub struct AltInVecConstraint {
    scope: Vec<VarId>,
    pub var: TypeVarId,
    pub branches: Vec<TypeVarId>,
    pub standard_var: TypeVarId,
}

impl AltInVecConstraint {
    pub fn new(var: TypeVarId, branches: Vec<TypeVarId>, standard_var: TypeVarId) -> Self {
        let mut scope = vec![var, standard_var];
        scope.extend_from_slice(&branches);
        scope.sort_unstable();
        scope.dedup();
        Self {
            scope,
            var,
            branches,
            standard_var,
        }
    }
}

impl Constraint<TypeDomain> for AltInVecConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let branch_types: Vec<TypeDesc> = self
            .branches
            .iter()
            .filter_map(|&b| vars[b as usize].domain.solved.clone())
            .collect();

        if branch_types.len() != self.branches.len() {
            return Revision::Unchanged;
        }

        let first = &branch_types[0];
        let all_same = branch_types.iter().all(|t| t == first);

        if all_same {
            if assign(vars, self.var, first.clone()) {
                return Revision::Changed;
            }
        } else {
            // Heterogeneous -- fall back to standard Alt projection.
            if let Some(std_ty) = vars[self.standard_var as usize].domain.solved.clone() {
                if assign(vars, self.var, std_ty) {
                    return Revision::Changed;
                }
            }
            // Standard var not yet solved -- wait.
        }

        Revision::Unchanged
    }
}
