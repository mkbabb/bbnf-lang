//! `GroundConstraint` — assigns a fixed type to a variable.
//! `EqualConstraint` — bidirectional type equality between two variables.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::helpers::assign;

/// Ground constraint: assigns a fixed type to a variable.
/// AC-3: singleton domain, immediately resolved.
#[derive(Debug)]
pub struct GroundConstraint {
    scope: [VarId; 1],
    pub var: TypeVarId,
    pub ty: TypeDesc,
}

impl GroundConstraint {
    pub fn new(var: TypeVarId, ty: TypeDesc) -> Self {
        Self {
            scope: [var],
            var,
            ty,
        }
    }
}

impl Constraint<TypeDomain> for GroundConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match &assignment[self.var as usize] {
            Some(Some(ty)) => *ty == self.ty,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if assign(vars, self.var, self.ty.clone()) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Equal constraint: two variables must have the same type.
/// Bidirectional: when either resolves, the other does too.
#[derive(Debug)]
pub struct EqualConstraint {
    scope: [VarId; 2],
    pub target: TypeVarId,
    pub source: TypeVarId,
}

impl EqualConstraint {
    pub fn new(target: TypeVarId, source: TypeVarId) -> Self {
        Self {
            scope: [target, source],
            target,
            source,
        }
    }
}

impl Constraint<TypeDomain> for EqualConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match (
            &assignment[self.target as usize],
            &assignment[self.source as usize],
        ) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        let source_ty = vars[self.source as usize].domain.solved.clone();
        let target_ty = vars[self.target as usize].domain.solved.clone();

        let mut changed = false;
        if let Some(ty) = source_ty {
            changed |= assign(vars, self.target, ty);
        } else if let Some(ty) = target_ty {
            changed |= assign(vars, self.source, ty);
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
