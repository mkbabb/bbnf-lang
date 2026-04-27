//! Operator constraints: `OptionalConstraint`, `RepeatConstraint`,
//! `ProjectConstraint` (Skip/Next/Minus), `MapConstraint` (return-type
//! override).

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::variable::Variable;

use crate::TypeDesc;

use super::TypeVarId;
use super::domain::TypeDomain;
use super::revise::assign;

/// Optional constraint: wraps inner type in `Option<T>`.
/// Optional Span collapses to Span. Transparent refs get Option<Enum>.
#[derive(Debug)]
pub struct OptionalConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub inner: TypeVarId,
    pub transparent_ref: bool,
}

impl OptionalConstraint {
    pub fn new(var: TypeVarId, inner: TypeVarId, transparent_ref: bool) -> Self {
        Self {
            scope: [var, inner],
            var,
            inner,
            transparent_ref,
        }
    }
}

impl Constraint<TypeDomain> for OptionalConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        // Tranche Y.10: peek the inner type by reference so the Span
        // and transparent_ref fast paths avoid the clone entirely.
        // Only the fallback branch that wraps the inner type in
        // `Option<Box<T>>` requires owned access.
        let result = {
            let inner_slot = &vars[self.inner as usize].domain.solved;
            match inner_slot {
                None => return Revision::Unchanged,
                Some(ty) if *ty == TypeDesc::Span => TypeDesc::Span,
                Some(_) if self.transparent_ref => {
                    TypeDesc::Option(Box::new(TypeDesc::Enum))
                }
                Some(ty) => TypeDesc::Option(Box::new(ty.clone())),
            }
        };
        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Repeat constraint: wraps inner type in `Vec<T>`.
/// Vec<Span> collapses to Span.
#[derive(Debug)]
pub struct RepeatConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub inner: TypeVarId,
}

impl RepeatConstraint {
    pub fn new(var: TypeVarId, inner: TypeVarId) -> Self {
        Self {
            scope: [var, inner],
            var,
            inner,
        }
    }
}

impl Constraint<TypeDomain> for RepeatConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        // Tranche Y.10: peek the inner type by reference. The Span
        // fast path avoids the clone; only the wrap-in-Vec path
        // needs owned access.
        let result = {
            let inner_slot = &vars[self.inner as usize].domain.solved;
            match inner_slot {
                None => return Revision::Unchanged,
                Some(ty) if *ty == TypeDesc::Span => TypeDesc::Span,
                Some(ty) => TypeDesc::Vec(Box::new(ty.clone())),
            }
        };
        if assign(vars, self.var, result) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Project constraint: keeps one side of Skip/Next/Minus.
#[derive(Debug)]
pub struct ProjectConstraint {
    scope: [VarId; 2],
    pub var: TypeVarId,
    pub kept: TypeVarId,
}

impl ProjectConstraint {
    pub fn new(var: TypeVarId, kept: TypeVarId) -> Self {
        Self {
            scope: [var, kept],
            var,
            kept,
        }
    }
}

impl Constraint<TypeDomain> for ProjectConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, _assignment: &[Option<Option<TypeDesc>>]) -> bool {
        true
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        // Tranche Y.10: projecting one variable's type onto another
        // still requires a clone because `assign` stores by value, but
        // we can skip the clone when the target already holds the
        // same type (assign() is a no-op in that case).
        let needs_assign = {
            let kept_slot = &vars[self.kept as usize].domain.solved;
            let var_slot = &vars[self.var as usize].domain.solved;
            match kept_slot {
                None => None,
                Some(ty) => {
                    if var_slot.as_ref() == Some(ty) {
                        Some(false)
                    } else {
                        Some(true)
                    }
                }
            }
        };

        match needs_assign {
            None => Revision::Unchanged,
            Some(false) => Revision::Unchanged,
            Some(true) => {
                let ty = vars[self.kept as usize].domain.solved.clone().unwrap();
                if assign(vars, self.var, ty) {
                    Revision::Changed
                } else {
                    Revision::Unchanged
                }
            }
        }
    }
}

/// Map constraint: overrides type from FnDescriptor return type.
#[derive(Debug)]
pub struct MapConstraint {
    scope: [VarId; 1],
    pub var: TypeVarId,
    pub return_type: TypeDesc,
}

impl MapConstraint {
    pub fn new(var: TypeVarId, return_type: TypeDesc) -> Self {
        Self {
            scope: [var],
            var,
            return_type,
        }
    }
}

impl Constraint<TypeDomain> for MapConstraint {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<Option<TypeDesc>>]) -> bool {
        match &assignment[self.var as usize] {
            Some(Some(ty)) => *ty == self.return_type,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<TypeDomain>], _depth: usize) -> Revision {
        if assign(vars, self.var, self.return_type.clone()) {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
