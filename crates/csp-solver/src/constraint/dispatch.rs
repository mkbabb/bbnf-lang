//! ConstraintEnum: devirtualized dispatch for hot-path constraints.

use crate::domain::Domain;
use crate::variable::Variable;

use super::all_different::AllDifferent;
use super::all_different_except::AllDifferentExcept;
use super::not_equal::NotEqual;
use super::soft::SoftLambdaConstraint;
use super::traits::{Constraint, Revision, VarId};

/// Enum-dispatched constraint storage. Avoids vtable indirection for built-in types.
///
/// There is deliberately no `Lambda` variant: every closure-based constraint
/// (`add_equals`, `add_less_than`, `add_greater_than`, and external
/// `LambdaConstraint`s) enters through [`Custom`](Self::Custom), which boxes
/// the `dyn Constraint`. A devirtualized `Lambda` arm carried zero
/// construction sites (Pass-1 R1) and was excised; W10 may re-introduce it
/// behind profiling if the boxed dispatch shows up hot.
pub enum ConstraintEnum<D: Domain> {
    NotEqual(NotEqual),
    AllDifferent(AllDifferent),
    AllDifferentExcept(AllDifferentExcept<D::Value>),
    Soft(SoftLambdaConstraint<D>),
    Custom(Box<dyn Constraint<D>>),
}

impl<D: Domain> std::fmt::Debug for ConstraintEnum<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotEqual(c) => c.fmt(f),
            Self::AllDifferent(c) => c.fmt(f),
            Self::AllDifferentExcept(c) => c.fmt(f),
            Self::Soft(c) => c.fmt(f),
            Self::Custom(c) => c.fmt(f),
        }
    }
}

impl<D: Domain> ConstraintEnum<D>
where
    D::Value: PartialEq,
{
    #[inline]
    pub fn scope(&self) -> &[VarId] {
        match self {
            Self::NotEqual(c) => &c.scope,
            Self::AllDifferent(c) => &c.scope,
            Self::AllDifferentExcept(c) => &c.scope,
            Self::Soft(c) => &c.scope,
            Self::Custom(c) => c.scope(),
        }
    }

    #[inline]
    pub fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        match self {
            Self::NotEqual(c) => c.check_impl(assignment),
            Self::AllDifferent(c) => c.check_impl(assignment),
            Self::AllDifferentExcept(c) => c.check_impl(assignment),
            Self::Soft(_) => true, // soft constraints never reject
            Self::Custom(c) => c.check(assignment),
        }
    }

    /// For soft constraints, compute the penalty if the underlying predicate
    /// is violated. Returns 0.0 for hard constraints or satisfied soft constraints.
    #[inline]
    pub fn soft_penalty(&self, assignment: &[Option<D::Value>]) -> f64 {
        match self {
            Self::Soft(c) => {
                if c.is_satisfied(assignment) {
                    0.0
                } else {
                    c.penalty
                }
            }
            _ => 0.0,
        }
    }
}

impl<D: Domain> ConstraintEnum<D>
where
    D::Value: PartialEq + 'static,
{
    #[inline]
    pub fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        match self {
            Self::NotEqual(c) => c.revise_impl(vars, depth),
            Self::AllDifferent(c) => c.revise_impl(vars, depth),
            Self::AllDifferentExcept(c) => c.revise_impl(vars, depth),
            Self::Soft(_) => Revision::Unchanged, // soft constraints don't prune
            Self::Custom(c) => c.revise(vars, depth),
        }
    }
}
