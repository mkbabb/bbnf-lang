//! Generic closure-based constraint.
//!
//! Tests: `tests/optimize.rs`.

use crate::domain::Domain;

use super::traits::{Constraint, VarId};

/// Boxed predicate over a partial assignment.
///
/// `Send + Sync` — required so `Csp<D>` can be moved into a
/// `pyo3::Python::allow_threads` closure (see `constraint::traits::Constraint`).
pub(crate) type CheckerFn<D> = Box<dyn Fn(&[Option<<D as Domain>::Value>]) -> bool + Send + Sync>;

pub struct LambdaConstraint<D: Domain> {
    pub(crate) scope: Vec<VarId>,
    pub(crate) checker: CheckerFn<D>,
    label: String,
}

impl<D: Domain> std::fmt::Debug for LambdaConstraint<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LambdaConstraint({}, {:?})", self.label, self.scope)
    }
}

impl<D: Domain> LambdaConstraint<D> {
    pub fn new(
        scope: Vec<VarId>,
        checker: impl Fn(&[Option<D::Value>]) -> bool + Send + Sync + 'static,
        label: impl Into<String>,
    ) -> Self {
        Self {
            scope,
            checker: Box::new(checker),
            label: label.into(),
        }
    }
}

impl<D: Domain> Constraint<D> for LambdaConstraint<D> {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }
    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        (self.checker)(assignment)
    }
}
