//! Generic closure-based constraint.

use crate::domain::Domain;

use super::traits::{Constraint, VarId};

pub struct LambdaConstraint<D: Domain> {
    pub(crate) scope: Vec<VarId>,
    pub(crate) checker: Box<dyn Fn(&[Option<D::Value>]) -> bool>,
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
        checker: impl Fn(&[Option<D::Value>]) -> bool + 'static,
        label: impl Into<String>,
    ) -> Self {
        Self { scope, checker: Box::new(checker), label: label.into() }
    }
}

impl<D: Domain> Constraint<D> for LambdaConstraint<D> {
    fn scope(&self) -> &[VarId] { &self.scope }
    fn check(&self, assignment: &[Option<D::Value>]) -> bool { (self.checker)(assignment) }
}
