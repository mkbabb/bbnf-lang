//! Generic closure-based soft constraint.
//!
//! Tests: `tests/optimize.rs`.

use crate::domain::Domain;

use super::lambda::CheckerFn;
use super::traits::{Constraint, SoftConstraint, VarId};

/// A soft constraint backed by a closure, analogous to `LambdaConstraint`.
///
/// When the checker returns `false`, the constraint is "violated" and its
/// `penalty` is added to the objective during optimization search.
pub struct SoftLambdaConstraint<D: Domain> {
    pub(crate) scope: Vec<VarId>,
    pub(crate) checker: CheckerFn<D>,
    pub(crate) penalty: f64,
    label: String,
}

impl<D: Domain> std::fmt::Debug for SoftLambdaConstraint<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SoftLambdaConstraint({}, {:?}, penalty={})",
            self.label, self.scope, self.penalty
        )
    }
}

impl<D: Domain> SoftLambdaConstraint<D> {
    pub fn new(
        scope: Vec<VarId>,
        checker: impl Fn(&[Option<D::Value>]) -> bool + Send + Sync + 'static,
        penalty: f64,
        label: impl Into<String>,
    ) -> Self {
        Self {
            scope,
            checker: Box::new(checker),
            penalty,
            label: label.into(),
        }
    }
}

impl<D: Domain> Constraint<D> for SoftLambdaConstraint<D> {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    /// Soft constraints always "pass" — they never prune domains.
    /// Their violation is tracked as cost, not as infeasibility.
    fn check(&self, _assignment: &[Option<D::Value>]) -> bool {
        true
    }
}

impl<D: Domain> SoftConstraint<D> for SoftLambdaConstraint<D> {
    fn penalty(&self) -> f64 {
        self.penalty
    }
}

impl<D: Domain> SoftLambdaConstraint<D> {
    /// Check whether the underlying predicate is satisfied.
    /// This is separate from `Constraint::check` (which always returns true
    /// so that soft constraints don't prune) — use this to compute penalty costs.
    pub fn is_satisfied(&self, assignment: &[Option<D::Value>]) -> bool {
        (self.checker)(assignment)
    }
}
