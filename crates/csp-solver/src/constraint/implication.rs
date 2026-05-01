//! Reified implication constraint: `antecedent = a → consequent ∈ allowed`.
//!
//! Encodes the gestalt rule "if variable X takes value `a`, then variable Y
//! must be drawn from the bitset `allowed`." Both variables share the same
//! `Domain::Value` type. Used by recognizer-tier CSPs to enforce things like
//! "if AltMode = TokenDispatch, all child engines must be one-pass-eligible."

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

/// `antecedent = trigger → consequent ∈ allowed`.
///
/// When `antecedent` is bound to `trigger`, prune all values from
/// `consequent` that are not in `allowed`. The antecedent is never
/// pruned by this constraint — only the consequent.
#[derive(Debug, Clone)]
pub struct ImplicationConstraint<V: Clone + PartialEq + std::fmt::Debug> {
    pub(crate) scope: [VarId; 2],
    pub(crate) trigger: V,
    pub(crate) allowed: Vec<V>,
}

impl<V: Clone + PartialEq + std::fmt::Debug> ImplicationConstraint<V> {
    /// Build a new implication constraint.
    ///
    /// `antecedent` is the variable whose binding triggers the constraint;
    /// `consequent` is the variable whose domain is filtered when triggered.
    pub fn new(antecedent: VarId, consequent: VarId, trigger: V, allowed: Vec<V>) -> Self {
        Self {
            scope: [antecedent, consequent],
            trigger,
            allowed,
        }
    }

    pub(crate) fn check_impl(&self, assignment: &[Option<V>]) -> bool {
        let ai = self.scope[0] as usize;
        let ci = self.scope[1] as usize;
        match (&assignment[ai], &assignment[ci]) {
            (Some(a), Some(c)) if *a == self.trigger => self.allowed.contains(c),
            _ => true,
        }
    }

    pub(crate) fn revise_impl<D: Domain<Value = V>>(
        &self,
        vars: &mut [Variable<D>],
        depth: usize,
    ) -> Revision {
        let ai = self.scope[0] as usize;
        let ci = self.scope[1] as usize;

        // Trigger only fires when the antecedent is bound to `trigger`.
        let antecedent_bound = vars[ai].domain.singleton_value();
        let triggered = matches!(antecedent_bound.as_ref(), Some(v) if *v == self.trigger);

        if !triggered {
            return Revision::Unchanged;
        }

        // Antecedent is the trigger value — filter the consequent.
        let mut changed = false;
        let consequent_values: Vec<V> = vars[ci].domain.iter().collect();
        for v in &consequent_values {
            if !self.allowed.contains(v)
                && vars[ci].prune(v, depth)
            {
                changed = true;
            }
        }

        if vars[ci].domain.is_empty() {
            return Revision::Unsatisfiable;
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

impl<D: Domain> Constraint<D> for ImplicationConstraint<D::Value>
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
