//! Cardinality constraint: at-least / at-most / exactly N variables
//! take a particular value.
//!
//! `Cardinality { vars, value, lo, hi }` enforces that the number of
//! variables in `vars` taking exactly `value` lies in `[lo, hi]`.
//! Used by recognizer-tier CSPs to enforce hoisting thresholds:
//! "at least N peer sites must commit to `Hoist` for the shared helper
//! to be eligible."

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

#[derive(Debug, Clone)]
pub struct CardinalityConstraint<V: Clone + PartialEq + std::fmt::Debug> {
    pub(crate) scope: Vec<VarId>,
    pub(crate) value: V,
    pub(crate) lo: usize,
    pub(crate) hi: usize,
}

impl<V: Clone + PartialEq + std::fmt::Debug> CardinalityConstraint<V> {
    pub fn new(vars: Vec<VarId>, value: V, lo: usize, hi: usize) -> Self {
        debug_assert!(lo <= hi);
        Self {
            scope: vars,
            value,
            lo,
            hi,
        }
    }

    /// Convenience: at-least-`lo`.
    pub fn at_least(vars: Vec<VarId>, value: V, lo: usize) -> Self {
        let hi = vars.len();
        Self::new(vars, value, lo, hi)
    }

    /// Convenience: at-most-`hi`.
    pub fn at_most(vars: Vec<VarId>, value: V, hi: usize) -> Self {
        Self::new(vars, value, 0, hi)
    }
}

impl<D: Domain> Constraint<D> for CardinalityConstraint<D::Value>
where
    D::Value: PartialEq,
{
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        let mut taken = 0usize;
        let mut unbound = 0usize;
        for &v in &self.scope {
            match &assignment[v as usize] {
                Some(val) if *val == self.value => taken += 1,
                Some(_) => {}
                None => unbound += 1,
            }
        }
        // The full constraint can still be satisfied as long as
        // [taken, taken + unbound] intersects [lo, hi].
        let max = taken + unbound;
        max >= self.lo && taken <= self.hi
    }

    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        // Count how many vars are forced to take `value`, how many are
        // forced not to, and how many are still flexible.
        let mut forced_yes = 0usize;
        let mut flexible: Vec<VarId> = Vec::new();

        for &v in &self.scope {
            let dom = &vars[v as usize].domain;
            let contains_value = dom.contains(&self.value);
            let only_value = dom
                .singleton_value()
                .map(|sv| sv == self.value)
                .unwrap_or(false);
            if only_value {
                forced_yes += 1;
            } else if contains_value {
                flexible.push(v);
            }
            // else: domain doesn't contain `value` at all — neutral.
        }

        // Total possible takers = forced_yes + flexible.len()
        let max_takers = forced_yes + flexible.len();
        // Minimum forced takers
        let min_takers = forced_yes;

        if max_takers < self.lo || min_takers > self.hi {
            return Revision::Unsatisfiable;
        }

        let mut changed = false;

        // If forced_yes already at upper bound, prune `value` from
        // every flexible variable.
        if forced_yes == self.hi {
            for v in &flexible {
                if vars[*v as usize].prune(&self.value, depth) {
                    changed = true;
                }
                if vars[*v as usize].domain.is_empty() {
                    return Revision::Unsatisfiable;
                }
            }
        }

        // If we need more takers than the flexible set can provide,
        // every flexible variable must be `value`. Restrict each
        // flexible domain to the singleton.
        if min_takers + flexible.len() == self.lo && self.lo > min_takers {
            for v in &flexible {
                vars[*v as usize].restrict_to(&self.value, depth);
                if vars[*v as usize].domain.is_empty() {
                    return Revision::Unsatisfiable;
                }
                changed = true;
            }
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
