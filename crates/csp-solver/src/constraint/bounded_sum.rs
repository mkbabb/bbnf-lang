//! Bounded-sum (knapsack) constraint: `Σ weight(var_i = value) ≤ capacity`.
//!
//! The existing constraint zoo carries `all_different`, `not_equal`,
//! `implication`, `cardinality`, and `lambda` — every one of them is a
//! *relational* predicate over discrete values. None of them can express a
//! **linear resource bound**: the sum of a per-value weight across a set of
//! variables must not exceed a fixed capacity. That is the one primitive a
//! *global* generated-size budget needs — one scarce pool (`generated_loc`)
//! shared across N independent scan-site decisions, where each site's chosen
//! shape spends a different number of lines.
//!
//! # Model
//!
//! Each variable in `scope` carries a per-value weight table
//! `weights[pos] : value → static_size_cost`. The constraint is satisfied by
//! an assignment iff `Σ_pos weight(pos, assigned_value) ≤ capacity`.
//!
//! # Propagation (the knapsack lower bound)
//!
//! `revise` performs bound-consistency filtering. Let the **floor** be the
//! minimum sum still reachable from the current domains:
//!
//! ```text
//! floor = Σ_pos  min over live domain of  weight(pos, ·)      // Σ min-cost residuals
//! ```
//!
//! For an unassigned variable `p` and a candidate `val`, replacing `p`'s
//! residual contribution with `weight(p, val)` yields the tightest lower bound
//! achievable if `p := val`:
//!
//! ```text
//! floor_if(p := val) = floor − min_residual(p) + weight(p, val)
//! ```
//!
//! Any `val` with `floor_if(p := val) > capacity` cannot participate in a
//! feasible completion and is pruned. When a domain wipes out the constraint
//! is `Unsatisfiable`. Weights are `≥ 0`, so the floor is monotone under domain
//! shrinkage and the filtering is sound.

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

/// `Σ weight(var_i = value) ≤ capacity` over `scope`.
///
/// `weights[i]` is the value→weight table for `scope[i]`; `min_weight[i]` is
/// the minimum weight over that table (the residual floor used by `check` for
/// still-unassigned variables). Weights must be non-negative.
#[derive(Debug, Clone)]
pub struct BoundedSumConstraint<V: Clone + PartialEq + std::fmt::Debug> {
    pub(crate) scope: Vec<VarId>,
    pub(crate) weights: Vec<Vec<(V, f64)>>,
    pub(crate) min_weight: Vec<f64>,
    pub(crate) capacity: f64,
}

impl<V: Clone + PartialEq + std::fmt::Debug> BoundedSumConstraint<V> {
    /// Build a bounded-sum constraint.
    ///
    /// `scope` and `weights` are parallel: `weights[i]` prices every legal
    /// value of `scope[i]`. `capacity` is the hard upper bound on the summed
    /// weight of any satisfying assignment.
    ///
    /// # Panics
    ///
    /// Panics if `scope.len() != weights.len()`, if any weight table is empty,
    /// or if any weight is negative (the floor argument requires `w ≥ 0`).
    pub fn new(scope: Vec<VarId>, weights: Vec<Vec<(V, f64)>>, capacity: f64) -> Self {
        assert_eq!(
            scope.len(),
            weights.len(),
            "BoundedSumConstraint: scope and weights length mismatch ({} vs {})",
            scope.len(),
            weights.len(),
        );
        let min_weight = weights
            .iter()
            .map(|table| {
                assert!(!table.is_empty(), "BoundedSumConstraint: empty weight table");
                table.iter().fold(f64::INFINITY, |acc, (_, w)| {
                    assert!(*w >= 0.0, "BoundedSumConstraint: negative weight {w}");
                    acc.min(*w)
                })
            })
            .collect();
        Self {
            scope,
            weights,
            min_weight,
            capacity,
        }
    }

    /// Weight of `val` at scope position `pos`. Unregistered values fence to
    /// `+∞` so an out-of-contract value is rejected rather than counted as free.
    fn weight_at(&self, pos: usize, val: &V) -> f64 {
        self.weights[pos]
            .iter()
            .find(|(v, _)| v == val)
            .map(|(_, w)| *w)
            .unwrap_or(f64::INFINITY)
    }

    /// Minimum weight over the *current* live domain at scope position `pos`.
    fn live_min<D>(&self, var: &Variable<D>, pos: usize) -> f64
    where
        D: Domain<Value = V>,
    {
        var.domain
            .values()
            .iter()
            .map(|v| self.weight_at(pos, v))
            .fold(f64::INFINITY, f64::min)
    }
}

impl<D: Domain> Constraint<D> for BoundedSumConstraint<D::Value>
where
    D::Value: PartialEq,
{
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    /// Feasibility of a partial assignment: assigned weights plus the static
    /// per-variable minimum for the unassigned tail must not exceed capacity.
    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        let mut floor = 0.0;
        for (pos, &v) in self.scope.iter().enumerate() {
            floor += match &assignment[v as usize] {
                Some(val) => self.weight_at(pos, val),
                None => self.min_weight[pos],
            };
        }
        floor <= self.capacity
    }

    /// Knapsack bound-consistency: prune every value whose selection would
    /// force the summed weight over `capacity`.
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        // Residual floor per scope position over the current live domains.
        let residual: Vec<f64> = self
            .scope
            .iter()
            .enumerate()
            .map(|(pos, &v)| self.live_min(&vars[v as usize], pos))
            .collect();
        let floor: f64 = residual.iter().sum();

        let mut changed = false;
        for (pos, &v) in self.scope.iter().enumerate() {
            // Budget available to this variable once every *other* variable
            // spends at least its residual minimum.
            let slack = self.capacity - (floor - residual[pos]);
            let doomed: Vec<D::Value> = vars[v as usize]
                .domain
                .values()
                .into_iter()
                .filter(|val| self.weight_at(pos, val) > slack)
                .collect();
            for val in &doomed {
                if vars[v as usize].prune(val, depth) {
                    changed = true;
                }
            }
            if vars[v as usize].domain.is_empty() {
                return Revision::Unsatisfiable;
            }
        }

        if changed {
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}
