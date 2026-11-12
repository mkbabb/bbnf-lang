//! BoundedSumConstraint — linear knapsack (`Σ weight ≤ cap`) with a
//! lower-bound-relaxation propagator.
//!
//! This is the one net-new primitive the CSP-central scan-plan design
//! requires: a *hard, solve-time* generated-size budget coupling every
//! scan site. Per-site lowering choices (`Exact` vs `Prefilter`) each carry
//! a non-negative static-size weight (generated LOC); the constraint holds
//! their sum below `cap` (`generated_loc_budget`). Unlike a post-hoc
//! `xtask lint-loc`, the budget is a live constraint the branch-and-bound
//! search prunes against, so a plan that busts it is never emitted — the
//! budget turns GREEN by construction and the solver is free to force a
//! size-cheap lowering on a legal-but-unaffordable site to afford a
//! fast-but-fat kernel elsewhere.
//!
//! # Relaxation
//!
//! The `revise` lower bound is `Σ_k min_live_weight(k)` — the cheapest
//! completion of every site under its *current* live domain. Because
//! weights are non-negative and the per-site minimum is never itself
//! pruned (see below), the bound is monotone within a pass and tightens as
//! search assigns variables (an assigned site's domain is a singleton, so
//! its `min_live_weight` becomes its chosen weight). When the bound already
//! exceeds `cap` the branch is infeasible; otherwise any value whose weight
//! exceeds its residual slack `cap − (base − min_k)` cannot appear in any
//! feasible completion and is pruned.
//!
//! Tests: `tests/bounded_sum.rs`.

use crate::domain::Domain;
use crate::variable::Variable;

use super::traits::{Constraint, Revision, VarId};

/// Sentinel weight for a value absent from a site's weight table. Acts as a
/// hard fence (any completion using it busts every finite budget) without
/// overflowing `i64` sums — see `weight_at`.
const UNREGISTERED: i64 = i64::MAX / 4;

/// `Σ_k weight(k, x_k) ≤ cap`, a linear knapsack over `scope`.
///
/// `weights[k]` is the `(value, weight)` table for `scope[k]`; every weight
/// must be non-negative. Constructed via [`BoundedSumConstraint::new`] and
/// added to a `Csp` through the generic `add_constraint` (`Custom`) seam —
/// no enum-dispatch edit, because its scope is n-ary and it supplies its own
/// `revise`.
pub struct BoundedSumConstraint<D: Domain> {
    pub(crate) scope: Vec<VarId>,
    weights: Vec<Vec<(D::Value, i64)>>,
    cap: i64,
}

impl<D: Domain> BoundedSumConstraint<D> {
    /// Build the knapsack. `scope[k]` is priced by `weights[k]`.
    ///
    /// # Panics
    /// If `scope.len() != weights.len()`, or any weight is negative (the
    /// relaxation's monotonicity requires `weight ≥ 0`).
    pub fn new(scope: Vec<VarId>, weights: Vec<Vec<(D::Value, i64)>>, cap: i64) -> Self {
        assert_eq!(
            scope.len(),
            weights.len(),
            "BoundedSumConstraint::new: scope and weights length mismatch ({} vs {})",
            scope.len(),
            weights.len(),
        );
        assert!(
            weights.iter().flatten().all(|(_, w)| *w >= 0),
            "BoundedSumConstraint::new: weights must be non-negative",
        );
        Self { scope, weights, cap }
    }

    #[inline]
    fn weight_at(&self, k: usize, val: &D::Value) -> i64 {
        self.weights[k]
            .iter()
            .find(|(v, _)| v == val)
            .map(|(_, w)| *w)
            .unwrap_or(UNREGISTERED)
    }

    /// Minimum weight among the *live* values at scope position `k`.
    /// `None` iff that site's domain is empty (a wipe-out).
    fn min_live_weight(&self, k: usize, vars: &[Variable<D>]) -> Option<i64> {
        vars[self.scope[k] as usize]
            .domain
            .iter()
            .map(|v| self.weight_at(k, &v))
            .min()
    }
}

impl<D: Domain> std::fmt::Debug for BoundedSumConstraint<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BoundedSumConstraint")
            .field("scope", &self.scope)
            .field("cap", &self.cap)
            .finish()
    }
}

impl<D: Domain> Constraint<D> for BoundedSumConstraint<D> {
    fn scope(&self) -> &[VarId] {
        &self.scope
    }

    /// Sound feasibility test. Because weights are non-negative, the sum
    /// over the *assigned* positions is a lower bound on the true total, so
    /// rejecting when it already exceeds `cap` never rejects a feasible
    /// assignment. On a fully-assigned scope this is the exact budget test.
    fn check(&self, assignment: &[Option<D::Value>]) -> bool {
        let mut assigned_sum: i64 = 0;
        for (k, &vid) in self.scope.iter().enumerate() {
            if let Some(val) = &assignment[vid as usize] {
                assigned_sum = assigned_sum.saturating_add(self.weight_at(k, val));
            }
        }
        assigned_sum <= self.cap
    }

    /// Knapsack lower-bound relaxation: prune every value that cannot fit in
    /// any feasible completion, and report infeasibility when the cheapest
    /// completion already busts `cap`.
    fn revise(&self, vars: &mut [Variable<D>], depth: usize) -> Revision {
        let n = self.scope.len();
        let mut mins = Vec::with_capacity(n);
        let mut base: i64 = 0;
        for k in 0..n {
            match self.min_live_weight(k, vars) {
                Some(m) => {
                    mins.push(m);
                    base = base.saturating_add(m);
                }
                None => return Revision::Unsatisfiable,
            }
        }
        if base > self.cap {
            return Revision::Unsatisfiable;
        }

        let mut changed = false;
        for k in 0..n {
            // Residual budget for site k with every other site at its floor.
            // `residual_cap ≥ mins[k]` (since base ≤ cap), so the per-site
            // minimum-weight value always survives — the bound stays valid
            // for the rest of the pass.
            let residual_cap = self.cap.saturating_sub(base.saturating_sub(mins[k]));
            let vid = self.scope[k] as usize;
            let doomed: Vec<D::Value> = vars[vid]
                .domain
                .iter()
                .filter(|v| self.weight_at(k, v) > residual_cap)
                .collect();
            for v in &doomed {
                changed |= vars[vid].prune(v, depth);
            }
            if vars[vid].domain.is_empty() {
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
