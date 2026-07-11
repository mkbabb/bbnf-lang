//! Cost-evaluation vocabulary for branch-and-bound optimization.
//!
//! The branch-and-bound *search* now lives in the unified kernel
//! ([`crate::solver::search::branch_and_bound`]); this module owns only the
//! cost abstraction the optimizer plugs in — [`DomainCostEval`] and its two
//! implementors ([`ZeroCost`], [`CostDomainEval`]).

use crate::domain::Domain;

/// Cost evaluator for domains. Passed into the optimizer so the same search
/// code works for both `CostDomain` and plain `Domain` (zero cost).
pub trait DomainCostEval<D: Domain> {
    /// Cost of assigning `val` to the variable whose current domain is `domain`.
    fn cost(&self, domain: &D, val: &D::Value) -> f64;
    /// Lower bound on the minimum cost achievable from `domain`.
    fn min_cost(&self, domain: &D) -> f64;
    /// Upper bound on the maximum cost achievable from `domain`.
    fn max_cost(&self, domain: &D) -> f64;
}

/// No-op evaluator: all costs are zero. Used when `D` doesn't implement `CostDomain`.
pub struct ZeroCost;

impl<D: Domain> DomainCostEval<D> for ZeroCost {
    #[inline]
    fn cost(&self, _domain: &D, _val: &D::Value) -> f64 {
        0.0
    }
    #[inline]
    fn min_cost(&self, _domain: &D) -> f64 {
        0.0
    }
    #[inline]
    fn max_cost(&self, _domain: &D) -> f64 {
        0.0
    }
}

/// Evaluator that delegates to `CostDomain` methods.
pub struct CostDomainEval;

impl<D: crate::domain::CostDomain> DomainCostEval<D> for CostDomainEval {
    #[inline]
    fn cost(&self, domain: &D, val: &D::Value) -> f64 {
        domain.cost(val)
    }
    #[inline]
    fn min_cost(&self, domain: &D) -> f64 {
        domain.min_cost()
    }
    #[inline]
    fn max_cost(&self, domain: &D) -> f64 {
        domain
            .values()
            .into_iter()
            .map(|v| domain.cost(&v))
            .fold(f64::NEG_INFINITY, f64::max)
    }
}
