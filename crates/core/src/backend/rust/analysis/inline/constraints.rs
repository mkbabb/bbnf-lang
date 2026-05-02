//! Inline-CSP domain + constraints.
//!
//! The inline plan models per-rule call-mode selection as a
//! Constraint Satisfaction Problem with one variable per rule and a
//! mix of unary force-mode constraints and a cost-budget constraint
//! that consolidates every cost / shape heuristic into a single
//! revise step.
//!
//! Tranche AF.2: every numeric threshold the cost-budget constraint
//! consults is a projection of [`egraph::CostWeights`] via
//! [`super::budgets::CostBudgets`]. The shape-guard predicate
//! [`should_force_direct_call`] reads the same budgets so the inline
//! decision boundary stays in lockstep with the internal CSP
//! constraint.

use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;
use egraph::CostWeights;

use super::budgets::{CostBudgets, MAX_ALT_BRANCHES};
use super::plan::CallMode;

// ── Domain ────────────────────────────────────────────────────────────────────

/// Lattice value for the inline decision.
///
/// Bottom = `Undecided`. Monotonically resolves to either
/// `Decided(InlineBody)` or `Decided(DirectCall)`. Once assigned,
/// it does not change.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum InlineDecision {
    Undecided,
    Decided(CallMode),
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct InlineDomain {
    pub(super) decision: InlineDecision,
}

impl InlineDomain {
    pub(super) fn undecided() -> Self {
        Self {
            decision: InlineDecision::Undecided,
        }
    }
}

impl Domain for InlineDomain {
    type Value = InlineDecision;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.decision.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.decision == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.decision.clone()]
    }
}

impl LatticeDomain for InlineDomain {
    fn bottom() -> Self {
        Self::undecided()
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.decision, &other.decision) {
            (InlineDecision::Undecided, InlineDecision::Decided(_)) => {
                self.decision = other.decision.clone();
                true
            }
            _ => false,
        }
    }
}

// ── Constraints ───────────────────────────────────────────────────────────────

/// Unary constraint that forces a rule to a specific call mode.
#[derive(Debug)]
pub(super) struct ForceCallMode {
    var: VarId,
    mode: CallMode,
}

impl ForceCallMode {
    pub(super) fn new(var: VarId, mode: CallMode) -> Self {
        Self { var, mode }
    }
}

impl Constraint<InlineDomain> for ForceCallMode {
    fn scope(&self) -> &[VarId] {
        std::slice::from_ref(&self.var)
    }

    fn check(&self, assignment: &[Option<InlineDecision>]) -> bool {
        match &assignment[self.var as usize] {
            Some(InlineDecision::Decided(m)) => *m == self.mode,
            _ => true,
        }
    }

    fn revise(&self, vars: &mut [Variable<InlineDomain>], _depth: usize) -> Revision {
        let slot = &mut vars[self.var as usize].domain.decision;
        if *slot == InlineDecision::Undecided {
            *slot = InlineDecision::Decided(self.mode);
            Revision::Changed
        } else {
            Revision::Unchanged
        }
    }
}

/// Cost-budget constraint: decides InlineBody or DirectCall based on
/// the pre-computed expansion cost, ref count, alt branch count, and
/// shape stats.
///
/// Consolidates all the heuristic checks from the original imperative
/// code into a single constraint that fires once during propagation.
/// Every numeric threshold is derived from [`CostBudgets`] (Tranche
/// AF.2), which in turn reads [`CostWeights`] from
/// `ir.cost_config.egraph.weights`.
#[derive(Debug)]
pub(super) struct CostBudgetConstraint {
    pub(super) var: VarId,
    pub(super) local_cost: usize,
    pub(super) total_budget: usize,
    pub(super) max_alt_branches: usize,
    pub(super) shape_control_nodes: usize,
    pub(super) shape_refs: usize,
    pub(super) shape_wrapper_heavy: bool,
    pub(super) budgets: CostBudgets,
}

impl Constraint<InlineDomain> for CostBudgetConstraint {
    fn scope(&self) -> &[VarId] {
        std::slice::from_ref(&self.var)
    }

    fn check(&self, _assignment: &[Option<InlineDecision>]) -> bool {
        true // Always satisfiable — just decides which mode.
    }

    fn revise(&self, vars: &mut [Variable<InlineDomain>], _depth: usize) -> Revision {
        let slot = &mut vars[self.var as usize].domain.decision;
        if *slot != InlineDecision::Undecided {
            return Revision::Unchanged;
        }

        // Shape-based force-to-DirectCall (same logic as
        // `should_force_direct_call`).
        let b = &self.budgets;
        let force_direct = if self.shape_control_nodes == 0 {
            false
        } else if self.shape_refs > 2 && self.local_cost > b.high_ref_local_cost {
            true
        } else if self.shape_wrapper_heavy
            && self.shape_refs > 1
            && self.total_budget > b.wrapper_heavy_total_budget
        {
            true
        } else {
            self.shape_control_nodes > 2 && self.total_budget > b.control_heavy_total_budget
        };

        let mode = if force_direct {
            CallMode::DirectCall
        } else if self.max_alt_branches <= MAX_ALT_BRANCHES
            && self.local_cost <= b.max_local_cost
            && self.total_budget <= b.max_total_budget
        {
            CallMode::InlineBody
        } else {
            CallMode::DirectCall
        };

        *slot = InlineDecision::Decided(mode);
        Revision::Changed
    }
}

/// Shape-based direct-call force predicate. Reads its numeric
/// thresholds from [`CostWeights`] via [`CostBudgets::from_weights`],
/// keeping this function's decision boundary in lockstep with the
/// internal CSP constraint that consults the same budgets.
///
/// Tranche AF.2: the former hardcoded constants (48 / 1024 / 1536)
/// are replaced by `weights.call_overhead`-scaled budgets so every
/// call site reads the single source of truth on
/// `ir.cost_config.egraph.weights`.
pub fn should_force_direct_call(
    shape: crate::backend::rust::analysis::specialize::InlineShapeStats,
    local_cost: usize,
    total_budget: usize,
    weights: &CostWeights,
) -> bool {
    let budgets = CostBudgets::from_weights(weights);

    if shape.control_nodes() == 0 {
        return false;
    }

    if shape.refs > 2 && local_cost > budgets.high_ref_local_cost {
        return true;
    }

    if shape.is_wrapper_heavy()
        && shape.refs > 1
        && total_budget > budgets.wrapper_heavy_total_budget
    {
        return true;
    }

    shape.control_nodes() > 2 && total_budget > budgets.control_heavy_total_budget
}
