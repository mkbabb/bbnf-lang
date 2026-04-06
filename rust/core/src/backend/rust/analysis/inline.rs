//! Inline plan analysis using CSP constraint propagation.
//!
//! Models the inlining decision for each rule as a Constraint Satisfaction
//! Problem:
//!
//! - **Variables**: One per rule → domain `{InlineBody, DirectCall}`
//! - **Constraints**:
//!   - Forced DirectCall: cyclic, recover, prettify, operator-chain rules
//!   - Forced InlineBody: single-site inline, @token rules
//!   - Cost budget: `cost(rule) * ref_count <= MAX_TOTAL_BUDGET`
//!   - Alt branch limit: `max_alt_branches <= MAX_ALT_BRANCHES`
//!   - Shape guard: heavy-wrapper / high-ref / high-control-flow → DirectCall
//!
//! Uses AC-3 propagation via `csp_solver::Csp<InlineDomain>` to resolve all
//! decisions in a single pass — the constraints are all unary (each rule's
//! decision is independent given the pre-computed metrics), so propagation
//! converges immediately.

use std::collections::HashSet;

use bbnf_ir::{GrammarIR, IrNode, RuleId};
use csp_solver::Csp;
use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;

use super::specialize::gather_inline_shape_stats;

// ── Domain ────────────────────────────────────────────────────────────────────

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum CallMode {
    #[default]
    DirectCall,
    InlineBody,
}

impl CallMode {
    pub const fn is_inline(self) -> bool {
        matches!(self, Self::InlineBody)
    }
}

/// Lattice domain for inline decisions.
///
/// Bottom = `Undecided`. Monotonically resolves to either `Decided(InlineBody)`
/// or `Decided(DirectCall)`. Once assigned, it does not change.
#[derive(Clone, Debug, PartialEq)]
enum InlineDecision {
    Undecided,
    Decided(CallMode),
}

#[derive(Clone, Debug, PartialEq)]
struct InlineDomain {
    decision: InlineDecision,
}

impl InlineDomain {
    fn undecided() -> Self {
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
struct ForceCallMode {
    var: VarId,
    mode: CallMode,
}

impl ForceCallMode {
    fn new(var: VarId, mode: CallMode) -> Self {
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

/// Cost-budget constraint: decides InlineBody or DirectCall based on the
/// pre-computed expansion cost, ref count, alt branch count, and shape stats.
///
/// This consolidates all the heuristic checks from the original imperative code
/// into a single constraint that fires once during propagation.
#[derive(Debug)]
struct CostBudgetConstraint {
    var: VarId,
    local_cost: usize,
    total_budget: usize,
    max_alt_branches: usize,
    shape_control_nodes: usize,
    shape_refs: usize,
    shape_wrapper_heavy: bool,
}

impl CostBudgetConstraint {
    const MAX_ALT_BRANCHES: usize = 32;
    const MAX_LOCAL_COST: usize = 80;
    const MAX_TOTAL_BUDGET: usize = 4096;
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

        // Shape-based force-to-DirectCall (same logic as `should_force_direct_call`).
        let force_direct = if self.shape_control_nodes == 0 {
            false
        } else if self.shape_refs > 2 && self.local_cost > 48 {
            true
        } else if self.shape_wrapper_heavy && self.shape_refs > 1 && self.total_budget > 1024 {
            true
        } else {
            self.shape_control_nodes > 2 && self.total_budget > 1536
        };

        let mode = if force_direct {
            CallMode::DirectCall
        } else if self.max_alt_branches <= Self::MAX_ALT_BRANCHES
            && self.local_cost <= Self::MAX_LOCAL_COST
            && self.total_budget <= Self::MAX_TOTAL_BUDGET
        {
            CallMode::InlineBody
        } else {
            CallMode::DirectCall
        };

        *slot = InlineDecision::Decided(mode);
        Revision::Changed
    }
}

// ── Public API ────────────────────────────────────────────────────────────────

#[derive(Clone, Debug, Default)]
pub struct InlinePlan {
    pub parse_call_modes: Vec<CallMode>,
    pub single_site_inline: Vec<bool>,
}

pub fn analyze_parse_inline_plan(
    ir: &GrammarIR,
    operator_chain_rules: &HashSet<RuleId>,
) -> InlinePlan {
    // Phase 1: Compute ref counts.
    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    // Phase 2: Compute single-site inline eligibility.
    let single_site_inline = compute_single_site_inline_with_ref_counts(ir, &ref_counts);

    // Phase 3: Build CSP — one variable per rule.
    let mut csp: Csp<InlineDomain> = Csp::new();
    let var_ids: Vec<VarId> = ir
        .rules
        .iter()
        .map(|_| csp.add_variable(InlineDomain::undecided()))
        .collect();

    // Phase 4: Add constraints.
    for (idx, rule) in ir.rules.iter().enumerate() {
        let var = var_ids[idx];

        // Priority 1: Single-site inline → forced InlineBody.
        if single_site_inline[idx] {
            csp.add_constraint(ForceCallMode::new(var, CallMode::InlineBody));
            continue;
        }

        // Priority 2: Operator chain → forced DirectCall.
        if operator_chain_rules.contains(&rule.id) {
            csp.add_constraint(ForceCallMode::new(var, CallMode::DirectCall));
            continue;
        }

        // Priority 3: @token → forced InlineBody.
        if rule.meta.directives.token {
            csp.add_constraint(ForceCallMode::new(var, CallMode::InlineBody));
            continue;
        }

        // Priority 4: Cyclic / recover / prettify → forced DirectCall.
        if rule.meta.is_cyclic
            || rule.meta.directives.recover.is_some()
            || rule.meta.directives.pretty.is_some()
        {
            csp.add_constraint(ForceCallMode::new(var, CallMode::DirectCall));
            continue;
        }

        // Priority 5: Cost-budget constraint (consolidates all heuristics).
        let alt_branches = max_alt_branches(&rule.body);
        let local_cost = estimate_expansion_cost(&rule.body);
        let total_budget = local_cost.saturating_mul(ref_counts[idx] as usize);
        let shape = gather_inline_shape_stats(&rule.body);

        csp.add_constraint(CostBudgetConstraint {
            var,
            local_cost,
            total_budget,
            max_alt_branches: alt_branches,
            shape_control_nodes: shape.control_nodes(),
            shape_refs: shape.refs,
            shape_wrapper_heavy: shape.is_wrapper_heavy(),
        });
    }

    // Phase 5: Solve via AC-3 propagation.
    let _ = csp.propagate();

    // Phase 6: Extract results.
    let parse_call_modes = var_ids
        .iter()
        .map(|&var| {
            match &csp.variables[var as usize].domain.decision {
                InlineDecision::Decided(mode) => *mode,
                InlineDecision::Undecided => CallMode::DirectCall, // conservative default
            }
        })
        .collect();

    InlinePlan {
        parse_call_modes,
        single_site_inline,
    }
}

// ── Re-exported helpers (unchanged from original) ─────────────────────────────

pub fn should_force_direct_call(
    shape: super::specialize::InlineShapeStats,
    local_cost: usize,
    total_budget: usize,
) -> bool {
    if shape.control_nodes() == 0 {
        return false;
    }

    if shape.refs > 2 && local_cost > 48 {
        return true;
    }

    if shape.is_wrapper_heavy() && shape.refs > 1 && total_budget > 1024 {
        return true;
    }

    shape.control_nodes() > 2 && total_budget > 1536
}

fn compute_single_site_inline_with_ref_counts(ir: &GrammarIR, ref_counts: &[u32]) -> Vec<bool> {
    ir.rules
        .iter()
        .enumerate()
        .map(|(i, rule)| {
            rule.meta.is_cyclic
                && rule.id != 0
                && ref_counts[i] == 1
                && !body_has_self_ref(&rule.body, rule.id)
                && rule.meta.directives.recover.is_none()
                && rule.meta.directives.pretty.is_none()
        })
        .collect()
}

/// Maximum Alt branch count in a tree (for fusion eligibility gating).
/// Returns the largest number of branches in any Alt node.
pub fn max_alt_branches(node: &IrNode) -> usize {
    match node {
        IrNode::Alt(branches, _) => {
            let inner_max = branches
                .iter()
                .map(|b| max_alt_branches(&b.node))
                .max()
                .unwrap_or(0);
            branches.len().max(inner_max)
        }
        IrNode::Seq(children) => children.iter().map(max_alt_branches).max().unwrap_or(0),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => max_alt_branches(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            max_alt_branches(a).max(max_alt_branches(b))
        }
        _ => 0,
    }
}

/// Estimate the code expansion cost of inlining a rule body.
pub fn estimate_expansion_cost(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => 2,
        IrNode::Ref(_) => 8,
        IrNode::Seq(children) => 1 + children.iter().map(estimate_expansion_cost).sum::<usize>(),
        IrNode::Alt(branches, _) => branches
            .iter()
            .map(|b| 5 + estimate_expansion_cost(&b.node))
            .sum::<usize>(),
        IrNode::Repeat { inner, .. } => 10 + estimate_expansion_cost(inner),
        IrNode::Map { inner, .. } => 2 + estimate_expansion_cost(inner),
        IrNode::OptionalWhitespace(inner) => 2 + estimate_expansion_cost(inner),
        IrNode::Negate(inner) => 3 + estimate_expansion_cost(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            estimate_expansion_cost(a) + estimate_expansion_cost(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            estimate_expansion_cost(token)
                + arms
                    .iter()
                    .map(|a| 3 + estimate_expansion_cost(&a.continuation))
                    .sum::<usize>()
                + estimate_expansion_cost(fallback)
        }
    }
}

pub fn count_refs_vec(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(c) = counts.get_mut(*id as usize) {
                *c += 1;
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                count_refs_vec(c, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                count_refs_vec(&b.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs_vec(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs_vec(a, counts);
            count_refs_vec(b, counts);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_refs_vec(token, counts);
            for arm in arms {
                count_refs_vec(&arm.continuation, counts);
            }
            count_refs_vec(fallback, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

fn body_has_self_ref(node: &IrNode, rule_id: RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children.iter().any(|c| body_has_self_ref(c, rule_id)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| body_has_self_ref(&b.node, rule_id)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => body_has_self_ref(inner, rule_id),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_has_self_ref(a, rule_id) || body_has_self_ref(b, rule_id)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            body_has_self_ref(token, rule_id)
                || arms
                    .iter()
                    .any(|a| body_has_self_ref(&a.continuation, rule_id))
                || body_has_self_ref(fallback, rule_id)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}
