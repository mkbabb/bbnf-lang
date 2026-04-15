//! Inline plan analysis using CSP constraint propagation.
//!
//! Models the inlining decision for each rule as a Constraint Satisfaction
//! Problem:
//!
//! - **Variables**: One per rule → domain `{InlineBody, DirectCall}`
//! - **Constraints**:
//!   - Forced DirectCall: preserve_identity, cyclic, recover, prettify, operator-chain rules
//!   - Forced InlineBody: single-site inline, @token rules
//!   - Cost budget: `cost(rule) * ref_count <= weight-derived total budget`
//!   - Alt branch limit: structural ceiling on alternation fan-out
//!   - Shape guard: heavy-wrapper / high-ref / high-control-flow → DirectCall
//!
//! Uses AC-3 propagation via `csp_solver::Csp<InlineDomain>` to resolve all
//! decisions in a single pass — the constraints are all unary (each rule's
//! decision is independent given the pre-computed metrics), so propagation
//! converges immediately.
//!
//! # Tranche AF.2 — universal cost model
//!
//! Every numeric cost weight in this file now reads from
//! [`egraph::CostWeights`] (via `ir.cost_config.egraph.weights`). The
//! per-node structural-cost estimates in [`estimate_expansion_cost`]
//! and the CSP budget thresholds in [`CostBudgetConstraint`] are
//! derived via [`CostBudgets::from_weights`] — a single source of
//! truth for the backend-driver inline-vs-call decision. The default
//! [`CostWeights`] values reproduce the pre-AF.2 hardcoded constants
//! exactly, so this migration is behavior-preserving at the default
//! weight configuration.

use std::collections::HashSet;

use bbnf_ir::{GrammarIR, IrNode, RuleId};
use csp_solver::Csp;
use csp_solver::constraint::{Constraint, Revision, VarId};
use csp_solver::domain::{Domain, LatticeDomain};
use csp_solver::variable::Variable;
use egraph::CostWeights;

use super::specialize::gather_inline_shape_stats;

// ── Cost budgets (Tranche AF.2) ──────────────────────────────────────────────

/// Structural budget derived from the shared [`CostWeights`]. Every
/// threshold the inline CSP consults lives here — the numeric
/// constants that used to be hardcoded in [`CostBudgetConstraint`] and
/// [`should_force_direct_call`] are all projections of the same
/// `call_overhead` / `inline_body_size_penalty` dimensions that the
/// CSP strategy solver (`crates/ir/src/passes/csp_strategy/`) reads
/// for its own decisions.
///
/// At `CostWeights::default()` values these projections reproduce the
/// pre-AF.2 hardcoded constants (`MAX_LOCAL_COST=80`,
/// `MAX_TOTAL_BUDGET=4096`, shape-guard thresholds 48/1024/1536,
/// leaf/ref/alt/repeat per-node costs 2/8/5/10).
#[derive(Clone, Copy, Debug)]
pub(crate) struct CostBudgets {
    // Per-node expansion-cost estimates. The `estimate_expansion_cost`
    // walker turns these into a single `usize` body-size number that
    // the budget thresholds below compare against.
    pub leaf_cost: usize,
    pub ref_cost: usize,
    pub alt_branch_cost: usize,
    pub repeat_cost: usize,
    pub negate_cost: usize,

    // CSP inline-vs-call thresholds.
    pub max_local_cost: usize,
    pub max_total_budget: usize,

    // Shape-guard thresholds for `should_force_direct_call`.
    pub high_ref_local_cost: usize,
    pub wrapper_heavy_total_budget: usize,
    pub control_heavy_total_budget: usize,
}

impl CostBudgets {
    /// Derive every inline-CSP budget from the shared [`CostWeights`].
    ///
    /// The per-node estimates use `call_overhead` / `inline_body_size_penalty`
    /// as dimensional anchors:
    /// - Leaf / Map / OptionalWhitespace: `4 * inline_body_size_penalty`
    ///   (= 2 at default weights — the baseline "tiny structural node" cost).
    /// - `Ref`: `2 * call_overhead` (= 8 at defaults — a direct call
    ///   pays `call_overhead`; inlining a call site is two call-equivalents
    ///   worth of work under the pre-AF.2 cost model).
    /// - `Alt` per-branch: `call_overhead + 2 * inline_body_size_penalty`
    ///   (= 5 at defaults — one call-worth plus a small per-branch structural
    ///   surcharge).
    /// - `Repeat`: `2 * call_overhead + 4 * inline_body_size_penalty`
    ///   (= 10 at defaults — two calls plus loop overhead).
    /// - `Negate`: `0.5 * call_overhead + 2 * inline_body_size_penalty`
    ///   (= 3 at defaults — half a call plus a small structural surcharge).
    ///
    /// The CSP thresholds scale `call_overhead`:
    /// - `max_local_cost = call_overhead * 20` (= 80 — at most ~20
    ///   call-equivalents of inlined body per rule).
    /// - `max_total_budget = call_overhead * 1024` (= 4096 — at most
    ///   ~1024 call-equivalents summed across all inline sites).
    /// - Shape guards: `call_overhead * 12 / 256 / 384` (= 48 / 1024 /
    ///   1536 at defaults).
    pub fn from_weights(w: &CostWeights) -> Self {
        let call = w.call_overhead;
        let body = w.inline_body_size_penalty;

        // Per-node costs (f64 → usize conversion with round-to-nearest
        // via `as usize`, which truncates but is stable for the
        // non-negative weights the CostWeights contract guarantees).
        let leaf_cost = (body * 4.0) as usize;
        let ref_cost = (call * 2.0) as usize;
        let alt_branch_cost = (call + body * 2.0) as usize;
        let repeat_cost = (call * 2.0 + body * 4.0) as usize;
        let negate_cost = (call * 0.5 + body * 2.0) as usize;

        // CSP thresholds.
        let max_local_cost = (call * 20.0) as usize;
        let max_total_budget = (call * 1024.0) as usize;

        // Shape-guard thresholds.
        let high_ref_local_cost = (call * 12.0) as usize;
        let wrapper_heavy_total_budget = (call * 256.0) as usize;
        let control_heavy_total_budget = (call * 384.0) as usize;

        Self {
            leaf_cost,
            ref_cost,
            alt_branch_cost,
            repeat_cost,
            negate_cost,
            max_local_cost,
            max_total_budget,
            high_ref_local_cost,
            wrapper_heavy_total_budget,
            control_heavy_total_budget,
        }
    }
}

/// Structural ceiling on alternation fan-out. Not a cost weight — a
/// pragmatic gate on dispatch-table complexity that every backend
/// respects regardless of the cost model. Tracked here next to the
/// inline CSP so the two structural gates live together.
const MAX_ALT_BRANCHES: usize = 32;

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
/// into a single constraint that fires once during propagation. Every numeric
/// threshold is derived from [`CostBudgets`] (Tranche AF.2), which in turn
/// reads [`egraph::CostWeights`] from `ir.cost_config.egraph.weights`.
#[derive(Debug)]
struct CostBudgetConstraint {
    var: VarId,
    local_cost: usize,
    total_budget: usize,
    max_alt_branches: usize,
    shape_control_nodes: usize,
    shape_refs: usize,
    shape_wrapper_heavy: bool,
    budgets: CostBudgets,
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
    // Tranche AF.2: derive every structural cost budget from the
    // shared CostWeights. Reads from `ir.cost_config.egraph.weights` —
    // the same single source of truth the CSP strategy solver
    // (`crates/ir/src/passes/csp_strategy/`) and both e-graph
    // extractors consume.
    let budgets = CostBudgets::from_weights(&ir.cost_config.egraph.weights);

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

        // Priority 0: preserve_identity → forced DirectCall.
        // These rules must always produce their own tape record so
        // consumers (e.g. the bootstrap host.rs extraction layer) can
        // locate them by variant_idx. Never inline them.
        if rule.meta.preserve_identity {
            csp.add_constraint(ForceCallMode::new(var, CallMode::DirectCall));
            continue;
        }

        // AU.2.5: rules carrying an aggregate payload layout must own
        // their emission site — the aggregate epilogue (push_leaf_with
        // + PayloadData::Aggregate) commits the 16-byte buffer populated
        // by the rule's body. An inlined aggregate body has no epilogue
        // to fire, so any rule
        // with a registered layout must stay callable. `payload_layouts`
        // is populated by `compute_payload_layouts` before backend
        // analysis runs (see `BackendAnalysis::from_ir`), so the map is
        // ready here.
        if ir.payload_layouts.contains_key(&rule.id) {
            csp.add_constraint(ForceCallMode::new(var, CallMode::DirectCall));
            continue;
        }

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
        let local_cost = estimate_expansion_cost_with_budgets(&rule.body, &budgets);
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
            budgets,
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

// ── Re-exported helpers ───────────────────────────────────────────────────────

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
    shape: super::specialize::InlineShapeStats,
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

fn compute_single_site_inline_with_ref_counts(ir: &GrammarIR, ref_counts: &[u32]) -> Vec<bool> {
    ir.rules
        .iter()
        .enumerate()
        .map(|(i, rule)| {
            rule.meta.is_cyclic
                && rule.id != 0
                && ref_counts[i] == 1
                && !rule.meta.preserve_identity
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
///
/// Convenience wrapper that derives [`CostBudgets`] from
/// [`CostWeights::default`] — useful for standalone callers and
/// tests that don't have a `CostWeights` handy. Production code
/// should call `estimate_expansion_cost_with_budgets` with the
/// per-compile budgets from
/// `CostBudgets::from_weights(&ir.cost_config.egraph.weights)`.
pub fn estimate_expansion_cost(node: &IrNode) -> usize {
    let budgets = CostBudgets::from_weights(&CostWeights::default());
    estimate_expansion_cost_with_budgets(node, &budgets)
}

/// Estimate the code expansion cost of inlining a rule body, reading
/// per-node cost estimates from the supplied `CostBudgets`.
///
/// Tranche AF.2: the per-node costs (leaf / ref / alt branch /
/// repeat / negate) that used to be hardcoded here are now scalar
/// projections of [`CostWeights::call_overhead`] and
/// [`CostWeights::inline_body_size_penalty`]. See
/// [`CostBudgets::from_weights`] for the derivation.
pub(crate) fn estimate_expansion_cost_with_budgets(
    node: &IrNode,
    budgets: &CostBudgets,
) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => budgets.leaf_cost,
        IrNode::Ref(_) => budgets.ref_cost,
        IrNode::Seq(children) => {
            1 + children
                .iter()
                .map(|c| estimate_expansion_cost_with_budgets(c, budgets))
                .sum::<usize>()
        }
        IrNode::Alt(branches, _) => branches
            .iter()
            .map(|b| {
                budgets.alt_branch_cost + estimate_expansion_cost_with_budgets(&b.node, budgets)
            })
            .sum::<usize>(),
        IrNode::Repeat { inner, .. } => {
            budgets.repeat_cost + estimate_expansion_cost_with_budgets(inner, budgets)
        }
        IrNode::Map { inner, .. } => {
            budgets.leaf_cost + estimate_expansion_cost_with_budgets(inner, budgets)
        }
        IrNode::OptionalWhitespace(inner) => {
            budgets.leaf_cost + estimate_expansion_cost_with_budgets(inner, budgets)
        }
        IrNode::Negate(inner) => {
            budgets.negate_cost + estimate_expansion_cost_with_budgets(inner, budgets)
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            estimate_expansion_cost_with_budgets(a, budgets)
                + estimate_expansion_cost_with_budgets(b, budgets)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            estimate_expansion_cost_with_budgets(token, budgets)
                + arms
                    .iter()
                    .map(|a| {
                        budgets.negate_cost
                            + estimate_expansion_cost_with_budgets(&a.continuation, budgets)
                    })
                    .sum::<usize>()
                + estimate_expansion_cost_with_budgets(fallback, budgets)
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
