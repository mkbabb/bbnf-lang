//! Inline plan: per-rule [`CallMode`] selection via CSP propagation.
//!
//! Models the inlining decision for each rule as a Constraint
//! Satisfaction Problem:
//!
//! - **Variables**: One per rule → domain `{InlineBody, DirectCall}`
//! - **Constraints**:
//!   - Forced DirectCall: preserve_identity, cyclic, recover,
//!     prettify, operator-chain rules
//!   - Forced InlineBody: single-site inline, @token rules
//!   - Cost budget: `cost(rule) * ref_count <= weight-derived total
//!     budget`
//!   - Alt branch limit: structural ceiling on alternation fan-out
//!   - Shape guard: heavy-wrapper / high-ref / high-control-flow →
//!     DirectCall
//!
//! Uses AC-3 propagation via `csp_solver::Csp<InlineDomain>` to resolve
//! all decisions in a single pass — the constraints are all unary
//! (each rule's decision is independent given the pre-computed
//! metrics), so propagation converges immediately.

use std::collections::HashSet;

use bbnf_ir::{GrammarIR, RuleId};
use csp_solver::Csp;
use csp_solver::constraint::VarId;

use super::budgets::CostBudgets;
use super::constraints::{CostBudgetConstraint, ForceCallMode, InlineDecision, InlineDomain};
use super::visit::{
    compute_single_site_inline_with_ref_counts, count_refs_vec,
    estimate_expansion_cost_with_budgets, max_alt_branches,
};
use crate::backend::rust::analysis::specialize::gather_inline_shape_stats;

/// Whether a rule's call site emits the body inline or threads through
/// the dispatch helper.
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

/// Per-rule inline plan: parallel vectors aligned to the
/// [`GrammarIR::rules`] order.
#[derive(Clone, Debug, Default)]
pub struct InlinePlan {
    pub parse_call_modes: Vec<CallMode>,
    pub single_site_inline: Vec<bool>,
}

/// Solve the inline plan for `ir` given the pre-computed
/// `operator_chain_rules` set.
///
/// Returns the per-rule [`CallMode`] vector (parallel to
/// `ir.rules`) plus the single-site-inline eligibility flags. Rules
/// the CSP fails to decide default to [`CallMode::DirectCall`] —
/// the conservative choice.
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
        // + PayloadData::Aggregate) commits the 16-byte buffer
        // populated by the rule's body. An inlined aggregate body has
        // no epilogue to fire, so any rule with a registered layout
        // must stay callable. `payload_layouts` is populated by
        // `compute_payload_layouts` before backend analysis runs (see
        // `BackendAnalysis::from_ir`), so the map is ready here.
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

        // Priority 5: Cost-budget constraint (consolidates all
        // heuristics).
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
                // conservative default
                InlineDecision::Undecided => CallMode::DirectCall,
            }
        })
        .collect();

    InlinePlan {
        parse_call_modes,
        single_site_inline,
    }
}
