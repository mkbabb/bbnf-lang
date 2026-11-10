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
//! Uses propagation over `csp_solver::Csp<InlineDomain>` to resolve
//! all decisions in a single pass — the constraints are all unary
//! (each rule's decision is independent given the pre-computed
//! metrics), so propagation converges immediately.
//!
//! A structural fact the compiler must honour (identity preservation,
//! an aggregate payload, single-site inlining, an operator chain)
//! decides a rule's mode first; otherwise the author's directives
//! (`@token`, `@recover`, `@pretty`) do, then a cycle, then the cost
//! budget. The directives are always asserted together, on the rule's
//! own variable or, when a structural fact overrides them, on a
//! variable of their own: two that disagree join to ⊤, propagation
//! reports `Unsatisfiable`, and the plan fails with a
//! [`CallModeConflict`] naming both.

use std::collections::HashSet;
use std::fmt;

use crate::{GrammarIR, RuleId};
use csp_solver::constraint::VarId;
use csp_solver::{Csp, Unsatisfiable};

use super::budgets::CostBudgets;
use super::constraints::{CostBudgetConstraint, ForceCallMode, InlineDecision, InlineDomain};
use super::shape::gather_inline_shape_stats;
use super::visit::{
    compute_single_site_inline_with_ref_counts, count_refs_vec,
    estimate_expansion_cost_with_budgets, max_alt_branches,
};

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

/// One reason a rule's call mode is forced.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallModeForce {
    /// The rule must keep its own tape record (`preserve_identity`).
    PreserveIdentity,
    /// The rule owns an aggregate payload epilogue.
    AggregatePayload,
    /// A cyclic rule with one call site and no self-reference.
    SingleSiteInline,
    /// The rule is an operator chain.
    OperatorChain,
    /// `@token` on the rule.
    TokenDirective,
    /// The rule is cyclic.
    Cyclic,
    /// `@recover` on the rule.
    RecoverDirective,
    /// `@pretty` on the rule.
    PrettyDirective,
}

impl CallModeForce {
    /// The call mode this force requires.
    pub const fn mode(self) -> CallMode {
        match self {
            Self::SingleSiteInline | Self::TokenDirective => CallMode::InlineBody,
            Self::PreserveIdentity
            | Self::AggregatePayload
            | Self::OperatorChain
            | Self::Cyclic
            | Self::RecoverDirective
            | Self::PrettyDirective => CallMode::DirectCall,
        }
    }
}

impl fmt::Display for CallModeForce {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::PreserveIdentity => "identity preservation",
            Self::AggregatePayload => "its aggregate payload",
            Self::SingleSiteInline => "single-site inlining",
            Self::OperatorChain => "its operator chain",
            Self::TokenDirective => "@token",
            Self::Cyclic => "its cycle",
            Self::RecoverDirective => "@recover",
            Self::PrettyDirective => "@pretty",
        })
    }
}

/// The inline CSP is unsatisfiable: two forces require different call
/// modes for one rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallModeConflict {
    pub rule: RuleId,
    pub first: CallModeForce,
    pub second: CallModeForce,
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
/// no constraint decides default to [`CallMode::DirectCall`] — the
/// conservative choice. A rule forced to both modes is a
/// [`CallModeConflict`].
pub fn analyze_parse_inline_plan(
    ir: &GrammarIR,
    operator_chain_rules: &HashSet<RuleId>,
) -> Result<InlinePlan, CallModeConflict> {
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

    // Phase 4: Add constraints. Every variable records the rule it
    // belongs to and the forces asserted on it, so an unsatisfiable plan
    // can name the pair that disagrees.
    let mut var_forces: Vec<(usize, Vec<CallModeForce>)> =
        (0..ir.rules.len()).map(|idx| (idx, Vec::new())).collect();
    fn force(
        csp: &mut Csp<InlineDomain>,
        var_forces: &mut [(usize, Vec<CallModeForce>)],
        var: VarId,
        force: CallModeForce,
    ) {
        var_forces[var as usize].1.push(force);
        csp.add_constraint(ForceCallMode::new(var, force));
    }
    for (idx, rule) in ir.rules.iter().enumerate() {
        let var = var_ids[idx];

        // The author's directives. Two that disagree make the grammar
        // contradictory, whatever else holds for the rule.
        let directives = &rule.meta.directives;
        let authored: Vec<CallModeForce> = [
            directives.token.then_some(CallModeForce::TokenDirective),
            directives
                .recover
                .is_some()
                .then_some(CallModeForce::RecoverDirective),
            directives
                .pretty
                .is_some()
                .then_some(CallModeForce::PrettyDirective),
        ]
        .into_iter()
        .flatten()
        .collect();

        // Structural facts the compiler must honour; the first that
        // holds overrides the directives.
        let structural = if rule.meta.preserve_identity {
            // These rules must always produce their own tape record so
            // consumers (e.g. the bootstrap host.rs extraction layer)
            // can locate them by variant_idx. Never inline them.
            Some(CallModeForce::PreserveIdentity)
        } else if ir.payload_layouts.contains_key(&rule.id) {
            // AU.2.5: the aggregate epilogue (push_leaf_with +
            // PayloadData::Aggregate) commits the 16-byte buffer the
            // rule's body populates; an inlined body has no epilogue to
            // fire. `payload_layouts` is populated by
            // `compute_payload_layouts` before backend analysis runs.
            Some(CallModeForce::AggregatePayload)
        } else if single_site_inline[idx] {
            Some(CallModeForce::SingleSiteInline)
        } else if operator_chain_rules.contains(&rule.id) {
            Some(CallModeForce::OperatorChain)
        } else {
            None
        };

        if let Some(structural) = structural {
            force(&mut csp, &mut var_forces, var, structural);
            // The overridden directives are still checked against each
            // other, on a variable of their own.
            if !authored.is_empty() {
                let intent = csp.add_variable(InlineDomain::undecided());
                var_forces.push((idx, Vec::new()));
                for &directive in &authored {
                    force(&mut csp, &mut var_forces, intent, directive);
                }
            }
        } else if !authored.is_empty() {
            for &directive in &authored {
                force(&mut csp, &mut var_forces, var, directive);
            }
        } else if rule.meta.is_cyclic {
            force(&mut csp, &mut var_forces, var, CallModeForce::Cyclic);
        } else {
            // No force applies: the cost-budget constraint
            // (consolidates all heuristics) decides.
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
    }

    // Phase 5: Solve by propagation. `Unsatisfiable` comes only from a
    // `ForceCallMode` joining its variable's domain to ⊤.
    if let Err(Unsatisfiable) = csp.propagate() {
        let (idx, forces) = csp
            .variables
            .iter()
            .zip(&var_forces)
            .find(|(variable, _)| variable.domain.decision == InlineDecision::Conflict)
            .map(|(_, owner)| owner)
            .expect("propagation is unsatisfiable only when a domain reaches ⊤");
        let first = forces[0];
        let second = *forces
            .iter()
            .find(|force| force.mode() != first.mode())
            .expect("a domain reaches ⊤ only when two of its forces disagree");
        return Err(CallModeConflict {
            rule: ir.rules[*idx].id,
            first,
            second,
        });
    }

    // Phase 6: Extract results.
    let parse_call_modes = var_ids
        .iter()
        .map(|&var| match &csp.variables[var as usize].domain.decision {
            InlineDecision::Decided(mode) => *mode,
            // conservative default
            InlineDecision::Undecided => CallMode::DirectCall,
            InlineDecision::Conflict => {
                unreachable!("propagation returned Ok, so no rule's domain is ⊤")
            }
        })
        .collect();

    Ok(InlinePlan {
        parse_call_modes,
        single_site_inline,
    })
}
