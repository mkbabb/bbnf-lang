//! `ParentCompatibility` — cross-rule tier ordering constraint
//! on every `Ref(target)` edge in the call graph.
//!
//! # Semantics
//!
//! The [`EmissionTier`] lattice is totally ordered
//! `Tape > Lazy > Direct` (per
//! [`EmissionTier::rank`](crate::passes::materialization::EmissionTier::rank)).
//! A parent rule's tier commitment bounds its children's tiers
//! from above:
//!
//! - A `Tape` parent may call any child (`Tape`, `Lazy`, or
//!   `Direct`). A `Tape` parent calling a `Direct` child pays
//!   a `cross_module_coercion` cost at the call-site boundary,
//!   because the parent's tape record needs a typed `T` value
//!   wrapped up into a synthetic tape push.
//!
//! - A `Lazy` parent may call `Lazy` or `Direct` children. A
//!   `Lazy` parent calling a `Direct` child likewise pays the
//!   coercion cost — the view-layer slot materializes a
//!   typed-value fast path from the child's direct return.
//!
//! - A `Direct` parent may call only `Direct` children.
//!   A `Direct` parent calling a `Tape` or `Lazy` child is
//!   illegal without an upstream tier upgrade pass — the
//!   parent's typed return type cannot reconstruct the child's
//!   tape record without walking it, and walking the child
//!   defeats the purpose of Tier B. The constraint's `check`
//!   prunes this pair so branch-and-bound rejects the
//!   assignment outright.
//!
//! The rule is:
//!
//! ```text
//!     parent.tier.rank() >= child.tier.rank()
//! ```
//!
//! encoded as a hard [`LambdaConstraint`] per `Ref` edge, and
//! augmented with a [`SoftLambdaConstraint`] that charges
//! `CostWeights::cross_module_coercion` whenever the tiers
//! differ (i.e., `rank > rank`). Both constraints share the
//! same pair scope `(parent_var, child_var)`.
//!
//! # Why `Ref` edges only
//!
//! A rule's materialized call graph is captured by its
//! `IrNode::Ref(target)` occurrences, which is exactly what
//! `compute_rule_deps` computes and what the AF.3 component
//! decomposition partitions. Intra-rule constructs (Seq, Alt,
//! Repeat) don't cross rule boundaries and don't install
//! per-call-site tiers — they inherit their parent rule's
//! tier. The constraint is strictly cross-rule.
//!
//! # AF.6 read-through
//!
//! The constraint closes the `emit_call` code path in the
//! AF.6 backend emitter. The backend reads
//! `ir.emission_tier[parent]` and `ir.emission_tier[child]`;
//! when they differ the emitter wraps the coerced call in a
//! synthetic tape push at the boundary. The CSP-level cost
//! weight makes that boundary visible during search, so the
//! solver picks a parent/child pair whose coercion cost is
//! justified by the per-rule tier rewards.

use csp_solver::constraint::{LambdaConstraint, SoftLambdaConstraint};
use csp_solver::Csp;

use super::ConstraintCtx;
use crate::passes::csp_strategy::{StrategyDomain, StrategyValue};
use crate::passes::materialization::EmissionTier;
use crate::{GrammarIR, IrNode, RuleId};

/// Install the `ParentCompatibility` constraint on every
/// `Ref`-edge pair within this component.
///
/// Walks each rule in `ctx.component`, scans its body for
/// `IrNode::Ref(target)` edges, and — for every (parent,
/// target) pair where **both** endpoints are in the component
/// and carry tier variables — installs two constraints on the
/// `(parent_var, child_var)` pair:
///
/// 1. A hard `LambdaConstraint` enforcing `parent.rank() >=
///    child.rank()`. Infeasible pairs (`Direct` parent → `Tape`
///    child, `Direct` parent → `Lazy` child, `Lazy` parent →
///    `Tape` child) are pruned by AC-3.
///
/// 2. A `SoftLambdaConstraint` that fires when the two tiers
///    differ, contributing `cross_module_coercion` to the
///    objective. The coercion cost reflects the per-call-site
///    tape-push wrapper AF.6 emits at the boundary.
///
/// Returns the count of edge-pairs wired (hard + soft
/// constraints added is `2 * returned_count`).
pub fn install(ctx: &ConstraintCtx<'_>, csp: &mut Csp<StrategyDomain>, ir: &GrammarIR) -> usize {
    let coercion_cost = ir.cost_config.egraph.weights.cross_module_coercion;

    let mut count = 0usize;

    for &parent in ctx.component {
        // Skip rules that don't carry a tier variable — the
        // dispatcher chose not to install one for this rule
        // (trivial / un-tiered), so there's nothing to wire.
        let Some(&parent_var) = ctx.tier_vars.get(&parent) else {
            continue;
        };

        let Some(rule) = ir.rules.get(parent as usize) else {
            continue;
        };

        // Walk the rule body collecting direct `Ref` targets.
        // Duplicate edges are deduplicated; the constraint is
        // pair-wise and re-adding it would be wasted solver
        // work.
        let mut targets: Vec<RuleId> = Vec::new();
        collect_refs(&rule.body, &mut targets);

        for child in targets {
            // Only constrain pairs within the same component —
            // a cross-component edge indicates the call graph
            // broke across the UnionFind boundary, which
            // should never happen if the components are
            // derived from `compute_rule_deps`. Guard
            // defensively.
            if !ctx.component.contains(&child) {
                continue;
            }

            let Some(&child_var) = ctx.tier_vars.get(&child) else {
                continue;
            };

            // ── Hard constraint: parent.rank() >= child.rank() ────────────
            //
            // Encoded as a two-variable `LambdaConstraint`.
            // The checker receives the current partial
            // assignment and returns `true` when either
            // endpoint is still unbound or when the rank
            // ordering holds.
            let p_idx = parent_var as usize;
            let c_idx = child_var as usize;
            csp.add_constraint(LambdaConstraint::new(
                vec![parent_var, child_var],
                move |assignment| {
                    let parent_tier = match &assignment[p_idx] {
                        Some(StrategyValue::Tier(t)) => *t,
                        _ => return true,
                    };
                    let child_tier = match &assignment[c_idx] {
                        Some(StrategyValue::Tier(t)) => *t,
                        _ => return true,
                    };
                    parent_tier.rank() >= child_tier.rank()
                },
                format!("parent_compat_hard({parent_var},{child_var})"),
            ));

            // ── Soft constraint: charge `cross_module_coercion`
            // whenever the tiers differ ──
            //
            // The soft checker returns `true` (satisfied) when
            // the tiers match exactly. When they differ the
            // checker returns `false` and the penalty
            // (`cross_module_coercion`) is added to the
            // objective by the optimizer. The branch-and-bound
            // solver compares the per-site tier reward against
            // this coercion cost to decide which parent/child
            // pair gets coerced.
            csp.add_soft_constraint(SoftLambdaConstraint::new(
                vec![parent_var, child_var],
                move |assignment| {
                    let parent_tier = match &assignment[p_idx] {
                        Some(StrategyValue::Tier(t)) => *t,
                        _ => return true,
                    };
                    let child_tier = match &assignment[c_idx] {
                        Some(StrategyValue::Tier(t)) => *t,
                        _ => return true,
                    };
                    // Satisfied (no penalty) when tiers match.
                    // Violated (penalty fires) on any boundary
                    // crossing regardless of direction — the
                    // hard constraint above has already pruned
                    // the illegal direction, so every
                    // remaining boundary crossing is a legal
                    // coercion and the soft cost is paid.
                    tier_eq(parent_tier, child_tier)
                },
                coercion_cost,
                format!("parent_compat_soft({parent_var},{child_var})"),
            ));

            count += 1;
        }
    }

    count
}

/// Local `IrNode::Ref` collector. The pass runs once per
/// component install, so we keep the walk inline rather than
/// pulling in `compute_rule_deps` (which rebuilds the entire
/// per-grammar adjacency list and is the wrong granularity
/// for a single-rule walk).
fn collect_refs(node: &IrNode, out: &mut Vec<RuleId>) {
    match node {
        IrNode::Ref(id) => {
            if !out.contains(id) {
                out.push(*id);
            }
        }

        IrNode::Seq(children) => {
            for child in children {
                collect_refs(child, out);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_refs(&branch.node, out);
            }
        }

        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => collect_refs(inner, out),

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_refs(a, out);
            collect_refs(b, out);
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_refs(token, out);
            for arm in arms {
                collect_refs(&arm.continuation, out);
            }
            collect_refs(fallback, out);
        }

        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// Exact equality on [`EmissionTier`]. Extracted for symmetry
/// with the `LambdaConstraint` soft-check body — keeps the
/// closure readable without a `==` forest.
#[inline]
fn tier_eq(a: EmissionTier, b: EmissionTier) -> bool {
    a.rank() == b.rank()
}
