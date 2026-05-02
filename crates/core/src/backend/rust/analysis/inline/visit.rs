//! IR-tree visitors used by the inline plan: ref counting, fan-out,
//! cost estimation, and self-reference detection.
//!
//! These walkers feed the cost-budget constraint and the single-site
//! inline eligibility predicate. Each walker is structural and
//! grammar-agnostic — every `IrNode` variant is reachable; per-node
//! cost estimates read budgets from [`CostBudgets`] so the inline
//! decision boundary stays in lockstep with the shared
//! [`egraph::CostWeights`].

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use super::budgets::CostBudgets;

/// Count references to each rule across every rule body.
///
/// Used by [`super::plan::analyze_parse_inline_plan`] to size the
/// ref-count vector that drives the cost-budget constraint and the
/// single-site-inline eligibility predicate.
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
/// [`egraph::CostWeights::default`] — useful for standalone callers
/// and tests that don't have a `CostWeights` handy. Production code
/// should call [`estimate_expansion_cost_with_budgets`] with the
/// per-compile budgets from
/// `CostBudgets::from_weights(&ir.cost_config.egraph.weights)`.
pub fn estimate_expansion_cost(node: &IrNode) -> usize {
    let budgets = CostBudgets::from_weights(&egraph::CostWeights::default());
    estimate_expansion_cost_with_budgets(node, &budgets)
}

/// Estimate the code expansion cost of inlining a rule body, reading
/// per-node cost estimates from the supplied [`CostBudgets`].
///
/// Tranche AF.2: the per-node costs (leaf / ref / alt branch /
/// repeat / negate) that used to be hardcoded here are now scalar
/// projections of [`egraph::CostWeights::call_overhead`] and
/// [`egraph::CostWeights::inline_body_size_penalty`]. See
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

/// Detect direct or transitive self-reference in a rule body.
///
/// Used by the single-site-inline predicate: a rule that references
/// itself cannot be inlined into its sole call site without breaking
/// recursion semantics, so single-site inline rejects self-referential
/// bodies even when ref-count is 1.
pub(super) fn body_has_self_ref(node: &IrNode, rule_id: RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children.iter().any(|c| body_has_self_ref(c, rule_id)),
        IrNode::Alt(branches, _) => {
            branches.iter().any(|b| body_has_self_ref(&b.node, rule_id))
        }
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

/// Compute single-site inline eligibility per rule.
///
/// A rule is single-site-inline-eligible when:
/// - it is cyclic (so it is not the root grammar rule),
/// - it is not the root rule (`rule.id != 0`),
/// - it has exactly one reference site,
/// - it does not preserve identity (consumers locate the rule by
///   variant_idx),
/// - its body does not directly self-reference,
/// - it has no `recover` or `pretty` directive.
pub(super) fn compute_single_site_inline_with_ref_counts(
    ir: &GrammarIR,
    ref_counts: &[u32],
) -> Vec<bool> {
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
