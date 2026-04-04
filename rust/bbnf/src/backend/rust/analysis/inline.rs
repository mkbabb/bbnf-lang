use std::collections::HashSet;

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use super::specialize::gather_inline_shape_stats;

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

#[derive(Clone, Debug, Default)]
pub struct InlinePlan {
    pub parse_call_modes: Vec<CallMode>,
    pub single_site_inline: Vec<bool>,
}

pub fn analyze_parse_inline_plan(
    ir: &GrammarIR,
    operator_chain_rules: &HashSet<RuleId>,
) -> InlinePlan {
    const MAX_ALT_BRANCHES: usize = 32;
    const MAX_LOCAL_COST: usize = 80;
    const MAX_TOTAL_BUDGET: usize = 4096;

    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    let single_site_inline = compute_single_site_inline_with_ref_counts(ir, &ref_counts);
    let parse_call_modes = ir
        .rules
        .iter()
        .enumerate()
        .map(|(idx, rule)| {
            if single_site_inline[idx] {
                return CallMode::InlineBody;
            }
            if operator_chain_rules.contains(&rule.id) {
                return CallMode::DirectCall;
            }
            if rule.meta.directives.token {
                return CallMode::InlineBody;
            }
            if rule.meta.is_cyclic
                || rule.meta.directives.recover.is_some()
                || rule.meta.directives.pretty.is_some()
            {
                return CallMode::DirectCall;
            }

            let alt_branches = max_alt_branches(&rule.body);
            let local_cost = estimate_expansion_cost(&rule.body);
            let total_budget = local_cost.saturating_mul(ref_counts[idx] as usize);
            let shape = gather_inline_shape_stats(&rule.body);

            if should_force_direct_call(shape, local_cost, total_budget) {
                CallMode::DirectCall
            } else if alt_branches <= MAX_ALT_BRANCHES
                && local_cost <= MAX_LOCAL_COST
                && total_budget <= MAX_TOTAL_BUDGET
            {
                CallMode::InlineBody
            } else {
                CallMode::DirectCall
            }
        })
        .collect();

    InlinePlan {
        parse_call_modes,
        single_site_inline,
    }
}

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

