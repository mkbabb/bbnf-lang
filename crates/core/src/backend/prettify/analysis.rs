//! Backend-agnostic IR node analysis for prettify checkpoint optimization.

use bbnf_ir::IrNode;

use super::types::{PrettyRulePlan, WrapperPolicy};

/// Returns true if the node only emits builder ops on success paths.
/// Atomic nodes (Literal, Regex, Epsilon) never leave partial state.
/// Used to decide whether checkpoint/restore is needed around the node.
pub fn emits_only_on_success(
    node: &IrNode,
    plans: &[PrettyRulePlan],
    ir: &bbnf_ir::GrammarIR,
) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Ref(rule_id) => {
            let plan = &plans[*rule_id as usize];
            plan.inline && emits_only_on_success(&ir.rules[*rule_id as usize].body, plans, ir)
        }
        IrNode::Map { inner, .. } => emits_only_on_success(inner, plans, ir),
        IrNode::OptionalWhitespace(_) => false,
        IrNode::Seq(_)
        | IrNode::Alt(_, _)
        | IrNode::Repeat { .. }
        | IrNode::Skip(_, _)
        | IrNode::Next(_, _)
        | IrNode::Minus(_, _)
        | IrNode::Negate(_)
        | IrNode::TokenDispatch { .. } => false,
    }
}

/// Returns true if the expression might open groups (via non-inlined rule calls
/// whose wrappers use Group or GroupIndent). When false, light_checkpoint is
/// safe because no stale group_stack entries can be left behind on failure.
pub fn may_open_groups(node: &IrNode, plans: &[PrettyRulePlan], ir: &bbnf_ir::GrammarIR) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
        IrNode::Ref(rule_id) => {
            let plan = &plans[*rule_id as usize];
            if plan.inline {
                may_open_groups(&ir.rules[*rule_id as usize].body, plans, ir)
            } else {
                matches!(
                    plan.policy.wrapper,
                    WrapperPolicy::Group | WrapperPolicy::GroupIndent
                )
            }
        }
        IrNode::Seq(children) => children.iter().any(|c| may_open_groups(c, plans, ir)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| may_open_groups(&b.node, plans, ir)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => may_open_groups(inner, plans, ir),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            may_open_groups(a, plans, ir) || may_open_groups(b, plans, ir)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            may_open_groups(token, plans, ir)
                || arms
                    .iter()
                    .any(|a| may_open_groups(&a.continuation, plans, ir))
                || may_open_groups(fallback, plans, ir)
        }
    }
}
