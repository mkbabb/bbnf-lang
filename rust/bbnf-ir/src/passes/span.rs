//! Pass 2.7: Span eligibility analysis on the IR.
//!
//! Identifies rules whose entire body can be expressed as a SpanParser
//! (zero-copy, enum-dispatched, vtable-free). Also identifies spannable
//! sub-expressions within non-span-eligible rules.

use std::collections::HashSet;

use crate::{GrammarIR, IrNode, RuleId};

/// Refine span eligibility by analyzing the IR structure.
///
/// A rule is span-eligible if its entire body can produce a `Span<'a>` without
/// any semantic transformations (Map, Box, enum wrapping). This means:
/// - Leaf nodes (Literal, Regex, Epsilon) are always span-eligible.
/// - Combinators (Seq, Alt, Repeat, Skip, Next) preserve span eligibility
///   if all children are span-eligible.
/// - Ref(id) is span-eligible only if the referenced rule is span-eligible.
/// - Minus is span-eligible if both sides are span-eligible.
/// - Map is never span-eligible (transforms the output type).
/// - Negate is zero-width — span-eligible.
/// - OptionalWhitespace is span-eligible if inner is span-eligible.
pub fn refine_span_eligibility(ir: &mut GrammarIR) {
    // Fixed-point iteration: span eligibility of Ref nodes depends on
    // the eligibility of the referenced rule, which may not be computed yet.
    let mut eligible: HashSet<RuleId> = HashSet::new();

    loop {
        let mut changed = false;

        for rule in &ir.rules {
            // Skip cyclic rules — they can't be fully span-eligible
            // (would require recursive SpanParser which doesn't exist).
            if rule.meta.is_cyclic {
                continue;
            }

            let was_eligible = eligible.contains(&rule.id);
            let is_eligible = node_is_span_eligible(&rule.body, &eligible);

            if is_eligible && !was_eligible {
                eligible.insert(rule.id);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    // Update the rules. @token rules are unconditionally span-eligible.
    for rule in &mut ir.rules {
        rule.meta.span_eligible = eligible.contains(&rule.id) || rule.meta.is_token;
    }
}

/// Check if an IrNode is span-eligible (can be expressed as SpanParser).
fn node_is_span_eligible(node: &IrNode, eligible_rules: &HashSet<RuleId>) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,

        IrNode::Seq(children) => children.iter().all(|c| node_is_span_eligible(c, eligible_rules)),

        IrNode::Alt(branches, _) => branches
            .iter()
            .all(|b| node_is_span_eligible(&b.node, eligible_rules)),

        IrNode::Repeat { inner, .. } => node_is_span_eligible(inner, eligible_rules),

        IrNode::Ref(id) => eligible_rules.contains(id),

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            node_is_span_eligible(a, eligible_rules) && node_is_span_eligible(b, eligible_rules)
        }

        IrNode::Negate(_) => true, // Zero-width assertion.

        IrNode::OptionalWhitespace(inner) => node_is_span_eligible(inner, eligible_rules),

        IrNode::Map { .. } => false, // Transforms output type.

        IrNode::TokenDispatch { .. } => false, // Complex dispatch node.
    }
}

/// Compute which span-eligible rules actually get `_sp()` methods.
///
/// A subset of `span_eligible`: a rule gets an `_sp()` method only if its body
/// (after unwrapping Map wrappers) can be fully expressed as a SpanParser, which
/// requires all referenced rules to also have `_sp()` methods.
///
/// Stores the result in `RuleMeta::has_sp_method` for use by `infer_types`.
pub fn compute_sp_method_rules(ir: &mut GrammarIR) {
    let mut sp_set: HashSet<RuleId> = HashSet::new();

    loop {
        let mut changed = false;

        for rule in &ir.rules {
            if !rule.meta.span_eligible || sp_set.contains(&rule.id) {
                continue;
            }
            // Unwrap Map wrappers (EnumWrap, BoxWrap) — SpanParser doesn't do enum wrapping.
            let body = unwrap_map_node(&rule.body);
            if can_be_span_parser(body, &sp_set) {
                sp_set.insert(rule.id);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    // Store in RuleMeta.
    for rule in &mut ir.rules {
        rule.meta.has_sp_method = sp_set.contains(&rule.id);
    }
}

/// Unwrap Map nodes (enum/box wrappers are transparent for span purposes).
fn unwrap_map_node(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_map_node(inner),
        other => other,
    }
}

/// Check if an IrNode body can be expressed as a SpanParser,
/// given the current set of rules that have `_sp()` methods.
fn can_be_span_parser(node: &IrNode, sp_set: &HashSet<RuleId>) -> bool {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Ref(id) => sp_set.contains(id),
        IrNode::Seq(children) => children.iter().all(|c| can_be_span_parser(c, sp_set)),
        IrNode::Alt(branches, _) => {
            branches.iter().all(|b| can_be_span_parser(&b.node, sp_set))
        }
        IrNode::Repeat { inner, .. } => can_be_span_parser(inner, sp_set),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            can_be_span_parser(a, sp_set) && can_be_span_parser(b, sp_set)
        }
        IrNode::Negate(inner) => can_be_span_parser(inner, sp_set),
        IrNode::OptionalWhitespace(inner) => can_be_span_parser(inner, sp_set),
        IrNode::Map { .. } => false, // Custom maps can't be span parsers.
        IrNode::TokenDispatch { .. } => false, // Complex dispatch node.
    }
}
