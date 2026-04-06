//! Pass 2.7: Span eligibility analysis on the IR.
//!
//! Identifies rules whose entire body can be expressed as a SpanParser
//! (zero-copy, enum-dispatched, vtable-free). Also identifies spannable
//! sub-expressions within non-span-eligible rules.
//!
//! Uses the `csp-solver` crate with a `BoolDomain` lattice for propagation.

use std::collections::HashMap;
use std::collections::HashSet;

use csp_solver::Csp;
use csp_solver::constraint::VarId;

use crate::{GrammarIR, IrNode, RuleId};

use super::csp_domains::{BoolAndConstraint, BoolDomain, BoolEqualConstraint, BoolGroundConstraint};

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
///
/// Uses CSP AC-3 propagation to reach the fixed point.
pub fn refine_span_eligibility(ir: &mut GrammarIR) {
    let mut csp: Csp<BoolDomain> = Csp::new();

    // Allocate a variable for each rule.
    let rule_vars: HashMap<RuleId, VarId> = ir
        .rules
        .iter()
        .map(|r| (r.id, csp.add_variable(BoolDomain::unsolved())))
        .collect();

    // Generate constraints from each rule's body.
    for rule in &ir.rules {
        let rule_var = rule_vars[&rule.id];

        // Cyclic rules can't be span-eligible.
        if rule.meta.is_cyclic {
            csp.add_constraint(BoolGroundConstraint::new(rule_var, false));
            continue;
        }

        // Walk the body to generate a variable + constraints for it, then
        // equate the rule variable with the body variable.
        let body_var = generate_span_constraints(&rule.body, &rule_vars, &mut csp);
        csp.add_constraint(BoolEqualConstraint::new(rule_var, body_var));
    }

    csp.finalize();
    let _ = csp.propagate();

    // Extract results. @token rules are unconditionally span-eligible.
    for rule in &mut ir.rules {
        let solved = csp.variables[rule_vars[&rule.id] as usize]
            .domain
            .solved
            .unwrap_or(false);
        rule.meta.span_eligible = solved || rule.meta.directives.token;
    }
}

/// Walk an IrNode and emit BoolDomain constraints, returning the variable
/// representing this node's span eligibility.
fn generate_span_constraints(
    node: &IrNode,
    rule_vars: &HashMap<RuleId, VarId>,
    csp: &mut Csp<BoolDomain>,
) -> VarId {
    match node {
        // Leaves are always span-eligible.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {
            let var = csp.add_variable(BoolDomain::unsolved());
            csp.add_constraint(BoolGroundConstraint::new(var, true));
            var
        }

        // Negate is zero-width — always span-eligible.
        IrNode::Negate(_) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            csp.add_constraint(BoolGroundConstraint::new(var, true));
            var
        }

        // Map/TokenDispatch break span eligibility.
        IrNode::Map { .. } | IrNode::TokenDispatch { .. } => {
            let var = csp.add_variable(BoolDomain::unsolved());
            csp.add_constraint(BoolGroundConstraint::new(var, false));
            var
        }

        // Ref: eligibility propagates from the referenced rule.
        IrNode::Ref(id) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            if let Some(&rule_var) = rule_vars.get(id) {
                csp.add_constraint(BoolEqualConstraint::new(var, rule_var));
            } else {
                // Unknown rule — not eligible.
                csp.add_constraint(BoolGroundConstraint::new(var, false));
            }
            var
        }

        // Seq: all children must be eligible.
        IrNode::Seq(children) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            let child_vars: Vec<VarId> = children
                .iter()
                .map(|c| generate_span_constraints(c, rule_vars, csp))
                .collect();
            csp.add_constraint(BoolAndConstraint::new(var, child_vars));
            var
        }

        // Alt: all branches must be eligible.
        IrNode::Alt(branches, _) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            let branch_vars: Vec<VarId> = branches
                .iter()
                .map(|b| generate_span_constraints(&b.node, rule_vars, csp))
                .collect();
            csp.add_constraint(BoolAndConstraint::new(var, branch_vars));
            var
        }

        // Repeat: inner must be eligible.
        IrNode::Repeat { inner, .. } => {
            let var = csp.add_variable(BoolDomain::unsolved());
            let inner_var = generate_span_constraints(inner, rule_vars, csp);
            csp.add_constraint(BoolEqualConstraint::new(var, inner_var));
            var
        }

        // Binary ops: both sides must be eligible.
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            let a_var = generate_span_constraints(a, rule_vars, csp);
            let b_var = generate_span_constraints(b, rule_vars, csp);
            csp.add_constraint(BoolAndConstraint::new(var, vec![a_var, b_var]));
            var
        }

        // OptionalWhitespace: transparent.
        IrNode::OptionalWhitespace(inner) => {
            let var = csp.add_variable(BoolDomain::unsolved());
            let inner_var = generate_span_constraints(inner, rule_vars, csp);
            csp.add_constraint(BoolEqualConstraint::new(var, inner_var));
            var
        }
    }
}

/// Compute which span-eligible rules actually get `_sp()` methods.
///
/// A subset of `span_eligible`: a rule gets an `_sp()` method only if its body
/// (after unwrapping Map wrappers) can be fully expressed as a SpanParser, which
/// requires all referenced rules to also have `_sp()` methods.
///
/// Stores the result in `RuleMeta::has_sp_method` for use by `project_types`.
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
        IrNode::Alt(branches, _) => branches.iter().all(|b| can_be_span_parser(&b.node, sp_set)),
        IrNode::Repeat { inner, .. } => can_be_span_parser(inner, sp_set),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            can_be_span_parser(a, sp_set) && can_be_span_parser(b, sp_set)
        }
        IrNode::Negate(inner) => can_be_span_parser(inner, sp_set),
        IrNode::OptionalWhitespace(inner) => can_be_span_parser(inner, sp_set),
        IrNode::Map { .. } => false, // Value maps change the result type.
        IrNode::TokenDispatch { .. } => false, // Complex dispatch node.
    }
}
