//! FIRST set computation for the IR.
//!
//! Uses the `csp-solver` crate with a `CharSetDomain` lattice for fixed-point
//! propagation. Nullable flags are computed as a separate pre-pass (simple boolean
//! fixed-point, no CSP needed).
//!
//! Equivalent to the AST-level `bbnf::graph::first_sets` module.

use std::collections::HashMap;

use csp_solver::Csp;
use csp_solver::constraint::VarId;

use crate::{CharSet128, GrammarIR, IrNode, RuleId, regex_first};

use super::super::csp_domains::{
    CharSetDomain, CharSetGroundConstraint, CharSetMultiUnionConstraint,
    CharSetSeqFirstConstraint, CharSetUnionConstraint,
};

/// Compute FIRST sets and nullable flags for all rules, and populate
/// per-branch FIRST sets on all `Alt` nodes.
pub fn compute_first_sets(ir: &mut GrammarIR) {
    let n = ir.rules.len();

    // Phase 1: Compute nullable flags via simple fixed-point iteration.
    // (No CSP needed — boolean convergence is trivial.)
    loop {
        let mut changed = false;
        let nullable_of: HashMap<RuleId, bool> = ir
            .rules
            .iter()
            .map(|r| (r.id, r.meta.nullable))
            .collect();

        for i in 0..n {
            let new_nullable = compute_node_nullable(&ir.rules[i].body, &nullable_of);
            if ir.rules[i].meta.nullable != new_nullable {
                ir.rules[i].meta.nullable = new_nullable;
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    // Phase 2: Compute FIRST sets via CSP propagation.
    let mut csp: Csp<CharSetDomain> = Csp::new();

    // Allocate a variable for each rule.
    let rule_vars: HashMap<RuleId, VarId> = ir
        .rules
        .iter()
        .map(|r| (r.id, csp.add_variable(CharSetDomain::empty())))
        .collect();

    // Build nullable snapshot for SeqFirst constraints.
    let nullable_of: HashMap<RuleId, bool> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.meta.nullable))
        .collect();

    // Generate constraints from each rule's body.
    for rule in &ir.rules {
        let rule_var = rule_vars[&rule.id];
        let body_var = generate_first_constraints(&rule.body, &rule_vars, &nullable_of, ir, &mut csp);
        csp.add_constraint(CharSetUnionConstraint::new(rule_var, body_var));
    }

    csp.finalize();
    let _ = csp.propagate();

    // Extract results into rule metadata.
    for rule in &mut ir.rules {
        let var = rule_vars[&rule.id];
        rule.meta.first_set = csp.variables[var as usize].domain.solved.clone();
    }

    // Phase 3: Populate per-branch FIRST sets for all Alt nodes.
    populate_branch_first_sets(ir);
}

/// Walk an IrNode and emit CharSetDomain constraints, returning the variable
/// representing this node's FIRST set.
fn generate_first_constraints(
    node: &IrNode,
    rule_vars: &HashMap<RuleId, VarId>,
    nullable_of: &HashMap<RuleId, bool>,
    ir: &GrammarIR,
    csp: &mut Csp<CharSetDomain>,
) -> VarId {
    match node {
        IrNode::Literal(sid) => {
            let var = csp.add_variable(CharSetDomain::empty());
            let mut cs = CharSet128::new();
            let s = ir.get_string(*sid);
            if let Some(&b) = s.as_bytes().first() {
                if b < 128 {
                    cs.add(b);
                }
            }
            csp.add_constraint(CharSetGroundConstraint::new(var, cs));
            var
        }

        IrNode::Regex(sid) => {
            let var = csp.add_variable(CharSetDomain::empty());
            let pattern = ir.get_string(*sid);
            let cs = regex_first::regex_first_chars(pattern).unwrap_or_default();
            csp.add_constraint(CharSetGroundConstraint::new(var, cs));
            var
        }

        IrNode::Epsilon => {
            csp.add_variable(CharSetDomain::empty())
            // No ground constraint needed — empty set is already the initial value.
        }

        IrNode::Ref(id) => {
            let var = csp.add_variable(CharSetDomain::empty());
            if let Some(&rule_var) = rule_vars.get(id) {
                csp.add_constraint(CharSetUnionConstraint::new(var, rule_var));
            }
            var
        }

        IrNode::Seq(children) => {
            let var = csp.add_variable(CharSetDomain::empty());
            let child_vars: Vec<VarId> = children
                .iter()
                .map(|c| generate_first_constraints(c, rule_vars, nullable_of, ir, csp))
                .collect();
            let nullable_flags: Vec<bool> = children
                .iter()
                .map(|c| is_node_nullable_with_map(c, nullable_of))
                .collect();
            csp.add_constraint(CharSetSeqFirstConstraint::new(var, child_vars, nullable_flags));
            var
        }

        IrNode::Alt(branches, _) => {
            let var = csp.add_variable(CharSetDomain::empty());
            let branch_vars: Vec<VarId> = branches
                .iter()
                .map(|b| generate_first_constraints(&b.node, rule_vars, nullable_of, ir, csp))
                .collect();
            csp.add_constraint(CharSetMultiUnionConstraint::new(var, branch_vars));
            var
        }

        IrNode::Repeat { inner, .. } => {
            let var = csp.add_variable(CharSetDomain::empty());
            let inner_var = generate_first_constraints(inner, rule_vars, nullable_of, ir, csp);
            csp.add_constraint(CharSetUnionConstraint::new(var, inner_var));
            var
        }

        IrNode::Skip(a, _) | IrNode::Next(a, _) | IrNode::Minus(a, _) => {
            let var = csp.add_variable(CharSetDomain::empty());
            let a_var = generate_first_constraints(a, rule_vars, nullable_of, ir, csp);
            csp.add_constraint(CharSetUnionConstraint::new(var, a_var));
            var
        }

        IrNode::Negate(_) => {
            // Zero-width assertion — empty FIRST set.
            csp.add_variable(CharSetDomain::empty())
        }

        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            let var = csp.add_variable(CharSetDomain::empty());
            let inner_var = generate_first_constraints(inner, rule_vars, nullable_of, ir, csp);
            csp.add_constraint(CharSetUnionConstraint::new(var, inner_var));
            var
        }

        IrNode::TokenDispatch { token, .. } => {
            let var = csp.add_variable(CharSetDomain::empty());
            let token_var = generate_first_constraints(token, rule_vars, nullable_of, ir, csp);
            csp.add_constraint(CharSetUnionConstraint::new(var, token_var));
            var
        }
    }
}

/// Check if a node is nullable, using the rule-level nullable map for Ref lookups.
/// This is a richer check than `is_node_nullable_simple` — it handles Ref nodes.
fn is_node_nullable_with_map(node: &IrNode, nullable_of: &HashMap<RuleId, bool>) -> bool {
    match node {
        IrNode::Epsilon | IrNode::OptionalWhitespace(_) => true,
        IrNode::Repeat { lo: 0, .. } => true,
        IrNode::Literal(_) | IrNode::Regex(_) => false,
        IrNode::Negate(_) => false,
        IrNode::Ref(id) => nullable_of.get(id).copied().unwrap_or(false),
        IrNode::Seq(children) => children.iter().all(|c| is_node_nullable_with_map(c, nullable_of)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| is_node_nullable_with_map(&b.node, nullable_of)),
        IrNode::Repeat { inner, .. } => is_node_nullable_with_map(inner, nullable_of),
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            is_node_nullable_with_map(a, nullable_of) && is_node_nullable_with_map(b, nullable_of)
        }
        IrNode::Minus(a, _) => is_node_nullable_with_map(a, nullable_of),
        IrNode::Map { inner, .. } => is_node_nullable_with_map(inner, nullable_of),
        IrNode::TokenDispatch { token, .. } => is_node_nullable_with_map(token, nullable_of),
    }
}

/// Compute FIRST set for an arbitrary `IrNode`.
///
/// Public for reuse by other passes (e.g., `compute_follow_sets`, `generate_dispatch_tables`).
pub fn compute_node_first(
    node: &IrNode,
    first_of: &HashMap<RuleId, CharSet128>,
    ir: &GrammarIR,
) -> CharSet128 {
    match node {
        IrNode::Literal(sid) => {
            let mut cs = CharSet128::new();
            let s = ir.get_string(*sid);
            if let Some(&b) = s.as_bytes().first() {
                if b < 128 {
                    cs.add(b);
                }
            }
            cs
        }
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            regex_first::regex_first_chars(pattern).unwrap_or_default()
        }
        IrNode::Epsilon => CharSet128::new(),
        IrNode::Ref(id) => first_of.get(id).cloned().unwrap_or_default(),
        IrNode::Seq(children) => {
            let mut cs = CharSet128::new();
            for child in children {
                cs.union(&compute_node_first(child, first_of, ir));
                if !is_node_nullable_simple(child) {
                    break;
                }
            }
            cs
        }
        IrNode::Alt(branches, _) => {
            let mut cs = CharSet128::new();
            for branch in branches {
                if let Some(ref fs) = branch.first_set {
                    cs.union(fs);
                } else {
                    cs.union(&compute_node_first(&branch.node, first_of, ir));
                }
            }
            cs
        }
        IrNode::Repeat { inner, .. } => compute_node_first(inner, first_of, ir),
        IrNode::Skip(a, _) | IrNode::Next(a, _) | IrNode::Minus(a, _) => {
            compute_node_first(a, first_of, ir)
        }
        IrNode::Negate(_) => CharSet128::new(),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            compute_node_first(inner, first_of, ir)
        }
        IrNode::TokenDispatch { token, .. } => compute_node_first(token, first_of, ir),
    }
}

/// Check if a node can match the empty string.
pub fn compute_node_nullable(node: &IrNode, nullable_of: &HashMap<RuleId, bool>) -> bool {
    match node {
        IrNode::Epsilon => true,
        IrNode::Literal(_) | IrNode::Regex(_) => false,
        IrNode::Ref(id) => nullable_of.get(id).copied().unwrap_or(false),
        IrNode::Repeat { lo: 0, .. } => true,
        IrNode::Repeat { inner, .. } => compute_node_nullable(inner, nullable_of),
        IrNode::Seq(children) => children.iter().all(|c| compute_node_nullable(c, nullable_of)),
        IrNode::Alt(branches, _) => {
            branches.iter().any(|b| compute_node_nullable(&b.node, nullable_of))
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            compute_node_nullable(a, nullable_of) && compute_node_nullable(b, nullable_of)
        }
        IrNode::Minus(a, _) => compute_node_nullable(a, nullable_of),
        IrNode::Negate(_) => false,
        IrNode::OptionalWhitespace(_) => true,
        IrNode::Map { inner, .. } => compute_node_nullable(inner, nullable_of),
        IrNode::TokenDispatch { token, .. } => compute_node_nullable(token, nullable_of),
    }
}

/// Quick nullable check without rule lookup (for leaf-level decisions in FIRST computation).
fn is_node_nullable_simple(node: &IrNode) -> bool {
    matches!(
        node,
        IrNode::Epsilon | IrNode::Repeat { lo: 0, .. } | IrNode::OptionalWhitespace(_)
    )
}

/// Walk all rule bodies and populate `AltBranch.first_set` for every `Alt` node.
fn populate_branch_first_sets(ir: &mut GrammarIR) {
    let first_of: HashMap<RuleId, CharSet128> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.meta.first_set.clone()))
        .collect();

    // We need owned strings for regex_first lookup; clone the string table reference.
    let strings: Vec<String> = ir.strings.clone();

    for rule in &mut ir.rules {
        populate_branch_first_in_node(&mut rule.body, &first_of, &strings);
    }
}

fn populate_branch_first_in_node(
    node: &mut IrNode,
    first_of: &HashMap<RuleId, CharSet128>,
    strings: &[String],
) {
    match node {
        IrNode::Alt(branches, _) => {
            for branch in branches.iter_mut() {
                if branch.first_set.is_none() {
                    branch.first_set =
                        Some(compute_node_first_with_strings(&branch.node, first_of, strings));
                }
                populate_branch_first_in_node(&mut branch.node, first_of, strings);
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                populate_branch_first_in_node(child, first_of, strings);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => {
            populate_branch_first_in_node(inner, first_of, strings);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            populate_branch_first_in_node(a, first_of, strings);
            populate_branch_first_in_node(b, first_of, strings);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            populate_branch_first_in_node(token, first_of, strings);
            for arm in arms {
                populate_branch_first_in_node(&mut arm.continuation, first_of, strings);
            }
            populate_branch_first_in_node(fallback, first_of, strings);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Variant of `compute_node_first` that takes `&[String]` instead of `&GrammarIR`.
/// Used during mutable tree walk where GrammarIR can't be borrowed.
fn compute_node_first_with_strings(
    node: &IrNode,
    first_of: &HashMap<RuleId, CharSet128>,
    strings: &[String],
) -> CharSet128 {
    match node {
        IrNode::Literal(sid) => {
            let mut cs = CharSet128::new();
            let s = &strings[*sid as usize];
            if let Some(&b) = s.as_bytes().first() {
                if b < 128 {
                    cs.add(b);
                }
            }
            cs
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            regex_first::regex_first_chars(pattern).unwrap_or_default()
        }
        IrNode::Epsilon => CharSet128::new(),
        IrNode::Ref(id) => first_of.get(id).cloned().unwrap_or_default(),
        IrNode::Seq(children) => {
            let mut cs = CharSet128::new();
            for child in children {
                cs.union(&compute_node_first_with_strings(child, first_of, strings));
                if !is_node_nullable_simple(child) {
                    break;
                }
            }
            cs
        }
        IrNode::Alt(branches, _) => {
            let mut cs = CharSet128::new();
            for branch in branches {
                if let Some(ref fs) = branch.first_set {
                    cs.union(fs);
                } else {
                    cs.union(&compute_node_first_with_strings(
                        &branch.node, first_of, strings,
                    ));
                }
            }
            cs
        }
        IrNode::Repeat { inner, .. } => {
            compute_node_first_with_strings(inner, first_of, strings)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) | IrNode::Minus(a, _) => {
            compute_node_first_with_strings(a, first_of, strings)
        }
        IrNode::Negate(_) => CharSet128::new(),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            compute_node_first_with_strings(inner, first_of, strings)
        }
        IrNode::TokenDispatch { token, .. } => {
            compute_node_first_with_strings(token, first_of, strings)
        }
    }
}
