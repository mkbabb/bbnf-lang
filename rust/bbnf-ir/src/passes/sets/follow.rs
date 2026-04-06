//! Pass 2.1: FOLLOW set computation.
//!
//! Computes FOLLOW sets for all rules in the IR. FOLLOW(A) is the set of
//! characters that can appear immediately after A in any sentential form.
//!
//! Uses the `csp-solver` crate with a `CharSetDomain` lattice for fixed-point
//! propagation. Runs AFTER FIRST sets and nullable flags are frozen.
//!
//! Used by dispatch tables (for nullable branches) and error messages.

use std::collections::HashMap;

use csp_solver::Csp;
use csp_solver::constraint::VarId;

use crate::{CharSet128, GrammarIR, IrNode, RuleId};

use super::first_sets::{compute_node_first, compute_node_nullable};
use super::super::csp_domains::{
    CharSetDomain, CharSetGroundConstraint, CharSetUnionConstraint,
};

/// FOLLOW sets for all rules in the grammar.
pub type FollowSets = HashMap<RuleId, CharSet128>;

/// Compute FOLLOW sets for all rules in the IR.
///
/// Algorithm (via CSP propagation):
/// 1. For each `Ref(B)` in the body of rule A, generate constraints:
///    - Add FIRST(suffix) to FOLLOW(B)
///    - If suffix is nullable, add FOLLOW(A) to FOLLOW(B)
/// 2. For `Repeat(inner)` containing `Ref(B)`, add FIRST(inner) to FOLLOW(B)
/// 3. AC-3 propagation reaches the fixed point.
pub fn compute_follow_sets(ir: &GrammarIR) -> FollowSets {
    let mut csp: Csp<CharSetDomain> = Csp::new();

    // Allocate a variable for each rule's FOLLOW set.
    let follow_vars: HashMap<RuleId, VarId> = ir
        .rules
        .iter()
        .map(|r| (r.id, csp.add_variable(CharSetDomain::empty())))
        .collect();

    // Build FIRST set + nullable lookup per rule.
    let first_of: HashMap<RuleId, CharSet128> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.meta.first_set.clone()))
        .collect();
    let nullable: HashMap<RuleId, bool> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.meta.nullable))
        .collect();

    // Walk each rule's body and emit FOLLOW constraints.
    for rule in &ir.rules {
        emit_follow_constraints(
            &rule.body,
            rule.id,
            &first_of,
            &nullable,
            ir,
            &follow_vars,
            &mut csp,
        );
    }

    let _ = csp.propagate_monotonic();

    // Extract results.
    let mut follow = HashMap::new();
    for rule in &ir.rules {
        let var = follow_vars[&rule.id];
        follow.insert(rule.id, csp.variables[var as usize].domain.solved.clone());
    }

    follow
}

/// Walk an IrNode tree and emit CharSetDomain constraints for FOLLOW sets.
fn emit_follow_constraints(
    node: &IrNode,
    container_rule: RuleId,
    first_of: &HashMap<RuleId, CharSet128>,
    nullable: &HashMap<RuleId, bool>,
    ir: &GrammarIR,
    follow_vars: &HashMap<RuleId, VarId>,
    csp: &mut Csp<CharSetDomain>,
) {
    match node {
        IrNode::Seq(children) => {
            for (i, child) in children.iter().enumerate() {
                // Recurse into child.
                emit_follow_constraints(
                    child,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow_vars,
                    csp,
                );

                // If child is Ref(B), compute what follows it.
                if let IrNode::Ref(ref_id) = child {
                    if let Some(&follow_var) = follow_vars.get(ref_id) {
                        // Compute FIRST of the suffix after this Ref.
                        let mut suffix_first = CharSet128::new();
                        let mut suffix_nullable = true;

                        for sibling in children.iter().skip(i + 1) {
                            let child_first = compute_node_first(sibling, first_of, ir);
                            suffix_first.union(&child_first);
                            if !compute_node_nullable(sibling, nullable) {
                                suffix_nullable = false;
                                break;
                            }
                        }

                        // Add FIRST(suffix) to FOLLOW(B) as a ground constraint.
                        if !suffix_first.is_empty() {
                            csp.add_constraint(CharSetGroundConstraint::new(
                                follow_var,
                                suffix_first,
                            ));
                        }

                        // If suffix is nullable, FOLLOW(container) flows into FOLLOW(B).
                        if suffix_nullable {
                            if let Some(&container_follow) = follow_vars.get(&container_rule) {
                                csp.add_constraint(CharSetUnionConstraint::new(
                                    follow_var,
                                    container_follow,
                                ));
                            }
                        }
                    }
                }
            }
        }

        IrNode::Alt(branches, _) => {
            for branch in branches {
                emit_follow_constraints(
                    &branch.node,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow_vars,
                    csp,
                );
            }
        }

        IrNode::Repeat { inner, .. } => {
            emit_follow_constraints(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow_vars,
                csp,
            );

            // For Ref(B) inside a repeat, FIRST(inner) is also in FOLLOW(B)
            // because the repeat can loop.
            let inner_first = compute_node_first(inner, first_of, ir);
            match inner.as_ref() {
                IrNode::Ref(ref_id) => {
                    if let Some(&follow_var) = follow_vars.get(ref_id) {
                        if !inner_first.is_empty() {
                            csp.add_constraint(CharSetGroundConstraint::new(
                                follow_var,
                                inner_first,
                            ));
                        }
                    }
                }
                // For Repeat { inner: Seq([..., Ref(B)]) }, the last Ref in the
                // sequence also has FIRST(inner) in its FOLLOW (the repeat loops back).
                IrNode::Seq(children) => {
                    for child in children.iter().rev() {
                        if let IrNode::Ref(ref_id) = child {
                            if let Some(&follow_var) = follow_vars.get(ref_id) {
                                if !inner_first.is_empty() {
                                    csp.add_constraint(CharSetGroundConstraint::new(
                                        follow_var,
                                        inner_first.clone(),
                                    ));
                                }
                            }
                        }
                        if !compute_node_nullable(child, nullable) {
                            break;
                        }
                    }
                }
                _ => {}
            }
        }

        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            emit_follow_constraints(a, container_rule, first_of, nullable, ir, follow_vars, csp);
            emit_follow_constraints(b, container_rule, first_of, nullable, ir, follow_vars, csp);

            // For Skip(Ref(A), B): FIRST(B) is in FOLLOW(A).
            if let IrNode::Ref(ref_id) = a.as_ref() {
                if let Some(&follow_var) = follow_vars.get(ref_id) {
                    let b_first = compute_node_first(b, first_of, ir);
                    if !b_first.is_empty() {
                        csp.add_constraint(CharSetGroundConstraint::new(follow_var, b_first));
                    }
                }
            }
        }

        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            emit_follow_constraints(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow_vars,
                csp,
            );
        }

        IrNode::Map { inner, .. } => {
            emit_follow_constraints(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow_vars,
                csp,
            );
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            emit_follow_constraints(
                token,
                container_rule,
                first_of,
                nullable,
                ir,
                follow_vars,
                csp,
            );
            for arm in arms {
                emit_follow_constraints(
                    &arm.continuation,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow_vars,
                    csp,
                );
            }
            emit_follow_constraints(
                fallback,
                container_rule,
                first_of,
                nullable,
                ir,
                follow_vars,
                csp,
            );
        }

        IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
