//! Pass 2.1: FOLLOW set computation.
//!
//! Computes FOLLOW sets for all rules in the IR. FOLLOW(A) is the set of
//! characters that can appear immediately after A in any sentential form.
//!
//! Used by dispatch tables (for nullable branches) and error messages.

use std::collections::HashMap;

use crate::{CharSet128, GrammarIR, IrNode, RuleId};

use super::first_sets::{compute_node_first, compute_node_nullable};

/// FOLLOW sets for all rules in the grammar.
pub type FollowSets = HashMap<RuleId, CharSet128>;

/// Compute FOLLOW sets for all rules in the IR.
///
/// Algorithm:
/// 1. Initialize FOLLOW(entry) with EOF marker (not in ASCII range, so empty).
/// 2. For each rule body, for each `Ref(B)` in the body of rule A:
///    - Add FIRST(β) to FOLLOW(B), where β is what follows B in the body.
///    - If β is nullable, add FOLLOW(A) to FOLLOW(B).
/// 3. Repeat until no changes (fixed-point for cyclic grammars).
pub fn compute_follow_sets(ir: &GrammarIR) -> FollowSets {
    let mut follow: HashMap<RuleId, CharSet128> = HashMap::new();

    // Initialize all rules with empty FOLLOW sets.
    for rule in &ir.rules {
        follow.insert(rule.id, CharSet128::new());
    }

    // Build FIRST set + nullable lookup per rule.
    let first_of: HashMap<RuleId, CharSet128> =
        ir.rules.iter().map(|r| (r.id, r.meta.first_set.clone())).collect();
    let nullable: HashMap<RuleId, bool> =
        ir.rules.iter().map(|r| (r.id, r.meta.nullable)).collect();

    // Fixed-point iteration.
    loop {
        let mut changed = false;

        for rule in &ir.rules {
            let rule_id = rule.id;
            propagate_follow(
                &rule.body,
                rule_id,
                &first_of,
                &nullable,
                ir,
                &mut follow,
                &mut changed,
            );
        }

        if !changed {
            break;
        }
    }

    follow
}

/// Propagate FOLLOW sets through an IrNode tree.
fn propagate_follow(
    node: &IrNode,
    container_rule: RuleId,
    first_of: &HashMap<RuleId, CharSet128>,
    nullable: &HashMap<RuleId, bool>,
    ir: &GrammarIR,
    follow: &mut HashMap<RuleId, CharSet128>,
    changed: &mut bool,
) {
    match node {
        IrNode::Seq(children) => {
            for (i, child) in children.iter().enumerate() {
                // Recurse into child.
                propagate_follow(
                    child,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow,
                    changed,
                );

                // If child is Ref(B), compute what follows it.
                if let IrNode::Ref(ref_id) = child {
                    // FIRST of the suffix after this Ref.
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

                    // Add FIRST(suffix) to FOLLOW(B).
                    let entry = follow.entry(*ref_id).or_default();
                    let old = entry.bits;
                    entry.union(&suffix_first);
                    if entry.bits != old {
                        *changed = true;
                    }

                    // If suffix is nullable, add FOLLOW(container) to FOLLOW(B).
                    if suffix_nullable {
                        let container_follow =
                            follow.get(&container_rule).cloned().unwrap_or_default();
                        let entry = follow.entry(*ref_id).or_default();
                        let old = entry.bits;
                        entry.union(&container_follow);
                        if entry.bits != old {
                            *changed = true;
                        }
                    }
                }
            }
        }

        IrNode::Alt(branches, _) => {
            for branch in branches {
                propagate_follow(
                    &branch.node,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow,
                    changed,
                );
            }
        }

        IrNode::Repeat { inner, .. } => {
            propagate_follow(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow,
                changed,
            );

            // For Ref(B) inside a repeat, FIRST(inner) is also in FOLLOW(B)
            // because the repeat can loop.
            let inner_first = compute_node_first(inner, first_of, ir);
            match inner.as_ref() {
                IrNode::Ref(ref_id) => {
                    let entry = follow.entry(*ref_id).or_default();
                    let old = entry.bits;
                    entry.union(&inner_first);
                    if entry.bits != old {
                        *changed = true;
                    }
                }
                // For Repeat { inner: Seq([..., Ref(B)]) }, the last Ref in the
                // sequence also has FIRST(inner) in its FOLLOW (the repeat loops back).
                IrNode::Seq(children) => {
                    // Propagate FIRST(inner) into the FOLLOW of the last Ref in the sequence.
                    for child in children.iter().rev() {
                        if let IrNode::Ref(ref_id) = child {
                            let entry = follow.entry(*ref_id).or_default();
                            let old = entry.bits;
                            entry.union(&inner_first);
                            if entry.bits != old {
                                *changed = true;
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
            propagate_follow(a, container_rule, first_of, nullable, ir, follow, changed);
            propagate_follow(b, container_rule, first_of, nullable, ir, follow, changed);

            // For Skip(Ref(A), B): FIRST(B) is in FOLLOW(A).
            if let IrNode::Ref(ref_id) = a.as_ref() {
                let b_first = compute_node_first(b, first_of, ir);
                let entry = follow.entry(*ref_id).or_default();
                let old = entry.bits;
                entry.union(&b_first);
                if entry.bits != old {
                    *changed = true;
                }
            }
        }

        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => {
            propagate_follow(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow,
                changed,
            );
        }

        IrNode::Map { inner, .. } => {
            propagate_follow(
                inner,
                container_rule,
                first_of,
                nullable,
                ir,
                follow,
                changed,
            );
        }

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            propagate_follow(
                token,
                container_rule,
                first_of,
                nullable,
                ir,
                follow,
                changed,
            );
            for arm in arms {
                propagate_follow(
                    &arm.continuation,
                    container_rule,
                    first_of,
                    nullable,
                    ir,
                    follow,
                    changed,
                );
            }
            propagate_follow(
                fallback,
                container_rule,
                first_of,
                nullable,
                ir,
                follow,
                changed,
            );
        }

        IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

