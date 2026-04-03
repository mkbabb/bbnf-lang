//! Left-recursion elimination passes (IR level).
//!
//! Direct left-recursion elimination follows the standard algorithm:
//! For a rule `A = A α₁ | A α₂ | ... | β₁ | β₂ | ...`
//! Transform to:
//!   `A  = β₁ A' | β₂ A' | ...`
//!   `A' = α₁ A' | α₂ A' | ... | ε`
//!
//! Indirect left-recursion uses Paull's algorithm: for rules ordered
//! A₁, A₂, ..., Aₙ within each multi-member SCC, substitute earlier
//! rules' bodies forward, converting indirect cycles to direct ones.
//!
//! Both are opt-in via flag, matching the TypeScript design.

use std::collections::HashMap;

use crate::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

// ── Direct Left-Recursion Elimination ──────────────────────────────────────

/// Eliminate all direct left-recursion from the grammar.
///
/// For each rule whose body is an `Alt` with branches that begin with
/// `Ref(self_id)`, split into alpha (recursive) and beta (non-recursive)
/// branches, then rewrite using the standard algorithm.
pub fn eliminate_direct_lr(ir: &mut GrammarIR) {
    // Collect new tail rules to append after the main loop.
    let mut new_rules: Vec<IrRule> = Vec::new();

    // String dedup for interning tail rule names.
    let mut string_dedup: HashMap<String, u32> = ir
        .strings
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i as u32))
        .collect();

    for rule_idx in 0..ir.rules.len() {
        let rule_id = ir.rules[rule_idx].id;

        // Only process Alt bodies.
        let branches = match &ir.rules[rule_idx].body {
            IrNode::Alt(branches, _) => branches.clone(),
            _ => continue,
        };

        // Partition into left-recursive (alpha) and non-recursive (beta).
        let mut alphas: Vec<IrNode> = Vec::new();
        let mut betas: Vec<AltBranch> = Vec::new();

        for branch in &branches {
            if starts_with_ref(&branch.node, rule_id) {
                if let Some(stripped) = strip_leading_ref(&branch.node, rule_id) {
                    alphas.push(stripped);
                } else {
                    betas.push(branch.clone());
                }
            } else {
                betas.push(branch.clone());
            }
        }

        if alphas.is_empty() {
            continue;
        }

        // Create the tail rule: fresh RuleId and interned name.
        let tail_id = (ir.rules.len() + new_rules.len()) as RuleId;
        let rule_name = ir.strings[ir.rules[rule_idx].name as usize].clone();
        let tail_name = format!("{}_tail", rule_name);
        let tail_name_id = intern_string(tail_name, &mut ir.strings, &mut string_dedup);

        // A = β₁ A' | β₂ A' | ...
        let new_betas: Vec<AltBranch> = betas
            .into_iter()
            .map(|beta| AltBranch {
                node: IrNode::Seq(vec![beta.node, IrNode::Ref(tail_id)]),
                first_set: None,
            })
            .collect();

        let new_body = if new_betas.len() == 1 {
            new_betas
                .into_iter()
                .next()
                .expect("new_betas verified to have exactly one element")
                .node
        } else {
            IrNode::Alt(new_betas, None)
        };

        ir.rules[rule_idx].body = new_body;

        // A' = α₁ A' | α₂ A' | ... | ε
        let mut tail_branches: Vec<AltBranch> = alphas
            .into_iter()
            .map(|alpha| AltBranch {
                node: IrNode::Seq(vec![alpha, IrNode::Ref(tail_id)]),
                first_set: None,
            })
            .collect();

        // Add epsilon branch.
        tail_branches.push(AltBranch {
            node: IrNode::Epsilon,
            first_set: None,
        });

        let tail_body = IrNode::Alt(tail_branches, None);

        new_rules.push(IrRule {
            id: tail_id,
            name: tail_name_id,
            body: tail_body,
            meta: RuleMeta::default(),
            source_span: None,
        });
    }

    // Append new tail rules.
    ir.rules.extend(new_rules);
}

// ── Indirect Left-Recursion Elimination (Paull's Algorithm) ────────────────

/// Eliminate indirect left-recursion using Paull's algorithm.
///
/// For each multi-member SCC (where indirect cycles occur), processes rules
/// in order: for A_i, substitute all earlier A_j (j < i) bodies where A_i
/// starts with Ref(A_j). After substitution, any remaining left-recursion
/// in A_i is direct and handled by `eliminate_direct_lr`.
///
/// Uses `RuleMeta.scc_id` and `RuleMeta.is_cyclic` set during lowering.
pub fn eliminate_indirect_lr(ir: &mut GrammarIR) {
    // Group rules by SCC id to find multi-member SCCs.
    let mut scc_members: HashMap<u32, Vec<RuleId>> = HashMap::new();
    for rule in &ir.rules {
        if let Some(scc_id) = rule.meta.scc_id {
            scc_members.entry(scc_id).or_default().push(rule.id);
        }
    }

    // Only process multi-member SCCs (where indirect cycles occur).
    let multi_sccs: Vec<Vec<RuleId>> = scc_members
        .into_values()
        .filter(|members| members.len() > 1)
        .collect();

    if multi_sccs.is_empty() {
        return;
    }

    // Build a rule_id → index lookup for efficient access.
    let id_to_idx: HashMap<RuleId, usize> = ir
        .rules
        .iter()
        .enumerate()
        .map(|(idx, r)| (r.id, idx))
        .collect();

    for scc_ids in &multi_sccs {
        // For each rule A_i in the SCC (in their stored order).
        for i in 0..scc_ids.len() {
            let id_i = scc_ids[i];
            let idx_i = match id_to_idx.get(&id_i) {
                Some(&idx) => idx,
                None => continue,
            };

            // For each earlier rule A_j in the SCC.
            for id_j in scc_ids.iter().take(i).copied() {
                let idx_j = match id_to_idx.get(&id_j) {
                    Some(&idx) => idx,
                    None => continue,
                };

                // Get A_j's body (clone to avoid borrow conflict).
                let body_j = ir.rules[idx_j].body.clone();

                // Substitute A_j's body where A_i starts with Ref(A_j).
                let body_i = ir.rules[idx_i].body.clone();
                if let Some(substituted) = substitute_leading_ref(&body_i, id_j, &body_j) {
                    ir.rules[idx_i].body = substituted;
                }
            }
        }
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Check if an IR node starts with a reference to the given rule.
pub fn starts_with_ref(node: &IrNode, rule_id: RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => {
            matches!(children.first(), Some(first) if starts_with_ref(first, rule_id))
        }
        _ => false,
    }
}

/// Strip the leading `Ref(rule_id)` from an IR node.
///
/// - `Ref(id)` -> `Epsilon`
/// - `Seq([Ref(id), rest...])` -> `Seq([rest...])` or `rest` if single
pub fn strip_leading_ref(node: &IrNode, rule_id: RuleId) -> Option<IrNode> {
    match node {
        IrNode::Ref(id) if *id == rule_id => Some(IrNode::Epsilon),
        IrNode::Seq(children) => {
            if children.is_empty() || !starts_with_ref(&children[0], rule_id) {
                return None;
            }
            let rest: Vec<IrNode> = children[1..].to_vec();
            Some(match rest.len() {
                0 => IrNode::Epsilon,
                1 => rest
                    .into_iter()
                    .next()
                    .expect("rest verified to have exactly one element"),
                _ => IrNode::Seq(rest),
            })
        }
        _ => None,
    }
}

/// Substitute `Ref(target_id)` at the leading position with `replacement`.
///
/// Handles alternations (distributes across branches) and concatenations
/// (replaces leading element).
pub fn substitute_leading_ref(
    node: &IrNode,
    target_id: RuleId,
    replacement: &IrNode,
) -> Option<IrNode> {
    match node {
        IrNode::Alt(branches, _) => {
            let mut any_changed = false;
            let mut new_branches: Vec<AltBranch> = Vec::new();

            for branch in branches {
                if let Some(sub) = substitute_leading_ref(&branch.node, target_id, replacement) {
                    // Flatten nested alternations produced by substitution.
                    if let IrNode::Alt(inner_branches, _) = sub {
                        for ib in inner_branches {
                            new_branches.push(ib);
                        }
                    } else {
                        new_branches.push(AltBranch {
                            node: sub,
                            first_set: None,
                        });
                    }
                    any_changed = true;
                } else {
                    new_branches.push(branch.clone());
                }
            }

            if any_changed {
                Some(IrNode::Alt(new_branches, None))
            } else {
                None
            }
        }

        IrNode::Seq(children) => {
            if children.is_empty() {
                return None;
            }

            if let IrNode::Ref(id) = &children[0] {
                if *id == target_id {
                    let rest: Vec<IrNode> = children[1..].to_vec();

                    // Distribute alternation replacement across rest.
                    if let IrNode::Alt(alt_branches, _) = replacement {
                        let new_branches: Vec<AltBranch> = alt_branches
                            .iter()
                            .map(|ab| {
                                let mut combined = vec![ab.node.clone()];
                                combined.extend(rest.iter().cloned());
                                AltBranch {
                                    node: IrNode::Seq(combined),
                                    first_set: None,
                                }
                            })
                            .collect();
                        return Some(IrNode::Alt(new_branches, None));
                    } else {
                        let mut combined = vec![replacement.clone()];
                        combined.extend(rest);
                        return Some(IrNode::Seq(combined));
                    }
                }
            }
            None
        }

        IrNode::Ref(id) if *id == target_id => Some(replacement.clone()),

        _ => None,
    }
}

/// Intern a string, reusing an existing entry if present.
pub fn intern_string(s: String, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> u32 {
    if let Some(&existing) = dedup.get(&s) {
        return existing;
    }
    let sid = strings.len() as u32;
    dedup.insert(s.clone(), sid);
    strings.push(s);
    sid
}

