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
fn starts_with_ref(node: &IrNode, rule_id: RuleId) -> bool {
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
fn strip_leading_ref(node: &IrNode, rule_id: RuleId) -> Option<IrNode> {
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
fn substitute_leading_ref(
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
fn intern_string(s: String, strings: &mut Vec<String>, dedup: &mut HashMap<String, u32>) -> u32 {
    if let Some(&existing) = dedup.get(&s) {
        return existing;
    }
    let sid = strings.len() as u32;
    dedup.insert(s.clone(), sid);
    strings.push(s);
    sid
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a minimal GrammarIR for testing.
    fn make_ir(rules: Vec<(&str, IrNode, Option<u32>, bool)>) -> GrammarIR {
        let mut strings = Vec::new();
        let mut string_dedup: HashMap<String, u32> = HashMap::new();
        let mut ir_rules = Vec::new();

        for (i, (name, body, scc_id, is_cyclic)) in rules.into_iter().enumerate() {
            let name_id = intern_string(name.to_string(), &mut strings, &mut string_dedup);
            ir_rules.push(IrRule {
                id: i as RuleId,
                name: name_id,
                body,
                meta: RuleMeta {
                    scc_id,
                    is_cyclic,
                    ..Default::default()
                },
                source_span: None,
            });
        }

        let entry = ir_rules.last().map(|r| r.id).unwrap_or(0);

        GrammarIR {
            rules: ir_rules,
            entry,
            strings,
            fns: Vec::new(),
            types: Vec::new(),
            follow_sets: HashMap::new(),
            ws_pattern: None,
            collapse_simple_spans: false,
            debug_all: false,
            debug_labels: Vec::new(),
            type_map: None,
        }
    }

    fn lit(s: &str, ir: &mut GrammarIR) -> IrNode {
        let sid = ir.strings.len() as u32;
        ir.strings.push(s.to_string());
        IrNode::Literal(sid)
    }

    fn ref_node(id: RuleId) -> IrNode {
        IrNode::Ref(id)
    }

    fn alt(branches: Vec<IrNode>) -> IrNode {
        IrNode::Alt(
            branches
                .into_iter()
                .map(|node| AltBranch {
                    node,
                    first_set: None,
                })
                .collect(),
            None,
        )
    }

    fn seq(children: Vec<IrNode>) -> IrNode {
        IrNode::Seq(children)
    }

    // ── Direct LR Tests ────────────────────────────────────────────────

    #[test]
    fn direct_lr_eliminated() {
        // A = A "+" "x" | "x"
        // Rule A has id=0.
        let mut ir = make_ir(vec![
            ("A", IrNode::Epsilon, None, false), // placeholder
        ]);

        let plus = lit("+", &mut ir);
        let x1 = lit("x", &mut ir);
        let x2 = lit("x", &mut ir);

        ir.rules[0].body = alt(vec![seq(vec![ref_node(0), plus, x1]), x2]);

        eliminate_direct_lr(&mut ir);

        // Should produce A and A_tail rules.
        assert_eq!(ir.rules.len(), 2, "Expected 2 rules (A + A_tail)");
        assert_eq!(
            ir.strings[ir.rules[1].name as usize], "A_tail",
            "Second rule should be A_tail"
        );

        // A_tail should reference itself recursively and have an epsilon branch.
        let tail_body = &ir.rules[1].body;
        if let IrNode::Alt(branches, _) = tail_body {
            assert_eq!(
                branches.len(),
                2,
                "A_tail should have 2 branches (alpha + epsilon)"
            );
            // Last branch should be Epsilon.
            assert!(
                matches!(&branches.last().unwrap().node, IrNode::Epsilon),
                "Last branch of A_tail should be Epsilon"
            );
        } else {
            panic!("Expected A_tail body to be Alt, got {:?}", tail_body);
        }
    }

    #[test]
    fn no_lr_unchanged() {
        let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);

        let x = lit("x", &mut ir);
        let y = lit("y", &mut ir);
        ir.rules[0].body = alt(vec![x, y]);

        eliminate_direct_lr(&mut ir);

        // No tail rule should be created.
        assert_eq!(ir.rules.len(), 1, "Expected 1 rule (no LR to eliminate)");
    }

    #[test]
    fn non_alt_body_unchanged() {
        let mut ir = make_ir(vec![("A", IrNode::Epsilon, None, false)]);

        let x = lit("x", &mut ir);
        ir.rules[0].body = x;

        eliminate_direct_lr(&mut ir);
        assert_eq!(ir.rules.len(), 1);
    }

    // ── Indirect LR Tests ──────────────────────────────────────────────

    #[test]
    fn indirect_lr_substituted() {
        // A = B "x"          (id=0, scc=0)
        // B = A "y" | "z"    (id=1, scc=0)
        // SCC: {A, B} — A starts with B, B starts with A (indirect cycle).
        let mut ir = make_ir(vec![
            ("A", IrNode::Epsilon, Some(0), true),
            ("B", IrNode::Epsilon, Some(0), true),
        ]);

        let x1 = lit("x", &mut ir);
        let y1 = lit("y", &mut ir);
        let z1 = lit("z", &mut ir);

        ir.rules[0].body = seq(vec![ref_node(1), x1]); // A = B "x"
        ir.rules[1].body = alt(vec![
            seq(vec![ref_node(0), y1]), // A "y"
            z1,                         // "z"
        ]);

        eliminate_indirect_lr(&mut ir);

        // B should now have A's body substituted for leading Ref(A).
        // B was: A "y" | "z"  →  (B "x") "y" | "z"
        let b_body = &ir.rules[1].body;
        if let IrNode::Alt(branches, _) = b_body {
            assert_eq!(branches.len(), 2, "Expected 2 branches after substitution");
            let first = &branches[0].node;
            // First branch should be Seq containing A's body (B "x") followed by "y".
            if let IrNode::Seq(elems) = first {
                // The substitution of A = (B "x") into (A "y") gives Seq([B, "x", "y"])
                // or Seq([Seq([B, "x"]), "y"]).
                // Our implementation creates Seq([Seq([B, "x"]), "y"]).
                assert!(
                    elems.len() >= 2,
                    "Expected at least 2 elements in substituted seq"
                );
                // The first element should contain a reference to B (rule 1).
                let has_b_ref = contains_ref(&elems[0], 1);
                assert!(
                    has_b_ref,
                    "Expected substituted branch to reference B (rule 1)"
                );
            } else {
                panic!("Expected Seq after substitution, got {:?}", first);
            }
        } else {
            panic!("Expected Alt after substitution, got {:?}", b_body);
        }
    }

    #[test]
    fn indirect_lr_no_multi_sccs_is_noop() {
        // Single-member SCCs should not trigger indirect LR elimination.
        let mut ir = make_ir(vec![("A", IrNode::Epsilon, Some(0), true)]);

        let x = lit("x", &mut ir);
        ir.rules[0].body = alt(vec![seq(vec![ref_node(0), x])]);

        let body_before = ir.rules[0].body.clone();
        eliminate_indirect_lr(&mut ir);
        assert_eq!(
            ir.rules[0].body, body_before,
            "Single-member SCC should be unchanged"
        );
    }

    /// Check if an IrNode tree contains a Ref to the given rule id.
    fn contains_ref(node: &IrNode, target: RuleId) -> bool {
        match node {
            IrNode::Ref(id) => *id == target,
            IrNode::Seq(children) => children.iter().any(|c| contains_ref(c, target)),
            IrNode::Alt(branches, _) => branches.iter().any(|b| contains_ref(&b.node, target)),
            IrNode::Repeat { inner, .. }
            | IrNode::Negate(inner)
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Map { inner, .. } => contains_ref(inner, target),
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                contains_ref(a, target) || contains_ref(b, target)
            }
            _ => false,
        }
    }

    // ── Helper Tests ───────────────────────────────────────────────────

    #[test]
    fn starts_with_ref_checks() {
        assert!(starts_with_ref(&IrNode::Ref(0), 0));
        assert!(!starts_with_ref(&IrNode::Ref(1), 0));
        assert!(starts_with_ref(
            &IrNode::Seq(vec![IrNode::Ref(0), IrNode::Epsilon]),
            0
        ));
        assert!(!starts_with_ref(
            &IrNode::Seq(vec![IrNode::Epsilon, IrNode::Ref(0)]),
            0
        ));
    }

    #[test]
    fn strip_leading_ref_checks() {
        // Ref(0) -> Epsilon
        assert_eq!(strip_leading_ref(&IrNode::Ref(0), 0), Some(IrNode::Epsilon));

        // Ref(1) with target 0 -> None
        assert_eq!(strip_leading_ref(&IrNode::Ref(1), 0), None);

        // Seq([Ref(0), Lit]) -> Lit
        let lit = IrNode::Literal(42);
        let node = IrNode::Seq(vec![IrNode::Ref(0), lit.clone()]);
        assert_eq!(strip_leading_ref(&node, 0), Some(lit));

        // Seq([Ref(0), Lit, Lit]) -> Seq([Lit, Lit])
        let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1), IrNode::Literal(2)]);
        assert_eq!(
            strip_leading_ref(&node, 0),
            Some(IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]))
        );
    }

    #[test]
    fn substitute_single_ref() {
        let node = IrNode::Ref(0);
        let replacement = IrNode::Literal(42);
        assert_eq!(
            substitute_leading_ref(&node, 0, &replacement),
            Some(IrNode::Literal(42))
        );
    }

    #[test]
    fn substitute_seq_leading() {
        let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1)]);
        let replacement = IrNode::Literal(42);
        let result = substitute_leading_ref(&node, 0, &replacement);
        assert_eq!(
            result,
            Some(IrNode::Seq(vec![IrNode::Literal(42), IrNode::Literal(1)]))
        );
    }

    #[test]
    fn substitute_with_alt_replacement() {
        // Seq([Ref(0), Lit(1)]) with replacement Alt([Lit(2), Lit(3)])
        // -> Alt([Seq([Lit(2), Lit(1)]), Seq([Lit(3), Lit(1)])])
        let node = IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1)]);
        let replacement = IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Literal(2),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Literal(3),
                    first_set: None,
                },
            ],
            None,
        );
        let result = substitute_leading_ref(&node, 0, &replacement);
        if let Some(IrNode::Alt(branches, _)) = result {
            assert_eq!(branches.len(), 2);
        } else {
            panic!("Expected Alt result, got {:?}", result);
        }
    }

    #[test]
    fn substitute_no_match() {
        let node = IrNode::Seq(vec![IrNode::Ref(1), IrNode::Literal(1)]);
        assert!(substitute_leading_ref(&node, 0, &IrNode::Literal(42)).is_none());
    }
}
