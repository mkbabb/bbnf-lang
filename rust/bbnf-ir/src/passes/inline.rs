//! Pass: Acyclic rule inlining.
//!
//! Replaces `Ref(id)` with the rule body when the target rule is small,
//! acyclic, and not the entry point. Reduces call overhead in the interpreter
//! and exposes further optimization opportunities (literal merging, dispatch).

use crate::{GrammarIR, IrNode, RuleId};

/// Maximum node count for a rule to be considered inlinable.
const INLINE_THRESHOLD: usize = 3;

/// Inline small acyclic rules at their call sites.
///
/// A rule is inlinable when:
/// 1. It is not cyclic (no SCC membership)
/// 2. It is not the grammar entry point
/// 3. Its body has at most `INLINE_THRESHOLD` nodes
///
/// After inlining, the original rule remains (it may be referenced externally).
/// Run `prune_unreachable` afterward to clean up dead rules.
pub fn inline_acyclic(ir: &mut GrammarIR) {
    // Identify inlinable rules.
    let inlinable: Vec<(RuleId, IrNode)> = ir
        .rules
        .iter()
        .filter(|r| {
            r.id != ir.entry
                && !r.meta.is_cyclic
                && r.meta.scc_id.is_none()
                && node_count(&r.body) <= INLINE_THRESHOLD
        })
        .map(|r| (r.id, r.body.clone()))
        .collect();

    if inlinable.is_empty() {
        return;
    }

    // Build a lookup: rule_id → body (for inlinable rules only).
    let max_id = inlinable.iter().map(|(id, _)| *id).max().unwrap_or(0) as usize;
    let mut bodies: Vec<Option<IrNode>> = vec![None; max_id + 1];
    for (id, body) in &inlinable {
        bodies[*id as usize] = Some(body.clone());
    }

    // Rewrite all rule bodies.
    for rule in &mut ir.rules {
        rule.body = inline_refs(
            std::mem::replace(&mut rule.body, IrNode::Epsilon),
            &bodies,
        );
    }
}

/// Count the number of nodes in an IR tree (for threshold check).
fn node_count(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 1,
        IrNode::Seq(children) => 1 + children.iter().map(node_count).sum::<usize>(),
        IrNode::Alt(branches, _) => {
            1 + branches.iter().map(|b| node_count(&b.node)).sum::<usize>()
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => 1 + node_count(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            1 + node_count(a) + node_count(b)
        }
    }
}

/// Recursively replace `Ref(id)` with the inlined body where applicable.
fn inline_refs(node: IrNode, bodies: &[Option<IrNode>]) -> IrNode {
    match node {
        IrNode::Ref(id) => {
            if let Some(Some(body)) = bodies.get(id as usize) {
                body.clone()
            } else {
                IrNode::Ref(id)
            }
        }
        IrNode::Seq(children) => {
            IrNode::Seq(children.into_iter().map(|c| inline_refs(c, bodies)).collect())
        }
        IrNode::Alt(branches, dispatch) => {
            let branches = branches
                .into_iter()
                .map(|mut b| {
                    b.node = inline_refs(b.node, bodies);
                    b
                })
                .collect();
            IrNode::Alt(branches, dispatch)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(inline_refs(*inner, bodies)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(inline_refs(*a, bodies)),
            Box::new(inline_refs(*b, bodies)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(inline_refs(*inner, bodies))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(inline_refs(*inner, bodies)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(inline_refs(*inner, bodies)),
            fn_id,
        },
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::{IrRule, RuleMeta};

    fn make_ir(rules: Vec<IrRule>, entry: RuleId) -> GrammarIR {
        GrammarIR {
            rules,
            entry,
            strings: vec!["entry".into(), "small".into(), "a".into(), "b".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        }
    }

    #[test]
    fn small_acyclic_inlined() {
        // Rule 0 (entry): Ref(1)
        // Rule 1 (small): Literal(2)  — 1 node, inlinable
        let mut ir = make_ir(
            vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta::default(),
                },
            ],
            0,
        );

        inline_acyclic(&mut ir);
        // Rule 0 should now have Literal(2) instead of Ref(1).
        assert_eq!(ir.rules[0].body, IrNode::Literal(2));
    }

    #[test]
    fn cyclic_not_inlined() {
        let mut ir = make_ir(
            vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(2),
                    meta: RuleMeta {
                        is_cyclic: true,
                        scc_id: Some(0),
                        ..Default::default()
                    },
                },
            ],
            0,
        );

        inline_acyclic(&mut ir);
        // Rule 0 should still be Ref(1) — rule 1 is cyclic.
        assert_eq!(ir.rules[0].body, IrNode::Ref(1));
    }

    #[test]
    fn entry_point_not_inlined() {
        // Entry rule is small but should not be inlined into itself.
        let mut ir = make_ir(
            vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
            }],
            0,
        );

        inline_acyclic(&mut ir);
        assert_eq!(ir.rules[0].body, IrNode::Literal(2));
    }

    #[test]
    fn large_rule_not_inlined() {
        // Rule 1 has 4 nodes (Seq + 3 Literals) — over threshold.
        let mut ir = make_ir(
            vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta::default(),
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Seq(vec![
                        IrNode::Literal(2),
                        IrNode::Literal(3),
                        IrNode::Literal(2),
                    ]),
                    meta: RuleMeta::default(),
                },
            ],
            0,
        );

        inline_acyclic(&mut ir);
        // Rule 0 should still be Ref(1) — rule 1 is too large.
        assert_eq!(ir.rules[0].body, IrNode::Ref(1));
    }
}
