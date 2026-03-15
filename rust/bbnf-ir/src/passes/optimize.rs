//! IR optimization passes: epsilon elimination and literal merging.
//!
//! These are lightweight tree-rewrite passes that reduce node count and match operations.
//! Run early in the pipeline (before dispatch table generation).

use crate::{GrammarIR, IrNode};

// ── Epsilon Elimination ─────────────────────────────────────────────────────

/// Remove `Epsilon` nodes from sequences and flatten trivial wrappers.
///
/// Rewrites:
/// - `Seq([..., Epsilon, ...])` → `Seq([...])` (remove epsilon elements)
/// - `Seq([single])` → `single` (unwrap singleton sequences)
/// - `Alt([single], _)` → `single.node` (unwrap singleton alternations)
pub fn eliminate_epsilon(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        rule.body = elim_epsilon(std::mem::replace(&mut rule.body, IrNode::Epsilon));
    }
}

fn elim_epsilon(node: IrNode) -> IrNode {
    match node {
        IrNode::Seq(children) => {
            let cleaned: Vec<IrNode> = children
                .into_iter()
                .map(elim_epsilon)
                .filter(|n| !matches!(n, IrNode::Epsilon))
                .collect();
            match cleaned.len() {
                0 => IrNode::Epsilon,
                1 => cleaned.into_iter().next().unwrap(),
                _ => IrNode::Seq(cleaned),
            }
        }
        IrNode::Alt(branches, dispatch) => {
            let cleaned: Vec<_> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = elim_epsilon(b.node);
                    b
                })
                .collect();
            if cleaned.len() == 1 && dispatch.is_none() {
                cleaned.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(cleaned, dispatch)
            }
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(elim_epsilon(*inner)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(elim_epsilon(*a)),
            Box::new(elim_epsilon(*b)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(elim_epsilon(*a)),
            Box::new(elim_epsilon(*b)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(elim_epsilon(*a)),
            Box::new(elim_epsilon(*b)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(elim_epsilon(*inner))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(elim_epsilon(*inner)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(elim_epsilon(*inner)),
            fn_id,
        },
        // Leaves are unchanged.
        other => other,
    }
}

// ── Literal Merging ─────────────────────────────────────────────────────────

/// Merge adjacent `Literal` nodes in sequences.
///
/// Rewrites `Seq([Lit("a"), Lit("b"), ...])` → `Seq([Lit("ab"), ...])`.
/// Reduces the number of match operations in the interpreter.
pub fn merge_literals(ir: &mut GrammarIR) {
    for rule in &mut ir.rules {
        rule.body = merge_lits(std::mem::replace(&mut rule.body, IrNode::Epsilon), &mut ir.strings);
    }
}

fn merge_lits(node: IrNode, strings: &mut Vec<String>) -> IrNode {
    match node {
        IrNode::Seq(children) => {
            let mut merged: Vec<IrNode> = Vec::with_capacity(children.len());
            let mut pending_lit: Option<String> = None;

            for child in children {
                let child = merge_lits(child, strings);
                if let IrNode::Literal(sid) = &child {
                    let s = strings[*sid as usize].clone();
                    if let Some(ref mut acc) = pending_lit {
                        acc.push_str(&s);
                        continue;
                    } else {
                        pending_lit = Some(s);
                        continue;
                    }
                }
                // Flush pending literal.
                if let Some(acc) = pending_lit.take() {
                    let sid = strings.len() as u32;
                    strings.push(acc);
                    merged.push(IrNode::Literal(sid));
                }
                merged.push(child);
            }
            // Flush trailing pending.
            if let Some(acc) = pending_lit.take() {
                let sid = strings.len() as u32;
                strings.push(acc);
                merged.push(IrNode::Literal(sid));
            }

            match merged.len() {
                0 => IrNode::Epsilon,
                1 => merged.into_iter().next().unwrap(),
                _ => IrNode::Seq(merged),
            }
        }
        IrNode::Alt(branches, dispatch) => {
            let cleaned: Vec<_> = branches
                .into_iter()
                .map(|mut b| {
                    b.node = merge_lits(b.node, strings);
                    b
                })
                .collect();
            IrNode::Alt(cleaned, dispatch)
        }
        IrNode::Repeat { inner, lo, hi } => IrNode::Repeat {
            inner: Box::new(merge_lits(*inner, strings)),
            lo,
            hi,
        },
        IrNode::Skip(a, b) => IrNode::Skip(
            Box::new(merge_lits(*a, strings)),
            Box::new(merge_lits(*b, strings)),
        ),
        IrNode::Next(a, b) => IrNode::Next(
            Box::new(merge_lits(*a, strings)),
            Box::new(merge_lits(*b, strings)),
        ),
        IrNode::Minus(a, b) => IrNode::Minus(
            Box::new(merge_lits(*a, strings)),
            Box::new(merge_lits(*b, strings)),
        ),
        IrNode::Negate(inner) => IrNode::Negate(Box::new(merge_lits(*inner, strings))),
        IrNode::OptionalWhitespace(inner) => {
            IrNode::OptionalWhitespace(Box::new(merge_lits(*inner, strings)))
        }
        IrNode::Map { inner, fn_id } => IrNode::Map {
            inner: Box::new(merge_lits(*inner, strings)),
            fn_id,
        },
        other => other,
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::{AltBranch, IrRule, RuleMeta};

    fn make_ir(body: IrNode, strings: Vec<String>) -> GrammarIR {
        GrammarIR {
            entry: 0,
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body,
                meta: RuleMeta::default(),
            }],
            strings,
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        }
    }

    #[test]
    fn epsilon_removed_from_seq() {
        let mut ir = make_ir(
            IrNode::Seq(vec![
                IrNode::Literal(1),
                IrNode::Epsilon,
                IrNode::Literal(2),
            ]),
            vec!["start".into(), "a".into(), "b".into()],
        );
        eliminate_epsilon(&mut ir);
        match &ir.rules[0].body {
            IrNode::Seq(children) => {
                assert_eq!(children.len(), 2);
                assert!(matches!(children[0], IrNode::Literal(1)));
                assert!(matches!(children[1], IrNode::Literal(2)));
            }
            other => panic!("expected Seq, got {:?}", other),
        }
    }

    #[test]
    fn singleton_seq_unwrapped() {
        let mut ir = make_ir(
            IrNode::Seq(vec![IrNode::Epsilon, IrNode::Literal(1)]),
            vec!["start".into(), "a".into()],
        );
        eliminate_epsilon(&mut ir);
        assert!(matches!(ir.rules[0].body, IrNode::Literal(1)));
    }

    #[test]
    fn all_epsilon_seq_becomes_epsilon() {
        let mut ir = make_ir(
            IrNode::Seq(vec![IrNode::Epsilon, IrNode::Epsilon]),
            vec!["start".into()],
        );
        eliminate_epsilon(&mut ir);
        assert!(matches!(ir.rules[0].body, IrNode::Epsilon));
    }

    #[test]
    fn singleton_alt_unwrapped() {
        let mut ir = make_ir(
            IrNode::Alt(
                vec![AltBranch {
                    node: IrNode::Literal(1),
                    first_set: None,
                }],
                None,
            ),
            vec!["start".into(), "a".into()],
        );
        eliminate_epsilon(&mut ir);
        assert!(matches!(ir.rules[0].body, IrNode::Literal(1)));
    }

    #[test]
    fn adjacent_literals_merged() {
        let mut ir = make_ir(
            IrNode::Seq(vec![
                IrNode::Literal(1),
                IrNode::Literal(2),
                IrNode::Literal(3),
            ]),
            vec!["start".into(), "a".into(), "b".into(), "c".into()],
        );
        merge_literals(&mut ir);
        // Should become a single literal "abc".
        match &ir.rules[0].body {
            IrNode::Literal(sid) => {
                assert_eq!(ir.strings[*sid as usize], "abc");
            }
            other => panic!("expected merged Literal, got {:?}", other),
        }
    }

    #[test]
    fn non_adjacent_literals_not_merged() {
        let mut ir = make_ir(
            IrNode::Seq(vec![
                IrNode::Literal(1),
                IrNode::Regex(2),
                IrNode::Literal(3),
            ]),
            vec!["start".into(), "a".into(), "\\d+".into(), "b".into()],
        );
        merge_literals(&mut ir);
        match &ir.rules[0].body {
            IrNode::Seq(children) => assert_eq!(children.len(), 3),
            other => panic!("expected Seq of 3, got {:?}", other),
        }
    }

    #[test]
    fn partial_merge() {
        // Seq([Lit("a"), Lit("b"), Regex, Lit("c"), Lit("d")])
        // → Seq([Lit("ab"), Regex, Lit("cd")])
        let mut ir = make_ir(
            IrNode::Seq(vec![
                IrNode::Literal(1),
                IrNode::Literal(2),
                IrNode::Regex(3),
                IrNode::Literal(4),
                IrNode::Literal(5),
            ]),
            vec![
                "start".into(),
                "a".into(),
                "b".into(),
                "\\d+".into(),
                "c".into(),
                "d".into(),
            ],
        );
        merge_literals(&mut ir);
        match &ir.rules[0].body {
            IrNode::Seq(children) => {
                assert_eq!(children.len(), 3);
                if let IrNode::Literal(sid) = &children[0] {
                    assert_eq!(ir.strings[*sid as usize], "ab");
                }
                assert!(matches!(children[1], IrNode::Regex(3)));
                if let IrNode::Literal(sid) = &children[2] {
                    assert_eq!(ir.strings[*sid as usize], "cd");
                }
            }
            other => panic!("expected Seq of 3, got {:?}", other),
        }
    }
}
