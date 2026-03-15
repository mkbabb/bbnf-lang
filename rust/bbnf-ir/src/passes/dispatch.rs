//! Pass 2.6: Dispatch table generation from IR FIRST sets.
//!
//! Walks the entire IR tree and annotates any `Alt` node with a dispatch table
//! when all branches have pairwise disjoint FIRST sets.
//!
//! When FOLLOW sets are available and an Alt branch is nullable, the pass uses
//! `FOLLOW(containing_rule)` to include the nullable branch in the dispatch
//! table — if `next_char ∈ FOLLOW(rule)` and FOLLOW is disjoint from other
//! branches' FIRST sets, the nullable branch gets O(1) dispatch instead of
//! falling through as a linear fallback.

use crate::{AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode};

/// Generate dispatch tables for all eligible Alt nodes in the IR.
///
/// Walks the entire IR tree (not just rule-level bodies), annotating each
/// `Alt` node whose branches have pairwise disjoint FIRST sets with an
/// `AltDispatch` table for O(1) branch selection.
///
/// Uses `ir.follow_sets` (when populated) to handle nullable branches:
/// a nullable branch is dispatched via `FOLLOW(rule)` when disjoint from
/// all other branches' FIRST sets.
pub fn generate_dispatch_tables(ir: &mut GrammarIR) {
    // Clone follow sets to avoid borrow conflict with mutable rule iteration.
    let follow_sets = ir.follow_sets.clone();

    for rule in &mut ir.rules {
        let follow = follow_sets.get(&rule.id);
        annotate_node(&mut rule.body, follow);
        if let Some(ref mut recover) = rule.meta.recover {
            annotate_node(recover, follow);
        }
    }
}

/// Recursively walk an IrNode tree and annotate eligible Alt nodes.
///
/// `containing_follow` is the FOLLOW set of the rule that contains this node,
/// used to assign dispatch entries to nullable branches.
fn annotate_node(node: &mut IrNode, containing_follow: Option<&CharSet128>) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            for branch in branches.iter_mut() {
                annotate_node(&mut branch.node, containing_follow);
            }

            // Skip if already annotated.
            if dispatch.is_some() {
                return;
            }

            // Try to build a dispatch table (with nullable branch support).
            if let Some(table) = try_build_dispatch(branches, containing_follow) {
                *dispatch = Some(table);
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                annotate_node(child, containing_follow);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            annotate_node(inner, containing_follow);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            annotate_node(a, containing_follow);
            annotate_node(b, containing_follow);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

/// Try to build a dispatch table for an alternation's branches.
///
/// Returns `Some(AltDispatch)` if all branches have pairwise disjoint effective
/// dispatch sets. A branch's effective set is its FIRST set, or — when the branch
/// is nullable and `containing_follow` is available — the FOLLOW set of the
/// containing rule.
fn try_build_dispatch(
    branches: &[AltBranch],
    containing_follow: Option<&CharSet128>,
) -> Option<AltDispatch> {
    // Limit to 127 branches (u8 range with 255 as sentinel).
    if branches.len() > 127 {
        return None;
    }

    // At most one nullable branch can be handled via FOLLOW dispatch.
    let mut nullable_idx: Option<usize> = None;

    // Build effective dispatch sets per branch.
    let mut effective_sets: Vec<CharSet128> = Vec::with_capacity(branches.len());

    for (i, branch) in branches.iter().enumerate() {
        if let Some(ref first) = branch.first_set {
            effective_sets.push(first.clone());
        } else if nullable_idx.is_none() {
            // Nullable branch — use FOLLOW(rule) if available.
            if let Some(follow) = containing_follow {
                if !follow.is_empty() {
                    nullable_idx = Some(i);
                    effective_sets.push(follow.clone());
                } else {
                    // Empty FOLLOW — can't dispatch.
                    return None;
                }
            } else {
                // No FOLLOW sets available — can't dispatch nullable branch.
                return None;
            }
        } else {
            // Multiple nullable branches — can't dispatch.
            return None;
        }
    }

    if effective_sets.len() != branches.len() {
        return None;
    }

    // Check pairwise disjointness.
    for i in 0..effective_sets.len() {
        for j in (i + 1)..effective_sets.len() {
            if !effective_sets[i].is_disjoint(&effective_sets[j]) {
                return None;
            }
        }
    }

    // Build dispatch table.
    let mut table = vec![255u8; 128];
    for (idx, cs) in effective_sets.iter().enumerate() {
        for code in cs.iter() {
            table[code as usize] = idx as u8;
        }
    }

    Some(AltDispatch { table })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::{AltBranch, IrRule, RuleMeta};

    #[test]
    fn dispatch_for_disjoint_branches() {
        let mut first_a = CharSet128::new();
        first_a.add(b't'); // "true"
        let mut first_b = CharSet128::new();
        first_b.add(b'f'); // "false"
        let mut first_c = CharSet128::new();
        first_c.add(b'n'); // "null"

        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Alt(vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                    AltBranch {
                        node: IrNode::Literal(3),
                        first_set: Some(first_c),
                    },
                ], None),
                meta: RuleMeta::default(),
            }],
            entry: 0,
            strings: vec!["value".into(), "true".into(), "false".into(), "null".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        generate_dispatch_tables(&mut ir);

        match &ir.rules[0].body {
            IrNode::Alt(_, Some(dispatch)) => {
                assert_eq!(dispatch.table[b't' as usize], 0);
                assert_eq!(dispatch.table[b'f' as usize], 1);
                assert_eq!(dispatch.table[b'n' as usize], 2);
                assert_eq!(dispatch.table[b'a' as usize], 255);
            }
            other => panic!("expected Alt with dispatch, got {:?}", other),
        }
    }

    #[test]
    fn no_dispatch_for_overlapping_branches() {
        let mut first_a = CharSet128::new();
        first_a.add(b'"');
        let mut first_b = CharSet128::new();
        first_b.add(b'"'); // Overlaps!

        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Alt(vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                ], None),
                meta: RuleMeta::default(),
            }],
            entry: 0,
            strings: vec!["rule".into(), "a".into(), "b".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        generate_dispatch_tables(&mut ir);
        match &ir.rules[0].body {
            IrNode::Alt(_, dispatch) => assert!(dispatch.is_none()),
            other => panic!("expected Alt, got {:?}", other),
        }
    }

    #[test]
    fn dispatch_for_nested_alt() {
        // Alt nested inside Seq should also get annotated.
        let mut first_a = CharSet128::new();
        first_a.add(b'a');
        let mut first_b = CharSet128::new();
        first_b.add(b'b');

        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Alt(vec![
                        AltBranch { node: IrNode::Literal(1), first_set: Some(first_a) },
                        AltBranch { node: IrNode::Literal(2), first_set: Some(first_b) },
                    ], None),
                    IrNode::Literal(3),
                ]),
                meta: RuleMeta::default(),
            }],
            entry: 0,
            strings: vec!["rule".into(), "a".into(), "b".into(), "end".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        generate_dispatch_tables(&mut ir);

        // The nested Alt should have been annotated.
        match &ir.rules[0].body {
            IrNode::Seq(children) => match &children[0] {
                IrNode::Alt(_, dispatch) => assert!(dispatch.is_some()),
                other => panic!("expected Alt, got {:?}", other),
            },
            other => panic!("expected Seq, got {:?}", other),
        }
    }

    #[test]
    fn dispatch_with_nullable_branch_via_follow() {
        // Grammar: rule = "x" | epsilon ;  FOLLOW(rule) = {';'}
        // Branch 0: FIRST = {'x'}, Branch 1: nullable (epsilon).
        // FOLLOW = {';'} is disjoint from {'x'}, so dispatch should work.
        let mut first_a = CharSet128::new();
        first_a.add(b'x');

        let mut follow_rule = CharSet128::new();
        follow_rule.add(b';');

        let mut follow_sets = HashMap::new();
        follow_sets.insert(0u32, follow_rule);

        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Alt(vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Epsilon,
                        first_set: None, // Nullable branch.
                    },
                ], None),
                meta: RuleMeta::default(),
            }],
            entry: 0,
            strings: vec!["rule".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets,
        };

        generate_dispatch_tables(&mut ir);

        match &ir.rules[0].body {
            IrNode::Alt(_, Some(dispatch)) => {
                // 'x' → branch 0.
                assert_eq!(dispatch.table[b'x' as usize], 0);
                // ';' ∈ FOLLOW → branch 1 (nullable).
                assert_eq!(dispatch.table[b';' as usize], 1);
                // Other bytes → no match.
                assert_eq!(dispatch.table[b'a' as usize], 255);
            }
            other => panic!("expected Alt with dispatch, got {:?}", other),
        }
    }

    #[test]
    fn no_dispatch_when_nullable_overlaps_follow() {
        // Branch 0: FIRST = {'x'}, Branch 1: nullable.
        // FOLLOW(rule) = {'x'} — overlaps with branch 0, so no dispatch.
        let mut first_a = CharSet128::new();
        first_a.add(b'x');

        let mut follow_rule = CharSet128::new();
        follow_rule.add(b'x'); // Overlaps!

        let mut follow_sets = HashMap::new();
        follow_sets.insert(0u32, follow_rule);

        let mut ir = GrammarIR {
            rules: vec![IrRule {
                id: 0,
                name: 0,
                body: IrNode::Alt(vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Epsilon,
                        first_set: None,
                    },
                ], None),
                meta: RuleMeta::default(),
            }],
            entry: 0,
            strings: vec!["rule".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets,
        };

        generate_dispatch_tables(&mut ir);

        match &ir.rules[0].body {
            IrNode::Alt(_, dispatch) => assert!(dispatch.is_none()),
            other => panic!("expected Alt, got {:?}", other),
        }
    }
}
