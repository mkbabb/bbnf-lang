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
