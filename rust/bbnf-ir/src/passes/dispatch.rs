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

use rayon::prelude::*;

use crate::{AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode, regex_first};

/// Generate dispatch tables for all eligible Alt nodes in the IR.
///
/// Walks the entire IR tree (not just rule-level bodies), annotating each
/// `Alt` node whose branches have pairwise disjoint FIRST sets with an
/// `AltDispatch` table for O(1) branch selection.
///
/// Uses contextual FOLLOW sets for nullable branch dispatch: within a Seq,
/// a nested Alt uses `FIRST(suffix)` (the first set of the remaining sequence
/// elements), not the rule-level FOLLOW. This prevents incorrect dispatch
/// table entries for nullable branches in non-tail positions.
pub fn generate_dispatch_tables(ir: &mut GrammarIR) {
    // Clone follow sets, strings, and rules metadata to avoid borrow conflicts.
    let follow_sets = ir.follow_sets.clone();
    let strings = ir.strings.clone();
    let rule_metas: Vec<(CharSet128, bool)> = ir.rules.iter()
        .map(|r| (r.meta.first_set.clone(), r.meta.nullable))
        .collect();

    if ir.rules.len() >= 16 {
        ir.rules.par_iter_mut().for_each(|rule| {
            let follow = follow_sets.get(&rule.id);
            annotate_node(&mut rule.body, follow, &rule_metas, &strings);
            if let Some(ref mut recover) = rule.meta.recover {
                annotate_node(recover, follow, &rule_metas, &strings);
            }
        });
    } else {
        for rule in &mut ir.rules {
            let follow = follow_sets.get(&rule.id);
            annotate_node(&mut rule.body, follow, &rule_metas, &strings);
            if let Some(ref mut recover) = rule.meta.recover {
                annotate_node(recover, follow, &rule_metas, &strings);
            }
        }
    }
}

/// Compute the FIRST set of an IrNode.
///
/// Returns `Some(charset)` for non-nullable nodes, `None` for nullable ones.
/// `rule_metas` provides pre-computed (first_set, nullable) for rule references.
fn node_first_set(node: &IrNode, rule_metas: &[(CharSet128, bool)], strings: &[String]) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if let Some(b) = s.bytes().next() {
                let mut cs = CharSet128::new();
                cs.add(b);
                Some(cs)
            } else {
                None // empty literal = nullable
            }
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            regex_first::regex_first_chars(pattern)
                .filter(|cs| !cs.is_empty())
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            if nullable {
                None
            } else {
                Some(first_set.clone())
            }
        }
        IrNode::Seq(children) => {
            let mut combined = CharSet128::new();
            for child in children {
                if let Some(fs) = node_first_set(child, rule_metas, strings) {
                    combined.union(&fs);
                    return Some(combined); // non-nullable child stops propagation
                }
                // Child is nullable — continue to include next child's FIRST
                if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
                    combined.union(&fs);
                }
            }
            None // all children nullable
        }
        IrNode::Alt(branches, _) => {
            let mut combined = CharSet128::new();
            for b in branches {
                if let Some(ref fs) = b.first_set {
                    combined.union(fs);
                } else {
                    return None; // nullable branch
                }
            }
            Some(combined)
        }
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon => None,
        IrNode::Repeat { inner, .. } => node_first_set(inner, rule_metas, strings),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            node_first_set(inner, rule_metas, strings)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Minus(a, _) => node_first_set(a, rule_metas, strings),
        IrNode::Negate(_) => None, // zero-width, nullable
        IrNode::TokenDispatch { token, .. } => node_first_set(token, rule_metas, strings),
    }
}

/// Get the FIRST set contribution from a nullable node (the non-None part).
fn node_first_set_nullable_part(node: &IrNode, rule_metas: &[(CharSet128, bool)]) -> Option<CharSet128> {
    match node {
        IrNode::Ref(rule_id) => {
            let (ref first_set, _) = rule_metas[*rule_id as usize];
            if first_set.is_empty() { None } else { Some(first_set.clone()) }
        }
        IrNode::Repeat { inner, .. } => node_first_set_nullable_part(inner, rule_metas),
        _ => None,
    }
}

/// Compute the contextual FOLLOW for a node at position `from` within a Seq.
///
/// Returns `FIRST(children[from..])` unioned with `rule_follow` if the suffix
/// is entirely nullable.
fn suffix_follow(
    children: &[IrNode],
    from: usize,
    rule_follow: Option<&CharSet128>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    let mut combined = CharSet128::new();
    for child in &children[from..] {
        if let Some(fs) = node_first_set(child, rule_metas, strings) {
            combined.union(&fs);
            // Non-nullable child — suffix can't produce anything beyond this.
            return Some(combined);
        }
        // Nullable child — include its FIRST and continue.
        if let Some(fs) = node_first_set_nullable_part(child, rule_metas) {
            combined.union(&fs);
        }
    }
    // All suffix children are nullable — include rule FOLLOW.
    if let Some(follow) = rule_follow {
        combined.union(follow);
    }
    if combined.is_empty() { None } else { Some(combined) }
}

/// Recursively walk an IrNode tree and annotate eligible Alt nodes.
///
/// `containing_follow` is the contextual FOLLOW set — for top-level nodes this
/// is `FOLLOW(rule)`, but for nodes inside a Seq it is `FIRST(suffix)` (the
/// first set of the remaining sequence elements).
fn annotate_node(
    node: &mut IrNode,
    containing_follow: Option<&CharSet128>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            for branch in branches.iter_mut() {
                annotate_node(&mut branch.node, containing_follow, rule_metas, strings);
            }

            // Skip if already annotated.
            if dispatch.is_some() {
                return;
            }

            // Recompute FIRST sets for branches that lack them (after inlining/fusing,
            // newly created AltBranch nodes may have first_set=None).
            for branch in branches.iter_mut() {
                if branch.first_set.is_none() {
                    branch.first_set = node_first_set(&branch.node, rule_metas, strings);
                }
            }

            // Try to build a full dispatch table (all branches disjoint).
            if let Some(table) = try_build_dispatch(branches, containing_follow) {
                *dispatch = Some(table);
            }
        }
        IrNode::Seq(children) => {
            // For each child in the Seq, compute the contextual follow from
            // FIRST(suffix) rather than blindly passing the rule-level FOLLOW.
            for i in 0..children.len() {
                let child_follow = if i + 1 < children.len() {
                    suffix_follow(children, i + 1, containing_follow, rule_metas, strings)
                } else {
                    containing_follow.cloned()
                };
                annotate_node(&mut children[i], child_follow.as_ref().or(containing_follow), rule_metas, strings);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => {
            annotate_node(inner, containing_follow, rule_metas, strings);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            annotate_node(a, containing_follow, rule_metas, strings);
            annotate_node(b, containing_follow, rule_metas, strings);
        }
        IrNode::TokenDispatch { token, arms, fallback } => {
            annotate_node(token, containing_follow, rule_metas, strings);
            for arm in arms {
                annotate_node(&mut arm.continuation, containing_follow, rule_metas, strings);
            }
            annotate_node(fallback, containing_follow, rule_metas, strings);
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
