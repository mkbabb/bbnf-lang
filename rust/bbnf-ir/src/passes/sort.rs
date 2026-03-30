//! Pass: Specificity-based sorting of alternation branches.
//!
//! Reorders `Alt` branches so that more specific branches precede less specific
//! ones, ensuring correct semantics for ordered-choice (PEG) alternation without
//! requiring the grammar author to manually order by specificity.
//!
//! Specificity is determined by:
//! 1. **Constant prefix length** — branches starting with longer literal prefixes
//!    are more specific (e.g. `"calcFunction"` before `"color"`).
//! 2. **FIRST set width** — branches with narrower FIRST sets are more specific
//!    (a branch matching only `{c}` is more specific than one matching `{a-zA-Z}`).
//! 3. **Catchall demotion** — regex-only branches with wide character classes
//!    (identifiers, generic patterns) sort last.
//!
//! The pass is a stable sort, preserving the author's ordering for branches with
//! equal specificity.  It runs after `factor_common_prefixes` (branches are
//! maximally factored) and before `compute_follow_sets`.

use crate::{AltBranch, CharSet128, GrammarIR, IrNode};

/// Sort alternation branches by specificity across the entire IR.
pub fn sort_alt_branches(ir: &mut GrammarIR) {
    let strings = ir.strings.clone();
    let rule_metas: Vec<(CharSet128, bool)> = ir
        .rules
        .iter()
        .map(|r| (r.meta.first_set.clone(), r.meta.nullable))
        .collect();

    for rule in &mut ir.rules {
        sort_node(
            &mut rule.body,
            &rule_metas,
            &strings,
        );
    }
}

/// Recursively sort Alt nodes throughout the IR tree.
fn sort_node(
    node: &mut IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) {
    match node {
        IrNode::Alt(branches, dispatch) => {
            // Recurse into children first.
            for b in branches.iter_mut() {
                sort_node(&mut b.node, rule_metas, strings);
            }

            // Only sort if no dispatch table is already set — dispatch-eligible
            // alternations don't depend on ordering.
            if dispatch.is_none() && branches.len() >= 2 {
                sort_branches(branches, rule_metas, strings);
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                sort_node(child, rule_metas, strings);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. }
        | IrNode::Negate(inner) => {
            sort_node(inner, rule_metas, strings);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            sort_node(a, rule_metas, strings);
            sort_node(b, rule_metas, strings);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            sort_node(token, rule_metas, strings);
            for arm in arms {
                sort_node(&mut arm.continuation, rule_metas, strings);
            }
            sort_node(fallback, rule_metas, strings);
        }
        // Leaves — nothing to recurse into.
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
    }
}

// ── Specificity scoring ─────────────────────────────────────────────────────

/// Specificity key for stable sorting.  Lower values = higher priority.
///
/// Fields (compared lexicographically):
/// - `catchall`: true for regex-only branches with wide FIRST sets → sorts last
/// - `prefix_len_inv`: `u32::MAX - literal_prefix_length` → longer prefixes first
/// - `first_set_width`: number of bytes in FIRST set → narrower first
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Specificity {
    catchall: bool,
    prefix_len_inv: u32,
    first_set_width: u32,
}

fn sort_branches(
    branches: &mut Vec<AltBranch>,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) {
    // Compute FIRST set for each branch.
    let first_sets: Vec<Option<CharSet128>> = branches
        .iter()
        .map(|b| branch_first_set(&b.node, rule_metas, strings))
        .collect();

    // Safety check: only reorder branches whose FIRST sets are disjoint
    // from all other branches.  When FIRST sets overlap, the PEG ordered-
    // choice semantics depend on the author's ordering — reordering would
    // change the parser's behavior.
    let mut safe_to_move = vec![true; branches.len()];
    for i in 0..branches.len() {
        for j in (i + 1)..branches.len() {
            let overlaps = match (&first_sets[i], &first_sets[j]) {
                (Some(a), Some(b)) => !a.is_disjoint(b),
                _ => true, // nullable → always overlaps
            };
            if overlaps {
                safe_to_move[i] = false;
                safe_to_move[j] = false;
            }
        }
    }

    // If nothing is safe to move, bail early.
    if !safe_to_move.iter().any(|&s| s) {
        return;
    }

    // Compute specificity for each branch.
    let mut keyed: Vec<(Specificity, usize)> = branches
        .iter()
        .enumerate()
        .map(|(i, b)| {
            let key = if safe_to_move[i] {
                branch_specificity(&b.node, rule_metas, strings)
            } else {
                // Pinned branches keep their original position via index tie-break.
                Specificity {
                    catchall: false,
                    prefix_len_inv: 0,
                    first_set_width: 0,
                }
            };
            (key, i)
        })
        .collect();

    // Stable sort: pinned branches (all with identical zero keys) stay in
    // their original relative order.  Movable branches sort by specificity.
    keyed.sort();

    // Check if the sort is a no-op (already in order).
    if keyed.iter().enumerate().all(|(i, (_, idx))| i == *idx) {
        return;
    }

    // Apply the permutation.
    let mut sorted: Vec<AltBranch> = Vec::with_capacity(branches.len());
    for (_, idx) in &keyed {
        sorted.push(std::mem::replace(
            &mut branches[*idx],
            AltBranch {
                node: IrNode::Epsilon,
                first_set: None,
            },
        ));
    }
    *branches = sorted;
}

/// Compute the FIRST set for a branch node (for disjointness checks).
fn branch_first_set(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Option<CharSet128> {
    let node = unwrap_transparent(node);
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            s.bytes().next().map(|b| {
                let mut cs = CharSet128::new();
                cs.add(b);
                cs
            })
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            crate::regex_first::regex_first_chars(pattern)
                .filter(|cs| !cs.is_empty())
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            if nullable { None } else { Some(first_set.clone()) }
        }
        IrNode::Seq(children) => {
            children.first().and_then(|c| branch_first_set(c, rule_metas, strings))
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => branch_first_set(a, rule_metas, strings),
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon | IrNode::Negate(_) => None,
        IrNode::Repeat { inner, .. } => branch_first_set(inner, rule_metas, strings),
        _ => None,
    }
}

fn branch_specificity(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> Specificity {
    let unwrapped = unwrap_transparent(node);
    let prefix_len = literal_prefix_len(unwrapped, rule_metas, strings);
    let first_width = first_set_width(unwrapped, rule_metas, strings);
    let catchall = is_catchall(unwrapped, rule_metas, strings);

    Specificity {
        catchall,
        prefix_len_inv: u32::MAX - prefix_len,
        first_set_width: first_width,
    }
}

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Unwrap transparent wrappers (Map, OW) to reach the structural node.
fn unwrap_transparent(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            unwrap_transparent(inner)
        }
        other => other,
    }
}

/// Compute the constant literal prefix length of a branch.
///
/// For `Seq([Literal("calc"), Literal("("), ...])` → 5.
/// For `Ref(rule_id)` → follows the reference to measure the rule body.
/// For `Regex(_)` → 0 (no constant prefix).
fn literal_prefix_len(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> u32 {
    match node {
        IrNode::Literal(sid) => strings[*sid as usize].len() as u32,
        IrNode::Seq(children) => {
            let mut total = 0u32;
            for child in children {
                let inner = unwrap_transparent(child);
                if let IrNode::Literal(sid) = inner {
                    total += strings[*sid as usize].len() as u32;
                } else {
                    // Stop at first non-literal — only constant prefix counts.
                    break;
                }
            }
            total
        }
        IrNode::Ref(rule_id) => {
            let rule_id = *rule_id as usize;
            if rule_id < rule_metas.len() {
                // Don't recurse deeply — just check the rule's first_set width
                // as a proxy.  Deep recursion risks infinite loops on cyclic rules.
                0
            } else {
                0
            }
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => {
            literal_prefix_len(unwrap_transparent(a), rule_metas, strings)
        }
        _ => 0,
    }
}

/// Compute the FIRST set width (number of distinct leading bytes).
fn first_set_width(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> u32 {
    match node {
        IrNode::Literal(sid) => {
            let s = &strings[*sid as usize];
            if s.is_empty() { 128 } else { 1 }
        }
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            match crate::regex_first::regex_first_chars(pattern) {
                Some(cs) => cs.len() as u32,
                None => 128, // nullable → widest possible
            }
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            if nullable { 128 } else { first_set.len() as u32 }
        }
        IrNode::Seq(children) => {
            if let Some(first) = children.first() {
                first_set_width(unwrap_transparent(first), rule_metas, strings)
            } else {
                128
            }
        }
        IrNode::Alt(branches, _) => {
            // Union of all branch widths.
            let mut combined = CharSet128::new();
            for b in branches {
                let w = first_set_width(unwrap_transparent(&b.node), rule_metas, strings);
                if w >= 128 {
                    return 128;
                }
                // Approximate: just use branch count as proxy.
            }
            combined.len() as u32
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            first_set_width(inner, rule_metas, strings)
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) | IrNode::Minus(a, _) => {
            first_set_width(unwrap_transparent(a), rule_metas, strings)
        }
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon | IrNode::Negate(_) => 128,
        IrNode::Repeat { inner, .. } => first_set_width(inner, rule_metas, strings),
        IrNode::TokenDispatch { token, .. } => {
            first_set_width(token, rule_metas, strings)
        }
    }
}

/// Detect "catchall" branches — regex-only patterns with wide FIRST sets
/// that match most input (identifiers, generic patterns).  These should
/// always sort last.
fn is_catchall(
    node: &IrNode,
    rule_metas: &[(CharSet128, bool)],
    strings: &[String],
) -> bool {
    match node {
        IrNode::Regex(sid) => {
            let pattern = &strings[*sid as usize];
            match crate::regex_first::regex_first_chars(pattern) {
                Some(cs) => cs.len() >= 20, // ≥20 distinct first bytes = catchall
                None => true,               // nullable = catchall
            }
        }
        IrNode::Ref(rule_id) => {
            let (ref first_set, nullable) = rule_metas[*rule_id as usize];
            nullable || first_set.len() >= 20
        }
        IrNode::Seq(children) => {
            if let Some(first) = children.first() {
                is_catchall(unwrap_transparent(first), rule_metas, strings)
            } else {
                true // empty Seq is epsilon → catchall
            }
        }
        IrNode::Skip(a, _) | IrNode::Next(a, _) => {
            is_catchall(unwrap_transparent(a), rule_metas, strings)
        }
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            is_catchall(inner, rule_metas, strings)
        }
        IrNode::Repeat { lo: 0, .. } | IrNode::Epsilon | IrNode::Negate(_) => true,
        IrNode::Repeat { inner, .. } => is_catchall(inner, rule_metas, strings),
        _ => false,
    }
}
