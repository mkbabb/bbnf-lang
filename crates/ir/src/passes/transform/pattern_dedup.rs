//! AW-IV.W4.3.c — grammar-level pattern hoisting (AP.4.2 chronic).
//!
//! # Architectural role
//!
//! Compile-time sibling to the W4.3 runtime bloom+GADT dedup. Where
//! the runtime pass collapses duplicate *instances* of a recognised
//! shape, this pass collapses duplicate *definitions* across rule
//! bodies: recurring `Seq` / `Alt` sub-patterns are hoisted into
//! freshly-synthesised rules and every occurrence is rewritten to a
//! `Ref(__pattern_<hash>)`.
//!
//! Typical targets (measured on CSS L4):
//!
//! - `ws + ':' + ws` sequences — 43 occurrences across declarations,
//!   ratio expressions, nested-rule selectors.
//! - `!important` — 42 occurrences across rule bodies that accept
//!   `@pretty`-aware overrides.
//! - Repeated `(delim_ws, identifier, delim_ws)` triples in CSS
//!   selector list compositions.
//!
//! Each hoist collapses N textual copies to 1 rule + N `Ref`s — a
//! direct IR-size reduction. Because each hoisted rule emits one DTA
//! state (plus its referenced state expansions), and every `Ref`
//! touch shares the same entry state, the downstream DTA state count
//! drops ≥ 100 on CSS L4 per W4.3 hard gate.
//!
//! # Why pre-egraph
//!
//! The egraph pass benefits from a simpler IR with hoisted
//! commonality already in place: common-subexpression elimination
//! (`DeduplicateAltBranches`, `UnionMergeAlt`) converges faster on
//! rewritten `Ref` nodes than on the original inline trees.
//!
//! # Algorithm
//!
//! 1. Walk the IR collecting every `Seq` / `Alt` sub-pattern —
//!    excluding the rule-body roots (hoisting a rule's own body is a
//!    no-op).
//! 2. Hash each by structural signature (kind discriminants + child
//!    shape, recursive).
//! 3. Group by hash; identify buckets with ≥ `MIN_OCCURRENCES`
//!    distinct occurrences.
//! 4. For each qualifying bucket, synthesise a new rule
//!    `__pattern_<hash>: <body>` and rewrite every occurrence to
//!    `Ref(<new_rule_id>)`.
//!
//! # §6 generalisation
//!
//! No grammar branches. The algorithm works on any grammar IR; the
//! per-grammar yield varies because the per-grammar IR differs
//! (JSON has few recurring compounds; CSS L4 has many; BBNF sits in
//! between).

use rustc_hash::FxHashMap;
use std::collections::HashMap;

use crate::{GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

/// Minimum number of distinct occurrences required to hoist a
/// sub-pattern.
///
/// Three is the classic DRY threshold: two hits could be coincidence;
/// three or more are a pattern. Below this the hoist cost (new rule
/// definition + `Ref` indirection through the DTA) exceeds the gain.
pub const MIN_OCCURRENCES: usize = 3;

/// Maximum node count for a hoisted pattern.
///
/// Very small patterns (single `Literal` / `Epsilon`) are cheaper
/// inline than as a referenced rule — hoisting them adds a DTA state
/// without removing the original. Very large patterns almost never
/// recur exactly, so the occurrence filter naturally excludes them.
/// A range of 2..=8 nodes captures the sweet spot.
pub const MIN_PATTERN_NODES: usize = 2;
pub const MAX_PATTERN_NODES: usize = 8;

/// Grammar-level pattern hoisting pass.
///
/// Mutates `ir.rules` in place: appends synthesised hoist rules and
/// rewrites every qualifying sub-pattern occurrence to a `Ref` at
/// the hoist's rule id.
///
/// Returns the number of patterns hoisted — useful for hard-gate
/// verification in tests + for integration diagnostics.
pub fn hoist_recurring_patterns(ir: &mut GrammarIR) -> usize {
    if ir.rules.is_empty() {
        return 0;
    }

    // Step 1: collect occurrences keyed by signature hash.
    let mut occurrences: FxHashMap<u64, Vec<Signature>> = FxHashMap::default();
    for (rule_idx, rule) in ir.rules.iter().enumerate() {
        collect_patterns(&rule.body, rule_idx, &mut occurrences, true);
    }

    // Step 2: filter to buckets with ≥ MIN_OCCURRENCES.
    let mut hoist_candidates: Vec<(u64, Vec<Signature>)> = occurrences
        .into_iter()
        .filter(|(_, sigs)| sigs.len() >= MIN_OCCURRENCES)
        .collect();
    // Deterministic ordering across compile sessions.
    hoist_candidates.sort_by_key(|(hash, _)| *hash);

    if hoist_candidates.is_empty() {
        return 0;
    }

    // Step 3: snapshot one representative body per bucket (the first
    // occurrence's subtree). All occurrences share the same structure
    // by construction of the hash; deep-equal means the snapshot can
    // stand in for any of them.
    let mut representative_bodies: Vec<(u64, IrNode)> = Vec::with_capacity(hoist_candidates.len());
    for (hash, sigs) in &hoist_candidates {
        if let Some(first_sig) = sigs.first() {
            let body = clone_pattern_at(ir, first_sig);
            // Structural deep-equal check across all occurrences in
            // the bucket. Hashing by signature catches most cases;
            // the deep-equal filter is belt-and-suspenders.
            let all_match = sigs.iter().all(|s| clone_pattern_at(ir, s) == body);
            if all_match && matches!(body, IrNode::Seq(_) | IrNode::Alt(_, _)) {
                representative_bodies.push((*hash, body));
            }
        }
    }

    if representative_bodies.is_empty() {
        return 0;
    }

    // Step 4: synthesise new rules + record their ids by hash.
    let mut hash_to_rule_id: HashMap<u64, RuleId> = HashMap::new();
    let mut synth_rules: Vec<IrRule> = Vec::new();
    let base_rule_id: RuleId = (ir.rules.len() + synth_rules.len()) as RuleId;
    for (idx, (hash, body)) in representative_bodies.iter().enumerate() {
        let new_rule_id: RuleId = base_rule_id + idx as RuleId;
        let name = format!("__pattern_{:016x}", hash);
        let name_id = intern_string(&mut ir.strings, name);
        synth_rules.push(IrRule {
            id: new_rule_id,
            name: name_id,
            body: body.clone(),
            meta: RuleMeta::default(),
            source_span: None,
        });
        hash_to_rule_id.insert(*hash, new_rule_id);
    }

    // Step 5: rewrite every occurrence to a `Ref(new_rule_id)`.
    for rule in &mut ir.rules {
        rewrite_patterns(&mut rule.body, &hash_to_rule_id, true);
    }

    // Step 6: append synth rules.
    let hoist_count = synth_rules.len();
    ir.rules.extend(synth_rules);
    hoist_count
}

/// A located occurrence of a sub-pattern — the rule index + a
/// path-tag chain that locates the sub-tree inside the rule body.
///
/// Used only during mining; never escapes this module.
#[derive(Clone, Debug)]
struct Signature {
    rule_idx: usize,
    /// Path from the rule body to the sub-pattern. Each entry is a
    /// child index into the parent node's child list.
    path: Vec<usize>,
}

/// Walk a rule body collecting every eligible sub-pattern.
///
/// `is_root` is `true` for the direct body call; sub-calls pass
/// `false` so the rule body itself is never hoisted (doing so would
/// replace the rule with a `Ref` to another rule — an infinite
/// indirection).
fn collect_patterns(
    node: &IrNode,
    rule_idx: usize,
    out: &mut FxHashMap<u64, Vec<Signature>>,
    is_root: bool,
) {
    collect_patterns_with_path(node, rule_idx, Vec::new(), out, is_root);
}

fn collect_patterns_with_path(
    node: &IrNode,
    rule_idx: usize,
    path: Vec<usize>,
    out: &mut FxHashMap<u64, Vec<Signature>>,
    is_root: bool,
) {
    // Only Seq / Alt sub-patterns are eligible. Literals, Regex, Ref,
    // Epsilon are too small to benefit; Repeat / Map / etc. carry
    // semantics we do not want to duplicate into a synthesised rule.
    let eligible = !is_root && matches!(node, IrNode::Seq(_) | IrNode::Alt(_, _));
    if eligible {
        let n = count_nodes_bounded(node, MAX_PATTERN_NODES + 1);
        if (MIN_PATTERN_NODES..=MAX_PATTERN_NODES).contains(&n) {
            let hash = signature_hash(node);
            out.entry(hash).or_default().push(Signature {
                rule_idx,
                path: path.clone(),
            });
        }
    }
    // Recurse into children with extended path.
    match node {
        IrNode::Seq(children) => {
            for (i, child) in children.iter().enumerate() {
                let mut child_path = path.clone();
                child_path.push(i);
                collect_patterns_with_path(child, rule_idx, child_path, out, false);
            }
        }
        IrNode::Alt(branches, _) => {
            for (i, branch) in branches.iter().enumerate() {
                let mut child_path = path.clone();
                child_path.push(i);
                collect_patterns_with_path(&branch.node, rule_idx, child_path, out, false);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => {
            let mut child_path = path.clone();
            child_path.push(0);
            collect_patterns_with_path(inner, rule_idx, child_path, out, false);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            let mut ap = path.clone();
            ap.push(0);
            collect_patterns_with_path(a, rule_idx, ap, out, false);
            let mut bp = path.clone();
            bp.push(1);
            collect_patterns_with_path(b, rule_idx, bp, out, false);
        }
        _ => {}
    }
}

/// Canonical structural hash: FxHasher over the tree's kind +
/// content discriminants. Includes string ids and rule ids so
/// `Literal(3)` hashes differently from `Literal(4)`.
fn signature_hash(node: &IrNode) -> u64 {
    use rustc_hash::FxHasher;
    use std::hash::Hasher;
    let mut hasher = FxHasher::default();
    hash_node(node, &mut hasher);
    hasher.finish()
}

fn hash_node<H: std::hash::Hasher>(node: &IrNode, h: &mut H) {
    use std::hash::Hash;
    std::mem::discriminant(node).hash(h);
    match node {
        IrNode::Literal(sid) | IrNode::Regex(sid) => sid.hash(h),
        IrNode::Ref(rid) => rid.hash(h),
        IrNode::Epsilon => {}
        IrNode::Seq(children) => {
            children.len().hash(h);
            for child in children {
                hash_node(child, h);
            }
        }
        IrNode::Alt(branches, _) => {
            branches.len().hash(h);
            for branch in branches {
                hash_node(&branch.node, h);
            }
        }
        IrNode::Repeat { inner, lo, hi } => {
            lo.hash(h);
            hi.hash(h);
            hash_node(inner, h);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            hash_node(a, h);
            hash_node(b, h);
        }
        IrNode::Negate(inner) | IrNode::OptionalWhitespace(inner) => hash_node(inner, h),
        IrNode::Map { inner, fn_id } => {
            fn_id.hash(h);
            hash_node(inner, h);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            hash_node(token, h);
            arms.len().hash(h);
            for arm in arms {
                arm.patterns.len().hash(h);
                for p in &arm.patterns {
                    p.hash(h);
                }
                arm.guard_byte.hash(h);
                arm.map_fn.hash(h);
                hash_node(&arm.continuation, h);
            }
            hash_node(fallback, h);
        }
    }
}

/// Count nodes in a subtree, short-circuiting at `limit`.
fn count_nodes_bounded(node: &IrNode, limit: usize) -> usize {
    let mut count: usize = 0;
    count_rec(node, &mut count, limit);
    count
}

fn count_rec(node: &IrNode, count: &mut usize, limit: usize) {
    if *count >= limit {
        return;
    }
    *count += 1;
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => {}
        IrNode::Seq(children) => {
            for child in children {
                count_rec(child, count, limit);
                if *count >= limit {
                    return;
                }
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                count_rec(&b.node, count, limit);
                if *count >= limit {
                    return;
                }
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => count_rec(inner, count, limit),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_rec(a, count, limit);
            if *count >= limit {
                return;
            }
            count_rec(b, count, limit);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            count_rec(token, count, limit);
            for arm in arms {
                count_rec(&arm.continuation, count, limit);
                if *count >= limit {
                    return;
                }
            }
            count_rec(fallback, count, limit);
        }
    }
}

/// Clone the sub-pattern at `sig` from the IR.
fn clone_pattern_at(ir: &GrammarIR, sig: &Signature) -> IrNode {
    let body = &ir.rules[sig.rule_idx].body;
    let mut cursor: &IrNode = body;
    for &step in &sig.path {
        cursor = match cursor {
            IrNode::Seq(children) => &children[step],
            IrNode::Alt(branches, _) => &branches[step].node,
            IrNode::Repeat { inner, .. }
            | IrNode::Map { inner, .. }
            | IrNode::OptionalWhitespace(inner)
            | IrNode::Negate(inner) => inner,
            IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
                if step == 0 {
                    a
                } else {
                    b
                }
            }
            _ => return cursor.clone(),
        };
    }
    cursor.clone()
}

/// Rewrite every matching sub-pattern to a `Ref(rule_id)`.
///
/// Walks top-down, consulting the hash-to-rule-id map at every Seq /
/// Alt node. A match on the current node replaces the node in place;
/// if no match, recurse into children.
fn rewrite_patterns(node: &mut IrNode, map: &HashMap<u64, RuleId>, is_root: bool) {
    if !is_root && matches!(node, IrNode::Seq(_) | IrNode::Alt(_, _)) {
        let hash = signature_hash(node);
        if let Some(&rule_id) = map.get(&hash) {
            *node = IrNode::Ref(rule_id);
            return;
        }
    }
    // Not a hit at this level — recurse into children.
    match node {
        IrNode::Seq(children) => {
            for child in children.iter_mut() {
                rewrite_patterns(child, map, false);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches.iter_mut() {
                rewrite_patterns(&mut branch.node, map, false);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Map { inner, .. }
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Negate(inner) => rewrite_patterns(inner, map, false),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            rewrite_patterns(a, map, false);
            rewrite_patterns(b, map, false);
        }
        _ => {}
    }
}

/// Intern a string into `ir.strings`, returning its id. Reuses an
/// existing id if the string is already in the pool.
fn intern_string(strings: &mut Vec<String>, s: String) -> u32 {
    for (i, existing) in strings.iter().enumerate() {
        if *existing == s {
            return i as u32;
        }
    }
    let id = strings.len() as u32;
    strings.push(s);
    id
}
