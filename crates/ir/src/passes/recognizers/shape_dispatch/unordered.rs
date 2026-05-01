//! Unordered-shape detector — disjoint-FIRST Alt under a non-empty Repeat.
//!
//! # Predicate
//!
//! A rule is Unordered-shaped when its body resolves to a
//! `Repeat { lo >= 1, .. }` over an `Alt` whose branches all have
//! mutually-disjoint FIRST byte sets.
//!
//! The `lo >= 1` bound excludes optional-Alt patterns (`( alt ) ?`)
//! which are either Wrap-shape (transparent dispatcher) or absorbed
//! into the enclosing shape. The Unordered emitter emits a sub-loop
//! that MUST iterate at least once.
//!
//! # Canonical sources
//!
//! - CSS `compoundSelector = (classSelector | idSelector |
//!   attrSelector | colonSelector | typeSelector) +` per
//!   `grammar/css/l4/selectors.bbnf:87-88` — 5 branches with disjoint
//!   FIRST bytes (`.`, `#`, `[`, `:`, `[a-z]`).
//!
//! # FIRST-byte projection
//!
//! FIRST byte sets per branch compose by structural walk:
//!
//! - `Literal(s)` — the byte at `s[0]`.
//! - `Regex(p)` — [`regex_first_chars`](
//!   crate::regex_first::regex_first_chars) over the pattern.
//! - `Ref(r)` — a cycle-guarded descent into the target rule's body.
//! - `Seq([c₁, c₂, …])` — `FIRST(c₁) ∪ (if c₁ nullable: FIRST(c₂) ∪ …)`.
//! - `Alt([b₁, b₂, …])` — `⋃ FIRST(bᵢ)`.
//! - `Map` / `OptionalWhitespace` / `Skip` / `Next` / `Minus` — strip
//!   and recurse into the head / inner as the structural head dictates.
//! - `Repeat { lo ≥ 1 }` — `FIRST(inner)` (non-nullable).
//! - `Repeat { lo = 0 }` — treated as nullable; caller handles.
//!
//! The detector avoids consulting the rule-level
//! [`RuleMeta::first_set`](crate::types::RuleMeta::first_set) because
//! that surface's nullable-`OptionalWhitespace` convention bleeds
//! trailing-position bytes into the FIRST set on rules like CSS
//! `attrSelector` whose `"[" ?w, …` head lowers to
//! `Seq(OW(Literal("[")), …)` — structurally the trailing `?w` is a
//! post-literal trim, but the shared nullable convention on
//! `OptionalWhitespace` causes the Seq FIRST computation to carry
//! through to the second element. The narrower structural-walk here
//! produces the FIRST set the Unordered emitter's byte-dispatch loop
//! actually needs: the set of leading bytes each branch's first
//! position matches.

use std::collections::HashSet;

use bbnf_regex::sets::charset::CharSet128;

use crate::passes::inspect::unwrap_map_ow;
use crate::types::{AltBranch, GrammarIR, IrNode, RuleId};

/// Detect Unordered-shape: body is `Repeat { lo >= 1 }` over an Alt
/// whose branches have mutually-disjoint FIRST byte sets.
pub fn detect_unordered(rule_id: RuleId, ir: &GrammarIR) -> bool {
    let rule = &ir.rules[rule_id as usize];
    let body = unwrap_map_ow(&rule.body);

    // Kleene-plus (or higher-floor) Repeat over an Alt.
    let IrNode::Repeat { inner, lo, hi: _ } = body else {
        return false;
    };
    if *lo < 1 {
        return false;
    }
    let inner_body = unwrap_map_ow(inner);
    let IrNode::Alt(branches, _) = inner_body else {
        return false;
    };
    // Two or more branches — single-branch Alts degenerate into
    // structural wrappers the walker handles directly.
    if branches.len() < 2 {
        return false;
    }

    // Project per-branch FIRST byte sets. Every branch must yield a
    // non-empty set; when any branch resolves to the empty set (e.g.,
    // a nullable Ref, a bare Epsilon), the detector rejects — the
    // emitter's byte-dispatch loop has nothing to key the branch off.
    let mut visited: HashSet<RuleId> = HashSet::new();
    let per_branch: Option<Vec<CharSet128>> = branches
        .iter()
        .map(|b| branch_first(b, ir, &mut visited))
        .collect();
    let Some(per_branch) = per_branch else {
        return false;
    };
    if per_branch.iter().any(|s| s.is_empty()) {
        return false;
    }

    // Pairwise disjointness. `CharSet128::is_disjoint` is a two-word
    // AND so the O(n²) loop costs 2·(n²/2) bitwise ops for any
    // Unordered candidate — bounded by the Alt's branch count.
    for i in 0..per_branch.len() {
        for j in (i + 1)..per_branch.len() {
            if !per_branch[i].is_disjoint(&per_branch[j]) {
                return false;
            }
        }
    }

    true
}

/// Compute the FIRST byte set for an Alt branch by structural walk.
fn branch_first(
    branch: &AltBranch,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<CharSet128> {
    node_first(&branch.node, ir, visited)
}

/// Node-level nullable predicate — exact semantics for the Unordered
/// detector's FIRST computation. Differs from the FIRST-set CSP's
/// nullable in that `OptionalWhitespace(inner)` is nullable iff
/// `inner` is nullable; the `?w` trailing trim doesn't make a non-
/// nullable `inner` match empty.
fn is_nullable(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> bool {
    match node {
        IrNode::Epsilon => true,
        IrNode::Repeat { lo: 0, .. } => true,
        IrNode::Literal(sid) => ir.get_string(*sid).is_empty(),
        IrNode::Regex(_) | IrNode::Negate(_) => false,
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return false;
            }
            let result = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .map(|r| is_nullable(&r.body, ir, visited))
                .unwrap_or(false);
            visited.remove(rid);
            result
        }
        IrNode::Seq(children) => children.iter().all(|c| is_nullable(c, ir, visited)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| is_nullable(&b.node, ir, visited)),
        IrNode::Repeat { lo, .. } => *lo == 0,
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            is_nullable(a, ir, visited) && is_nullable(b, ir, visited)
        }
        IrNode::Minus(a, _) => is_nullable(a, ir, visited),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            is_nullable(inner, ir, visited)
        }
        IrNode::TokenDispatch { token, .. } => is_nullable(token, ir, visited),
    }
}

/// Project a node's FIRST byte set by structural walk with a cycle
/// guard on Ref descent.
///
/// Returns [`None`] for node shapes where FIRST is structurally
/// undecidable — Negate / TokenDispatch — or for a Ref cycle.
fn node_first(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            if bytes.is_empty() {
                None
            } else {
                let mut cs = CharSet128::new();
                cs.add(bytes[0]);
                Some(cs)
            }
        }
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            Some(crate::regex_first::regex_first_chars(pattern).unwrap_or_default())
        }
        IrNode::Epsilon => Some(CharSet128::new()),
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return None;
            }
            let result = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .and_then(|r| node_first(&r.body, ir, visited));
            visited.remove(rid);
            result
        }
        IrNode::Seq(children) => {
            let mut acc = CharSet128::new();
            for c in children {
                let part = node_first(c, ir, visited)?;
                acc.union(&part);
                if !is_nullable(c, ir, visited) {
                    return Some(acc);
                }
            }
            Some(acc)
        }
        IrNode::Alt(branches, _) => {
            let mut acc = CharSet128::new();
            for b in branches {
                let part = node_first(&b.node, ir, visited)?;
                acc.union(&part);
            }
            Some(acc)
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            let mut acc = node_first(a, ir, visited)?;
            if is_nullable(a, ir, visited) {
                let part = node_first(b, ir, visited)?;
                acc.union(&part);
            }
            Some(acc)
        }
        IrNode::Minus(a, _) => node_first(a, ir, visited),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            node_first(inner, ir, visited)
        }
        IrNode::Repeat { inner, .. } => node_first(inner, ir, visited),
        IrNode::Negate(_) | IrNode::TokenDispatch { .. } => None,
    }
}
