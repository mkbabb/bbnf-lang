//! Ref-inlining rewrite rules: combined port of the legacy
//! `inline_acyclic` and `fuse_single_use` passes.
//!
//! Both legacy passes have the same shape — they identify inlineable
//! rules by metadata, then rewrite `Ref(id)` nodes to the inlined
//! body. The e-graph version unifies both into a single
//! `InlineEligibleRef` rule that unions a `Ref(id)` e-class with the
//! rule body's e-class when the target is eligible by either
//! criterion:
//!
//! 1. **Acyclic + small** — body has ≤ [`INLINE_SIZE_THRESHOLD`] nodes,
//!    the rule is not cyclic, not the entry point, not
//!    `preserve_identity`.
//! 2. **Single-use** — referenced exactly once across the grammar,
//!    same acyclic / non-entry / non-identity guards.
//!
//! Unlike the destructive passes, nothing is deleted — the `Ref(id)`
//! form stays in the e-graph and the cost model picks the cheaper
//! representation (inlined body vs. ref) during extraction. For small
//! bodies, inlining is cheaper; for large shared bodies, the cost
//! model keeps the ref.

use std::collections::{HashMap, HashSet};

use egraph::{EGraph, Id, Rewrite};

use crate::egraph::analysis::GrammarAnalysis;
use crate::egraph::node::GrammarENode;
use crate::{GrammarIR, IrNode, RuleId};

/// Body-size threshold for acyclic inlining (matches the legacy
/// `INLINE_THRESHOLD` constant in `passes/transform/inline.rs`).
pub const INLINE_SIZE_THRESHOLD: usize = 4;

/// Inline a `Ref(rule_id)` e-class with the target rule's body
/// e-class when the rule is inlining-eligible by either the
/// size-bounded acyclic criterion or the single-use criterion.
pub struct InlineEligibleRef {
    /// Set of rule ids that satisfy at least one inlining criterion.
    eligible: HashSet<RuleId>,
    /// Map from RuleId → the root e-class Id of that rule's body in
    /// the current e-graph. Populated by `build_and_saturate` after
    /// inserting all rule bodies, before running saturation.
    rule_body_ids: HashMap<RuleId, Id>,
    /// Reverse lookup: the set of rule-root e-classes. Used as a
    /// guard against firing inside *another* rule's root class,
    /// which would fold the rule's body into the Ref site and
    /// dissolve rule boundaries during extraction.
    root_classes: HashSet<Id>,
}

impl InlineEligibleRef {
    /// Construct the rule from grammar metadata and per-rule body
    /// root Ids (returned by the e-graph build phase).
    pub fn new(ir: &GrammarIR, rule_body_ids: HashMap<RuleId, Id>) -> Self {
        let root_classes: HashSet<Id> = rule_body_ids.values().copied().collect();
        Self {
            eligible: compute_eligible(ir),
            rule_body_ids,
            root_classes,
        }
    }
}

impl Rewrite<GrammarENode, GrammarAnalysis> for InlineEligibleRef {
    type Match = Id;

    fn name(&self) -> &str {
        "inline-eligible-ref"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        // Snapshot the canonical form of every rule root — after
        // rebuilds, the canonical ids shift, so we recompute here.
        let canonical_roots: HashSet<Id> = self
            .root_classes
            .iter()
            .map(|&id| egraph.find_ref(id))
            .collect();
        for class in egraph.classes() {
            // Guard: never fire inside a class that IS the root of
            // any rule. Unioning a rule root with another rule's
            // body would dissolve the rule boundary during extraction
            // (both rules' write_backs would see the same class and
            // could extract each other's Ref/body interchangeably,
            // producing self-referential `rule_X = Ref(rule_X)`).
            if canonical_roots.contains(&egraph.find_ref(class.id)) {
                continue;
            }
            for node in class.iter() {
                let GrammarENode::Ref(rule_id) = node else {
                    continue;
                };
                if !self.eligible.contains(rule_id) {
                    continue;
                }
                let Some(&body_id) = self.rule_body_ids.get(rule_id) else {
                    continue;
                };
                // Don't union a class with itself.
                if egraph.find_ref(body_id) == egraph.find_ref(class.id) {
                    continue;
                }
                matches.push((class.id, body_id));
                break;
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        body_id: Self::Match,
    ) -> bool {
        let before = egraph.find(class_id);
        egraph.union(class_id, body_id);
        egraph.find(class_id) != before
    }
}

// ── Eligibility computation ─────────────────────────────────────────────────

/// Compute the set of rule ids eligible for inlining by either the
/// size-bounded acyclic criterion or the single-use criterion.
fn compute_eligible(ir: &GrammarIR) -> HashSet<RuleId> {
    let ref_counts = count_refs(ir);
    let mut eligible = HashSet::new();
    for rule in &ir.rules {
        if rule.id == ir.entry {
            continue;
        }
        if rule.meta.is_cyclic || rule.meta.preserve_identity {
            continue;
        }
        if rule.meta.scc_id.is_some() {
            continue;
        }
        let small = count_nodes(&rule.body) <= INLINE_SIZE_THRESHOLD;
        let single_use = ref_counts.get(rule.id as usize).copied().unwrap_or(0) == 1;
        if small || single_use {
            eligible.insert(rule.id);
        }
    }
    eligible
}

/// Count per-rule reference totals across every rule body (and
/// `@recover` sync expression). Indexed by `RuleId`.
fn count_refs(ir: &GrammarIR) -> Vec<u32> {
    let mut counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        walk_refs(&rule.body, &mut counts);
        if let Some(ref recover) = rule.meta.directives.recover {
            walk_refs(recover, &mut counts);
        }
    }
    counts
}

fn walk_refs(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(c) = counts.get_mut(*id as usize) {
                *c += 1;
            }
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
        IrNode::Seq(children) => children.iter().for_each(|c| walk_refs(c, counts)),
        IrNode::Alt(branches, _) => {
            branches.iter().for_each(|b| walk_refs(&b.node, counts))
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => walk_refs(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            walk_refs(a, counts);
            walk_refs(b, counts);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            walk_refs(token, counts);
            for arm in arms {
                walk_refs(&arm.continuation, counts);
            }
            walk_refs(fallback, counts);
        }
    }
}

/// Count the total number of nodes in an IR tree (for the acyclic
/// size-bound check). Matches the legacy `node_count` helper.
fn count_nodes(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 1,
        IrNode::Seq(children) => 1 + children.iter().map(count_nodes).sum::<usize>(),
        IrNode::Alt(branches, _) => {
            1 + branches.iter().map(|b| count_nodes(&b.node)).sum::<usize>()
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => 1 + count_nodes(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            1 + count_nodes(a) + count_nodes(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            1 + count_nodes(token)
                + count_nodes(fallback)
                + arms.iter().map(|a| count_nodes(&a.continuation)).sum::<usize>()
        }
    }
}
