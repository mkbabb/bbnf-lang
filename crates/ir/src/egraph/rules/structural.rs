//! Structural rewrites that depend on grammar-level metadata
//! (alias chains, rule reference counts, etc.) rather than pure
//! e-graph shape. Each rule carries the pre-computed metadata as
//! constructor data so `search` stays O(classes · nodes).

use std::collections::{HashMap, HashSet};

use egraph::{EGraph, Id, Rewrite};

use crate::egraph::analysis::GrammarAnalysis;
use crate::egraph::node::GrammarENode;
use crate::{GrammarIR, RuleId};

// ── Rule: Ref(alias_id) ≡ Ref(canonical_id) ─────────────────────────────────

/// Non-destructive port of the legacy `canonicalize_aliases` pass.
///
/// For each `Ref(rule_id)` in the e-graph where `rule_id` is an alias,
/// the rule adds `Ref(canonical(rule_id))` into the same e-class. The
/// cost model (lower cost for canonical refs via fewer indirections)
/// picks the canonical form during extraction.
///
/// The `canonical` map is built from `GrammarIR::rules[].meta.is_alias`
/// chains by [`build_alias_map`]. It is immutable across the
/// saturation loop — alias relationships come from grammar structure,
/// not from e-graph rewrites.
pub struct CanonicalizeAlias {
    canonical: HashMap<RuleId, RuleId>,
}

impl CanonicalizeAlias {
    pub fn new(ir: &GrammarIR) -> Self {
        Self {
            canonical: build_alias_map(ir),
        }
    }

    /// Number of aliases that will be canonicalized (diagnostic).
    pub fn alias_count(&self) -> usize {
        self.canonical
            .iter()
            .filter(|(src, dst)| *src != *dst)
            .count()
    }
}

impl Rewrite<GrammarENode, GrammarAnalysis> for CanonicalizeAlias {
    /// The canonical RuleId we want to install as a `Ref`.
    type Match = RuleId;

    fn name(&self) -> &str {
        "canonicalize-alias"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Ref(rid) = node else {
                    continue;
                };
                // Resolve `rid` to its canonical target. If the map
                // doesn't contain it, it's already canonical.
                if let Some(&canon) = self.canonical.get(rid) {
                    if canon != *rid {
                        matches.push((class.id, canon));
                        break;
                    }
                }
            }
        }
        matches
    }

    fn apply(
        &self,
        egraph: &mut EGraph<GrammarENode, GrammarAnalysis>,
        class_id: Id,
        canon: Self::Match,
    ) -> bool {
        let new_id = egraph.add(GrammarENode::Ref(canon));
        let before = egraph.find(class_id);
        egraph.union(class_id, new_id);
        egraph.find(class_id) != before
    }
}

/// Build a `RuleId → canonical_RuleId` map by resolving every
/// `meta.is_alias` chain to its terminal target. Cycles break at the
/// first repeat.
pub fn build_alias_map(ir: &GrammarIR) -> HashMap<RuleId, RuleId> {
    let alias_targets: HashMap<RuleId, Option<RuleId>> =
        ir.rules.iter().map(|r| (r.id, r.meta.is_alias)).collect();

    let mut canonical = HashMap::new();
    for rule in &ir.rules {
        if rule.meta.is_alias.is_some() {
            canonical.insert(rule.id, resolve_chain(rule.id, &alias_targets));
        }
    }
    canonical
}

fn resolve_chain(
    start: RuleId,
    alias_targets: &HashMap<RuleId, Option<RuleId>>,
) -> RuleId {
    let mut current = start;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            // Cycle — return current to break the loop.
            return current;
        }
        match alias_targets.get(&current) {
            Some(Some(target)) if *target != current => {
                current = *target;
            }
            _ => return current,
        }
    }
}
