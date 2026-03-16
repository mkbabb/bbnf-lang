//! Pass 2.3: Alias canonicalization.
//!
//! Resolve all `is_alias` chains to their terminal (non-alias) target,
//! then rewrite `Ref(alias_id)` → `Ref(canonical_id)` throughout the IR.

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId};

/// Resolve alias chains and rewrite Ref nodes to point to canonical targets.
pub fn canonicalize_aliases(ir: &mut GrammarIR) {
    // Build O(1) lookup from RuleId → alias target.
    let alias_targets: HashMap<RuleId, Option<RuleId>> = ir
        .rules
        .iter()
        .map(|r| (r.id, r.meta.is_alias))
        .collect();

    // Phase 1: Build alias chain resolution map.
    let mut canonical: HashMap<RuleId, RuleId> = HashMap::new();

    for rule in &ir.rules {
        if rule.meta.is_alias.is_some() {
            canonical.insert(rule.id, resolve_chain(rule.id, &alias_targets));
        }
    }

    // Phase 2: Update is_alias to point to canonical targets.
    for rule in &mut ir.rules {
        if let Some(ref mut alias_id) = rule.meta.is_alias {
            if let Some(&canon) = canonical.get(&rule.id) {
                *alias_id = canon;
            }
        }
    }

    // Phase 3: Rewrite Ref nodes.
    // Only rewrite refs that point to aliases — don't collapse all refs.
    for rule in &mut ir.rules {
        rewrite_refs(&mut rule.body, &canonical);
        if let Some(ref mut recover) = rule.meta.recover {
            rewrite_refs(recover, &canonical);
        }
    }
}

/// Follow the alias chain from `start` to find the terminal (non-alias) target.
fn resolve_chain(
    start: RuleId,
    alias_targets: &std::collections::HashMap<RuleId, Option<RuleId>>,
) -> RuleId {
    let mut current = start;
    let mut visited = std::collections::HashSet::new();

    loop {
        if !visited.insert(current) {
            // Cycle detected — return current to break the loop.
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

/// Rewrite `Ref(alias_id)` → `Ref(canonical_id)` in an IrNode tree.
fn rewrite_refs(node: &mut IrNode, aliases: &HashMap<RuleId, RuleId>) {
    match node {
        IrNode::Ref(id) => {
            if let Some(&canonical) = aliases.get(id) {
                *id = canonical;
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                rewrite_refs(child, aliases);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                rewrite_refs(&mut branch.node, aliases);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            rewrite_refs(inner, aliases);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            rewrite_refs(a, aliases);
            rewrite_refs(b, aliases);
        }
        IrNode::Map { inner, .. } => {
            rewrite_refs(inner, aliases);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
