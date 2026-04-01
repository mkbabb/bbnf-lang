//! Pass 2.4: Import graph pruning — remove unreachable rules.
//!
//! DFS from the entry rule to collect all reachable rule IDs,
//! then remove rules not in the reachable set and remap RuleIds.

use std::collections::{HashMap, HashSet};

use crate::{GrammarIR, IrNode, RuleId};

/// Remove rules unreachable from the entry rule and compact RuleIds.
pub fn prune_unreachable(ir: &mut GrammarIR) {
    if ir.rules.is_empty() {
        return;
    }

    // Phase 1: Collect reachable rule IDs via DFS.
    // Build O(1) lookup from RuleId → index.
    let id_to_idx: HashMap<crate::RuleId, usize> = ir
        .rules
        .iter()
        .enumerate()
        .map(|(i, r)| (r.id, i))
        .collect();

    let mut reachable = HashSet::new();
    let mut stack = vec![ir.entry];

    while let Some(rule_id) = stack.pop() {
        if !reachable.insert(rule_id) {
            continue;
        }
        if let Some(&idx) = id_to_idx.get(&rule_id) {
            let rule = &ir.rules[idx];
            collect_refs(&rule.body, &mut stack);
            // Also follow recovery expressions.
            if let Some(ref recover) = rule.meta.directives.recover {
                collect_refs(recover, &mut stack);
            }
        }
    }

    // Phase 2: Filter to reachable rules only.
    ir.rules.retain(|r| reachable.contains(&r.id));

    // Phase 3: Build old→new RuleId mapping and compact.
    let old_to_new: HashMap<RuleId, RuleId> = ir
        .rules
        .iter()
        .enumerate()
        .map(|(new_id, rule)| (rule.id, new_id as RuleId))
        .collect();

    // Phase 4: Remap all RuleIds.
    for rule in &mut ir.rules {
        rule.id = old_to_new[&rule.id];
        remap_refs(&mut rule.body, &old_to_new);
        if let Some(ref mut recover) = rule.meta.directives.recover {
            remap_refs(recover, &old_to_new);
        }
        if let Some(ref mut alias_id) = rule.meta.is_alias {
            if let Some(&new_id) = old_to_new.get(alias_id) {
                *alias_id = new_id;
            }
        }
    }

    // Update entry.
    if let Some(&new_entry) = old_to_new.get(&ir.entry) {
        ir.entry = new_entry;
    }
}

/// Collect all `Ref(rule_id)` from an IrNode tree.
fn collect_refs(node: &IrNode, out: &mut Vec<RuleId>) {
    match node {
        IrNode::Ref(id) => out.push(*id),
        IrNode::Seq(children) => {
            for child in children {
                collect_refs(child, out);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                collect_refs(&branch.node, out);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            collect_refs(inner, out);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            collect_refs(a, out);
            collect_refs(b, out);
        }
        IrNode::Map { inner, .. } => {
            collect_refs(inner, out);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            collect_refs(token, out);
            for arm in arms {
                collect_refs(&arm.continuation, out);
            }
            collect_refs(fallback, out);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// Remap all `Ref(old_id)` to `Ref(new_id)` throughout an IrNode tree.
fn remap_refs(node: &mut IrNode, mapping: &HashMap<RuleId, RuleId>) {
    match node {
        IrNode::Ref(id) => {
            if let Some(&new_id) = mapping.get(id) {
                *id = new_id;
            }
        }
        IrNode::Seq(children) => {
            for child in children {
                remap_refs(child, mapping);
            }
        }
        IrNode::Alt(branches, _) => {
            for branch in branches {
                remap_refs(&mut branch.node, mapping);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner) => {
            remap_refs(inner, mapping);
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            remap_refs(a, mapping);
            remap_refs(b, mapping);
        }
        IrNode::Map { inner, .. } => {
            remap_refs(inner, mapping);
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            remap_refs(token, mapping);
            for arm in arms {
                remap_refs(&mut arm.continuation, mapping);
            }
            remap_refs(fallback, mapping);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}
