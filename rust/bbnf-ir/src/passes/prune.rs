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
    let mut reachable = HashSet::new();
    let mut stack = vec![ir.entry];

    while let Some(rule_id) = stack.pop() {
        if !reachable.insert(rule_id) {
            continue;
        }
        if let Some(rule) = ir.rules.iter().find(|r| r.id == rule_id) {
            collect_refs(&rule.body, &mut stack);
            // Also follow recovery expressions.
            if let Some(ref recover) = rule.meta.recover {
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
        if let Some(ref mut recover) = rule.meta.recover {
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
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AltBranch, IrRule, RuleMeta};

    fn make_rule(id: RuleId, name_id: u32, body: IrNode) -> IrRule {
        IrRule {
            id,
            name: name_id,
            body,
            meta: RuleMeta::default(),
        }
    }

    #[test]
    fn prune_removes_unreachable() {
        let mut ir = GrammarIR {
            rules: vec![
                make_rule(0, 0, IrNode::Ref(1)),       // start → a
                make_rule(1, 1, IrNode::Literal(2)),    // a → "x"
                make_rule(2, 3, IrNode::Literal(4)),    // dead → "y"
            ],
            entry: 0,
            strings: vec![
                "start".into(),
                "a".into(),
                "x".into(),
                "dead".into(),
                "y".into(),
            ],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        prune_unreachable(&mut ir);

        assert_eq!(ir.rules.len(), 2);
        // IDs should be compacted to 0, 1.
        assert_eq!(ir.rules[0].id, 0);
        assert_eq!(ir.rules[1].id, 1);
        // The Ref in rule 0 should point to new id of rule "a".
        assert!(matches!(ir.rules[0].body, IrNode::Ref(1)));
    }

    #[test]
    fn prune_keeps_all_when_all_reachable() {
        let mut ir = GrammarIR {
            rules: vec![
                make_rule(0, 0, IrNode::Ref(1)),
                make_rule(1, 1, IrNode::Literal(2)),
            ],
            entry: 0,
            strings: vec!["start".into(), "a".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        prune_unreachable(&mut ir);
        assert_eq!(ir.rules.len(), 2);
    }

    #[test]
    fn prune_follows_alt_branches() {
        let mut ir = GrammarIR {
            rules: vec![
                make_rule(
                    0,
                    0,
                    IrNode::Alt(vec![
                        AltBranch { node: IrNode::Ref(1), first_set: None },
                        AltBranch { node: IrNode::Ref(2), first_set: None },
                    ], None),
                ),
                make_rule(1, 1, IrNode::Literal(3)),
                make_rule(2, 2, IrNode::Literal(4)),
                make_rule(3, 5, IrNode::Literal(6)), // dead
            ],
            entry: 0,
            strings: vec![
                "start".into(), "a".into(), "b".into(),
                "x".into(), "y".into(), "dead".into(), "z".into(),
            ],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        prune_unreachable(&mut ir);
        assert_eq!(ir.rules.len(), 3); // start, a, b — dead removed
    }
}
