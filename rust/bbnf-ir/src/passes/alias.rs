//! Pass 2.3: Alias canonicalization.
//!
//! Resolve all `is_alias` chains to their terminal (non-alias) target,
//! then rewrite `Ref(alias_id)` → `Ref(canonical_id)` throughout the IR.

use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId};

/// Resolve alias chains and rewrite Ref nodes to point to canonical targets.
pub fn canonicalize_aliases(ir: &mut GrammarIR) {
    // Phase 1: Build alias chain resolution map.
    let mut canonical: HashMap<RuleId, RuleId> = HashMap::new();

    for rule in &ir.rules {
        if rule.meta.is_alias.is_some() {
            canonical.insert(rule.id, resolve_chain(rule.id, ir));
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
fn resolve_chain(start: RuleId, ir: &GrammarIR) -> RuleId {
    let mut current = start;
    let mut visited = std::collections::HashSet::new();

    loop {
        if !visited.insert(current) {
            // Cycle detected — return current to break the loop.
            return current;
        }
        match ir.rules.iter().find(|r| r.id == current) {
            Some(rule) => match rule.meta.is_alias {
                Some(target) if target != current => {
                    current = target;
                }
                _ => return current,
            },
            None => return current,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{IrRule, RuleMeta};

    fn make_rule(id: RuleId, name_id: u32, body: IrNode, alias: Option<RuleId>) -> IrRule {
        IrRule {
            id,
            name: name_id,
            body,
            meta: RuleMeta {
                is_alias: alias,
                ..Default::default()
            },
        }
    }

    #[test]
    fn resolve_direct_alias() {
        let mut ir = GrammarIR {
            rules: vec![
                make_rule(0, 0, IrNode::Ref(1), None),          // start → a
                make_rule(1, 1, IrNode::Ref(2), Some(2)),        // a → b (alias)
                make_rule(2, 2, IrNode::Literal(3), None),       // b → "x"
            ],
            entry: 0,
            strings: vec!["start".into(), "a".into(), "b".into(), "x".into()],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        canonicalize_aliases(&mut ir);

        // start's body Ref(1) should now point to Ref(2) since a is an alias for b.
        match &ir.rules[0].body {
            IrNode::Ref(id) => assert_eq!(*id, 2),
            other => panic!("expected Ref, got {:?}", other),
        }
    }

    #[test]
    fn resolve_chain_alias() {
        let mut ir = GrammarIR {
            rules: vec![
                make_rule(0, 0, IrNode::Ref(1), None),          // start → a
                make_rule(1, 1, IrNode::Ref(2), Some(2)),        // a → b (alias)
                make_rule(2, 2, IrNode::Ref(3), Some(3)),        // b → c (alias)
                make_rule(3, 3, IrNode::Literal(4), None),       // c → "x"
            ],
            entry: 0,
            strings: vec![
                "start".into(), "a".into(), "b".into(), "c".into(), "x".into(),
            ],
            fns: vec![],
            types: vec![],
            follow_sets: HashMap::new(),
        };

        canonicalize_aliases(&mut ir);

        // start's Ref(1) → Ref(3) (skip both aliases).
        match &ir.rules[0].body {
            IrNode::Ref(id) => assert_eq!(*id, 3),
            other => panic!("expected Ref, got {:?}", other),
        }

        // a's alias should point to 3 (canonical).
        assert_eq!(ir.rules[1].meta.is_alias, Some(3));
        // b's alias should point to 3 (canonical).
        assert_eq!(ir.rules[2].meta.is_alias, Some(3));
    }
}
