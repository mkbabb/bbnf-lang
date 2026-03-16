use std::collections::HashMap;

use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleId, RuleMeta};
use bbnf_ir::passes::canonicalize_aliases;

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
            make_rule(0, 0, IrNode::Ref(1), None),          // start -> a
            make_rule(1, 1, IrNode::Ref(2), Some(2)),        // a -> b (alias)
            make_rule(2, 2, IrNode::Literal(3), None),       // b -> "x"
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
            make_rule(0, 0, IrNode::Ref(1), None),          // start -> a
            make_rule(1, 1, IrNode::Ref(2), Some(2)),        // a -> b (alias)
            make_rule(2, 2, IrNode::Ref(3), Some(3)),        // b -> c (alias)
            make_rule(3, 3, IrNode::Literal(4), None),       // c -> "x"
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

    // start's Ref(1) -> Ref(3) (skip both aliases).
    match &ir.rules[0].body {
        IrNode::Ref(id) => assert_eq!(*id, 3),
        other => panic!("expected Ref, got {:?}", other),
    }

    // a's alias should point to 3 (canonical).
    assert_eq!(ir.rules[1].meta.is_alias, Some(3));
    // b's alias should point to 3 (canonical).
    assert_eq!(ir.rules[2].meta.is_alias, Some(3));
}
