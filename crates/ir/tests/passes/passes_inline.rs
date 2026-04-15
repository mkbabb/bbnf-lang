use std::collections::HashMap;

use bbnf_ir::passes::inline_acyclic;
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

fn make_ir(rules: Vec<IrRule>, entry: RuleId) -> GrammarIR {
    GrammarIR {
        rules,
        entry,
        strings: vec!["entry".into(), "small".into(), "a".into(), "b".into()],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None, cost_config: bbnf_ir::CostConfig::default(), type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
    }
}

#[test]
fn small_acyclic_inlined() {
    // Rule 0 (entry): Ref(1)
    // Rule 1 (small): Literal(2)  -- 1 node, inlinable
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        0,
    );

    inline_acyclic(&mut ir);
    // Rule 0 should now have Literal(2) instead of Ref(1).
    assert_eq!(ir.rules[0].body, IrNode::Literal(2));
}

#[test]
fn cyclic_not_inlined() {
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(2),
                meta: RuleMeta {
                    is_cyclic: true,
                    scc_id: Some(0),
                    ..Default::default()
                },
                source_span: None,
            },
        ],
        0,
    );

    inline_acyclic(&mut ir);
    // Rule 0 should still be Ref(1) -- rule 1 is cyclic.
    assert_eq!(ir.rules[0].body, IrNode::Ref(1));
}

#[test]
fn entry_point_not_inlined() {
    // Entry rule is small but should not be inlined into itself.
    let mut ir = make_ir(
        vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Literal(2),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        0,
    );

    inline_acyclic(&mut ir);
    assert_eq!(ir.rules[0].body, IrNode::Literal(2));
}

#[test]
fn large_rule_not_inlined() {
    // Rule 1 has 5 nodes (Seq + 4 Literal children), over INLINE_THRESHOLD (4).
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Seq(vec![
                    IrNode::Literal(2),
                    IrNode::Literal(3),
                    IrNode::Literal(2),
                    IrNode::Literal(3),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        0,
    );

    inline_acyclic(&mut ir);
    // Rule 0 should still be Ref(1) -- rule 1 is too large.
    assert_eq!(ir.rules[0].body, IrNode::Ref(1));
}
