use std::collections::HashMap;

use bbnf_ir::passes::{NoopTraceSink, inline_acyclic};
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
        regex_info: std::collections::HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
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

    inline_acyclic(&mut ir, &mut NoopTraceSink);
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

    inline_acyclic(&mut ir, &mut NoopTraceSink);
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

    inline_acyclic(&mut ir, &mut NoopTraceSink);
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

    inline_acyclic(&mut ir, &mut NoopTraceSink);
    // Rule 0 should still be Ref(1) -- rule 1 is too large.
    assert_eq!(ir.rules[0].body, IrNode::Ref(1));
}
