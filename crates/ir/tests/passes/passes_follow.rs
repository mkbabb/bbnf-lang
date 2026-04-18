use std::collections::HashMap;

use bbnf_ir::passes::compute_follow_sets;
use bbnf_ir::{CharSet128, GrammarIR, IrNode, IrRule, RuleMeta};

#[test]
fn follow_set_basic_seq() {
    // Grammar: start = a , "x" ;  a = "y" ;
    // FOLLOW(a) should contain {x}
    let mut first_a = CharSet128::new();
    first_a.add(b'y');

    let ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![IrNode::Ref(1), IrNode::Literal(2)]),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'y');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(3),
                meta: RuleMeta {
                    first_set: first_a,
                    ..Default::default()
                },
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec!["start".into(), "a".into(), "x".into(), "y".into()],
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
        structural_alphabet: None,
        push_fingerprint: None,
            dedup_eligible_rules: Vec::new(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
    };

    let follow = compute_follow_sets(&ir);
    assert!(follow[&1].has(b'x'));
}

#[test]
fn follow_set_propagates_through_nullable() {
    // Grammar: start = a , b? , "z" ;  a = "x" ;  b = "y" ;
    // b is nullable (Optional -> Repeat{0,1}), so FOLLOW(a) should contain {y, z}
    let ir = GrammarIR {
        rules: vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Ref(1),
                    IrNode::Repeat {
                        inner: Box::new(IrNode::Ref(2)),
                        lo: 0,
                        hi: 1,
                    },
                    IrNode::Literal(3),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(4),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'x');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
            IrRule {
                id: 2,
                name: 2,
                body: IrNode::Literal(5),
                meta: RuleMeta {
                    first_set: {
                        let mut cs = CharSet128::new();
                        cs.add(b'y');
                        cs
                    },
                    ..Default::default()
                },
                source_span: None,
            },
        ],
        entry: 0,
        strings: vec![
            "start".into(),
            "a".into(),
            "b".into(),
            "z".into(),
            "x".into(),
            "y".into(),
        ],
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
        structural_alphabet: None,
        push_fingerprint: None,
            dedup_eligible_rules: Vec::new(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
    };

    let follow = compute_follow_sets(&ir);
    // FOLLOW(a) should contain 'y' (from FIRST(b?)) and 'z' (from literal after nullable b?).
    assert!(follow[&1].has(b'y'));
    assert!(follow[&1].has(b'z'));
}
