use std::collections::HashMap;

use bbnf_ir::passes::prune_unreachable;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

fn make_rule(id: RuleId, name_id: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name: name_id,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

#[test]
fn prune_removes_unreachable() {
    let mut ir = GrammarIR {
        rules: vec![
            make_rule(0, 0, IrNode::Ref(1)),     // start -> a
            make_rule(1, 1, IrNode::Literal(2)), // a -> "x"
            make_rule(2, 3, IrNode::Literal(4)), // dead -> "y"
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
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Ref(1),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Ref(2),
                            first_set: None,
                        },
                    ],
                    None,
                ),
            ),
            make_rule(1, 1, IrNode::Literal(3)),
            make_rule(2, 2, IrNode::Literal(4)),
            make_rule(3, 5, IrNode::Literal(6)), // dead
        ],
        entry: 0,
        strings: vec![
            "start".into(),
            "a".into(),
            "b".into(),
            "x".into(),
            "y".into(),
            "dead".into(),
            "z".into(),
        ],
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
    };

    prune_unreachable(&mut ir);
    assert_eq!(ir.rules.len(), 3); // start, a, b -- dead removed
}
