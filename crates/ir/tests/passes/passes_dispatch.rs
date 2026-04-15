use std::collections::HashMap;

use bbnf_ir::passes::generate_dispatch_tables;
use bbnf_ir::{AltBranch, CharSet128, GrammarIR, IrNode, IrRule, RuleMeta};

#[test]
fn dispatch_for_disjoint_branches() {
    let mut first_a = CharSet128::new();
    first_a.add(b't'); // "true"
    let mut first_b = CharSet128::new();
    first_b.add(b'f'); // "false"
    let mut first_c = CharSet128::new();
    first_c.add(b'n'); // "null"

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                    AltBranch {
                        node: IrNode::Literal(3),
                        first_set: Some(first_c),
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["value".into(), "true".into(), "false".into(), "null".into()],
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, Some(dispatch)) => {
            assert_eq!(dispatch.table[b't' as usize], 0);
            assert_eq!(dispatch.table[b'f' as usize], 1);
            assert_eq!(dispatch.table[b'n' as usize], 2);
            assert_eq!(dispatch.table[b'a' as usize], 255);
        }
        other => panic!("expected Alt with dispatch, got {:?}", other),
    }
}

#[test]
fn no_dispatch_for_overlapping_branches() {
    let mut first_a = CharSet128::new();
    first_a.add(b'"');
    let mut first_b = CharSet128::new();
    first_b.add(b'"'); // Overlaps!

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "a".into(), "b".into()],
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);
    match &ir.rules[0].body {
        IrNode::Alt(_, dispatch) => assert!(dispatch.is_none()),
        other => panic!("expected Alt, got {:?}", other),
    }
}

#[test]
fn dispatch_for_nested_alt() {
    // Alt nested inside Seq should also get annotated.
    let mut first_a = CharSet128::new();
    first_a.add(b'a');
    let mut first_b = CharSet128::new();
    first_b.add(b'b');

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Seq(vec![
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Literal(1),
                            first_set: Some(first_a),
                        },
                        AltBranch {
                            node: IrNode::Literal(2),
                            first_set: Some(first_b),
                        },
                    ],
                    None,
                ),
                IrNode::Literal(3),
            ]),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "a".into(), "b".into(), "end".into()],
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    // The nested Alt should have been annotated.
    match &ir.rules[0].body {
        IrNode::Seq(children) => match &children[0] {
            IrNode::Alt(_, dispatch) => assert!(dispatch.is_some()),
            other => panic!("expected Alt, got {:?}", other),
        },
        other => panic!("expected Seq, got {:?}", other),
    }
}

#[test]
fn dispatch_with_nullable_branch_via_follow() {
    // Grammar: rule = "x" | epsilon ;  FOLLOW(rule) = {';'}
    // Branch 0: FIRST = {'x'}, Branch 1: nullable (epsilon).
    // FOLLOW = {';'} is disjoint from {'x'}, so dispatch should work.
    let mut first_a = CharSet128::new();
    first_a.add(b'x');

    let mut follow_rule = CharSet128::new();
    follow_rule.add(b';');

    let mut follow_sets = HashMap::new();
    follow_sets.insert(0u32, follow_rule);

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Epsilon,
                        first_set: None, // Nullable branch.
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets,
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, Some(dispatch)) => {
            // 'x' -> branch 0.
            assert_eq!(dispatch.table[b'x' as usize], 0);
            // ';' in FOLLOW -> branch 1 (nullable).
            assert_eq!(dispatch.table[b';' as usize], 1);
            // Other bytes -> no match.
            assert_eq!(dispatch.table[b'a' as usize], 255);
        }
        other => panic!("expected Alt with dispatch, got {:?}", other),
    }
}

#[test]
fn no_dispatch_when_nullable_overlaps_follow() {
    // Branch 0: FIRST = {'x'}, Branch 1: nullable.
    // FOLLOW(rule) = {'x'} -- overlaps with branch 0, so no dispatch.
    let mut first_a = CharSet128::new();
    first_a.add(b'x');

    let mut follow_rule = CharSet128::new();
    follow_rule.add(b'x'); // Overlaps!

    let mut follow_sets = HashMap::new();
    follow_sets.insert(0u32, follow_rule);

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Epsilon,
                        first_set: None,
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
        fns: vec![],
        types: vec![],
        follow_sets,
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, dispatch) => assert!(dispatch.is_none()),
        other => panic!("expected Alt, got {:?}", other),
    }
}

// ── Fallback dispatch tests ─────────────────────────────────────────────────

#[test]
fn fallback_dispatch_typed_plus_catchall() {
    // 3 typed branches (disjoint) + 1 catch-all (superset).
    let mut first_a = CharSet128::new();
    first_a.add(b'a');
    let mut first_b = CharSet128::new();
    first_b.add(b'b');
    let mut first_c = CharSet128::new();
    first_c.add(b'c');
    let mut first_all = CharSet128::new();
    for b in b'a'..=b'z' {
        first_all.add(b);
    }

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                    AltBranch {
                        node: IrNode::Literal(3),
                        first_set: Some(first_c),
                    },
                    AltBranch {
                        node: IrNode::Regex(4),
                        first_set: Some(first_all),
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec![
            "rule".into(),
            "aa".into(),
            "bb".into(),
            "cc".into(),
            "[a-z]+".into(),
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, Some(dispatch)) => {
            assert_eq!(dispatch.fallback_idx, Some(3));
            assert_eq!(dispatch.table[b'a' as usize], 0);
            assert_eq!(dispatch.table[b'b' as usize], 1);
            assert_eq!(dispatch.table[b'c' as usize], 2);
            assert_eq!(dispatch.table[b'd' as usize], 255);
        }
        other => panic!("expected Alt with fallback dispatch, got {:?}", other),
    }
}

#[test]
fn fallback_dispatch_not_superset() {
    // Typed branches overlap with catch-all (strict dispatch fails),
    // but catch-all FIRST {a, x} is NOT a superset of typed union {a, b} → no fallback.
    let mut first_a = CharSet128::new();
    first_a.add(b'a');
    let mut first_b = CharSet128::new();
    first_b.add(b'b');
    let mut first_partial = CharSet128::new();
    first_partial.add(b'a'); // Overlaps with branch 0 → strict dispatch fails
    first_partial.add(b'x'); // But doesn't cover 'b' → not a superset

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(first_b),
                    },
                    AltBranch {
                        node: IrNode::Regex(3),
                        first_set: Some(first_partial),
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "aa".into(), "bb".into(), "[xy]+".into()],
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, disp) => assert!(disp.is_none()),
        other => panic!("expected Alt without dispatch, got {:?}", other),
    }
}

#[test]
fn fallback_dispatch_too_few_branches() {
    // Only 2 branches — fallback dispatch requires ≥3.
    let mut first_a = CharSet128::new();
    first_a.add(b'a');
    let mut first_all = CharSet128::new();
    for b in b'a'..=b'z' {
        first_all.add(b);
    }

    let mut ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(first_a),
                    },
                    AltBranch {
                        node: IrNode::Regex(2),
                        first_set: Some(first_all),
                    },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "aa".into(), "[a-z]+".into()],
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
    };

    bbnf_ir::dag::ensure_dag(&mut ir);
    generate_dispatch_tables(&mut ir);

    match &ir.rules[0].body {
        IrNode::Alt(_, disp) => assert!(disp.is_none()),
        other => panic!("expected Alt without dispatch, got {:?}", other),
    }
}
