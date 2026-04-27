use std::collections::HashMap;

use bbnf_ir::bytecode::Op;
use bbnf_ir::compiler::compile;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, MemoStrategy, RuleMeta, TokenDispatchArm};

fn make_simple_ir(body: IrNode) -> GrammarIR {
    GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "hello".into(), "world".into()],
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

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    }
}

#[test]
fn compile_literal() {
    let ir = make_simple_ir(IrNode::Literal(1));
    let program = compile(&ir);

    assert_eq!(program.entries[0], 0);
    assert!(matches!(program.code[0], Op::MatchString(1)));
    // MakeTagged wraps the result with the rule name before Return.
    assert!(matches!(program.code[1], Op::MakeTagged(0)));
    assert!(matches!(program.code[2], Op::Return));
}

#[test]
fn compile_seq() {
    let ir = make_simple_ir(IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]));
    let program = compile(&ir);

    assert!(program.code.iter().any(|op| matches!(op, Op::SaveState)));
    assert!(
        program
            .code
            .iter()
            .any(|op| matches!(op, Op::MatchString(1)))
    );
    assert!(
        program
            .code
            .iter()
            .any(|op| matches!(op, Op::MatchString(2)))
    );
}

#[test]
fn compile_seq_nested_no_cross_patch() {
    // Nested Seq should NOT cross-patch JumpIfFail targets.
    // inner = Seq(["hello", "world"]), outer = Seq([inner, Literal("!")])
    let ir = make_simple_ir(IrNode::Seq(vec![
        IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        IrNode::Literal(1), // reuse "hello"
    ]));
    let program = compile(&ir);

    // All JumpIfFail targets should be nonzero (properly patched).
    for op in &program.code {
        if let Op::JumpIfFail(target) = op {
            assert_ne!(*target, 0, "Found unpatched JumpIfFail(0)");
        }
    }
}

#[test]
fn compile_alt() {
    let ir = make_simple_ir(IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
        ],
        None,
    ));
    let program = compile(&ir);

    assert!(program.code.iter().any(|op| matches!(op, Op::SaveState)));
    assert!(program.code.iter().any(|op| matches!(op, Op::RestoreState)));
}

#[test]
fn compile_repeat() {
    let ir = make_simple_ir(IrNode::Repeat {
        inner: Box::new(IrNode::Literal(1)),
        lo: 0,
        hi: u32::MAX,
    });
    let program = compile(&ir);

    assert!(
        program
            .code
            .iter()
            .any(|op| matches!(op, Op::RepeatBegin { .. }))
    );
    assert!(program.code.iter().any(|op| matches!(op, Op::RepeatEnd)));
}

#[test]
fn compile_memo_rule() {
    let ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Literal(1),
            meta: RuleMeta {
                memo: MemoStrategy::Full,
                ..Default::default()
            },
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
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

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    };
    let program = compile(&ir);

    assert!(!program.memo_enabled);
    assert!(
        !program
            .code
            .iter()
            .any(|op| matches!(op, Op::MemoCheck { .. }))
    );
    assert!(!program.code.iter().any(|op| matches!(op, Op::MemoStore(_))));
}

#[test]
fn compile_direct_left_recursive_rule_keeps_memo() {
    let ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Seq(vec![IrNode::Ref(0), IrNode::Literal(1)]),
                        first_set: None,
                    },
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: None,
                    },
                ],
                None,
            ),
            meta: RuleMeta {
                memo: MemoStrategy::Full,
                ..Default::default()
            },
            source_span: None,
        }],
        entry: 0,
        strings: vec!["rule".into(), "x".into()],
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

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    };
    let program = compile(&ir);

    assert!(program.memo_enabled);
    assert!(
        program
            .code
            .iter()
            .any(|op| matches!(op, Op::MemoCheck { .. }))
    );
    assert!(program.code.iter().any(|op| matches!(op, Op::MemoStore(_))));
}

#[test]
fn compile_alt_dispatch() {
    use bbnf_ir::AltDispatch;
    use bbnf_ir::CharSet128;

    // Two branches with pre-computed dispatch table -> should emit Dispatch.
    let mut fs_a = CharSet128::new();
    fs_a.add(b'a');
    let mut fs_b = CharSet128::new();
    fs_b.add(b'b');

    // Build dispatch table matching what the dispatch pass would produce.
    let mut table = vec![255u8; 128];
    table[b'a' as usize] = 0;
    table[b'b' as usize] = 1;

    let ir = make_simple_ir(IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: Some(fs_a),
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: Some(fs_b),
            },
        ],
        Some(AltDispatch {
            table,
            fallback_idx: None,
        }),
    ));
    let program = compile(&ir);

    // Should use Dispatch wrapped in SaveState/RestoreState for offset recovery.
    assert!(
        program.code.iter().any(|op| matches!(op, Op::Dispatch(..))),
        "Expected Dispatch for pre-computed dispatch table"
    );
    assert!(
        program.code.iter().any(|op| matches!(op, Op::SaveState)),
        "Dispatch should use SaveState for offset restoration on branch failure"
    );
    assert!(
        program.code.iter().any(|op| matches!(op, Op::RestoreState)),
        "Dispatch should use RestoreState for offset restoration on branch failure"
    );
}

#[test]
fn compile_alt_fallback_dispatch_uses_dispatch() {
    use bbnf_ir::AltDispatch;
    use bbnf_ir::CharSet128;

    let mut fs_a = CharSet128::new();
    fs_a.add(b'a');
    let mut fs_b = CharSet128::new();
    fs_b.add(b'b');
    let mut fs_ab = CharSet128::new();
    fs_ab.add(b'a');
    fs_ab.add(b'b');

    let mut table = vec![255u8; 128];
    table[b'a' as usize] = 0;
    table[b'b' as usize] = 1;

    let ir = make_simple_ir(IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: Some(fs_a),
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: Some(fs_b),
            },
            AltBranch {
                node: IrNode::Regex(1),
                first_set: Some(fs_ab),
            },
        ],
        Some(AltDispatch {
            table,
            fallback_idx: Some(2),
        }),
    ));
    let program = compile(&ir);

    assert!(
        program.code.iter().any(|op| matches!(op, Op::Dispatch(..))),
        "Fallback-aware dispatch should still compile to Dispatch"
    );
}

#[test]
fn compile_token_dispatch_emits_dispatch_token() {
    let ir = GrammarIR {
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::TokenDispatch {
                token: Box::new(IrNode::Regex(1)),
                arms: vec![TokenDispatchArm {
                    patterns: vec![2],
                    guard_byte: Some(b'('),
                    continuation: IrNode::Literal(3),
                    map_fn: None,
                }],
                fallback: Box::new(IrNode::Literal(4)),
            },
            meta: RuleMeta::default(),
            source_span: None,
        }],
        entry: 0,
        strings: vec![
            "rule".into(),
            "[a-z]+".into(),
            "calc".into(),
            "(".into(),
            "fallback".into(),
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

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    };
    let program = compile(&ir);

    assert!(
        program
            .code
            .iter()
            .any(|op| matches!(op, Op::DispatchToken(..))),
        "TokenDispatch should compile to DispatchToken"
    );
}

#[test]
fn compile_alt_no_dispatch_uses_backtracking() {
    // No dispatch table -> should fall back to backtracking.
    let ir = make_simple_ir(IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
        ],
        None,
    ));
    let program = compile(&ir);

    // No dispatch, uses SaveState.
    assert!(
        !program.code.iter().any(|op| matches!(op, Op::Dispatch(..))),
        "No dispatch table should not use Dispatch"
    );
    assert!(program.code.iter().any(|op| matches!(op, Op::SaveState)));
}

#[test]
fn compile_call() {
    let ir = GrammarIR {
        rules: vec![
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
        entry: 0,
        strings: vec!["start".into(), "item".into(), "x".into()],
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

            shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
    };
    let program = compile(&ir);

    assert!(program.code.iter().any(|op| matches!(op, Op::Call(1))));
}
