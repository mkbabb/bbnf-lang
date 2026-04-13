use std::collections::HashMap;

use bbnf_ir::passes::{compute_structural_bytes, generate_dispatch_tables};
use bbnf_ir::{AltBranch, CharSet128, GrammarIR, IrNode, IrRule, RuleMeta};

/// Helper: build a minimal GrammarIR with the given rules.
fn make_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
    let mut ir = GrammarIR {
        rules,
        entry: 0,
        strings,
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
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        structural_bytes: None,
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    ir
}

/// JSON-like grammar: value = object | array | string | number | "true" | "false" | "null"
///
/// After dispatch, the number branch maps 11 bytes (-, 0-9) to the same
/// branch index. Those should be excluded as a character class.
#[test]
fn structural_bytes_excludes_char_class_branches() {
    let mut first_obj = CharSet128::new();
    first_obj.add(b'{');
    let mut first_arr = CharSet128::new();
    first_arr.add(b'[');
    let mut first_str = CharSet128::new();
    first_str.add(b'"');
    let mut first_num = CharSet128::new();
    first_num.add(b'-');
    for b in b'0'..=b'9' {
        first_num.add(b);
    }
    let mut first_true = CharSet128::new();
    first_true.add(b't');
    let mut first_false = CharSet128::new();
    first_false.add(b'f');
    let mut first_null = CharSet128::new();
    first_null.add(b'n');

    let mut ir = make_ir(
        vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Literal(1), first_set: Some(first_obj) },
                    AltBranch { node: IrNode::Literal(2), first_set: Some(first_arr) },
                    AltBranch { node: IrNode::Regex(3), first_set: Some(first_str) },
                    AltBranch { node: IrNode::Regex(4), first_set: Some(first_num) },
                    AltBranch { node: IrNode::Literal(5), first_set: Some(first_true) },
                    AltBranch { node: IrNode::Literal(6), first_set: Some(first_false) },
                    AltBranch { node: IrNode::Literal(7), first_set: Some(first_null) },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        vec![
            "value".into(), "{".into(), "[".into(),
            r#""str""#.into(), r#"-?(0|[1-9]\d*)"#.into(),
            "true".into(), "false".into(), "null".into(),
        ],
    );

    generate_dispatch_tables(&mut ir);

    // Dispatch table should exist.
    assert!(
        matches!(&ir.rules[0].body, IrNode::Alt(_, Some(_))),
        "dispatch table should be created"
    );

    compute_structural_bytes(&mut ir);

    let bytes = ir.structural_bytes.as_ref().expect("structural_bytes should be Some");

    // Structural delimiters (single-byte branches) should be included.
    assert!(bytes.contains(&b'{'), "missing {{");
    assert!(bytes.contains(&b'['), "missing [");
    assert!(bytes.contains(&b'"'), "missing quote");
    assert!(bytes.contains(&b't'), "missing t");
    assert!(bytes.contains(&b'f'), "missing f");
    assert!(bytes.contains(&b'n'), "missing n");

    // Character-class bytes (digits, minus) should NOT be included.
    assert!(!bytes.contains(&b'0'), "digit 0 should be excluded as char class");
    assert!(!bytes.contains(&b'9'), "digit 9 should be excluded as char class");
    assert!(!bytes.contains(&b'-'), "minus should be excluded as char class");
}

/// When all branches have single-byte FIRST sets, all bytes are structural.
#[test]
fn structural_bytes_all_single_byte_branches() {
    let mut first_a = CharSet128::new();
    first_a.add(b'{');
    let mut first_b = CharSet128::new();
    first_b.add(b'[');
    let mut first_c = CharSet128::new();
    first_c.add(b'"');

    let mut ir = make_ir(
        vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Alt(
                vec![
                    AltBranch { node: IrNode::Literal(1), first_set: Some(first_a) },
                    AltBranch { node: IrNode::Literal(2), first_set: Some(first_b) },
                    AltBranch { node: IrNode::Literal(3), first_set: Some(first_c) },
                ],
                None,
            ),
            meta: RuleMeta::default(),
            source_span: None,
        }],
        vec!["rule".into(), "{".into(), "[".into(), "\"".into()],
    );

    generate_dispatch_tables(&mut ir);
    compute_structural_bytes(&mut ir);

    let bytes = ir.structural_bytes.as_ref().expect("structural_bytes should be Some");
    assert!(bytes.contains(&b'{'));
    assert!(bytes.contains(&b'['));
    assert!(bytes.contains(&b'"'));
}

/// TokenDispatch guard bytes are structural.
#[test]
fn structural_bytes_includes_token_dispatch_guard_bytes() {
    let mut ir = make_ir(
        vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::TokenDispatch {
                token: Box::new(IrNode::Regex(1)),
                arms: vec![
                    bbnf_ir::TokenDispatchArm {
                        patterns: vec![2],
                        guard_byte: Some(b'('),
                        continuation: IrNode::Literal(3),
                        map_fn: None,
                    },
                    bbnf_ir::TokenDispatchArm {
                        patterns: vec![4],
                        guard_byte: Some(b'['),
                        continuation: IrNode::Literal(5),
                        map_fn: None,
                    },
                ],
                fallback: Box::new(IrNode::Epsilon),
            },
            meta: RuleMeta::default(),
            source_span: None,
        }],
        vec![
            "rule".into(), "[a-z]+".into(), "func".into(),
            "body".into(), "index".into(), "idx_body".into(),
        ],
    );

    compute_structural_bytes(&mut ir);

    let bytes = ir.structural_bytes.as_ref().expect("structural_bytes should be Some");
    assert!(bytes.contains(&b'('), "missing guard byte (");
    assert!(bytes.contains(&b'['), "missing guard byte [");
    assert!(bytes.contains(&b'"'), "missing hardcoded quote");
}

/// Nested Alt with dispatch inside a Seq also contributes structural bytes.
#[test]
fn structural_bytes_from_nested_dispatch() {
    // object = "{" >> pair* << "}"
    // pair uses an Alt with dispatch for value

    let mut first_open = CharSet128::new();
    first_open.add(b'{');
    let mut first_close = CharSet128::new();
    first_close.add(b'}');

    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![
                    IrNode::Literal(1), // "{"
                    IrNode::Alt(
                        vec![
                            AltBranch {
                                node: IrNode::Literal(2), // starts with {
                                first_set: Some(first_open),
                            },
                            AltBranch {
                                node: IrNode::Literal(3), // starts with }
                                first_set: Some(first_close),
                            },
                        ],
                        None,
                    ),
                ]),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        vec!["rule".into(), "{".into(), "{nested".into(), "}close".into()],
    );

    generate_dispatch_tables(&mut ir);
    compute_structural_bytes(&mut ir);

    let bytes = ir.structural_bytes.as_ref().expect("structural_bytes should be Some");
    assert!(bytes.contains(&b'{'), "missing nested {{ from dispatch");
    assert!(bytes.contains(&b'}'), "missing nested }} from dispatch");
}
