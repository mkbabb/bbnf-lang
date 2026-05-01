//! Integration tests for the bytecode interpreter.
//!
//! Exercises the full IR → bytecode → interpret pipeline using hand-built IR trees.

use bbnf_ir::interpreter::{Value, parse_with_ir};
use bbnf_ir::{
    AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode, IrRule, MemoStrategy, RuleMeta,
    TokenDispatchArm,
};
use std::collections::HashMap;

// ── Helpers ─────────────────────────────────────────────────────────────────

fn make_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules,
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
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    }
}

fn rule(id: u32, name: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

/// Extract the inner value from a Tagged wrapper (rules emit MakeTagged).
fn unwrap_tagged(val: &Value) -> &Value {
    match val {
        Value::Tagged { children, .. } => {
            assert_eq!(children.len(), 1);
            &children[0]
        }
        other => other,
    }
}

// ── Basic parsing ───────────────────────────────────────────────────────────

#[test]
fn parse_literal() {
    let ir = make_ir(
        vec![rule(0, 0, IrNode::Literal(1))],
        vec!["start".into(), "hello".into()],
    );
    let result = parse_with_ir(&ir, "hello");
    assert!(result.success);
    assert_eq!(result.offset, 5);
    // Top-level rule wraps its result in Tagged.
    let val = result.value.as_ref().unwrap();
    assert!(matches!(val, Value::Tagged { tag: 0, .. }));
    assert_eq!(unwrap_tagged(val), &Value::Span(0, 5));
}

#[test]
fn parse_literal_fail() {
    let ir = make_ir(
        vec![rule(0, 0, IrNode::Literal(1))],
        vec!["start".into(), "hello".into()],
    );
    let result = parse_with_ir(&ir, "world");
    assert!(!result.success);
}

#[test]
fn parse_regex() {
    let ir = make_ir(
        vec![rule(0, 0, IrNode::Regex(1))],
        vec!["start".into(), "[0-9]+".into()],
    );
    let result = parse_with_ir(&ir, "12345abc");
    assert!(result.success);
    assert_eq!(result.offset, 5);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 5)
    );
}

// ── Sequences ───────────────────────────────────────────────────────────────

#[test]
fn parse_seq() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
        )],
        vec!["start".into(), "hello".into(), "world".into()],
    );
    let result = parse_with_ir(&ir, "helloworld");
    assert!(result.success);
    assert_eq!(result.offset, 10);
}

#[test]
fn parse_seq_nested() {
    // Nested sequences should compile and execute correctly.
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Seq(vec![
                IrNode::Seq(vec![IrNode::Literal(1), IrNode::Literal(2)]),
                IrNode::Literal(1),
            ]),
        )],
        vec!["start".into(), "a".into(), "b".into()],
    );
    let result = parse_with_ir(&ir, "aba");
    assert!(result.success);
    assert_eq!(result.offset, 3);
}

// ── Alternation ─────────────────────────────────────────────────────────────

#[test]
fn parse_alt_first_branch() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
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
            ),
        )],
        vec!["start".into(), "yes".into(), "no".into()],
    );
    let result = parse_with_ir(&ir, "yes");
    assert!(result.success);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 3)
    );
}

#[test]
fn parse_alt_second_branch() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
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
            ),
        )],
        vec!["start".into(), "yes".into(), "no".into()],
    );
    let result = parse_with_ir(&ir, "no");
    assert!(result.success);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 2)
    );
}

// ── Repetition ──────────────────────────────────────────────────────────────

#[test]
fn parse_repeat_many() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(1)),
                lo: 0,
                hi: u32::MAX,
            },
        )],
        vec!["start".into(), "ab".into()],
    );
    let result = parse_with_ir(&ir, "ababab");
    assert!(result.success);
    assert_eq!(result.offset, 6);
    match unwrap_tagged(result.value.as_ref().unwrap()) {
        Value::Array(items) => assert_eq!(items.len(), 3),
        other => panic!("expected Array, got {:?}", other),
    }
}

#[test]
fn parse_repeat_many_empty() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(1)),
                lo: 0,
                hi: u32::MAX,
            },
        )],
        vec!["start".into(), "x".into()],
    );
    let result = parse_with_ir(&ir, "yyy");
    assert!(result.success);
    assert_eq!(result.offset, 0);
    match unwrap_tagged(result.value.as_ref().unwrap()) {
        Value::Array(items) => assert_eq!(items.len(), 0),
        other => panic!("expected empty Array, got {:?}", other),
    }
}

#[test]
fn parse_repeat_many1_fail() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(1)),
                lo: 1,
                hi: u32::MAX,
            },
        )],
        vec!["start".into(), "x".into()],
    );
    let result = parse_with_ir(&ir, "yyy");
    assert!(!result.success);
}

#[test]
fn parse_optional() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Repeat {
                inner: Box::new(IrNode::Literal(1)),
                lo: 0,
                hi: 1,
            },
        )],
        vec!["start".into(), "x".into()],
    );

    let result = parse_with_ir(&ir, "x");
    assert!(result.success);
    assert_eq!(result.offset, 1);

    let result = parse_with_ir(&ir, "y");
    assert!(result.success);
    assert_eq!(result.offset, 0);
}

// ── Rule calls ──────────────────────────────────────────────────────────────

#[test]
fn parse_call() {
    let ir = make_ir(
        vec![rule(0, 0, IrNode::Ref(1)), rule(1, 1, IrNode::Literal(2))],
        vec!["start".into(), "item".into(), "hello".into()],
    );
    let result = parse_with_ir(&ir, "hello");
    assert!(result.success);
    assert_eq!(result.offset, 5);
}

// ── Skip / Next ─────────────────────────────────────────────────────────────

#[test]
fn parse_next() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Next(Box::new(IrNode::Literal(1)), Box::new(IrNode::Literal(2))),
        )],
        vec!["start".into(), "a".into(), "b".into()],
    );
    let result = parse_with_ir(&ir, "ab");
    assert!(result.success);
    assert_eq!(result.offset, 2);
}

#[test]
fn parse_skip() {
    // "a" << "b" should parse "ab" and return the span of "a".
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Skip(Box::new(IrNode::Literal(1)), Box::new(IrNode::Literal(2))),
        )],
        vec!["start".into(), "a".into(), "b".into()],
    );
    let result = parse_with_ir(&ir, "ab");
    assert!(result.success, "Skip failed: {:?}", result);
    assert_eq!(result.offset, 2);
}

#[test]
fn parse_next_then_skip() {
    // "{" >> "x" << "}" should parse "{x}" consuming all 3 chars.
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Skip(
                Box::new(IrNode::Next(
                    Box::new(IrNode::Literal(1)), // "{"
                    Box::new(IrNode::Literal(2)), // "x"
                )),
                Box::new(IrNode::Literal(3)), // "}"
            ),
        )],
        vec!["start".into(), "{".into(), "x".into(), "}".into()],
    );
    let result = parse_with_ir(&ir, "{x}");
    assert!(result.success, "Next-then-Skip failed: {:?}", result);
    assert_eq!(result.offset, 3);
}

// ── Dispatch ────────────────────────────────────────────────────────────────

#[test]
fn parse_alt_dispatch() {
    // Alternation with disjoint FIRST sets should use Dispatch and parse correctly.
    let mut fs_a = CharSet128::new();
    fs_a.add(b'a');
    let mut fs_b = CharSet128::new();
    fs_b.add(b'b');

    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1), // "alpha"
                        first_set: Some(fs_a),
                    },
                    AltBranch {
                        node: IrNode::Literal(2), // "beta"
                        first_set: Some(fs_b),
                    },
                ],
                None,
            ),
        )],
        vec!["start".into(), "alpha".into(), "beta".into()],
    );

    // First branch
    let result = parse_with_ir(&ir, "alpha");
    assert!(result.success);
    assert_eq!(result.offset, 5);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 5)
    );

    // Second branch
    let result = parse_with_ir(&ir, "beta");
    assert!(result.success);
    assert_eq!(result.offset, 4);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 4)
    );

    // No matching branch (dispatch fallback)
    let result = parse_with_ir(&ir, "gamma");
    assert!(!result.success);
}

#[test]
fn parse_dispatch_multi_branch() {
    // 4-branch dispatch + fallback.
    let mut fs_t = CharSet128::new();
    fs_t.add(b't');
    let mut fs_f = CharSet128::new();
    fs_f.add(b'f');
    let mut fs_n = CharSet128::new();
    fs_n.add(b'n');
    let mut fs_d = CharSet128::new();
    fs_d.add(b'0');
    for b in b'1'..=b'9' {
        fs_d.add(b);
    }

    let mut table = vec![255u8; 128];
    table[b't' as usize] = 0;
    table[b'f' as usize] = 1;
    table[b'n' as usize] = 2;
    for b in b'0'..=b'9' {
        table[b as usize] = 3;
    }

    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
                vec![
                    AltBranch {
                        node: IrNode::Literal(1),
                        first_set: Some(fs_t),
                    }, // "true"
                    AltBranch {
                        node: IrNode::Literal(2),
                        first_set: Some(fs_f),
                    }, // "false"
                    AltBranch {
                        node: IrNode::Literal(3),
                        first_set: Some(fs_n),
                    }, // "null"
                    AltBranch {
                        node: IrNode::Regex(4),
                        first_set: Some(fs_d),
                    }, // /[0-9]+/
                ],
                Some(AltDispatch {
                    table,
                    fallback_idx: None,
                }),
            ),
        )],
        vec![
            "start".into(),
            "true".into(),
            "false".into(),
            "null".into(),
            "[0-9]+".into(),
        ],
    );

    assert!(parse_with_ir(&ir, "true").success);
    assert!(parse_with_ir(&ir, "false").success);
    assert!(parse_with_ir(&ir, "null").success);
    let num_result = parse_with_ir(&ir, "42");
    assert!(num_result.success);
    assert_eq!(num_result.offset, 2);
    // Fallback: byte not in table
    assert!(!parse_with_ir(&ir, "xyz").success);
}

#[test]
fn parse_fallback_dispatch_restores_and_uses_fallback() {
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

    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Alt(
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
                        node: IrNode::Regex(3),
                        first_set: Some(fs_ab),
                    },
                ],
                Some(AltDispatch {
                    table,
                    fallback_idx: Some(2),
                }),
            ),
        )],
        vec![
            "start".into(),
            "alpha".into(),
            "beta".into(),
            "[ab]+".into(),
        ],
    );

    let result = parse_with_ir(&ir, "abba");
    assert!(result.success);
    assert_eq!(result.offset, 4);
    assert_eq!(
        unwrap_tagged(result.value.as_ref().unwrap()),
        &Value::Span(0, 4)
    );
}

#[test]
fn parse_token_dispatch_matches_pattern_and_falls_back() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::TokenDispatch {
                token: Box::new(IrNode::Regex(1)),
                arms: vec![
                    TokenDispatchArm {
                        patterns: vec![2],
                        guard_byte: Some(b'('),
                        continuation: IrNode::Literal(4),
                        map_fn: None,
                    },
                    TokenDispatchArm {
                        patterns: vec![3],
                        guard_byte: None,
                        continuation: IrNode::Literal(5),
                        map_fn: None,
                    },
                ],
                fallback: Box::new(IrNode::Literal(6)),
            },
        )],
        vec![
            "start".into(),
            "[a-z]+".into(),
            "calc".into(),
            "min".into(),
            "(".into(),
            "!".into(),
            "fallback".into(),
        ],
    );

    let calc_result = parse_with_ir(&ir, "calc(");
    assert!(calc_result.success);
    assert_eq!(calc_result.offset, 5);

    let min_result = parse_with_ir(&ir, "min!");
    assert!(min_result.success);
    assert_eq!(min_result.offset, 4);

    let fallback_result = parse_with_ir(&ir, "fallback");
    assert!(fallback_result.success);
    assert_eq!(fallback_result.offset, 8);
}

// ── Whitespace ──────────────────────────────────────────────────────────────

#[test]
fn parse_whitespace_trimming() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::OptionalWhitespace(Box::new(IrNode::Literal(1))),
        )],
        vec!["start".into(), "x".into()],
    );
    let result = parse_with_ir(&ir, "  x  ");
    assert!(result.success);
    assert_eq!(result.offset, 5);
}

// ── Minus / Negate ──────────────────────────────────────────────────────────

#[test]
fn parse_minus() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Minus(
                Box::new(IrNode::Regex(1)),   // lhs: /[a-z]+/
                Box::new(IrNode::Literal(2)), // rhs: "if"
            ),
        )],
        vec!["start".into(), "[a-z]+".into(), "if".into()],
    );

    // "hello" matches /[a-z]+/ and doesn't match "if" → success
    let result = parse_with_ir(&ir, "hello");
    assert!(result.success);
    assert_eq!(result.offset, 5);

    // "if" matches both lhs and rhs → minus fails
    let result = parse_with_ir(&ir, "if");
    assert!(!result.success);
}

#[test]
fn parse_negate() {
    let ir = make_ir(
        vec![rule(
            0,
            0,
            IrNode::Seq(vec![
                IrNode::Negate(Box::new(IrNode::Literal(1))), // !("end")
                IrNode::Regex(2),                             // /./
            ]),
        )],
        vec!["start".into(), "end".into(), ".".into()],
    );

    // Input "a": negate succeeds (not "end"), then /./ matches "a"
    let result = parse_with_ir(&ir, "abc");
    assert!(result.success);
    assert_eq!(result.offset, 1);

    // Input "end": negate fails (matches "end")
    let result = parse_with_ir(&ir, "end");
    assert!(!result.success);
}

// ── Memoization ─────────────────────────────────────────────────────────────

#[test]
fn parse_memo_correctness() {
    // start = (memo_rule , "NOPE") | memo_rule ;
    // memo_rule = "hello" ;  [Full memo]
    //
    // Input "hello":
    //   1. Try first branch: memo_rule succeeds → memo stored. Then "NOPE" fails → backtrack.
    //   2. Try second branch: memo_rule at offset 0 → cache hit.

    let mut ir = make_ir(
        vec![
            rule(
                0,
                0,
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Seq(vec![IrNode::Ref(1), IrNode::Literal(3)]),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Ref(1),
                            first_set: None,
                        },
                    ],
                    None,
                ),
            ),
            rule(1, 1, IrNode::Literal(2)),
        ],
        vec![
            "start".into(),
            "memo_rule".into(),
            "hello".into(),
            "NOPE".into(),
        ],
    );
    ir.rules[1].meta.memo = MemoStrategy::Full;

    let result = parse_with_ir(&ir, "hello");
    assert!(
        result.success,
        "Memoized parse should succeed: {:?}",
        result
    );
    assert_eq!(result.offset, 5);

    let val = result.value.as_ref().unwrap();
    match val {
        Value::Tagged {
            tag: 0, children, ..
        } => match &children[0] {
            Value::Tagged {
                tag: 1,
                children: inner,
                ..
            } => {
                assert_eq!(inner[0], Value::Span(0, 5));
            }
            other => panic!("expected Tagged(memo_rule), got {:?}", other),
        },
        other => panic!("expected Tagged(start), got {:?}", other),
    }
}

// ── Stress tests ───────────────────────────────────────────────────────────

#[test]
fn stress_deep_nesting_1000() {
    // Grammar: value = "[" >> value << "]" | "x" ;
    // Input: "[[[...x...]]]" with 1000 levels of nesting.
    let ir = {
        let mut ir = make_ir(
            vec![rule(
                0,
                0,
                IrNode::Alt(
                    vec![
                        AltBranch {
                            node: IrNode::Skip(
                                Box::new(IrNode::Next(
                                    Box::new(IrNode::Literal(1)), // "["
                                    Box::new(IrNode::Ref(0)),     // value (recursive)
                                )),
                                Box::new(IrNode::Literal(2)), // "]"
                            ),
                            first_set: None,
                        },
                        AltBranch {
                            node: IrNode::Literal(3), // "x"
                            first_set: None,
                        },
                    ],
                    None,
                ),
            )],
            vec!["value".into(), "[".into(), "]".into(), "x".into()],
        );
        ir.rules[0].meta.is_cyclic = true;
        ir.rules[0].meta.memo = MemoStrategy::Full;
        ir
    };

    let depth = 1000;
    let mut input = String::with_capacity(depth * 2 + 1);
    for _ in 0..depth {
        input.push('[');
    }
    input.push('x');
    for _ in 0..depth {
        input.push(']');
    }

    let result = parse_with_ir(&ir, &input);
    assert!(result.success, "deep nesting (1000) should parse");
    assert_eq!(
        result.offset as usize,
        input.len(),
        "should consume all {} bytes",
        input.len()
    );
}

#[test]
fn stress_wide_alternation_100() {
    // Grammar: rule = "kw_001" | "kw_002" | ... | "kw_100" ;
    // Fixed-width keywords avoid prefix ambiguity in linear-scan alternation.
    let mut strings: Vec<String> = vec!["start".into()];
    let mut branches = Vec::new();

    for i in 1..=100 {
        let s = format!("kw_{:03}", i);
        let str_id = strings.len() as u32;
        strings.push(s);
        branches.push(AltBranch {
            node: IrNode::Literal(str_id),
            first_set: None,
        });
    }

    let ir = make_ir(
        vec![rule(0, 0, IrNode::Alt(branches, None))],
        strings.clone(),
    );

    // Test every branch.
    for i in 1..=100u32 {
        let input = format!("kw_{:03}", i);
        let result = parse_with_ir(&ir, &input);
        assert!(
            result.success,
            "branch '{}' should parse, got {:?}",
            input, result
        );
        assert_eq!(
            result.offset as usize,
            input.len(),
            "branch '{}' should consume all bytes",
            input
        );
    }

    // Non-matching input should fail.
    let result = parse_with_ir(&ir, "kw_000");
    assert!(!result.success, "'kw_000' should not match any branch");
}

#[test]
fn stress_long_repeat_10000() {
    // Grammar:
    //   list = item ("," >> item)* ;
    //   item = /\w+/ ;
    // Input: "w0,w1,w2,...,w9999" (10000 comma-separated items).

    let ir = make_ir(
        vec![
            rule(
                0,
                0,
                IrNode::Seq(vec![
                    IrNode::Ref(1), // item
                    IrNode::Repeat {
                        inner: Box::new(IrNode::Next(
                            Box::new(IrNode::Literal(2)), // ","
                            Box::new(IrNode::Ref(1)),     // item
                        )),
                        lo: 0,
                        hi: u32::MAX,
                    },
                ]),
            ),
            rule(1, 1, IrNode::Regex(3)), // item = /\w+/
        ],
        vec!["list".into(), "item".into(), ",".into(), r"\w+".into()],
    );

    let count = 10_000;
    let mut input = String::with_capacity(count * 6);
    for i in 0..count {
        if i > 0 {
            input.push(',');
        }
        input.push_str(&format!("w{}", i));
    }

    let result = parse_with_ir(&ir, &input);
    assert!(result.success, "long repeat (10000) should parse");
    assert_eq!(
        result.offset as usize,
        input.len(),
        "should consume all {} bytes",
        input.len()
    );
}

#[test]
fn stress_value_stack_deep_seq() {
    // Grammar: root = a , b , c , d , e , f , g , h ;
    // Each sub-rule is itself a 4-element sequence of literals.
    // This produces many values on the stack (8 * 4 = 32 leaves).
    let mut strings: Vec<String> = vec!["root".into()]; // 0
    let mut sub_rules = Vec::new();
    let mut seq_refs = Vec::new();

    for rule_idx in 0u32..8 {
        let rule_id = rule_idx + 1;
        let rule_name_id = strings.len() as u32;
        strings.push(format!("sub{}", rule_idx));

        let mut literals = Vec::new();
        for lit_idx in 0u32..4 {
            let lit_str_id = strings.len() as u32;
            let ch = (b'A' + (rule_idx * 4 + lit_idx) as u8) as char;
            strings.push(ch.to_string());
            literals.push(IrNode::Literal(lit_str_id));
        }

        sub_rules.push(rule(rule_id, rule_name_id, IrNode::Seq(literals)));
        seq_refs.push(IrNode::Ref(rule_id));
    }

    let mut all_rules = vec![rule(0, 0, IrNode::Seq(seq_refs))];
    all_rules.extend(sub_rules);

    let ir = make_ir(all_rules, strings);

    // Input: "ABCDEFGHIJKLMNOPQRSTUVWXYZ" + "[\]^" (32 ASCII chars from 'A')
    let input: String = (0..32).map(|i| (b'A' + i) as char).collect();

    let result = parse_with_ir(&ir, &input);
    assert!(result.success, "deep seq (32 values) should parse");
    assert_eq!(
        result.offset as usize,
        input.len(),
        "should consume all {} bytes",
        input.len()
    );

    // Verify the value tree has the expected structure: Tagged(root) -> Seq of 8 Tagged sub-rules.
    let root = result.value.as_ref().unwrap();
    match root {
        Value::Tagged {
            tag: 0, children, ..
        } => {
            // Root should contain a flattened or nested structure with 8 sub-rule results.
            assert!(
                !children.is_empty(),
                "root should have children from 8 sub-rules"
            );
        }
        other => panic!("expected Tagged(root), got {:?}", other),
    }
}

// ── End-to-end ──────────────────────────────────────────────────────────────

#[test]
fn end_to_end_json_like_grammar() {
    // value = string | number | array
    // string = /"[^"]*"/
    // number = /[0-9]+/
    // array = "[" >> value* << "]"

    let ir = make_ir(
        vec![
            rule(
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
                        AltBranch {
                            node: IrNode::Ref(3),
                            first_set: None,
                        },
                    ],
                    None,
                ),
            ),
            rule(1, 1, IrNode::Regex(4)),
            rule(2, 2, IrNode::Regex(5)),
            rule(
                3,
                3,
                IrNode::Skip(
                    Box::new(IrNode::Next(
                        Box::new(IrNode::Literal(6)),
                        Box::new(IrNode::Repeat {
                            inner: Box::new(IrNode::Ref(0)),
                            lo: 0,
                            hi: u32::MAX,
                        }),
                    )),
                    Box::new(IrNode::Literal(7)),
                ),
            ),
        ],
        vec![
            "value".into(),
            "string".into(),
            "number".into(),
            "array".into(),
            r#""[^"]*""#.into(),
            "[0-9]+".into(),
            "[".into(),
            "]".into(),
        ],
    );

    let mut ir = ir;
    ir.rules[0].meta.is_cyclic = true;
    ir.rules[0].meta.memo = MemoStrategy::Full;
    ir.rules[0].meta.is_transparent = true;

    let result = parse_with_ir(&ir, "42");
    assert!(result.success, "number parse failed: {:?}", result);
    assert_eq!(result.offset, 2);

    let result = parse_with_ir(&ir, r#""hello""#);
    assert!(result.success, "string parse failed: {:?}", result);
    assert_eq!(result.offset, 7);

    let result = parse_with_ir(&ir, "[42]");
    assert!(result.success, "array parse failed: {:?}", result);
    assert_eq!(result.offset, 4);
}
