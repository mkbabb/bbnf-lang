//! Integration tests for the bytecode interpreter.
//!
//! Exercises the full IR → bytecode → interpret pipeline using hand-built IR trees.

use bbnf_ir::interpreter::{Value, parse_with_ir};
use bbnf_ir::{
    AltBranch, AltDispatch, CharSet128, GrammarIR, IrNode, IrRule, MemoStrategy, RuleMeta,
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
        b1_span_collapse: false,
        debug_all: false,
        debug_labels: Vec::new(),
        infer_map: None,
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
