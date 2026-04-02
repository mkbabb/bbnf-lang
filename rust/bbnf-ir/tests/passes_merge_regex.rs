use std::collections::HashMap;

use bbnf_ir::passes::merge_regex_alts;
use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};

fn make_ir(body: IrNode, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings,
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
        type_map: None,
    }
}

#[test]
fn merges_two_regex_alts() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Regex(1),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Regex(2),
                    first_set: None,
                },
            ],
            None,
        ),
        vec!["rule".into(), "[a-z]+".into(), "[0-9]+".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            // Patterns without top-level pipes are not wrapped in (?:...).
            assert_eq!(ir.get_string(*sid), "[a-z]+|[0-9]+");
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}

#[test]
fn merges_three_css_property_regex() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Regex(1),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Regex(2),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Regex(3),
                    first_set: None,
                },
            ],
            None,
        ),
        vec![
            "rule".into(),
            r"[a-zA-Z_][\w-]*".into(),
            r"--[\w-]+".into(),
            r"-[a-zA-Z][\w-]*".into(),
        ],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            assert_eq!(
                ir.get_string(*sid),
                r"[a-zA-Z_][\w-]*|--[\w-]+|-[a-zA-Z][\w-]*"
            );
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}

#[test]
fn merges_mixed_regex_and_literal() {
    // Mixed Regex + Literal branches are now merged (B.2.1).
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Regex(1),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Literal(2),
                    first_set: None,
                },
            ],
            None,
        ),
        vec!["rule".into(), "[a-z]+".into(), "x".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            // "x" is a literal, escaped for regex but unchanged since it's not special.
            assert_eq!(ir.get_string(*sid), "[a-z]+|x");
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}

#[test]
fn skips_all_literal_alts() {
    // All-literal branches should NOT be merged (no regex benefit).
    let mut ir = make_ir(
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
        vec!["rule".into(), "foo".into(), "bar".into()],
    );
    merge_regex_alts(&mut ir);
    assert!(matches!(&ir.rules[0].body, IrNode::Alt(..)));
}

#[test]
fn escapes_special_chars_in_literal() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Regex(1),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Literal(2),
                    first_set: None,
                },
            ],
            None,
        ),
        vec!["rule".into(), "[a-z]+".into(), "a.b".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            assert_eq!(ir.get_string(*sid), r"[a-z]+|a\.b");
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}

#[test]
fn wraps_pipe_containing_patterns() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch {
                    node: IrNode::Regex(1),
                    first_set: None,
                },
                AltBranch {
                    node: IrNode::Regex(2),
                    first_set: None,
                },
            ],
            None,
        ),
        vec!["rule".into(), "a|b".into(), "c".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            // "a|b" has a top-level pipe so gets wrapped; "c" does not.
            assert_eq!(ir.get_string(*sid), "(?:a|b)|c");
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}
