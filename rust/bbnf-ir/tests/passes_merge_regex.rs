use std::collections::HashMap;

use bbnf_ir::{AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};
use bbnf_ir::passes::merge_regex_alts;

fn make_ir(body: IrNode, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body,
            meta: RuleMeta::default(),
        }],
        strings,
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
    }
}

#[test]
fn merges_two_regex_alts() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Regex(1), first_set: None },
                AltBranch { node: IrNode::Regex(2), first_set: None },
            ],
            None,
        ),
        vec!["rule".into(), "[a-z]+".into(), "[0-9]+".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
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
                AltBranch { node: IrNode::Regex(1), first_set: None },
                AltBranch { node: IrNode::Regex(2), first_set: None },
                AltBranch { node: IrNode::Regex(3), first_set: None },
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
fn skips_non_regex_alts() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Regex(1), first_set: None },
                AltBranch { node: IrNode::Literal(2), first_set: None },
            ],
            None,
        ),
        vec!["rule".into(), "[a-z]+".into(), "x".into()],
    );
    merge_regex_alts(&mut ir);
    // Should not merge -- not all branches are Regex.
    assert!(matches!(&ir.rules[0].body, IrNode::Alt(..)));
}

#[test]
fn wraps_pipe_containing_patterns() {
    let mut ir = make_ir(
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Regex(1), first_set: None },
                AltBranch { node: IrNode::Regex(2), first_set: None },
            ],
            None,
        ),
        vec!["rule".into(), "a|b".into(), "c".into()],
    );
    merge_regex_alts(&mut ir);
    match &ir.rules[0].body {
        IrNode::Regex(sid) => {
            assert_eq!(ir.get_string(*sid), "(?:a|b)|c");
        }
        other => panic!("Expected Regex, got {:?}", other),
    }
}
