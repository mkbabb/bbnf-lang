use bbnf_ir::passes::regex::algebra::{RewriteRule, RedundantAlternation};
use bbnf_regex::algebra::{extract_char_class_bytes, byteset_to_pattern};
use bbnf_ir::{AltBranch, IrNode};

#[test]
fn extract_char_class_simple() {
    let bs = extract_char_class_bytes("[a-z]").unwrap();
    for b in b'a'..=b'z' {
        assert!(bs.contains(b));
    }
    let bs2 = extract_char_class_bytes("[a-c]").unwrap();
    assert_eq!(bs2.len(), 3);
}

#[test]
fn extract_char_class_multi_range() {
    let result = extract_char_class_bytes("[0-9a-fA-F]").unwrap();
    assert!(result.contains(b'0'));
    assert!(result.contains(b'9'));
    assert!(result.contains(b'a'));
    assert!(result.contains(b'f'));
    assert!(result.contains(b'A'));
    assert!(result.contains(b'F'));
}

#[test]
fn bytes_to_class_roundtrip() {
    let mut bs = bbnf_regex::ByteSet::empty();
    for b in b'a'..=b'z' {
        bs.insert(b);
    }
    assert_eq!(byteset_to_pattern(&bs), "[a-z]");
}

#[test]
fn redundant_elimination() {
    let mut strings = vec!["[a-z]".to_string()];
    let mut branches = vec![
        AltBranch {
            node: IrNode::Regex(0),
            first_set: None,
        },
        AltBranch {
            node: IrNode::Regex(0),
            first_set: None,
        },
    ];
    let rule = RedundantAlternation;
    assert!(rule.apply(&mut branches, &mut strings));
    assert_eq!(branches.len(), 1);
}
