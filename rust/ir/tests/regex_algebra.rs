use bbnf_ir::passes::regex::algebra::{
    RewriteRule, RedundantAlternation, bytes_to_char_class, extract_single_char_class,
};
use bbnf_ir::{AltBranch, IrNode};

#[test]
fn extract_char_class_simple() {
    assert_eq!(
        extract_single_char_class("[a-z]"),
        Some((b'a'..=b'z').collect())
    );
    assert_eq!(
        extract_single_char_class("[a-c]"),
        Some(vec![b'a', b'b', b'c'])
    );
}

#[test]
fn extract_char_class_multi_range() {
    let result = extract_single_char_class("[0-9a-fA-F]").unwrap();
    assert!(result.contains(&b'0'));
    assert!(result.contains(&b'9'));
    assert!(result.contains(&b'a'));
    assert!(result.contains(&b'f'));
    assert!(result.contains(&b'A'));
    assert!(result.contains(&b'F'));
}

#[test]
fn bytes_to_class_roundtrip() {
    let bytes: Vec<u8> = (b'a'..=b'z').collect();
    assert_eq!(bytes_to_char_class(&bytes), "[a-z]");
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
