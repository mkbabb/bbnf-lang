use bbnf_ir::regex_first::regex_first_chars;

#[test]
fn simple_literal() {
    let cs = regex_first_chars("abc").unwrap();
    assert!(cs.has(b'a'));
    assert!(!cs.has(b'b'));
}

#[test]
fn char_class() {
    let cs = regex_first_chars("[a-zA-Z_]").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'Z'));
    assert!(cs.has(b'_'));
    assert!(!cs.has(b'0'));
}

#[test]
fn alternation() {
    let cs = regex_first_chars("abc|[0-9]").unwrap();
    assert!(cs.has(b'a'));
    assert!(cs.has(b'5'));
}

#[test]
fn optional_prefix() {
    let cs = regex_first_chars("-?[0-9]").unwrap();
    assert!(cs.has(b'-'));
    assert!(cs.has(b'0'));
}

#[test]
fn dot_returns_none() {
    assert!(regex_first_chars(".*").is_none());
}

#[test]
fn escape_sequences() {
    let cs = regex_first_chars(r"\d+").unwrap();
    assert!(cs.has(b'0'));
    assert!(cs.has(b'9'));
    assert!(!cs.has(b'a'));
}
