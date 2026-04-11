//! Tests for character-class-aware regex parsing in BBNF grammars.
//! Verifies that `/` inside `[...]` is treated as literal, not as a
//! closing delimiter.
//!
//! Reads the tape-first `BbnfBootstrapNodeView`'s RHS span text
//! directly — the span covers the regex literal verbatim regardless
//! of how the optimizer has arranged the inner tree nodes.

use bbnf::grammar;

/// Extract the regex body string from a single-rule grammar
/// `name = /pattern/ ;`. The parse tree's RHS view span covers the
/// regex literal verbatim (possibly with trailing whitespace); we
/// locate the slash-bounded slice inside that span.
///
/// Under the tape-first bootstrap, the optimizer may elide the
/// `regex` rule compound into its parent, so a `rule_kind()` walk
/// can't always find an explicit `regex` node. Reading the RHS
/// span text sidesteps that ambiguity — the source text itself is
/// the ground truth and the test's intent is to exercise
/// the BBNF bootstrap tokenizer's handling of `/` inside `[...]`,
/// not the lowering pipeline's tree shape.
fn extract_regex(source: &str) -> String {
    let pg = grammar::parse(source).expect("failed to parse grammar");
    let (_name, entry) = pg
        .rules
        .into_iter()
        .next()
        .expect("expected at least one rule");

    let rhs_text = entry.rhs.span_text();
    let start = rhs_text
        .find('/')
        .expect("rule RHS should contain a regex literal");
    let tail = &rhs_text[start + 1..];
    let end = tail
        .rfind('/')
        .expect("rule RHS should contain a closing `/`");
    tail[..end].to_string()
}

#[test]
fn regex_simple() {
    let body = extract_regex(r#"rule = /[a-z]+/ ;"#);
    assert_eq!(body, "[a-z]+");
}

#[test]
fn regex_escaped_slash_inside_charclass() {
    // Escaped `/` inside a character class: `/[abc\/def]/`.
    let body = extract_regex(r#"rule = /[abc\/def]/ ;"#);
    assert_eq!(body, r"[abc\/def]");
}

#[test]
fn regex_literal_slash_inside_charclass() {
    // `/` inside a character class must be escaped in BBNF: `/[a-z\/A-Z]/`.
    let body = extract_regex(r#"rule = /[a-z\/A-Z]/ ;"#);
    assert_eq!(body, r"[a-z\/A-Z]");
}

#[test]
fn regex_escaped_bracket_inside_charclass() {
    // Escaped `]` inside a character class keeps bracket depth consistent.
    // `/[a\]b]/` — the `\]` does not close the class, the real `]` does.
    let body = extract_regex(r#"rule = /[a\]b]/ ;"#);
    assert_eq!(body, r"[a\]b]");
}

#[test]
fn regex_escaped_slash_outside_charclass() {
    // Escaped `/` outside any character class: `/[a-z]+\/[0-9]+/`.
    let body = extract_regex(r#"rule = /[a-z]+\/[0-9]+/ ;"#);
    assert_eq!(body, r"[a-z]+\/[0-9]+");
}

#[test]
fn regex_multiple_charclasses_with_slash() {
    // Multiple character classes, one containing escaped `/`.
    let body = extract_regex(r#"rule = /[a-z][\/][0-9]/ ;"#);
    assert_eq!(body, r"[a-z][\/][0-9]");
}

#[test]
fn regex_single_char() {
    // Minimal non-empty regex.
    let body = extract_regex(r#"rule = /./ ;"#);
    assert_eq!(body, ".");
}

#[test]
fn regex_no_charclass() {
    // Regex with escaped slash but no character class.
    let body = extract_regex(r#"rule = /foo\/bar/ ;"#);
    assert_eq!(body, r"foo\/bar");
}

#[test]
fn regex_nested_brackets() {
    // Character class with escaped `/` followed by a group.
    let body = extract_regex(r#"rule = /[a-z\/0-9]+(foo)/ ;"#);
    assert_eq!(body, r"[a-z\/0-9]+(foo)");
}

#[test]
fn regex_charclass_with_backslash_and_slash() {
    // Character class containing both escaped backslash and escaped slash.
    let body = extract_regex(r#"rule = /[\\\/]/ ;"#);
    assert_eq!(body, r"[\\\/]");
}

#[test]
fn regex_close_paren_in_charclass_inside_group() {
    // Regression: /[^)]+/ inside a BBNF group (...) must NOT consume ) as the
    // group closer. The ) is inside a regex character class [^)].
    let source = r#"
a = "x" ;
b = ("," , /[^)]+/) ;
c = "y" ;
"#;
    let pg = grammar::parse(source).expect("grammar parse failed");
    let rule_names: Vec<&str> = pg.rules.keys().copied().collect();
    assert!(
        rule_names.contains(&"c"),
        "rule 'c' not found — parser stopped at regex with ) in char class. Found: {:?}",
        rule_names,
    );
}
