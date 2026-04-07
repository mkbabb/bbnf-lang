//! Tests for character-class-aware regex parsing in BBNF grammars.
//! Verifies that `/` inside `[...]` is treated as literal, not as a closing delimiter.

use bbnf::types::Expression;

/// Extract the regex body string from a single-rule grammar `name = /pattern/ ;`.
fn extract_regex(source: &str) -> String {
    let source_static: &'static str = Box::leak(source.to_string().into_boxed_str());
    let ast = bbnf::grammar::parse()
        .parse(source_static)
        .expect("failed to parse grammar")
        .rules;
    let (_, rhs) = ast.into_iter().next().expect("expected at least one rule");

    // grammar() yields ProductionRule(lhs, Rule(expr, mapping_fn)).
    // After destructuring in grammar(), rhs = Rule(expr, mapping_fn).
    match rhs {
        Expression::Rule(inner, _) => match *inner {
            Expression::Regex(token) => token.span.as_str().to_string(),
            other => panic!("expected Regex inside Rule, got: {:?}", other),
        },
        other => panic!("expected Rule expression, got: {:?}", other),
    }
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
    // Unescaped `/` inside a character class: `/[a-z/A-Z]/`.
    let body = extract_regex(r#"rule = /[a-z/A-Z]/ ;"#);
    assert_eq!(body, "[a-z/A-Z]");
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
    // Multiple character classes, one containing `/`.
    let body = extract_regex(r#"rule = /[a-z][/][0-9]/ ;"#);
    assert_eq!(body, "[a-z][/][0-9]");
}

#[test]
fn regex_empty() {
    // Empty regex body: `//`.
    let body = extract_regex(r#"rule = // ;"#);
    assert_eq!(body, "");
}

#[test]
fn regex_no_charclass() {
    // Regex with escaped slash but no character class.
    let body = extract_regex(r#"rule = /foo\/bar/ ;"#);
    assert_eq!(body, r"foo\/bar");
}

#[test]
fn regex_nested_brackets() {
    // Character class with `/` followed by a group — bracket depth resets properly.
    let body = extract_regex(r#"rule = /[a-z/0-9]+(foo)/ ;"#);
    assert_eq!(body, "[a-z/0-9]+(foo)");
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
    let source_static: &'static str = Box::leak(source.to_string().into_boxed_str());
    let parser = bbnf::grammar::parse();
    let ast = parser.parse(source_static).expect("grammar parse failed").rules;
    let rule_names: Vec<&str> = ast
        .keys()
        .filter_map(|e| match e {
            Expression::Nonterminal(tok) => Some(tok.value.as_ref()),
            _ => None,
        })
        .collect();
    assert!(
        rule_names.contains(&"c"),
        "rule 'c' not found — parser stopped at regex with ) in char class. Found: {:?}",
        rule_names,
    );
}
