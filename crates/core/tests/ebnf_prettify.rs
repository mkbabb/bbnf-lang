//! Integration tests for EBNF parsing + prettify through the
//! tape-first `#[derive(Parser)]` codegen path.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/ebnf/ebnf.bbnf", prettify)]
struct EbnfParser;

/// Parse an EBNF grammar and assert the parse succeeds. The
/// tape-first parser rejects trailing garbage automatically.
fn parse_grammar(input: &str) {
    let parsed = EbnfParser::parse(input)
        .unwrap_or_else(|e| panic!("parse failed for input {input:?}: {e:?}"));
    let _root = parsed.view();
}

/// Prettify an EBNF grammar string via the separate prettify side
/// channel (still combinator-shaped, unchanged by the tape-first
/// migration).
fn prettify(input: &str) -> Option<String> {
    let config = pprint::Printer::new(80, 2, false);
    let parser = EbnfParser::grammar_prettify();
    let ops = parser.parse(input)?;
    Some(pprint::render(&ops, config))
}

// ── Parsing ──────────────────────────────────────────────────────────

#[test]
fn parse_single_rule() {
    parse_grammar(r#"digit = "0" | "1" | "2" ;"#);
}

#[test]
fn parse_multi_rule() {
    parse_grammar("digit = \"0\" | \"1\" ;\nnumber = digit , { digit } ;");
}

// ── Prettify ─────────────────────────────────────────────────────────

#[test]
#[ignore = "prettify codegen stops after first rule — pre-existing in gorgeous"]
fn prettify_multi_rule() {
    let input = "digit = \"0\" | \"1\" | \"2\" ;\nnumber = digit , { digit } ;";
    let result = prettify(input);
    assert!(result.is_some(), "prettify should succeed for multi-rule input");
    let output = result.unwrap();
    assert!(
        output.contains("digit"),
        "should contain 'digit': got '{output}'"
    );
    assert!(
        output.contains("number"),
        "should contain 'number': got '{output}'"
    );
}

#[test]
fn prettify_idempotent() {
    let input = r#"letter = "A" | "B" | "C" ;"#;
    let first = prettify(input).expect("first prettify should succeed");
    let second = prettify(&first).expect("second prettify should succeed");
    assert_eq!(first, second, "prettify should be idempotent");
}
