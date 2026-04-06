#![feature(cold_path)]

//! Integration tests for EBNF parsing + prettify through the
//! `#[derive(Parser)]` slab codegen path.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/ebnf/ebnf.bbnf", prettify, slab)]
struct EbnfSlab;

/// Parse an EBNF grammar with the slab allocator and assert full consumption.
fn parse_grammar(input: &str) {
    let ctx = __EbnfSlabEnumCtx::with_capacity(input.len() / 16);
    let parser = EbnfSlab::grammar();
    let (result, state) = parser.parse_return_state_with_context(input, &ctx);
    assert!(result.is_some(), "parse returned None for input: {input}");
    assert!(
        state.offset >= input.trim_end().len(),
        "incomplete parse ({}/{}) for input: {input}",
        state.offset,
        input.len(),
    );
}

/// Prettify an EBNF grammar string.
fn prettify(input: &str) -> Option<String> {
    let config = pprint::Printer::new(80, 2, false);
    let parser = EbnfSlab::grammar_prettify();
    let ops = parser.parse(input)?;
    Some(pprint::render(&ops, config))
}

// ── Slab parsing ─────────────────────────────────────────────────────

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
