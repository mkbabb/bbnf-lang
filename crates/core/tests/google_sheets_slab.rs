
//! Integration tests for Google Sheets formula parsing through the
//! `#[derive(Parser)]` slab codegen path.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", prettify, slab)]
struct SheetsSlab;

/// Parse a formula with the slab allocator and assert full consumption.
fn parse_formula(input: &str) {
    let ctx = __SheetsSlabEnumCtx::with_capacity(input.len() / 16);
    let parser = SheetsSlab::formula();
    let (result, state) = parser.parse_return_state_with_context(input, &ctx);
    assert!(
        result.is_some(),
        "parse returned None for input: {input}"
    );
    assert_eq!(
        state.offset,
        input.len(),
        "incomplete parse ({}/{}) for input: {input}",
        state.offset,
        input.len(),
    );
}

// ── Simple formulas ──────────────────────────────────────────────────

#[test]
fn parse_sum() {
    parse_formula("=SUM(A1:A10)");
}

#[test]
fn parse_if() {
    parse_formula("=IF(A1>0,A1,0)");
}

#[test]
fn parse_arithmetic() {
    parse_formula("=1+2*3");
}

// ── Nested formulas ──────────────────────────────────────────────────

#[test]
fn parse_let_nested() {
    parse_formula("=LET(x,1,y,2,x+y)");
}

#[test]
fn parse_index_match() {
    parse_formula("=INDEX(A1:C10,MATCH(\"x\",A1:A10,0),1)");
}

// ── Prettify ─────────────────────────────────────────────────────────

#[test]
fn prettify_simple_formula() {
    let parser = SheetsSlab::formula_prettify();
    let result = parser.parse("=SUM(A1:A10)");
    assert!(result.is_some(), "prettify should succeed for =SUM(A1:A10)");
}

#[test]
fn prettify_nested_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = SheetsSlab::formula_prettify();
    let ops = parser.parse("=LET(x,1,y,2,x+y)");
    assert!(ops.is_some(), "prettify should succeed for LET formula");
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("LET"),
        "rendered output should contain LET: got '{rendered}'"
    );
}

#[test]
fn prettify_if_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = SheetsSlab::formula_prettify();
    let ops = parser.parse("=IF(A1>0,A1,0)");
    assert!(ops.is_some(), "prettify should succeed for IF formula");
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("IF"),
        "rendered output should contain IF: got '{rendered}'"
    );
}

#[test]
fn prettify_index_match_formula() {
    let config = pprint::Printer::new(80, 2, false);
    let parser = SheetsSlab::formula_prettify();
    let ops = parser.parse("=INDEX(A1:C10,MATCH(\"x\",A1:A10,0),1)");
    assert!(
        ops.is_some(),
        "prettify should succeed for INDEX/MATCH formula"
    );
    let rendered = pprint::render(&ops.unwrap(), config);
    assert!(
        rendered.contains("INDEX"),
        "rendered output should contain INDEX: got '{rendered}'"
    );
    assert!(
        rendered.contains("MATCH"),
        "rendered output should contain MATCH: got '{rendered}'"
    );
}
