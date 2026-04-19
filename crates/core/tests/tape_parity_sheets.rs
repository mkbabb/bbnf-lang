//! Google Sheets tape-parity tests — AX.W0a.2.e single-grammar split.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;
use common::{assert_tape_parity, HasCursor, ParseGrammar};

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", skip_recover)]
struct SheetsGrammar;

impl<'tape> HasCursor<'tape> for <SheetsGrammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<SheetsGrammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for SheetsGrammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <SheetsGrammar>::parse(input)
    }
}

#[test]
fn sheets_simple_formula_tape_parity() {
    let input = "=SUM(A1:A10) + AVERAGE(B1:B10)";
    assert_tape_parity::<SheetsGrammar>("sheets", "simple", input);
}

#[test]
fn sheets_nested_if_tape_parity() {
    let input = "=IF(A1>10, IF(B1<5, \"low\", \"mid\"), \"high\")";
    assert_tape_parity::<SheetsGrammar>("sheets", "nested_if", input);
}

#[test]
fn sheets_arithmetic_tape_parity() {
    let input = "=(1+2)*3-4/5";
    assert_tape_parity::<SheetsGrammar>("sheets", "arithmetic", input);
}
