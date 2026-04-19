//! BNF tape-parity tests — AX.W0a.2.e single-grammar split.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;
use common::{assert_tape_parity, HasCursor, ParseGrammar};

#[derive(Parser)]
#[parser(path = "../../grammar/bnf/bnf.bbnf")]
struct BnfGrammar;

impl<'tape> HasCursor<'tape> for <BnfGrammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<BnfGrammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for BnfGrammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <BnfGrammar>::parse(input)
    }
}

#[test]
fn bnf_minimal_tape_parity() {
    let input = "<digit> ::= \"0\" | \"1\"\n";
    assert_tape_parity::<BnfGrammar>("bnf", "minimal", input);
}
