//! BBNF self-hosted tape-parity tests — AX.W0a.2.e single-grammar split.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;
use common::{assert_tape_parity, load_grammar_sample, HasCursor, ParseGrammar};

#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
struct BbnfGrammar;

impl<'tape> HasCursor<'tape> for <BbnfGrammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<BbnfGrammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for BbnfGrammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <BbnfGrammar>::parse(input)
    }
}

#[test]
fn bbnf_self_hosted_bbnf_tape_parity() {
    let input = load_grammar_sample("bbnf/bbnf.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "bbnf", &input);
}

#[test]
fn bbnf_expressions_tape_parity() {
    let input = load_grammar_sample("bbnf/expressions.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "expressions", &input);
}

#[test]
fn bbnf_types_tape_parity() {
    let input = load_grammar_sample("bbnf/types.bbnf");
    assert_tape_parity::<BbnfGrammar>("bbnf", "types", &input);
}

#[test]
fn bbnf_total_records_nonzero() {
    let input = "start = \"a\" | \"b\" ;";
    let parsed = BbnfGrammar::parse(input).expect("parse");
    assert!(parsed.tape().len() > 0, "bbnf parse produced empty tape");
}
