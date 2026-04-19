//! EBNF tape-parity tests — AX.W0a.2.e single-grammar split.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;
use common::{assert_tape_parity, HasCursor, ParseGrammar};

#[derive(Parser)]
#[parser(path = "../../grammar/ebnf/ebnf.bbnf")]
struct EbnfGrammar;

impl<'tape> HasCursor<'tape> for <EbnfGrammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<EbnfGrammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for EbnfGrammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <EbnfGrammar>::parse(input)
    }
}

#[test]
fn ebnf_minimal_tape_parity() {
    let input = "digit = \"0\" | \"1\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "minimal", input);
}

#[test]
fn ebnf_expr_grammar_tape_parity() {
    let input = "expr = term , { \"+\" , term } ; term = factor , { \"*\" , factor } ; factor = digit ; digit = \"0\" | \"1\" | \"2\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "expr", input);
}

#[test]
fn ebnf_recursive_list_tape_parity() {
    let input = "list = \"[\" , [ item , { \",\" , item } ] , \"]\" ; item = \"a\" | \"b\" | \"c\" ;";
    assert_tape_parity::<EbnfGrammar>("ebnf", "recursive_list", input);
}

#[test]
fn ebnf_root_has_at_least_one_rule() {
    let input = "a = \"x\" ;";
    let parsed = EbnfGrammar::parse(input).expect("parse");
    let view = parsed.view();
    let cursor = view.cursor_of();
    assert!(cursor.children().count() >= 1);
}
