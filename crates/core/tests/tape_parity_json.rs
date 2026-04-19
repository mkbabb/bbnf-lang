//! JSON tape-parity tests — AX.W0a.2.e single-grammar split.
//!
//! Isolated from other grammars to keep the compiled test binary
//! small — the aggregate `tape_parity` pre-split peaked at 26 GB RSS
//! during rustc codegen with debuginfo=2 and all five grammar
//! derive-Parser sites in one translation unit.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::{TapeCursor, TapeKind};
use bbnf_derive::Parser;
use common::{assert_tape_parity, load, HasCursor, ParseGrammar};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonGrammar;

impl<'tape> HasCursor<'tape> for <JsonGrammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<JsonGrammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for JsonGrammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <JsonGrammar>::parse(input)
    }
}

#[test]
fn json_rejects_malformed() {
    let input = "{ \"key\": [1, 2, ";
    let parsed = JsonGrammar::parse(input);
    assert!(
        parsed.is_err(),
        "expected parse failure on truncated JSON, got Ok"
    );
}

#[test]
fn json_canada_tape_parity() {
    let input = load("json/canada.json");
    assert_tape_parity::<JsonGrammar>("json", "canada", &input);
}

#[test]
fn json_twitter_tape_parity() {
    let input = load("json/twitter.json");
    assert_tape_parity::<JsonGrammar>("json", "twitter", &input);
}

#[test]
fn json_citm_tape_parity() {
    let input = load("json/citm_catalog.json");
    assert_tape_parity::<JsonGrammar>("json", "citm_catalog", &input);
}

#[test]
fn json_data_tape_parity() {
    let input = load("json/data.json");
    assert_tape_parity::<JsonGrammar>("json", "data", &input);
}

#[test]
fn json_data_xl_tape_parity() {
    let input = load("json/data_xl.json");
    assert_tape_parity::<JsonGrammar>("json", "data_xl", &input);
}

#[test]
fn json_root_is_compound() {
    let input = "{\"key\": [1, 2, 3]}";
    let parsed = JsonGrammar::parse(input).expect("parse");
    let view = parsed.view();
    let cursor = view.cursor_of();
    assert!(
        matches!(
            cursor.kind(),
            TapeKind::Rule | TapeKind::Seq | TapeKind::Alt
        ),
        "json root cursor kind = {:?}, expected a compound",
        cursor.kind()
    );
    assert!(cursor.children().count() >= 1);
}
