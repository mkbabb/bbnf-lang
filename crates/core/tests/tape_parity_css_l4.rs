//! CSS L4 tape-parity tests — AX.W0a.2.e single-grammar split.

#[path = "tape_parity_common/mod.rs"]
mod common;

use bbnf::runtime::Root;
use bbnf::runtime::tape::TapeCursor;
use bbnf_derive::Parser;
use common::{assert_tape_parity, load, HasCursor, ParseGrammar};

/// AU.2.4: the CSS L4 grammar's `hex` rule references
/// `crate::css_types::parse_hex_color` through the HexConvert route;
/// every derive-Parser site that includes CSS L4 must expose this
/// module with a compatible signature so the generated parser compiles.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Grammar;

impl<'tape> HasCursor<'tape> for <CssL4Grammar as Root>::View<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape> {
        <<CssL4Grammar as Root>::View<'tape>>::cursor(self)
    }
}

impl ParseGrammar for CssL4Grammar {
    fn parse_input(
        input: &str,
    ) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr> {
        <CssL4Grammar>::parse(input)
    }
}

#[test]
fn css_bootstrap_tape_parity() {
    let input = load("css/bootstrap.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "bootstrap", &input);
}

#[test]
fn css_normalize_tape_parity() {
    let input = load("css/normalize.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "normalize", &input);
}

#[test]
fn css_tailwind_tape_parity() {
    let input = load("css/tailwind.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "tailwind", &input);
}

#[test]
fn css_test_import_tape_parity() {
    let input = load("css/test_import.css");
    assert_tape_parity::<CssL4Grammar>("css_l4", "test_import", &input);
}
