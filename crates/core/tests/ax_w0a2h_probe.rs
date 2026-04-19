//! AX.W0a.2.h probe — isolate BbnfBootstrap::parse failure offsets.
//!
//! Temporary probe test exercising shape-dispatched `BbnfBootstrap::parse`
//! on incrementally-larger BBNF snippets to pinpoint which positions
//! diverge from walker parity.

use bbnf::grammar::generated::BbnfBootstrap;

#[test]
#[ignore]
fn probe_bbnf_parse_snippets() {
    let tests = [
        ("empty", ""),
        ("comment", "// comment\n"),
        ("just_rule", "foo = \"bar\" ;\n"),
        ("just_rule_no_quotes", "foo = bar ;\n"),
        ("just_rule_alt", "foo = \"a\" | \"b\" ;\n"),
        ("just_rule_no_semi", "foo = \"bar\"\n"),
        ("just_rule_inline_no_ws", "foo=\"bar\";"),
        ("just_rule_regex", "foo = /abc/ ;\n"),
        ("just_ident_rule_ident", "foo = bar ;"),
        ("just_ident_rule_lit", "foo = \"bar\";"),
        ("import_path_short", "@import \"x\";"),
        ("import_path_nl", "@import \"x\";\n"),
        ("import_path_full", "@import \"foo\" ;\n"),
        ("import_items_simple", "@import {a} from \"x\";"),
        ("import_items", "@import { a } from \"foo\" ;\n"),
        ("import_items_multi", "@import { a, b } from \"foo\" ;\n"),
        ("two_comments", "// a\n// b\n"),
        ("comment_then_import",
         "// a\n@import \"foo\" ;\n"),
        ("comment_then_import_items",
         "// a\n@import { a } from \"foo\" ;\n"),
        ("first_72_of_bbnf",
         "// BBNF — Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n"),
    ];
    for (name, input) in &tests {
        match BbnfBootstrap::parse(input) {
            Ok(_) => println!("{name}: OK (len={})", input.len()),
            Err(e) => println!("{name}: ERR {:?}", e),
        }
    }
    let input = std::fs::read_to_string("../../grammar/bbnf/bbnf.bbnf")
        .expect("bbnf.bbnf must load");
    match BbnfBootstrap::parse(&input) {
        Ok(_) => println!("bbnf.bbnf: OK (len={})", input.len()),
        Err(e) => println!("bbnf.bbnf: ERR {:?}", e),
    }
}
