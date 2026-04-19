//! AX.W0a.2.h probe — isolate BbnfBootstrap::parse failure offsets.
//!
//! Temporary probe test exercising shape-dispatched `BbnfBootstrap::parse`
//! on incrementally-larger BBNF snippets to pinpoint which positions
//! diverge from walker parity.
//!
//! AX.W0a.2.i.a — asserts that parse + pipeline extraction succeed on
//! every input. Under widened admission + shape-authoritative tape,
//! host.rs's `walk_tape` uses `find_rhs_expression_descendant` to
//! identify the rhs expression head regardless of Wrap elision.

use bbnf::grammar::generated::BbnfBootstrap;

#[test]
#[ignore]
fn probe_bbnf_parse_snippets() {
    let tests: &[(&str, &str)] = &[
        ("empty", ""),
        ("comment", "// comment\n"),
        ("just_rule", "foo = \"bar\" ;\n"),
        ("just_rule_no_quotes", "foo = bar ;\n"),
        ("just_rule_alt", "foo = \"a\" | \"b\" ;\n"),
        // `just_rule_no_semi` removed — terminator is required under
        // both walker + shape dispatch; not a host.rs cursor concern.
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
        ("comment_then_import", "// a\n@import \"foo\" ;\n"),
        (
            "comment_then_import_items",
            "// a\n@import { a } from \"foo\" ;\n",
        ),
        (
            "first_72_of_bbnf",
            "// BBNF — Better Backus-Naur Form\n// Self-hosted grammar definition.\n\n",
        ),
    ];
    let mut failures: Vec<(String, String)> = Vec::new();
    for (name, input) in tests {
        match BbnfBootstrap::parse(input) {
            Ok(parsed) => {
                let extract = bbnf::grammar::host::extract_observational(&parsed);
                println!(
                    "{name}: OK (len={}; rules={}; imports={})",
                    input.len(),
                    extract.rules.len(),
                    extract.imports.len()
                );
            }
            Err(e) => {
                eprintln!("{name}: PARSE ERR {:?}", e);
                failures.push((name.to_string(), format!("{:?}", e)));
            }
        }
    }
    // bbnf.bbnf — scope of W0a.2.h probe + AX.W0a.2.i.a host.rs fix.
    // Other grammar files (json.bbnf, ebnf.bbnf, bnf.bbnf, google-
    // sheets.bbnf) use richer constructs that the shape emitter handles
    // separately; they are out of scope for host.rs cursor work.
    let input = std::fs::read_to_string("../../grammar/bbnf/bbnf.bbnf")
        .expect("bbnf.bbnf must load");
    match BbnfBootstrap::parse(&input) {
        Ok(parsed) => {
            let extract = bbnf::grammar::host::extract_observational(&parsed);
            println!(
                "bbnf.bbnf: OK (len={}; rules={}; imports={})",
                input.len(),
                extract.rules.len(),
                extract.imports.len()
            );
        }
        Err(e) => {
            eprintln!("bbnf.bbnf: PARSE ERR {:?}", e);
            failures.push(("bbnf.bbnf".to_string(), format!("{:?}", e)));
        }
    }
    assert!(failures.is_empty(), "failures: {:?}", failures);
}
