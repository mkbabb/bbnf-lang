//! Integration tests for CSS parsing through the tape-first
//! `#[derive(Parser)]` codegen path using the pretty.bbnf grammar.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover)]
struct CssPrettyParser;

fn load_css(name: &str) -> String {
    let candidates = [
        format!("../../data/css/{}", name),
        format!("../data/css/{}", name),
    ];
    for path in &candidates {
        if let Ok(contents) = std::fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find data file '{}'; tried: {:?}",
        name, candidates
    );
}

fn parse_full(input: &str) -> bool {
    // The tape-first parser enforces full input consumption inside
    // `parse()`, so a successful `Ok` implies the old "offset reached
    // end" invariant.
    CssPrettyParser::parse(input).is_ok()
}

#[test]
fn parse_normalize_css() {
    let input = load_css("normalize.css");
    assert!(
        parse_full(&input),
        "normalize.css: parse failed or incomplete"
    );
}

#[test]
fn parse_bootstrap_css() {
    let input = load_css("bootstrap.css");
    assert!(
        parse_full(&input),
        "bootstrap.css: parse failed or incomplete"
    );
}

#[test]
fn parse_tailwind_css() {
    let input = load_css("tailwind.css");
    assert!(
        parse_full(&input),
        "tailwind.css: parse failed or incomplete"
    );
}
