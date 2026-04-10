
//! Integration tests for CSS parsing through the `#[derive(Parser)]` slab codegen path
//! using the pretty.bbnf grammar.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, slab)]
struct CssPrettySlab;

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
    let slab = __CssPrettySlabEnumCtx::with_capacity(input.len() / 32);
    let parser = CssPrettySlab::stylesheet();
    let (result, state) = parser.parse_return_state_with_context(input, &slab);
    result.is_some() && state.offset >= input.trim_end().len()
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
