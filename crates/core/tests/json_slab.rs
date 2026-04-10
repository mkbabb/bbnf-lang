
//! Integration tests for JSON parsing through the `#[derive(Parser)]` slab codegen path.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", slab)]
struct JsonSlab;

fn load(name: &str) -> String {
    let candidates = [
        format!("../../data/json/{}", name),
        format!("../data/json/{}", name),
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

fn parse_and_assert(name: &str) {
    let input = load(name);
    let ctx = __JsonSlabEnumCtx::with_capacity(input.len() / 32);
    let parser = JsonSlab::value();
    let (result, state) = parser.parse_return_state_with_context(&input, &ctx);
    assert!(result.is_some(), "{}: parse returned None", name);
    assert!(
        state.offset >= input.trim_end().len(),
        "{}: incomplete parse ({}/{})",
        name,
        state.offset,
        input.len(),
    );
}

#[test]
fn parse_data_json() {
    parse_and_assert("data.json");
}

#[test]
fn parse_twitter_json() {
    parse_and_assert("twitter.json");
}

#[test]
fn parse_citm_catalog_json() {
    parse_and_assert("citm_catalog.json");
}

#[test]
fn parse_canada_json() {
    parse_and_assert("canada.json");
}
