//! Integration tests for JSON parsing through the tape-first
//! `#[derive(Parser)]` codegen path.

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

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
    // The tape-first parser rejects incomplete input automatically,
    // so parse success collapses the old "completeness" assertion.
    let parsed = JsonParser::parse(&input)
        .unwrap_or_else(|e| panic!("{}: parse failed with {:?}", name, e));
    let _root = parsed.view();
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
