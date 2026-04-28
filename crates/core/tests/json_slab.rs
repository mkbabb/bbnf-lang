//! AZ-I.W2-act.B1 — JSON struct-direct slab smoke tests.
//!
//! Post-flip, `JsonParser::parse(input)` returns
//! `Result<JsonDocument<'_>, ParseErr>` (the orchestrator's regen
//! consumes the `EmitStrategy::StructDirect` resolver arm and emits a
//! struct-builder body in `crates/core/src/grammar/generated/json.rs`).
//! These tests exercise the document accessor surface — root borrow,
//! arena handle resolution via [`JsonView`], and the typed
//! discriminator — for the canonical fixture corpus.
//!
//! No tape symbols are touched: every assertion routes through
//! [`JsonDocument::view`] and the returned [`JsonView`]'s accessors,
//! mirroring the API in `runtime/json/document.rs`.

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::json::JsonKind;

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
    // Post-W2-act.B1 the struct-direct path returns `JsonDocument`
    // directly — the document carries every typed compound + leaf
    // produced by the parse, no tape walk required.
    let doc = JsonParser::parse(&input)
        .unwrap_or_else(|e| panic!("{}: parse failed with {:?}", name, e));

    // Sanity: the root resolves to a recognised JSON kind. The test
    // corpus consists of object / array fixtures, so the root must
    // dispatch to one of the compound shapes.
    let view = doc.view();
    let kind = view.kind();
    assert!(
        matches!(kind, JsonKind::Object | JsonKind::Array),
        "{name}: top-level JSON value must resolve to Object or Array, got {kind:?}",
    );

    // Empty-handle vs populated-handle resolution must round-trip
    // through the arena without panicking. Walk the immediate
    // children once to confirm the substrate is wired.
    match view.kind() {
        JsonKind::Object => {
            // The root is an object handle; resolve through the view.
            if let bbnf::runtime::JsonValue::Object(id) = doc.root {
                let pairs = doc.object(id);
                // Even an empty object must resolve to an empty
                // slice without aborting.
                let _ = pairs.len();
            } else {
                panic!("{name}: kind() reported Object but root is not Object");
            }
        }
        JsonKind::Array => {
            if let bbnf::runtime::JsonValue::Array(id) = doc.root {
                let items = doc.array(id);
                let _ = items.len();
            } else {
                panic!("{name}: kind() reported Array but root is not Array");
            }
        }
        other => panic!(
            "{name}: unexpected root kind {other:?} — fixture corpus is object/array only",
        ),
    }
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
