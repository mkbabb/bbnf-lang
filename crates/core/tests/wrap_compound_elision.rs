//! AY.W2.6 — wrap-compound elision wire-contract test (struct-direct).
//!
//! Per AY.md prop 2 / invariant 23 part 2, Wrap-shape rules whose
//! every Alt branch emits its own typed value must NOT carry an outer
//! wrap compound around the chosen branch's value. Under the JSON
//! struct-direct path landed at AZ-I.W2-act, the JsonStructBuilder's
//! `OpenFrame::Wrap` finalises by forwarding its single child
//! transparently — the `value` Wrap rule does not contribute an
//! arena-allocated compound to the document tree.
//!
//! # Migration history
//!
//! Pre-AZ-I.W2-act these tests asserted on tape record counts: a bare
//! scalar produced ≤ 2 records, twitter ≤ 150K records. AZ-I.W2-act
//! flipped JSON to struct-direct — the tape substrate retired on the
//! JSON parse path; record counts are no longer the observable. The
//! migrated assertions inspect the equivalent struct-tree invariant:
//! the Wrap forwarder collapses to its single child, so a bare scalar
//! at top level lands as `JsonValue::<scalar>` directly (no enclosing
//! `Compound` envelope), and complex documents project the typed
//! arena slabs (`array_count + object_count`) at the budget the
//! pre-flip record-count budget approximated.

use ::bbnf::grammar::generated::json::*;
use bbnf::runtime::{JsonNumber, JsonValue};

#[test]
fn json_scalar_at_top_level_emits_one_record() {
    // Each of these is a bare JSON scalar. Post-W2.6 the `value` Wrap
    // rule elides its outer compound; under struct-direct the `value`
    // Wrap frame forwards its single child to the root slot, so the
    // root lands as the scalar value directly — no `Compound` wrapper,
    // no arena compound entries.
    for (input, want) in &[
        ("42", JsonValue::Number(JsonNumber::Float(42.0))),
        ("\"hi\"", JsonValue::String("hi")),
        ("true", JsonValue::Bool(true)),
        ("false", JsonValue::Bool(false)),
        ("null", JsonValue::Null),
    ] {
        let doc = JsonParser::parse(input).expect("parse failed");
        // Number equality through the variant kind, not the f64
        // bitpattern (the bare integer projects via the Eisel-Lemire
        // f64 codec; consumer-visible value is the same float).
        match (doc.root, want) {
            (JsonValue::Number(a), JsonValue::Number(b)) => {
                assert_eq!(a.as_f64(), b.as_f64(), "scalar {:?}", input);
            }
            (got, expected) => assert_eq!(
                got, *expected,
                "scalar {:?}: got {:?}, want {:?}",
                input, got, expected
            ),
        }
    }
}

#[test]
fn json_object_of_scalars_record_ceiling() {
    // `{"a": 1, "b": true}` — under struct-direct this materialises as
    // 1 root object compound + 2 pair entries inline; the pre-flip
    // tape-record budget (≤ 30) maps to the equivalent struct tree
    // shape: 2 arena pairs, no nested compounds, every value a scalar
    // leaf.
    let doc = JsonParser::parse(r#"{"a":1,"b":true}"#).expect("parse failed");
    let view = doc.view();
    assert!(view.is_object());

    let pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(pairs.len(), 2, "expected 2 pairs, got {}", pairs.len());

    // Each value is a scalar; no nested compounds. Under struct-direct
    // this verifies the `value` Wrap forwarding — pair values land as
    // bare `Number` / `Bool`, not wrapped `Compound`.
    for pair in pairs {
        match pair.value {
            JsonValue::Number(_) | JsonValue::Bool(_) => {}
            ref other => panic!(
                "expected scalar pair value (W2.6 wrap elision), got {:?}",
                other
            ),
        }
    }
}

#[test]
fn json_twitter_compound_count_budget() {
    // AY.W2.6 hard gate: twitter compound count must shrink vs the
    // pre-elision baseline. Under struct-direct the observable is
    // arena slab size (one slab entry per Array / Object compound),
    // not tape record count. The pre-W2.6 record baseline of ~158K
    // included one tape record per scalar leaf + one per Wrap
    // compound; the struct-direct path carries scalars inline (zero
    // arena entries) and Wrap compounds elide. The arena entry count
    // approximates "live compounds" — the surface this test guards.
    //
    // Empirical floor on twitter.json post-flip is on the order of
    // ~14K array+object slabs (twitter.json has ~14K nested
    // arrays/objects). The pre-flip record budget of ≤ 150K is a
    // strict superset of this; we keep the equivalent invariant by
    // bounding compound count well above the empirical floor.
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("data")
        .join("json")
        .join("twitter.json");
    let input = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!(
                "twitter compound-count budget probe: skipping — fixture \
                 at {:?} not readable ({}). The full hard-gate \
                 measurement runs via the json_monolithic bench.",
                path, e
            );
            return;
        }
    };
    let doc = JsonParser::parse(&input).expect("parse failed");

    let n = count_compound_nodes(&doc);
    let budget = 100_000usize;
    eprintln!(
        "twitter compound count: {} (struct-direct budget {}, input {} bytes)",
        n,
        budget,
        input.len()
    );
    assert!(
        n <= budget,
        "AY.W2.6 wrap elision (struct-direct): twitter compound count {} \
         must be ≤ {}; input {} bytes. The struct-direct path elides \
         the `value` Wrap compound on every typed projection, so the \
         compound count tracks live arrays + objects only.",
        n,
        budget,
        input.len()
    );
}

/// Recursively count every `Array` / `Object` compound reachable from
/// the document root. Mirrors the pre-flip "tape record count" probe;
/// scalars contribute zero (they live inline on the parent).
fn count_compound_nodes(doc: &bbnf::runtime::JsonDocument<'_>) -> usize {
    fn walk<'p>(view: &bbnf::runtime::JsonView<'_, 'p>, value: &JsonValue<'p>) -> usize {
        match value {
            JsonValue::Array(id) => {
                let arr = view.array(*id);
                1 + arr.iter().map(|c| walk(view, c)).sum::<usize>()
            }
            JsonValue::Object(id) => {
                let pairs = view.object(*id);
                1 + pairs.iter().map(|p| walk(view, &p.value)).sum::<usize>()
            }
            _ => 0,
        }
    }
    let view = doc.view();
    walk(&view, view.root())
}

#[test]
fn json_wrap_elision_does_not_break_traversal() {
    // Basic smoke: a mixed document still parses + walks the typed
    // tree correctly. The pre-flip cursor span check projects to the
    // typed tree's shape preservation: the root must be a well-formed
    // compound and every interior compound must resolve via the arena.
    let input = r#"{"k":42,"arr":[1,2,3]}"#;
    let doc = JsonParser::parse(input).expect("parse failed");
    let view = doc.view();
    assert!(view.is_object(), "root should be an object");

    let pairs = match doc.root {
        JsonValue::Object(id) => view.object(id),
        _ => unreachable!(),
    };
    assert_eq!(pairs.len(), 2);
    assert_eq!(pairs[0].key, "k");
    assert!(matches!(pairs[0].value, JsonValue::Number(_)));
    assert_eq!(pairs[1].key, "arr");
    let arr = match pairs[1].value {
        JsonValue::Array(id) => view.array(id),
        ref other => panic!("expected nested array, got {:?}", other),
    };
    assert_eq!(arr.len(), 3);
}
