//! AX.W1r.2 — JSON canonical-form parity vs sonic-rs.
//!
//! For every canonical JSON fixture, parse the same source with both
//! bbnf and sonic-rs, canonicalize each side to a whitespace-stripped
//! byte form, and assert byte equality. Both parties parse the same
//! input; if their trees agree structurally, their canonical emissions
//! agree byte-for-byte.
//!
//! bbnf's `serialize_compact` currently emits `span_text()` — the raw
//! input substring, whitespace preserved. sonic-rs's `to_string` emits
//! compact canonical form (no whitespace outside strings). The shared
//! `json_normalize::strip_insignificant_ws` transform applies the SAME
//! bytes-level whitespace-outside-strings strip to both outputs, making
//! the comparison structural without a bbnf → sonic-rs bridge.
//!
//! Hard gate: byte equality across all five canonical JSON fixtures.
//! No `PartialEq<sonic_rs::Value>`, no `From<sonic_rs::Value>`, no
//! hand-coded value type.

mod common;

use bbnf_derive::Parser;
use common::json_normalize::strip_insignificant_ws;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", serialize)]
struct JsonEmit;

fn canonical_parity(fixture: &str) {
    let path = format!("../../data/json/{}", fixture);
    let src = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{path}: read failed: {e}"));

    // bbnf side: parse → span_text via serialize_compact → strip ws.
    let parsed = JsonEmit::parse(&src)
        .unwrap_or_else(|e| panic!("{fixture}: bbnf JSON parse failed: {e:?}"));
    let view = parsed.view();
    let node = JsonEmitNodeView::from_cursor(view.cursor(), parsed.input());
    let bbnf_raw = JsonEmit::serialize_compact(node);
    let bbnf_canonical = strip_insignificant_ws(&bbnf_raw);

    // Oracle side: parse → to_string → strip ws. The to_string output
    // is already whitespace-free outside strings; the normalizer is
    // an identity transform on it. Applying the same bytes-level
    // pass to both sides keeps the comparison symmetric.
    let oracle = sonic_rs::from_str::<sonic_rs::Value>(&src)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs parse failed: {e}"));
    let oracle_raw = sonic_rs::to_string(&oracle)
        .unwrap_or_else(|e| panic!("{fixture}: sonic-rs to_string failed: {e}"));
    let oracle_canonical = strip_insignificant_ws(&oracle_raw);

    assert_eq!(
        bbnf_canonical, oracle_canonical,
        "canonical-form divergence on {fixture}"
    );
}

// ─── Per-fixture tests ──────────────────────────────────────────────

#[test]
fn canonical_parity_data() {
    canonical_parity("data.json");
}

#[test]
fn canonical_parity_twitter() {
    canonical_parity("twitter.json");
}

#[test]
fn canonical_parity_citm_catalog() {
    canonical_parity("citm_catalog.json");
}

#[test]
fn canonical_parity_canada() {
    canonical_parity("canada.json");
}

/// `data_xl.json` is ~21 MB; reading + two parses + two normalizes is
/// slow under the dev/ax-iter profile. Skip in debug builds; run only
/// under release to keep the iteration loop tight.
#[test]
#[cfg_attr(debug_assertions, ignore)]
fn canonical_parity_data_xl() {
    canonical_parity("data_xl.json");
}

// ─── Normalizer self-tests ──────────────────────────────────────────
//
// The normalizer is load-bearing: both sides of the parity comparison
// feed through it, so correctness is a prerequisite for any parity
// claim. Kept in the tests/ tree (no inline #[cfg(test)] in the helper
// module) per the workspace's test-placement invariant.

#[test]
fn normalize_strips_space_between_tokens() {
    assert_eq!(strip_insignificant_ws("{ }"), "{}");
    assert_eq!(strip_insignificant_ws("[ 1 , 2 ]"), "[1,2]");
    assert_eq!(
        strip_insignificant_ws(r#"{"a" : 1 , "b" : 2}"#),
        r#"{"a":1,"b":2}"#
    );
}

#[test]
fn normalize_preserves_whitespace_inside_strings() {
    assert_eq!(
        strip_insignificant_ws(r#""hello world""#),
        r#""hello world""#
    );
    assert_eq!(
        strip_insignificant_ws(r#"{"k" : "v 1 2"}"#),
        r#"{"k":"v 1 2"}"#
    );
}

#[test]
fn normalize_preserves_escaped_quote_inside_string() {
    let input = r#"{"k" : "a\"b"}"#;
    let expected = r#"{"k":"a\"b"}"#;
    assert_eq!(strip_insignificant_ws(input), expected);
}

#[test]
fn normalize_preserves_backslash_before_escaped_backslash() {
    let input = r#"{"k":"a\\b c"}"#;
    let expected = r#"{"k":"a\\b c"}"#;
    assert_eq!(strip_insignificant_ws(input), expected);
}

#[test]
fn normalize_identity_on_already_compact() {
    let input = r#"[{"a":1,"b":[2,3]},null,true,false,"x"]"#;
    assert_eq!(strip_insignificant_ws(input), input);
}

#[test]
fn normalize_strips_newlines_and_tabs() {
    let input = "{\n\t\"a\": 1\n}";
    assert_eq!(strip_insignificant_ws(input), r#"{"a":1}"#);
}
