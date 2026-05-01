//! Integration tests for CSS L4 typed parsing through the tape-first
//! `the proc-macro derive (retired B2)` codegen path using the l4/stylesheet.bbnf grammar.

// Host function `parse_hex_color` referenced by
// `grammar/css/l4/color.bbnf` lives in `bbnf::css_types` (canonical
// shim per REAUDIT-2026-04-30 lane 3 §5.1). The pre-AZ-III inline
// `mod css_types {…}` shadow — which also carried unused
// `LengthUnit` / `AngleUnit` / `TimeUnit` / `CssColor` /
// `parse_rgb_color` symbols with zero references — is retired.
#[allow(unused_imports)]
use bbnf::css_types as _css_types_link;

use ::bbnf::grammar::generated::css_l4::*;

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
    CssL4Parser::parse(input).is_ok()
}

// ---------------------------------------------------------------------------
// Unit value tests
// ---------------------------------------------------------------------------

#[test]
fn unit_px() {
    assert!(parse_full("a { x: 100px; }"), "100px should parse");
}

#[test]
fn unit_percent() {
    assert!(parse_full("a { x: 100%; }"), "100% should parse");
}

#[test]
fn unit_deg() {
    assert!(parse_full("a { x: 100deg; }"), "100deg should parse");
}

#[test]
fn unit_ms() {
    assert!(parse_full("a { x: 100ms; }"), "100ms should parse");
}

#[test]
fn unit_em() {
    assert!(parse_full("a { x: 100em; }"), "100em should parse");
}

#[test]
fn unit_rem() {
    assert!(parse_full("a { x: 100rem; }"), "100rem should parse");
}

#[test]
fn unit_fr() {
    assert!(parse_full("a { x: 100fr; }"), "100fr should parse");
}

// ---------------------------------------------------------------------------
// var() / calc() tests
// ---------------------------------------------------------------------------

#[test]
fn calc_expression() {
    assert!(
        parse_full("body { width: calc(100% - 20px); }"),
        "calc() should parse"
    );
}

#[test]
fn var_simple() {
    assert!(
        parse_full("body { color: var(--x); }"),
        "var(--x) should parse"
    );
}

#[test]
fn var_with_fallback() {
    assert!(
        parse_full("body { color: var(--x, red); }"),
        "var(--x, red) should parse"
    );
}

// ---------------------------------------------------------------------------
// @media test
// ---------------------------------------------------------------------------

#[test]
fn media_query() {
    assert!(
        parse_full("@media (min-width: 768px) { body { margin: 0; } }"),
        "@media query should parse"
    );
}

// ---------------------------------------------------------------------------
// Vendor-prefix and multi-rule isolation
// ---------------------------------------------------------------------------

#[test]
fn vendor_prefix_property() {
    assert!(
        parse_full("html { -webkit-text-size-adjust: 100%; }"),
        "-webkit vendor prefix property should parse"
    );
}

#[test]
fn two_rules_sequential() {
    assert!(
        parse_full("html { margin: 0; }\nbody { margin: 0; }"),
        "two sequential rules should parse"
    );
}

// ---------------------------------------------------------------------------
// Full file tests
// ---------------------------------------------------------------------------

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
    // bootstrap.css requires a large stack due to deep recursion in funcBody.
    let input = load_css("bootstrap.css");
    let child = std::thread::Builder::new()
        .name("bootstrap-parse".into())
        .stack_size(67_108_864) // 64 MiB
        .spawn(move || {
            assert!(
                parse_full(&input),
                "bootstrap.css: parse failed or incomplete"
            );
        })
        .expect("failed to spawn thread");
    child.join().expect("bootstrap parse thread panicked");
}

// ---------------------------------------------------------------------------
// AU.2.4 — Typed hex colour round-trip (W2 agent B)
//
// Asserts that the aggregate u32 payload produced by the tape-first
// emitter round-trips through the parsed tape: the matched span is
// the hex digits and the payload bytes decode back to the
// lightningcss-equivalent 0xRRGGBBAA u32.
// ---------------------------------------------------------------------------

/// AZ-I.W2-act.close B3 — walk the parsed [`CssDocument`] for a
/// hex-colour typed leaf, returning the decoded u32 payload when it
/// matches `target`. Returns `None` when the parse fails or no
/// matching `CssColor::Hex` (the variant `parse_hex_color`'s u32
/// projection lands on per the struct-direct builder) appears in the
/// document's typed value graph.
///
/// The post-W2-act.B3 builder routes `hex = "#" , /…/ -> u32`
/// through [`bbnf::runtime::css_l4::CssTypedValue::Color`] /
/// [`bbnf::runtime::css_l4::CssColor::Hex`]; the typed walk is more
/// direct than the pre-W2-act `payload_bytes(rec, 4)` tape inspection
/// because the value IS the parse output.
fn find_hex_payload_u32(css_input: &str, target: u32) -> Option<u32> {
    use ::bbnf::runtime::css_l4::{CssColor, CssTypedValue};

    let doc = CssL4Parser::parse(css_input).ok()?;
    for (_property, value) in doc.walk_values() {
        if let CssTypedValue::Color(CssColor::Hex(packed)) = value {
            if *packed == target {
                return Some(*packed);
            }
        }
    }
    None
}

#[test]
fn hex_color_roundtrip_6digit() {
    let target: u32 = 0xFF00_FFFF;
    let u32_val = find_hex_payload_u32("a { color: #FF00FF; }", target)
        .expect("hex #FF00FF must materialise as a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}

#[test]
fn hex_color_roundtrip_3digit() {
    // #abc -> r=0xaa, g=0xbb, b=0xcc, a=0xff = 0xaabbccff
    let target: u32 = 0xAABB_CCFF;
    let u32_val = find_hex_payload_u32("a { color: #abc; }", target)
        .expect("hex #abc must expand to 0xAABBCCFF in a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}

#[test]
fn hex_color_roundtrip_8digit() {
    let target: u32 = 0x1234_5678;
    let u32_val = find_hex_payload_u32("a { color: #12345678; }", target)
        .expect("hex #12345678 must materialise as a 4-byte KvPair payload");
    assert_eq!(u32_val, target);
}
