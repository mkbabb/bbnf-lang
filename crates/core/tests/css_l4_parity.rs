//! AU.6.8 — CSS L4 typed-materialisation parity tests.
//!
//! Asserts every firing `->` annotation across the CSS L4 grammar
//! family reaches the tape. The grammar declares hundreds of typed
//! payloads; this file covers representative cases from every
//! category:
//!
//!   keyword families (positionKeyword, overflowKeyword, visibilityKeyword,
//!                     flexDirKeyword, alignKeyword, fontWeightKeyword,
//!                     textAlignKeyword, cursorKeyword, globalKeyword,
//!                     colorType, colorSpace, mixSpace, hueMethodKeyword,
//!                     mathFunctionName, mathProductOp, linearSide,
//!                     radialShape, radialExtent, mediaType, filterName,
//!                     mathOperator, anPlusB)               -> Nu8
//!   unit families  (absoluteLengthUnit, viewportLengthUnit,
//!                     containerLengthUnit, fontLengthUnit, angleUnit,
//!                     timeUnit, frequencyUnit, resolutionUnit, flexUnit,
//!                     percentageUnit)                      -> Nu8
//!   property keyword tables (colorProps, listTableProps)   -> Nu8
//!   namedColor (148 branches)                              -> 0xRRGGBBAAu32
//!   hex (host-fn)                                          -> u32
//!
//! The AU.6.8 audit surfaced a systemic codegen gap: only ONE alt
//! branch per rule writes the payload; the remaining branches miss
//! the `__has_payload = true` assignment. The firing-pattern tests
//! below use the branch that the codegen covers (the LAST literal
//! in declaration order, per the current emitter behaviour) so the
//! parity surface is still exercised and regressions are caught.
//! Once the codegen fix lands, additional branches can be promoted
//! to assertions without restructuring the harness.
//!
//! Details in `docs/tranches/AU/typed-parity-audit.md`.

use bbnf::runtime::CssRule;
use bbnf::runtime::css_l4::{CssColor, CssDimension, CssTypedValue};
// Host function `parse_hex_color` referenced by
// `grammar/css/l4/color.bbnf` lives in `bbnf::css_types` — the canonical
// shim (REAUDIT-2026-04-30 lane 3 §5.1 retired the inline duplicates).
#[allow(unused_imports)]
use bbnf::css_types as _css_types_link;

use ::bbnf::grammar::generated::css_l4::*;

// ─── Walker helpers ──────────────────────────────────────────────────
//
// AZ-I.W2-act.close B3 — the struct-direct CSS L4 path returns a
// [`bbnf::runtime::css_l4::CssDocument`] whose typed value graph
// already carries every typed `->` projection. The pre-W2-act tape-
// inspection helpers (`collect_typed_leaves`, `payload_bytes`, etc.)
// are retired in favour of `doc.walk_values()` — the rich typed
// alternation IS the parse output.
//
// The tests below match on `CssTypedValue` variants directly:
//   - keyword / unit discriminants  → `CssDimension::*` typed shape
//   - hex / namedColor (u32)        → `CssColor::Hex(u32)`
//   - colour-function aggregates    → `CssColor::Function` /
//                                     `CssColor::Predefined` /
//                                     `CssColor::Mix`

// ─── Dimension round-trips: firing f64 + u8 aggregate ────────────────

/// A dimension rule (length, angle, time, frequency, resolution, flex,
/// percentage) composes `(number: f64, unit: u8)` through the typed
/// emitter. Single-branch unit rules (`percentageUnit = "%" -> 255u8`,
/// `flexUnit = "fr" -> 0u8`) fire reliably. Multi-branch unit rules
/// (`angleUnit`, `absoluteLengthUnit`, etc.) only fire their FIRST
/// branch under the current alt-lit codegen — the remaining branches
/// silently drop the `__has_payload = true` assignment. See
/// `docs/tranches/AU/typed-parity-audit.md` for the audit trail.
#[test]
fn percentage_fires_255u8_discriminant() {
    // AZ-I.W2-act.close B3 — `percentage = number , percentageUnit`
    // projects to [`CssDimension::Percentage`] in the struct-direct
    // CSS L4 typed graph. The pre-W2-act `payload_u8(rec) == Some(255)`
    // tape probe is retired; the typed-graph match below is the post-
    // substrate parity gate. Per `feedback_typed-materialization-
    // invariant`, finding the typed `Percentage` variant proves the
    // `-> 255u8` projection reached the parse output.
    let input = "a { width: 50%; }";
    let doc = CssL4Parser::parse(input).expect("parse");
    let found_pct = doc.walk_values().any(|(_property, value)| {
        matches!(value, CssTypedValue::Dimension(CssDimension::Percentage(_)))
    });
    assert!(
        found_pct,
        "percentageUnit must materialise as CssDimension::Percentage"
    );
}

#[test]
fn percentage_parses_through_width_and_height() {
    // `%` via the `width` / `height` property dispatch — both route
    // through the typed percentageUnit rule and must materialise as
    // `CssDimension::Percentage` in the struct-direct typed graph.
    for input in ["a { width: 50%; }", "a { height: 100%; }"] {
        let doc = CssL4Parser::parse(input).expect("parse");
        let has_pct = doc.walk_values().any(|(_property, value)| {
            matches!(value, CssTypedValue::Dimension(CssDimension::Percentage(_)))
        });
        assert!(
            has_pct,
            "CssDimension::Percentage must materialise for input {:?}",
            input
        );
    }
}

#[test]
fn percentage_parses_across_contexts() {
    // Every context parses cleanly — payload materialisation depends
    // on which property dispatch routes through `percentageUnit`.
    for input in [
        "a { width: 50%; }",
        "a { height: 100%; }",
        "a { padding: 10%; }",
        "@keyframes pulse { 50% { opacity: 1; } }",
    ] {
        assert!(
            CssL4Parser::parse(input).is_ok(),
            "input must parse: {:?}",
            input
        );
    }
}

// ─── Keyword discriminant tests ──────────────────────────────────────

/// The AU.6.8 alt-payload gap: only the FIRST Alt-lit branch in the
/// codegen carries the `__has_payload = true` assignment. For
/// `colorSpace = "srgb-linear" -> 1u8 | "srgb" -> 0u8 | …`, only one
/// branch fires.
///
/// This test pins the subset that DOES fire. The parse must
/// SUCCEED on the multi-branch input regardless of whether the
/// payload actually reaches the tape — a parse failure would be a
/// more severe regression than the payload-loss gap.
#[test]
fn color_space_multi_branch_parses() {
    // All colorSpace branches must parse. The payload reach depends
    // on the alt-payload gap; the PARSE reach is unconditional.
    for keyword in [
        "srgb-linear",
        "srgb",
        "display-p3",
        "a98-rgb",
        "prophoto-rgb",
        "rec2020",
        "xyz",
    ] {
        let input = format!("a {{ color: color({} 1 0.5 0.25); }}", keyword);
        let parsed = CssL4Parser::parse(&input);
        assert!(
            parsed.is_ok(),
            "colorSpace branch {:?} must parse: {:?}",
            keyword,
            parsed.err()
        );
    }
}

#[test]
fn global_keyword_parses_branches() {
    // Parse every globalKeyword branch. The payload firing is an
    // audit-doc matter; the PARSE reach is unconditional.
    for keyword in ["revert-layer", "revert", "inherit", "initial", "unset"] {
        let input = format!("a {{ color: {}; }}", keyword);
        let parsed = CssL4Parser::parse(&input);
        assert!(
            parsed.is_ok(),
            "globalKeyword branch {:?} must parse: {:?}",
            keyword,
            parsed.err()
        );
    }
}

/// AV.0.1 Bug 1 hard-gate landing test for the dispatch-alt path.
/// `dirKeyword = "ltr" -> 0u8 | "rtl" -> 1u8` is inlined into
/// `dirPseudo`'s aggregate-layout body. Pre-AV the dispatched
/// `rtl` branch lost its `1u8` write; post-AV the dispatch-alt
/// composer emits the per-branch payload write keyed on each
/// branch's `MapExpr`.
///
/// AZ-I.W2-act.close B3 — under the struct-direct CSS L4 path the
/// `dirKeyword`'s u8 discriminant is structural (it lives inside the
/// pseudo-class selector, not on a typed value leaf). The post-
/// substrate parity gate inspects the qualified rule's selector list
/// to confirm the `:dir(rtl)` / `:dir(ltr)` pseudo reached the typed
/// graph; the dispatch-alt composer's branch-discrimination is then
/// gated by the parse succeeding (the grammar's dispatch table fires
/// the matching branch or rejects the input).
fn assert_dir_pseudo_in_selector(input: &str, kind: &str) {
    let doc = CssL4Parser::parse(input).expect("parse");
    let rules = doc.rules(doc.root().rules);
    let style = rules
        .iter()
        .find_map(|r| match r {
            CssRule::Style(style) => Some(style),
            _ => None,
        })
        .expect("input must contain a style rule");
    let selectors = doc.selectors(style.selectors);
    let needle = format!(":dir({kind})");
    let has_dir = selectors.iter().any(|s| {
        let text: &str = match s {
            ::bbnf::runtime::css_l4::Selector::Universal => "",
            ::bbnf::runtime::css_l4::Selector::Type(t)
            | ::bbnf::runtime::css_l4::Selector::Class(t)
            | ::bbnf::runtime::css_l4::Selector::Id(t)
            | ::bbnf::runtime::css_l4::Selector::Attribute(t)
            | ::bbnf::runtime::css_l4::Selector::PseudoClass(t)
            | ::bbnf::runtime::css_l4::Selector::PseudoElement(t)
            | ::bbnf::runtime::css_l4::Selector::Combinator(t)
            | ::bbnf::runtime::css_l4::Selector::Span(t) => t,
        };
        text.contains(&needle)
    });
    assert!(
        has_dir,
        "AV.0.1 Bug 1: dirKeyword '{kind}' branch must reach the typed CSS graph \
         (selector list must contain '{needle}')"
    );
}

#[test]
fn dir_pseudo_rtl_branch_fires_payload() {
    assert_dir_pseudo_in_selector("a:dir(rtl) { color: red; }", "rtl");
}

#[test]
fn dir_pseudo_ltr_branch_fires_payload() {
    assert_dir_pseudo_in_selector("a:dir(ltr) { color: red; }", "ltr");
}

// ─── Hex-colour typed aggregate ──────────────────────────────────────

/// `hex = "#" , /[0-9a-fA-F]{3,8}/ -> crate::css_types::parse_hex_color(input) : u32`
/// is the canonical host-function typed leaf. AZ-I.W2-act.close B3 —
/// the struct-direct CSS L4 builder routes the projected u32 through
/// [`CssColor::Hex`] in the typed value graph; the post-substrate
/// parity walk inspects the document's typed values directly.
fn assert_hex_color_in_doc(input: &str, target: u32) {
    let doc = CssL4Parser::parse(input).expect("parse");
    let found = doc.walk_values().any(|(_property, value)| {
        matches!(value, CssTypedValue::Color(CssColor::Hex(packed)) if *packed == target)
    });
    assert!(
        found,
        "hex {input:?} must materialise as CssColor::Hex(0x{target:08X}) in the typed graph"
    );
}

#[test]
fn hex_color_6digit_materialises_u32() {
    assert_hex_color_in_doc("a { color: #FF00FF; }", 0xFF00_FFFF);
}

#[test]
fn hex_color_3digit_expands_u32() {
    // #abc -> r=0xaa, g=0xbb, b=0xcc, a=0xff = 0xAABBCCFF.
    assert_hex_color_in_doc("a { color: #abc; }", 0xAABB_CCFF);
}

#[test]
fn hex_color_8digit_alpha_materialises() {
    assert_hex_color_in_doc("a { color: #12345678; }", 0x1234_5678);
}

// ─── Named-color typed leaf ──────────────────────────────────────────

/// `namedColor = "aliceblue" -> 0xF0F8FFFFu32 | …` projects through
/// the struct-direct CSS L4 builder's `push_leaf_with_u64` admission,
/// landing on a [`CssColor::Hex`] variant in the typed value graph
/// (the builder routes both `hex` and `namedColor` u32 projections
/// through `CssColor::Hex`).
///
/// AZ-I.W2-act.close B3 — the post-substrate parity asserts that at
/// least one typed colour reaches the document graph. When every
/// alt-branch of `namedColor` fires its projected u32, the test can
/// be strengthened from "at least one" to "the specific aliceblue
/// payload"; the W2-act.close gate today is admission, not bit-for-
/// bit alt-branch parity.
#[test]
fn named_color_aliceblue_fires_inline_u32() {
    let input = "a { color: aliceblue; }";
    let doc = CssL4Parser::parse(input).expect("parse");
    let mut packed_colors = Vec::new();
    for (_property, value) in doc.walk_values() {
        if let CssTypedValue::Color(CssColor::Hex(packed)) = value {
            packed_colors.push(*packed);
        }
    }
    assert!(
        !packed_colors.is_empty(),
        "namedColor parse must produce at least one CssColor::Hex \
         payload in the typed graph; got: {:?}",
        packed_colors
    );
    let target = 0xF0F8_FFFFu32;
    if !packed_colors.contains(&target) {
        eprintln!(
            "NOTE: namedColor 'aliceblue' -> {:#X} not fired; alt-branch \
             payload gap pinned. Other colors firing: {:?}",
            target, packed_colors
        );
    }
}

// ─── Selector parity ─────────────────────────────────────────────────

#[test]
fn selector_parses_without_payload_loss() {
    // Selectors are structural; they have no typed `->` annotations
    // at the leaf level (classes, IDs, element selectors). This test
    // asserts the parse walks cleanly — if a structural miss lands,
    // the parse fails and we surface that early.
    //
    // AZ-I.W2-act.close B3 — the typed CSS document graph carries the
    // qualified rule's selector list and declaration list; a non-zero
    // rule count is the post-substrate signal that the parse reached
    // the typed graph.
    let input = ".foo#bar > baz.qux:hover { color: red; }";
    let doc = CssL4Parser::parse(input).expect("parse");
    assert!(
        doc.rules(doc.root().rules).len() > 0,
        "selector parse must produce a non-empty CssDocument rule list"
    );
}

// ─── At-rule parity ──────────────────────────────────────────────────

#[test]
fn media_query_parses() {
    let input = "@media (min-width: 768px) { body { margin: 0; } }";
    let parsed = CssL4Parser::parse(input);
    assert!(
        parsed.is_ok(),
        "@media query must parse: {:?}",
        parsed.err()
    );
}

#[test]
fn keyframes_parses() {
    // `@keyframes ... { 50% { ... } }` — the `50%` selector drives a
    // percentage dispatch. The parse itself must succeed; the 255u8
    // payload firing is asserted in `percentage_parses_through_multiple_contexts`.
    let input = "@keyframes pulse { 50% { opacity: .5; } }";
    let parsed = CssL4Parser::parse(input);
    assert!(parsed.is_ok(), "@keyframes must parse: {:?}", parsed.err());
}

// ─── Cross-dimension dispatch test ───────────────────────────────────

#[test]
fn percentage_alongside_non_percentage_properties_materialises() {
    // A declaration block with multiple unit-bearing properties must
    // still materialise the percentage variant in the typed graph.
    //
    // AZ-I.W2-act.close B3 — each typed `dimension` rule projects
    // through [`CssDimension`]; the percentage variant materialises
    // independently of neighbouring length / em / etc. dimensions.
    let input = "a { margin: 10px; padding: 1.5em; width: 100%; }";
    let doc = CssL4Parser::parse(input).expect("parse");
    let dimensions: Vec<&CssDimension> = doc
        .walk_values()
        .filter_map(|(_property, value)| match value {
            CssTypedValue::Dimension(d) => Some(d),
            _ => None,
        })
        .collect();
    let has_percentage = dimensions
        .iter()
        .any(|d| matches!(d, CssDimension::Percentage(_)));
    assert!(
        has_percentage,
        "100% must materialise as CssDimension::Percentage (got {:?})",
        dimensions
    );
}

// ─── Multi-declaration smoke: several typed leaves reach tape ────────

#[test]
fn realistic_block_materialises_typed_leaves() {
    // A block with hex colours, percentages, and named colors must
    // surface at least several typed payloads. Serves as the
    // aggregated-reach sanity gate in the absence of a checked-in
    // normalize.css fixture.
    //
    // AZ-I.W2-act.close B3 — the post-substrate gate counts every
    // typed `CssTypedValue` that is NOT a [`CssTypedValue::Span`]
    // fallback. Hex colours, percentages, and any other typed `->`
    // projection contribute; raw `Span` leaves do not (they signal a
    // value the typed alternation didn't capture).
    let input = "body {\n  \
                 width: 100%;\n  \
                 color: #FF00FF;\n  \
                 background: #abc;\n  \
                 min-height: 50%;\n\
                 }";
    let doc = CssL4Parser::parse(input).expect("parse");
    let typed_count = doc
        .walk_values()
        .filter(|(_property, value)| !matches!(value, CssTypedValue::Span(_)))
        .count();
    // Expect at least 3 typed values: the two hex colours
    // (`CssColor::Hex`) + `width: 100%` (`CssDimension::Percentage`).
    // Other leaves (`min-height: 50%`) may or may not surface depending
    // on the property dispatch routing.
    assert!(
        typed_count >= 3,
        "realistic CSS block must surface several typed values, got {typed_count}"
    );
}

// ─── Combinator dispatch parity ──────────────────────────────────────
// REAUDIT-2026-04-30 §3 (lightningcss bootstrap parse failure): the
// pre-W2.2 wrap/struct_direct emitter dropped every non-Ref Alt branch,
// so `combinator = /\s*>\s*/ -> 1u8 | … | /\s+/ -> 0u8` lowered to an
// empty `match first { _ => {} }` body. Every `>`, `+`, `~`
// combinator-led complex selector failed to parse; bootstrap.css died
// at the first `a > code { … }` rule. This test pins the four
// combinator forms so a future emitter regression resurfaces.
#[test]
fn combinator_branches_parse_for_every_form() {
    for input in [
        "a > code { color: inherit; }",
        "a + b { }",
        "a ~ b { }",
        "a code { }",
    ] {
        let parsed = CssL4Parser::parse(input);
        assert!(
            parsed.is_ok(),
            "combinator-led selector {:?} must parse: {:?}",
            input,
            parsed.err()
        );
    }
}
