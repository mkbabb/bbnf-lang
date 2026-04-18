//! AW-V.W3.3 — Shape-dispatch emission regression tests.
//!
//! For each of the six W3-active shape categories (Object / Array /
//! String / Number / Keyword / Scalar), this suite pairs:
//!
//! 1. **Classifier tag assertion** — a synthetic IR whose rule bodies
//!    mirror the canonical shape, run through `mine_recognizers` +
//!    `shape_dispatch`, with the relevant [`RuleId`] asserted to carry
//!    the expected [`ShapeTag`].
//! 2. **Emitted-TokenStream golden** — the per-shape emitter invoked
//!    directly over the same rule, formatted through `prettyplease`,
//!    compared against a committed golden under
//!    `tests/fixtures/shape_dispatch_emission/`.
//!
//! The goldens freeze the *shape* of W3.2's emitter output; any
//! deliberate structural change regenerates them. The classifier
//! assertions enforce the wire contract that carries mined data from
//! `shape_dispatch` through the emitter into the `pub fn
//! parse_<shape>_<grammar>_<rule>` symbols `emit_grammar_impl`
//! consumes.
//!
//! W4 shapes (Pratt / Unordered / ArgList / Flat / Wrap / HRegex)
//! landed as part of AW-V.W4.1. The W4 detectors admit the structural
//! predicates their emitter modules consume; the emitter scaffolding
//! produces parsable-Rust TokenStreams verified by the per-shape
//! `w4_<shape>_emitter_produces_parsable_tokens` tests. The per-
//! grammar consumer wiring for CSS / Sheets lands in W4.2 / W4.3.

use bbnf::backend::rust::emitter::shapes::{
    arglist, array, flat, hregex, keyword, number, object, pratt, scalar,
    string, unordered, wrap,
};
use bbnf_ir::passes::recognizers::shape_dispatch::{
    scalar as scalar_detect, shape_dispatch, ShapeTag,
};
use bbnf_ir::{IrNode, RuleId};

#[path = "shape_dispatch_emission/fixtures.rs"]
mod fixtures;

use fixtures::*;

// ─── Shared helpers ──────────────────────────────────────────────────

/// Format a `TokenStream` into a readable string via `prettyplease`.
///
/// The emitters produce top-level `pub fn`/`pub(crate) mod` items, so
/// the token stream parses cleanly as a `syn::File`. The unparsed
/// output is deterministic and survives round-trip — regenerating a
/// golden from the same emitter output yields byte-identical text.
fn format_tokens(ts: &proc_macro2::TokenStream) -> String {
    let file: syn::File = syn::parse2(ts.clone())
        .expect("emitter output must parse as a syn::File");
    prettyplease::unparse(&file)
}

/// Compare actual emitter output against a committed golden. The
/// goldens omit the trailing newline `prettyplease` adds so the
/// diff reads cleanly; we trim both sides before comparison.
#[track_caller]
fn assert_matches_golden(actual: &str, expected: &str, golden_name: &str) {
    let a = actual.trim_end();
    let e = expected.trim_end();
    assert_eq!(
        a, e,
        "{golden_name} golden drift — re-run the golden bootstrap if the \
         emitter intentionally changed.",
    );
}

// ─── Classify + emit — Object ───────────────────────────────────────

#[test]
fn object_shape_classifies_correctly() {
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.get(rules.object), ShapeTag::Object);
}

#[test]
fn object_shape_emit_matches_golden() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.object as usize];
    let ts = object::emit_parse_object("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/object.rs.expected"
    );
    assert_matches_golden(&actual, expected, "object.rs");
}

// ─── Classify + emit — Array ────────────────────────────────────────

#[test]
fn array_shape_classifies_correctly() {
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.get(rules.array), ShapeTag::Array);
}

#[test]
fn array_shape_emit_matches_golden() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.array as usize];
    let ts = array::emit_parse_array("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/array.rs.expected"
    );
    assert_matches_golden(&actual, expected, "array.rs");
}

// ─── Classify + emit — String ───────────────────────────────────────

#[test]
fn string_shape_classifies_correctly() {
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.get(rules.string), ShapeTag::String);
}

#[test]
fn string_shape_emit_matches_golden() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.string as usize];
    let ts = string::emit_parse_string("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/string.rs.expected"
    );
    assert_matches_golden(&actual, expected, "string.rs");
}

// ─── Classify + emit — Number ───────────────────────────────────────

#[test]
fn number_shape_classifies_correctly() {
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.get(rules.number), ShapeTag::Number);
}

#[test]
fn number_shape_emit_matches_golden() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.number as usize];
    let ts = number::emit_parse_number("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/number.rs.expected"
    );
    assert_matches_golden(&actual, expected, "number.rs");
}

// ─── Classify + emit — Keyword ──────────────────────────────────────

#[test]
fn keyword_shape_classifies_correctly() {
    let (ir, rules) = build_json_ir();
    // JSON's `bool = "true" | "false"` classifies as Keyword (Alt of
    // literal-led branches).
    assert_eq!(ir.shape_assignments.get(rules.bool_rule), ShapeTag::Keyword);
    // JSON's `null = "null"` also classifies as Keyword (single-literal
    // body).
    assert_eq!(ir.shape_assignments.get(rules.null), ShapeTag::Keyword);
}

#[test]
fn keyword_shape_emit_matches_golden() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.bool_rule as usize];
    let ts = keyword::emit_parse_keyword("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/keyword.rs.expected"
    );
    assert_matches_golden(&actual, expected, "keyword.rs");
}

// ─── Classify + emit — Scalar ───────────────────────────────────────
//
// The Scalar detector admits single-Literal bodies, but so does the
// Keyword detector (Case 1 in `shape_dispatch/keyword.rs`), and
// Keyword precedes Scalar in the dispatch order. So under
// [`shape_dispatch`] a bare Literal rule classifies as [`ShapeTag::Keyword`]
// — Scalar is a defensive catch-all for single-Literal bodies a
// future Keyword-detector refinement might reject (e.g. if Keyword
// adds a `-> payload` requirement). The tag-assertion test here
// invokes [`scalar_detect::detect_scalar`] directly so we still
// exercise the shape-admission contract, and separately asserts that
// the dispatch orchestration routes the rule to Keyword — exactly
// what the wire contract requires for W3's inclusion ordering.

#[test]
fn scalar_shape_detector_admits_literal_body() {
    // The Scalar detector admits any Literal body after stripping Map
    // / OptionalWhitespace wrappers — this is the shape's invariant.
    let (ir, rules) = build_json_ir();
    assert!(
        scalar_detect::detect_scalar(rules.comma, &ir),
        "Scalar detector must admit the `comma = \",\" ?w` rule body",
    );
    assert!(
        scalar_detect::detect_scalar(rules.colon, &ir),
        "Scalar detector must admit the `colon = \":\" ?w` rule body",
    );
}

#[test]
fn scalar_shape_classifier_routes_literal_to_keyword() {
    // Keyword precedes Scalar in `shape_dispatch`, so bare Literal
    // bodies route to Keyword — this test documents the dispatch
    // ordering and guarantees Scalar is reserved for future detector
    // refinements without breaking the W3 wire contract.
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.comma),
        ShapeTag::Keyword,
        "Keyword precedes Scalar — `comma` routes to Keyword under the \
         dispatch ordering",
    );
    assert_eq!(
        ir.shape_assignments.get(rules.colon),
        ShapeTag::Keyword,
    );
}

#[test]
fn scalar_shape_emit_matches_golden() {
    // The Scalar emitter is reached via direct invocation (W3
    // dispatch routes single-Literal bodies to Keyword per the
    // precedence). The emitted shape is the Literal-only byte
    // match + push_leaf(Literal) path.
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.comma as usize];
    let ts = scalar::emit_parse_scalar("JsonFixture", rule, &ir);
    let actual = format_tokens(&ts);
    let expected = include_str!(
        "fixtures/shape_dispatch_emission/scalar.rs.expected"
    );
    assert_matches_golden(&actual, expected, "scalar.rs");
}

// ─── W4 detector activation assertions ──────────────────────────────
//
// The W4 detectors (Pratt / Unordered / ArgList / Flat / Wrap /
// HRegex) landed in W4.1. The JSON fixture exercises Wrap via
// `value = object | array | ...`; it has no Pratt / Unordered /
// ArgList / Flat / HRegex rules. Per-grammar coverage for Sheets /
// CSS / BBNF lands in the parity tests + bench suites.

#[test]
fn w4_pratt_no_hits_on_json_fixture() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Pratt), 0);
}

#[test]
fn w4_unordered_no_hits_on_json_fixture() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Unordered), 0);
}

#[test]
fn w4_arglist_no_hits_on_json_fixture() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::ArgList), 0);
}

#[test]
fn w4_flat_classifies_json_pair() {
    // AW-V.W4-fix — the Flat detector now admits Ref-headed typed
    // Seqs. JSON's `pair = string, colon >> value` is a three-Ref
    // Seq that matches. The JSON fixture therefore carries exactly
    // one Flat-classified rule.
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Flat), 1);
    assert_eq!(ir.shape_assignments.get(rules.pair), ShapeTag::Flat);
}

#[test]
fn w4_wrap_classifies_json_value() {
    // JSON `value = object | array | string | number | bool | null`
    // is the canonical Wrap-shape rule — exactly one hit.
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Wrap), 1);
    assert_eq!(ir.shape_assignments.get(rules.value), ShapeTag::Wrap);
}

#[test]
fn w4_hregex_no_hits_on_json_fixture() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::HRegex), 0);
}

// ─── W4 emitter TokenStream parse check ─────────────────────────────
//
// Each W4 emitter produces a `pub fn parse_<shape>_<grammar>_<rule>`
// item that MUST parse as valid Rust. We invoke each emitter directly
// over a rule whose shape matches the emitter's domain and assert the
// TokenStream parses — proving the emitter output is syntactically
// well-formed even before W4.2 / W4.3 wires per-grammar consumers.
//
// The emitters are referenced via JSON's `value` (Wrap) rule for
// Wrap; for the other five shapes we pass any JSON rule — the
// emitters' outputs are scaffold-level and shape-agnostic beyond
// the rule's ident + variant stamping.

#[test]
fn w4_pratt_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.number as usize];
    let ts = pratt::emit_parse_pratt("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = pratt::emit_parse_pratt_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

#[test]
fn w4_unordered_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.array as usize];
    let ts = unordered::emit_parse_unordered("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = unordered::emit_parse_unordered_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

#[test]
fn w4_arglist_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.object as usize];
    let ts = arglist::emit_parse_arglist("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = arglist::emit_parse_arglist_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

#[test]
fn w4_flat_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.pair as usize];
    let ts = flat::emit_parse_flat("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = flat::emit_parse_flat_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

#[test]
fn w4_wrap_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.value as usize];
    let ts = wrap::emit_parse_wrap("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = wrap::emit_parse_wrap_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

#[test]
fn w4_hregex_emitter_produces_parsable_tokens() {
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.string as usize];
    let ts = hregex::emit_parse_hregex("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts);
    let ts_v = hregex::emit_parse_hregex_visitor("JsonFixture", rule, &ir);
    let _ = format_tokens(&ts_v);
}

// ─── Wire-contract invariants ───────────────────────────────────────

#[test]
fn shape_dispatch_is_idempotent() {
    // Running the classifier twice over the same IR yields identical
    // assignments — it is a pure projection.
    let (ir, _) = build_json_ir();
    let direct = shape_dispatch(&ir);
    assert_eq!(
        direct.per_rule.len(),
        ir.shape_assignments.per_rule.len(),
        "shape_dispatch must be idempotent over a stable IR",
    );
    for (rule, tag) in &ir.shape_assignments.per_rule {
        assert_eq!(
            direct.per_rule.get(rule).copied(),
            Some(*tag),
            "shape_dispatch is not idempotent at rule {rule}: got {:?}, \
             expected {:?}",
            direct.per_rule.get(rule),
            Some(*tag),
        );
    }
}

#[test]
fn classified_tags_match_their_category_check() {
    // Every rule in `shape_assignments` carries either a W3 or a W4
    // tag. `None` is absent by construction (the dispatch orchestrator
    // skips None tags when populating the map).
    let (ir, _) = build_json_ir();
    for (rule_id, tag) in &ir.shape_assignments.per_rule {
        assert!(
            tag.is_classified(),
            "rule {rule_id} classified as {tag:?} — only W3 or W4 tags \
             may appear in shape_assignments",
        );
    }
}

// The primary six — object / array / string / number / bool /
// null — are the W3.1 hard-gate rules, all W3-classified regardless
// of W4 activation (Wrap / HRegex etc. don't claim them first).
#[test]
fn json_fixture_structural_rules_all_classified() {
    let (ir, rules) = build_json_ir();
    for rid in [
        rules.object,
        rules.array,
        rules.string,
        rules.number,
        rules.bool_rule,
        rules.null,
    ] {
        let tag = ir.shape_assignments.get(rid);
        assert!(
            tag.is_w3_classified(),
            "JSON rule id {rid} classified as {tag:?}; W3 hard gate \
             requires a W3-active tag",
        );
    }
}

/// AW-V.W4-fix — `value` classifies as Wrap; `pair` classifies as
/// Flat (the W4-fix Flat detector admits Ref-headed typed Seqs).
#[test]
fn json_fixture_value_is_wrap_pair_is_flat() {
    let (ir, rules) = build_json_ir();
    let _ = matches!(&ir.rules[rules.pair as usize].body, IrNode::Seq(_));
    let _ = matches!(&ir.rules[rules.value as usize].body, IrNode::Alt(..));
    assert_eq!(
        ir.shape_assignments.get(rules.pair),
        ShapeTag::Flat,
        "W4-fix: `pair`'s Ref-headed typed Seq classifies as Flat",
    );
    assert_eq!(
        ir.shape_assignments.get(rules.value),
        ShapeTag::Wrap,
        "`value` is W4's Wrap shape (Alt of Refs)",
    );
}

// Touch bare aliases exported from fixtures so the compiler keeps
// them in the test binary's symbol table — keeps the module tidy
// without `#[allow]` masks.
#[allow(dead_code)]
fn _fixtures_type_touch() {
    let _: RuleId = 0;
    let _ = IrNode::Epsilon;
}

