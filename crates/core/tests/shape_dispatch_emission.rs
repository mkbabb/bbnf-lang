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
//! remain stubs returning `false` from their detectors; the deferral
//! assertions at the bottom document that contract so a partial W4
//! landing can't silently claim coverage the mechanism hasn't
//! produced.

use bbnf::backend::rust::emitter::shapes::{array, number, object, string};
use bbnf_ir::passes::recognizers::shape_dispatch::{shape_dispatch, ShapeTag};
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
fn w3_active_tags_are_only_w3_shapes() {
    // The shape_assignments map should contain no W4 tags — W4
    // detectors are stubbed. Every classified rule carries one of
    // the six W3-active tags.
    let (ir, _) = build_json_ir();
    for (rule_id, tag) in &ir.shape_assignments.per_rule {
        assert!(
            tag.is_w3_classified(),
            "rule {rule_id} classified as {tag:?} — only W3 tags may \
             appear in shape_assignments until W4 lands",
        );
    }
}

// The bodies-are-classified invariant: every rule that can be
// classified by structural inspection alone (no miner output
// dependency) should resolve to a W3 tag for the JSON fixture.
#[test]
fn json_fixture_structural_rules_all_classified() {
    let (ir, rules) = build_json_ir();
    // The primary six — object / array / string / number / bool /
    // null — are the W3.1 hard-gate rules.
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

/// The `pair` and `value` rules defer to the walker in W3 — `pair`
/// is W4's Flat shape (typed Seq), `value` is W4's Wrap shape
/// (transparent Alt-of-Refs dispatcher). Both must carry
/// [`ShapeTag::None`] until the W4 detectors fire.
#[test]
fn json_fixture_pair_and_value_defer_to_walker() {
    let (ir, rules) = build_json_ir();
    let _ = matches!(&ir.rules[rules.pair as usize].body, IrNode::Seq(_));
    let _ = matches!(&ir.rules[rules.value as usize].body, IrNode::Alt(..));
    assert_eq!(
        ir.shape_assignments.get(rules.pair),
        ShapeTag::None,
        "`pair` is W4's Flat shape; W3 must route it to the walker",
    );
    assert_eq!(
        ir.shape_assignments.get(rules.value),
        ShapeTag::None,
        "`value` is W4's Wrap shape; W3 must route it to the walker",
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
