//! StructDirect per-shape emission guard.
//!
//! The old file-backed snapshots were useful during the initial
//! emitter split, but they drifted into stale runtime-surface text.
//! This test now derives every checked shape from the live JSON
//! fixture and asserts the emitted Rust is parsable, builder-routed,
//! and free of retired column-runtime symbols.

use bbnf::backend::rust::emitter::EmitStrategy;
use bbnf::backend::rust::emitter::shapes::{
    alt_dispatch, array, flat, keyword, number, object, scalar, string, wrap,
};
use bbnf_ir::registry::SubstrateBinding;

#[path = "shape_dispatch_emission/fixtures.rs"]
mod fixtures;

use fixtures::*;

fn json_strategy() -> EmitStrategy {
    EmitStrategy::StructDirect {
        rust: SubstrateBinding {
            builder_path: "crate::runtime::json::JsonStructBuilder",
            document_path: "crate::runtime::json::JsonDocument",
        },
        ts: None,
        wasm: None,
    }
}

fn format_tokens(ts: &proc_macro2::TokenStream) -> String {
    let file: syn::File =
        syn::parse2(ts.clone()).expect("emitter output must parse as a syn::File");
    prettyplease::unparse(&file)
}

#[track_caller]
fn assert_struct_direct_output(name: &str, ts: proc_macro2::TokenStream) {
    let text = format_tokens(&ts);
    assert!(
        text.contains("builder"),
        "{name} StructDirect output must route through the builder surface",
    );
    for forbidden in [
        "runtime::tape",
        "::tape::",
        "TapeOffset",
        "TapeCursor",
        "TapeRec",
        "DtaStateId",
        "DtaRuleId",
        "tape.push",
        "tape.",
    ] {
        assert!(
            !text.contains(forbidden),
            "{name} StructDirect output still contains retired symbol {forbidden:?}",
        );
    }
}

#[test]
fn object_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.object as usize];
    assert_struct_direct_output(
        "object",
        object::emit_parse_object("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn array_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.array as usize];
    assert_struct_direct_output(
        "array",
        array::emit_parse_array("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn number_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.number as usize];
    assert_struct_direct_output(
        "number",
        number::emit_parse_number("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn string_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.string as usize];
    assert_struct_direct_output(
        "string",
        string::emit_parse_string("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn scalar_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.comma as usize];
    assert_struct_direct_output(
        "scalar",
        scalar::emit_parse_scalar("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn keyword_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.bool_rule as usize];
    assert_struct_direct_output(
        "keyword",
        keyword::emit_parse_keyword("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn wrap_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.value as usize];
    assert_struct_direct_output(
        "wrap",
        wrap::emit_parse_wrap("JsonFixture", rule, &ir, &strategy),
    );
}

#[test]
fn flat_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.pair as usize];
    assert_struct_direct_output(
        "flat",
        flat::emit_parse_flat(&strategy, "JsonFixture", rule, &ir),
    );
}

#[test]
fn alt_dispatch_output_is_struct_direct() {
    let strategy = json_strategy();
    let (ir, rules) = build_json_ir();
    let rule = &ir.rules[rules.value as usize];
    assert_struct_direct_output(
        "alt_dispatch",
        alt_dispatch::emit_parse_alt_dispatch("JsonFixture", rule, &ir, &strategy),
    );
}
