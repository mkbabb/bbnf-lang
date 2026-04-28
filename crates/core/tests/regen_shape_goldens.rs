//! AX.W0a.2.f — one-shot golden regen helper.
//!
//! Rewrites the shape_dispatch_emission golden files from the live
//! emitter output. Exists solely so the inline-attr downgrade + array
//! structural rewrite can propagate into the committed goldens without
//! hand-transcription. After the goldens are regenerated and
//! `shape_dispatch_emission` tests pass, this helper can be retained
//! as a regen utility for subsequent sub-waves.

use bbnf::backend::rust::emitter::shapes::{array, keyword, number, object, scalar, string};
use bbnf::backend::rust::emitter::EmitStrategy;

#[path = "shape_dispatch_emission/fixtures.rs"]
mod fixtures;

use fixtures::*;

fn format_tokens(ts: &proc_macro2::TokenStream) -> String {
    let file: syn::File = syn::parse2(ts.clone())
        .expect("emitter output must parse as a syn::File");
    prettyplease::unparse(&file)
}

fn golden_path(name: &str) -> std::path::PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("tests")
        .join("fixtures")
        .join("shape_dispatch_emission")
        .join(name)
}

#[test]
#[ignore = "regen-only — run with `--ignored` to refresh goldens"]
fn regen_shape_goldens() {
    let (ir, rules) = build_json_ir();
    // AZ-I.W2.RB — pin TapeDirect for golden regen (the goldens
    // capture the legacy tape body; struct-direct goldens are a
    // future-tranche concern).
    let strategy = EmitStrategy::TapeDirect;

    // AZ-I.W2.RC — per-shape Number / String / Scalar emitters now
    // dual-emit via `&EmitStrategy`. The regen helper exercises the
    // legacy TapeDirect path so the goldens reflect the same byte
    // surface they captured before per-shape rewire; the StructDirect
    // path's emission is captured under
    // `tests/struct_direct_snapshots/`.
    let strategy = bbnf::backend::rust::emitter::EmitStrategy::TapeDirect;

    let object_ts = object::emit_parse_object(
        "JsonFixture",
        &ir.rules[rules.object as usize],
        &ir,
        &strategy,
    );
    let array_ts = array::emit_parse_array(
        "JsonFixture",
        &ir.rules[rules.array as usize],
        &ir,
        &strategy,
    );
    let string_ts = string::emit_parse_string(
        "JsonFixture",
        &ir.rules[rules.string as usize],
        &ir,
        &strategy,
    );
    let number_ts = number::emit_parse_number(
        "JsonFixture",
        &ir.rules[rules.number as usize],
        &ir,
        &strategy,
    );
    let keyword_ts =
        keyword::emit_parse_keyword("JsonFixture", &ir.rules[rules.bool_rule as usize], &ir);
    let scalar_ts = scalar::emit_parse_scalar(
        "JsonFixture",
        &ir.rules[rules.comma as usize],
        &ir,
        &strategy,
    );

    for (name, ts) in [
        ("object.rs.expected", &object_ts),
        ("array.rs.expected", &array_ts),
        ("string.rs.expected", &string_ts),
        ("number.rs.expected", &number_ts),
        ("keyword.rs.expected", &keyword_ts),
        ("scalar.rs.expected", &scalar_ts),
    ] {
        let text = format_tokens(ts);
        let path = golden_path(name);
        std::fs::write(&path, text).expect("write golden");
        println!("wrote {}", path.display());
    }
}
