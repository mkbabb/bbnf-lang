//! AX.W0a.2.f / AZ-II.cutover.O4 — one-shot shape golden regen helper
//! plus StructDirect generated-residue guard.
//!
//! Rewrites the shape_dispatch_emission golden files from the live
//! emitter output. Exists solely so the inline-attr downgrade + array
//! structural rewrite can propagate into the committed goldens without
//! hand-transcription.

use std::fs;

use bbnf::backend::rust::emitter::EmitStrategy;
use bbnf::backend::rust::emitter::shapes::{array, keyword, number, object, scalar, string};
use bbnf_ir::registry::SubstrateBinding;

#[path = "shape_dispatch_emission/fixtures.rs"]
mod fixtures;

use fixtures::*;

fn format_tokens(ts: &proc_macro2::TokenStream) -> String {
    let file: syn::File =
        syn::parse2(ts.clone()).expect("emitter output must parse as a syn::File");
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

fn generated_path(name: &str) -> std::path::PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("src")
        .join("grammar")
        .join("generated")
        .join(name)
}

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

#[test]
fn struct_direct_generated_files_have_no_tape_view_residue() {
    const GENERATED_GRAMMARS: &[&str] = &[
        "bbnf.rs",
        "bnf.rs",
        "css_l4.rs",
        "css_pretty.rs",
        "csv.rs",
        "ebnf.rs",
        "google_sheets.rs",
        "json.rs",
        "math.rs",
    ];
    const RESIDUE_PATTERNS: &[&str] = &[
        "TapeCursor",
        "NodeView",
        "ValueRoot",
        "materialize_projection_",
        "PROJECTION_MATERIALIZERS",
        "PROJECTION_CONSUMERS",
    ];

    let mut hits = Vec::new();
    for grammar in GENERATED_GRAMMARS {
        let path = generated_path(grammar);
        let text = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("{}: read failed: {err}", path.display()));
        for (line_idx, line) in text.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            for pattern in RESIDUE_PATTERNS {
                if line.contains(pattern) {
                    hits.push(format!(
                        "{}:{}: `{}` in {}",
                        path.display(),
                        line_idx + 1,
                        pattern,
                        trimmed,
                    ));
                    break;
                }
            }
        }
    }

    if !hits.is_empty() {
        let shown = hits.iter().take(20).cloned().collect::<Vec<_>>().join("\n");
        panic!(
            "StructDirect generated files still carry tape-backed view residue \
             ({} hits; first {} shown):\n{}",
            hits.len(),
            hits.len().min(20),
            shown,
        );
    }
}

#[test]
#[ignore = "regen-only — run with `--ignored` to refresh shape goldens"]
fn regen_shape_goldens() {
    let (ir, rules) = build_json_ir();
    let strategy = json_strategy();

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
    let keyword_ts = keyword::emit_parse_keyword(
        "JsonFixture",
        &ir.rules[rules.bool_rule as usize],
        &ir,
        &strategy,
    );
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
