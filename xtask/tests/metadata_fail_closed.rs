//! AZ-IV.W0.5 — manifest metadata fail-closed gate.
//!
//! Exercises [`xtask::regen::validate_grammar_features`] against a
//! synthetic `[workspace.metadata.bbnf.grammars]` row that names a
//! feature outside [`xtask::regen::KNOWN_FEATURES`]. The pre-AZ-IV
//! behaviour silently `eprintln!`d and continued, letting typos like
//! `serializ` or `pretify` drift into shipped manifests; the new
//! contract returns a structured error citing the offending grammar
//! ident, the offending feature key, and the accepted feature set.
//!
//! See `docs/tranches/AZ-IV/waves/W0.md` §AZ-IV.W0.5 for scope and
//! the staged-trigger sibling check.
//!
//! The test deserialises a synthetic TOML fragment (matching the
//! exact shape of the real workspace manifest) into a
//! `Vec<GrammarEntry>` so the validator runs against the same input
//! type the manifest reader produces.

use xtask::regen::{KNOWN_FEATURES, validate_grammar_features};

#[derive(serde::Deserialize)]
struct WorkspaceMeta {
    workspace: WorkspaceTable,
}

#[derive(serde::Deserialize)]
struct WorkspaceTable {
    metadata: BbnfMeta,
}

#[derive(serde::Deserialize)]
struct BbnfMeta {
    bbnf: GrammarsTable,
}

#[derive(serde::Deserialize)]
struct GrammarsTable {
    grammars: Vec<xtask::regen::GrammarEntry>,
}

fn parse_grammars(toml_src: &str) -> Vec<xtask::regen::GrammarEntry> {
    let meta: WorkspaceMeta = toml::from_str(toml_src).expect("synthetic manifest must parse");
    meta.workspace.metadata.bbnf.grammars
}

#[test]
fn unknown_feature_is_rejected_with_structured_error() {
    let synthetic = r#"
[workspace]
[workspace.metadata]
[workspace.metadata.bbnf]
grammars = [
    { ident = "json", path = "grammar/json/json.bbnf", features = ["pretify"] },
]
"#;
    let grammars = parse_grammars(synthetic);
    let err = validate_grammar_features(&grammars)
        .expect_err("unknown feature `pretify` must fail validation");
    let msg = format!("{err:#}");
    assert!(
        msg.contains("`pretify`"),
        "error message must cite the offending feature key, got: {msg}"
    );
    assert!(
        msg.contains("grammar `json`"),
        "error message must cite the offending grammar ident, got: {msg}"
    );
    assert!(
        msg.contains("[workspace.metadata.bbnf.grammars]"),
        "error message must cite the manifest table, got: {msg}"
    );
    for known in KNOWN_FEATURES {
        assert!(
            msg.contains(known),
            "error message must enumerate the accepted feature `{known}`, got: {msg}"
        );
    }
}

#[test]
fn every_known_feature_passes_validation() {
    // Every feature listed in KNOWN_FEATURES must round-trip through
    // the validator without error. Catches accidental drift between
    // the constant and the per-entry match arms in
    // `GrammarEntry::parser_attributes`.
    let features_inline = KNOWN_FEATURES
        .iter()
        .map(|f| format!("\"{f}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let synthetic = format!(
        r#"
[workspace]
[workspace.metadata]
[workspace.metadata.bbnf]
grammars = [
    {{ ident = "json", path = "grammar/json/json.bbnf", features = [{features_inline}] }},
]
"#
    );
    let grammars = parse_grammars(&synthetic);
    validate_grammar_features(&grammars).expect("every KNOWN_FEATURES entry must pass validation");
}

#[test]
fn empty_features_passes_validation() {
    // A grammar with no `features = [...]` is the no-capability
    // baseline (the manifest's `#[serde(default)]`); validation must
    // accept it.
    let synthetic = r#"
[workspace]
[workspace.metadata]
[workspace.metadata.bbnf]
grammars = [
    { ident = "csv", path = "grammar/misc/csv.bbnf" },
]
"#;
    let grammars = parse_grammars(synthetic);
    validate_grammar_features(&grammars).expect("empty features must pass validation");
}
