use std::collections::BTreeSet;
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AllowlistEntry {
    pub path: &'static str,
    pub class: &'static str,
    pub w0_mutability: &'static str,
    pub behavior_surface: &'static str,
}

pub const ALLOWLIST: &[AllowlistEntry] = &[
    entry(
        "grammars/json.bbnf",
        "grammar_input",
        "read_only",
        "grammar",
    ),
    entry(
        "crates/test-fixtures/corpus/json/manifest.toml",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "crates/test-fixtures/corpus/json/twitter.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "crates/test-fixtures/corpus/json/citm_catalog.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "crates/test-fixtures/corpus/json/canada.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/apache_builds.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/github_events.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/update-center.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/mesh.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/random.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/gsoc-2018.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/marine_ik.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/instruments.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/numbers.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/unicode_mixed.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/unicode_escapes.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/unicode_basic.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/distinct_values.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "test_data/y_string_unicode.json",
        "fixture_input",
        "read_only",
        "fixture",
    ),
    entry(
        "crates/runtime/src/grammars/json/generated.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/host.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/mod.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/parser.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/scan.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/sink.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/value.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/view.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/json/visitor.rs",
        "generated_json_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/bbnf-bench/src/generated_real_typed.rs",
        "generated_typed_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/codegen/src/json_templates/generated.rs",
        "per_grammar_template",
        "read_only",
        "template",
    ),
    entry(
        "crates/codegen/src/json_templates/parser.rs",
        "per_grammar_template",
        "read_only",
        "template",
    ),
    entry(
        "crates/codegen/src/json_templates/value.rs",
        "per_grammar_template",
        "read_only",
        "template",
    ),
    entry(
        "crates/codegen/src/json_templates/view.rs",
        "per_grammar_template",
        "read_only",
        "template",
    ),
    entry(
        "crates/codegen/src/json_templates/visitor.rs",
        "per_grammar_template",
        "read_only",
        "template",
    ),
    entry(
        "crates/test-fixtures/src/lib.rs",
        "test_fixture",
        "read_only",
        "fixture",
    ),
    entry(
        "crates/bbnf-bench/src/metadata.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/src/report.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/src/gate.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/src/bin/gate.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/src/lock14_baseline.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/benches/json_parity.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "crates/bbnf-bench/benches/simd_scan.rs",
        "bench_gate_schema",
        "telemetry_only",
        "bench_gate",
    ),
    entry(
        "xtask/src/real_typed_schema.rs",
        "host_api_schema_fact",
        "read_only",
        "host_api",
    ),
    entry(
        "crates/bbnf-bench/src/real_typed_struct.rs",
        "host_api_schema_fact",
        "read_only",
        "host_api",
    ),
];

const fn entry(
    path: &'static str,
    class: &'static str,
    w0_mutability: &'static str,
    behavior_surface: &'static str,
) -> AllowlistEntry {
    AllowlistEntry {
        path,
        class,
        w0_mutability,
        behavior_surface,
    }
}

pub fn validate(root: &Path) -> Result<(), String> {
    validate_entries(ALLOWLIST, root, true)
}

fn validate_entries(
    entries: &[AllowlistEntry],
    root: &Path,
    check_exists: bool,
) -> Result<(), String> {
    let mut seen = BTreeSet::new();
    for item in entries {
        if !seen.insert(item.path) {
            return Err(format!("duplicate Lock 14 allowlist path {}", item.path));
        }
        if !is_allowed_class(item.class) {
            return Err(format!(
                "{} has unsupported Lock 14 class {}",
                item.path, item.class
            ));
        }
        if item.w0_mutability != "read_only" && item.w0_mutability != "telemetry_only" {
            return Err(format!(
                "{} has unsupported W0 mutability {}",
                item.path, item.w0_mutability
            ));
        }
        if item.path.contains("UnionTape") || item.path.contains("directive") {
            return Err(format!("{} names a forbidden W0 surface", item.path));
        }
        if check_exists && !root.join(item.path).exists() {
            return Err(format!("{} is missing from Lock 14 baseline", item.path));
        }
    }
    Ok(())
}

fn is_allowed_class(class: &str) -> bool {
    matches!(
        class,
        "grammar_input"
            | "fixture_input"
            | "generated_json_output"
            | "generated_typed_output"
            | "per_grammar_template"
            | "test_fixture"
            | "bench_gate_schema"
            | "host_api_schema_fact"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn accepts_current_allowlist() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        validate(&root).unwrap();
    }

    #[test]
    fn rejects_unknown_class() {
        let entries = [entry(
            "grammars/json.bbnf",
            "generic_json_helper",
            "read_only",
            "grammar",
        )];
        assert!(validate_entries(&entries, Path::new("."), false).is_err());
    }

    #[test]
    fn rejects_duplicate_path() {
        let entries = [
            entry(
                "grammars/json.bbnf",
                "grammar_input",
                "read_only",
                "grammar",
            ),
            entry(
                "grammars/json.bbnf",
                "grammar_input",
                "read_only",
                "grammar",
            ),
        ];
        assert!(validate_entries(&entries, Path::new("."), false).is_err());
    }

    #[test]
    fn rejects_forbidden_surface_name() {
        let entries = [entry(
            "crates/runtime/src/UnionTape.rs",
            "generated_json_output",
            "read_only",
            "generated",
        )];
        assert!(validate_entries(&entries, Path::new("."), false).is_err());
    }
}
