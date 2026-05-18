use std::collections::BTreeSet;
use std::path::Path;
use std::process::Command;

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
        "crates/runtime/src/lib.rs",
        "generic_surface",
        "read_only",
        "runtime_api",
    ),
    entry(
        "crates/runtime/src/tape/mod.rs",
        "generic_surface",
        "read_only",
        "runtime_tape",
    ),
    entry(
        "crates/ir/src/lib.rs",
        "generic_surface",
        "read_only",
        "bir_backend_shape",
    ),
    entry(
        "crates/passes/src/lib.rs",
        "generic_surface",
        "read_only",
        "passes",
    ),
    entry(
        "crates/codegen/src/lib.rs",
        "generic_surface",
        "read_only",
        "codegen_api",
    ),
    entry(
        "crates/codegen/src/lower/mod.rs",
        "generic_surface",
        "read_only",
        "codegen_lowering",
    ),
    entry(
        "crates/bbnf-simd/src/lib.rs",
        "generic_surface",
        "read_only",
        "simd_scanner",
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
    validate_entries(ALLOWLIST, root, true)?;
    validate_git_freeze(root)?;
    validate_backend_shape_surface(root)?;
    Ok(())
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

const FROZEN_ROOTS: &[&str] = &[
    "grammars",
    "test_data",
    "crates/test-fixtures",
    "crates/runtime/src",
    "crates/ir/src",
    "crates/passes/src",
    "crates/codegen/src",
    "crates/grammar/src",
    "crates/bbnf/src",
    "crates/bbnf-simd/src",
    "crates/bbnf-simd/build.rs",
    "crates/bbnf-simd/ext",
    "crates/parse-that-regex/src",
    "crates/bbnf-bench/src/direct_struct.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/track2",
    "crates/bbnf-bench/src/parity.rs",
    "crates/bbnf-bench/src/scan.rs",
    "crates/bbnf-bench/src/materialization.rs",
    "xtask/src/real_typed_schema.rs",
];

fn validate_git_freeze(root: &Path) -> Result<(), String> {
    let frozen_status = git_output(root, &git_path_args("status", "--porcelain", FROZEN_ROOTS))?;
    validate_frozen_status_output(&frozen_status)?;
    git_quiet(root, &git_path_args("diff", "--quiet", FROZEN_ROOTS))?;
    if git_quiet(root, &["rev-parse", "--verify", "HEAD^"]).is_ok() {
        git_quiet(root, &git_diff_from_parent_args())?;
    }
    Ok(())
}

fn git_path_args(
    command: &'static str,
    mode: &'static str,
    paths: &[&'static str],
) -> Vec<&'static str> {
    let mut args = vec![command, mode, "--"];
    args.extend_from_slice(paths);
    args
}

fn git_diff_from_parent_args() -> Vec<&'static str> {
    let mut args = vec!["diff", "--quiet", "HEAD^", "--"];
    args.extend_from_slice(FROZEN_ROOTS);
    args
}

fn git_output(root: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .current_dir(root)
        .args(args)
        .output()
        .map_err(|error| format!("failed to run git {}: {error}", args.join(" ")))?;
    if !output.status.success() {
        return Err(format!("git {} failed", args.join(" ")));
    }
    String::from_utf8(output.stdout)
        .map(|text| text.trim().to_string())
        .map_err(|error| format!("git output was not UTF-8: {error}"))
}

fn git_quiet(root: &Path, args: &[&str]) -> Result<(), String> {
    let output = Command::new("git")
        .current_dir(root)
        .args(args)
        .output()
        .map_err(|error| format!("failed to run git {}: {error}", args.join(" ")))?;
    if output.status.success() {
        Ok(())
    } else {
        Err(format!(
            "Lock 14 frozen diff failed: git {}",
            args.join(" ")
        ))
    }
}

fn validate_frozen_status_output(output: &str) -> Result<(), String> {
    if output.trim().is_empty() {
        return Ok(());
    }
    Err(format!("Lock 14 frozen roots are dirty: {output}"))
}

fn validate_backend_shape_surface(root: &Path) -> Result<(), String> {
    let source = std::fs::read_to_string(root.join("crates/ir/src/lib.rs"))
        .map_err(|error| format!("failed to read BackendShape surface: {error}"))?;
    for variant in [
        "EagerTape",
        "OffsetTape",
        "EventTape",
        "SinkOnly",
        "CollapsedStage",
    ] {
        if !source.contains(&format!("    {variant},")) {
            return Err(format!("BackendShape missing expected variant {variant}"));
        }
    }
    let backend_shape_body = source
        .split("pub enum BackendShape {")
        .nth(1)
        .and_then(|rest| rest.split('}').next())
        .ok_or_else(|| "BackendShape enum not found".to_string())?;
    let variants = backend_shape_body
        .lines()
        .filter(|line| line.trim().ends_with(','))
        .count();
    if variants != 5 {
        return Err(format!("BackendShape variant count changed to {variants}"));
    }
    if source.contains("UnionTape") || source.contains("union_tape") {
        return Err("Lock 14 forbids UnionTape in IR surface".to_string());
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
            | "generic_surface"
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

    #[test]
    fn rejects_frozen_status_output() {
        assert!(validate_frozen_status_output("").is_ok());
        assert!(validate_frozen_status_output(" M crates/runtime/src/tape/mod.rs").is_err());
        assert!(validate_frozen_status_output("?? crates/runtime/src/union_tape.rs").is_err());
        assert!(validate_frozen_status_output(" M crates/grammar/src/lib.rs").is_err());
        assert!(validate_frozen_status_output(" M crates/bbnf-simd/build.rs").is_err());
        assert!(validate_frozen_status_output("?? crates/bbnf-simd/ext/x86/new.S").is_err());
    }

    #[test]
    fn frozen_roots_cover_directive_and_asm_surfaces() {
        for root in [
            "crates/grammar/src",
            "crates/bbnf/src",
            "crates/bbnf-simd/build.rs",
            "crates/bbnf-simd/ext",
            "crates/parse-that-regex/src",
        ] {
            assert!(FROZEN_ROOTS.contains(&root), "{root} is not frozen");
        }
        let status_args = git_path_args("status", "--porcelain", FROZEN_ROOTS).join(" ");
        assert!(status_args.contains("crates/grammar/src"));
        assert!(status_args.contains("crates/bbnf-simd/build.rs"));
        assert!(status_args.contains("crates/bbnf-simd/ext"));
    }

    #[test]
    fn rejects_backend_shape_variant_drift() {
        let source = r#"
pub enum BackendShape {
    EagerTape,
    OffsetTape,
    EventTape,
    SinkOnly,
    CollapsedStage,
    UnionTape,
}
"#;
        let variants = source
            .split("pub enum BackendShape {")
            .nth(1)
            .and_then(|rest| rest.split('}').next())
            .unwrap()
            .lines()
            .filter(|line| line.trim().ends_with(','))
            .count();
        assert_ne!(variants, 5);
        assert!(source.contains("UnionTape"));
    }
}
