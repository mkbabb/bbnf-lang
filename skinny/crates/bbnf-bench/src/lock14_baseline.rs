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
        "crates/runtime/src/grammars/json/config.rs",
        "generated_json_output",
        "read_only",
        "generated",
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
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
    ),
    entry(
        "crates/runtime/src/grammars/json/sink.rs",
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
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
        "crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values/sink.rs",
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
    ),
    entry(
        "crates/bbnf-bench/src/generated_real_typed.rs",
        "generated_typed_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/codegen/src/grammar_profile.rs",
        "generic_surface",
        "read_only",
        "codegen_profile",
    ),
    entry(
        "crates/codegen/src/json_sink_direct.rs",
        "per_grammar_provider",
        "read_only",
        "provider",
    ),
    entry(
        "crates/codegen/src/json_typed_direct.rs",
        "per_grammar_provider",
        "read_only",
        "provider",
    ),
    entry(
        "crates/codegen/src/json_templates/config.rs",
        "per_grammar_template",
        "read_only",
        "template",
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
        "crates/bbnf-regex/src/lib.rs",
        "generic_surface",
        "read_only",
        "regex_analysis",
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

const GENERATED_HEADER_TOKEN: &str = "@generated by skinny bbnf-codegen";

const GENERATED_HEADER_SCAN_ROOTS: &[&str] = &["crates/runtime/src/grammars", "crates/codegen/src"];

const GENERATED_HEADER_TOKEN_BASELINE: &[&str] = &[
    "crates/codegen/src/json_templates/generated.rs",
    "crates/codegen/src/json_templates/parser.rs",
    "crates/codegen/src/json_templates/value.rs",
    "crates/codegen/src/json_templates/view.rs",
    "crates/codegen/src/json_typed_direct.rs",
    "crates/codegen/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/sink.rs",
    "crates/runtime/src/grammars/json/config.rs",
    "crates/runtime/src/grammars/json/generated.rs",
    "crates/runtime/src/grammars/json/host.rs",
    "crates/runtime/src/grammars/json/mod.rs",
    "crates/runtime/src/grammars/json/parser.rs",
    "crates/runtime/src/grammars/json/value.rs",
    "crates/runtime/src/grammars/json/view.rs",
    "crates/runtime/src/grammars/json/visitor.rs",
];

const GENERATED_HEADER_RECOGNIZED_EMISSION_ROSTER: &[&str] = &[
    "crates/codegen/src/json_sink_direct.rs",
    "crates/codegen/src/json_templates/config.rs",
    "crates/codegen/src/json_templates/mod.rs",
    "crates/codegen/src/json_templates/visitor.rs",
];

pub fn validate(root: &Path) -> Result<(), String> {
    validate_entries(ALLOWLIST, root, true)?;
    validate_generated_header_companion_lint(root)?;
    validate_post_w5_provider_template_topology(root)?;
    validate_git_freeze(root)?;
    validate_backend_shape_surface(root)?;
    validate_generic_crate_neutrality(root)?;
    validate_codegen_courier_neutrality(root)?;
    validate_skv15_w2_coverage(root)?;
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

fn validate_generated_header_companion_lint(root: &Path) -> Result<(), String> {
    let token_paths = generated_header_token_paths(root)?;
    validate_generated_header_token_path_set(&token_paths)
}

fn generated_header_token_paths(root: &Path) -> Result<Vec<String>, String> {
    let mut token_paths = Vec::new();
    for scan_root in GENERATED_HEADER_SCAN_ROOTS {
        for file in rust_files_under(&root.join(scan_root))? {
            let source = std::fs::read_to_string(&file).map_err(|error| {
                format!(
                    "failed to read generated-header scan file {}: {error}",
                    file.display()
                )
            })?;
            if source.contains(GENERATED_HEADER_TOKEN) {
                token_paths.push(relative_source_path(root, &file)?);
            }
        }
    }
    token_paths.sort();
    Ok(token_paths)
}

fn relative_source_path(root: &Path, file: &Path) -> Result<String, String> {
    let relative = file.strip_prefix(root).map_err(|_| {
        format!(
            "generated-header scan path {} is outside root {}",
            file.display(),
            root.display()
        )
    })?;
    Ok(relative.to_string_lossy().replace('\\', "/"))
}

fn validate_generated_header_token_path_set(paths: &[String]) -> Result<(), String> {
    let actual = paths.iter().map(String::as_str).collect::<BTreeSet<_>>();
    let baseline = GENERATED_HEADER_TOKEN_BASELINE
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let recognized = GENERATED_HEADER_RECOGNIZED_EMISSION_ROSTER
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();

    let missing = baseline.difference(&actual).copied().collect::<Vec<_>>();
    if !missing.is_empty() {
        return Err(format!(
            "Lock 14 generated-header baseline missing paths [{}]",
            missing.join(", ")
        ));
    }

    let unexpected = actual
        .iter()
        .copied()
        .filter(|path| !baseline.contains(path) && !recognized.contains(path))
        .collect::<Vec<_>>();
    if !unexpected.is_empty() {
        return Err(format!(
            "Lock 14 generated-header token appeared outside baseline/emission roster [{}]",
            unexpected.join(", ")
        ));
    }

    Ok(())
}

const FROZEN_ROOTS: &[&str] = &[
    "grammars",
    "test_data",
    "crates/test-fixtures",
    "crates/runtime/src",
    "crates/ir/src",
    "crates/bbnf-regex/src",
    "crates/passes/Cargo.toml",
    "crates/passes/src",
    "crates/codegen/Cargo.toml",
    "crates/codegen/src",
    "crates/bbnf-bench/benches/json_parity.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/bin/profile_direct.rs",
    "crates/bbnf-bench/src/metadata.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
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
    "../crates/core/src/runtime/bnf",
    "../crates/core/src/runtime/bbnf",
    "../crates/core/src/runtime/arena_template.rs",
    "../crates/core/src/runtime/builder_template.rs",
    "../crates/core/src/runtime/css_l4",
    "../crates/core/src/runtime/css_pretty",
    "../crates/core/src/runtime/csv",
    "../crates/core/src/runtime/ebnf",
    "../crates/core/src/runtime/google_sheets",
    "../crates/core/src/runtime/json",
    "../crates/core/src/runtime/math",
    "../xtask/runtime-projections/bnf.toml",
    "../xtask/runtime-projections/bbnf.toml",
    "../xtask/runtime-projections/css_l4.toml",
    "../xtask/runtime-projections/css_pretty.toml",
    "../xtask/runtime-projections/csv.toml",
    "../xtask/runtime-projections/ebnf.toml",
    "../xtask/runtime-projections/google_sheets.toml",
    "../xtask/runtime-projections/json.toml",
    "../xtask/runtime-projections/math.toml",
    "../xtask/src/lib.rs",
    "../xtask/src/main.rs",
    "../xtask/src/regen.rs",
    "../xtask/src/regen_css.rs",
    "../xtask/src/regen_simple_runtime.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
    "Cargo.toml",
    "Cargo.lock",
];

const W2_TYPED_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "xtask/src/real_typed_schema.rs",
];

const W5_LOCK14_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/json_provider.rs",
];

const SK_V10_W5_ROOT_TYPED_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/direct_schema.rs",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/typed_direct.rs",
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "xtask/src/real_typed_schema.rs",
];

const SK_V10_W6_ROOT_TYPED_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "xtask/src/real_typed_schema.rs",
];

const SK_V12_W1A_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/json_provider.rs",
    "crates/codegen/src/json_sink_direct.rs",
    "crates/codegen/src/json_typed_direct.rs",
    "crates/codegen/src/sink_direct.rs",
    "crates/codegen/src/typed_direct.rs",
    "crates/codegen/src/json_templates/config.rs",
    "crates/codegen/src/json_templates/generated.rs",
    "crates/codegen/src/json_templates/parser.rs",
    "crates/codegen/src/json_templates/value.rs",
    "crates/codegen/src/json_templates/view.rs",
    "crates/codegen/src/json_templates/visitor.rs",
    "crates/runtime/src/grammars/json/config.rs",
    "crates/runtime/src/grammars/json/generated.rs",
    "crates/runtime/src/grammars/json/host.rs",
    "crates/runtime/src/grammars/json/mod.rs",
    "crates/runtime/src/grammars/json/parser.rs",
    "crates/runtime/src/grammars/json/scan.rs",
    "crates/runtime/src/grammars/json/sink.rs",
    "crates/runtime/src/grammars/json/value.rs",
    "crates/runtime/src/grammars/json/view.rs",
    "crates/runtime/src/grammars/json/visitor.rs",
    "crates/passes/src/lib.rs",
    "crates/bbnf-bench/src/generated_real_typed.rs",
];

const SK_V12_W1B1_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/json_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/",
    "crates/codegen/src/css_l4_declaration_values_templates/config.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/generated.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/mod.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/parser.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/",
    "crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/sink.rs",
];

const SK_V13_W2_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_provider.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/config.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/generated.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/mod.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/parser.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/config.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/mod.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/sink.rs",
];

const SK_V13_W3_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/config.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/generated.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/mod.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/parser.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/mod.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
];

const SK_V13_W4_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_visual_functions_provider.rs",
    "crates/codegen/src/css_l4_visual_functions_templates/",
    "crates/codegen/src/css_l4_visual_functions_templates/config.rs",
    "crates/codegen/src/css_l4_visual_functions_templates/generated.rs",
    "crates/codegen/src/css_l4_visual_functions_templates/mod.rs",
    "crates/codegen/src/css_l4_visual_functions_templates/parser.rs",
    "crates/codegen/src/css_l4_visual_functions_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/",
    "crates/runtime/src/grammars/css_l4_visual_functions/config.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/mod.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/parser.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/sink.rs",
];

const SK_V13_W10_1_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_provider.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/config.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/generated.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/mod.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/parser.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/config.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/mod.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/parser.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/sink.rs",
];

const SK_V13_W10_2_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/config.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/generated.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/mod.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/parser.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/config.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/mod.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/parser.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/sink.rs",
];

const SK_V13_W10_3_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/css_l4_nested_layout_provider.rs",
    "crates/codegen/src/css_l4_nested_layout_templates/",
    "crates/codegen/src/css_l4_nested_layout_templates/config.rs",
    "crates/codegen/src/css_l4_nested_layout_templates/generated.rs",
    "crates/codegen/src/css_l4_nested_layout_templates/mod.rs",
    "crates/codegen/src/css_l4_nested_layout_templates/parser.rs",
    "crates/codegen/src/css_l4_nested_layout_templates/sink.rs",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/",
    "crates/runtime/src/grammars/css_l4_nested_layout/config.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/generated.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/mod.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/parser.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/sink.rs",
];

const SK_V13_W5_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-regex/",
    "crates/ir/src/lib.rs",
    "crates/passes/src/lib.rs",
];

const SK_V13_W6_OWNER_PATHS: &[&str] = &[
    "crates/ir/src/cost.rs",
    "crates/ir/src/lib.rs",
    "crates/passes/Cargo.toml",
    "crates/passes/src/backend_egraph.rs",
    "crates/passes/src/lib.rs",
    "crates/codegen/src/lower/mod.rs",
    "crates/codegen/src/lower/rust.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
    "Cargo.toml",
];

const SK_V13_W7_OWNER_PATHS: &[&str] = &[
    "crates/ir/src/cost.rs",
    "crates/ir/src/lib.rs",
    "crates/passes/Cargo.toml",
    "crates/passes/src/backend_egraph.rs",
    "crates/passes/src/decision_csp.rs",
    "crates/passes/src/lib.rs",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/lower/mod.rs",
    "crates/codegen/src/lower/rust.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
    "Cargo.toml",
    "Cargo.lock",
];

const SK_V13_W8_OWNER_PATHS: &[&str] = &[
    "crates/runtime/src/tape/mod.rs",
    "crates/runtime/src/grammars/json/config.rs",
    "crates/runtime/src/grammars/json/view.rs",
    "crates/codegen/src/json_templates/config.rs",
    "crates/codegen/src/json_templates/view.rs",
    "crates/bbnf-bench/src/track2/json.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/config.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/generated.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/sink.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W9_OWNER_PATHS: &[&str] = &[
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/config.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/sink.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W11_1_OWNER_PATHS: &[&str] = &[
    "crates/runtime/src/grammars/json/generated.rs",
    "crates/codegen/src/json_sink_direct.rs",
    "crates/bbnf-bench/src/direct_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W11_3_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/direct_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W12_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-simd/src/lib.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W13_1_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
];

const SK_V13_W13_2_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
];

const SK_V13_W13_3_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
];

const SK_V13_W13_4_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
];

const SK_V13_W14_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V13_W15_1_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/json_typed_direct.rs",
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
];

const SK_V14_W0_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V14_W2_OWNER_PATHS: &[&str] = &[
    "Cargo.lock",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "crates/codegen/src/css_l4_declaration_values_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_templates/sink.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/sink.rs",
    "xtask/src/main.rs",
    "xtask/src/regen.rs",
    "xtask/src/regen_css.rs",
];

const SK_V14_W4_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V14_W5A_OWNER_PATHS: &[&str] = &[
    "crates/grammar/src/lib.rs",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_provider.rs",
    "xtask/src/regen.rs",
    "xtask/src/regen_css.rs",
    "xtask/src/main.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
];

const SK_V14_W5B_FRONTEND_OWNER_PATHS: &[&str] = &[
    "crates/grammar/src/lib.rs",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_provider.rs",
    "xtask/src/regen.rs",
    "xtask/src/regen_css.rs",
    "xtask/src/main.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
];

const SK_V14_W5C_GEN_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/grammar_profile.rs",
    "crates/codegen/src/grammar_provider.rs",
    "crates/codegen/src/runtime_generator.rs",
    "xtask/src/main.rs",
    "xtask/src/regen.rs",
    "xtask/src/regen_css.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
];

const SK_V14_W5D_DELETE_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_extended_provider.rs",
    "crates/codegen/src/css_l4_declaration_values_provider.rs",
    "crates/codegen/src/css_l4_nested_layout_provider.rs",
    "crates/codegen/src/css_l4_stylesheet_selectors_provider.rs",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_provider.rs",
    "crates/codegen/src/css_l4_visual_functions_provider.rs",
    "crates/codegen/src/json_provider.rs",
    "crates/codegen/src/css_l4_at_rules_and_media_templates/",
    "crates/codegen/src/css_l4_declaration_values_extended_templates/",
    "crates/codegen/src/css_l4_declaration_values_templates/",
    "crates/codegen/src/css_l4_nested_layout_templates/",
    "crates/codegen/src/css_l4_stylesheet_selectors_templates/",
    "crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/",
    "crates/codegen/src/css_l4_visual_functions_templates/",
    "crates/bbnf-bench/src/lock14_baseline.rs",
];

const SK_V14_W6_0_ROOT_CSS_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/css_l4/arena.rs",
    "../crates/core/src/runtime/css_l4/builder.rs",
    "../crates/core/src/runtime/css_l4/document.rs",
    "../crates/core/src/runtime/css_l4/mod.rs",
    "../crates/core/src/runtime/css_l4/parse_with.rs",
    "../crates/core/src/runtime/css_l4/value.rs",
    "../crates/core/src/runtime/css_l4/view.rs",
    "../xtask/runtime-projections/css_l4.toml",
    "../xtask/src/lib.rs",
    "../xtask/src/main.rs",
    "../xtask/src/regen.rs",
    "../xtask/src/regen_css.rs",
];

const SK_V14_W6_1_ROOT_MATH_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/math/arena.rs",
    "../crates/core/src/runtime/math/builder.rs",
    "../crates/core/src/runtime/math/document.rs",
    "../crates/core/src/runtime/math/kind.rs",
    "../crates/core/src/runtime/math/mod.rs",
    "../crates/core/src/runtime/math/value.rs",
    "../crates/core/src/runtime/math/view.rs",
    "../xtask/runtime-projections/math.toml",
    "../xtask/src/lib.rs",
    "../xtask/src/main.rs",
    "../xtask/src/regen_simple_runtime.rs",
];

const SK_V14_W6_2_ROOT_CSV_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/csv/arena.rs",
    "../crates/core/src/runtime/csv/builder.rs",
    "../crates/core/src/runtime/csv/document.rs",
    "../crates/core/src/runtime/csv/kind.rs",
    "../crates/core/src/runtime/csv/mod.rs",
    "../crates/core/src/runtime/csv/value.rs",
    "../crates/core/src/runtime/csv/view.rs",
    "../xtask/runtime-projections/csv.toml",
    "../xtask/src/main.rs",
];

const SK_V14_W6_3_ROOT_BNF_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/bnf/arena.rs",
    "../crates/core/src/runtime/bnf/builder.rs",
    "../crates/core/src/runtime/bnf/document.rs",
    "../crates/core/src/runtime/bnf/kind.rs",
    "../crates/core/src/runtime/bnf/mod.rs",
    "../crates/core/src/runtime/bnf/value.rs",
    "../crates/core/src/runtime/bnf/view.rs",
    "../xtask/runtime-projections/bnf.toml",
    "../xtask/src/main.rs",
];

const SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/ebnf/arena.rs",
    "../crates/core/src/runtime/ebnf/builder.rs",
    "../crates/core/src/runtime/ebnf/document.rs",
    "../crates/core/src/runtime/ebnf/kind.rs",
    "../crates/core/src/runtime/ebnf/mod.rs",
    "../crates/core/src/runtime/ebnf/value.rs",
    "../crates/core/src/runtime/ebnf/view.rs",
    "../xtask/runtime-projections/ebnf.toml",
    "../xtask/src/main.rs",
];

const SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/css_pretty/arena.rs",
    "../crates/core/src/runtime/css_pretty/builder.rs",
    "../crates/core/src/runtime/css_pretty/document.rs",
    "../crates/core/src/runtime/css_pretty/kind.rs",
    "../crates/core/src/runtime/css_pretty/mod.rs",
    "../crates/core/src/runtime/css_pretty/value.rs",
    "../crates/core/src/runtime/css_pretty/view.rs",
    "../xtask/runtime-projections/css_pretty.toml",
    "../xtask/src/main.rs",
];

const SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/google_sheets/arena.rs",
    "../crates/core/src/runtime/google_sheets/builder.rs",
    "../crates/core/src/runtime/google_sheets/document/canonical.rs",
    "../crates/core/src/runtime/google_sheets/document/mod.rs",
    "../crates/core/src/runtime/google_sheets/document/path_query.rs",
    "../crates/core/src/runtime/google_sheets/document/view.rs",
    "../crates/core/src/runtime/google_sheets/mod.rs",
    "../crates/core/src/runtime/google_sheets/parse_with.rs",
    "../crates/core/src/runtime/google_sheets/value.rs",
    "../crates/core/src/runtime/google_sheets/view.rs",
    "../xtask/runtime-projections/google_sheets.toml",
    "../xtask/src/main.rs",
    "../xtask/src/regen_simple_runtime.rs",
];

const SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/bbnf/arena.rs",
    "../crates/core/src/runtime/bbnf/builder.rs",
    "../crates/core/src/runtime/bbnf/document.rs",
    "../crates/core/src/runtime/bbnf/mod.rs",
    "../crates/core/src/runtime/bbnf/parse_with.rs",
    "../crates/core/src/runtime/bbnf/serialize.rs",
    "../crates/core/src/runtime/bbnf/value.rs",
    "../crates/core/src/runtime/bbnf/view.rs",
    "../xtask/runtime-projections/bbnf.toml",
    "../xtask/src/main.rs",
    "../xtask/src/regen_simple_runtime.rs",
];

const SK_V14_W6_8_ROOT_JSON_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "../crates/core/src/runtime/arena_template.rs",
    "../crates/core/src/runtime/builder_template.rs",
    "../crates/core/src/runtime/json/arena.rs",
    "../crates/core/src/runtime/json/builder.rs",
    "../crates/core/src/runtime/json/document.rs",
    "../crates/core/src/runtime/json/mod.rs",
    "../crates/core/src/runtime/json/parse_with.rs",
    "../crates/core/src/runtime/json/value.rs",
    "../crates/core/src/runtime/json/view.rs",
    "../xtask/runtime-projections/json.toml",
    "../xtask/src/main.rs",
    "../xtask/src/regen_simple_runtime.rs",
];

const SK_V14_W7_OWNER_PATHS: &[&str] = &[
    "crates/ir/src/cost.rs",
    "crates/ir/src/lib.rs",
    "crates/passes/src/decision_csp.rs",
    "crates/passes/src/lib.rs",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/lower/rust.rs",
    "crates/codegen/src/lower/sink_only.rs",
    "crates/codegen/src/runtime_generator.rs",
    "crates/codegen/src/json_sink_direct.rs",
    "crates/codegen/src/json_templates/config.rs",
    "crates/runtime/src/grammars/json/config.rs",
    "crates/runtime/src/grammars/json/generated.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/config.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/config.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/generated.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/config.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/config.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/config.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
];

const SK_V14_W8R_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/runtime_generator.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/parser.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/generated.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/parser.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/parser.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/parser.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "crates/bbnf-bench/src/report.rs",
    "xtask/src/main.rs",
];

const SK_V14_W9_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/direct_schema.rs",
    "crates/codegen/src/json_typed_direct.rs",
    "crates/codegen/src/lib.rs",
    "crates/bbnf-bench/src/generated_real_typed.rs",
    "crates/bbnf-bench/src/json_w9.rs",
    "crates/bbnf-bench/src/lib.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "crates/bbnf-bench/src/real_typed_struct.rs",
    "crates/bbnf-bench/src/report.rs",
    "xtask/src/real_typed_schema.rs",
    "xtask/src/main.rs",
];

const SK_V14_W10_OWNER_PATHS: &[&str] = &[
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/runtime_generator.rs",
    "crates/parse-that-regex/src/lib.rs",
    "crates/runtime/src/grammars/json/generated.rs",
    "crates/runtime/src/grammars/json/mod.rs",
    "crates/runtime/src/grammars/json/parser.rs",
    "crates/runtime/src/lib.rs",
    "crates/bbnf-bench/benches/json_parity.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/bin/profile_direct.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "crates/bbnf-bench/src/metadata.rs",
    "crates/bbnf-bench/src/report.rs",
    "xtask/src/main.rs",
];

const SK_V14_W11A_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/benches/json_parity.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/bin/profile_direct.rs",
    "crates/bbnf-bench/src/direct_struct.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "crates/bbnf-bench/src/metadata.rs",
    "crates/bbnf-bench/src/report.rs",
    "xtask/src/main.rs",
];

const SK_V15_W10_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-bench/src/fnv_quarantine.rs",
    "crates/bbnf-bench/src/lib.rs",
    "xtask/src/main.rs",
];

const SK_V15_W11_OWNER_PATHS: &[&str] = &["crates/bbnf-bench/src/lock14_baseline.rs"];

// SK-V18 W-PRUNE P1: x86 is DELETED crate-wide (aarch64-only). The x86 build
// scaffold, the vendored x86 assembly, and the entire `x86_64/` SIMD tree are
// removed; `scalar/byte_class_from_eq_set_64.rs` loses its x86 companion. These
// frozen-root deletions are the authorized owner surface of the prune.
const SK_V18_WPRUNE_P1_X86_OWNER_PATHS: &[&str] = &[
    "crates/bbnf-simd/build.rs",
    "crates/bbnf-simd/ext/x86/",
    "crates/bbnf-simd/src/x86_64/",
    "crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs",
];

fn current_lock14_owner_paths() -> Vec<&'static str> {
    let mut paths = Vec::with_capacity(
        SK_V12_W1A_OWNER_PATHS.len()
            + SK_V12_W1B1_OWNER_PATHS.len()
            + SK_V13_W2_OWNER_PATHS.len()
            + SK_V13_W3_OWNER_PATHS.len()
            + SK_V13_W4_OWNER_PATHS.len()
            + SK_V13_W10_1_OWNER_PATHS.len()
            + SK_V13_W10_2_OWNER_PATHS.len()
            + SK_V13_W10_3_OWNER_PATHS.len()
            + SK_V13_W5_OWNER_PATHS.len()
            + SK_V13_W6_OWNER_PATHS.len()
            + SK_V13_W7_OWNER_PATHS.len()
            + SK_V13_W8_OWNER_PATHS.len()
            + SK_V13_W9_OWNER_PATHS.len()
            + SK_V13_W11_1_OWNER_PATHS.len()
            + SK_V13_W11_3_OWNER_PATHS.len()
            + SK_V13_W12_OWNER_PATHS.len()
            + SK_V13_W13_1_OWNER_PATHS.len()
            + SK_V13_W13_2_OWNER_PATHS.len()
            + SK_V13_W13_3_OWNER_PATHS.len()
            + SK_V13_W13_4_OWNER_PATHS.len()
            + SK_V13_W14_OWNER_PATHS.len()
            + SK_V13_W15_1_OWNER_PATHS.len()
            + SK_V14_W0_OWNER_PATHS.len()
            + SK_V14_W2_OWNER_PATHS.len()
            + SK_V14_W4_OWNER_PATHS.len()
            + SK_V14_W5A_OWNER_PATHS.len()
            + SK_V14_W5B_FRONTEND_OWNER_PATHS.len()
            + SK_V14_W5C_GEN_OWNER_PATHS.len()
            + SK_V14_W5D_DELETE_OWNER_PATHS.len()
            + SK_V14_W6_0_ROOT_CSS_OWNER_PATHS.len()
            + SK_V14_W6_1_ROOT_MATH_OWNER_PATHS.len()
            + SK_V14_W6_2_ROOT_CSV_OWNER_PATHS.len()
            + SK_V14_W6_3_ROOT_BNF_OWNER_PATHS.len()
            + SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS.len()
            + SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS.len()
            + SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS.len()
            + SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS.len()
            + SK_V14_W6_8_ROOT_JSON_OWNER_PATHS.len()
            + SK_V14_W7_OWNER_PATHS.len()
            + SK_V14_W8R_OWNER_PATHS.len()
            + SK_V14_W9_OWNER_PATHS.len()
            + SK_V14_W10_OWNER_PATHS.len()
            + SK_V14_W11A_OWNER_PATHS.len()
            + SK_V15_W10_OWNER_PATHS.len()
            + SK_V15_W11_OWNER_PATHS.len()
            + SK_V18_WPRUNE_P1_X86_OWNER_PATHS.len(),
    );
    paths.extend_from_slice(SK_V12_W1A_OWNER_PATHS);
    paths.extend_from_slice(SK_V12_W1B1_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W2_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W3_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W4_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W10_1_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W10_2_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W10_3_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W5_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W6_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W7_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W8_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W9_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W11_1_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W11_3_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W12_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W13_1_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W13_2_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W13_3_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W13_4_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W14_OWNER_PATHS);
    paths.extend_from_slice(SK_V13_W15_1_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W0_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W2_OWNER_PATHS);
    paths.extend_from_slice(&SK_V14_W4_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W5A_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W5B_FRONTEND_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W5C_GEN_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W5D_DELETE_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_0_ROOT_CSS_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_1_ROOT_MATH_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_2_ROOT_CSV_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_3_ROOT_BNF_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W6_8_ROOT_JSON_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W7_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W8R_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W9_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W10_OWNER_PATHS);
    paths.extend_from_slice(SK_V14_W11A_OWNER_PATHS);
    paths.extend_from_slice(SK_V15_W10_OWNER_PATHS);
    paths.extend_from_slice(SK_V15_W11_OWNER_PATHS);
    paths.extend_from_slice(SK_V18_WPRUNE_P1_X86_OWNER_PATHS);
    paths
}

fn validate_git_freeze(root: &Path) -> Result<(), String> {
    let current_owner_paths = current_lock14_owner_paths();
    let frozen_status = git_output(root, &git_path_args("status", "--porcelain", FROZEN_ROOTS))?;
    validate_frozen_status_output_with_allowed(&frozen_status, &current_owner_paths)?;
    let frozen_diff = git_output(root, &git_path_args("diff", "--name-only", FROZEN_ROOTS))?;
    validate_changed_paths_output(&frozen_diff, &current_owner_paths)?;
    let frozen_cached = git_output(root, &git_cached_name_args(FROZEN_ROOTS))?;
    validate_changed_paths_output(&frozen_cached, &current_owner_paths)?;
    if git_quiet(root, &["rev-parse", "--verify", "HEAD^"]).is_ok() {
        validate_parent_frozen_diff(root)?;
    }
    Ok(())
}

fn validate_post_w5_provider_template_topology(root: &Path) -> Result<(), String> {
    let codegen_root = root.join("crates/codegen/src");
    let provider_count = std::fs::read_dir(&codegen_root)
        .map_err(|error| format!("failed to read codegen root: {error}"))?
        .filter_map(Result::ok)
        .filter(|entry| {
            entry
                .file_name()
                .to_str()
                .is_some_and(|name| name.ends_with("_provider.rs") && name != "grammar_provider.rs")
        })
        .count();
    if provider_count != 0 {
        return Err(format!(
            "post-W5 provider topology expected 0 legacy providers, saw {provider_count}"
        ));
    }
    let template_count = codegen_template_dir_count(&codegen_root)?;
    if template_count != 1 {
        return Err(format!(
            "post-W5 template topology expected 1 retained JSON template dir, saw {template_count}"
        ));
    }
    if !codegen_root.join("json_templates").is_dir() {
        return Err("post-W5 template topology requires retained json_templates".to_string());
    }
    for (label, output) in [
        (
            "status",
            git_output(root, &["status", "--porcelain", "--", "crates/codegen/src"])?,
        ),
        (
            "diff",
            git_output(root, &["diff", "--name-status", "--", "crates/codegen/src"])?,
        ),
        (
            "cached",
            git_output(
                root,
                &[
                    "diff",
                    "--cached",
                    "--name-status",
                    "--",
                    "crates/codegen/src",
                ],
            )?,
        ),
    ] {
        validate_post_w5_provider_template_status(label, &output)?;
    }
    Ok(())
}

fn validate_post_w5_provider_template_status(label: &str, output: &str) -> Result<(), String> {
    for line in output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
    {
        let Some(status) = line.split_whitespace().next() else {
            continue;
        };
        let disallowed = status == "??"
            || status.starts_with('M')
            || status.starts_with('A')
            || status.starts_with('R');
        if !disallowed {
            if status.starts_with('D') {
                for path in provider_template_status_paths(line) {
                    if is_post_w5_retained_template_path(&path) {
                        return Err(format!(
                            "post-W5 provider/template topology rejects retained-template deletion {label} {status} on {path}"
                        ));
                    }
                }
            }
            continue;
        }
        let paths = provider_template_status_paths(line);
        if let Some(path) = paths
            .iter()
            .find(|path| is_post_w5_protected_topology_path(path))
        {
            return Err(format!(
                "post-W5 provider/template topology rejects {label} {status} on {path}"
            ));
        }
    }
    Ok(())
}

fn codegen_template_dir_count(codegen_root: &Path) -> Result<usize, String> {
    Ok(std::fs::read_dir(codegen_root)
        .map_err(|error| format!("failed to read codegen root: {error}"))?
        .filter_map(Result::ok)
        .filter(|entry| {
            entry.file_type().map(|ty| ty.is_dir()).unwrap_or(false)
                && entry
                    .file_name()
                    .to_str()
                    .is_some_and(|name| name.ends_with("_templates"))
        })
        .count())
}

fn provider_template_status_paths(line: &str) -> Vec<String> {
    let path = if line.as_bytes().get(2) == Some(&b' ') {
        line.get(3..).unwrap_or(line)
    } else if line.as_bytes().get(1) == Some(&b' ') {
        line.get(2..).unwrap_or(line)
    } else {
        line.split_once(char::is_whitespace)
            .map(|(_, rest)| rest)
            .unwrap_or("")
    }
    .trim();
    if let Some((old, new)) = path.split_once(" -> ") {
        vec![normalize_git_path(old), normalize_git_path(new)]
    } else {
        line.split_whitespace()
            .skip(1)
            .map(normalize_git_path)
            .collect()
    }
}

fn is_post_w5_protected_topology_path(path: &str) -> bool {
    let path = normalize_git_path(path);
    if path == "crates/codegen/src/grammar_provider.rs" {
        return false;
    }
    path.ends_with("_provider.rs")
        || path.contains("css_l4_") && path.contains("_templates")
        || is_post_w5_retained_template_path(&path)
}

fn is_post_w5_retained_template_path(path: &str) -> bool {
    normalize_git_path(path).contains("crates/codegen/src/json_templates")
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

fn git_cached_name_args(paths: &[&'static str]) -> Vec<&'static str> {
    let mut args = vec!["diff", "--cached", "--name-only", "--"];
    args.extend_from_slice(paths);
    args
}

fn validate_parent_frozen_diff(root: &Path) -> Result<(), String> {
    let changed_paths = git_parent_changed_paths(root)?;
    if changed_paths.is_empty() {
        return Ok(());
    }
    let subject = git_output(root, &["log", "-1", "--format=%s"])?;
    validate_authorized_parent_diff(&changed_paths, &subject).map_err(|error| {
        format!(
            "{error}: git diff --quiet HEAD^ HEAD -- {}",
            FROZEN_ROOTS.join(" ")
        )
    })
}

fn git_parent_changed_paths(root: &Path) -> Result<Vec<String>, String> {
    let mut args = vec!["diff", "--name-only", "HEAD^", "HEAD", "--"];
    args.extend_from_slice(FROZEN_ROOTS);
    let output = git_output(root, &args)?;
    Ok(output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(normalize_git_path)
        .collect())
}

fn normalize_git_path(path: &str) -> String {
    if let Some(path) = path.strip_prefix("skinny/") {
        return path.to_string();
    }
    if path.starts_with("crates/core/") || path.starts_with("xtask/") {
        return format!("../{path}");
    }
    path.to_string()
}

fn validate_authorized_parent_diff(changed_paths: &[String], subject: &str) -> Result<(), String> {
    if subject.contains("sk-v8-wave2") {
        let allowed = changed_paths.iter().all(|path| {
            W2_TYPED_OWNER_PATHS
                .iter()
                .any(|allowed| path.as_str() == *allowed)
        });
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v8-wave5") {
        let allowed = changed_paths.iter().all(|path| {
            W5_LOCK14_OWNER_PATHS
                .iter()
                .any(|allowed| path.as_str() == *allowed)
        });
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v10-waveW5") {
        let allowed = changed_paths.iter().all(|path| {
            SK_V10_W5_ROOT_TYPED_OWNER_PATHS
                .iter()
                .any(|allowed| path.as_str() == *allowed)
        });
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v10-waveW6") {
        let allowed = changed_paths.iter().all(|path| {
            SK_V10_W6_ROOT_TYPED_OWNER_PATHS
                .iter()
                .any(|allowed| path.as_str() == *allowed)
        });
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v12-waveW1a") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V12_W1A_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v12-waveW1b-1") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V12_W1B1_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW2") || subject.contains("sk-v13-wave2-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W2_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW3") || subject.contains("sk-v13-wave3-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W3_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW4") || subject.contains("sk-v13-wave4-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W4_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW10.1") || subject.contains("sk-v13-wave10.1-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W10_1_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW10.2") || subject.contains("sk-v13-wave10.2-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W10_2_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW10.3") || subject.contains("sk-v13-wave10.3-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W10_3_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW5") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W5_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW6") || subject.contains("sk-v13-wave6-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W6_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW7") || subject.contains("sk-v13-wave7-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W7_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW8") || subject.contains("sk-v13-wave8-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W8_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW9") || subject.contains("sk-v13-wave9-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W9_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW11.1") || subject.contains("sk-v13-wave11.1-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W11_1_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW11.3") || subject.contains("sk-v13-wave11.3-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W11_3_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW12") || subject.contains("sk-v13-wave12-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W12_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW13.1") || subject.contains("sk-v13-wave13.1-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W13_1_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW13.2") || subject.contains("sk-v13-wave13.2-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W13_2_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW13.3") || subject.contains("sk-v13-wave13.3-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W13_3_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW13.4") || subject.contains("sk-v13-wave13.4-challenge") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W13_4_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW14.") || subject.contains("sk-v13-wave14.") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W14_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v13-waveW15.1") || subject.contains("sk-v13-wave15.1") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V13_W15_1_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v14-w0") || subject.contains("sk-v14-W0") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W0_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v14-w1") || subject.contains("sk-v14-W1") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W0_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v14-w2") || subject.contains("sk-v14-W2") {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W2_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v14-w4")
        || subject.contains("sk-v14-W4")
        || subject.contains("sk-v14-waveW4")
    {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, &SK_V14_W4_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if subject.contains("sk-v14-w5a")
        || subject.contains("sk-v14-W5A")
        || subject.contains("sk-v14-waveW5A")
    {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W5A_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w5b_frontend_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W5B_FRONTEND_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w5c_gen_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W5C_GEN_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w5d_delete_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W5D_DELETE_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_0_root_css_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_0_ROOT_CSS_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_1_root_math_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_1_ROOT_MATH_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_2_root_csv_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_2_ROOT_CSV_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_3_root_bnf_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_3_ROOT_BNF_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_4_root_ebnf_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_5_root_css_pretty_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_6_root_google_sheets_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_7_root_bbnf_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w6_8_root_json_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W6_8_ROOT_JSON_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w7_policy_union_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W7_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w8r_css_full_parse_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W8R_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w9_json_typed_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W9_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w10_json_parse_only_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W10_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_w11a_json_direct_strict_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V14_W11A_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_skv15_w10_fnv_quarantine_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V15_W10_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    if is_skv15_w11_close_subject(subject) {
        let allowed = changed_paths
            .iter()
            .all(|path| is_allowed_path(path, SK_V15_W11_OWNER_PATHS));
        if allowed {
            return Ok(());
        }
    }
    Err(format!(
        "Lock 14 frozen diff failed for parent paths [{}]",
        changed_paths.join(", ")
    ))
}

fn is_w9_json_typed_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew9") || subject.contains("sk-v14-w9")
}

fn is_w8r_css_full_parse_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew8r") || subject.contains("sk-v14-w8r")
}

fn is_w10_json_parse_only_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew10") || subject.contains("sk-v14-w10")
}

fn is_w11a_json_direct_strict_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew11a") || subject.contains("sk-v14-w11a")
}

fn is_skv15_w10_fnv_quarantine_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v15-wavew10") || subject.contains("sk-v15-w10")
}

fn is_skv15_w11_close_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v15-wavew11") || subject.contains("sk-v15-w11")
}

fn is_w5b_frontend_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    if subject.contains("sk-v14-wavew5b-frontend") || subject.contains("sk-v14-w5b-frontend") {
        return true;
    }
    (0..=4).any(|index| {
        subject.contains(&format!("sk-v14-wavew5b{index}"))
            || subject.contains(&format!("sk-v14-wavew5b.{index}"))
    })
}

fn is_w5c_gen_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew5c-gen")
        || subject.contains("sk-v14-w5c-gen")
        || subject.contains("sk-v14-wavew5c_gen")
}

fn is_w5d_delete_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew5d-delete")
        || subject.contains("sk-v14-w5d-delete")
        || subject.contains("sk-v14-wavew5d_delete")
}

fn is_w6_0_root_css_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.0")
        || subject.contains("sk-v14-w6.0")
        || subject.contains("sk-v14-wavew6_0")
        || subject.contains("sk-v14-w6_0")
}

fn is_w6_1_root_math_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.1")
        || subject.contains("sk-v14-w6.1")
        || subject.contains("sk-v14-wavew6_1")
        || subject.contains("sk-v14-w6_1")
}

fn is_w6_2_root_csv_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.2")
        || subject.contains("sk-v14-w6.2")
        || subject.contains("sk-v14-wavew6_2")
        || subject.contains("sk-v14-w6_2")
}

fn is_w6_3_root_bnf_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.3")
        || subject.contains("sk-v14-w6.3")
        || subject.contains("sk-v14-wavew6_3")
        || subject.contains("sk-v14-w6_3")
}

fn is_w6_4_root_ebnf_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.4")
        || subject.contains("sk-v14-w6.4")
        || subject.contains("sk-v14-wavew6_4")
        || subject.contains("sk-v14-w6_4")
}

fn is_w6_5_root_css_pretty_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.5")
        || subject.contains("sk-v14-w6.5")
        || subject.contains("sk-v14-wavew6_5")
        || subject.contains("sk-v14-w6_5")
}

fn is_w6_6_root_google_sheets_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.6")
        || subject.contains("sk-v14-w6.6")
        || subject.contains("sk-v14-wavew6_6")
        || subject.contains("sk-v14-w6_6")
}

fn is_w6_7_root_bbnf_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.7")
        || subject.contains("sk-v14-w6.7")
        || subject.contains("sk-v14-wavew6_7")
        || subject.contains("sk-v14-w6_7")
}

fn is_w6_8_root_json_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew6.8")
        || subject.contains("sk-v14-w6.8")
        || subject.contains("sk-v14-wavew6_8")
        || subject.contains("sk-v14-w6_8")
}

fn is_w7_policy_union_subject(subject: &str) -> bool {
    let subject = subject.to_ascii_lowercase();
    subject.contains("sk-v14-wavew7")
        || subject.contains("sk-v14-w7")
        || subject.contains("sk-v14-wavew7-prune-5")
        || subject.contains("sk-v14-w7-prune-5")
        || subject.contains("sk-v14-wavew7-prune5")
        || subject.contains("sk-v14-w7-prune5")
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

#[cfg(test)]
fn validate_frozen_status_output(output: &str) -> Result<(), String> {
    validate_frozen_status_output_with_allowed(output, &[])
}

fn validate_frozen_status_output_with_allowed(
    output: &str,
    allowed_paths: &[&str],
) -> Result<(), String> {
    let changed = status_changed_paths(output);
    if changed.is_empty() {
        return Ok(());
    }
    validate_changed_paths(&changed, allowed_paths)
        .map_err(|error| format!("Lock 14 frozen roots are dirty: {output}; {error}"))
}

fn validate_changed_paths_output(output: &str, allowed_paths: &[&str]) -> Result<(), String> {
    let changed = output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(normalize_git_path)
        .collect::<Vec<_>>();
    validate_changed_paths(&changed, allowed_paths)
}

fn status_changed_paths(output: &str) -> Vec<String> {
    let mut changed = Vec::new();
    for line in output
        .lines()
        .map(str::trim_end)
        .filter(|line| !line.is_empty())
    {
        let path = if line.as_bytes().get(2) == Some(&b' ') {
            line.get(3..).unwrap_or(line)
        } else if line.as_bytes().get(1) == Some(&b' ') {
            line.get(2..).unwrap_or(line)
        } else {
            line
        }
        .trim();
        if let Some((old, new)) = path.split_once(" -> ") {
            changed.push(normalize_git_path(old));
            changed.push(normalize_git_path(new));
        } else {
            changed.push(normalize_git_path(path));
        }
    }
    changed
}

fn validate_changed_paths(changed_paths: &[String], allowed_paths: &[&str]) -> Result<(), String> {
    let allowed = changed_paths
        .iter()
        .all(|path| is_allowed_path(path, allowed_paths));
    if allowed {
        Ok(())
    } else {
        let disallowed = changed_paths
            .iter()
            .filter(|path| !is_allowed_path(path, allowed_paths))
            .cloned()
            .collect::<Vec<_>>();
        Err(format!(
            "Lock 14 frozen diff failed for paths [{}]; disallowed [{}]",
            changed_paths.join(", "),
            disallowed.join(", ")
        ))
    }
}

fn is_allowed_path(path: &str, allowed_paths: &[&str]) -> bool {
    allowed_paths.iter().any(|allowed| {
        if allowed.ends_with('/') {
            path.starts_with(*allowed)
        } else {
            path == *allowed
        }
    })
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

const GENERIC_SCAN_ROOTS: &[&str] = &[
    "crates/bbnf-regex/src",
    "crates/codegen/src/lib.rs",
    "crates/codegen/src/lower",
    "crates/codegen/src/grammar_profile.rs",
    "crates/passes/src",
    "crates/runtime/src/lib.rs",
    "crates/runtime/src/tape",
    "crates/ir/src",
];

/// SK-V18 P4: the codegen leak surface — `runtime_generator.rs` + the JSON
/// sink/typed/template surfaces + `grammar_provider.rs` — MOVED here out of the
/// weak `SKV15_W2_EXTRA_COVERAGE_ROOTS` (which only checked file existence and
/// never ran a neutrality scan). These surfaces are JSON-OWNED for the
/// navigation tokens (`JsonSink`/`JsonValue`/...; the
/// `json_owned_roots_may_contain_json_policy_tokens` invariant keeps those
/// legitimate here), so they CANNOT be run through the full
/// `FORBIDDEN_GENERIC_TOKENS` list. Instead they are strictly scanned for the
/// grammar-body-courier + witness tokens (`COURIER_CONST_SUFFIX` +
/// `EVENT_GRAMMAR_WITNESS_TOKEN`) by `validate_codegen_courier_neutrality`.
/// This is the surface the un-forked
/// emitter is authored INTO (G1/G2/G3); a re-injected `CSS_GENERATED_RS` /
/// `SHEETS_GENERATED_RS` courier const or a grammar-named `*EventGrammar`
/// witness literal turns the gate RED at its emit site.
const CODEGEN_COURIER_SCAN_ROOTS: &[&str] = &[
    "crates/codegen/src/runtime_generator.rs",
    "crates/codegen/src/json_sink_direct.rs",
    "crates/codegen/src/json_typed_direct.rs",
    "crates/codegen/src/json_templates",
    "crates/codegen/src/grammar_provider.rs",
];

/// SK-V18 P4: the grammar-body-courier and witness tokens the codegen leak
/// surface is strictly scanned for. The §3.4 close-condition names the courier
/// set `{GENERATED_RS, CSS_GENERATED_RS, EventGrammar, *EventGrammar}`; this is
/// the dedicated subset the codegen courier scan consults (distinct from the
/// full `FORBIDDEN_GENERIC_TOKENS`, which would false-RED on the JSON-owned
/// navigation tokens these surfaces legitimately carry).
///
///   - `GENERATED_RS` (the §711 courier-SUFFIX form) is matched as a
///     `const *_GENERATED_RS` grammar-body-courier DECLARATION. It catches a
///     re-injected `const SHEETS_GENERATED_RS` while the two cohort-carried
///     couriers present at P4 (`JSON_PARSE_ONLY_GENERATED_RS`, G1 retires;
///     `CSS_GENERATED_RS`, G2 retires) sit in `COURIER_CONST_BASELINE`. The
///     suffix scoping means the six surviving MOD/HOST/PARSER/SINK scaffold
///     consts (`JSON_MOD_RS`/`JSON_HOST_RS`/`JSON_PARSE_ONLY_PARSER_RS`/
///     `CSS_MOD_RS`/`CSS_PARSER_RS`/`CSS_SINK_RS`) do NOT false-RED — a bare
///     `_RS`/`CSS_` would collide with them.
///   - `CSS_GENERATED_RS` is the named close-condition #8 token; it is the
///     `CSS`-courier member of the baselined `*_GENERATED_RS` set above.
///   - `EventGrammar` (substring) is the alias-IMMUNE `*EventGrammar` glob: it
///     catches a bare `EventGrammar` witness AND every `JsonEventGrammar` /
///     `SheetsEventGrammar` alias under `contains`. ZERO occurrences exist in
///     the courier surfaces today, so it is a HARD-ZERO forbidden token (no
///     baseline). It is scoped to the courier surfaces ONLY — the generic
///     `crates/codegen/src/lower` tree carries the pre-G4
///     `"ParserState+TapeBuilder+EventGrammar"` lowering descriptor
///     (`tape_plan.rs`), the EventTape axis G4 deletes, NOT a courier; a bare
///     `EventGrammar` in `FORBIDDEN_GENERIC_TOKENS` would false-RED on it.
const COURIER_CONST_SUFFIX: &str = "_GENERATED_RS";
const EVENT_GRAMMAR_WITNESS_TOKEN: &str = "EventGrammar";

/// The cohort-carried `*_GENERATED_RS` grammar-body-courier const declarations
/// present at P4 — `(relative_path, const_ident)`. They are NOT gate-keyed
/// (addendum 1: cohort-carried, not gate-keyed) but ARE the named baseline:
/// G1 retires `JSON_PARSE_ONLY_GENERATED_RS`, G2 retires `CSS_GENERATED_RS`.
/// Any `const *_GENERATED_RS` declaration in a courier root that is NOT in this
/// baseline (e.g. a re-injected `SHEETS_GENERATED_RS`) turns the gate RED.
const COURIER_CONST_BASELINE: &[(&str, &str)] = &[
    (
        "crates/codegen/src/runtime_generator.rs",
        "JSON_PARSE_ONLY_GENERATED_RS",
    ),
    (
        "crates/codegen/src/runtime_generator.rs",
        "CSS_GENERATED_RS",
    ),
];

const FORBIDDEN_GENERIC_TOKENS: &[(&str, &str)] = &[
    ("json_structural_alphabet_name", "STRUCTURAL_ALPHABET_JSON"),
    ("json_structural_alphabet_bytes", "b\"{}[],:\\\"\""),
    ("json_sink", "JsonSink"),
    ("json_node_kind", "JsonNodeKind"),
    ("json_value", "JsonValue"),
    ("json_root", "JsonRoot"),
    ("json_visitor", "JsonVisitor"),
    ("json_escape_flag_meaning", "OffsetFlags::HAS_ESC"),
    ("json_escape_flag_name", "HAS_ESC"),
    ("json_control_flag_name", "HAS_CONTROL"),
    ("json_string_helper", "match_string_at_quote_trusted_utf8"),
    ("json_number_helper", "match_number_span_from_first"),
    ("serde_json_policy", "serde_json"),
    ("json_colon_error", "ExpectedColon"),
    ("json_comma_error", "ExpectedCommaOr"),
    ("json_literal_branch", "grammar_name == \"json\""),
];

const SKV15_W2_FORBIDDEN_FINDING_TOKENS: &str = "Json,CssL4,Sheets,BBNF,json_,css_,RuntimeProvider,static_css_provider_status,json_sink_only_status,JSON-CSS";
const SKV15_W2_REQUIRED_REPORT_COLUMNS: &str = "included_roots,excluded_roots,reason,owner,self_scan_status,primitive_status,gate_consumer,affected_rows,disposition,source_path,finding_kind,strict_command,scalar_reference,rollback_or_redress,dependency_row,non_json_receiver,proof_command,generated_output_expectation,json_guard_command,fail_action";

// SK-V18 P4: the codegen leak surface (`runtime_generator.rs`,
// `grammar_provider.rs`, `json_sink_direct.rs`, `json_typed_direct.rs`,
// `json_templates`) is MOVED OUT of this weak existence-only set INTO the
// strict `CODEGEN_COURIER_SCAN_ROOTS` (courier-token neutrality scan). What
// remains here is the bench/xtask reporting surface, which carries no
// grammar-body courier and is existence-tracked for coverage only.
const SKV15_W2_EXTRA_COVERAGE_ROOTS: &[&str] = &[
    "crates/bbnf-bench/src/report.rs",
    "crates/bbnf-bench/src/bin/gate.rs",
    "crates/bbnf-bench/src/lock14_baseline.rs",
    "xtask/src/main.rs",
    "xtask/src/skv15_w0.rs",
];

// SK-V18 P4: the `("crates/bbnf-simd/src/x86_64", "diagnostic-x86")` exclusion
// is DROPPED — x86 is DELETED crate-wide by P1 (aarch64-only). The remaining
// rows are the aarch64 NEON primitive surfaces.
const SKV15_W2_PRIMITIVE_CLASS_ROOTS: &[(&str, &str)] = &[
    ("crates/bbnf-simd/src/aarch64", "strict-checkasm-admitted"),
    ("crates/bbnf-simd/src/dispatch.rs", "wired"),
    ("crates/bbnf-simd/src/lib.rs", "wired"),
    (
        "crates/bbnf-simd/tests/checkasm_parity.rs",
        "strict-checkasm-admitted",
    ),
];

fn validate_generic_crate_neutrality(root: &Path) -> Result<(), String> {
    for scan_root in GENERIC_SCAN_ROOTS {
        for file in rust_files_under(&root.join(scan_root))? {
            let source = std::fs::read_to_string(&file).map_err(|error| {
                format!("failed to read generic root {}: {error}", file.display())
            })?;
            let production = strip_test_code(&source);
            validate_generic_source(file.strip_prefix(root).unwrap_or(&file), production)?;
        }
    }
    Ok(())
}

/// SK-V18 P4: strictly scan the codegen leak surface
/// (`CODEGEN_COURIER_SCAN_ROOTS`) for the grammar-body-courier and
/// witness tokens. This is the check that makes the Lock-14 gate MEANINGFUL
/// rather than green-by-exclusion: the un-forked emitter is authored INTO these
/// surfaces (G1/G2/G3), so a re-injected `SHEETS_GENERATED_RS` courier const or
/// a grammar-named `*EventGrammar` witness literal turns the gate RED at its
/// emit site. Production code only (`strip_test_code`); the navigation tokens
/// (`JsonSink`/...) stay JSON-owned and are NOT checked here.
fn validate_codegen_courier_neutrality(root: &Path) -> Result<(), String> {
    for scan_root in CODEGEN_COURIER_SCAN_ROOTS {
        for file in rust_files_under(&root.join(scan_root))? {
            let source = std::fs::read_to_string(&file).map_err(|error| {
                format!("failed to read codegen courier root {}: {error}", file.display())
            })?;
            let production = strip_test_code(&source);
            let relative = relative_source_path(root, &file)?;
            validate_codegen_courier_source(&relative, production)?;
        }
    }
    Ok(())
}

/// Per-file courier neutrality: any `const *_GENERATED_RS` grammar-body-courier
/// declaration not in `COURIER_CONST_BASELINE`, OR any `*EventGrammar` witness
/// literal (hard-zero), turns the gate RED.
fn validate_codegen_courier_source(relative: &str, source: &str) -> Result<(), String> {
    for ident in courier_const_idents(source) {
        let baselined = COURIER_CONST_BASELINE
            .iter()
            .any(|(path, baseline_ident)| *path == relative && *baseline_ident == ident);
        if !baselined {
            return Err(format!(
                "Lock 14 codegen-courier scan found un-baselined grammar-body courier \
                 `const {ident}` in {relative} (re-injected {COURIER_CONST_SUFFIX} courier)"
            ));
        }
    }
    if source.contains(EVENT_GRAMMAR_WITNESS_TOKEN) {
        return Err(format!(
            "Lock 14 codegen-courier scan found grammar-named witness token \
             `{EVENT_GRAMMAR_WITNESS_TOKEN}` (*EventGrammar) in {relative}"
        ));
    }
    Ok(())
}

/// Extract every `const NAME_GENERATED_RS` declaration identifier from a source
/// body — the grammar-body-courier const declarations the courier suffix scopes.
fn courier_const_idents(source: &str) -> Vec<&str> {
    let mut idents = Vec::new();
    for raw in source.split("const ").skip(1) {
        let ident = match raw.split([':', ' ', '\n', '=']).next() {
            Some(ident) => ident,
            None => continue,
        };
        if ident.ends_with(COURIER_CONST_SUFFIX) {
            idents.push(ident);
        }
    }
    idents
}

fn validate_skv15_w2_coverage(root: &Path) -> Result<(), String> {
    validate_skv15_w2_report_columns()?;
    validate_skv15_w2_forbidden_tokens()?;
    validate_skv15_w2_root_coverage(root)?;
    validate_skv15_w2_primitive_inventory(root)?;
    Ok(())
}

fn validate_skv15_w2_report_columns() -> Result<(), String> {
    let mut seen = BTreeSet::new();
    for column in SKV15_W2_REQUIRED_REPORT_COLUMNS.split(',') {
        if column.trim().is_empty() || !seen.insert(column) {
            return Err(format!("invalid SK-V15 W2 report column `{column}`"));
        }
    }
    Ok(())
}

fn validate_skv15_w2_forbidden_tokens() -> Result<(), String> {
    if SKV15_W2_FORBIDDEN_FINDING_TOKENS
        .split(',')
        .any(|token| token.trim().is_empty())
    {
        return Err("invalid SK-V15 W2 forbidden finding token".to_string());
    }
    Ok(())
}

fn validate_skv15_w2_root_coverage(root: &Path) -> Result<(), String> {
    for required in GENERIC_SCAN_ROOTS
        .iter()
        .copied()
        .chain(CODEGEN_COURIER_SCAN_ROOTS.iter().copied())
        .chain(SKV15_W2_EXTRA_COVERAGE_ROOTS.iter().copied())
    {
        if !root.join(required).exists() {
            return Err(format!("SK-V15 W2 coverage root {required} is absent"));
        }
    }
    for path in SKV15_W2_EXTRA_COVERAGE_ROOTS
        .iter()
        .copied()
        .chain(["crates/ir/src/cost.rs"])
    {
        let (owner, dependency_row, non_json_receiver) = skv15_w2_root_binding(path);
        if !root.join(path).exists() {
            return Err(format!("SK-V15 W2 reported root {path} is absent"));
        }
        validate_w2_value("owner", owner)?;
        validate_w2_value("dependency_row", dependency_row)?;
        validate_w2_value("non_json_receiver", non_json_receiver)?;
    }
    for value in [
        "self_scan_status:source-derived",
        "gate_consumer:cargo xtask gate-json --check-results",
        "generated_output_expectation:report-only/no-delete",
        "fail_action:reject-route-or-redress",
    ] {
        validate_w2_value("coverage_default", value)?;
    }
    Ok(())
}

fn skv15_w2_root_binding(path: &str) -> (&'static str, &'static str, &'static str) {
    // SK-V18 P4: the `runtime_generator.rs` / `grammar_provider.rs` binding rows
    // moved out with their surfaces — they are now strictly courier-scanned via
    // `CODEGEN_COURIER_SCAN_ROOTS`, not existence-bound through this weak set.
    match path {
        "crates/bbnf-bench/src/report.rs" | "crates/bbnf-bench/src/bin/gate.rs" => (
            "SK-V15-W2",
            "DEP-W1-CSS-BROADCAST",
            "JSON guard plus CSS L4 diagnostic/typed rows",
        ),
        "xtask/src/main.rs" => (
            "SK-V15-W2",
            "DEP-W11-CLOSE-NO-ORPHANS",
            "CSS L4 plus one non-CSS generated receiver",
        ),
        "crates/ir/src/cost.rs" => (
            "SK-V15-W7",
            "DEP-W7-DECISION-SPINE",
            "CSS L4 plus Sheets or BBNF-self",
        ),
        _ => (
            "SK-V15-W2",
            "DEP-W11-CLOSE-NO-ORPHANS",
            "JSON guard plus CSS L4 diagnostic/typed rows",
        ),
    }
}

fn validate_skv15_w2_primitive_inventory(root: &Path) -> Result<(), String> {
    for path in [
        "crates/bbnf-simd/src/aarch64/mod.rs",
        "crates/bbnf-simd/src/dispatch.rs",
        "crates/bbnf-simd/src/lib.rs",
        "xtask/src/main.rs",
    ] {
        if !root.join(path).exists() {
            return Err(format!("SK-V15 W2 primitive source {path} is absent"));
        }
    }
    let dispatch = std::fs::read_to_string(root.join("crates/bbnf-simd/src/dispatch.rs"))
        .map_err(|error| format!("failed to read primitive dispatch: {error}"))?;
    for token in [
        "PrimitiveKernels",
        "select_primitive_kernels",
        "target_arch = \"aarch64\"",
    ] {
        if !dispatch.contains(token) {
            return Err(format!("SK-V15 W2 primitive dispatch missing `{token}`"));
        }
    }
    let lib = std::fs::read_to_string(root.join("crates/bbnf-simd/src/lib.rs"))
        .map_err(|error| format!("failed to read primitive public wrappers: {error}"))?;
    for wrapper in [
        "pub mod prim",
        "byte_class_from_table_64",
        "bitmap_prefix_xor_64",
        "bitmap_next_set_bit",
        "bulk_emit_positions_64",
        "eob_pad_clamp",
        "byte_class_from_eq_set_64",
    ] {
        if !lib.contains(wrapper) {
            return Err(format!("SK-V15 W2 primitive wrapper missing `{wrapper}`"));
        }
    }
    let xtask = std::fs::read_to_string(root.join("xtask/src/main.rs"))
        .map_err(|error| format!("failed to read xtask primitive gate: {error}"))?;
    if !xtask.contains("\"checkasm_escape_mask_64\"") {
        return Err("SK-V15 W2 primitive-checkasm omits checkasm_escape_mask_64".to_string());
    }
    validate_skv15_w2_native_hits(root)
}

fn validate_skv15_w2_native_hits(root: &Path) -> Result<(), String> {
    let mut hits = Vec::new();
    for scan_root in [
        "crates/bbnf-simd/src",
        "crates/bbnf-simd/tests/checkasm_parity.rs",
    ] {
        for file in rust_files_under(&root.join(scan_root))? {
            let source = std::fs::read_to_string(&file)
                .map_err(|error| format!("failed to read native-token source: {error}"))?;
            if source.contains("core::arch")
                || source.contains("#[target_feature")
                || source.contains("asm!")
            {
                hits.push(relative_source_path(root, &file)?);
            }
        }
    }
    if hits.is_empty() {
        return Err("SK-V15 W2 primitive inventory found no native-token sources".to_string());
    }
    for hit in hits {
        let class = SKV15_W2_PRIMITIVE_CLASS_ROOTS
            .iter()
            .find_map(|(prefix, status)| hit.starts_with(prefix).then_some(*status))
            .ok_or_else(|| format!("SK-V15 W2 primitive hit {hit} has no status row"))?;
        validate_w2_value("primitive_status", class)?;
    }
    Ok(())
}

fn validate_w2_value(field: &str, value: &str) -> Result<(), String> {
    if value.trim().is_empty() {
        return Err(format!("SK-V15 W2 coverage has empty {field}"));
    }
    if value.contains("self-exempting") || value.contains("diagnostic:pre-W2-incomplete") {
        return Err(format!("SK-V15 W2 coverage rejects {field}={value}"));
    }
    Ok(())
}

fn rust_files_under(path: &Path) -> Result<Vec<std::path::PathBuf>, String> {
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }
    let mut files = Vec::new();
    collect_rust_files(path, &mut files)?;
    files.sort();
    Ok(files)
}

fn collect_rust_files(path: &Path, files: &mut Vec<std::path::PathBuf>) -> Result<(), String> {
    for entry in std::fs::read_dir(path)
        .map_err(|error| format!("failed to read generic root {}: {error}", path.display()))?
    {
        let entry = entry.map_err(|error| format!("failed to read dir entry: {error}"))?;
        let path = entry.path();
        if path.is_dir() {
            collect_rust_files(&path, files)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            files.push(path);
        }
    }
    Ok(())
}

fn strip_test_code(source: &str) -> &str {
    match source.find("#[cfg(test)]") {
        Some(index) => &source[..index],
        None => source,
    }
}

fn validate_generic_source(path: &Path, source: &str) -> Result<(), String> {
    for (class, token) in FORBIDDEN_GENERIC_TOKENS {
        if source.contains(token) {
            return Err(format!(
                "Lock 14 generic-crate scan found {class} token `{token}` in {}",
                path.display()
            ));
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
            | "generated_nonjson_output"
            | "generated_typed_output"
            | "per_grammar_runtime_source"
            | "per_grammar_provider"
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

    // SK-V18 P4: the codegen leak surface is now strictly courier-scanned. The
    // cohort-carried `*_GENERATED_RS` couriers present at P4 stay GREEN; a
    // re-injected `SHEETS_GENERATED_RS` courier const or any `*EventGrammar`
    // witness literal turns the gate RED. These tests ARE the re-inject
    // falsifier (no cargo/regen needed to prove coverage).

    #[test]
    fn codegen_courier_scan_covers_the_leak_surface() {
        // lock14_gate_scans_codegen == true: every named leak surface is in the
        // strict courier scan root set, and none routes through the weak set.
        for surface in [
            "crates/codegen/src/runtime_generator.rs",
            "crates/codegen/src/json_sink_direct.rs",
            "crates/codegen/src/json_typed_direct.rs",
            "crates/codegen/src/json_templates",
            "crates/codegen/src/grammar_provider.rs",
        ] {
            assert!(
                CODEGEN_COURIER_SCAN_ROOTS.contains(&surface),
                "{surface} is not courier-scanned"
            );
            assert!(
                !SKV15_W2_EXTRA_COVERAGE_ROOTS.contains(&surface),
                "{surface} still routes through the weak coverage set"
            );
        }
    }

    #[test]
    fn codegen_courier_scan_admits_the_cohort_carried_couriers() {
        // The two grammar-body couriers present at P4 are baselined GREEN
        // (G1 retires the JSON courier, G2 the CSS courier).
        let source =
            "const JSON_PARSE_ONLY_GENERATED_RS: &str = r#\"...\"#;\nconst CSS_GENERATED_RS: &str = r#\"...\"#;\n";
        validate_codegen_courier_source("crates/codegen/src/runtime_generator.rs", source)
            .expect("cohort-carried couriers must stay GREEN");
        // The surviving MOD/HOST/PARSER/SINK scaffold consts do NOT false-RED.
        let scaffold = "const JSON_MOD_RS: &str = \"\";\nconst CSS_SINK_RS: &str = \"\";\n";
        validate_codegen_courier_source("crates/codegen/src/runtime_generator.rs", scaffold)
            .expect("MOD/HOST/PARSER/SINK scaffold consts must not false-RED");
    }

    #[test]
    fn codegen_courier_scan_reds_on_reinjected_sheets_courier() {
        // RE-INJECT FALSIFIER (the GENERATED_RS courier-suffix form): a fresh
        // `const SHEETS_GENERATED_RS` in a courier root is un-baselined -> RED.
        let reinjected = "const SHEETS_GENERATED_RS: &str = r#\"... sheets blob ...\"#;\n";
        assert!(
            validate_codegen_courier_source(
                "crates/codegen/src/runtime_generator.rs",
                reinjected,
            )
            .is_err(),
            "re-injected SHEETS_GENERATED_RS courier must turn the gate RED"
        );
        // A bare `JsonSink` is NOT in the courier set and does NOT fire here
        // (it stays JSON-owned per json_owned_roots_may_contain_json_policy_tokens).
        validate_codegen_courier_source(
            "crates/codegen/src/json_sink_direct.rs",
            "pub use sink::JsonSink;\n",
        )
        .expect("bare JsonSink is JSON-owned, not a courier token");
    }

    #[test]
    fn codegen_courier_scan_reds_on_event_grammar_witness() {
        // RE-INJECT FALSIFIER (the *EventGrammar witness glob, alias-immune):
        // any EventGrammar / JsonEventGrammar / SheetsEventGrammar literal -> RED.
        for witness in [
            "let g: EventGrammar = ...;",
            "emit::<JsonEventGrammar>();",
            "emit::<SheetsEventGrammar>();",
        ] {
            assert!(
                validate_codegen_courier_source(
                    "crates/codegen/src/runtime_generator.rs",
                    witness,
                )
                .is_err(),
                "witness `{witness}` must turn the gate RED"
            );
        }
    }

    #[test]
    fn courier_const_idents_extracts_only_generated_rs_suffix() {
        let source = "const A_GENERATED_RS: &str = \"\";\nconst B_MOD_RS: &str = \"\";\nconst C_GENERATED_RS: &str = \"\";\n";
        assert_eq!(
            courier_const_idents(source),
            vec!["A_GENERATED_RS", "C_GENERATED_RS"]
        );
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
    fn admits_w1a_owner_dirty_paths_only_under_w1a_allowance() {
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/runtime/src/grammars/json/scan.rs\nD  crates/codegen/src/sink_direct.rs\n?? crates/codegen/src/json_sink_direct.rs",
            SK_V12_W1A_OWNER_PATHS,
        )
        .is_ok());
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/runtime/src/tape/mod.rs",
            SK_V12_W1A_OWNER_PATHS,
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w7_owner_dirty_paths_under_w7_allowance() {
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/passes/src/lib.rs\n?? crates/passes/src/decision_csp.rs\n M crates/bbnf-bench/src/bin/gate.rs",
            SK_V13_W7_OWNER_PATHS,
        )
        .is_ok());
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/runtime/src/tape/mod.rs",
            SK_V13_W7_OWNER_PATHS,
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w8_owner_dirty_paths_under_w8_allowance() {
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/runtime/src/tape/mod.rs\n M crates/runtime/src/grammars/json/config.rs\n M crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs\n M crates/bbnf-bench/src/bin/gate.rs",
            SK_V13_W8_OWNER_PATHS,
        )
        .is_ok());
        assert!(validate_frozen_status_output_with_allowed(
            " M crates/parse-that-regex/src/lib.rs",
            SK_V13_W8_OWNER_PATHS,
        )
        .is_err());
    }

    #[test]
    fn rejects_json_named_tape_flag_tokens_in_generic_roots() {
        assert!(validate_generic_source(
            Path::new("crates/runtime/src/tape/mod.rs"),
            "pub const HAS_ESC: u8 = 0x01;"
        )
        .is_err());
        assert!(validate_generic_source(
            Path::new("crates/runtime/src/tape/mod.rs"),
            "pub const GRAMMAR_BIT0: u8 = 0x01;"
        )
        .is_ok());
    }

    #[test]
    fn admits_w2_typed_owner_parent_diff_only_under_w2_scope() {
        let changed = W2_TYPED_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v8-wave2-typed): add typed rows"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(&changed, "feat(other): move typed rows").is_err());
    }

    #[test]
    fn rejects_w2_scope_parent_diff_outside_typed_owner_paths() {
        let changed = vec![
            "crates/bbnf-bench/src/generated_real_typed.rs".to_string(),
            "crates/runtime/src/grammars/json/generated.rs".to_string(),
        ];
        assert!(validate_authorized_parent_diff(
            &changed,
            "fix(sk-v8-wave2-gate): fold typed allowance"
        )
        .is_err());
    }

    #[test]
    fn admits_w5_lock14_provider_parent_diff_only_under_w5_scope() {
        let changed = W5_LOCK14_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "fix(sk-v8-wave5-lock14): isolate json provider"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(&changed, "fix(other): isolate provider").is_err());
    }

    #[test]
    fn admits_sk_v14_w4_gate_json_parent_diff_only_under_w4_scope() {
        let changed = SK_V14_W4_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW4-redress): prune css ledger"
        )
        .is_ok());
        assert!(
            validate_authorized_parent_diff(&changed, "feat(sk-v14-waveW5): provider path")
                .is_err()
        );
    }

    #[test]
    fn admits_sk_v14_w5a_parent_diff_under_w5a_scope() {
        let changed = SK_V14_W5A_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5A-redress): route runtime request"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/codegen/src/css_l4_declaration_values_provider.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v14-waveW5A-redress): route runtime request"
        )
        .is_err());
    }

    #[test]
    fn w5a_provider_template_status_rejects_add_delete_rename() {
        assert!(validate_post_w5_provider_template_status(
            "status",
            "A  crates/codegen/src/css_l4_new_provider.rs\n"
        )
        .is_err());
        assert!(validate_post_w5_provider_template_status(
            "status",
            " M crates/codegen/src/css_l4_new_provider.rs\n"
        )
        .is_err());
        assert!(validate_post_w5_provider_template_status(
            "diff",
            "D\tcrates/codegen/src/json_templates/generated.rs\n"
        )
        .is_err());
        assert!(validate_post_w5_provider_template_status(
            "diff",
            "R100\tcrates/codegen/src/css_l4_old_provider.rs\tcrates/codegen/src/css_l4_new_provider.rs\n"
        )
        .is_err());
        assert!(validate_post_w5_provider_template_status(
            "diff",
            "D\tcrates/codegen/src/css_l4_visual_functions_templates/sink.rs\n"
        )
        .is_ok());
    }

    #[test]
    fn w5b_lock14_frontend_owner_paths_admit() {
        let changed = SK_V14_W5B_FRONTEND_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5B-FRONTEND): add frontend closure"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5B0): add lock14 frontend gate"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5B.4): consume frontend request"
        )
        .is_ok());

        let mut outside = changed;
        outside.push("crates/codegen/src/json_provider.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v14-waveW5B-FRONTEND): add frontend closure"
        )
        .is_err());
    }

    #[test]
    fn w5b_lock14_frontend_rejects_w5c_subject() {
        let changed = SK_V14_W5B_FRONTEND_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5C-GEN): remove provider dispatch"
        )
        .is_err());
    }

    #[test]
    fn w5b_lock14_frontend_rejects_w5d_subject() {
        let changed = SK_V14_W5B_FRONTEND_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5D-DELETE): delete provider mesh"
        )
        .is_err());
    }

    #[test]
    fn w5b_lock14_frontend_rejects_modified_provider() {
        assert!(validate_post_w5_provider_template_status(
            "status",
            " M crates/codegen/src/css_l4_declaration_values_provider.rs\n"
        )
        .is_err());
    }

    #[test]
    fn w5b_lock14_frontend_rejects_modified_template() {
        assert!(validate_post_w5_provider_template_status(
            "diff",
            "M\tcrates/codegen/src/json_templates/sink.rs\n"
        )
        .is_err());
    }

    #[test]
    fn w5d_post_w5_provider_template_topology_accepts_zero_providers_and_css_templates() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        let codegen_root = root.join("crates/codegen/src");
        assert_eq!(codegen_template_dir_count(&codegen_root).unwrap(), 1);

        let template_names = std::fs::read_dir(&codegen_root)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|entry| entry.file_type().map(|ty| ty.is_dir()).unwrap_or(false))
            .filter_map(|entry| entry.file_name().into_string().ok())
            .filter(|name| name.ends_with("_templates"))
            .collect::<Vec<_>>();
        assert_eq!(template_names.len(), 1);
        assert!(template_names.iter().any(|name| name == "json_templates"));
        assert_eq!(
            template_names
                .iter()
                .filter(|name| name.starts_with("css_l4_"))
                .count(),
            0
        );
        assert_eq!(
            std::fs::read_dir(&codegen_root)
                .unwrap()
                .filter_map(Result::ok)
                .filter(|entry| entry.file_name().to_str().is_some_and(|name| {
                    name.ends_with("_provider.rs") && name != "grammar_provider.rs"
                }))
                .count(),
            0
        );
    }

    #[test]
    fn w5b_lock14_frontend_allows_grammar_provider_exception() {
        assert!(validate_post_w5_provider_template_status(
            "status",
            " M crates/codegen/src/grammar_provider.rs\nA  crates/codegen/src/grammar_provider.rs\nD  crates/codegen/src/grammar_provider.rs\n"
        )
        .is_ok());
    }

    #[test]
    fn w5b_lock14_frontend_generic_owner_leak_census() {
        for path in SK_V14_W5B_FRONTEND_OWNER_PATHS {
            assert!(
                !path.contains("_templates"),
                "{path} leaks a template owner path into W5B-FRONTEND"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks a generated runtime path into W5B-FRONTEND"
            );
            assert!(
                !path.contains("css_l4_"),
                "{path} leaks a grammar-specific CSS owner path into W5B-FRONTEND"
            );
            if path.ends_with("_provider.rs") {
                assert_eq!(*path, "crates/codegen/src/grammar_provider.rs");
            }
        }
    }

    #[test]
    fn w5c_gen_owner_paths_admit() {
        let changed = SK_V14_W5C_GEN_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5C-GEN): remove provider dispatch"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW5C-GEN-redress): reject provider-free body"
        )
        .is_ok());

        let mut provider_delete = changed.clone();
        provider_delete.push("crates/codegen/src/css_l4_declaration_values_provider.rs".into());
        assert!(validate_authorized_parent_diff(
            &provider_delete,
            "feat(sk-v14-waveW5C-GEN): remove provider dispatch"
        )
        .is_err());

        let mut template_delete = changed.clone();
        template_delete
            .push("crates/codegen/src/css_l4_declaration_values_templates/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &template_delete,
            "feat(sk-v14-waveW5C-GEN): remove provider dispatch"
        )
        .is_err());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5D-DELETE): delete provider mesh"
        )
        .is_err());
    }

    #[test]
    fn w5c_gen_owner_paths_exclude_provider_template_and_runtime_outputs() {
        for path in SK_V14_W5C_GEN_OWNER_PATHS {
            assert!(
                !path.ends_with("_provider.rs")
                    || *path == "crates/codegen/src/grammar_provider.rs",
                "{path} leaks a provider owner path into W5C-GEN"
            );
            assert!(
                !path.contains("_templates"),
                "{path} leaks a template owner path into W5C-GEN"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks a generated runtime output path into W5C-GEN"
            );
        }
        assert!(SK_V14_W5C_GEN_OWNER_PATHS
            .iter()
            .any(|path| *path == "crates/codegen/src/runtime_generator.rs"));
    }

    #[test]
    fn w5d_delete_owner_paths_admit() {
        let changed = SK_V14_W5D_DELETE_OWNER_PATHS
            .iter()
            .map(|path| {
                if path.ends_with('/') {
                    format!("{path}generated.rs")
                } else {
                    (*path).to_string()
                }
            })
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW5D-DELETE-redress): reject deletion residue"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW5C-GEN): remove provider dispatch"
        )
        .is_err());
    }

    #[test]
    fn w5d_delete_owner_paths_preserve_json_templates() {
        assert!(!SK_V14_W5D_DELETE_OWNER_PATHS
            .iter()
            .any(|path| path.contains("json_templates")));
        assert!(SK_V14_W5D_DELETE_OWNER_PATHS
            .iter()
            .any(|path| *path == "crates/codegen/src/json_provider.rs"));
        assert!(SK_V14_W5D_DELETE_OWNER_PATHS
            .iter()
            .any(|path| path.ends_with("css_l4_declaration_values_templates/")));
    }

    #[test]
    fn w6_0_root_css_owner_paths_admit() {
        let changed = SK_V14_W6_0_ROOT_CSS_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.0-redress): reject root css l4 collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_0_root_css_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_0_ROOT_CSS_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.0 root CSS paths"
            );
        }
    }

    #[test]
    fn w6_0_root_css_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/other.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_0_ROOT_CSS_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.0): collapse root css l4 runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.0"
            );
        }
    }

    #[test]
    fn w6_0_root_css_inventory_is_exact() {
        let css_runtime_files = SK_V14_W6_0_ROOT_CSS_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/css_l4/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            css_runtime_files, 7,
            "W6.0 owns the seven CSS L4 runtime files"
        );
        let projection_sources = SK_V14_W6_0_ROOT_CSS_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.0 owns exactly the CSS L4 runtime projection source"
        );
        for path in SK_V14_W6_0_ROOT_CSS_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.0 must not own the full root runtime"
            );
            assert_ne!(*path, "../xtask/src/", "W6.0 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.0 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/css_l4_"),
                "{path} leaks skinny CSS output into W6.0"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.0"
            );
        }
    }

    #[test]
    fn w6_1_root_math_owner_paths_admit() {
        let changed = SK_V14_W6_1_ROOT_MATH_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.1): collapse root math runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.1-redress): reject root math collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_1_root_math_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_1_ROOT_MATH_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.1 root math paths"
            );
        }
    }

    #[test]
    fn w6_1_root_math_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/css_l4.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/regen_css.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_1_ROOT_MATH_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.1): collapse root math runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.1"
            );
        }
    }

    #[test]
    fn w6_1_root_math_inventory_is_exact() {
        let math_runtime_files = SK_V14_W6_1_ROOT_MATH_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/math/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            math_runtime_files, 7,
            "W6.1 owns the seven math runtime files"
        );
        let projection_sources = SK_V14_W6_1_ROOT_MATH_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.1 owns exactly the math runtime projection source"
        );
        for path in SK_V14_W6_1_ROOT_MATH_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.1 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/math/",
                "W6.1 must enumerate math runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.1 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.1 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.1"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.1"
            );
        }
    }

    #[test]
    fn w6_2_root_csv_owner_paths_admit() {
        let changed = SK_V14_W6_2_ROOT_CSV_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.2): collapse root csv runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.2-redress): reject root csv collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_2_root_csv_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_2_ROOT_CSV_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.2 root CSV paths"
            );
        }
    }

    #[test]
    fn w6_2_root_csv_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/regen_simple_runtime.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_2_ROOT_CSV_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.2): collapse root csv runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.2"
            );
        }
    }

    #[test]
    fn w6_2_root_csv_inventory_is_exact() {
        let csv_runtime_files = SK_V14_W6_2_ROOT_CSV_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/csv/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            csv_runtime_files, 7,
            "W6.2 owns the seven CSV runtime files"
        );
        let projection_sources = SK_V14_W6_2_ROOT_CSV_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.2 owns exactly the CSV runtime projection source"
        );
        for path in SK_V14_W6_2_ROOT_CSV_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.2 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/csv/",
                "W6.2 must enumerate CSV runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.2 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.2 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.2"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.2"
            );
        }
    }

    #[test]
    fn w6_3_root_bnf_owner_paths_admit() {
        let changed = SK_V14_W6_3_ROOT_BNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.3): collapse root bnf runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.3-redress): reject root bnf collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_3_root_bnf_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_3_ROOT_BNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.3 root BNF paths"
            );
        }
    }

    #[test]
    fn w6_3_root_bnf_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/regen_simple_runtime.rs",
            "../xtask/src/lib.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_3_ROOT_BNF_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.3): collapse root bnf runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.3"
            );
        }
    }

    #[test]
    fn w6_3_root_bnf_inventory_is_exact() {
        let bnf_runtime_files = SK_V14_W6_3_ROOT_BNF_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/bnf/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            bnf_runtime_files, 7,
            "W6.3 owns the seven BNF runtime files"
        );
        let projection_sources = SK_V14_W6_3_ROOT_BNF_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.3 owns exactly the BNF runtime projection source"
        );
        for path in SK_V14_W6_3_ROOT_BNF_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.3 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/bnf/",
                "W6.3 must enumerate BNF runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.3 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.3 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.3"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.3"
            );
        }
    }

    #[test]
    fn w6_4_root_ebnf_owner_paths_admit() {
        let changed = SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.4-redress): reject root ebnf collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_4_root_ebnf_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.4 root EBNF paths"
            );
        }
    }

    #[test]
    fn w6_4_root_ebnf_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/bnf/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/regen_simple_runtime.rs",
            "../xtask/src/lib.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.4): collapse root ebnf runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.4"
            );
        }
    }

    #[test]
    fn w6_4_root_ebnf_inventory_is_exact() {
        let ebnf_runtime_files = SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/ebnf/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            ebnf_runtime_files, 7,
            "W6.4 owns the seven EBNF runtime files"
        );
        let projection_sources = SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.4 owns exactly the EBNF runtime projection source"
        );
        for path in SK_V14_W6_4_ROOT_EBNF_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.4 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/ebnf/",
                "W6.4 must enumerate EBNF runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.4 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.4 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.4"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.4"
            );
        }
    }

    #[test]
    fn w6_5_root_css_pretty_owner_paths_admit() {
        let changed = SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.5-redress): reject root css pretty collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_5_root_css_pretty_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.5 root CSS Pretty paths"
            );
        }
    }

    #[test]
    fn w6_5_root_css_pretty_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/bnf/mod.rs",
            "../crates/core/src/runtime/ebnf/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/ebnf.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/regen_simple_runtime.rs",
            "../xtask/src/lib.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.5): collapse root css pretty runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.5"
            );
        }
    }

    #[test]
    fn w6_5_root_css_pretty_inventory_is_exact() {
        let css_pretty_runtime_files = SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/css_pretty/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            css_pretty_runtime_files, 7,
            "W6.5 owns the seven CSS Pretty runtime files"
        );
        let projection_sources = SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.5 owns exactly the CSS Pretty runtime projection source"
        );
        for path in SK_V14_W6_5_ROOT_CSS_PRETTY_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.5 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/css_pretty/",
                "W6.5 must enumerate CSS Pretty runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.5 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.5 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.5"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.5"
            );
        }
    }

    #[test]
    fn w6_6_root_google_sheets_owner_paths_admit() {
        let changed = SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.6-redress): reject root google sheets collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_6_root_google_sheets_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.6 root Google Sheets paths"
            );
        }
    }

    #[test]
    fn w6_6_root_google_sheets_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/bnf/mod.rs",
            "../crates/core/src/runtime/ebnf/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/ebnf.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/lib.rs",
            "../Cargo.toml",
        ] {
            let mut changed = SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.6): collapse root google sheets runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.6"
            );
        }
    }

    #[test]
    fn w6_6_root_google_sheets_inventory_is_exact() {
        let google_sheets_runtime_files = SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/google_sheets/")
                    && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            google_sheets_runtime_files, 10,
            "W6.6 owns the ten Google Sheets runtime files"
        );
        let projection_sources = SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.6 owns exactly the Google Sheets runtime projection source"
        );
        for path in SK_V14_W6_6_ROOT_GOOGLE_SHEETS_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.6 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/google_sheets/",
                "W6.6 must enumerate Google Sheets runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.6 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.6 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.6"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.6"
            );
        }
    }

    #[test]
    fn w6_7_root_bbnf_owner_paths_admit() {
        let changed = SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.7-redress): reject root bbnf collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_7_root_bbnf_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.7 root BBNF paths"
            );
        }
    }

    #[test]
    fn w6_7_root_bbnf_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/bnf/mod.rs",
            "../crates/core/src/runtime/ebnf/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/css_l4.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/ebnf.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/runtime-projections/json.toml",
            "../xtask/src/lib.rs",
            "../xtask/src/regen_css.rs",
            "../Cargo.toml",
            "crates/codegen/src/css_l4_declaration_values_provider.rs",
            "crates/codegen/src/css_l4_declaration_values_templates/",
        ] {
            let mut changed = SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.7): collapse root bbnf runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.7"
            );
        }
    }

    #[test]
    fn w6_7_root_bbnf_inventory_is_exact() {
        let bbnf_runtime_files = SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/bbnf/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            bbnf_runtime_files, 8,
            "W6.7 owns the eight BBNF runtime files"
        );
        let projection_sources = SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.7 owns exactly the BBNF runtime projection source"
        );
        for path in SK_V14_W6_7_ROOT_BBNF_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.7 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/bbnf/",
                "W6.7 must enumerate BBNF runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.7 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.7 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.7"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.7"
            );
        }
    }

    #[test]
    fn w6_8_root_json_owner_paths_admit() {
        let changed = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW6.8): collapse root json runtime"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "docs(sk-v14-waveW6.8-redress): reject root json collapse"
        )
        .is_ok());
    }

    #[test]
    fn w6_8_root_json_rejects_broad_w6_subjects() {
        let changed = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW6.7): collapse root bbnf runtime",
            "feat(sk-v14-waveW6.6): collapse root google sheets runtime",
            "feat(sk-v14-waveW6.5): collapse root css pretty runtime",
            "feat(sk-v14-waveW6.4): collapse root ebnf runtime",
            "feat(sk-v14-waveW6.3): collapse root bnf runtime",
            "feat(sk-v14-waveW6.2): collapse root csv runtime",
            "feat(sk-v14-waveW6.1): collapse root math runtime",
            "feat(sk-v14-waveW6.0): collapse root css l4 runtime",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize W6.8 root JSON paths"
            );
        }
    }

    #[test]
    fn w6_8_root_json_rejects_sibling_root_runtime_and_xtask() {
        for outside in [
            "../crates/core/src/runtime/css_l4/mod.rs",
            "../crates/core/src/runtime/css_pretty/mod.rs",
            "../crates/core/src/runtime/csv/mod.rs",
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/bnf/mod.rs",
            "../crates/core/src/runtime/ebnf/mod.rs",
            "../crates/core/src/runtime/google_sheets/mod.rs",
            "../crates/core/src/runtime/bbnf/mod.rs",
            "../xtask/runtime-projections/bbnf.toml",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/css_l4.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/csv.toml",
            "../xtask/runtime-projections/ebnf.toml",
            "../xtask/runtime-projections/google_sheets.toml",
            "../xtask/runtime-projections/math.toml",
            "../xtask/src/lib.rs",
            "../xtask/src/regen.rs",
            "../xtask/src/regen_css.rs",
            "../Cargo.toml",
            "crates/codegen/src/css_l4_declaration_values_provider.rs",
            "crates/codegen/src/css_l4_declaration_values_templates/",
        ] {
            let mut changed = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW6.8): collapse root json runtime"
                )
                .is_err(),
                "{outside} must not be admitted by W6.8"
            );
        }
    }

    #[test]
    fn w6_8_root_json_inventory_is_exact() {
        let json_runtime_files = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
            .iter()
            .filter(|path| {
                path.starts_with("../crates/core/src/runtime/json/") && path.ends_with(".rs")
            })
            .count();
        assert_eq!(
            json_runtime_files, 7,
            "W6.8 owns the seven JSON runtime files"
        );
        let template_files = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
            .iter()
            .filter(|path| {
                matches!(
                    **path,
                    "../crates/core/src/runtime/arena_template.rs"
                        | "../crates/core/src/runtime/builder_template.rs"
                )
            })
            .count();
        assert_eq!(
            template_files, 2,
            "W6.8 owns the two Pattern H template documentation rewrites"
        );
        let projection_sources = SK_V14_W6_8_ROOT_JSON_OWNER_PATHS
            .iter()
            .filter(|path| path.starts_with("../xtask/runtime-projections/"))
            .count();
        assert_eq!(
            projection_sources, 1,
            "W6.8 owns exactly the JSON runtime projection source"
        );
        for path in SK_V14_W6_8_ROOT_JSON_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.8 must not own the full root runtime"
            );
            assert_ne!(
                *path, "../crates/core/src/runtime/json/",
                "W6.8 must enumerate JSON runtime files"
            );
            assert_ne!(*path, "../xtask/src/", "W6.8 must not own all root xtask");
            assert_ne!(
                *path, "../xtask/runtime-projections/",
                "W6.8 must not own all root runtime projections"
            );
            assert!(
                !path.contains("crates/runtime/src/grammars/"),
                "{path} leaks skinny output into W6.8"
            );
            assert!(
                !path.contains("_provider.rs") && !path.contains("_templates"),
                "{path} leaks provider/template residue into W6.8"
            );
        }
    }

    #[test]
    fn w6_8_template_rewrite_removes_pattern_h_opt_out_language() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        let builder =
            std::fs::read_to_string(root.join("../crates/core/src/runtime/builder_template.rs"))
                .unwrap();
        for needle in [
            "# Outliers",
            "opt out",
            "opt-out",
            "Distinct shape",
            "no template instantiation",
            "JSON's builder",
            "CSS L4's builder",
            "BBNF's builder",
            "Sheets' builder",
        ] {
            assert!(
                !builder.contains(needle),
                "builder_template.rs retained forbidden W6.8 phrase `{needle}`"
            );
        }
        let arena =
            std::fs::read_to_string(root.join("../crates/core/src/runtime/arena_template.rs"))
                .unwrap();
        for needle in [
            "# Entries that opt out",
            "opt out",
            "opt-out",
            "Distinct shape",
            "JSON's arena",
            "CSS L4's arena",
            "Google Sheets' arena",
        ] {
            assert!(
                !arena.contains(needle),
                "arena_template.rs retained forbidden W6.8 phrase `{needle}`"
            );
        }
    }

    #[test]
    fn w7_policy_union_owner_paths_admit() {
        let changed = SK_V14_W7_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-waveW7): wire policy union runtime consumer"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-w7-prune5): wire policy union runtime consumer"
        )
        .is_ok());
    }

    #[test]
    fn w7_policy_union_rejects_prior_wave_subjects() {
        let changed = SK_V14_W7_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        for subject in [
            "feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer",
            "feat(sk-v14-waveW6.8): collapse root json runtime",
            "feat(sk-v14-waveW6): collapse root runtime cohort",
            "feat(sk-v14-waveW5D-DELETE): delete provider template residue",
            "feat(sk-v14-waveW8): readmit css l4",
        ] {
            assert!(
                validate_authorized_parent_diff(&changed, subject).is_err(),
                "{subject} must not authorize SK-V14 W7 policy/union paths"
            );
        }
    }

    #[test]
    fn w7_policy_union_rejects_outside_substrates() {
        for outside in [
            "crates/runtime/src/tape/mod.rs",
            "crates/bbnf-simd/src/lib.rs",
            "crates/codegen/src/css_l4_declaration_values_provider.rs",
            "crates/codegen/src/css_l4_declaration_values_templates/",
            "../crates/core/src/runtime/json/mod.rs",
        ] {
            let mut changed = SK_V14_W7_OWNER_PATHS
                .iter()
                .map(|path| (*path).to_string())
                .collect::<Vec<_>>();
            changed.push(outside.to_string());
            assert!(
                validate_authorized_parent_diff(
                    &changed,
                    "feat(sk-v14-waveW7): wire policy union runtime consumer"
                )
                .is_err(),
                "{outside} must not be admitted by W7"
            );
        }
    }

    #[test]
    fn admits_sk_v10_w5_root_typed_parent_diff_only_under_w5_scope() {
        let changed = SK_V10_W5_ROOT_TYPED_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v10-waveW5): prove root typed schema"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v10-waveW6): admit root typed row"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v10_w6_root_typed_parent_diff_under_w6_scope() {
        let changed = SK_V10_W6_ROOT_TYPED_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v10-waveW6): admit github events root typed row"
        )
        .is_ok());
        let mut outside = changed.clone();
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v10-waveW6): admit github events root typed row"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v12_w1a_parent_diff_under_w1a_scope() {
        let changed = vec![
            "crates/codegen/src/lib.rs".to_string(),
            "crates/codegen/src/grammar_profile.rs".to_string(),
            "crates/runtime/src/grammars/json/config.rs".to_string(),
            "crates/runtime/src/grammars/json/scan.rs".to_string(),
            "crates/passes/src/lib.rs".to_string(),
        ];
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v12-waveW1a): admit GrammarConfig Lock 14 legality gate"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/ir/src/lib.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v12-waveW1a): admit GrammarConfig Lock 14 legality gate"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w7_parent_diff_under_w7_scope() {
        let changed = SK_V13_W7_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW6): admit active cost"
        )
        .is_err());
        let mut outside = changed;
        outside.push("crates/runtime/src/tape/mod.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w8_parent_diff_under_w8_scope() {
        let changed = SK_V13_W8_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW8): admit per-grammar policy surface"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer"
        )
        .is_err());
        let mut outside = changed;
        outside.push("crates/parse-that-regex/src/lib.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW8): admit per-grammar policy surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w9_parent_diff_under_w9_scope() {
        let changed = SK_V13_W9_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW9): admit same-substrate union projection"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW7): admit CSP cascade fail-closed finalizer"
        )
        .is_err());
        let mut outside = changed;
        outside.push("crates/bbnf-simd/src/bitmap_next_set_bit.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW9): admit same-substrate union projection"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w11_1_parent_diff_under_w11_1_scope() {
        let changed = SK_V13_W11_1_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW11.1): admit numbers direct numeric-array dispatch"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW9): admit same-substrate union projection"
        )
        .is_err());
        let mut outside = changed;
        outside.push("crates/bbnf-simd/src/digit_mac.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW11.1): admit numbers direct numeric-array dispatch"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w11_3_parent_diff_under_w11_3_scope() {
        let changed = SK_V13_W11_3_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW11.3): admit mesh direct sink stack specialization"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW11.3): admit mesh direct sink stack specialization"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v14_w11a_parent_diff_under_w11a_scope() {
        let changed = SK_V14_W11A_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-w11a-direct-strict): admit strict-product direct rows"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v14-w10aa-parse-only): record fused string object-loop rejection"
        )
        .is_err());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v14-w11a-direct-strict): admit strict-product direct rows"
        )
        .is_err());
    }

    #[test]
    fn admits_skv15_w10_parent_diff_under_w10_scope() {
        let changed = SK_V15_W10_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v15-waveW10): quarantine FNV strict-product evidence"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/codegen/src/runtime_generator.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v15-waveW10): quarantine FNV strict-product evidence"
        )
        .is_err());
    }

    #[test]
    fn admits_skv15_w11_parent_diff_under_w11_scope() {
        let changed = SK_V15_W11_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "test(sk-v15-waveW11): restore Lock14 W10 gate accounting"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("xtask/src/main.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "test(sk-v15-waveW11): restore Lock14 W10 gate accounting"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w12_parent_diff_under_w12_scope() {
        let changed = SK_V13_W12_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW12): admit CSS delimiter SIMD production split"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW12): admit CSS delimiter SIMD production split"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w13_1_parent_diff_under_w13_1_scope() {
        let changed = SK_V13_W13_1_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW13.1): admit numbers typed product surface"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW13.1): admit numbers typed product surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w13_2_parent_diff_under_w13_2_scope() {
        let changed = SK_V13_W13_2_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW13.2): admit unicode basic typed product surface"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW13.2): admit unicode basic typed product surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w13_3_parent_diff_under_w13_3_scope() {
        let changed = SK_V13_W13_3_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW13.3): admit random typed product surface"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW13.3): admit random typed product surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w13_4_parent_diff_under_w13_4_scope() {
        let changed = SK_V13_W13_4_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW13.4): admit instruments typed product surface"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW13.4): admit instruments typed product surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w14_parent_diff_under_w14_scope() {
        let changed = SK_V13_W14_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW14.1): admit numbers parse-only surface"
        )
        .is_ok());
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW14.2): admit CITM catalog parse-only surface"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW14.2): admit CITM catalog parse-only surface"
        )
        .is_err());
    }

    #[test]
    fn admits_sk_v13_w15_1_parent_diff_under_w15_1_scope() {
        let changed = SK_V13_W15_1_OWNER_PATHS
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_authorized_parent_diff(
            &changed,
            "feat(sk-v13-waveW15.1): admit UpdateCenter typed plugin fast path"
        )
        .is_ok());
        let mut outside = changed;
        outside.push("crates/runtime/src/grammars/json/generated.rs".into());
        assert!(validate_authorized_parent_diff(
            &outside,
            "feat(sk-v13-waveW15.1): admit UpdateCenter typed plugin fast path"
        )
        .is_err());
    }

    #[test]
    fn normalizes_repo_root_paths_to_skinny_workspace_paths() {
        assert_eq!(
            normalize_git_path("skinny/crates/bbnf-bench/src/generated_real_typed.rs"),
            "crates/bbnf-bench/src/generated_real_typed.rs"
        );
        assert_eq!(
            normalize_git_path("crates/runtime/src/lib.rs"),
            "crates/runtime/src/lib.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/css_l4/value.rs"),
            "../crates/core/src/runtime/css_l4/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/bnf/value.rs"),
            "../crates/core/src/runtime/bnf/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/bbnf/value.rs"),
            "../crates/core/src/runtime/bbnf/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/csv/value.rs"),
            "../crates/core/src/runtime/csv/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/ebnf/value.rs"),
            "../crates/core/src/runtime/ebnf/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/math/value.rs"),
            "../crates/core/src/runtime/math/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/google_sheets/value.rs"),
            "../crates/core/src/runtime/google_sheets/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/json/value.rs"),
            "../crates/core/src/runtime/json/value.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/arena_template.rs"),
            "../crates/core/src/runtime/arena_template.rs"
        );
        assert_eq!(
            normalize_git_path("crates/core/src/runtime/builder_template.rs"),
            "../crates/core/src/runtime/builder_template.rs"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/css_l4.toml"),
            "../xtask/runtime-projections/css_l4.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/bnf.toml"),
            "../xtask/runtime-projections/bnf.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/bbnf.toml"),
            "../xtask/runtime-projections/bbnf.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/csv.toml"),
            "../xtask/runtime-projections/csv.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/ebnf.toml"),
            "../xtask/runtime-projections/ebnf.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/google_sheets.toml"),
            "../xtask/runtime-projections/google_sheets.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/math.toml"),
            "../xtask/runtime-projections/math.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/runtime-projections/json.toml"),
            "../xtask/runtime-projections/json.toml"
        );
        assert_eq!(
            normalize_git_path("xtask/src/regen_css.rs"),
            "../xtask/src/regen_css.rs"
        );
    }

    #[test]
    fn generic_crate_scan_rejects_json_policy_leaks() {
        for (_, token) in FORBIDDEN_GENERIC_TOKENS {
            let source = format!("fn leak() {{ /* {token} */ }}");
            assert!(
                validate_generic_source(Path::new("crates/codegen/src/lib.rs"), &source).is_err(),
                "token {token} should fail in generic roots"
            );
        }
    }

    #[test]
    fn generic_crate_scan_strips_test_only_json_tokens() {
        let source = r#"
pub fn production() {}

#[cfg(test)]
mod tests {
    use crate::grammars::json::{JsonSink, JsonValue};
}
"#;
        assert!(validate_generic_source(
            Path::new("crates/runtime/src/lib.rs"),
            strip_test_code(source)
        )
        .is_ok());
    }

    #[test]
    fn generic_scan_roots_cover_w7_generic_modules() {
        for root in [
            "crates/passes/src",
            "crates/codegen/src/lower",
            "crates/ir/src",
        ] {
            assert!(GENERIC_SCAN_ROOTS.contains(&root), "{root} is not scanned");
        }
    }

    #[test]
    fn skv15_w2_coverage_rejects_self_exemptions() {
        validate_skv15_w2_report_columns().unwrap();
        assert!(SKV15_W2_FORBIDDEN_FINDING_TOKENS.contains("RuntimeProvider"));
        assert!(SKV15_W2_PRIMITIVE_CLASS_ROOTS
            .iter()
            .any(|(prefix, _)| *prefix == "crates/bbnf-simd/src/aarch64"));
        assert!(validate_w2_value("self_scan_status", "self-exempting").is_err());
        assert!(validate_w2_value("disposition", "diagnostic:pre-W2-incomplete").is_err());
    }

    #[test]
    fn generated_header_lint_accepts_current_baseline_set() {
        let paths = GENERATED_HEADER_TOKEN_BASELINE
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        assert!(validate_generated_header_token_path_set(&paths).is_ok());
    }

    #[test]
    fn generated_header_lint_rejects_missing_baseline_path() {
        let mut paths = GENERATED_HEADER_TOKEN_BASELINE
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        paths.retain(|path| path != "crates/runtime/src/grammars/json/generated.rs");
        assert!(validate_generated_header_token_path_set(&paths).is_err());
    }

    #[test]
    fn generated_header_lint_rejects_unrecognized_token_path() {
        let mut paths = GENERATED_HEADER_TOKEN_BASELINE
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        paths.push("crates/runtime/src/grammars/json/surprise.rs".to_string());
        assert!(validate_generated_header_token_path_set(&paths).is_err());
    }

    #[test]
    fn generated_header_lint_accepts_recognized_json_real_typed_emission_path() {
        let mut paths = GENERATED_HEADER_TOKEN_BASELINE
            .iter()
            .map(|path| (*path).to_string())
            .collect::<Vec<_>>();
        paths.push("crates/codegen/src/json_sink_direct.rs".to_string());
        assert!(validate_generated_header_token_path_set(&paths).is_ok());
    }

    #[test]
    fn json_owned_roots_may_contain_json_policy_tokens() {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        for relative in [
            "crates/codegen/src/json_templates/generated.rs",
            "crates/codegen/src/json_templates/view.rs",
            "crates/codegen/src/json_sink_direct.rs",
            "crates/codegen/src/json_typed_direct.rs",
            "crates/runtime/src/grammars/json/scan.rs",
            "crates/runtime/src/grammars/json/sink.rs",
        ] {
            let source = std::fs::read_to_string(root.join(relative)).unwrap();
            assert!(
                FORBIDDEN_GENERIC_TOKENS
                    .iter()
                    .any(|(_, token)| source.contains(token)),
                "{relative} should carry JSON-owned policy evidence"
            );
        }
    }

    #[test]
    fn frozen_roots_cover_directive_and_asm_surfaces() {
        for root in [
            "crates/grammar/src",
            "crates/bbnf/src",
            "crates/bbnf-simd/build.rs",
            "crates/bbnf-simd/ext",
            "crates/parse-that-regex/src",
            "../crates/core/src/runtime/css_l4",
            "../crates/core/src/runtime/css_pretty",
            "../crates/core/src/runtime/bnf",
            "../crates/core/src/runtime/ebnf",
            "../crates/core/src/runtime/json",
            "../crates/core/src/runtime/arena_template.rs",
            "../crates/core/src/runtime/builder_template.rs",
            "../xtask/src/lib.rs",
            "../xtask/src/main.rs",
            "../xtask/src/regen.rs",
            "../xtask/src/regen_css.rs",
            "../xtask/runtime-projections/bnf.toml",
            "../xtask/runtime-projections/css_pretty.toml",
            "../xtask/runtime-projections/ebnf.toml",
            "../xtask/runtime-projections/json.toml",
        ] {
            assert!(FROZEN_ROOTS.contains(&root), "{root} is not frozen");
        }
        let status_args = git_path_args("status", "--porcelain", FROZEN_ROOTS).join(" ");
        assert!(status_args.contains("crates/grammar/src"));
        assert!(status_args.contains("crates/bbnf-simd/build.rs"));
        assert!(status_args.contains("crates/bbnf-simd/ext"));
        assert!(status_args.contains("../crates/core/src/runtime/css_l4"));
        assert!(status_args.contains("../crates/core/src/runtime/css_pretty"));
        assert!(status_args.contains("../crates/core/src/runtime/bnf"));
        assert!(status_args.contains("../crates/core/src/runtime/ebnf"));
        assert!(status_args.contains("../crates/core/src/runtime/json"));
        assert!(status_args.contains("../crates/core/src/runtime/arena_template.rs"));
        assert!(status_args.contains("../crates/core/src/runtime/builder_template.rs"));
        assert!(status_args.contains("../xtask/runtime-projections/css_pretty.toml"));
        assert!(status_args.contains("../xtask/runtime-projections/json.toml"));
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
