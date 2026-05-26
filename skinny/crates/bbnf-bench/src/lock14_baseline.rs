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
        "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values_extended/mod.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values_extended/parser.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_stylesheet_selectors/config.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_stylesheet_selectors/mod.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_stylesheet_selectors/sink.rs",
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_visual_functions/config.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_visual_functions/mod.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_visual_functions/parser.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_visual_functions/sink.rs",
        "per_grammar_runtime_source",
        "read_only",
        "runtime_source",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_at_rules_and_media/config.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_at_rules_and_media/mod.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_at_rules_and_media/parser.rs",
        "generated_nonjson_output",
        "read_only",
        "generated",
    ),
    entry(
        "crates/runtime/src/grammars/css_l4_at_rules_and_media/sink.rs",
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
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/config.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/generated.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/mod.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/parser.rs",
    "crates/runtime/src/grammars/css_l4_at_rules_and_media/sink.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/mod.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values/sink.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/mod.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/parser.rs",
    "crates/runtime/src/grammars/css_l4_declaration_values_extended/sink.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/config.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/generated.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/mod.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/parser.rs",
    "crates/runtime/src/grammars/css_l4_nested_layout/sink.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/config.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/generated.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/mod.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/parser.rs",
    "crates/runtime/src/grammars/css_l4_stylesheet_selectors/sink.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/config.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/generated.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/mod.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/parser.rs",
    "crates/runtime/src/grammars/css_l4_vendor_and_custom_atrules/sink.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/config.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/generated.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/mod.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/parser.rs",
    "crates/runtime/src/grammars/css_l4_visual_functions/sink.rs",
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
    "crates/bbnf-bench/src/bin/gate.rs",
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
    "../crates/core/src/runtime/css_l4",
    "../xtask/src/lib.rs",
    "../xtask/src/main.rs",
    "../xtask/src/regen.rs",
    "../xtask/src/regen_css.rs",
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
    "../xtask/src/lib.rs",
    "../xtask/src/main.rs",
    "../xtask/src/regen.rs",
    "../xtask/src/regen_css.rs",
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
            + SK_V14_W6_0_ROOT_CSS_OWNER_PATHS.len(),
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
    path.strip_prefix("skinny/").unwrap_or(path).to_string()
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
    Err(format!(
        "Lock 14 frozen diff failed for parent paths [{}]",
        changed_paths.join(", ")
    ))
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
            "feat(sk-v14-waveW6.1): collapse root math runtime",
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
            "../crates/core/src/runtime/math/mod.rs",
            "../crates/core/src/runtime/json/mod.rs",
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
        assert_eq!(css_runtime_files, 7, "W6.0 owns the seven CSS L4 runtime files");
        for path in SK_V14_W6_0_ROOT_CSS_OWNER_PATHS {
            assert_ne!(
                *path, "../crates/core/src/runtime/",
                "W6.0 must not own the full root runtime"
            );
            assert_ne!(*path, "../xtask/src/", "W6.0 must not own all root xtask");
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
            "../xtask/src/lib.rs",
            "../xtask/src/main.rs",
            "../xtask/src/regen.rs",
            "../xtask/src/regen_css.rs",
        ] {
            assert!(FROZEN_ROOTS.contains(&root), "{root} is not frozen");
        }
        let status_args = git_path_args("status", "--porcelain", FROZEN_ROOTS).join(" ");
        assert!(status_args.contains("crates/grammar/src"));
        assert!(status_args.contains("crates/bbnf-simd/build.rs"));
        assert!(status_args.contains("crates/bbnf-simd/ext"));
        assert!(status_args.contains("../crates/core/src/runtime/css_l4"));
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
