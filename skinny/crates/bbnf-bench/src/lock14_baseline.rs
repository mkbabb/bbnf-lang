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
        "crates/codegen/src/json_provider.rs",
        "per_grammar_provider",
        "read_only",
        "provider",
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

pub fn validate(root: &Path) -> Result<(), String> {
    validate_entries(ALLOWLIST, root, true)?;
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

const FROZEN_ROOTS: &[&str] = &[
    "grammars",
    "test_data",
    "crates/test-fixtures",
    "crates/runtime/src",
    "crates/ir/src",
    "crates/bbnf-regex/src",
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
            + SK_V13_W5_OWNER_PATHS.len(),
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
            "{error}: git diff --quiet HEAD^ -- {}",
            FROZEN_ROOTS.join(" ")
        )
    })
}

fn git_parent_changed_paths(root: &Path) -> Result<Vec<String>, String> {
    let mut args = vec!["diff", "--name-only", "HEAD^", "--"];
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
    Err(format!(
        "Lock 14 frozen diff failed for parent paths [{}]",
        changed_paths.join(", ")
    ))
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
    "crates/codegen/src/grammar_profile.rs",
    "crates/passes/src/lib.rs",
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
