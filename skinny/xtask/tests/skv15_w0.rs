use xtask::skv15_w0::{
    validate_results, CSS_FEATURES, JSON_CORPORA, JSON_WORKLOADS, MANIFEST_ALIGN, MANIFEST_HEADER,
    MANIFEST_HEADING,
};

#[test]
fn skv15_w0_accepts_json_guard_and_css_diagnostic_manifest() {
    validate_results(&results_fixture()).unwrap();
}

#[test]
fn skv15_w0_rejects_missing_appended_field() {
    let bad = results_fixture().replace(
        "grammar=skinny/grammars/json.bbnf;generator=skinny-json-runtime",
        "",
    );
    assert!(validate_results(&bad).is_err());
}

#[test]
fn skv15_w0_rejects_css_live_admission_from_broadcast() {
    let bad = results_fixture()
        .replacen(
            "not_admitted:SK-V15-W0-broadcast-diagnostic",
            "PASS:scope=css_l4_w8_full_parse;checks=28;mismatches=0",
            1,
        )
        .replacen("AUDIT-FALSIFIED", "AUDIT-SUSTAINED", 1);
    assert!(validate_results(&bad).is_err());
}

#[test]
fn skv15_w0_rejects_self_exempting_gate_exclusion() {
    let bad = results_fixture().replacen(
        "none:full-surface-scan",
        "self-exempting:skips-new-leak-root",
        1,
    );
    assert!(validate_results(&bad).is_err());
}

#[test]
fn skv15_w0_rejects_hidden_json_broadcast() {
    let bad = results_fixture().replace(
        "json/twitter/direct_to_struct/main | row=json/twitter/direct_to_struct/main",
        "json/twitter/parse_only/main | row=json/twitter/direct_to_struct/main",
    );
    assert!(validate_results(&bad).is_err());
}

fn results_fixture() -> String {
    let mut out = String::from("# Skinny JSON Bench Results\n\n");
    out.push_str("| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Delta vs SK-V6 | Delta vs sonic-strict | Delta vs simdjson DOM | Delta vs yyjson | Hot leaf | Signal |\n");
    out.push_str("|---|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---|---|\n");
    for corpus in JSON_CORPORA {
        for workload in JSON_WORKLOADS {
            out.push_str(&markdown_row(&[
                *corpus,
                *workload,
                "A",
                "GO",
                "strict",
                "measured-row",
                "yes",
                "probe",
                value_plane(workload),
                "200.000",
                "190.000",
                "100.000",
                "n/a",
                "n/a",
                "n/a",
                "n/a",
                "n/a",
                "n/a",
                "n/a",
                "90.000",
                "n/a",
                "+100.0%",
                "n/a",
                "n/a",
                "fixture",
                "signal",
            ]));
        }
    }
    out.push('\n');
    out.push_str(MANIFEST_HEADING);
    out.push_str("\n\n");
    out.push_str(MANIFEST_HEADER);
    out.push('\n');
    out.push_str(MANIFEST_ALIGN);
    out.push('\n');
    for corpus in JSON_CORPORA {
        for workload in JSON_WORKLOADS {
            out.push_str(&json_manifest_row(corpus, workload));
        }
    }
    for feature in CSS_FEATURES {
        out.push_str(&css_manifest_row(feature));
    }
    out
}

fn json_manifest_row(corpus: &str, workload: &str) -> String {
    let row_id = format!("json/{corpus}/{workload}/main");
    let run_id = "SK-V15-W0:test-json";
    let sample_cost = "profile=test;iters=400";
    let measurement_origin = format!(
        "row={row_id};run={run_id};profile=test;sample_count=400;sample_cost={sample_cost}"
    );
    markdown_row(&[
        row_id.as_str(),
        "json",
        "json_bench",
        "SK-V14-W11W",
        run_id,
        "runtime::generated_json::parse_only",
        "bbnf_bench::json_parity::track2_structural_oracle",
        "parse_only/sonic_rs::Skipper",
        "PASS:scope=profile-direct-cold;checks=400;mismatches=0",
        "AUDIT-SUSTAINED",
        "sk-v15-W0:test-json",
        "absent:not-collected-for-json",
        "parse_only_validator",
        "generated_function",
        "generated_grammar",
        "measured-row",
        "profile_direct:test",
        sample_cost,
        "400",
        "profile=release;rustflags=-C target-cpu=native;target_cpu=native",
        "aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max",
        "arch=aarch64;os=macos;simd=Scalar;target_cpu=native",
        "none:pre-W1:none:pre-W1:none:pre-W1",
        "none:SK-V15-W0-test",
        "admitted:SK-V14-json-test",
        value_plane(workload),
        "n/a",
        "zero_or_inert",
        "generated_json_parse_only_contract",
        "independent_verified",
        "structural_scan+masking_probes+pmu+cycles:nonproducer",
        "sonic_rs_strict[plane=parse_only/sonic_rs::Skipper,strictness=strict,freshness=same-run-native,sidecar=n/a,mbps=100.000,source=test]",
        row_id.as_str(),
        measurement_origin.as_str(),
        value_plane(workload),
        "n/a:not-css",
        "grammar=skinny/grammars/json.bbnf;generator=skinny-json-runtime",
        "included=json-runtime+bench+gate;excluded=none:full-surface-scan;owner=SK-V15-W0",
        "not-applicable:no-simd-or-asm",
        "pass:json_same_run_parity",
        "none:full-surface-scan",
        "none:independent",
    ])
}

fn css_manifest_row(feature: &str) -> String {
    let row_id = format!("css_l4/{feature}/direct_to_struct/main");
    markdown_row(&[
        row_id.as_str(),
        "css_l4",
        "css_l4_bench",
        "SK-V14-W8R",
        "SK-V14-W8R:css-full-parse-profile-cold-8",
        "runtime::generated_css_l4_stylesheet_selectors::parser::parse_full",
        "cssparser::StyleSheetParser full-parse probe",
        "lightningcss full-parse",
        "not_admitted:SK-V15-W0-broadcast-diagnostic",
        "AUDIT-FALSIFIED",
        "sk-v15-W0:broadcast-diagnostic;sk-v13/v1-css-l4-validation:section-1-6",
        "same-run:production-corpus-full-parse",
        "css_l4_full_parse",
        "full_parse_summary",
        "generated_grammar",
        "restart/skinny/tranches/sk-v14/research/skv14-redress-215-css-full-parse-prototype.md",
        "cargo-test-release:bbnf-bench::css_l4_w8",
        "track1_mbps=2319.041;cssparser_mbps=2362.037;lightningcss_mbps=929.281",
        "8",
        "profile=release;rustflags=-C target-cpu=native;target_cpu=native",
        "aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max",
        "arch=aarch64;os=macos;simd=Scalar;target_cpu=native",
        "schema=css-l4-full-parse-v1;current_status=AUDIT-FALSIFIED_OPEN;current_reason=SK-V15-W0_broadcast_diagnostic",
        "pending:SK-V15-W1-CSS-BROADCAST",
        "diagnostic:SK-V15-W0-broadcast",
        "generated_css_l4_stylesheet_selectors",
        "css_l4_full_parse",
        "one",
        "gate_json_skv15_broadcast_diagnostic",
        "diagnostic:cssparser+lightningcss-workload-mismatch",
        "scalar_reference=diagnostic:cssparser_full_parse;checkasm_or_parity=pass:cssparser_full_parse_diagnostic;json_guard_state=maintain:sk-v15",
        "track1_generated[plane=css_l4_full_parse,strictness=strict,freshness=same-run-native,sidecar=n/a,mbps=2319.041,source=test]",
        "SK-V14-W8R-css-full-parse-profile-cold-8",
        "diagnostic-broadcast:SK-V14-W8R-css-full-parse-profile-cold-8;run=SK-V14-W8R:css-full-parse-profile-cold-8;profile=cargo-test-release:bbnf-bench::css_l4_w8",
        "full_parse_summary",
        "mismatch:track1_full_parse_summary_vs_lightningcss_cssom;cssparser=stylesheet_full_parse",
        "diagnostic:CSS_GENERATED_RS-string-literal;path=skinny/crates/codegen/src/runtime_generator.rs",
        "diagnostic:pre-W2-incomplete;included=legacy-lock14;excluded=known-leak-roots;owner=SK-V15-W2",
        "not-applicable:no-simd-or-asm",
        "pass:cssparser_full_parse_diagnostic",
        "diagnostic:pre-W2-exclusions-reported;owner=SK-V15-W2;disposition=non-admission",
        "SK-V14-W8R-css-l4-full-parse",
    ])
}

fn value_plane(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "json_parse_only",
        "direct_to_struct" => "json_direct_strict_product",
        "real_typed_struct" => "json_typed_direct",
        _ => "unknown",
    }
}

fn markdown_row(cells: &[&str]) -> String {
    let mut out = String::from("| ");
    out.push_str(&cells.join(" | "));
    out.push_str(" |\n");
    out
}
