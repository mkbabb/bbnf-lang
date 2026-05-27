use crate::gate::{self, Outcome, Verdict};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::Path;

pub const SCHEMA_V3_HEADER: &str = "| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Δ vs SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM | Δ vs yyjson | Hot leaf | Signal |";
const SCHEMA_V3_ALIGN: &str = "|---|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---|---|";
pub const SKV14_W0_MANIFEST_HEADER: &str = "| Row id | Grammar | Domain | Wave | Run id | Track 1 entry | Track 2 entry | Comparator plane | Per-iter equality | Audit overlay | Audit reference | Sidecar freshness | Substrate target | Retention lifetime | Policy owner | Validation | Profile artifact | Sample cost | Sample count | Build flags | Host triple | Feature mask | CostFacts | Redress | SK-V14-open delta | Substrate | Structural projection | Cardinality | Consumer | Track 2 | Diagnostic nonproducer | Comparator evidence |";
const SKV14_W0_MANIFEST_ALIGN: &str = "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---:|---|---|---|---|---|---|---|---|---|---|---|---|---|";

const SKV14_JSON_CORPORA: &[&str] = &[
    "twitter",
    "citm_catalog",
    "canada",
    "apache_builds",
    "github_events",
    "update_center",
    "mesh",
    "random",
    "gsoc-2018",
    "marine_ik",
    "instruments",
    "numbers",
    "unicode_mixed",
    "unicode_escapes",
    "unicode_basic",
    "distinct_values",
    "y_string_unicode",
];

const SKV14_JSON_WORKLOADS: &[&str] = &["parse_only", "direct_to_struct", "real_typed_struct"];

const SKV14_W9_TYPED_ADMIT_ROWS: &[&str] = &[
    "json/twitter/real_typed_struct/main",
    "json/citm_catalog/real_typed_struct/main",
    "json/canada/real_typed_struct/main",
    "json/apache_builds/real_typed_struct/main",
    "json/github_events/real_typed_struct/main",
    "json/update_center/real_typed_struct/main",
    "json/mesh/real_typed_struct/main",
    "json/random/real_typed_struct/main",
    "json/marine_ik/real_typed_struct/main",
    "json/instruments/real_typed_struct/main",
    "json/numbers/real_typed_struct/main",
    "json/unicode_basic/real_typed_struct/main",
    "json/distinct_values/real_typed_struct/main",
];

const SKV14_W11A_DIRECT_STRICT_ADMIT_ROWS: &[&str] = &[
    "json/twitter/direct_to_struct/main",
    "json/citm_catalog/direct_to_struct/main",
    "json/canada/direct_to_struct/main",
    "json/apache_builds/direct_to_struct/main",
    "json/github_events/direct_to_struct/main",
    "json/update_center/direct_to_struct/main",
    "json/mesh/direct_to_struct/main",
    "json/random/direct_to_struct/main",
    "json/marine_ik/direct_to_struct/main",
    "json/instruments/direct_to_struct/main",
    "json/numbers/direct_to_struct/main",
    "json/unicode_basic/direct_to_struct/main",
    "json/distinct_values/direct_to_struct/main",
];

const SKV14_W11L_TOKEN_PRODUCT_TYPED_ROWS: &[&str] =
    &["json/y_string_unicode/real_typed_struct/main"];

const SKV14_W11L_TOKEN_PRODUCT_DIRECT_ROWS: &[&str] =
    &["json/y_string_unicode/direct_to_struct/main"];

const SKV14_W11N_UNICODE_MIXED_TYPED_ROWS: &[&str] = &["json/unicode_mixed/real_typed_struct/main"];

const SKV14_W11N_UNICODE_MIXED_DIRECT_ROWS: &[&str] = &["json/unicode_mixed/direct_to_struct/main"];

const SKV14_W11O_GSOC_TYPED_ROWS: &[&str] = &["json/gsoc-2018/real_typed_struct/main"];

const SKV14_W11O_GSOC_DIRECT_ROWS: &[&str] = &["json/gsoc-2018/direct_to_struct/main"];

const SKV14_W11U_UNICODE_ESCAPES_TYPED_ROWS: &[&str] =
    &["json/unicode_escapes/real_typed_struct/main"];

const SKV14_W11U_UNICODE_ESCAPES_DIRECT_ROWS: &[&str] =
    &["json/unicode_escapes/direct_to_struct/main"];

const SKV14_CSS_FEATURES: &[&str] = &[
    "declaration_values",
    "declarations",
    "stylesheet_root",
    "selectors",
    "at_rules_keyframes",
    "nested_rules",
    "css_variables",
    "calc_expressions",
    "var_url_functions",
    "color_functions",
    "gradients",
    "transforms",
    "filters",
    "easing_functions",
    "media_queries",
    "vendor_prefixes",
    "custom_at_rules",
    "pseudo_classes",
    "pseudo_elements",
    "attribute_selectors",
    "logical_properties",
    "grid",
    "flexbox",
    "typed_property_groups",
];

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct Report {
    pub title: String,
    pub rows: Vec<TelemetryRow>,
    pub probe_rows: Vec<ProbeReportRow>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct ComparatorSet {
    pub sonic_strict_mbps: Option<f64>,
    pub sonic_lossy_mbps: Option<f64>,
    pub simdjson_dom_mbps: Option<f64>,
    pub simdjson_ondemand_mbps: Option<f64>,
    pub yyjson_default_mbps: Option<f64>,
    pub asmjson_swar_mbps: Option<f64>,
    pub asmjson_avx512_mbps: Option<f64>,
    pub rapidjson_default_mbps: Option<f64>,
    pub serde_json_mbps: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV8ComparatorEvidence {
    pub comparator_id: String,
    pub comparator_plane: String,
    pub comparator_strictness: String,
    pub comparator_freshness: String,
    pub sidecar_freshness: String,
    pub value_mbps: Option<f64>,
    pub source_artifact: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV8Telemetry {
    pub row_id: String,
    pub grammar_id: String,
    pub domain: String,
    pub measured_validation_path: String,
    pub profile_artifact: String,
    pub sample_cost: String,
    pub sample_count: u64,
    pub build_flags: String,
    pub host_triple: String,
    pub feature_mask: String,
    pub costfacts_rule_id: String,
    pub costfacts_chosen_shape: String,
    pub costfacts_rejected_alternative_ids: Vec<String>,
    pub redress_entry: String,
    pub wave_id: String,
    pub run_id: String,
    pub sk_v9_open_delta: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub track1_entry_point: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub track2_entry_point: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub comparator_plane: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub per_iter_equality: String,
    #[serde(default = "default_audit_pending")]
    pub audit_overlay_verdict: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub audit_overlay_reference: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub sidecar_freshness: String,
    #[serde(default = "default_local_temp_only")]
    pub substrate_target: String,
    #[serde(default = "default_local_loop")]
    pub retention_lifetime: String,
    #[serde(default = "default_no_policy_owner")]
    pub policy_owner: String,
    #[serde(default = "legacy_skv14_telemetry_value")]
    pub sk_v14_open_delta: String,
    pub substrate_surface: String,
    pub structural_projection_status: String,
    pub substrate_cardinality: String,
    pub same_wave_consumer_class: String,
    pub track2_independence_status: String,
    pub diagnostic_nonproducer_status: String,
    pub comparators: Vec<SkV8ComparatorEvidence>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct TelemetryRow {
    pub corpus: String,
    pub workload: String,
    pub outcome_id: String,
    pub verdict: String,
    pub strictness: String,
    pub parse_utf8: String,
    pub escape_complete: String,
    pub flaw_probe: String,
    pub output_plane: String,
    pub track1_mbps: Option<f64>,
    pub track2_mbps: Option<f64>,
    pub competitors: ComparatorSet,
    pub delta_vs_skv6: String,
    pub delta_vs_sonic_strict: Option<f64>,
    pub delta_vs_simdjson_dom: Option<f64>,
    pub delta_vs_yyjson: Option<f64>,
    pub hot_leaf: String,
    pub signal: String,
    pub sk_v8: SkV8Telemetry,
}

fn legacy_skv14_telemetry_value() -> String {
    "legacy:pre-skv14-manifest".to_string()
}

fn default_audit_pending() -> String {
    "AUDIT-PENDING".to_string()
}

fn default_local_temp_only() -> String {
    "local_temp_only".to_string()
}

fn default_local_loop() -> String {
    "local_loop".to_string()
}

fn default_no_policy_owner() -> String {
    "none".to_string()
}

#[derive(Debug, Clone, PartialEq)]
struct SkV14ManifestRow {
    row_id: String,
    grammar_id: String,
    domain: String,
    wave_id: String,
    run_id: String,
    track1_entry_point: String,
    track2_entry_point: String,
    comparator_plane: String,
    per_iter_equality: String,
    audit_overlay_verdict: String,
    audit_overlay_reference: String,
    sidecar_freshness: String,
    substrate_target: String,
    retention_lifetime: String,
    policy_owner: String,
    measured_validation_path: String,
    profile_artifact: String,
    sample_cost: String,
    sample_count: u64,
    build_flags: String,
    host_triple: String,
    feature_mask: String,
    costfacts: String,
    redress_entry: String,
    sk_v14_open_delta: String,
    substrate_surface: String,
    structural_projection_status: String,
    substrate_cardinality: String,
    same_wave_consumer_class: String,
    track2_independence_status: String,
    diagnostic_nonproducer_status: String,
    comparator_evidence: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct ProbeReportRow {
    pub corpus: String,
    pub probe: String,
    pub mbps: Option<f64>,
    pub ns_per_iter: Option<f64>,
    pub vs_track1: Option<f64>,
    pub signal: String,
}

pub const W1A_NON_JSON_REPORT_SCHEMA: &str = "sk-v11-w1a-nonjson-v1";
const W1A_RUN_ID_PREFIX: &str = "sk-v11-w1a:fixture-fnv64-";
pub const SKV12_NON_JSON_REPORT_SCHEMA: &str = "sk-v12-nonjson-generated-v1";
pub const SKV12_CSS_L4_SOTA_REPORT_SCHEMA: &str = "sk-v12-css-l4-sota-v1";
pub const SKV13_CSS_COMPARATOR_ORACLE_REPORT_SCHEMA: &str = "sk-v13-css-comparator-oracle-v1";
pub const SKV13_CSS_STYLESHEET_SELECTORS_REPORT_SCHEMA: &str =
    "sk-v13-css-stylesheet-selectors-sota-v1";
pub const SKV13_CSS_DECLARATION_VALUES_EXTENDED_REPORT_SCHEMA: &str =
    "sk-v13-css-declaration-values-extended-sota-v1";
pub const SKV13_CSS_VISUAL_FUNCTIONS_REPORT_SCHEMA: &str = "sk-v13-css-visual-functions-sota-v1";
pub const SKV13_CSS_AT_RULES_AND_MEDIA_REPORT_SCHEMA: &str = "sk-v13-css-at-rules-media-sota-v1";
pub const SKV13_CSS_VENDOR_CUSTOM_REPORT_SCHEMA: &str = "sk-v13-css-vendor-custom-sota-v1";
pub const SKV13_CSS_NESTED_LAYOUT_REPORT_SCHEMA: &str = "sk-v13-css-nested-layout-sota-v1";
pub const SKV13_DECISION_REGEX_REPORT_SCHEMA: &str = "sk-v13-decision-regex-v1";
pub const SKV13_DECISION_ACTIVE_COST_REPORT_SCHEMA: &str = "sk-v13-decision-active-cost-v1";
pub const SKV13_DECISION_CSP_CASCADE_REPORT_SCHEMA: &str = "sk-v13-decision-csp-cascade-v1";
pub const SKV13_PER_GRAMMAR_POLICY_REPORT_SCHEMA: &str = "sk-v13-per-grammar-policy-v1";
pub const SKV13_SAME_SUBSTRATE_UNION_REPORT_SCHEMA: &str = "sk-v13-same-substrate-union-v1";
pub const SKV13_JSON_DIRECT_REOPEN_REPORT_SCHEMA: &str = "sk-v13-json-direct-reopen-v1";
pub const SKV13_JSON_PARSE_ONLY_REPORT_SCHEMA: &str = "sk-v13-json-parse-only-v1";
pub const SKV14_JSON_PARSE_ONLY_REPORT_SCHEMA: &str = "sk-v14-json-parse-only-v1";
pub const SKV13_TYPED_PRODUCT_REPORT_SCHEMA: &str = "sk-v13-typed-product-v1";
pub const SKV13_SIMD_ASM_PRODUCTION_REPORT_SCHEMA: &str = "sk-v13-simd-asm-production-v1";
pub type NonJsonEvidenceRow = TelemetryRow;
pub type NonJsonOracleEvidence = SkV8ComparatorEvidence;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JsonParseOnlyAdmissionSpec {
    pub label: &'static str,
    pub wave_id: &'static str,
    pub run_id_prefix: &'static str,
    pub consumer_gate: &'static str,
    pub row_id: &'static str,
    pub corpus: &'static str,
    pub criterion_group: &'static str,
    pub bytes: u64,
    pub route_id: &'static str,
    pub redress_entry: &'static str,
    pub prior_redress_citation: &'static str,
}

pub const JSON_PARSE_ONLY_ADMISSION_SPECS: &[JsonParseOnlyAdmissionSpec] = &[
    JsonParseOnlyAdmissionSpec {
        label: "W11W.1",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/twitter/parse_only/main",
        corpus: "twitter",
        criterion_group: "json_twitter",
        bytes: 631_515,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10V.1",
        wave_id: "SK-V14-W10V",
        run_id_prefix: "SK-V14-W10V:",
        consumer_gate: "G-SK-V14-W10V-JSON-PARSE-ONLY-CURRENT-HEAD-RESWEEP",
        row_id: "json/citm_catalog/parse_only/main",
        corpus: "citm_catalog",
        criterion_group: "json_citm_catalog",
        bytes: 1_727_204,
        route_id: "generated-json-parse-only-current-head-resweep",
        redress_entry: "none:SK-V14-W10V-admit",
        prior_redress_citation: "221",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10R.1",
        wave_id: "SK-V14-W10R",
        run_id_prefix: "SK-V14-W10R:",
        consumer_gate: "G-SK-V14-W10R-JSON-PARSE-ONLY-PREFIX",
        row_id: "json/canada/parse_only/main",
        corpus: "canada",
        criterion_group: "json_canada",
        bytes: 2_251_051,
        route_id: "generated-json-parse-only-prefix-continuation",
        redress_entry: "none:SK-V14-W10R-admit",
        prior_redress_citation: "217",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10W.1",
        wave_id: "SK-V14-W10W",
        run_id_prefix: "SK-V14-W10W:",
        consumer_gate: "G-SK-V14-W10W-JSON-PARSE-ONLY-ITERATIVE-STACK",
        row_id: "json/apache_builds/parse_only/main",
        corpus: "apache_builds",
        criterion_group: "json_apache_builds",
        bytes: 127_275,
        route_id: "generated-json-parse-only-iterative-stack",
        redress_entry: "none:SK-V14-W10W-admit",
        prior_redress_citation: "222",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W11W.2",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/github_events/parse_only/main",
        corpus: "github_events",
        criterion_group: "json_github_events",
        bytes: 65_132,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W11W.3",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/update_center/parse_only/main",
        corpus: "update_center",
        criterion_group: "json_update_center",
        bytes: 533_178,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.7",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/mesh/parse_only/main",
        corpus: "mesh",
        criterion_group: "json_mesh",
        bytes: 723_597,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W11W.4",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/random/parse_only/main",
        corpus: "random",
        criterion_group: "json_random",
        bytes: 510_476,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W11W.5",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/gsoc-2018/parse_only/main",
        corpus: "gsoc-2018",
        criterion_group: "json_gsoc-2018",
        bytes: 3_327_831,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.10",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/marine_ik/parse_only/main",
        corpus: "marine_ik",
        criterion_group: "json_marine_ik",
        bytes: 2_983_466,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10T.1",
        wave_id: "SK-V14-W10T",
        run_id_prefix: "SK-V14-W10T:",
        consumer_gate: "G-SK-V14-W10T-JSON-PARSE-ONLY-OPEN-SWEEP",
        row_id: "json/instruments/parse_only/main",
        corpus: "instruments",
        criterion_group: "json_instruments",
        bytes: 220_346,
        route_id: "generated-json-parse-only-open-sweep",
        redress_entry: "none:SK-V14-W10T-admit",
        prior_redress_citation: "219",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.12",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/numbers/parse_only/main",
        corpus: "numbers",
        criterion_group: "json_numbers",
        bytes: 150_124,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10S.1",
        wave_id: "SK-V14-W10S",
        run_id_prefix: "SK-V14-W10S:",
        consumer_gate: "G-SK-V14-W10S-JSON-PARSE-ONLY-STRING-END",
        row_id: "json/unicode_mixed/parse_only/main",
        corpus: "unicode_mixed",
        criterion_group: "json_unicode_mixed",
        bytes: 1_053_086,
        route_id: "generated-json-parse-only-string-end-prefix-scan",
        redress_entry: "none:SK-V14-W10S-admit",
        prior_redress_citation: "218",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.14",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/unicode_escapes/parse_only/main",
        corpus: "unicode_escapes",
        criterion_group: "json_unicode_escapes",
        bytes: 1_050_797,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.15",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/unicode_basic/parse_only/main",
        corpus: "unicode_basic",
        criterion_group: "json_unicode_basic",
        bytes: 1_048_586,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W11W.6",
        wave_id: "SK-V14-W11W",
        run_id_prefix: "SK-V14-W11W:",
        consumer_gate: "G-SK-V14-W11W-JSON-PARSE-ONLY-MEMCHR",
        row_id: "json/distinct_values/parse_only/main",
        corpus: "distinct_values",
        criterion_group: "json_distinct_values",
        bytes: 153_630,
        route_id: "generated-json-parse-only-memchr-trusted-string-split",
        redress_entry: "none:SK-V14-W11W-admit",
        prior_redress_citation: "247",
    },
    JsonParseOnlyAdmissionSpec {
        label: "W10.17",
        wave_id: "SK-V14-W10",
        run_id_prefix: "SK-V14-W10:",
        consumer_gate: "G-W10-JSON-PARSE-ONLY",
        row_id: "json/y_string_unicode/parse_only/main",
        corpus: "y_string_unicode",
        criterion_group: "json_y_string_unicode",
        bytes: 35_601,
        route_id: "generated-json-parse-only-distinct-path",
        redress_entry: "none:SK-V14-W10-admit",
        prior_redress_citation: "102",
    },
];

pub const SKV13_JSON_PARSE_ONLY_ADMISSION_SPECS: &[JsonParseOnlyAdmissionSpec] =
    JSON_PARSE_ONLY_ADMISSION_SPECS;

pub fn json_parse_only_admission_spec_for_report(
    report: &SkV13JsonParseOnlyReport,
) -> Result<&'static JsonParseOnlyAdmissionSpec, String> {
    let Some(spec) = json_parse_only_admission_spec_for_row_id(&report.row_id) else {
        return Err(format!("unsupported JSON parse-only row {}", report.row_id));
    };
    if report.wave_id != spec.wave_id || report.corpus != spec.corpus {
        return Err(format!(
            "{} has invalid JSON parse-only report identity {}/{}",
            spec.label, report.wave_id, report.corpus
        ));
    }
    Ok(spec)
}

pub fn json_parse_only_admission_spec_for_row_id(
    row_id: &str,
) -> Option<&'static JsonParseOnlyAdmissionSpec> {
    JSON_PARSE_ONLY_ADMISSION_SPECS
        .iter()
        .find(|spec| spec.row_id == row_id)
}

pub fn json_parse_only_admission_spec_for_corpus(
    corpus: &str,
    bytes: u64,
) -> Option<&'static JsonParseOnlyAdmissionSpec> {
    JSON_PARSE_ONLY_ADMISSION_SPECS
        .iter()
        .find(|spec| spec.corpus == corpus && spec.bytes == bytes)
}

pub fn json_parse_only_audit_reference(spec: &JsonParseOnlyAdmissionSpec) -> &'static str {
    match spec.wave_id {
        "SK-V14-W10R" => "sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        "SK-V14-W10S" => "sk-v14-W10S:parse-only-string-end-prefix-scan;sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        "SK-V14-W10T" => "sk-v14-W10T:parse-only-open-sweep;sk-v14-W10S:parse-only-string-end-prefix-scan;sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        "SK-V14-W10V" => "sk-v14-W10V:parse-only-current-head-resweep;sk-v14-W10U:number-end-reject;sk-v14-W10T:parse-only-open-sweep;sk-v14-W10S:parse-only-string-end-prefix-scan;sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        "SK-V14-W10W" => "sk-v14-W10W:parse-only-iterative-stack;sk-v14-W10V:parse-only-current-head-resweep;sk-v14-W10U:number-end-reject;sk-v14-W10T:parse-only-open-sweep;sk-v14-W10S:parse-only-string-end-prefix-scan;sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        "SK-V14-W11W" => "sk-v14-W11W:parse-only-memchr-trusted-string-split;sk-v14-W11V:string64-reject;sk-v14-W11T:structural-stream-reject;sk-v14-W11S:parse-only-stage0-attribution;sk-v14-W10W:parse-only-iterative-stack;sk-v14-W10V:parse-only-current-head-resweep;sk-v14-W10T:parse-only-open-sweep;sk-v14-W10S:parse-only-string-end-prefix-scan;sk-v14-W10R:parse-only-prefix-continuation;sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
        _ => "sk-v14-W10:distinct-parse-only;sk-v13/v6-comparator-integrity:§1+§3",
    }
}

pub fn json_parse_only_open_delta(spec: &JsonParseOnlyAdmissionSpec) -> &'static str {
    match spec.wave_id {
        "SK-V14-W10R" => "admitted:SK-V14-W10R-parse-only-prefix-continuation",
        "SK-V14-W10S" => "admitted:SK-V14-W10S-parse-only-string-end-prefix-scan",
        "SK-V14-W10T" => "admitted:SK-V14-W10T-parse-only-open-sweep",
        "SK-V14-W10V" => "admitted:SK-V14-W10V-current-head-resweep",
        "SK-V14-W10W" => "admitted:SK-V14-W10W-parse-only-iterative-stack",
        "SK-V14-W11W" => "admitted:SK-V14-W11W-parse-only-memchr",
        _ => "admitted:SK-V14-W10-parse-only-distinct",
    }
}

const SKV13_CSS_FEATURES: &[&str] = &[
    "declaration_values",
    "declarations",
    "stylesheet_root",
    "selectors",
    "at_rules_keyframes",
    "nested_rules",
    "css_variables",
    "calc_expressions",
    "var_url_functions",
    "color_functions",
    "gradients",
    "transforms",
    "filters",
    "easing_functions",
    "media_queries",
    "vendor_prefixes",
    "custom_at_rules",
    "pseudo_classes",
    "pseudo_elements",
    "attribute_selectors",
    "logical_properties",
    "grid",
    "flexbox",
    "typed_property_groups",
];

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct NonJsonEvidenceReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub rows: Vec<NonJsonEvidenceRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13DecisionRegexReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub regex_fact_source: String,
    pub regex_fact_artifact_path: String,
    pub regex_fact_sha256: String,
    pub regex_fact_consumer_path: Vec<String>,
    pub generated_selection_path: String,
    pub hardcoded_regex_scan_status: String,
    pub feature_gate_status: String,
    pub cascade_fallback_status: String,
    pub row_move_toward_sota_status: String,
    pub block_id: Option<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13DecisionActiveCostReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub regex_fact_artifact_path: String,
    pub regex_fact_sha256: String,
    pub egraph_language_status: String,
    pub rewrite_set_id: String,
    pub egraph_node_count: u32,
    pub egraph_eclass_count: u32,
    pub egraph_iteration_count: u32,
    pub egraph_memory_peak_bytes: u64,
    pub egraph_budget_status: String,
    pub cost_function_source: String,
    pub cost_formula_version: String,
    pub candidate_total_count: u32,
    pub candidate_hard_pruned_count: u32,
    pub candidate_ranked_count: u32,
    pub candidate_stale_count: u32,
    pub candidate_cost_stale_rate: f64,
    pub selected_candidate_id: String,
    pub selected_rule_id: String,
    pub selected_shape: String,
    pub selected_cost_freshness: String,
    pub capacity_policy_cost_status: String,
    pub determinism_replay_status: String,
    pub rewrite_order_replay_count: u32,
    pub rewrite_order_variance_pct: f64,
    pub selection_trace_sha256: String,
    pub cost_facts_artifact_path: String,
    pub cost_facts_sha256: String,
    pub generated_selection_path: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub row_move_toward_sota_status: String,
    pub block_id: Option<String>,
    pub cascade_fallback_status: String,
    pub abrogate_status: String,
    pub material_differential: String,
    pub redress_entry: String,
    pub csp_solve_ms: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13DecisionCspCascadeReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub regex_fact_artifact_path: String,
    pub regex_fact_sha256: String,
    pub active_cost_artifact_path: String,
    pub active_cost_sha256: String,
    pub selection_trace_sha256: String,
    pub csp_problem_artifact_path: String,
    pub csp_problem_sha256: String,
    pub csp_solution_artifact_path: String,
    pub csp_solution_sha256: String,
    pub css_l4_witness_artifact_path: String,
    pub css_l4_witness_sha256: String,
    pub css_l4_witness_command: String,
    pub sheets_witness_artifact_path: String,
    pub sheets_witness_sha256: String,
    pub sheets_witness_command: String,
    pub bbnf_self_witness_artifact_path: String,
    pub bbnf_self_witness_sha256: String,
    pub bbnf_self_witness_command: String,
    pub scoped_witness_label: String,
    pub csp_solver_source: String,
    pub csp_solver_version: String,
    pub csp_status: String,
    pub csp_variable_count: u32,
    pub csp_constraint_count: u32,
    pub csp_objective_count: u32,
    pub csp_named_grammars: Vec<String>,
    pub csp_solve_ms: f64,
    pub csp_timeout_ms: u64,
    pub csp_node_budget: u64,
    pub csp_nodes_explored: u64,
    pub csp_budget_status: String,
    pub selected_rule_count: u32,
    pub selected_candidate_id: String,
    pub selected_shape: String,
    pub parity_constraint_status: String,
    pub recognizer_constraint_status: String,
    pub substrate_constraint_status: String,
    pub simd_constraint_status: String,
    pub capacity_constraint_status: String,
    pub resolver_output_piping: String,
    pub fused_solver_status: String,
    pub generated_selection_path: String,
    pub compile_consumer_path: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub cascade_retirement_status: String,
    pub choose_backend_shape_status: String,
    pub priority_table_status: String,
    pub p1_p8_fallback_status: String,
    pub legacy_cascade_admission_status: String,
    pub priority_data_role: String,
    pub priority_hard_prune_status: String,
    pub priority_objective_status: String,
    pub fallback_invoked: bool,
    pub compat_fallback_status: String,
    pub static_css_provider_status: String,
    pub json_sink_only_status: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub sheets_fail_closed_status: String,
    pub bbnf_self_fail_closed_status: String,
    pub lock14_status: String,
    pub generated_runtime_diff_status: String,
    pub generated_runtime_diff_artifact_path: String,
    pub generated_runtime_diff_sha256: String,
    pub row_move_toward_sota_status: String,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub abrogate_status: String,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13PerGrammarPolicyReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub json_consumer_row_id: String,
    pub json_consumer_path: String,
    pub css_consumer_row_id: String,
    pub css_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub generic_storage_status: String,
    pub public_grammar_config_status: String,
    pub generic_json_sink_acceleration_status: String,
    pub generic_json_policy_token_status: String,
    pub json_flag_semantics_owner: String,
    pub json_flag_physical_bit_status: String,
    pub css_policy_owner: String,
    pub css_policy_consumer_status: String,
    pub json_strict_equality_status: String,
    pub css_strict_equality_status: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub json_row_mbps_before: f64,
    pub json_row_mbps_after: f64,
    pub css_row_mbps_before: f64,
    pub css_row_mbps_after: f64,
    pub row_move_toward_sota_status: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub policy_artifact_path: String,
    pub policy_artifact_sha256: String,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13SameSubstrateUnionReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub union_variant_id: String,
    pub material_differential_status: String,
    pub prior_redress_citations: Vec<String>,
    pub substrate_cardinality: String,
    pub public_union_tape_status: String,
    pub public_substrate_api_status: String,
    pub backend_shape_expansion_status: String,
    pub bir_directive_expansion_status: String,
    pub class_column_status: String,
    pub retained_structural_index_status: String,
    pub sidecar_vector_status: String,
    pub second_scan_status: String,
    pub parser_owned_cursor_status: String,
    pub bbnf_simd_touch_status: String,
    pub css_consumer_row_id: String,
    pub css_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub css_strict_equality_status: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub css_row_mbps_before: f64,
    pub css_row_mbps_after: f64,
    pub lightningcss_mbps: f64,
    pub threshold_mbps: f64,
    pub row_move_toward_sota_status: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub union_artifact_path: String,
    pub union_artifact_sha256: String,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13JsonDirectReopenReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub row_id: String,
    pub corpus: String,
    pub workload: String,
    pub output_plane: String,
    pub route_id: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub strict_equality_status: String,
    pub track2_independence_status: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub track1_mbps_before: f64,
    pub track1_mbps_after: f64,
    pub track2_mbps_after: f64,
    pub sonic_strict_mbps_after: f64,
    pub serde_mbps_after: f64,
    pub threshold_mbps: f64,
    pub row_move_toward_sota_status: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub measurement_artifact_path: String,
    pub measurement_artifact_sha256: String,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub prior_redress_citations: Vec<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13JsonParseOnlyReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub row_id: String,
    pub corpus: String,
    pub workload: String,
    pub output_plane: String,
    pub route_id: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub strict_equality_status: String,
    pub strict_equality_artifact_path: String,
    pub strict_equality_artifact_sha256: String,
    pub track2_independence_status: String,
    pub measured_validation_path: String,
    pub parse_utf8: String,
    pub escape_complete: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub track1_mbps_before: f64,
    pub track1_mbps_after: f64,
    pub track2_mbps_after: f64,
    pub sonic_strict_mbps_after: f64,
    pub serde_mbps_after: f64,
    pub threshold_mbps: f64,
    pub admission_margin_mbps: f64,
    pub row_move_toward_sota_status: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub measurement_artifact_path: String,
    pub measurement_artifact_sha256: String,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub prior_redress_citations: Vec<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13TypedProductReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub row_id: String,
    pub corpus: String,
    pub workload: String,
    pub output_plane: String,
    pub route_id: String,
    pub same_wave_consumer_path: String,
    pub same_wave_consumer_class: String,
    pub strict_equality_status: String,
    pub track2_independence_status: String,
    pub oracle_model: String,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub track1_mbps_after: f64,
    pub track2_mbps_after: f64,
    pub sonic_strict_mbps_after: f64,
    pub serde_mbps_after: f64,
    pub threshold_mbps: f64,
    pub row_move_toward_sota_status: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub generated_size_status: String,
    pub measurement_artifact_path: String,
    pub measurement_artifact_sha256: String,
    pub affected_row_ids: Vec<String>,
    pub prior_redress_citations: Vec<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13SimdAsmProductionReport {
    pub schema_version: String,
    pub wave_id: String,
    pub run_id: String,
    pub source_commit: String,
    pub host_triple: String,
    pub build_flags: String,
    pub feature_mask: String,
    pub consumer_gate: String,
    pub g_omega_status: String,
    pub route_id: String,
    pub selected_primitive: String,
    pub primitive_source_paths: Vec<String>,
    pub scalar_reference_status: String,
    pub checkasm_status: String,
    pub checkasm_command: String,
    pub checkasm_artifact_path: String,
    pub checkasm_artifact_sha256: String,
    pub corpus_parity_status: String,
    pub consumer_row_id: String,
    pub consumer_runtime_path: String,
    pub consumer_bench_path: String,
    pub same_wave_consumer_class: String,
    pub production_consumer_status: String,
    pub track1_mbps_before: f64,
    pub track1_mbps_after: f64,
    pub lightningcss_mbps: f64,
    pub threshold_mbps: f64,
    pub criterion_delta_pct: f64,
    pub row_move_toward_sota_status: String,
    pub measurement_artifact_path: String,
    pub measurement_artifact_sha256: String,
    pub orphan_count_before: u32,
    pub orphan_count_after: u32,
    pub orphan_inventory_artifact_path: String,
    pub orphan_inventory_sha256: String,
    pub deleted_or_demoted_primitives: Vec<String>,
    pub json_guard_state: String,
    pub css_guard_state: String,
    pub lock14_status: String,
    pub lock14_owner_path_status: String,
    pub lock14_generic_scan_status: String,
    pub prior_redress_citations: Vec<String>,
    pub affected_row_ids: Vec<String>,
    pub block_id: Option<String>,
    pub material_differential: String,
    pub redress_entry: String,
}

impl SkV13DecisionActiveCostReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 decision-active-cost report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_DECISION_ACTIVE_COST_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 decision-active-cost schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W6" {
            return Err(format!("{} cannot claim W6 authority", self.wave_id));
        }
        if !self.run_id.starts_with("sk-v13-w6:") {
            return Err(format!("invalid W6 run id {}", self.run_id));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W6 report missing source/build provenance".into());
        }
        if self.consumer_gate != "G-W6-DECISION-ACTIVE-COST" || self.g_omega_status != "user-signed"
        {
            return Err("W6 consumer gate or G-Omega status invalid".into());
        }
        if self.regex_fact_artifact_path.trim().is_empty()
            || self.cost_facts_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.regex_fact_sha256)
            || !is_hex_sha256(&self.cost_facts_sha256)
            || !is_hex_sha256(&self.selection_trace_sha256)
        {
            return Err("W6 artifact path/hash invalid".into());
        }
        if self.egraph_language_status != "pass"
            || self.egraph_budget_status != "pass"
            || self.egraph_node_count == 0
            || self.egraph_node_count > 100_000
            || self.egraph_eclass_count == 0
            || self.egraph_iteration_count == 0
            || self.egraph_iteration_count > 100
            || self.egraph_memory_peak_bytes >= (1u64 << 30)
        {
            return Err("W6 egraph bounds invalid".into());
        }
        if self.cost_function_source != "passes::backend_egraph::DecisionCostModel"
            || self.cost_formula_version.trim().is_empty()
            || self.candidate_total_count == 0
            || self.candidate_ranked_count == 0
            || self.candidate_stale_count * 10 > self.candidate_ranked_count * 3
            || self.candidate_cost_stale_rate > 0.30
            || self.selected_candidate_id.trim().is_empty()
            || self.selected_rule_id.trim().is_empty()
            || self.selected_shape.trim().is_empty()
            || self.selected_cost_freshness != "fresh"
        {
            return Err("W6 active cost evidence invalid".into());
        }
        if self.determinism_replay_status != "pass"
            || self.rewrite_order_replay_count < 2
            || self.rewrite_order_variance_pct > 10.0
        {
            return Err("W6 determinism or rewrite-order evidence invalid".into());
        }
        for value in [
            self.generated_selection_path.as_str(),
            self.same_wave_consumer_path.as_str(),
            self.same_wave_consumer_class.as_str(),
        ] {
            if value.trim().is_empty()
                || matches!(
                    value,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err("W6 generated selection path is paper-close".into());
            }
        }
        if self.same_wave_consumer_class != "gate_json_decision_active_cost_contract"
            || self.cascade_fallback_status != "fail-closed"
            || self.abrogate_status != "not-triggered"
            || self.csp_solve_ms != "n/a:w6-before-csp"
        {
            return Err("W6 consumer/cascade/abrogate/CSP status invalid".into());
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {}
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME")
                {
                    return Err("W6 measured block id missing".into());
                }
            }
            other => return Err(format!("W6 row movement status {other} is rejected")),
        }
        if self.material_differential.trim().is_empty() || self.redress_entry.trim().is_empty() {
            return Err("W6 report missing material differential or REDRESS entry".into());
        }
        Ok(())
    }
}

impl SkV13DecisionRegexReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 decision-regex report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_DECISION_REGEX_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 decision-regex schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W5" {
            return Err(format!("{} cannot claim W5 authority", self.wave_id));
        }
        if !self.run_id.starts_with("sk-v13-w5:") {
            return Err(format!("invalid W5 run id {}", self.run_id));
        }
        if self.regex_fact_source != "bbnf-regex::analyze" {
            return Err("W5 regex fact source must be bbnf-regex::analyze".into());
        }
        if self.regex_fact_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.regex_fact_sha256)
        {
            return Err("W5 regex fact artifact path/hash invalid".into());
        }
        for required in ["ir::nullability", "passes::recognizers", "passes::extract"] {
            if !self
                .regex_fact_consumer_path
                .iter()
                .any(|path| path.contains(required))
            {
                return Err(format!("W5 report missing consumer path {required}"));
            }
        }
        if self.generated_selection_path.trim().is_empty()
            || self.generated_selection_path == "support_only"
            || self.generated_selection_path == "gate_only"
        {
            return Err("W5 generated selection path is paper-close".into());
        }
        if self.hardcoded_regex_scan_status != "no-hardcoded-json-patterns" {
            return Err("W5 hardcoded regex scan did not pass".into());
        }
        if self.feature_gate_status != "pass" || self.cascade_fallback_status != "fail-closed" {
            return Err("W5 feature gate or cascade fallback status invalid".into());
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {}
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH")
                {
                    return Err("W5 measured block id missing".into());
                }
            }
            other => return Err(format!("W5 row movement status {other} is rejected")),
        }
        if self.material_differential.trim().is_empty() || self.redress_entry.trim().is_empty() {
            return Err("W5 report missing material differential or REDRESS entry".into());
        }
        Ok(())
    }
}

impl SkV13DecisionCspCascadeReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 decision-CSP-cascade report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_DECISION_CSP_CASCADE_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 decision-CSP-cascade schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W7" {
            return Err(format!("{} cannot claim W7 authority", self.wave_id));
        }
        if !self.run_id.starts_with("sk-v13-w7:") {
            return Err(format!("invalid W7 run id {}", self.run_id));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W7 report missing source/build provenance".into());
        }
        if self.consumer_gate != "G-W7-DECISION-CSP-CASCADE" || self.g_omega_status != "user-signed"
        {
            return Err("W7 consumer gate or G-Omega status invalid".into());
        }
        if !is_hex_sha256(&self.selection_trace_sha256) {
            return Err("W7 selection trace hash invalid".into());
        }
        for (label, path, sha) in [
            (
                "W5 regex facts",
                self.regex_fact_artifact_path.as_str(),
                self.regex_fact_sha256.as_str(),
            ),
            (
                "W6 active-cost facts",
                self.active_cost_artifact_path.as_str(),
                self.active_cost_sha256.as_str(),
            ),
            (
                "W7 CSP problem",
                self.csp_problem_artifact_path.as_str(),
                self.csp_problem_sha256.as_str(),
            ),
            (
                "W7 CSP solution",
                self.csp_solution_artifact_path.as_str(),
                self.csp_solution_sha256.as_str(),
            ),
            (
                "W7 CSS L4 witness",
                self.css_l4_witness_artifact_path.as_str(),
                self.css_l4_witness_sha256.as_str(),
            ),
            (
                "W7 Sheets witness",
                self.sheets_witness_artifact_path.as_str(),
                self.sheets_witness_sha256.as_str(),
            ),
            (
                "W7 BBNF-self witness",
                self.bbnf_self_witness_artifact_path.as_str(),
                self.bbnf_self_witness_sha256.as_str(),
            ),
        ] {
            if path.trim().is_empty() || !is_hex_sha256(sha) {
                return Err(format!("{label} artifact path/hash invalid"));
            }
        }
        for (label, command) in [
            ("CSS L4", self.css_l4_witness_command.as_str()),
            ("Sheets", self.sheets_witness_command.as_str()),
            ("BBNF-self", self.bbnf_self_witness_command.as_str()),
        ] {
            if command.trim().is_empty()
                || matches!(command, "status-only" | "support_only" | "future")
            {
                return Err(format!("W7 {label} witness command is status-only"));
            }
        }
        if self.scoped_witness_label.trim().is_empty() {
            return Err("W7 scoped witness label missing".into());
        }
        if self.csp_solver_source != "csp_solver::Csp<CostFiniteDomain>"
            || self.csp_solver_version.trim().is_empty()
            || !matches!(self.csp_status.as_str(), "sat" | "unsat" | "timeout")
            || self.csp_variable_count == 0
            || self.csp_constraint_count < 6
            || self.csp_objective_count == 0
            || self.csp_named_grammars.is_empty()
            || !self.csp_solve_ms.is_finite()
            || self.csp_solve_ms > self.csp_timeout_ms as f64
            || self.csp_timeout_ms > 1_000
            || self.csp_node_budget == 0
            || self.csp_budget_status != "pass"
        {
            return Err("W7 CSP bounds invalid".into());
        }
        if self.selected_rule_count == 0
            || self.selected_candidate_id.trim().is_empty()
            || self.selected_shape.trim().is_empty()
        {
            return Err("W7 selected rule/candidate evidence invalid".into());
        }
        for (label, status) in [
            ("parity", self.parity_constraint_status.as_str()),
            ("recognizer", self.recognizer_constraint_status.as_str()),
            ("substrate", self.substrate_constraint_status.as_str()),
            ("simd", self.simd_constraint_status.as_str()),
            ("capacity", self.capacity_constraint_status.as_str()),
        ] {
            if status != "pass" {
                return Err(format!("W7 {label} constraint did not pass"));
            }
        }
        if self.resolver_output_piping != "regex_facts->egraph_active_cost->csp->compile_codegen"
            || self.fused_solver_status != "not-fused"
            || self.generated_selection_path.trim().is_empty()
            || self.compile_consumer_path.trim().is_empty()
            || self.same_wave_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class != "gate_json_decision_csp_cascade_contract"
        {
            return Err("W7 resolver/consumer path invalid".into());
        }
        if self.cascade_retirement_status != "fail_closed"
            || self.choose_backend_shape_status != "csp-finalized"
            || self.priority_table_status != "evidence-only"
            || self.p1_p8_fallback_status != "non-admission"
            || self.legacy_cascade_admission_status != "blocked"
            || self.priority_data_role != "evidence-only"
            || self.priority_hard_prune_status != "not-used"
            || self.priority_objective_status != "not-used"
            || self.fallback_invoked
            || self.compat_fallback_status != "not-invoked"
        {
            return Err("W7 cascade retirement or fallback status invalid".into());
        }
        if self.static_css_provider_status.trim().is_empty()
            || self.json_sink_only_status.trim().is_empty()
            || self.static_css_provider_status == "pass"
            || self.json_sink_only_status == "pass"
        {
            return Err("W7 static provider blocker evidence invalid".into());
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
            (
                "Sheets fail-closed",
                self.sheets_fail_closed_status.as_str(),
            ),
            (
                "BBNF-self fail-closed",
                self.bbnf_self_fail_closed_status.as_str(),
            ),
            ("Lock 14", self.lock14_status.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("W7 {label} status invalid"));
            }
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {
                if self.generated_runtime_diff_status != "present"
                    || self.generated_runtime_diff_artifact_path.trim().is_empty()
                    || !is_hex_sha256(&self.generated_runtime_diff_sha256)
                {
                    return Err("W7 admitted row requires generated runtime diff artifact".into());
                }
            }
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT")
                    || self.generated_runtime_diff_status != "absent"
                    || !self.generated_runtime_diff_artifact_path.trim().is_empty()
                    || !self.generated_runtime_diff_sha256.trim().is_empty()
                {
                    return Err("W7 measured block/diff status invalid".into());
                }
            }
            other => return Err(format!("W7 row movement status {other} is rejected")),
        }
        if self.affected_row_ids.is_empty()
            || self.abrogate_status.trim().is_empty()
            || self.material_differential.trim().is_empty()
            || self.redress_entry.trim().is_empty()
        {
            return Err("W7 report missing material differential or REDRESS entry".into());
        }
        Ok(())
    }
}

impl SkV13PerGrammarPolicyReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 per-grammar-policy report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_PER_GRAMMAR_POLICY_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 per-grammar-policy schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W8" {
            return Err(format!("{} cannot claim W8 authority", self.wave_id));
        }
        if !self.run_id.starts_with("sk-v13-w8:") {
            return Err(format!("invalid W8 run id {}", self.run_id));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W8 report missing source/build provenance".into());
        }
        if self.consumer_gate != "G-W8-PER-GRAMMAR-POLICY" || self.g_omega_status != "user-signed" {
            return Err("W8 consumer gate or G-Omega status invalid".into());
        }
        if self.json_consumer_row_id != "json/y_string_unicode/direct_to_struct/main"
            || self.css_consumer_row_id
                != "css_l4/declaration_values_extended/direct_to_struct/main"
            || self.json_consumer_path.trim().is_empty()
            || self.css_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class != "generated_json_and_css_policy_rows"
        {
            return Err("W8 same-wave consumer evidence invalid".into());
        }
        for (label, actual, expected) in [
            (
                "generic storage",
                self.generic_storage_status.as_str(),
                "stable",
            ),
            (
                "public GrammarConfig",
                self.public_grammar_config_status.as_str(),
                "absent",
            ),
            (
                "generic JsonSink acceleration",
                self.generic_json_sink_acceleration_status.as_str(),
                "absent",
            ),
            (
                "generic JSON policy token",
                self.generic_json_policy_token_status.as_str(),
                "absent",
            ),
            (
                "JSON flag owner",
                self.json_flag_semantics_owner.as_str(),
                "generated_json_config",
            ),
            (
                "JSON flag physical bit",
                self.json_flag_physical_bit_status.as_str(),
                "preserved",
            ),
            (
                "CSS policy owner",
                self.css_policy_owner.as_str(),
                "generated_css_config",
            ),
            (
                "CSS policy consumer",
                self.css_policy_consumer_status.as_str(),
                "generated_scanner_and_sink",
            ),
            (
                "JSON strict equality",
                self.json_strict_equality_status.as_str(),
                "pass",
            ),
            (
                "CSS strict equality",
                self.css_strict_equality_status.as_str(),
                "pass",
            ),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!("W8 {label} status {actual} != {expected}"));
            }
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("W8 {label} status invalid"));
            }
        }
        for (label, value) in [
            ("JSON Mbps before", self.json_row_mbps_before),
            ("JSON Mbps after", self.json_row_mbps_after),
            ("CSS Mbps before", self.css_row_mbps_before),
            ("CSS Mbps after", self.css_row_mbps_after),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(format!("W8 {label} invalid"));
            }
        }
        if self.policy_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.policy_artifact_sha256)
        {
            return Err("W8 policy artifact path/hash invalid".into());
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {
                if self.json_row_mbps_after <= self.json_row_mbps_before
                    && self.css_row_mbps_after <= self.css_row_mbps_before
                {
                    return Err("W8 pass/admitted requires row movement".into());
                }
            }
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT")
                {
                    return Err("W8 measured block id missing".into());
                }
            }
            other => return Err(format!("W8 row movement status {other} is rejected")),
        }
        if self.affected_row_ids.len() < 2
            || self.material_differential.trim().is_empty()
            || self.redress_entry.trim().is_empty()
        {
            return Err(
                "W8 report missing affected rows, material differential, or REDRESS entry".into(),
            );
        }
        Ok(())
    }
}

impl SkV13SameSubstrateUnionReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 same-substrate-union report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_SAME_SUBSTRATE_UNION_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 same-substrate-union schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W9" {
            return Err(format!("{} cannot claim W9 authority", self.wave_id));
        }
        if !self.run_id.starts_with("sk-v13-w9:") {
            return Err(format!("invalid W9 run id {}", self.run_id));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W9 report missing source/build provenance".into());
        }
        if self.consumer_gate != "G-W9-SAME-SUBSTRATE-UNION" || self.g_omega_status != "user-signed"
        {
            return Err("W9 consumer gate or G-Omega status invalid".into());
        }
        if self.union_variant_id != "union-c1-per-rule-same-tape"
            || self.material_differential_status != "accepted"
            || !["96", "97", "98"]
                .iter()
                .all(|id| self.prior_redress_citations.iter().any(|entry| entry == id))
        {
            return Err("W9 material differential evidence invalid".into());
        }
        for (label, actual, expected) in [
            (
                "substrate cardinality",
                self.substrate_cardinality.as_str(),
                "one",
            ),
            (
                "public UnionTape",
                self.public_union_tape_status.as_str(),
                "absent",
            ),
            (
                "public substrate API",
                self.public_substrate_api_status.as_str(),
                "absent",
            ),
            (
                "BackendShape expansion",
                self.backend_shape_expansion_status.as_str(),
                "absent",
            ),
            (
                "BIR/directive expansion",
                self.bir_directive_expansion_status.as_str(),
                "absent",
            ),
            ("class column", self.class_column_status.as_str(), "absent"),
            (
                "retained structural index",
                self.retained_structural_index_status.as_str(),
                "absent",
            ),
            (
                "sidecar vector",
                self.sidecar_vector_status.as_str(),
                "absent",
            ),
            ("second scan", self.second_scan_status.as_str(), "absent"),
            (
                "parser-owned cursor",
                self.parser_owned_cursor_status.as_str(),
                "absent",
            ),
            (
                "bbnf-simd touch",
                self.bbnf_simd_touch_status.as_str(),
                "read-only",
            ),
            (
                "CSS strict equality",
                self.css_strict_equality_status.as_str(),
                "pass",
            ),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!("W9 {label} status {actual} != {expected}"));
            }
        }
        if self.css_consumer_row_id != "css_l4/declaration_values_extended/direct_to_struct/main"
            || self.css_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class
                != "generated_css_decl_values_extended_same_substrate_projection"
        {
            return Err("W9 same-wave consumer evidence invalid".into());
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("W9 {label} status invalid"));
            }
        }
        for (label, value) in [
            ("CSS Mbps before", self.css_row_mbps_before),
            ("CSS Mbps after", self.css_row_mbps_after),
            ("lightningcss Mbps", self.lightningcss_mbps),
            ("threshold Mbps", self.threshold_mbps),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(format!("W9 {label} invalid"));
            }
        }
        if self.union_artifact_path.trim().is_empty() || !is_hex_sha256(&self.union_artifact_sha256)
        {
            return Err("W9 union artifact path/hash invalid".into());
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {
                if self.css_row_mbps_after <= self.css_row_mbps_before
                    || self.css_row_mbps_after <= self.threshold_mbps
                {
                    return Err("W9 pass/admitted requires row movement and SOTA pass".into());
                }
                if self.block_id.is_some() {
                    return Err("W9 pass/admitted cannot carry a block id".into());
                }
            }
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("JSON-CSS-W9-SAME-SUBSTRATE-UNION-CONSUMED-BUT-NO-ROW-MOVEMENT")
                {
                    return Err("W9 measured block id missing".into());
                }
            }
            other => return Err(format!("W9 row movement status {other} is rejected")),
        }
        if self.affected_row_ids.is_empty()
            || self.material_differential.trim().is_empty()
            || self.redress_entry.trim().is_empty()
        {
            return Err(
                "W9 report missing affected rows, material differential, or REDRESS entry".into(),
            );
        }
        Ok(())
    }
}

impl SkV13JsonDirectReopenReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 JSON direct reopen report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        let spec = skv13_json_direct_reopen_gate_spec(&self.wave_id)?;
        if self.schema_version != SKV13_JSON_DIRECT_REOPEN_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 JSON direct reopen schema {}",
                self.schema_version
            ));
        }
        if !self.run_id.starts_with(spec.run_id_prefix) {
            return Err(format!("invalid {} run id {}", spec.label, self.run_id));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err(format!(
                "{} report missing source/build provenance",
                spec.label
            ));
        }
        if self.consumer_gate != spec.consumer_gate || self.g_omega_status != "user-signed" {
            return Err(format!(
                "{} consumer gate or G-Omega status invalid",
                spec.label
            ));
        }
        if self.row_id != spec.row_id
            || self.corpus != spec.corpus
            || self.workload != "direct_to_struct"
            || self.output_plane != "digest"
        {
            return Err(format!("{} row identity invalid", spec.label));
        }
        if self.route_id != spec.route_id
            || self.same_wave_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class != spec.consumer_class
        {
            return Err(format!(
                "{} same-wave consumer evidence invalid",
                spec.label
            ));
        }
        for (label, actual, expected) in [
            (
                "strict equality",
                self.strict_equality_status.as_str(),
                "pass",
            ),
            (
                "Track 2 independence",
                self.track2_independence_status.as_str(),
                "independent",
            ),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!(
                    "{} {label} status {actual} != {expected}",
                    spec.label
                ));
            }
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("{} {label} status invalid", spec.label));
            }
        }
        for (label, value) in [
            ("Track 1 before", self.track1_mbps_before),
            ("Track 1 after", self.track1_mbps_after),
            ("Track 2 after", self.track2_mbps_after),
            ("sonic strict after", self.sonic_strict_mbps_after),
            ("serde after", self.serde_mbps_after),
            ("threshold", self.threshold_mbps),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(format!("{} {label} invalid", spec.label));
            }
        }
        if (self.threshold_mbps - (self.sonic_strict_mbps_after + 1.0)).abs() > 0.01 {
            return Err(format!(
                "{} threshold must equal sonic strict + 1 Mbps",
                spec.label
            ));
        }
        if self.measurement_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.measurement_artifact_sha256)
        {
            return Err(format!(
                "{} measurement artifact path/hash invalid",
                spec.label
            ));
        }
        if !spec
            .required_redress
            .iter()
            .all(|id| self.prior_redress_citations.iter().any(|entry| entry == id))
        {
            return Err(format!("{} prior REDRESS citations missing", spec.label));
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {
                if self.track1_mbps_after <= self.track1_mbps_before
                    || self.track1_mbps_after <= self.threshold_mbps
                {
                    return Err(format!(
                        "{} pass/admitted requires row movement and SOTA pass",
                        spec.label
                    ));
                }
                if self.block_id.is_some() {
                    return Err(format!(
                        "{} pass/admitted cannot carry a block id",
                        spec.label
                    ));
                }
            }
            "measured_architectural_block" => {
                if self.block_id.as_deref() != Some(spec.architectural_block_id) {
                    return Err(format!("{} measured block id missing", spec.label));
                }
            }
            other => {
                return Err(format!(
                    "{} row movement status {other} is rejected",
                    spec.label
                ))
            }
        }
        if self.affected_row_ids.len() != 1
            || self.affected_row_ids[0] != spec.row_id
            || self.material_differential.trim().is_empty()
            || self.redress_entry.trim().is_empty()
        {
            return Err(format!(
                "{} report missing affected rows, material differential, or REDRESS entry",
                spec.label
            ));
        }
        Ok(())
    }
}

impl SkV13JsonParseOnlyReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 JSON parse-only report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_JSON_PARSE_ONLY_REPORT_SCHEMA
            && self.schema_version != SKV14_JSON_PARSE_ONLY_REPORT_SCHEMA
        {
            return Err(format!(
                "unsupported JSON parse-only schema {}",
                self.schema_version
            ));
        }
        let spec = json_parse_only_admission_spec_for_report(self)?;
        if self.wave_id != spec.wave_id || !self.run_id.starts_with(spec.run_id_prefix) {
            return Err(format!(
                "{} invalid JSON parse-only identity {}/{}",
                spec.label, self.wave_id, self.run_id
            ));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err(format!(
                "{} JSON parse-only report missing source/build provenance",
                spec.label
            ));
        }
        if self.consumer_gate != spec.consumer_gate
            || !matches!(
                self.g_omega_status.as_str(),
                "user-signed" | "not-applicable:wave-implementation"
            )
            || self.row_id != spec.row_id
            || self.corpus != spec.corpus
            || self.workload != "parse_only"
            || self.output_plane != "parse_only"
        {
            return Err(format!(
                "{} JSON parse-only row identity invalid",
                spec.label
            ));
        }
        if self.route_id != spec.route_id
            || self.same_wave_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class != "generated_json_parse_only_contract"
        {
            return Err(format!(
                "{} JSON parse-only consumer evidence invalid",
                spec.label
            ));
        }
        for (label, actual, expected) in [
            (
                "strict equality",
                self.strict_equality_status.as_str(),
                "pass",
            ),
            (
                "Track 2 independence",
                self.track2_independence_status.as_str(),
                "independent",
            ),
            (
                "measured validation",
                self.measured_validation_path.as_str(),
                "measured-row",
            ),
            ("parse UTF-8", self.parse_utf8.as_str(), "measured-row"),
            ("escape completeness", self.escape_complete.as_str(), "yes"),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!(
                    "{} JSON parse-only {label} status {actual} != {expected}",
                    spec.label
                ));
            }
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!(
                    "{} JSON parse-only {label} status invalid",
                    spec.label
                ));
            }
        }
        for (label, value) in [
            ("Track 1 before", self.track1_mbps_before),
            ("Track 1 after", self.track1_mbps_after),
            ("Track 2 after", self.track2_mbps_after),
            ("sonic strict after", self.sonic_strict_mbps_after),
            ("serde after", self.serde_mbps_after),
            ("threshold", self.threshold_mbps),
            ("admission margin", self.admission_margin_mbps),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(format!("{} JSON parse-only {label} invalid", spec.label));
            }
        }
        if (self.threshold_mbps - (self.sonic_strict_mbps_after + 1.0)).abs() > 0.01 {
            return Err(format!(
                "{} JSON parse-only threshold must equal sonic strict + 1 Mbps",
                spec.label
            ));
        }
        if (self.admission_margin_mbps - (self.track1_mbps_after - self.threshold_mbps)).abs()
            > 0.01
        {
            return Err(format!(
                "{} JSON parse-only admission margin mismatch",
                spec.label
            ));
        }
        if self.track1_mbps_after <= self.threshold_mbps
            || !matches!(
                self.row_move_toward_sota_status.as_str(),
                "pass" | "admitted"
            )
            || self.block_id.is_some()
        {
            return Err(format!(
                "{} JSON parse-only did not admit over sonic + 1",
                spec.label
            ));
        }
        for (label, path, sha) in [
            (
                "strict equality artifact",
                self.strict_equality_artifact_path.as_str(),
                self.strict_equality_artifact_sha256.as_str(),
            ),
            (
                "measurement artifact",
                self.measurement_artifact_path.as_str(),
                self.measurement_artifact_sha256.as_str(),
            ),
        ] {
            if path.trim().is_empty() || !is_hex_sha256(sha) {
                return Err(format!(
                    "{} JSON parse-only {label} path/hash invalid",
                    spec.label
                ));
            }
        }
        if self.affected_row_ids.len() != 1
            || self.affected_row_ids[0] != spec.row_id
            || self.material_differential.trim().is_empty()
            || self.redress_entry != spec.redress_entry
            || !self
                .prior_redress_citations
                .iter()
                .any(|entry| entry == spec.prior_redress_citation)
        {
            return Err(format!(
                "{} JSON parse-only report missing rows, REDRESS citation, or differential",
                spec.label
            ));
        }
        Ok(())
    }
}

impl SkV13TypedProductReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 typed-product report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_TYPED_PRODUCT_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 typed-product schema {}",
                self.schema_version
            ));
        }
        let (
            expected_wave,
            expected_run_prefix,
            expected_gate,
            expected_corpus,
            expected_route,
            expected_redress,
        ) = match self.row_id.as_str() {
            "json/numbers/real_typed_struct/main" => (
                "SK-V13-W13.1",
                "sk-v13-w13.1:",
                "G-W13.1-TYPED-NUMBERS",
                "numbers",
                "generated-real-typed-numeric-array",
                "REDRESS-145",
            ),
            "json/unicode_basic/real_typed_struct/main" => (
                "SK-V13-W13.2",
                "sk-v13-w13.2:",
                "G-W13.2-TYPED-UNICODE-BASIC",
                "unicode_basic",
                "generated-real-typed-unicode-basic-record-array",
                "REDRESS-146",
            ),
            "json/random/real_typed_struct/main" => (
                "SK-V13-W13.3",
                "sk-v13-w13.3:",
                "G-W13.3-TYPED-RANDOM",
                "random",
                "generated-real-typed-random-document",
                "REDRESS-147",
            ),
            "json/instruments/real_typed_struct/main" => (
                "SK-V13-W13.4",
                "sk-v13-w13.4:",
                "G-W13.4-TYPED-INSTRUMENTS",
                "instruments",
                "generated-real-typed-instruments-document",
                "REDRESS-148",
            ),
            "json/update_center/real_typed_struct/main" => (
                "SK-V13-W15.1",
                "sk-v13-w15.1:",
                "G-W15.1-JSON-TYPED-UPDATE-CENTER-PLUGIN",
                "update_center",
                "generated-real-typed-update-center-plugin-fast-path",
                "REDRESS-160",
            ),
            _ => return Err(format!("unsupported W13 typed-product row {}", self.row_id)),
        };
        if self.wave_id != expected_wave || !self.run_id.starts_with(expected_run_prefix) {
            return Err(format!(
                "invalid W13 typed-product identity {}/{}",
                self.wave_id, self.run_id
            ));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W13 typed-product report missing source/build provenance".to_string());
        }
        if self.consumer_gate != expected_gate
            || self.g_omega_status != "user-signed"
            || self.corpus != expected_corpus
            || self.workload != "real_typed_struct"
            || self.output_plane != "typed direct"
        {
            return Err("W13 typed-product row identity invalid".to_string());
        }
        if self.route_id != expected_route
            || self.same_wave_consumer_path.trim().is_empty()
            || self.same_wave_consumer_class != "gate_json_typed_contract"
            || self.oracle_model != "serde-track2-plus-sonic-strict"
        {
            return Err("W13 typed-product consumer/oracle evidence invalid".to_string());
        }
        for (label, actual, expected) in [
            (
                "strict equality",
                self.strict_equality_status.as_str(),
                "pass",
            ),
            (
                "Track 2 independence",
                self.track2_independence_status.as_str(),
                "independent",
            ),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
            (
                "generated size",
                self.generated_size_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!(
                    "W13 typed-product {label} status {actual} != {expected}"
                ));
            }
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("W13 typed-product {label} status invalid"));
            }
        }
        for (label, value) in [
            ("Track 1 after", self.track1_mbps_after),
            ("Track 2 after", self.track2_mbps_after),
            ("sonic strict after", self.sonic_strict_mbps_after),
            ("serde after", self.serde_mbps_after),
            ("threshold", self.threshold_mbps),
        ] {
            if !value.is_finite() || value < 0.0 {
                return Err(format!("W13 typed-product {label} invalid"));
            }
        }
        if (self.threshold_mbps - (self.sonic_strict_mbps_after + 1.0)).abs() > 0.01 {
            return Err("W13 typed-product threshold must equal sonic strict + 1 Mbps".to_string());
        }
        if self.track1_mbps_after <= self.threshold_mbps
            || !matches!(
                self.row_move_toward_sota_status.as_str(),
                "pass" | "admitted"
            )
        {
            return Err("W13 typed-product did not admit over sonic + 1".to_string());
        }
        if self.measurement_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.measurement_artifact_sha256)
        {
            return Err("W13 typed-product measurement artifact path/hash invalid".to_string());
        }
        if self.affected_row_ids.len() != 1
            || self.affected_row_ids[0] != self.row_id
            || self.material_differential.trim().is_empty()
            || self.redress_entry != expected_redress
            || !expected_prior_typed_redress(self.row_id.as_str())
                .iter()
                .all(|id| self.prior_redress_citations.iter().any(|entry| entry == id))
        {
            return Err(
                "W13 typed-product report missing rows, REDRESS citations, or differential"
                    .to_string(),
            );
        }
        Ok(())
    }
}

fn expected_prior_typed_redress(row_id: &str) -> &'static [&'static str] {
    match row_id {
        "json/update_center/real_typed_struct/main" => {
            &["70", "103", "105", "110", "119", "120", "143", "159"]
        }
        _ => &["70", "103", "105", "110"],
    }
}

impl SkV13SimdAsmProductionReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 SIMD/ASM production report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_version != SKV13_SIMD_ASM_PRODUCTION_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 SIMD/ASM production schema {}",
                self.schema_version
            ));
        }
        if self.wave_id != "SK-V13-W12" || !self.run_id.starts_with("sk-v13-w12:") {
            return Err(format!(
                "{} run {} cannot claim W12 authority",
                self.wave_id, self.run_id
            ));
        }
        if self.source_commit.trim().is_empty()
            || self.host_triple.trim().is_empty()
            || self.build_flags.trim().is_empty()
            || self.feature_mask.trim().is_empty()
        {
            return Err("W12 report missing source/build provenance".into());
        }
        if self.consumer_gate != "G-W12-SIMD-ASM-PRODUCTION" || self.g_omega_status != "user-signed"
        {
            return Err("W12 consumer gate or G-Omega status invalid".into());
        }
        if self.route_id != "css-delimiter-ascii-set-member64"
            || self.selected_primitive != "bbnf_simd::find_ascii_set_member64"
            || !self
                .primitive_source_paths
                .iter()
                .any(|path| path == "crates/bbnf-simd/src/lib.rs")
            || !self
                .primitive_source_paths
                .iter()
                .any(|path| path == "crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs")
        {
            return Err("W12 primitive route evidence invalid".into());
        }
        for (label, actual, expected) in [
            (
                "scalar reference",
                self.scalar_reference_status.as_str(),
                "pass",
            ),
            ("checkasm", self.checkasm_status.as_str(), "pass"),
            ("corpus parity", self.corpus_parity_status.as_str(), "pass"),
            (
                "production consumer",
                self.production_consumer_status.as_str(),
                "wired",
            ),
            ("Lock 14", self.lock14_status.as_str(), "pass"),
            (
                "Lock 14 owner path",
                self.lock14_owner_path_status.as_str(),
                "pass",
            ),
            (
                "Lock 14 generic scan",
                self.lock14_generic_scan_status.as_str(),
                "pass",
            ),
        ] {
            if actual != expected {
                return Err(format!("W12 {label} status {actual} != {expected}"));
            }
        }
        if self.checkasm_command.trim().is_empty()
            || matches!(
                self.checkasm_command.as_str(),
                "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
            )
            || self.checkasm_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.checkasm_artifact_sha256)
            || self.measurement_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.measurement_artifact_sha256)
            || self.orphan_inventory_artifact_path.trim().is_empty()
            || !is_hex_sha256(&self.orphan_inventory_sha256)
        {
            return Err("W12 artifact path/hash evidence invalid".into());
        }
        if self.consumer_row_id != "css_l4/declaration_values/direct_to_struct/main"
            || !self
                .consumer_runtime_path
                .contains("generated_css_l4_declaration_values")
            || !self.consumer_bench_path.contains("nonjson_css_l4")
            || self.same_wave_consumer_class != "generated_css_l4_declaration_values_scan_block"
        {
            return Err("W12 same-wave production consumer evidence invalid".into());
        }
        for (label, status) in [
            ("JSON guard", self.json_guard_state.as_str()),
            ("CSS guard", self.css_guard_state.as_str()),
        ] {
            if status.trim().is_empty()
                || matches!(
                    status,
                    "support_only" | "gate_only" | "telemetry_only" | "future_consumer"
                )
            {
                return Err(format!("W12 {label} status invalid"));
            }
        }
        for (label, value) in [
            ("Track 1 before", self.track1_mbps_before),
            ("Track 1 after", self.track1_mbps_after),
            ("lightningcss", self.lightningcss_mbps),
            ("threshold", self.threshold_mbps),
            ("Criterion delta", self.criterion_delta_pct),
        ] {
            if !value.is_finite() {
                return Err(format!("W12 {label} invalid"));
            }
        }
        if self.track1_mbps_before <= 0.0
            || self.track1_mbps_after <= 0.0
            || self.lightningcss_mbps <= 0.0
            || self.threshold_mbps <= 0.0
            || (self.threshold_mbps - (self.lightningcss_mbps + 1.0)).abs() > 0.01
        {
            return Err("W12 throughput threshold evidence invalid".into());
        }
        if self.orphan_count_after != 0 {
            return Err("W12 leaves aarch64 orphan primitives".into());
        }
        if !["88", "89", "90", "122", "126"]
            .iter()
            .all(|id| self.prior_redress_citations.iter().any(|entry| entry == id))
        {
            return Err("W12 prior REDRESS citations missing".into());
        }
        match self.row_move_toward_sota_status.as_str() {
            "pass" | "admitted" => {
                if self.track1_mbps_after <= self.track1_mbps_before
                    || self.track1_mbps_after <= self.threshold_mbps
                    || self.criterion_delta_pct <= 0.0
                    || self.block_id.is_some()
                {
                    return Err("W12 admitted report requires row movement and no block id".into());
                }
            }
            "measured_architectural_block" => {
                if self.block_id.as_deref()
                    != Some("CSS-W12-SIMD-PRODUCTION-CONSUMED-BUT-NO-ROW-MOVEMENT")
                {
                    return Err("W12 measured block id missing".into());
                }
            }
            other => return Err(format!("W12 row movement status {other} is rejected")),
        }
        if self.affected_row_ids.len() != 1
            || self.affected_row_ids[0] != "css_l4/declaration_values/direct_to_struct/main"
            || self.material_differential.trim().is_empty()
            || self.redress_entry.trim().is_empty()
        {
            return Err(
                "W12 report missing affected row, material differential, or REDRESS entry".into(),
            );
        }
        Ok(())
    }
}

struct SkV13JsonDirectReopenGateSpec {
    label: &'static str,
    run_id_prefix: &'static str,
    consumer_gate: &'static str,
    row_id: &'static str,
    corpus: &'static str,
    route_id: &'static str,
    consumer_class: &'static str,
    architectural_block_id: &'static str,
    required_redress: &'static [&'static str],
}

fn skv13_json_direct_reopen_gate_spec(
    wave_id: &str,
) -> Result<SkV13JsonDirectReopenGateSpec, String> {
    match wave_id {
        "SK-V13-W11.1" => Ok(SkV13JsonDirectReopenGateSpec {
            label: "W11.1",
            run_id_prefix: "sk-v13-w11.1:",
            consumer_gate: "G-W11.1-JSON-DIRECT-NUMBERS",
            row_id: "json/numbers/direct_to_struct/main",
            corpus: "numbers",
            route_id: "generated-json-direct-numeric-array-dispatch",
            consumer_class: "generated_json_direct_numeric_array_dispatch",
            architectural_block_id: "JSON-W11-1-NUMBERS-DIRECT-NUMERIC-DISPATCH-INTRINSIC-BLOCK",
            required_redress: &["119", "120"],
        }),
        "SK-V13-W11.3" => Ok(SkV13JsonDirectReopenGateSpec {
            label: "W11.3",
            run_id_prefix: "sk-v13-w11.3:",
            consumer_gate: "G-W11.3-JSON-DIRECT-SINK-STACK",
            row_id: "json/mesh/direct_to_struct/main",
            corpus: "mesh",
            route_id: "direct-sink-stack-specialization",
            consumer_class: "direct_sink_stack_specialization",
            architectural_block_id: "JSON-W11-3-MESH-DIRECT-SINK-STACK-INTRINSIC-BLOCK",
            required_redress: &["119", "120", "142"],
        }),
        other => Err(format!(
            "{other} cannot claim SK-V13 JSON direct reopen authority"
        )),
    }
}

fn is_hex_sha256(value: &str) -> bool {
    value.len() == 64 && value.as_bytes().iter().all(u8::is_ascii_hexdigit)
}

impl NonJsonEvidenceReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text).map_err(|error| format!("invalid W1a non-JSON report: {error}"))
    }

    pub fn validate_w1a_non_json_gate(&self) -> Result<(), String> {
        if self.schema_version != W1A_NON_JSON_REPORT_SCHEMA {
            return Err(format!("unsupported W1a schema {}", self.schema_version));
        }
        if self.wave_id != "SK-V11-W1a" {
            return Err(format!("{} cannot claim W1a authority", self.wave_id));
        }
        if !is_w1a_run_id(&self.run_id) {
            return Err(format!("invalid W1a run id {}", self.run_id));
        }
        if self.rows.is_empty() {
            return Err("W1a non-JSON report has no rows".to_string());
        }
        let mut seen = BTreeSet::new();
        for row in &self.rows {
            if row.sk_v8.wave_id != self.wave_id || row.sk_v8.run_id != self.run_id {
                return Err(format!(
                    "{} does not match report identity",
                    row.sk_v8.row_id
                ));
            }
            if !seen.insert(row.sk_v8.row_id.as_str()) {
                return Err(format!("duplicate W1a row {}", row.sk_v8.row_id));
            }
            validate_w1a_non_json_row(row)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV12NonJsonReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub rows: Vec<SkV12NonJsonRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV12NonJsonRow {
    pub row_id: String,
    pub grammar_id: String,
    pub domain: String,
    pub corpus_or_workload: String,
    pub workload: String,
    pub workload_class: String,
    pub output_plane: String,
    pub outcome_id: String,
    pub verdict: String,
    pub strictness: String,
    pub generated_track1_source_path: String,
    pub generated_runtime_path: String,
    pub generated_input_provenance: String,
    pub grammar_checksum: String,
    pub input_checksum: String,
    pub input_bytes: u64,
    pub track1_mbps: f64,
    pub track1_artifact: String,
    pub track2_or_oracle_source_path: String,
    pub track2_independence_status: String,
    pub track2_or_oracle_mbps: Option<f64>,
    pub strict_output_equality: String,
    pub oracle_status: String,
    pub baseline_row_id: String,
    pub baseline_mbps: Option<f64>,
    pub threshold_mbps: Option<f64>,
    pub host_triple: String,
    pub feature_mask: String,
    pub build_flags: String,
    pub sample_count: u64,
    pub sample_cost: String,
    pub benchmark_artifact_path: String,
    pub measured_validation_path: String,
    pub profile_artifact: String,
    pub generated_loc: u64,
    pub generated_module_bytes: u64,
    pub grammar_size_guard: String,
    pub lock14_status: String,
    pub lock16_status: String,
    pub scalar_reference_status: String,
    pub checkasm_or_parity_status: String,
    pub json_guard_state: String,
    pub redress_entry: String,
    pub same_wave_consumer_class: String,
    pub gate_status: String,
}

impl SkV12NonJsonReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V12 non-JSON report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV12_NON_JSON_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V12 non-JSON schema {}",
                self.schema_id
            ));
        }
        if !self.wave_id.starts_with("SK-V12-W") || !is_skv12_run_id(&self.run_id) {
            return Err("invalid SK-V12 non-JSON report identity".to_string());
        }
        if self.rows.is_empty() {
            return Err("SK-V12 non-JSON report has no rows".to_string());
        }
        let mut seen = BTreeSet::new();
        for row in &self.rows {
            if !seen.insert(row.row_id.as_str()) {
                return Err(format!("duplicate SK-V12 non-JSON row {}", row.row_id));
            }
            validate_skv12_non_json_row(row, &self.run_id)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV12CssL4SotaReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub rows: Vec<SkV12CssL4SotaRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV12CssL4SotaRow {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub row_id: String,
    pub grammar_id: String,
    pub domain: String,
    pub corpus_or_workload: String,
    pub workload: String,
    pub output_plane: String,
    pub strictness: String,
    pub outcome_id: String,
    pub verdict: String,
    pub gate_status: String,
    pub generated_track1_source_path: String,
    pub generated_runtime_path: String,
    pub generated_input_provenance: String,
    pub grammar_checksum: String,
    pub input_checksum: String,
    pub input_bytes: u64,
    pub generated_loc: u64,
    pub generated_module_bytes: u64,
    pub grammar_size_guard: String,
    pub track1_mbps: f64,
    pub track2_or_oracle_mbps: f64,
    pub lightningcss_mbps: f64,
    pub threshold_mbps: f64,
    pub admission_margin_mbps: f64,
    pub admission_status: String,
    pub track1_artifact: String,
    pub cssparser_artifact_path: String,
    pub track2_or_oracle_source_path: String,
    pub lightningcss_command: String,
    pub lightningcss_artifact: String,
    pub lightningcss_fact_artifact_path: String,
    pub fact_stream_sha256: String,
    pub strict_output_equality: String,
    pub three_way_equality: String,
    pub lightningcss_sequence_status: String,
    pub track2_independence_status: String,
    pub measured_validation_path: String,
    pub benchmark_artifact_path: String,
    pub profile_artifact: String,
    pub sample_count: u64,
    pub sample_cost: String,
    pub host_triple: String,
    pub feature_mask: String,
    pub build_flags: String,
    pub lock14_status: String,
    pub lock16_status: String,
    pub scalar_reference_status: String,
    pub checkasm_or_parity_status: String,
    pub json_guard_state: String,
    pub same_wave_consumer_class: String,
    pub redress_entry: String,
}

impl SkV12CssL4SotaReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V12 CSS L4 SOTA report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV12_CSS_L4_SOTA_REPORT_SCHEMA {
            return Err(format!("unsupported CSS L4 SOTA schema {}", self.schema_id));
        }
        if self.wave_id != "SK-V12-W1b-2b" || !is_skv12_run_id(&self.run_id) {
            return Err("invalid CSS L4 SOTA report identity".to_string());
        }
        if self.rows.len() != 1 {
            return Err("CSS L4 SOTA report must contain exactly one row".to_string());
        }
        validate_skv12_css_l4_sota_row(&self.rows[0], self)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssStylesheetSelectorsReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssStylesheetSelectorsRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssStylesheetSelectorsRow {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub row_id: String,
    pub grammar_id: String,
    pub domain: String,
    pub corpus_or_workload: String,
    pub workload: String,
    pub output_plane: String,
    pub strictness: String,
    pub outcome_id: String,
    pub verdict: String,
    pub gate_status: String,
    pub generated_track1_source_path: String,
    pub generated_runtime_path: String,
    pub generated_input_provenance: String,
    pub grammar_checksum: String,
    pub input_checksum: String,
    pub input_bytes: u64,
    pub generated_loc: u64,
    pub generated_module_bytes: u64,
    pub grammar_size_guard: String,
    pub track1_mbps: f64,
    pub track2_or_oracle_mbps: f64,
    pub lightningcss_mbps: f64,
    pub threshold_mbps: f64,
    pub admission_margin_mbps: f64,
    pub admission_status: String,
    pub track1_artifact: String,
    pub oracle_artifact_path: String,
    pub track2_or_oracle_source_path: String,
    pub lightningcss_command: String,
    pub lightningcss_artifact: String,
    pub lightningcss_fact_artifact_path: String,
    pub fact_stream_sha256: String,
    pub strict_output_equality: String,
    pub three_way_equality: String,
    pub lightningcss_sequence_status: String,
    pub track2_independence_status: String,
    pub measured_validation_path: String,
    pub benchmark_artifact_path: String,
    pub profile_artifact: String,
    pub sample_count: u64,
    pub sample_cost: String,
    pub host_triple: String,
    pub feature_mask: String,
    pub build_flags: String,
    pub lock14_status: String,
    pub lock16_status: String,
    pub scalar_reference_status: String,
    pub checkasm_or_parity_status: String,
    pub json_guard_state: String,
    pub same_wave_consumer_class: String,
    pub redress_entry: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssDeclarationValuesExtendedReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssDeclarationValuesExtendedRow>,
}

pub type SkV13CssDeclarationValuesExtendedRow = SkV13CssStylesheetSelectorsRow;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssVisualFunctionsReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssVisualFunctionsRow>,
}

pub type SkV13CssVisualFunctionsRow = SkV13CssStylesheetSelectorsRow;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssAtRulesAndMediaReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssAtRulesAndMediaRow>,
}

pub type SkV13CssAtRulesAndMediaRow = SkV13CssStylesheetSelectorsRow;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssVendorCustomReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssVendorCustomRow>,
}

pub type SkV13CssVendorCustomRow = SkV13CssStylesheetSelectorsRow;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssNestedLayoutReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub covered_feature_rows: Vec<String>,
    pub rows: Vec<SkV13CssNestedLayoutRow>,
}

pub type SkV13CssNestedLayoutRow = SkV13CssStylesheetSelectorsRow;

impl SkV13CssStylesheetSelectorsReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS stylesheet/selectors report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_STYLESHEET_SELECTORS_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 stylesheet/selectors schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W2" || !self.run_id.starts_with("sk-v13-w2:") {
            return Err("invalid SK-V13 stylesheet/selectors report identity".to_string());
        }
        let expected_features = [
            "attribute_selectors",
            "pseudo_classes",
            "pseudo_elements",
            "selectors",
            "stylesheet_root",
        ];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W2 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W2 report must contain exactly one row".to_string());
        }
        validate_skv13_css_stylesheet_selectors_row(&self.rows[0], self)
    }
}

impl SkV13CssDeclarationValuesExtendedReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text).map_err(|error| {
            format!("invalid SK-V13 CSS declaration-values-extended report: {error}")
        })
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_DECLARATION_VALUES_EXTENDED_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 declaration-values-extended schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W3" || !self.run_id.starts_with("sk-v13-w3:") {
            return Err("invalid SK-V13 declaration-values-extended report identity".to_string());
        }
        let expected_features = [
            "calc_expressions",
            "color_functions",
            "css_variables",
            "declarations",
            "var_url_functions",
        ];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W3 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W3 report must contain exactly one row".to_string());
        }
        validate_skv13_css_declaration_values_extended_row(&self.rows[0], self)
    }
}

impl SkV13CssVisualFunctionsReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS visual-functions report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_VISUAL_FUNCTIONS_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 visual-functions schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W4" || !self.run_id.starts_with("sk-v13-w4:") {
            return Err("invalid SK-V13 visual-functions report identity".to_string());
        }
        let expected_features = ["easing_functions", "filters", "gradients", "transforms"];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W4 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W4 report must contain exactly one row".to_string());
        }
        validate_skv13_css_visual_functions_row(&self.rows[0], self)
    }
}

impl SkV13CssAtRulesAndMediaReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS at-rules/media report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_AT_RULES_AND_MEDIA_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 at-rules/media schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W10.1" || !self.run_id.starts_with("sk-v13-w10-1:") {
            return Err("invalid SK-V13 at-rules/media report identity".to_string());
        }
        let expected_features = ["at_rules_keyframes", "media_queries"];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W10.1 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W10.1 report must contain exactly one row".to_string());
        }
        validate_skv13_css_at_rules_and_media_row(&self.rows[0], self)
    }
}

impl SkV13CssVendorCustomReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS vendor/custom report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_VENDOR_CUSTOM_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 vendor/custom schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W10.2" || !self.run_id.starts_with("sk-v13-w10-2:") {
            return Err("invalid SK-V13 vendor/custom report identity".to_string());
        }
        let expected_features = ["custom_at_rules", "vendor_prefixes"];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W10.2 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W10.2 report must contain exactly one row".to_string());
        }
        validate_skv13_css_vendor_custom_row(&self.rows[0], self)
    }
}

impl SkV13CssNestedLayoutReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS nested/layout report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_NESTED_LAYOUT_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 nested/layout schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W10.3" || !self.run_id.starts_with("sk-v13-w10-3:") {
            return Err("invalid SK-V13 nested/layout report identity".to_string());
        }
        let expected_features = [
            "flexbox",
            "grid",
            "logical_properties",
            "nested_rules",
            "typed_property_groups",
        ];
        let mut actual = self
            .covered_feature_rows
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>();
        actual.sort_unstable();
        if actual != expected_features {
            return Err("SK-V13 W10.3 covered feature rows are stale".to_string());
        }
        if self.rows.len() != 1 {
            return Err("SK-V13 W10.3 report must contain exactly one row".to_string());
        }
        validate_skv13_css_nested_layout_row(&self.rows[0], self)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssComparatorOracleReport {
    pub schema_id: String,
    pub wave_id: String,
    pub run_id: String,
    pub declaration_values_sota_report_path: String,
    pub coverage: SkV13CssCoverageSummary,
    pub rows: Vec<SkV13CssFeatureCoverageRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssCoverageSummary {
    pub feature_row_count: u64,
    pub measured_row_count: u64,
    pub open_absent_row_count: u64,
    pub admission_eligible_row_count: u64,
    pub admitted_row_count: u64,
    pub feature_accept_count: u64,
    pub feature_reject_count: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct SkV13CssFeatureCoverageRow {
    pub row_id: String,
    pub css_feature_id: String,
    pub row_state: String,
    pub row_presence: String,
    pub css_feature_status: String,
    pub planned_wave: String,
    pub absence_reason: String,
    pub output_plane: String,
    pub feature_accept_count: u64,
    pub feature_reject_count: u64,
    pub feature_coverage_status: String,
    pub cssparser_or_golden_oracle: String,
    pub same_plane_fact_contract: String,
    pub admission_status: String,
}

impl SkV13CssComparatorOracleReport {
    pub fn from_json_str(text: &str) -> Result<Self, String> {
        serde_json::from_str(text)
            .map_err(|error| format!("invalid SK-V13 CSS comparator report: {error}"))
    }

    pub fn validate_gate(&self) -> Result<(), String> {
        if self.schema_id != SKV13_CSS_COMPARATOR_ORACLE_REPORT_SCHEMA {
            return Err(format!(
                "unsupported SK-V13 CSS comparator schema {}",
                self.schema_id
            ));
        }
        if self.wave_id != "SK-V13-W1" || !self.run_id.starts_with("sk-v13-w1:") {
            return Err("invalid SK-V13 CSS comparator report identity".to_string());
        }
        if self.declaration_values_sota_report_path.trim().is_empty() {
            return Err("SK-V13 CSS report missing declaration-values SOTA path".to_string());
        }
        if self.rows.len() != SKV13_CSS_FEATURES.len() {
            return Err(format!(
                "SK-V13 CSS comparator expected 24 feature rows, saw {}",
                self.rows.len()
            ));
        }
        let mut seen = BTreeSet::new();
        let mut measured = 0u64;
        let mut open_absent = 0u64;
        let mut eligible = 0u64;
        let mut admitted = 0u64;
        let mut accepts = 0u64;
        let mut rejects = 0u64;
        for feature in SKV13_CSS_FEATURES {
            let row_id = format!("css_l4/{feature}/direct_to_struct/main");
            let row = self
                .rows
                .iter()
                .find(|row| row.row_id == row_id)
                .ok_or_else(|| format!("SK-V13 CSS comparator missing {row_id}"))?;
            if !seen.insert(row.row_id.as_str()) {
                return Err(format!("duplicate SK-V13 CSS feature row {}", row.row_id));
            }
            if row.css_feature_id != *feature || row.css_feature_status == "PARTIAL" {
                return Err(format!("{} has invalid CSS feature identity", row.row_id));
            }
            accepts += row.feature_accept_count;
            rejects += row.feature_reject_count;
            if *feature == "declaration_values" {
                validate_skv13_measured_css_row(row)?;
                measured += 1;
                eligible += 1;
                admitted += 1;
            } else {
                validate_skv13_open_css_row(row)?;
                open_absent += 1;
            }
        }
        if self.coverage.feature_row_count != SKV13_CSS_FEATURES.len() as u64
            || self.coverage.measured_row_count != measured
            || self.coverage.open_absent_row_count != open_absent
            || self.coverage.admission_eligible_row_count != eligible
            || self.coverage.admitted_row_count != admitted
            || self.coverage.feature_accept_count != accepts
            || self.coverage.feature_reject_count != rejects
        {
            return Err("SK-V13 CSS comparator coverage totals are stale".to_string());
        }
        Ok(())
    }
}

fn validate_skv13_measured_css_row(row: &SkV13CssFeatureCoverageRow) -> Result<(), String> {
    if row.row_state != "admission_candidate"
        || row.row_presence != "measured"
        || row.css_feature_status != "ADMITTED-PARITY"
        || row.planned_wave != "SK-V12-W1b-2b"
        || row.absence_reason != "n/a"
        || row.output_plane != "css_l4_declaration_value_fact_stream"
        || row.admission_status != "PASS-MAINTAIN"
        || row.feature_accept_count == 0
        || row.feature_reject_count != 0
        || row.feature_coverage_status != "pass:strict-equality"
        || !row.cssparser_or_golden_oracle.contains("cssparser")
        || !row
            .same_plane_fact_contract
            .contains("track1=cssparser=lightningcss")
    {
        return Err(format!(
            "{} has invalid measured CSS comparator context",
            row.row_id
        ));
    }
    Ok(())
}

fn validate_skv13_open_css_row(row: &SkV13CssFeatureCoverageRow) -> Result<(), String> {
    if row.row_state != "open"
        || row.row_presence != "absent_until_planned_wave"
        || row.css_feature_status != "OPEN"
        || row.output_plane != "pending:same-plane-fact-stream"
        || row.feature_accept_count != 0
        || row.feature_reject_count != 0
        || row.feature_coverage_status != "open:awaiting-row-wave"
        || row.cssparser_or_golden_oracle != "pending"
        || row.same_plane_fact_contract != "pending"
        || row.admission_status != "not-admitted:absent"
        || row.planned_wave.trim().is_empty()
        || row.absence_reason.trim().is_empty()
    {
        return Err(format!(
            "{} has invalid open CSS comparator context",
            row.row_id
        ));
    }
    Ok(())
}

impl TelemetryRow {
    pub fn parse(
        corpus: impl Into<String>,
        outcome: Outcome,
        bytes: u64,
        track1_ns: Option<f64>,
        track2_ns: Option<f64>,
        competitors: ComparatorSet,
        hot_leaf: impl Into<String>,
    ) -> Self {
        let track1_mbps = throughput_mbps(bytes, track1_ns);
        let track2_mbps = throughput_mbps(bytes, track2_ns);
        let signal = parse_signal(outcome);
        Self::new(
            corpus,
            "parse_only",
            outcome.id().to_string(),
            verdict_label(outcome.verdict()).to_string(),
            "deferred",
            "view-boundary",
            "yes",
            "invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes",
            "parse_only",
            track1_mbps,
            track2_mbps,
            competitors,
            hot_leaf,
            signal,
        )
    }

    pub fn workload(
        corpus: impl Into<String>,
        workload: impl Into<String>,
        outcome: Option<Outcome>,
        bytes: u64,
        track1_ns: Option<f64>,
        track2_ns: Option<f64>,
        competitors: ComparatorSet,
        output_plane: impl Into<String>,
        flaw_probe: impl Into<String>,
        signal: impl Into<String>,
        hot_leaf: impl Into<String>,
    ) -> Self {
        let (outcome_id, verdict) = match outcome {
            Some(outcome) => (
                outcome.id().to_string(),
                verdict_label(outcome.verdict()).to_string(),
            ),
            None => ("A".to_string(), "GO".to_string()),
        };
        Self::new(
            corpus,
            workload,
            outcome_id,
            verdict,
            "deferred",
            "view-boundary",
            "yes",
            flaw_probe,
            output_plane,
            throughput_mbps(bytes, track1_ns),
            throughput_mbps(bytes, track2_ns),
            competitors,
            hot_leaf,
            signal,
        )
    }

    fn new(
        corpus: impl Into<String>,
        workload: impl Into<String>,
        outcome_id: String,
        verdict: String,
        strictness: impl Into<String>,
        parse_utf8: impl Into<String>,
        escape_complete: impl Into<String>,
        flaw_probe: impl Into<String>,
        output_plane: impl Into<String>,
        track1_mbps: Option<f64>,
        track2_mbps: Option<f64>,
        competitors: ComparatorSet,
        hot_leaf: impl Into<String>,
        signal: impl Into<String>,
    ) -> Self {
        let corpus = corpus.into();
        let workload = workload.into();
        let output_plane = output_plane.into();
        Self {
            sk_v8: SkV8Telemetry::placeholder(&corpus, &workload, &output_plane),
            corpus,
            workload,
            outcome_id,
            verdict,
            strictness: strictness.into(),
            parse_utf8: parse_utf8.into(),
            escape_complete: escape_complete.into(),
            flaw_probe: flaw_probe.into(),
            output_plane,
            track1_mbps,
            track2_mbps,
            delta_vs_skv6: "n/a (no machine-readable SK-V6 baseline in W0b)".to_string(),
            delta_vs_sonic_strict: delta_ratio(track1_mbps, competitors.sonic_strict_mbps),
            delta_vs_simdjson_dom: delta_ratio(track1_mbps, competitors.simdjson_dom_mbps),
            delta_vs_yyjson: delta_ratio(track1_mbps, competitors.yyjson_default_mbps),
            competitors,
            hot_leaf: hot_leaf.into(),
            signal: signal.into(),
        }
    }

    pub fn with_sk_v8(mut self, sk_v8: SkV8Telemetry) -> Self {
        self.sk_v8 = sk_v8;
        self
    }

    fn validate_schema_v3(&self) -> Result<(), String> {
        let required_text = [
            ("Corpus", self.corpus.as_str()),
            ("Workload", self.workload.as_str()),
            ("Outcome", self.outcome_id.as_str()),
            ("Verdict", self.verdict.as_str()),
            ("Strictness", self.strictness.as_str()),
            ("parse_utf8", self.parse_utf8.as_str()),
            ("escape_complete", self.escape_complete.as_str()),
            ("flaw_probe", self.flaw_probe.as_str()),
            ("Output plane", self.output_plane.as_str()),
            ("Delta vs SK-V6", self.delta_vs_skv6.as_str()),
            ("Hot leaf", self.hot_leaf.as_str()),
            ("Signal", self.signal.as_str()),
        ];
        for (field, value) in required_text {
            if value.trim().is_empty() {
                return Err(format!(
                    "{} {} row missing required {field}",
                    self.corpus, self.workload
                ));
            }
        }
        if self.track1_mbps.is_none() {
            return Err(format!(
                "{} {} row missing Track 1 Mbps",
                self.corpus, self.workload
            ));
        }
        if self.track2_mbps.is_none() {
            return Err(format!(
                "{} {} row missing Track 2 Mbps",
                self.corpus, self.workload
            ));
        }
        if self.competitors.sonic_strict_mbps.is_none() {
            return Err(format!(
                "{} {} row missing sonic-rs strict Mbps",
                self.corpus, self.workload
            ));
        }
        if self.competitors.serde_json_mbps.is_none() {
            return Err(format!(
                "{} {} row missing serde_json Mbps",
                self.corpus, self.workload
            ));
        }
        if self.delta_vs_sonic_strict.is_none() {
            return Err(format!(
                "{} {} row missing Delta vs sonic-strict",
                self.corpus, self.workload
            ));
        }
        Ok(())
    }

    fn validate_sk_v8_w0(&self) -> Result<(), String> {
        let telemetry = &self.sk_v8;
        let required_text = [
            ("row_id", telemetry.row_id.as_str()),
            ("grammar_id", telemetry.grammar_id.as_str()),
            ("domain", telemetry.domain.as_str()),
            (
                "measured_validation_path",
                telemetry.measured_validation_path.as_str(),
            ),
            ("profile_artifact", telemetry.profile_artifact.as_str()),
            ("sample_cost", telemetry.sample_cost.as_str()),
            ("build_flags", telemetry.build_flags.as_str()),
            ("host_triple", telemetry.host_triple.as_str()),
            ("feature_mask", telemetry.feature_mask.as_str()),
            ("costfacts_rule_id", telemetry.costfacts_rule_id.as_str()),
            (
                "costfacts_chosen_shape",
                telemetry.costfacts_chosen_shape.as_str(),
            ),
            ("redress_entry", telemetry.redress_entry.as_str()),
            ("wave_id", telemetry.wave_id.as_str()),
            ("run_id", telemetry.run_id.as_str()),
            ("sk_v9_open_delta", telemetry.sk_v9_open_delta.as_str()),
            ("track1_entry_point", telemetry.track1_entry_point.as_str()),
            ("track2_entry_point", telemetry.track2_entry_point.as_str()),
            ("comparator_plane", telemetry.comparator_plane.as_str()),
            ("per_iter_equality", telemetry.per_iter_equality.as_str()),
            (
                "audit_overlay_verdict",
                telemetry.audit_overlay_verdict.as_str(),
            ),
            (
                "audit_overlay_reference",
                telemetry.audit_overlay_reference.as_str(),
            ),
            ("sidecar_freshness", telemetry.sidecar_freshness.as_str()),
            ("substrate_target", telemetry.substrate_target.as_str()),
            ("retention_lifetime", telemetry.retention_lifetime.as_str()),
            ("policy_owner", telemetry.policy_owner.as_str()),
            ("sk_v14_open_delta", telemetry.sk_v14_open_delta.as_str()),
            ("substrate_surface", telemetry.substrate_surface.as_str()),
            (
                "structural_projection_status",
                telemetry.structural_projection_status.as_str(),
            ),
            (
                "substrate_cardinality",
                telemetry.substrate_cardinality.as_str(),
            ),
            (
                "same_wave_consumer_class",
                telemetry.same_wave_consumer_class.as_str(),
            ),
            (
                "track2_independence_status",
                telemetry.track2_independence_status.as_str(),
            ),
            (
                "diagnostic_nonproducer_status",
                telemetry.diagnostic_nonproducer_status.as_str(),
            ),
        ];
        for (field, value) in required_text {
            if value.trim().is_empty() {
                return Err(format!("{} missing SK-V9 W0 {field}", telemetry.row_id));
            }
        }
        if telemetry.grammar_id != "json" || telemetry.domain != "json_bench" {
            return Err(format!(
                "{} has unsupported grammar/domain",
                telemetry.row_id
            ));
        }
        validate_w0_row_identity(self)?;
        validate_w0_outcome(&telemetry.row_id, &self.outcome_id)?;
        if telemetry.wave_id != "SK-V14-open"
            || telemetry.sk_v9_open_delta != "baseline"
            || telemetry.sk_v14_open_delta != "baseline"
        {
            return Err(format!(
                "{} is not marked as SK-V14-open baseline",
                telemetry.row_id
            ));
        }
        validate_skv14_manifest_row(&self.skv14_manifest_row())?;
        if telemetry.diagnostic_nonproducer_status
            != "structural_scan+masking_probes+pmu+cycles:nonproducer"
        {
            return Err(format!(
                "{} has unsupported diagnostic nonproducer status",
                telemetry.row_id
            ));
        }
        if !is_skv9_open_run_id(&telemetry.run_id) {
            return Err(format!(
                "{} has invalid SK-V14-open run_id {}",
                telemetry.row_id, telemetry.run_id
            ));
        }
        if telemetry.sample_count == 0 {
            return Err(format!("{} missing sample_count", telemetry.row_id));
        }
        if telemetry.sample_cost.contains("n/a") || !telemetry.sample_cost.contains("ns_per_byte=")
        {
            return Err(format!("{} missing sample_cost", telemetry.row_id));
        }
        validate_w0_profile_artifact(&telemetry.row_id, &telemetry.profile_artifact)?;
        validate_w0_hot_leaf(
            &telemetry.row_id,
            &self.hot_leaf,
            &telemetry.profile_artifact,
        )?;
        validate_w0_manifest_semantics(self)?;
        if telemetry.same_wave_consumer_class != "gate_only" {
            return Err(format!(
                "{} has unsupported same-wave consumer class",
                telemetry.row_id
            ));
        }
        if self.workload == "parse_only"
            && !matches!(self.outcome_id.as_str(), "I" | "J" | "K" | "L" | "M" | "S")
        {
            return Err(format!(
                "{} parse row admitted outside substrate guard",
                telemetry.row_id
            ));
        }
        validate_comparator_evidence(&telemetry.row_id, &self.workload, &telemetry.comparators)?;
        validate_w0_admission_boundary(self)?;
        Ok(())
    }
}

impl SkV8Telemetry {
    fn placeholder(corpus: &str, workload: &str, output_plane: &str) -> Self {
        let row_id = format!("json/{corpus}/{workload}/main");
        Self {
            row_id: row_id.clone(),
            grammar_id: "json".to_string(),
            domain: "json_bench".to_string(),
            measured_validation_path: "view-boundary".to_string(),
            profile_artifact: format!("criterion:unbound;row={row_id}"),
            sample_cost: "ns_per_byte=1.000000".to_string(),
            sample_count: 1,
            build_flags: "profile=test;rustflags=;target_cpu=default".to_string(),
            host_triple: "test-host".to_string(),
            feature_mask: "test".to_string(),
            costfacts_rule_id: "none:pre-W1".to_string(),
            costfacts_chosen_shape: "none:pre-W1".to_string(),
            costfacts_rejected_alternative_ids: vec!["none:pre-W1".to_string()],
            redress_entry: "none".to_string(),
            wave_id: "SK-V14-open".to_string(),
            run_id: "test-run".to_string(),
            sk_v9_open_delta: "baseline".to_string(),
            track1_entry_point: skv14_track1_entry_point(workload).to_string(),
            track2_entry_point: skv14_track2_entry_point(workload).to_string(),
            comparator_plane: skv14_comparator_plane(corpus, workload),
            per_iter_equality: skv14_per_iter_equality(workload, 1),
            audit_overlay_verdict: skv14_audit_overlay_verdict(corpus, workload).to_string(),
            audit_overlay_reference: skv14_audit_overlay_reference(corpus, workload).to_string(),
            sidecar_freshness: format!("absent:not-collected-for-{workload}"),
            substrate_target: skv14_substrate_target(workload).to_string(),
            retention_lifetime: skv14_retention_lifetime(workload).to_string(),
            policy_owner: skv14_policy_owner(workload).to_string(),
            sk_v14_open_delta: "baseline".to_string(),
            substrate_surface: output_plane.to_string(),
            structural_projection_status: "n/a".to_string(),
            substrate_cardinality: "zero_or_inert".to_string(),
            same_wave_consumer_class: "gate_only".to_string(),
            track2_independence_status: "independent_verified".to_string(),
            diagnostic_nonproducer_status: "structural_scan+masking_probes+pmu+cycles:nonproducer"
                .to_string(),
            comparators: Vec::new(),
        }
    }
}

impl TelemetryRow {
    fn skv14_manifest_row(&self) -> SkV14ManifestRow {
        let telemetry = &self.sk_v8;
        SkV14ManifestRow {
            row_id: telemetry.row_id.clone(),
            grammar_id: telemetry.grammar_id.clone(),
            domain: telemetry.domain.clone(),
            wave_id: telemetry.wave_id.clone(),
            run_id: telemetry.run_id.clone(),
            track1_entry_point: telemetry.track1_entry_point.clone(),
            track2_entry_point: telemetry.track2_entry_point.clone(),
            comparator_plane: telemetry.comparator_plane.clone(),
            per_iter_equality: telemetry.per_iter_equality.clone(),
            audit_overlay_verdict: telemetry.audit_overlay_verdict.clone(),
            audit_overlay_reference: telemetry.audit_overlay_reference.clone(),
            sidecar_freshness: telemetry.sidecar_freshness.clone(),
            substrate_target: telemetry.substrate_target.clone(),
            retention_lifetime: telemetry.retention_lifetime.clone(),
            policy_owner: telemetry.policy_owner.clone(),
            measured_validation_path: telemetry.measured_validation_path.clone(),
            profile_artifact: telemetry.profile_artifact.clone(),
            sample_cost: telemetry.sample_cost.clone(),
            sample_count: telemetry.sample_count,
            build_flags: telemetry.build_flags.clone(),
            host_triple: telemetry.host_triple.clone(),
            feature_mask: telemetry.feature_mask.clone(),
            costfacts: format!(
                "{}:{}:{}",
                telemetry.costfacts_rule_id,
                telemetry.costfacts_chosen_shape,
                telemetry.costfacts_rejected_alternative_ids.join(",")
            ),
            redress_entry: telemetry.redress_entry.clone(),
            sk_v14_open_delta: telemetry.sk_v14_open_delta.clone(),
            substrate_surface: telemetry.substrate_surface.clone(),
            structural_projection_status: telemetry.structural_projection_status.clone(),
            substrate_cardinality: telemetry.substrate_cardinality.clone(),
            same_wave_consumer_class: telemetry.same_wave_consumer_class.clone(),
            track2_independence_status: telemetry.track2_independence_status.clone(),
            diagnostic_nonproducer_status: telemetry.diagnostic_nonproducer_status.clone(),
            comparator_evidence: format_comparator_evidence(&telemetry.comparators),
        }
    }
}

fn skv14_missing_json_typed_manifest(corpus: &str) -> SkV14ManifestRow {
    let row_id = format!("json/{corpus}/real_typed_struct/main");
    let workload = "real_typed_struct";
    let audit_overlay_verdict = skv14_audit_overlay_verdict(corpus, workload).to_string();
    let audit_overlay_reference = skv14_audit_overlay_reference(corpus, workload);
    SkV14ManifestRow {
        row_id,
        grammar_id: "json".into(),
        domain: "json_bench".into(),
        wave_id: "SK-V14-open".into(),
        run_id: "SK-V14-open:absent-product-surface-not-generated".into(),
        track1_entry_point: "absent:generated-product-surface-not-generated".into(),
        track2_entry_point: "absent:typed-oracle-product-surface-not-generated".into(),
        comparator_plane: skv14_comparator_plane(corpus, workload),
        per_iter_equality: "INTRINSIC-BLOCK:missing-product-surface".into(),
        audit_overlay_verdict,
        audit_overlay_reference,
        sidecar_freshness: "absent:not-collected-for-real_typed_struct".into(),
        substrate_target: skv14_substrate_target(workload).into(),
        retention_lifetime: skv14_retention_lifetime(workload).into(),
        policy_owner: skv14_policy_owner(workload).into(),
        measured_validation_path: "absent:product-surface-not-generated".into(),
        profile_artifact: "absent:product-surface-not-generated".into(),
        sample_cost: "absent:product-surface-not-generated".into(),
        sample_count: 0,
        build_flags: "absent:product-surface-not-generated".into(),
        host_triple: "absent:product-surface-not-generated".into(),
        feature_mask: "absent:product-surface-not-generated".into(),
        costfacts: "none:pre-W1:none:pre-W1:none:pre-W1".into(),
        redress_entry: "none:missing-product-surface".into(),
        sk_v14_open_delta: "absent:product-surface-not-generated".into(),
        substrate_surface: "typed_direct_projection".into(),
        structural_projection_status: "n/a".into(),
        substrate_cardinality: "zero_or_inert".into(),
        same_wave_consumer_class: "gate_only".into(),
        track2_independence_status: "not_applicable:missing-product-surface".into(),
        diagnostic_nonproducer_status: "not_applicable:missing-product-surface".into(),
        comparator_evidence: "absent:product-surface-not-generated".into(),
    }
}

fn skv14_css_manifest_row(feature: &str) -> SkV14ManifestRow {
    let row_id = format!("css_l4/{feature}/direct_to_struct/main");
    SkV14ManifestRow {
        row_id,
        grammar_id: "css_l4".into(),
        domain: "css_l4_bench".into(),
        wave_id: "SK-V14-open".into(),
        run_id: "SK-V14-open:retained-css-l4-audit-overlay".into(),
        track1_entry_point: format!("skinny::generated_css_l4::{feature}::parse"),
        track2_entry_point: "cssparser::Parser::parse_entirely".into(),
        comparator_plane: "lightningcss full-parse".into(),
        per_iter_equality: "not_admitted:pre-W8-css-full-parse-equality".into(),
        audit_overlay_verdict: "AUDIT-FALSIFIED".into(),
        audit_overlay_reference: "sk-v13/v1-css-l4-validation:§1-6".into(),
        sidecar_freshness: "absent:not-collected-for-css_l4".into(),
        substrate_target: "admitted_fact_output".into(),
        retention_lifetime: "output_row".into(),
        policy_owner: "generated_grammar".into(),
        measured_validation_path: "retained:pre-W4-audit-falsified-css-row".into(),
        profile_artifact: "retained:pre-W4-css-l4-results-row".into(),
        sample_cost: "retained:pre-W4-css-l4-results-row".into(),
        sample_count: 0,
        build_flags: "retained:pre-W4-css-l4-results-row".into(),
        host_triple: "retained:pre-W4-css-l4-results-row".into(),
        feature_mask: "retained:pre-W4-css-l4-results-row".into(),
        costfacts: "none:pre-W8:none:pre-W8:none:pre-W8".into(),
        redress_entry: "pending:W4-PRUNE-2".into(),
        sk_v14_open_delta: "baseline".into(),
        substrate_surface: "css_l4_fact_stream".into(),
        structural_projection_status: "audit-falsified:fake-generated-template".into(),
        substrate_cardinality: "one".into(),
        same_wave_consumer_class: "gate_only".into(),
        track2_independence_status: "pending:W8-cssparser-full-parse".into(),
        diagnostic_nonproducer_status: "css-l4-audit-overlay:nonproducer".into(),
        comparator_evidence: "lightningcss_strict[plane=full-parse,strictness=strict,freshness=historical:pre-W8,sidecar=absent:not-collected-for-css_l4,mbps=n/a,source=sk-v13/v1-css-l4-validation]".into(),
    }
}

fn skv14_css_reconciled_costfacts(costfacts: &str) -> String {
    let mut value = costfacts.to_string();
    value = value.replace(
        "outcome=A;verdict=GO;gate=pass;admission=PASS-ADMIT-CANDIDATE",
        "historical_claim=A_GO_PASS_ADMIT_CANDIDATE;current_status=AUDIT-FALSIFIED_OPEN;current_reason=REDRESS-215_fact_stream_not_full_parse",
    );
    value = value.replace(
        "outcome=A;verdict=GO;gate=pass;feature_status=ADMITTED-PARITY",
        "historical_claim=A_GO_ADMITTED_PARITY;current_status=AUDIT-FALSIFIED_OPEN;current_reason=REDRESS-215_fact_stream_not_full_parse",
    );
    value
}

fn validate_skv14_manifest_rows(rows: &[SkV14ManifestRow]) -> Result<(), String> {
    let mut seen = BTreeSet::new();
    let mut falsified = 0usize;
    let mut pending = 0usize;
    let mut sustained = 0usize;
    for row in rows {
        validate_skv14_manifest_row(row)?;
        if !seen.insert(row.row_id.as_str()) {
            return Err(format!("duplicate SK-V14 manifest row {}", row.row_id));
        }
        match row.audit_overlay_verdict.as_str() {
            "AUDIT-FALSIFIED" => falsified += 1,
            "AUDIT-PENDING" => pending += 1,
            "AUDIT-SUSTAINED" => {
                validate_skv14_sustained_row(row)?;
                sustained += 1;
            }
            other => {
                return Err(format!(
                    "{} has unsupported audit overlay {other}",
                    row.row_id
                ))
            }
        }
    }
    let expected = SKV14_JSON_CORPORA.len() * SKV14_JSON_WORKLOADS.len() + SKV14_CSS_FEATURES.len();
    if rows.len() != expected {
        return Err(format!(
            "SK-V14 manifest expected {expected} rows, saw {}",
            rows.len()
        ));
    }
    for corpus in SKV14_JSON_CORPORA {
        for workload in SKV14_JSON_WORKLOADS {
            let row_id = format!("json/{corpus}/{workload}/main");
            if !seen.contains(row_id.as_str()) {
                return Err(format!("SK-V14 manifest missing {row_id}"));
            }
        }
    }
    for feature in SKV14_CSS_FEATURES {
        let row_id = format!("css_l4/{feature}/direct_to_struct/main");
        if !seen.contains(row_id.as_str()) {
            return Err(format!("SK-V14 manifest missing {row_id}"));
        }
    }
    if pending != 0 || falsified + sustained != 75 {
        return Err(format!(
            "SK-V14 audit overlay expected pending=0 and falsified+sustained=75 after authorized W9/W9AA/W9AB/W10/W10R/W10S/W10T/W10V/W10W/W11A/W11L/W11N/W11O/W11U/W11W admits, saw {falsified} / {pending} / {sustained}"
        ));
    }
    Ok(())
}

fn validate_skv14_sustained_row(row: &SkV14ManifestRow) -> Result<(), String> {
    if SKV14_W11L_TOKEN_PRODUCT_TYPED_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11L"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_real_typed_struct"
            || row.track2_entry_point != "bbnf_bench::real_typed_struct::track2_typed"
            || row.comparator_plane != format!("{corpus}::typed_strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_typed_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11L-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11L-decoded-token-product"
            || row.substrate_surface != "typed_direct_projection"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11L sustained typed token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11L_TOKEN_PRODUCT_DIRECT_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11L"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_direct_to_struct"
            || row.track2_entry_point != "bbnf_bench::direct_struct::track2_strict_product"
            || row.comparator_plane != format!("{corpus}::strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11L-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11L-decoded-token-product"
            || row.substrate_surface != "direct_strict_product"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11L sustained direct token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11N_UNICODE_MIXED_TYPED_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11N"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_real_typed_struct"
            || row.track2_entry_point != "bbnf_bench::real_typed_struct::track2_typed"
            || row.comparator_plane != format!("{corpus}::typed_strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_typed_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11N-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11N-unicode-mixed-decoded-token-product"
            || row.substrate_surface != "typed_direct_projection"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11N sustained unicode_mixed typed token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11N_UNICODE_MIXED_DIRECT_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11N"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_direct_to_struct"
            || row.track2_entry_point != "bbnf_bench::direct_struct::track2_strict_product"
            || row.comparator_plane != format!("{corpus}::strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11N-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11N-unicode-mixed-decoded-token-product"
            || row.substrate_surface != "direct_strict_product"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11N sustained unicode_mixed direct token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11O_GSOC_TYPED_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11O"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_real_typed_struct"
            || row.track2_entry_point != "bbnf_bench::real_typed_struct::track2_typed"
            || row.comparator_plane != format!("{corpus}::typed_strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_typed_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11O-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11O-gsoc-decoded-token-product"
            || row.substrate_surface != "typed_direct_projection"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11O sustained gsoc-2018 typed token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11O_GSOC_DIRECT_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11O"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_direct_to_struct"
            || row.track2_entry_point != "bbnf_bench::direct_struct::track2_strict_product"
            || row.comparator_plane != format!("{corpus}::strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11O-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11O-gsoc-decoded-token-product"
            || row.substrate_surface != "direct_strict_product"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11O sustained gsoc-2018 direct token-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11U_UNICODE_ESCAPES_TYPED_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11U"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_real_typed_struct"
            || row.track2_entry_point != "bbnf_bench::real_typed_struct::track2_typed"
            || row.comparator_plane != format!("{corpus}::typed_strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_typed_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11U-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11U-unicode-escapes-raw-lexeme-product"
            || row.substrate_surface != "typed_direct_projection"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11U sustained unicode_escapes typed raw-lexeme row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11U_UNICODE_ESCAPES_DIRECT_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11U"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_direct_to_struct"
            || row.track2_entry_point != "bbnf_bench::direct_struct::track2_strict_product"
            || row.comparator_plane != format!("{corpus}::strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.redress_entry != "none:SK-V14-W11U-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11U-unicode-escapes-raw-lexeme-product"
            || row.substrate_surface != "direct_strict_product"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11U sustained unicode_escapes direct raw-lexeme row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W9_TYPED_ADMIT_ROWS.contains(&row.row_id.as_str()) {
        let expected_wave = if row.row_id == "json/distinct_values/real_typed_struct/main" {
            "SK-V14-W9AA"
        } else if row.row_id == "json/canada/real_typed_struct/main" {
            "SK-V14-W9AB"
        } else {
            "SK-V14-W9"
        };
        if row.wave_id != expected_wave
            || row.same_wave_consumer_class != "gate_json_typed_contract"
            || row.track2_independence_status != "independent_verified"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W9 sustained typed row",
                row.row_id
            ));
        }
        return Ok(());
    }
    if SKV14_W11A_DIRECT_STRICT_ADMIT_ROWS.contains(&row.row_id.as_str()) {
        let (corpus, _) = parse_row_id(&row.row_id)?;
        if row.wave_id != "SK-V14-W11A"
            || row.track1_entry_point != "bbnf_bench::json_parity::track1_direct_to_struct"
            || row.track2_entry_point != "bbnf_bench::direct_struct::track2_strict_product"
            || row.comparator_plane != format!("{corpus}::strict_struct_deser")
            || row.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "direct_sink"
            || row.substrate_surface != "direct_strict_product"
            || row.redress_entry != "none:SK-V14-W11A-admit"
            || row.sk_v14_open_delta != "admitted:SK-V14-W11A-direct-strict-product"
        {
            return Err(format!(
                "{} is not a valid SK-V14 W11A sustained direct strict-product row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if is_skv14_w10_parse_row(&row.row_id) {
        let spec = json_parse_only_admission_spec_for_row_id(&row.row_id)
            .ok_or_else(|| format!("{} lacks SK-V14 parse_only spec", row.row_id))?;
        if row.wave_id != spec.wave_id
            || row.track1_entry_point != "runtime::generated_json::parse_only"
            || row.track2_entry_point != "bbnf_bench::json_parity::track2_structural_oracle"
            || row.comparator_plane != "parse_only/sonic_rs::Skipper"
            || row.same_wave_consumer_class != "generated_json_parse_only_contract"
            || row.track2_independence_status != "independent_verified"
            || row.substrate_target != "parse_only_validator"
            || row.redress_entry != spec.redress_entry
            || row.sk_v14_open_delta != json_parse_only_open_delta(spec)
        {
            return Err(format!(
                "{} is not a valid SK-V14 sustained parse_only row",
                row.row_id
            ));
        }
        if !valid_skv14_per_iter_pass(&row.per_iter_equality) {
            return Err(format!(
                "{} lacks SK-V14 timed per-iteration equality PASS",
                row.row_id
            ));
        }
        return Ok(());
    }
    if is_skv14_w8r_css_row(&row.row_id) {
        if row.wave_id != "SK-V14-W8R"
            || !row
                .track1_entry_point
                .starts_with("runtime::generated_css_l4_")
            || !row.track1_entry_point.ends_with("::parser::parse_full")
            || row.track2_entry_point != "cssparser::StyleSheetParser full-parse probe"
            || row.comparator_plane != "lightningcss full-parse"
            || !valid_skv14_per_iter_pass(&row.per_iter_equality)
            || !row
                .audit_overlay_reference
                .contains("sk-v14-W8R:css-full-parse-same-plane")
            || row.sidecar_freshness != "same-run:production-corpus-full-parse"
            || row.substrate_target != "css_l4_full_parse"
            || row.retention_lifetime != "full_parse_summary"
            || row.policy_owner != "generated_grammar"
            || row.redress_entry != "REDRESS-215-superseded-by-W8R"
            || row.sk_v14_open_delta != "admitted:SK-V14-W8R-full-parse"
            || !row.substrate_surface.starts_with("generated_css_l4_")
            || row.structural_projection_status != "css_l4_full_parse"
            || row.substrate_cardinality != "one"
            || row.same_wave_consumer_class != "gate_css_l4_w8_full_parse_contract"
            || !row
                .track2_independence_status
                .starts_with("independent_verified:lightningcss+cssparser")
            || !row
                .comparator_evidence
                .contains("strict_equality[status=pass")
            || !row.comparator_evidence.contains("wrong_plane_outputs=0")
        {
            return Err(format!(
                "{} is not a valid SK-V14 W8R sustained CSS full-parse row",
                row.row_id
            ));
        }
        return Ok(());
    }
    Err(format!(
        "{} is AUDIT-SUSTAINED without W9 typed, W10/W10R/W10S/W10T/W10V/W10W/W11W parse_only, W11A direct strict-product, W11L/W11N/W11O token-product, W11U raw-lexeme product, or W8R CSS full-parse authority",
        row.row_id
    ))
}

fn is_skv14_w10_parse_row(row_id: &str) -> bool {
    matches!(
        parse_row_id(row_id),
        Ok((corpus, "parse_only")) if SKV14_JSON_CORPORA.contains(&corpus)
    )
}

fn is_skv14_w8r_css_row(row_id: &str) -> bool {
    legacy_css_feature_from_row_id(row_id)
        .is_some_and(|feature| SKV14_CSS_FEATURES.contains(&feature))
}

fn validate_skv14_manifest_row(row: &SkV14ManifestRow) -> Result<(), String> {
    for (field, value) in [
        ("row_id", row.row_id.as_str()),
        ("grammar_id", row.grammar_id.as_str()),
        ("domain", row.domain.as_str()),
        ("wave_id", row.wave_id.as_str()),
        ("run_id", row.run_id.as_str()),
        ("track1_entry_point", row.track1_entry_point.as_str()),
        ("track2_entry_point", row.track2_entry_point.as_str()),
        ("comparator_plane", row.comparator_plane.as_str()),
        ("per_iter_equality", row.per_iter_equality.as_str()),
        ("audit_overlay_verdict", row.audit_overlay_verdict.as_str()),
        (
            "audit_overlay_reference",
            row.audit_overlay_reference.as_str(),
        ),
        ("sidecar_freshness", row.sidecar_freshness.as_str()),
        ("substrate_target", row.substrate_target.as_str()),
        ("retention_lifetime", row.retention_lifetime.as_str()),
        ("policy_owner", row.policy_owner.as_str()),
        ("sk_v14_open_delta", row.sk_v14_open_delta.as_str()),
    ] {
        if value.trim().is_empty() {
            return Err(format!("{} missing SK-V14 {field}", row.row_id));
        }
    }
    if !matches!(
        row.substrate_target.as_str(),
        "local_temp_only"
            | "existing_tape"
            | "parse_only_validator"
            | "direct_sink"
            | "css_l4_full_parse"
            | "admitted_fact_output"
    ) {
        return Err(format!(
            "{} has invalid substrate_target {}",
            row.row_id, row.substrate_target
        ));
    }
    if !matches!(
        row.retention_lifetime.as_str(),
        "local_loop" | "generated_function" | "full_parse_summary" | "output_row"
    ) {
        return Err(format!(
            "{} has invalid retention_lifetime {}",
            row.row_id, row.retention_lifetime
        ));
    }
    if !matches!(
        row.policy_owner.as_str(),
        "generated_grammar" | "caller_data" | "none"
    ) {
        return Err(format!(
            "{} has invalid policy_owner {}",
            row.row_id, row.policy_owner
        ));
    }
    if row.sidecar_freshness == "sidecar-same-run" {
        return Err(format!(
            "{} claims sidecar-same-run without structured manifest",
            row.row_id
        ));
    }
    if row.comparator_plane.contains("from_slice::<Value>") {
        return Err(format!(
            "{} comparator_plane reopens eager DOM comparator",
            row.row_id
        ));
    }
    if row.track1_entry_point == row.track2_entry_point {
        return Err(format!(
            "{} has identical Track 1/Track 2 entry",
            row.row_id
        ));
    }
    if row.track2_entry_point.starts_with("runtime::tape::")
        && !matches!(
            row.track2_entry_point.as_str(),
            "runtime::tape::Tape" | "runtime::tape::OffsetFlags"
        )
    {
        return Err(format!(
            "{} Track 2 reaches private runtime tape internals",
            row.row_id
        ));
    }
    if row.grammar_id == "json"
        && row.sample_count > 0
        && !valid_skv14_per_iter_pass(&row.per_iter_equality)
    {
        return Err(format!(
            "{} lacks W1 timed per-iteration equality PASS",
            row.row_id
        ));
    }
    if row.grammar_id == "json"
        && row.sample_count == 0
        && row.per_iter_equality != "INTRINSIC-BLOCK:missing-product-surface"
    {
        return Err(format!(
            "{} missing product row lacks intrinsic-block equality marker",
            row.row_id
        ));
    }
    if row.grammar_id == "json"
        && (row
            .comparator_evidence
            .contains(&["sonic_rs", "anchor"].join("_"))
            || row
                .comparator_evidence
                .contains(&["from_slice::<sonic_rs", "::Value>"].concat())
            || row
                .comparator_evidence
                .contains("historical:sk-v7-sidecar-profile")
            || row
                .comparator_evidence
                .contains("sidecar-profile:sk-v7-cpp"))
    {
        return Err(format!("{} carries stale comparator evidence", row.row_id));
    }
    if row.row_id.contains("/direct_to_struct/")
        && row.track2_entry_point == "bbnf_bench::direct_struct::sonic_digest"
    {
        return Err(format!("{} Track 2 points to sonic comparator", row.row_id));
    }
    if row.row_id.contains("/real_typed_struct/")
        && row.track2_entry_point == "bbnf_bench::real_typed_struct::sonic_typed"
    {
        return Err(format!("{} Track 2 points to sonic comparator", row.row_id));
    }
    if row.audit_overlay_verdict == "AUDIT-FALSIFIED"
        && row.audit_overlay_reference.starts_with("pending:")
    {
        return Err(format!(
            "{} falsified row lacks validation reference",
            row.row_id
        ));
    }
    if row.row_id.starts_with("css_l4/")
        && row.audit_overlay_verdict == "AUDIT-FALSIFIED"
        && (row.costfacts.contains("outcome=A;verdict=GO")
            || row.costfacts.contains("feature_status=ADMITTED-PARITY"))
    {
        return Err(format!(
            "{} embeds live-looking admitted CSS CostFacts after W11 close",
            row.row_id
        ));
    }
    Ok(())
}

fn skv14_track1_entry_point(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "runtime::generated_json::parse_only",
        "direct_to_struct" => "bbnf_bench::json_parity::track1_direct_to_struct",
        "real_typed_struct" => "bbnf_bench::json_parity::track1_real_typed_struct",
        _ => "unknown",
    }
}

fn skv14_track2_entry_point(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "bbnf_bench::json_parity::track2_structural_oracle",
        "direct_to_struct" => "bbnf_bench::direct_struct::track2_strict_product",
        "real_typed_struct" => "bbnf_bench::real_typed_struct::track2_typed",
        _ => "unknown",
    }
}

fn skv14_comparator_plane(corpus: &str, workload: &str) -> String {
    match workload {
        "parse_only" => "parse_only/sonic_rs::Skipper".to_string(),
        "direct_to_struct" => format!("{corpus}::strict_struct_deser"),
        "real_typed_struct" => format!("{corpus}::typed_strict_struct_deser"),
        _ => "unknown".to_string(),
    }
}

fn skv14_per_iter_equality(workload: &str, sample_count: u64) -> String {
    let checks = sample_count.max(1);
    match workload {
        "parse_only" | "direct_to_struct" | "real_typed_struct" => {
            format!("PASS:scope=criterion-timing;checks={checks};mismatches=0")
        }
        _ => "INTRINSIC-BLOCK:unsupported-workload".to_string(),
    }
}

fn valid_skv14_per_iter_pass(value: &str) -> bool {
    if !value.starts_with("PASS:") {
        return false;
    }
    let mut has_scope = false;
    let mut has_checks = false;
    let mut has_mismatches = false;
    for field in value.trim_start_matches("PASS:").split(';') {
        if matches!(
            field,
            "scope=criterion-timing" | "scope=profile-direct-cold" | "scope=css_l4_w8_full_parse"
        ) {
            has_scope = true;
        } else if let Some(checks) = field.strip_prefix("checks=") {
            has_checks = checks.parse::<u64>().is_ok_and(|value| value > 0);
        } else if field == "mismatches=0" {
            has_mismatches = true;
        }
    }
    has_scope && has_checks && has_mismatches
}

fn skv14_audit_overlay_verdict(corpus: &str, workload: &str) -> &'static str {
    if skv14_json_audit_falsified(corpus, workload) {
        "AUDIT-FALSIFIED"
    } else {
        "AUDIT-PENDING"
    }
}

fn skv14_audit_overlay_reference(corpus: &str, workload: &str) -> String {
    if !skv14_json_audit_falsified(corpus, workload) {
        return "pending:SK-V14-W1-rebind-or-maintain".to_string();
    }
    match workload {
        "parse_only" => "sk-v13/v2-json-validation:§1-2;sk-v13/v6-comparator-integrity:§1+§3",
        "direct_to_struct" => "sk-v13/v6-comparator-integrity:§1+§3;sk-v13/v2-json-validation:§3",
        "real_typed_struct" => "sk-v13/v6-comparator-integrity:§1+§3;sk-v13/v2-json-validation:§4",
        _ => "sk-v13/audit-overfit:unknown",
    }
    .to_string()
}

fn skv14_json_audit_falsified(corpus: &str, workload: &str) -> bool {
    match workload {
        "parse_only" => matches!(
            corpus,
            "numbers" | "citm_catalog" | "canada" | "marine_ik" | "mesh"
        ),
        "direct_to_struct" => matches!(
            corpus,
            "citm_catalog"
                | "apache_builds"
                | "marine_ik"
                | "instruments"
                | "numbers"
                | "unicode_basic"
        ),
        "real_typed_struct" => matches!(
            corpus,
            "twitter"
                | "citm_catalog"
                | "apache_builds"
                | "github_events"
                | "update_center"
                | "mesh"
                | "random"
                | "marine_ik"
                | "instruments"
                | "numbers"
                | "unicode_basic"
                | "distinct_values"
        ),
        _ => false,
    }
}

fn skv14_substrate_target(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "parse_only_validator",
        "direct_to_struct" | "real_typed_struct" => "direct_sink",
        _ => "local_temp_only",
    }
}

fn skv14_retention_lifetime(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "generated_function",
        "direct_to_struct" | "real_typed_struct" => "generated_function",
        _ => "local_loop",
    }
}

fn skv14_policy_owner(workload: &str) -> &'static str {
    match workload {
        "parse_only" => "generated_grammar",
        "direct_to_struct" | "real_typed_struct" => "generated_grammar",
        _ => "none",
    }
}

impl Report {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            rows: Vec::new(),
            probe_rows: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn push_row(
        &mut self,
        corpus: impl Into<String>,
        outcome: Outcome,
        bytes: u64,
        track1_ns: Option<f64>,
        track2_ns: Option<f64>,
        competitors: ComparatorSet,
    ) {
        self.rows.push(TelemetryRow::parse(
            corpus,
            outcome,
            bytes,
            track1_ns,
            track2_ns,
            competitors,
            "unprofiled in W0b; no kernel prescription from this row",
        ));
    }

    pub fn push_workload_row(
        &mut self,
        corpus: impl Into<String>,
        workload: impl Into<String>,
        outcome: Option<Outcome>,
        bytes: u64,
        track1_ns: Option<f64>,
        track2_ns: Option<f64>,
        competitors: ComparatorSet,
        output_plane: impl Into<String>,
        flaw_probe: impl Into<String>,
        signal: impl Into<String>,
    ) {
        self.rows.push(TelemetryRow::workload(
            corpus,
            workload,
            outcome,
            bytes,
            track1_ns,
            track2_ns,
            competitors,
            output_plane,
            flaw_probe,
            signal,
            "unprofiled in W0b; no kernel prescription from this row",
        ));
    }

    pub fn push_probe_row(
        &mut self,
        corpus: impl Into<String>,
        probe: impl Into<String>,
        bytes: u64,
        probe_ns: Option<f64>,
        track1_ns: Option<f64>,
        signal: impl Into<String>,
    ) {
        self.probe_rows.push(ProbeReportRow {
            corpus: corpus.into(),
            probe: probe.into(),
            mbps: throughput_mbps(bytes, probe_ns),
            ns_per_iter: probe_ns,
            vs_track1: ratio_to_track1(probe_ns, track1_ns),
            signal: signal.into(),
        });
    }

    pub fn validate_schema_v3(&self) -> Result<(), String> {
        if self.rows.is_empty() {
            return Err("schema-v3 report has no telemetry rows".to_string());
        }
        for row in &self.rows {
            row.validate_schema_v3()?;
        }
        Ok(())
    }

    pub fn validate_sk_v8_w0(&self) -> Result<(), String> {
        let mut seen = BTreeSet::new();
        let mut run_id = None::<String>;
        let mut w6_github_events_typed_seen = false;
        let mut w13_numbers_typed_seen = false;
        let mut w13_unicode_basic_typed_seen = false;
        let mut w13_random_typed_seen = false;
        let mut w13_instruments_typed_seen = false;
        let mut w11l_y_string_typed_seen = false;
        let mut w11n_unicode_mixed_typed_seen = false;
        let mut w11o_gsoc_typed_seen = false;
        let mut w11u_unicode_escapes_typed_seen = false;
        for row in &self.rows {
            let row_id = row.sk_v8.row_id.as_str();
            if !seen.insert(row_id) {
                return Err(format!("duplicate SK-V9 W0 row_id {row_id}"));
            }
            if row_id == W11O_GSOC_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open" {
                validate_w11o_gsoc_typed_row(row)?;
                w11o_gsoc_typed_seen = true;
            } else if row_id == W11U_UNICODE_ESCAPES_TYPED_ROW_ID
                && row.sk_v8.wave_id != "SK-V14-open"
            {
                validate_w11u_unicode_escapes_typed_row(row)?;
                w11u_unicode_escapes_typed_seen = true;
            } else if row_id == W11N_UNICODE_MIXED_TYPED_ROW_ID
                && row.sk_v8.wave_id != "SK-V14-open"
            {
                validate_w11n_unicode_mixed_typed_row(row)?;
                w11n_unicode_mixed_typed_seen = true;
            } else if row_id == W11L_Y_STRING_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open" {
                validate_w11l_y_string_typed_row(row)?;
                w11l_y_string_typed_seen = true;
            } else if row_id == W6_GITHUB_EVENTS_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open"
            {
                validate_w6_github_events_typed_row(row)?;
                w6_github_events_typed_seen = true;
            } else if row_id == W13_NUMBERS_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open" {
                validate_w13_numbers_typed_row(row)?;
                w13_numbers_typed_seen = true;
            } else if row_id == W13_UNICODE_BASIC_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open"
            {
                validate_w13_unicode_basic_typed_row(row)?;
                w13_unicode_basic_typed_seen = true;
            } else if row_id == W13_RANDOM_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open" {
                validate_w13_random_typed_row(row)?;
                w13_random_typed_seen = true;
            } else if row_id == W13_INSTRUMENTS_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open" {
                validate_w13_instruments_typed_row(row)?;
                w13_instruments_typed_seen = true;
            } else if row_id == W15_UPDATE_CENTER_TYPED_ROW_ID && row.sk_v8.wave_id != "SK-V14-open"
            {
                validate_w15_update_center_typed_row(row)?;
            } else if row.outcome_id == "A" {
                if let Some(spec) = json_parse_only_admission_spec_for_row_id(row_id) {
                    validate_json_parse_only_admission_row(row, spec)?;
                } else if row_id == W11L_Y_STRING_DIRECT_ROW_ID {
                    validate_w11l_y_string_direct_row(row)?;
                } else if row_id == W11N_UNICODE_MIXED_DIRECT_ROW_ID {
                    validate_w11n_unicode_mixed_direct_row(row)?;
                } else if row_id == W11O_GSOC_DIRECT_ROW_ID {
                    validate_w11o_gsoc_direct_row(row)?;
                } else if row_id == W11U_UNICODE_ESCAPES_DIRECT_ROW_ID {
                    validate_w11u_unicode_escapes_direct_row(row)?;
                } else {
                    let Some(baseline) = sk_v8_open_baseline(row_id) else {
                        return Err(format!("unknown SK-V8 comparison row_id {row_id}"));
                    };
                    if direct_contract_row_changed(row, baseline) {
                        validate_direct_row_movement(row, baseline)?;
                    } else {
                        row.validate_sk_v8_w0()?;
                        if row.outcome_id != baseline.outcome_id
                            && !w0_allows_fresh_diagnostic_outcome(
                                baseline.outcome_id,
                                row.outcome_id.as_str(),
                            )
                        {
                            return Err(format!(
                                "{row_id} outcome moved from SK-V8 comparison baseline {} to {}",
                                baseline.outcome_id, row.outcome_id
                            ));
                        }
                        if row.verdict != baseline.verdict {
                            return Err(format!(
                                "{row_id} verdict moved from SK-V8 comparison baseline {} to {}",
                                baseline.verdict, row.verdict
                            ));
                        }
                    }
                }
            } else {
                let Some(baseline) = sk_v8_open_baseline(row_id) else {
                    return Err(format!("unknown SK-V8 comparison row_id {row_id}"));
                };
                if direct_contract_row_changed(row, baseline) {
                    validate_direct_row_movement(row, baseline)?;
                } else {
                    row.validate_sk_v8_w0()?;
                    if row.outcome_id != baseline.outcome_id
                        && !w0_allows_fresh_diagnostic_outcome(
                            baseline.outcome_id,
                            row.outcome_id.as_str(),
                        )
                    {
                        return Err(format!(
                            "{row_id} outcome moved from SK-V8 comparison baseline {} to {}",
                            baseline.outcome_id, row.outcome_id
                        ));
                    }
                    if row.verdict != baseline.verdict {
                        return Err(format!(
                            "{row_id} verdict moved from SK-V8 comparison baseline {} to {}",
                            baseline.verdict, row.verdict
                        ));
                    }
                }
            }
            let normalized_run_id = json_parse_only_admission_spec_for_row_id(row_id)
                .and_then(|spec| {
                    row.sk_v8
                        .run_id
                        .strip_prefix(spec.run_id_prefix)
                        .map(|suffix| format!("{SK_V9_OPEN_RUN_ID_PREFIX}{suffix}"))
                })
                .unwrap_or_else(|| row.sk_v8.run_id.clone());
            match &run_id {
                Some(expected) if expected != &normalized_run_id => {
                    return Err(format!(
                        "{row_id} run_id {} differs from report run_id {}",
                        row.sk_v8.run_id, expected
                    ));
                }
                Some(_) => {}
                None => run_id = Some(normalized_run_id),
            }
        }
        let expected_rows = SK_V8_OPEN_BASELINE.len()
            + usize::from(w6_github_events_typed_seen)
            + usize::from(w13_numbers_typed_seen)
            + usize::from(w13_unicode_basic_typed_seen)
            + usize::from(w13_random_typed_seen)
            + usize::from(w13_instruments_typed_seen)
            + usize::from(w11l_y_string_typed_seen)
            + usize::from(w11n_unicode_mixed_typed_seen)
            + usize::from(w11o_gsoc_typed_seen)
            + usize::from(w11u_unicode_escapes_typed_seen);
        if self.rows.len() != expected_rows {
            return Err(format!(
                "SK-V9 W0 expected {expected_rows} main rows, saw {}",
                self.rows.len()
            ));
        }
        for baseline in SK_V8_OPEN_BASELINE {
            if !seen.contains(baseline.row_id) {
                return Err(format!(
                    "missing SK-V8 comparison row_id {}",
                    baseline.row_id
                ));
            }
        }
        validate_existing_typed_maintain_floors(self)?;
        validate_skv14_manifest_rows(&self.skv14_manifest_rows()?)?;
        Ok(())
    }

    fn skv14_manifest_rows(&self) -> Result<Vec<SkV14ManifestRow>, String> {
        let mut rows = Vec::new();
        let mut seen = BTreeSet::new();
        for row in &self.rows {
            let manifest = row.skv14_manifest_row();
            seen.insert(manifest.row_id.clone());
            rows.push(manifest);
        }

        for corpus in SKV14_JSON_CORPORA {
            for workload in SKV14_JSON_WORKLOADS {
                let row_id = format!("json/{corpus}/{workload}/main");
                if seen.contains(&row_id) {
                    continue;
                }
                if *workload != "real_typed_struct" {
                    return Err(format!("SK-V14 manifest missing required {row_id}"));
                }
                seen.insert(row_id.clone());
                rows.push(skv14_missing_json_typed_manifest(corpus));
            }
        }
        for feature in SKV14_CSS_FEATURES {
            rows.push(skv14_css_manifest_row(feature));
        }
        Ok(rows)
    }

    pub fn render_markdown(&self) -> String {
        let mut out = String::new();
        out.push_str("# ");
        out.push_str(&self.title);
        out.push_str("\n\n");
        out.push_str(SCHEMA_V3_HEADER);
        out.push('\n');
        out.push_str(SCHEMA_V3_ALIGN);
        out.push('\n');
        for row in &self.rows {
            out.push_str(&format!(
                "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
                row.corpus,
                row.workload,
                row.outcome_id,
                row.verdict,
                row.strictness,
                row.parse_utf8,
                row.escape_complete,
                row.flaw_probe,
                row.output_plane,
                format_optional(row.track1_mbps),
                format_optional(row.track2_mbps),
                format_optional(row.competitors.sonic_strict_mbps),
                format_optional(row.competitors.sonic_lossy_mbps),
                format_optional(row.competitors.simdjson_dom_mbps),
                format_optional(row.competitors.simdjson_ondemand_mbps),
                format_optional(row.competitors.yyjson_default_mbps),
                format_optional(row.competitors.asmjson_swar_mbps),
                format_optional(row.competitors.asmjson_avx512_mbps),
                format_optional(row.competitors.rapidjson_default_mbps),
                format_optional(row.competitors.serde_json_mbps),
                row.delta_vs_skv6,
                format_delta(row.delta_vs_sonic_strict),
                format_delta(row.delta_vs_simdjson_dom),
                format_delta(row.delta_vs_yyjson),
                row.hot_leaf,
                row.signal
            ));
        }
        if !self.rows.is_empty() {
            let manifest_rows = self.skv14_manifest_rows().unwrap_or_else(|_| {
                self.rows
                    .iter()
                    .map(TelemetryRow::skv14_manifest_row)
                    .collect()
            });
            out.push('\n');
            out.push_str(&render_skv14_manifest_rows(&manifest_rows));
        }
        if !self.probe_rows.is_empty() {
            out.push_str("\n## Masking Probes\n\n");
            out.push_str("| Corpus | Probe | Mbps | ns/iter | vs Track 1 | Signal |\n");
            out.push_str("|---|---|---:|---:|---:|---|\n");
            for row in &self.probe_rows {
                out.push_str(&format!(
                    "| {} | {} | {} | {} | {} | {} |\n",
                    row.corpus,
                    row.probe,
                    format_optional(row.mbps),
                    format_optional_precise(row.ns_per_iter),
                    format_ratio(row.mbps, track1_mbps_from_ratio(row.mbps, row.vs_track1)),
                    row.signal
                ));
            }
        }
        if !self.notes.is_empty() {
            out.push_str("\n## Notes\n\n");
            for note in &self.notes {
                out.push_str("- ");
                out.push_str(note);
                out.push('\n');
            }
        }
        out
    }

    pub fn write_markdown(&self, path: &Path) -> io::Result<()> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, self.render_markdown())
    }
}

pub fn skv14_existing_results_capture_markdown(results_text: &str) -> Result<String, String> {
    let (section_start, section_end) = skv14_manifest_section_bounds(results_text)?;
    let section = &results_text[section_start..section_end];
    let mut rows = Vec::new();
    let mut seen = BTreeSet::new();

    for line in section.lines() {
        let cells = report_markdown_cells(line);
        if cells.is_empty()
            || cells[0] == "Row id"
            || cells[0] == "---"
            || !(cells[0].starts_with("json/") || cells[0].starts_with("css_l4/"))
        {
            continue;
        }
        if cells[0].starts_with("css_l4/")
            && cells.len() != 32
            && !(cells.len() == 22 && cells[0].ends_with("/direct_to_struct/main"))
        {
            continue;
        }
        if cells[0].starts_with("css_l4/")
            && cells.len() == 22
            && !legacy_css_feature_from_row_id(&cells[0])
                .is_some_and(|feature| SKV14_CSS_FEATURES.contains(&feature))
        {
            continue;
        }
        let row = match cells.len() {
            22 => skv14_manifest_row_from_legacy_cells(&cells)?,
            32 => skv14_manifest_row_from_skv14_cells(&cells)?,
            other => {
                return Err(format!(
                    "SK-V14 capture row {} expected 22 or 32 cells, saw {other}",
                    cells[0]
                ))
            }
        };
        if seen.insert(row.row_id.clone()) {
            rows.push(row);
        }
    }

    for corpus in SKV14_JSON_CORPORA {
        for workload in SKV14_JSON_WORKLOADS {
            let row_id = format!("json/{corpus}/{workload}/main");
            if seen.contains(&row_id) {
                continue;
            }
            if *workload != "real_typed_struct" {
                return Err(format!("SK-V14 capture missing required {row_id}"));
            }
            let row = skv14_missing_json_typed_manifest(corpus);
            seen.insert(row.row_id.clone());
            rows.push(row);
        }
    }
    for feature in SKV14_CSS_FEATURES {
        let row_id = format!("css_l4/{feature}/direct_to_struct/main");
        if seen.contains(&row_id) {
            continue;
        }
        let row = skv14_css_manifest_row(feature);
        seen.insert(row.row_id.clone());
        rows.push(row);
    }

    rows.sort_by(|left, right| {
        skv14_manifest_order(&left.row_id)
            .cmp(&skv14_manifest_order(&right.row_id))
            .then_with(|| left.row_id.cmp(&right.row_id))
    });
    validate_skv14_manifest_rows(&rows)?;
    let mut output = String::new();
    output.push_str(&results_text[..section_start]);
    output.push_str(&render_skv14_manifest_rows(&rows));
    output.push_str(&results_text[section_end..]);
    Ok(ensure_trailing_newline(&output.replace(
        "SK-V9 W0 telemetry: gate-json consumes the manifest below",
        "SK-V14-open telemetry: gate-json consumes the manifest below",
    )))
}

fn skv14_manifest_section_bounds(results_text: &str) -> Result<(usize, usize), String> {
    let section_start = find_heading(results_text, "## SK-V14 W0 Telemetry Manifest")
        .or_else(|| find_heading(results_text, "## SK-V9 W0 Telemetry Manifest"))
        .ok_or_else(|| {
            "RESULTS.md missing SK-V14/SK-V9 W0 Telemetry Manifest section".to_string()
        })?;
    let tail_start = results_text[section_start..]
        .find('\n')
        .map(|offset| section_start + offset)
        .unwrap_or(results_text.len());
    let section_end = results_text[tail_start..]
        .find("\n## ")
        .map(|offset| tail_start + offset)
        .unwrap_or(results_text.len());
    Ok((section_start, section_end))
}

fn find_heading(text: &str, heading: &str) -> Option<usize> {
    if text.starts_with(heading) {
        return Some(0);
    }
    text.find(&format!("\n{heading}")).map(|index| index + 1)
}

fn skv14_manifest_row_from_legacy_cells(cells: &[String]) -> Result<SkV14ManifestRow, String> {
    if cells[0].starts_with("css_l4/") {
        return skv14_css_manifest_row_from_legacy_cells(cells);
    }
    let (corpus, workload) = parse_row_id(&cells[0])?;
    let sample_count = cells[8].parse::<u64>().map_err(|error| {
        format!(
            "{} has invalid SK-V14 legacy sample count {}: {error}",
            cells[0], cells[8]
        )
    })?;
    Ok(SkV14ManifestRow {
        row_id: cells[0].clone(),
        grammar_id: cells[1].clone(),
        domain: cells[2].clone(),
        wave_id: if cells[3] == "SK-V9-open" {
            "SK-V14-open".to_string()
        } else {
            cells[3].clone()
        },
        run_id: cells[4].replacen("sk-v9-open:", "SK-V14-open:", 1),
        track1_entry_point: skv14_track1_entry_point(workload).to_string(),
        track2_entry_point: skv14_track2_entry_point(workload).to_string(),
        comparator_plane: skv14_comparator_plane(corpus, workload),
        per_iter_equality: skv14_per_iter_equality(workload, sample_count),
        audit_overlay_verdict: skv14_audit_overlay_verdict(corpus, workload).to_string(),
        audit_overlay_reference: skv14_audit_overlay_reference(corpus, workload),
        sidecar_freshness: skv14_sidecar_freshness_from_evidence(workload, &cells[21]),
        substrate_target: skv14_substrate_target(workload).to_string(),
        retention_lifetime: skv14_retention_lifetime(workload).to_string(),
        policy_owner: skv14_policy_owner(workload).to_string(),
        measured_validation_path: cells[5].clone(),
        profile_artifact: cells[6].clone(),
        sample_cost: cells[7].clone(),
        sample_count,
        build_flags: cells[9].clone(),
        host_triple: cells[10].clone(),
        feature_mask: cells[11].clone(),
        costfacts: cells[12].clone(),
        redress_entry: cells[13].clone(),
        sk_v14_open_delta: "baseline".to_string(),
        substrate_surface: cells[15].clone(),
        structural_projection_status: cells[16].clone(),
        substrate_cardinality: cells[17].clone(),
        same_wave_consumer_class: cells[18].clone(),
        track2_independence_status: cells[19].clone(),
        diagnostic_nonproducer_status: cells[20].clone(),
        comparator_evidence: cells[21].clone(),
    })
}

fn skv14_css_manifest_row_from_legacy_cells(cells: &[String]) -> Result<SkV14ManifestRow, String> {
    let feature = legacy_css_feature_from_row_id(&cells[0])
        .ok_or_else(|| format!("{} is not a valid CSS L4 manifest row", cells[0]))?;
    if !SKV14_CSS_FEATURES.contains(&feature) {
        return Err(format!("{} is not an SK-V14 CSS L4 feature row", cells[0]));
    }
    let mut row = skv14_css_manifest_row(feature);
    row.measured_validation_path = cells[5].clone();
    row.profile_artifact = cells[6].clone();
    row.sample_cost = cells[7].clone();
    row.sample_count = cells[8].parse::<u64>().map_err(|error| {
        format!(
            "{} has invalid SK-V14 legacy CSS sample count {}: {error}",
            cells[0], cells[8]
        )
    })?;
    row.build_flags = cells[9].clone();
    row.host_triple = cells[10].clone();
    row.feature_mask = cells[11].clone();
    row.costfacts = skv14_css_reconciled_costfacts(&cells[12]);
    row.redress_entry = cells[13].clone();
    row.substrate_surface = cells[15].clone();
    row.structural_projection_status = cells[16].clone();
    row.substrate_cardinality = cells[17].clone();
    row.same_wave_consumer_class = cells[18].clone();
    row.track2_independence_status = cells[19].clone();
    row.diagnostic_nonproducer_status = cells[20].clone();
    row.comparator_evidence = cells[21].clone();
    Ok(row)
}

fn legacy_css_feature_from_row_id(row_id: &str) -> Option<&str> {
    row_id
        .strip_prefix("css_l4/")
        .and_then(|tail| tail.strip_suffix("/direct_to_struct/main"))
}

fn skv14_manifest_order(row_id: &str) -> (usize, usize, usize) {
    if let Ok((corpus, workload)) = parse_row_id(row_id) {
        let corpus_index = SKV14_JSON_CORPORA
            .iter()
            .position(|candidate| *candidate == corpus)
            .unwrap_or(usize::MAX);
        let workload_index = SKV14_JSON_WORKLOADS
            .iter()
            .position(|candidate| *candidate == workload)
            .unwrap_or(usize::MAX);
        return (0, corpus_index, workload_index);
    }
    if let Some(feature) = legacy_css_feature_from_row_id(row_id) {
        let feature_index = SKV14_CSS_FEATURES
            .iter()
            .position(|candidate| *candidate == feature)
            .unwrap_or(usize::MAX);
        return (1, feature_index, 0);
    }
    (usize::MAX, usize::MAX, usize::MAX)
}

fn skv14_manifest_row_from_skv14_cells(cells: &[String]) -> Result<SkV14ManifestRow, String> {
    let sample_count = cells[18].parse::<u64>().map_err(|error| {
        format!(
            "{} has invalid SK-V14 sample count {}: {error}",
            cells[0], cells[18]
        )
    })?;
    let json_row = parse_row_id(&cells[0]).ok();
    let track1_entry_point = json_row
        .map(|(_, workload)| skv14_track1_entry_point(workload).to_string())
        .unwrap_or_else(|| cells[5].clone());
    let track2_entry_point = match json_row {
        Some((_, "direct_to_struct" | "real_typed_struct")) if sample_count == 0 => {
            cells[6].clone()
        }
        Some((_, workload)) => skv14_track2_entry_point(workload).to_string(),
        None => cells[6].clone(),
    };
    let comparator_plane = json_row
        .map(|(corpus, workload)| skv14_comparator_plane(corpus, workload))
        .unwrap_or_else(|| cells[7].clone());
    let per_iter_equality = match json_row {
        Some((_, "real_typed_struct")) if sample_count == 0 => {
            "INTRINSIC-BLOCK:missing-product-surface".to_string()
        }
        Some((_, "direct_to_struct")) if sample_count == 0 => {
            "INTRINSIC-BLOCK:missing-product-surface".to_string()
        }
        Some((_, "parse_only"))
            if cells[4].starts_with("SK-V14-W10:") || cells[31].contains("profile_direct:") =>
        {
            cells[8].clone()
        }
        Some((_, workload)) => skv14_per_iter_equality(workload, sample_count),
        None => cells[8].clone(),
    };
    let sidecar_freshness = json_row
        .map(|(_, workload)| format!("absent:not-collected-for-{workload}"))
        .unwrap_or_else(|| cells[11].clone());
    let comparator_evidence = match json_row {
        Some((_, "parse_only"))
            if cells[4].starts_with("SK-V14-W10:") || cells[31].contains("profile_direct:") =>
        {
            cells[31].clone()
        }
        Some((_, "real_typed_struct"))
            if cells[4].starts_with("SK-V14-W9:") || cells[31].contains("profile_direct:") =>
        {
            cells[31].clone()
        }
        Some((_, "direct_to_struct"))
            if sample_count == 0
                || cells[4].starts_with("SK-V14-W11A:")
                || cells[31].contains("profile_direct:") =>
        {
            cells[31].clone()
        }
        Some((corpus, workload)) => skv14_rebound_comparator_evidence(corpus, workload, &cells[31]),
        None => cells[31].clone(),
    };
    Ok(SkV14ManifestRow {
        row_id: cells[0].clone(),
        grammar_id: cells[1].clone(),
        domain: cells[2].clone(),
        wave_id: cells[3].clone(),
        run_id: cells[4].clone(),
        track1_entry_point,
        track2_entry_point,
        comparator_plane,
        per_iter_equality,
        audit_overlay_verdict: cells[9].clone(),
        audit_overlay_reference: cells[10].clone(),
        sidecar_freshness,
        substrate_target: cells[12].clone(),
        retention_lifetime: cells[13].clone(),
        policy_owner: cells[14].clone(),
        measured_validation_path: cells[15].clone(),
        profile_artifact: cells[16].clone(),
        sample_cost: cells[17].clone(),
        sample_count,
        build_flags: cells[19].clone(),
        host_triple: cells[20].clone(),
        feature_mask: cells[21].clone(),
        costfacts: if cells[0].starts_with("css_l4/") {
            skv14_css_reconciled_costfacts(&cells[22])
        } else {
            cells[22].clone()
        },
        redress_entry: cells[23].clone(),
        sk_v14_open_delta: cells[24].clone(),
        substrate_surface: cells[25].clone(),
        structural_projection_status: cells[26].clone(),
        substrate_cardinality: cells[27].clone(),
        same_wave_consumer_class: cells[28].clone(),
        track2_independence_status: cells[29].clone(),
        diagnostic_nonproducer_status: cells[30].clone(),
        comparator_evidence,
    })
}

fn skv14_sidecar_freshness_from_evidence(workload: &str, evidence: &str) -> String {
    let _ = evidence;
    format!("absent:not-collected-for-{workload}")
}

fn skv14_rebound_comparator_evidence(corpus: &str, workload: &str, existing: &str) -> String {
    let native_plane = match workload {
        "parse_only" => "parse_only/sonic_rs::Skipper",
        "direct_to_struct" => "direct strict product",
        "real_typed_struct" => "typed direct",
        _ => "unknown",
    };
    let serde_plane = match workload {
        "parse_only" => "DOM",
        _ => native_plane,
    };
    let (sonic_bench, serde_bench, lossy_bench) = match workload {
        "parse_only" => ("sonic_rs_skipper", "serde_json", Some("sonic_rs_lossy")),
        "direct_to_struct" => (
            "sonic_rs_direct_to_struct",
            "serde_json_direct_to_struct",
            None,
        ),
        "real_typed_struct" => (
            "sonic_rs_real_typed_struct",
            "serde_json_real_typed_struct",
            None,
        ),
        _ => ("sonic_rs_skipper", "serde_json", None),
    };
    let mut entries = vec![
        format!(
            "sonic_rs_strict[plane={native_plane},strictness=strict,freshness=same-run-native,sidecar=n/a,mbps={},source=criterion:json_{corpus}/{sonic_bench}/new/estimates.json]",
            comparator_mbps(existing, "sonic_rs_strict")
        ),
        format!(
            "serde_json[plane={serde_plane},strictness=strict,freshness=same-run-native,sidecar=n/a,mbps={},source=criterion:json_{corpus}/{serde_bench}/new/estimates.json]",
            comparator_mbps(existing, "serde_json")
        ),
    ];
    if let Some(lossy_bench) = lossy_bench {
        entries.push(format!(
            "sonic_rs_lossy[plane=DOM,strictness=permissive,freshness=same-run-native,sidecar=n/a,mbps={},source=criterion:json_{corpus}/{lossy_bench}/new/estimates.json]",
            comparator_mbps(existing, "sonic_rs_lossy")
        ));
    }
    for id in SK_V8_SIDECAR_COMPARATORS {
        entries.push(format!(
            "{id}[plane=DOM,strictness=strict,freshness=absent:not-collected-for-{workload},sidecar=absent:not-collected-for-{workload},mbps=n/a,source=absence:w1:{corpus}:{workload}:{id}]"
        ));
    }
    entries.join("; ")
}

fn comparator_mbps(existing: &str, comparator_id: &str) -> String {
    existing
        .split("; ")
        .find_map(|entry| {
            let rest = entry.strip_prefix(comparator_id)?.strip_prefix('[')?;
            rest.split(',')
                .find_map(|field| field.strip_prefix("mbps=").map(str::to_string))
        })
        .filter(|value| value != "n/a")
        .unwrap_or_else(|| "1".to_string())
}

fn render_skv14_manifest_rows(rows: &[SkV14ManifestRow]) -> String {
    let mut out = String::new();
    out.push_str("## SK-V14 W0 Telemetry Manifest\n\n");
    out.push_str(SKV14_W0_MANIFEST_HEADER);
    out.push('\n');
    out.push_str(SKV14_W0_MANIFEST_ALIGN);
    out.push('\n');
    for telemetry in rows {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            cell(&telemetry.row_id),
            cell(&telemetry.grammar_id),
            cell(&telemetry.domain),
            cell(&telemetry.wave_id),
            cell(&telemetry.run_id),
            cell(&telemetry.track1_entry_point),
            cell(&telemetry.track2_entry_point),
            cell(&telemetry.comparator_plane),
            cell(&telemetry.per_iter_equality),
            cell(&telemetry.audit_overlay_verdict),
            cell(&telemetry.audit_overlay_reference),
            cell(&telemetry.sidecar_freshness),
            cell(&telemetry.substrate_target),
            cell(&telemetry.retention_lifetime),
            cell(&telemetry.policy_owner),
            cell(&telemetry.measured_validation_path),
            cell(&telemetry.profile_artifact),
            cell(&telemetry.sample_cost),
            telemetry.sample_count,
            cell(&telemetry.build_flags),
            cell(&telemetry.host_triple),
            cell(&telemetry.feature_mask),
            cell(&telemetry.costfacts),
            cell(&telemetry.redress_entry),
            cell(&telemetry.sk_v14_open_delta),
            cell(&telemetry.substrate_surface),
            cell(&telemetry.structural_projection_status),
            cell(&telemetry.substrate_cardinality),
            cell(&telemetry.same_wave_consumer_class),
            cell(&telemetry.track2_independence_status),
            cell(&telemetry.diagnostic_nonproducer_status),
            cell(&telemetry.comparator_evidence)
        ));
    }
    out
}

fn report_markdown_cells(line: &str) -> Vec<String> {
    if !line.trim_start().starts_with('|') {
        return Vec::new();
    }
    let inner = line.trim().trim_matches('|');
    let mut cells = Vec::new();
    let mut current = String::new();
    let mut escaped = false;
    for ch in inner.chars() {
        if escaped {
            if ch == '|' {
                current.push('|');
            } else {
                current.push('\\');
                current.push(ch);
            }
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
        } else if ch == '|' {
            cells.push(current.trim().to_string());
            current.clear();
        } else {
            current.push(ch);
        }
    }
    if escaped {
        current.push('\\');
    }
    cells.push(current.trim().to_string());
    cells
}

fn ensure_trailing_newline(text: &str) -> String {
    let mut out = text.trim_end().to_string();
    out.push('\n');
    out
}

pub struct SkV8OpenBaseline {
    pub row_id: &'static str,
    pub outcome_id: &'static str,
    pub verdict: &'static str,
    pub track1_mbps: f64,
    pub track2_mbps: f64,
}

pub const SK_V9_OPEN_RUN_ID_PREFIX: &str = "SK-V14-open:criterion-fnv64-";

fn is_skv9_open_run_id(run_id: &str) -> bool {
    let Some(suffix) = run_id.strip_prefix(SK_V9_OPEN_RUN_ID_PREFIX) else {
        return false;
    };
    suffix.len() == 16
        && suffix
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
}

macro_rules! sk_v8_open_baseline {
    ($row_id:literal, $outcome_id:literal, $verdict:literal, $track1:literal, $track2:literal) => {
        SkV8OpenBaseline {
            row_id: $row_id,
            outcome_id: $outcome_id,
            verdict: $verdict,
            track1_mbps: $track1,
            track2_mbps: $track2,
        }
    };
}

pub const SK_V8_OPEN_BASELINE: &[SkV8OpenBaseline] = &[
    sk_v8_open_baseline!("json/twitter/parse_only/main", "S", "NO-GO", 9581.0, 9741.0),
    sk_v8_open_baseline!(
        "json/twitter/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        11859.0,
        9881.0
    ),
    sk_v8_open_baseline!(
        "json/twitter/real_typed_struct/main",
        "A",
        "GO",
        15333.0,
        14516.0
    ),
    sk_v8_open_baseline!(
        "json/citm_catalog/parse_only/main",
        "S",
        "NO-GO",
        28644.0,
        19214.0
    ),
    sk_v8_open_baseline!(
        "json/citm_catalog/direct_to_struct/main",
        "A",
        "GO",
        21151.0,
        19434.0
    ),
    sk_v8_open_baseline!(
        "json/citm_catalog/real_typed_struct/main",
        "A",
        "GO",
        35102.0,
        19143.0
    ),
    sk_v8_open_baseline!(
        "json/canada/parse_only/main",
        "L",
        "NO-GO",
        15497.0,
        12171.0
    ),
    sk_v8_open_baseline!(
        "json/canada/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        6586.0,
        9769.0
    ),
    sk_v8_open_baseline!(
        "json/apache_builds/parse_only/main",
        "S",
        "NO-GO",
        12694.0,
        11715.0
    ),
    sk_v8_open_baseline!(
        "json/apache_builds/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        8306.0,
        7796.0
    ),
    sk_v8_open_baseline!(
        "json/apache_builds/real_typed_struct/main",
        "A",
        "GO",
        8174.0,
        6728.0
    ),
    sk_v8_open_baseline!(
        "json/github_events/parse_only/main",
        "S",
        "NO-GO",
        10689.0,
        10073.0
    ),
    sk_v8_open_baseline!(
        "json/github_events/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        9088.0,
        7337.0
    ),
    sk_v8_open_baseline!(
        "json/update_center/parse_only/main",
        "S",
        "NO-GO",
        11926.0,
        9312.0
    ),
    sk_v8_open_baseline!(
        "json/update_center/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        7863.0,
        7514.0
    ),
    sk_v8_open_baseline!(
        "json/update_center/real_typed_struct/main",
        "A",
        "GO",
        11958.0,
        10367.0
    ),
    sk_v8_open_baseline!("json/mesh/parse_only/main", "S", "NO-GO", 9367.0, 10000.0),
    sk_v8_open_baseline!(
        "json/mesh/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        8640.0,
        9049.0
    ),
    sk_v8_open_baseline!(
        "json/mesh/real_typed_struct/main",
        "A",
        "GO",
        9623.0,
        7674.0
    ),
    sk_v8_open_baseline!("json/random/parse_only/main", "S", "NO-GO", 10011.0, 8018.0),
    sk_v8_open_baseline!(
        "json/random/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        7751.0,
        6952.0
    ),
    sk_v8_open_baseline!(
        "json/gsoc-2018/parse_only/main",
        "S",
        "NO-GO",
        23209.0,
        21857.0
    ),
    sk_v8_open_baseline!(
        "json/gsoc-2018/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        15042.0,
        14380.0
    ),
    sk_v8_open_baseline!(
        "json/marine_ik/parse_only/main",
        "S",
        "NO-GO",
        13100.0,
        12164.0
    ),
    sk_v8_open_baseline!(
        "json/marine_ik/direct_to_struct/main",
        "A",
        "GO",
        9357.0,
        9488.0
    ),
    sk_v8_open_baseline!(
        "json/marine_ik/real_typed_struct/main",
        "A",
        "GO",
        11783.0,
        8321.0
    ),
    sk_v8_open_baseline!(
        "json/instruments/parse_only/main",
        "S",
        "NO-GO",
        13320.0,
        11351.0
    ),
    sk_v8_open_baseline!(
        "json/instruments/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        8494.0,
        8766.0
    ),
    sk_v8_open_baseline!(
        "json/numbers/parse_only/main",
        "S",
        "NO-GO",
        12818.0,
        13537.0
    ),
    sk_v8_open_baseline!(
        "json/numbers/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        9773.0,
        6966.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_mixed/parse_only/main",
        "S",
        "NO-GO",
        6390.0,
        4970.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_mixed/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        3596.0,
        3694.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_escapes/parse_only/main",
        "S",
        "NO-GO",
        12731.0,
        8521.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_escapes/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        4020.0,
        4016.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_basic/parse_only/main",
        "S",
        "NO-GO",
        11189.0,
        10040.0
    ),
    sk_v8_open_baseline!(
        "json/unicode_basic/direct_to_struct/main",
        "A",
        "GO",
        9363.0,
        8420.0
    ),
    sk_v8_open_baseline!(
        "json/distinct_values/parse_only/main",
        "S",
        "NO-GO",
        10279.0,
        6457.0
    ),
    sk_v8_open_baseline!(
        "json/distinct_values/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        4438.0,
        4151.0
    ),
    sk_v8_open_baseline!(
        "json/y_string_unicode/parse_only/main",
        "S",
        "NO-GO",
        5577.0,
        5480.0
    ),
    sk_v8_open_baseline!(
        "json/y_string_unicode/direct_to_struct/main",
        "N-direct",
        "NO-GO",
        4828.0,
        3563.0
    ),
];

pub const SK_V8_SIDECAR_COMPARATORS: &[&str] = &[
    "simdjson_dom",
    "simdjson_ondemand",
    "yyjson_default",
    "asmjson_swar",
    "asmjson_avx512",
    "rapidjson_default",
];
const SK_V8_NATIVE_STRICT_COMPARATORS: &[&str] = &["sonic_rs_strict", "serde_json"];
const SK_V8_NATIVE_FLAW_PROBES: &[&str] = &["sonic_rs_lossy"];

pub fn sk_v8_open_baseline(row_id: &str) -> Option<&'static SkV8OpenBaseline> {
    SK_V8_OPEN_BASELINE
        .iter()
        .find(|baseline| baseline.row_id == row_id)
}

fn validate_w0_outcome(row_id: &str, outcome_id: &str) -> Result<(), String> {
    if gate::parse_outcome_id(outcome_id).is_none() {
        return Err(format!("{row_id} has unsupported outcome {outcome_id}"));
    }
    if !matches!(
        outcome_id,
        "A" | "C" | "G" | "I" | "J" | "K" | "L" | "M" | "N-direct" | "S"
    ) {
        return Err(format!("{row_id} has non-W0 outcome {outcome_id}"));
    }
    Ok(())
}

fn w0_allows_fresh_diagnostic_outcome(baseline: &str, observed: &str) -> bool {
    // W0 opens fresh telemetry; numeric diagnostic NO-GO reasons may relabel,
    // but correctness, invalid, direct, product, and admitted rows stay exact.
    matches!(baseline, "G" | "L" | "M" | "S") && matches!(observed, "G" | "L" | "M" | "S")
}

fn direct_contract_row_changed(row: &TelemetryRow, baseline: &SkV8OpenBaseline) -> bool {
    baseline.outcome_id == "N-direct"
        && row.workload == "direct_to_struct"
        && (row.outcome_id != baseline.outcome_id || row.verdict != baseline.verdict)
}

fn validate_direct_row_movement(
    row: &TelemetryRow,
    baseline: &SkV8OpenBaseline,
) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    if baseline.outcome_id != "N-direct" || row.workload != "direct_to_struct" {
        return Err(format!("{row_id} is not a direct contract row"));
    }
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} direct contract admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    let Some(floor) = sk_v10_direct_floor(&row.corpus) else {
        return Err(format!("{row_id} has no SK-V10 direct floor"));
    };
    let (Some(track1), Some(track2)) = (row.track1_mbps, row.track2_mbps) else {
        return Err(format!(
            "{row_id} direct contract lacks Track 1/Track 2 Mbps"
        ));
    };
    if (track1 < floor || track2 < floor) && !direct_track1_sota_reopen_passes(row, track1) {
        return Err(format!(
            "{row_id} direct contract floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, floor {floor:.0}"
        ));
    }
    if row.output_plane != "digest" {
        return Err(format!(
            "{row_id} direct contract output plane {} is not digest",
            row.output_plane
        ));
    }
    if row.strictness != "strict" {
        return Err(format!(
            "{row_id} direct contract strictness {} is not strict",
            row.strictness
        ));
    }
    if row.parse_utf8 != "measured-row" || row.sk_v8.measured_validation_path != "measured-row" {
        return Err(format!(
            "{row_id} direct contract is not measured-row validated"
        ));
    }
    if row.escape_complete != "yes" {
        return Err(format!(
            "{row_id} direct contract has incomplete escape validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified" {
        return Err(format!(
            "{row_id} direct contract lacks Track 2 independence"
        ));
    }
    if row.sk_v8.same_wave_consumer_class == "gate_only" {
        return Err(format!("{row_id} direct contract has only a gate consumer"));
    }
    if row.sk_v8.redress_entry == "none" {
        return Err(format!("{row_id} direct contract lacks REDRESS provenance"));
    }
    if row.sk_v8.wave_id == "SK-V9-open" {
        return Err(format!(
            "{row_id} direct contract still uses SK-V9-open wave id"
        ));
    }
    if row.sk_v8.run_id.trim().is_empty() || !is_skv9_open_run_id(&row.sk_v8.run_id) {
        return Err(format!(
            "{row_id} direct contract has invalid run_id {}",
            row.sk_v8.run_id
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

const W6_GITHUB_EVENTS_TYPED_ROW_ID: &str = "json/github_events/real_typed_struct/main";
const W13_NUMBERS_TYPED_ROW_ID: &str = "json/numbers/real_typed_struct/main";
const W13_UNICODE_BASIC_TYPED_ROW_ID: &str = "json/unicode_basic/real_typed_struct/main";
const W13_RANDOM_TYPED_ROW_ID: &str = "json/random/real_typed_struct/main";
const W13_INSTRUMENTS_TYPED_ROW_ID: &str = "json/instruments/real_typed_struct/main";
const W15_UPDATE_CENTER_TYPED_ROW_ID: &str = "json/update_center/real_typed_struct/main";
const W11L_Y_STRING_TYPED_ROW_ID: &str = "json/y_string_unicode/real_typed_struct/main";
const W11L_Y_STRING_DIRECT_ROW_ID: &str = "json/y_string_unicode/direct_to_struct/main";
const W11N_UNICODE_MIXED_TYPED_ROW_ID: &str = "json/unicode_mixed/real_typed_struct/main";
const W11N_UNICODE_MIXED_DIRECT_ROW_ID: &str = "json/unicode_mixed/direct_to_struct/main";
const W11O_GSOC_TYPED_ROW_ID: &str = "json/gsoc-2018/real_typed_struct/main";
const W11O_GSOC_DIRECT_ROW_ID: &str = "json/gsoc-2018/direct_to_struct/main";
const W11U_UNICODE_ESCAPES_TYPED_ROW_ID: &str = "json/unicode_escapes/real_typed_struct/main";
const W11U_UNICODE_ESCAPES_DIRECT_ROW_ID: &str = "json/unicode_escapes/direct_to_struct/main";
fn validate_json_parse_only_admission_row(
    row: &TelemetryRow,
    spec: &JsonParseOnlyAdmissionSpec,
) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} {} parse row admits only A / GO, saw {} / {}",
            spec.label, row.outcome_id, row.verdict
        ));
    }
    if row.sk_v8.row_id != spec.row_id || row.corpus != spec.corpus || row.workload != "parse_only"
    {
        return Err(format!(
            "{row_id} is not the {} {} parse row",
            spec.label, spec.corpus
        ));
    }
    if row.output_plane != "parse_only" {
        return Err(format!(
            "{row_id} {} parse output plane {} is not parse_only",
            spec.label, row.output_plane
        ));
    }
    if row.strictness != "strict" {
        return Err(format!(
            "{row_id} {} parse strictness {} is not strict",
            spec.label, row.strictness
        ));
    }
    if row.parse_utf8 != "measured-row" || row.sk_v8.measured_validation_path != "measured-row" {
        return Err(format!(
            "{row_id} {} parse row is not measured-row validated",
            spec.label
        ));
    }
    if row.escape_complete != "yes" {
        return Err(format!(
            "{row_id} {} parse row has incomplete escape validation",
            spec.label
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified" {
        return Err(format!(
            "{row_id} {} parse row lacks Track 2 independence",
            spec.label
        ));
    }
    if row.sk_v8.same_wave_consumer_class != "generated_json_parse_only_contract" {
        return Err(format!(
            "{row_id} {} parse row consumer {} is not generated_json_parse_only_contract",
            spec.label, row.sk_v8.same_wave_consumer_class
        ));
    }
    if row.sk_v8.redress_entry != spec.redress_entry || row.sk_v8.wave_id != spec.wave_id {
        return Err(format!(
            "{row_id} {} parse row lacks REDRESS/W10 provenance",
            spec.label
        ));
    }
    if row.sk_v8.audit_overlay_verdict != "AUDIT-SUSTAINED" {
        return Err(format!(
            "{row_id} {} parse row audit overlay {} is not AUDIT-SUSTAINED",
            spec.label, row.sk_v8.audit_overlay_verdict
        ));
    }
    if row.sk_v8.sk_v9_open_delta != json_parse_only_open_delta(spec) {
        return Err(format!(
            "{row_id} {} parse row delta {} is not authorized SK-V14 parse-only admission",
            spec.label, row.sk_v8.sk_v9_open_delta
        ));
    }
    if row.sk_v8.run_id.trim().is_empty() || !row.sk_v8.run_id.starts_with(spec.run_id_prefix) {
        return Err(format!(
            "{row_id} {} parse row has invalid run_id {}",
            spec.label, row.sk_v8.run_id
        ));
    }
    let expected = w0_substrate_tuple(&row.workload).ok_or_else(|| {
        format!(
            "{row_id} has unsupported {} parse workload {}",
            spec.label, row.workload
        )
    })?;
    let actual = (
        row.sk_v8.substrate_surface.as_str(),
        row.sk_v8.structural_projection_status.as_str(),
        row.sk_v8.substrate_cardinality.as_str(),
    );
    if actual != expected {
        return Err(format!(
            "{row_id} {} parse row substrate tuple {:?} does not match {:?}",
            spec.label, actual, expected
        ));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} {} parse row lacks Track 1, Track 2, or sonic Mbps",
            spec.label
        ));
    };
    if track1 <= sonic + 1.0 {
        return Err(format!(
            "{row_id} {} parse row misses sonic+1: Track 1 {track1:.0}, sonic {sonic:.0}",
            spec.label
        ));
    }
    if !track2.is_finite() || track2 <= 0.0 {
        return Err(format!(
            "{row_id} {} parse row has invalid Track 2 oracle {track2:.0}",
            spec.label
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w6_github_events_typed_row(row: &TelemetryRow) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} W6 typed contract admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != "github_events" || row.workload != "real_typed_struct" {
        return Err(format!("{row_id} is not the W6 github_events typed row"));
    }
    if row.output_plane != "typed direct" {
        return Err(format!(
            "{row_id} W6 typed output plane {} is not typed direct",
            row.output_plane
        ));
    }
    if row.strictness != "strict" {
        return Err(format!(
            "{row_id} W6 typed strictness {} is not strict",
            row.strictness
        ));
    }
    if row.parse_utf8 != "measured-row" || row.sk_v8.measured_validation_path != "measured-row" {
        return Err(format!(
            "{row_id} W6 typed row is not measured-row validated"
        ));
    }
    if row.escape_complete != "yes" {
        return Err(format!(
            "{row_id} W6 typed row has incomplete escape validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified" {
        return Err(format!("{row_id} W6 typed row lacks Track 2 independence"));
    }
    if row.sk_v8.same_wave_consumer_class != "gate_json_typed_contract" {
        return Err(format!(
            "{row_id} W6 typed row consumer {} is not gate_json_typed_contract",
            row.sk_v8.same_wave_consumer_class
        ));
    }
    if row.sk_v8.redress_entry != "REDRESS-105" || row.sk_v8.wave_id != "SK-V10-W6" {
        return Err(format!("{row_id} W6 typed row lacks REDRESS/W6 provenance"));
    }
    if row.sk_v8.sk_v9_open_delta != "typed-row-added" {
        return Err(format!(
            "{row_id} W6 typed row delta {} is not typed-row-added",
            row.sk_v8.sk_v9_open_delta
        ));
    }
    if row.sk_v8.run_id.trim().is_empty() || !is_skv9_open_run_id(&row.sk_v8.run_id) {
        return Err(format!(
            "{row_id} W6 typed row has invalid run_id {}",
            row.sk_v8.run_id
        ));
    }
    let expected = w0_substrate_tuple(&row.workload).ok_or_else(|| {
        format!(
            "{row_id} has unsupported W6 typed workload {}",
            row.workload
        )
    })?;
    let actual = (
        row.sk_v8.substrate_surface.as_str(),
        row.sk_v8.structural_projection_status.as_str(),
        row.sk_v8.substrate_cardinality.as_str(),
    );
    if actual != expected {
        return Err(format!(
            "{row_id} W6 typed row substrate tuple {:?} does not match {:?}",
            actual, expected
        ));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} W6 typed row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    let floor = (sonic / 1.10).ceil();
    if track1 < floor || track2 < floor {
        return Err(format!(
            "{row_id} W6 typed floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, floor {floor:.0}"
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w13_numbers_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "numbers",
        "REDRESS-145",
        "SK-V13-W13.1",
        "W13.1",
        "typed-row-added",
    )
}

fn validate_w13_unicode_basic_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "unicode_basic",
        "REDRESS-146",
        "SK-V13-W13.2",
        "W13.2",
        "typed-row-added",
    )
}

fn validate_w13_random_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "random",
        "REDRESS-147",
        "SK-V13-W13.3",
        "W13.3",
        "typed-row-added",
    )
}

fn validate_w13_instruments_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "instruments",
        "REDRESS-148",
        "SK-V13-W13.4",
        "W13.4",
        "typed-row-added",
    )
}

fn validate_w15_update_center_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "update_center",
        "REDRESS-160",
        "SK-V13-W15.1",
        "W15.1",
        "typed-plugin-fast-path",
    )
}

fn validate_w11l_y_string_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "y_string_unicode",
        "none:SK-V14-W11L-admit",
        "SK-V14-W11L",
        "W11L",
        "admitted:SK-V14-W11L-decoded-token-product",
    )
}

fn validate_w11l_y_string_direct_row(row: &TelemetryRow) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} W11L direct token product admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != "y_string_unicode" || row.workload != "direct_to_struct" {
        return Err(format!(
            "{row_id} is not the W11L y_string_unicode direct row"
        ));
    }
    if row.output_plane != "direct strict product" {
        return Err(format!(
            "{row_id} W11L direct output plane {} is not direct strict product",
            row.output_plane
        ));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.sk_v8.measured_validation_path != "measured-row"
        || row.escape_complete != "yes"
    {
        return Err(format!(
            "{row_id} W11L direct row lacks strict measured validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified"
        || row.sk_v8.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
    {
        return Err(format!(
            "{row_id} W11L direct row lacks independent strict-product consumer"
        ));
    }
    if row.sk_v8.redress_entry != "none:SK-V14-W11L-admit"
        || row.sk_v8.wave_id != "SK-V14-W11L"
        || row.sk_v8.sk_v9_open_delta != "admitted:SK-V14-W11L-decoded-token-product"
    {
        return Err(format!("{row_id} W11L direct row lacks W11L provenance"));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} W11L direct row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    if track1 <= sonic + 1.0 || !track2.is_finite() {
        return Err(format!(
            "{row_id} W11L direct floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, sonic+1 {:.0}",
            sonic + 1.0
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w11n_unicode_mixed_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "unicode_mixed",
        "none:SK-V14-W11N-admit",
        "SK-V14-W11N",
        "W11N",
        "admitted:SK-V14-W11N-unicode-mixed-decoded-token-product",
    )
}

fn validate_w11n_unicode_mixed_direct_row(row: &TelemetryRow) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} W11N direct token product admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != "unicode_mixed" || row.workload != "direct_to_struct" {
        return Err(format!("{row_id} is not the W11N unicode_mixed direct row"));
    }
    if row.output_plane != "direct strict product" {
        return Err(format!(
            "{row_id} W11N direct output plane {} is not direct strict product",
            row.output_plane
        ));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.sk_v8.measured_validation_path != "measured-row"
        || row.escape_complete != "yes"
    {
        return Err(format!(
            "{row_id} W11N direct row lacks strict measured validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified"
        || row.sk_v8.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
    {
        return Err(format!(
            "{row_id} W11N direct row lacks independent strict-product consumer"
        ));
    }
    if row.sk_v8.redress_entry != "none:SK-V14-W11N-admit"
        || row.sk_v8.wave_id != "SK-V14-W11N"
        || row.sk_v8.sk_v9_open_delta != "admitted:SK-V14-W11N-unicode-mixed-decoded-token-product"
    {
        return Err(format!("{row_id} W11N direct row lacks W11N provenance"));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} W11N direct row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    if track1 <= sonic + 1.0 || !track2.is_finite() {
        return Err(format!(
            "{row_id} W11N direct floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, sonic+1 {:.0}",
            sonic + 1.0
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w11o_gsoc_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "gsoc-2018",
        "none:SK-V14-W11O-admit",
        "SK-V14-W11O",
        "W11O",
        "admitted:SK-V14-W11O-gsoc-decoded-token-product",
    )
}

fn validate_w11o_gsoc_direct_row(row: &TelemetryRow) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} W11O direct token product admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != "gsoc-2018" || row.workload != "direct_to_struct" {
        return Err(format!("{row_id} is not the W11O gsoc-2018 direct row"));
    }
    if row.output_plane != "direct strict product" {
        return Err(format!(
            "{row_id} W11O direct output plane {} is not direct strict product",
            row.output_plane
        ));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.sk_v8.measured_validation_path != "measured-row"
        || row.escape_complete != "yes"
    {
        return Err(format!(
            "{row_id} W11O direct row lacks strict measured validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified"
        || row.sk_v8.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
    {
        return Err(format!(
            "{row_id} W11O direct row lacks independent strict-product consumer"
        ));
    }
    if row.sk_v8.redress_entry != "none:SK-V14-W11O-admit"
        || row.sk_v8.wave_id != "SK-V14-W11O"
        || row.sk_v8.sk_v9_open_delta != "admitted:SK-V14-W11O-gsoc-decoded-token-product"
    {
        return Err(format!("{row_id} W11O direct row lacks W11O provenance"));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} W11O direct row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    if track1 <= sonic + 1.0 || !track2.is_finite() {
        return Err(format!(
            "{row_id} W11O direct floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, sonic+1 {:.0}",
            sonic + 1.0
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w11u_unicode_escapes_typed_row(row: &TelemetryRow) -> Result<(), String> {
    validate_w13_typed_row(
        row,
        "unicode_escapes",
        "none:SK-V14-W11U-admit",
        "SK-V14-W11U",
        "W11U",
        "admitted:SK-V14-W11U-unicode-escapes-raw-lexeme-product",
    )
}

fn validate_w11u_unicode_escapes_direct_row(row: &TelemetryRow) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} W11U direct raw-lexeme product admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != "unicode_escapes" || row.workload != "direct_to_struct" {
        return Err(format!(
            "{row_id} is not the W11U unicode_escapes direct row"
        ));
    }
    if row.output_plane != "direct strict product" {
        return Err(format!(
            "{row_id} W11U direct output plane {} is not direct strict product",
            row.output_plane
        ));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.sk_v8.measured_validation_path != "measured-row"
        || row.escape_complete != "yes"
    {
        return Err(format!(
            "{row_id} W11U direct row lacks strict measured validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified"
        || row.sk_v8.same_wave_consumer_class != "gate_json_direct_strict_product_contract"
    {
        return Err(format!(
            "{row_id} W11U direct row lacks independent strict-product consumer"
        ));
    }
    if row.sk_v8.redress_entry != "none:SK-V14-W11U-admit"
        || row.sk_v8.wave_id != "SK-V14-W11U"
        || row.sk_v8.sk_v9_open_delta != "admitted:SK-V14-W11U-unicode-escapes-raw-lexeme-product"
    {
        return Err(format!("{row_id} W11U direct row lacks W11U provenance"));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} W11U direct row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    if track1 <= sonic + 1.0 || !track2.is_finite() {
        return Err(format!(
            "{row_id} W11U direct floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, sonic+1 {:.0}",
            sonic + 1.0
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_w13_typed_row(
    row: &TelemetryRow,
    corpus: &str,
    redress_entry: &str,
    wave_id: &str,
    label: &str,
    expected_delta: &str,
) -> Result<(), String> {
    let row_id = row.sk_v8.row_id.as_str();
    row.validate_schema_v3()?;
    validate_w0_row_identity(row)?;
    validate_w0_outcome(row_id, &row.outcome_id)?;
    if row.outcome_id != "A" || row.verdict != "GO" {
        return Err(format!(
            "{row_id} {label} typed contract admits only A / GO, saw {} / {}",
            row.outcome_id, row.verdict
        ));
    }
    if row.corpus != corpus || row.workload != "real_typed_struct" {
        return Err(format!("{row_id} is not the {label} {corpus} typed row"));
    }
    if row.output_plane != "typed direct" {
        return Err(format!(
            "{row_id} {label} typed output plane {} is not typed direct",
            row.output_plane
        ));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.sk_v8.measured_validation_path != "measured-row"
        || row.escape_complete != "yes"
    {
        return Err(format!(
            "{row_id} {label} typed row lacks strict measured validation"
        ));
    }
    if row.sk_v8.track2_independence_status != "independent_verified"
        || row.sk_v8.same_wave_consumer_class != "gate_json_typed_contract"
    {
        return Err(format!(
            "{row_id} {label} typed row lacks independent gate consumer"
        ));
    }
    if row.sk_v8.redress_entry != redress_entry || row.sk_v8.wave_id != wave_id {
        return Err(format!(
            "{row_id} {label} typed row lacks REDRESS/W13 provenance"
        ));
    }
    if row.sk_v8.sk_v9_open_delta != expected_delta {
        return Err(format!(
            "{row_id} {label} typed row delta {} is not {expected_delta}",
            row.sk_v8.sk_v9_open_delta,
        ));
    }
    let (Some(track1), Some(track2), Some(sonic)) = (
        row.track1_mbps,
        row.track2_mbps,
        row.competitors.sonic_strict_mbps,
    ) else {
        return Err(format!(
            "{row_id} {label} typed row lacks Track 1, Track 2, or sonic Mbps"
        ));
    };
    if track1 <= sonic + 1.0 || !track2.is_finite() {
        return Err(format!(
            "{row_id} {label} typed floor miss: Track 1 {track1:.0}, Track 2 {track2:.0}, sonic+1 {:.0}",
            sonic + 1.0
        ));
    }
    validate_w0_profile_artifact(row_id, &row.sk_v8.profile_artifact)?;
    validate_w0_hot_leaf(row_id, &row.hot_leaf, &row.sk_v8.profile_artifact)?;
    validate_comparator_evidence(row_id, &row.workload, &row.sk_v8.comparators)?;
    Ok(())
}

fn validate_existing_typed_maintain_floors(report: &Report) -> Result<(), String> {
    for row in &report.rows {
        if row.workload != "real_typed_struct" {
            continue;
        }
        let Some(floor) = sk_v10_typed_maintain_floor(&row.corpus) else {
            continue;
        };
        let Some(track1) = row.track1_mbps else {
            return Err(format!(
                "{} missing typed maintain Track 1",
                row.sk_v8.row_id
            ));
        };
        if track1 < floor {
            return Err(format!(
                "{} typed maintain floor miss: Track 1 {track1:.0}, floor {floor:.0}",
                row.sk_v8.row_id
            ));
        }
    }
    Ok(())
}

fn sk_v10_typed_maintain_floor(corpus: &str) -> Option<f64> {
    match corpus {
        "twitter" => Some(14_424.0),
        "citm_catalog" => Some(20_053.0),
        "apache_builds" => Some(7_373.0),
        "update_center" => Some(11_365.0),
        "mesh" => Some(8_428.0),
        "marine_ik" => Some(7_369.0),
        _ => None,
    }
}

fn sk_v10_direct_floor(corpus: &str) -> Option<f64> {
    match corpus {
        "twitter" => Some(13_840.0),
        "canada" => Some(10_977.0),
        "apache_builds" => Some(10_020.0),
        "github_events" => Some(14_364.0),
        "update_center" => Some(10_160.0),
        "mesh" => Some(8_916.0),
        "random" => Some(7_734.0),
        "gsoc-2018" => Some(20_980.0),
        "instruments" => Some(11_086.0),
        "numbers" => Some(11_788.0),
        "unicode_mixed" => Some(9_314.0),
        "unicode_escapes" => Some(12_527.0),
        "distinct_values" => Some(10_022.0),
        "y_string_unicode" => Some(8_027.0),
        _ => None,
    }
}

fn direct_track1_sota_reopen_passes(row: &TelemetryRow, track1: f64) -> bool {
    row.corpus == "mesh"
        && row.sk_v8.wave_id == "SK-V13-W11.3"
        && row.sk_v8.redress_entry == "REDRESS-143"
        && row.sk_v8.same_wave_consumer_class == "direct_sink_stack_specialization"
        && row
            .competitors
            .sonic_strict_mbps
            .is_some_and(|sonic| track1 > sonic + 1.0)
}

fn validate_w0_profile_artifact(row_id: &str, profile_artifact: &str) -> Result<(), String> {
    if let Some(rest) = profile_artifact.strip_prefix("profile_direct-cold:") {
        if matches!(
            rest,
            "restart/skinny/tranches/sk-v14/research/skv14-W10-profile-direct.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W10R-parse-only-profile-direct.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W10S-parse-only-string-end-profile-direct.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W10T-parse-only-open-sweep.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W10V-parse-only-current-head-resweep.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W10W-parse-only-iterative-stack.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W11W-parse-only-memchr.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W11L-y-string-token-product.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W11N-unicode-mixed-decoded-token-product.tsv"
                | "restart/skinny/tranches/sk-v14/research/skv14-W11O-gsoc-decoded-token-product.tsv"
        ) {
            return Ok(());
        }
        return Err(format!(
            "{row_id} profile_direct artifact {rest} does not match an authorized SK-V14 parse_only profile_direct TSV"
        ));
    }
    let Some(rest) = profile_artifact.strip_prefix("criterion-slope-profile:") else {
        return Err(format!("{row_id} missing criterion slope profile artifact"));
    };
    let expected = expected_profile_path(row_id)?;
    if rest != expected {
        return Err(format!(
            "{row_id} profile artifact {rest} does not match expected {expected}"
        ));
    }
    Ok(())
}

fn validate_w0_hot_leaf(
    row_id: &str,
    hot_leaf: &str,
    profile_artifact: &str,
) -> Result<(), String> {
    if hot_leaf.contains("unprofiled") || hot_leaf.contains("criterion-slope;") {
        return Err(format!("{row_id} still has placeholder hot leaf"));
    }
    if profile_artifact.starts_with("profile_direct-cold:") {
        let hot_leaf_marker = if profile_artifact
            .contains("skv14-W10R-parse-only-profile-direct.tsv")
        {
            "not-collected-in-W10R-row"
        } else if profile_artifact.contains("skv14-W10S-parse-only-string-end-profile-direct.tsv") {
            "not-collected-in-W10S-row"
        } else if profile_artifact.contains("skv14-W10T-parse-only-open-sweep.tsv") {
            "not-collected-in-W10T-row"
        } else if profile_artifact.contains("skv14-W10V-parse-only-current-head-resweep.tsv") {
            "not-collected-in-W10V-row"
        } else if profile_artifact.contains("skv14-W10W-parse-only-iterative-stack.tsv") {
            "not-collected-in-W10W-row"
        } else if profile_artifact.contains("skv14-W11W-parse-only-memchr.tsv") {
            "not-collected-in-W11W-row"
        } else if profile_artifact.contains("skv14-W9AA-distinct-values-typed.tsv") {
            "not-collected-in-W9AA-row"
        } else if profile_artifact.contains("skv14-W9AB-canada-typed.tsv") {
            "not-collected-in-W9AB-row"
        } else if profile_artifact.contains("skv14-W11L-y-string-token-product.tsv") {
            "not-collected-in-W11L-row"
        } else if profile_artifact.contains("skv14-W11N-unicode-mixed-decoded-token-product.tsv") {
            "not-collected-in-W11N-row"
        } else if profile_artifact.contains("skv14-W11O-gsoc-decoded-token-product.tsv") {
            "not-collected-in-W11O-row"
        } else {
            "not-collected-in-W10-row"
        };
        let expected = format!("{profile_artifact};hot-leaf={hot_leaf_marker};row={row_id}");
        if hot_leaf != expected {
            return Err(format!(
                "{row_id} hot leaf does not match SK-V14 profile_direct artifact"
            ));
        }
        return Ok(());
    }
    let expected = format!("{profile_artifact};hot-leaf=criterion-slope-profile;row={row_id}");
    if hot_leaf != expected {
        return Err(format!("{row_id} hot leaf does not match profile artifact"));
    }
    Ok(())
}

fn expected_profile_path(row_id: &str) -> Result<String, String> {
    let (corpus, workload) = parse_row_id(row_id)?;
    let bench = match workload {
        "parse_only" => "track1_generated",
        "direct_to_struct" => "track1_direct_to_struct",
        "real_typed_struct" => "track1_real_typed_struct",
        _ => return Err(format!("{row_id} has unsupported workload {workload}")),
    };
    Ok(format!("json_{corpus}/{bench}/new/estimates.json"))
}

fn validate_w0_manifest_semantics(row: &TelemetryRow) -> Result<(), String> {
    let telemetry = &row.sk_v8;
    if telemetry.costfacts_rule_id != "none:pre-W1"
        || telemetry.costfacts_chosen_shape != "none:pre-W1"
        || telemetry.costfacts_rejected_alternative_ids.as_slice() != ["none:pre-W1"]
        || telemetry.redress_entry != "none"
        || telemetry.track2_independence_status != "independent_verified"
    {
        return Err(format!(
            "{} has unsupported W0 manifest sentinel",
            telemetry.row_id
        ));
    }
    let has_exact = |value: &str, expected: &str| value.split(';').any(|part| part == expected);
    let has_nonempty = |value: &str, prefix: &str| {
        value.split(';').any(|part| {
            part.strip_prefix(prefix)
                .is_some_and(|tail| !tail.trim().is_empty())
        })
    };
    if !has_exact(&telemetry.build_flags, "profile=bench")
        && !has_exact(&telemetry.build_flags, "profile=release")
    {
        return Err(format!(
            "{} build_flags missing profile=bench or profile=release",
            telemetry.row_id
        ));
    }
    for required in ["rustflags=-C target-cpu=native", "target_cpu=native"] {
        if !has_exact(&telemetry.build_flags, required) {
            return Err(format!(
                "{} build_flags missing {required}",
                telemetry.row_id
            ));
        }
    }
    let (host, rest) = telemetry
        .host_triple
        .split_once(';')
        .ok_or_else(|| format!("{} host_triple missing arch/cpu facts", telemetry.row_id))?;
    if host.trim().is_empty()
        || !host.contains('-')
        || !has_nonempty(rest, "arch=")
        || !has_nonempty(rest, "cpu=")
    {
        return Err(format!(
            "{} host_triple is not structured W0 host metadata",
            telemetry.row_id
        ));
    }
    if ["arch=", "os=", "simd="]
        .iter()
        .any(|prefix| !has_nonempty(&telemetry.feature_mask, prefix))
        || !has_exact(&telemetry.feature_mask, "target_cpu=native")
    {
        return Err(format!(
            "{} feature_mask is not structured W0 feature metadata",
            telemetry.row_id
        ));
    }
    let expected = w0_substrate_tuple(&row.workload).ok_or_else(|| {
        format!(
            "{} has unsupported W0 workload {}",
            row.sk_v8.row_id, row.workload
        )
    })?;
    let actual = (
        row.sk_v8.substrate_surface.as_str(),
        row.sk_v8.structural_projection_status.as_str(),
        row.sk_v8.substrate_cardinality.as_str(),
    );
    if actual != expected {
        return Err(format!(
            "{} substrate tuple {:?} does not match W0 workload {:?}",
            row.sk_v8.row_id, actual, expected
        ));
    }
    Ok(())
}

fn w0_substrate_tuple(workload: &str) -> Option<(&'static str, &'static str, &'static str)> {
    Some(match workload {
        "parse_only" => ("parse_only_validator", "n/a", "zero_or_inert"),
        "direct_to_struct" => ("direct_strict_product", "n/a", "zero_or_inert"),
        "real_typed_struct" => ("typed_direct_projection", "n/a", "zero_or_inert"),
        _ => return None,
    })
}

fn validate_w0_admission_boundary(row: &TelemetryRow) -> Result<(), String> {
    if row.strictness != "deferred" {
        return Err(format!(
            "{} has unsupported W0 strictness {}",
            row.sk_v8.row_id, row.strictness
        ));
    }
    if row.sk_v8.measured_validation_path != "view-boundary" {
        return Err(format!(
            "{} has unsupported W0 validation path {}",
            row.sk_v8.row_id, row.sk_v8.measured_validation_path
        ));
    }
    if row.parse_utf8 != "view-boundary" {
        return Err(format!(
            "{} has unsupported W0 parse_utf8 {}",
            row.sk_v8.row_id, row.parse_utf8
        ));
    }
    if row.escape_complete != "yes" {
        return Err(format!(
            "{} has unsupported W0 escape_complete {}",
            row.sk_v8.row_id, row.escape_complete
        ));
    }
    Ok(())
}

fn validate_w0_row_identity(row: &TelemetryRow) -> Result<(), String> {
    let (corpus, workload) = parse_row_id(&row.sk_v8.row_id)?;
    if row.corpus != corpus || row.workload != workload {
        return Err(format!(
            "{} does not match rendered row {}/{}",
            row.sk_v8.row_id, row.corpus, row.workload
        ));
    }
    Ok(())
}

fn validate_comparator_evidence(
    row_id: &str,
    workload: &str,
    comparators: &[SkV8ComparatorEvidence],
) -> Result<(), String> {
    if comparators.is_empty() {
        return Err(format!("{row_id} missing comparator evidence"));
    }
    let mut seen = BTreeSet::new();
    for comparator in comparators {
        if !seen.insert(comparator.comparator_id.as_str()) {
            return Err(format!(
                "{row_id} duplicate comparator evidence {}",
                comparator.comparator_id
            ));
        }
        for (field, value) in [
            ("comparator_plane", comparator.comparator_plane.as_str()),
            (
                "comparator_strictness",
                comparator.comparator_strictness.as_str(),
            ),
            (
                "comparator_freshness",
                comparator.comparator_freshness.as_str(),
            ),
            ("sidecar_freshness", comparator.sidecar_freshness.as_str()),
            ("source_artifact", comparator.source_artifact.as_str()),
        ] {
            if value.trim().is_empty() {
                return Err(format!(
                    "{row_id} {} missing {field}",
                    comparator.comparator_id
                ));
            }
        }
        if let Some(value) = comparator.value_mbps {
            if !value.is_finite() || value <= 0.0 {
                return Err(format!(
                    "{row_id} {} has invalid Mbps",
                    comparator.comparator_id
                ));
            }
        }
        if SK_V8_NATIVE_STRICT_COMPARATORS.contains(&comparator.comparator_id.as_str()) {
            continue;
        } else if SK_V8_NATIVE_FLAW_PROBES.contains(&comparator.comparator_id.as_str()) {
            validate_flaw_probe_comparator(row_id, workload, comparator)?;
        } else if SK_V8_SIDECAR_COMPARATORS.contains(&comparator.comparator_id.as_str()) {
            validate_sidecar_comparator(row_id, workload, comparator)?;
            match comparator.value_mbps {
                Some(_) => {
                    if comparator.sidecar_freshness.starts_with("absent:") {
                        return Err(format!(
                            "{row_id} populated {} is marked absent",
                            comparator.comparator_id
                        ));
                    }
                    if !(comparator.sidecar_freshness.starts_with("historical:")
                        || comparator.sidecar_freshness == "sidecar-same-run")
                    {
                        return Err(format!(
                            "{row_id} populated {} lacks sidecar freshness",
                            comparator.comparator_id
                        ));
                    }
                }
                None => {
                    if !comparator.sidecar_freshness.starts_with("absent:")
                        || comparator.sidecar_freshness == "absent:"
                    {
                        return Err(format!(
                            "{row_id} absent {} lacks absent:<reason>",
                            comparator.comparator_id
                        ));
                    }
                }
            }
        } else {
            return Err(format!(
                "{row_id} has unsupported comparator id {}",
                comparator.comparator_id
            ));
        }
    }
    validate_native_comparator_source(row_id, workload, comparators, "sonic_rs_strict")?;
    validate_native_comparator_source(row_id, workload, comparators, "serde_json")?;
    for sidecar in SK_V8_SIDECAR_COMPARATORS {
        if !seen.contains(sidecar) {
            return Err(format!("{row_id} missing sidecar slot {sidecar}"));
        }
    }
    Ok(())
}

fn validate_flaw_probe_comparator(
    row_id: &str,
    workload: &str,
    comparator: &SkV8ComparatorEvidence,
) -> Result<(), String> {
    if workload != "parse_only" {
        return Err(format!(
            "{row_id} {} is not valid for {workload}",
            comparator.comparator_id
        ));
    }
    let (corpus, _) = parse_row_id(row_id)?;
    if comparator.comparator_plane != "DOM"
        || comparator.comparator_strictness != "permissive"
        || comparator.comparator_freshness != "same-run-native"
        || comparator.sidecar_freshness != "n/a"
        || comparator.value_mbps.is_none()
    {
        return Err(format!(
            "{row_id} {} has invalid flaw-probe evidence",
            comparator.comparator_id
        ));
    }
    let expected = format!("criterion:json_{corpus}/sonic_rs_lossy/new/estimates.json");
    if comparator.source_artifact != expected {
        return Err(format!(
            "{row_id} {} source {} does not match expected {}",
            comparator.comparator_id, comparator.source_artifact, expected
        ));
    }
    Ok(())
}

fn validate_sidecar_comparator(
    row_id: &str,
    workload: &str,
    comparator: &SkV8ComparatorEvidence,
) -> Result<(), String> {
    let (corpus, _) = parse_row_id(row_id)?;
    if comparator.comparator_plane != "DOM" {
        return Err(format!(
            "{row_id} {} sidecar plane {} is not DOM",
            comparator.comparator_id, comparator.comparator_plane
        ));
    }
    if comparator.comparator_strictness != "strict" {
        return Err(format!(
            "{row_id} {} sidecar strictness {} is not strict",
            comparator.comparator_id, comparator.comparator_strictness
        ));
    }
    if comparator.comparator_freshness != comparator.sidecar_freshness {
        return Err(format!(
            "{row_id} {} sidecar freshness mismatch",
            comparator.comparator_id
        ));
    }
    if comparator.sidecar_freshness == "sidecar-same-run" {
        return Err(format!(
            "{row_id} {} claims sidecar-same-run without structured manifest",
            comparator.comparator_id
        ));
    }
    let expected_source = if comparator.value_mbps.is_some() {
        format!(
            "sidecar-profile:sk-v7-cpp:{corpus}:{}",
            comparator.comparator_id
        )
    } else {
        format!(
            "absence:w1:{corpus}:{workload}:{}",
            comparator.comparator_id
        )
    };
    if comparator.source_artifact != expected_source {
        return Err(format!(
            "{row_id} {} source {} does not match expected {}",
            comparator.comparator_id, comparator.source_artifact, expected_source
        ));
    }
    Ok(())
}

fn validate_native_comparator_source(
    row_id: &str,
    workload: &str,
    comparators: &[SkV8ComparatorEvidence],
    comparator_id: &str,
) -> Result<(), String> {
    let comparator = comparators
        .iter()
        .find(|entry| entry.comparator_id == comparator_id)
        .ok_or_else(|| format!("{row_id} missing native comparator {comparator_id}"))?;
    let (corpus, _) = parse_row_id(row_id)?;
    let (expected_bench, expected_plane) = match (comparator_id, workload) {
        ("sonic_rs_strict", "parse_only") => ("sonic_rs_skipper", "parse_only/sonic_rs::Skipper"),
        ("sonic_rs_strict", "direct_to_struct") => {
            ("sonic_rs_direct_to_struct", "direct strict product")
        }
        ("sonic_rs_strict", "real_typed_struct") => ("sonic_rs_real_typed_struct", "typed direct"),
        ("serde_json", "parse_only") => ("serde_json", "DOM"),
        ("serde_json", "direct_to_struct") => {
            ("serde_json_direct_to_struct", "direct strict product")
        }
        ("serde_json", "real_typed_struct") => ("serde_json_real_typed_struct", "typed direct"),
        _ => {
            return Err(format!(
                "{row_id} has unsupported comparator/workload {comparator_id}/{workload}"
            ))
        }
    };
    if comparator.comparator_plane != expected_plane {
        return Err(format!(
            "{row_id} {} plane {} does not match expected {}",
            comparator.comparator_id, comparator.comparator_plane, expected_plane
        ));
    }
    if comparator.comparator_strictness != "strict" {
        return Err(format!(
            "{row_id} {} is not a strict native comparator",
            comparator.comparator_id
        ));
    }
    if comparator.comparator_freshness != "same-run-native" {
        return Err(format!(
            "{row_id} {} freshness {} is not same-run-native",
            comparator.comparator_id, comparator.comparator_freshness
        ));
    }
    if comparator.sidecar_freshness != "n/a" {
        return Err(format!(
            "{row_id} {} native comparator has sidecar freshness {}",
            comparator.comparator_id, comparator.sidecar_freshness
        ));
    }
    if comparator.value_mbps.is_none() {
        return Err(format!(
            "{row_id} {} missing native comparator Mbps",
            comparator.comparator_id
        ));
    }
    let mode = match (comparator_id, workload) {
        ("sonic_rs_strict", "parse_only") => "parse_only_sonic",
        ("serde_json", "parse_only") => "parse_only_serde",
        ("sonic_rs_strict", "direct_to_struct") => "direct_strict_sonic",
        ("serde_json", "direct_to_struct") => "direct_strict_serde",
        ("sonic_rs_strict", "real_typed_struct") => "real_typed_sonic",
        ("serde_json", "real_typed_struct") => "real_typed_serde",
        _ => "",
    };
    let accepted_profile = [
        "restart/skinny/tranches/sk-v14/research/skv14-W10-profile-direct.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W10R-parse-only-profile-direct.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W10S-parse-only-string-end-profile-direct.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W10T-parse-only-open-sweep.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W10V-parse-only-current-head-resweep.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W10W-parse-only-iterative-stack.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W9AA-distinct-values-typed.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W9AB-canada-typed.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W11L-y-string-token-product.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W11N-unicode-mixed-decoded-token-product.tsv",
        "restart/skinny/tranches/sk-v14/research/skv14-W11O-gsoc-decoded-token-product.tsv",
    ]
    .into_iter()
    .map(|path| format!("profile_direct:{path},mode={mode}"))
    .any(|expected| comparator.source_artifact == expected);
    if accepted_profile {
        return Ok(());
    }
    let expected = format!("criterion:json_{corpus}/{expected_bench}/new/estimates.json");
    if comparator.source_artifact != expected {
        return Err(format!(
            "{row_id} {} source {} does not match expected {}",
            comparator.comparator_id, comparator.source_artifact, expected
        ));
    }
    Ok(())
}

fn parse_row_id(row_id: &str) -> Result<(&str, &str), String> {
    let mut parts = row_id.split('/');
    let grammar = parts.next();
    let corpus = parts.next();
    let workload = parts.next();
    let suffix = parts.next();
    if grammar != Some("json") || corpus.is_none() || workload.is_none() || suffix != Some("main") {
        return Err(format!("{row_id} is not a valid SK-V9 row id"));
    }
    Ok((corpus.unwrap(), workload.unwrap()))
}

fn is_w1a_run_id(run_id: &str) -> bool {
    let Some(suffix) = run_id.strip_prefix(W1A_RUN_ID_PREFIX) else {
        return false;
    };
    suffix.len() == 16
        && suffix
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
}

fn is_skv12_run_id(run_id: &str) -> bool {
    let Some(rest) = run_id
        .strip_prefix("sk-v12-")
        .or_else(|| run_id.strip_prefix("sk-v12:"))
    else {
        return false;
    };
    !rest.is_empty() && !rest.contains("sk-v11")
}

fn is_lower_hex_64(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
}

macro_rules! require_w1a_text {
    ($id:expr; $($name:literal = $value:expr),+ $(,)?) => {
        $(
            if $value.trim().is_empty() {
                return Err(format!("{} missing {}", $id, $name));
            }
        )+
    };
}

fn validate_skv13_css_stylesheet_selectors_row(
    row: &SkV13CssStylesheetSelectorsRow,
    report: &SkV13CssStylesheetSelectorsReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W2 stylesheet/selectors report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/stylesheet_and_selectors/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:stylesheet_and_selectors"
        || row.corpus_or_workload != "stylesheet_and_selectors"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_stylesheet_selector_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W2 CSS identity", row.row_id));
    }
    if row.input_checksum != "7fc890301ed7cdd79224fdca8d174bac80069b518c100156ed5b6e1f96cb9530"
        || !row
            .generated_input_provenance
            .contains("sha256=7fc890301ed7cdd79224fdca8d174bac80069b518c100156ed5b6e1f96cb9530")
        || row.input_bytes != 117
        || row.generated_loc == 0
        || row.generated_loc > 720
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=720"
    {
        return Err(format!(
            "{} has invalid W2 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_stylesheet_selectors_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_stylesheet_selectors::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W2 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=golden=lightningcss"
        || row.lightningcss_sequence_status != "pass:strict-parse-source-sidecar"
        || !row.track2_independence_status.contains("golden-fixture")
    {
        return Err(format!("{} has invalid W2 measurement proof", row.row_id));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W2 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("golden-fixture")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_stylesheet_selectors")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W2 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w2")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w2")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=117")
        || !row.lock14_status.contains("sk-v13-waveW2")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:golden_fixture_oracle"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_stylesheet_selectors_sota"
        || row.redress_entry != "REDRESS-130"
    {
        return Err(format!("{} has incomplete W2 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv13_css_declaration_values_extended_row(
    row: &SkV13CssDeclarationValuesExtendedRow,
    report: &SkV13CssDeclarationValuesExtendedReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W3 declaration-values-extended report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/declaration_values_extended/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:declaration_values_extended"
        || row.corpus_or_workload != "declaration_values_extended"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_declaration_value_extended_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W3 CSS identity", row.row_id));
    }
    if row.input_checksum != "399593fe9848954d3570c67a588a7c352e252327f60445f3bc0670c11df88d64"
        || !row
            .generated_input_provenance
            .contains("sha256=399593fe9848954d3570c67a588a7c352e252327f60445f3bc0670c11df88d64")
        || row.input_bytes != 305
        || row.generated_loc == 0
        || row.generated_loc > 820
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=820"
    {
        return Err(format!(
            "{} has invalid W3 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_declaration_values_extended_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_declaration_values_extended::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W3 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=cssparser=lightningcss"
        || row.lightningcss_sequence_status != "pass:strict-parse-source-sidecar"
        || !row.track2_independence_status.contains("cssparser")
    {
        return Err(format!("{} has invalid W3 measurement proof", row.row_id));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W3 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("cssparser-0.34")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_declaration_values_extended")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W3 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w3")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w3")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=305")
        || !row.lock14_status.contains("sk-v13-waveW3")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:cssparser_oracle"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_declaration_values_extended_sota"
        || row.redress_entry != "REDRESS-131"
    {
        return Err(format!("{} has incomplete W3 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv13_css_visual_functions_row(
    row: &SkV13CssVisualFunctionsRow,
    report: &SkV13CssVisualFunctionsReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W4 visual-functions report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/visual_functions/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:visual_functions"
        || row.corpus_or_workload != "visual_functions"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_visual_function_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W4 CSS identity", row.row_id));
    }
    if row.input_checksum != "5dc7cc1098401900af32b534893c9bd007245f88af3cc683926a4abaf5f531c0"
        || !row
            .generated_input_provenance
            .contains("sha256=5dc7cc1098401900af32b534893c9bd007245f88af3cc683926a4abaf5f531c0")
        || row.input_bytes != 357
        || row.generated_loc == 0
        || row.generated_loc > 950
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=950"
    {
        return Err(format!(
            "{} has invalid W4 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_visual_functions_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_visual_functions::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W4 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=golden=lightningcss"
        || row.lightningcss_sequence_status != "pass:strict-parse-source-sidecar"
        || !row.track2_independence_status.contains("golden")
    {
        return Err(format!("{} has invalid W4 measurement proof", row.row_id));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W4 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("cssparser-0.34")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_visual_functions")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W4 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w4")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w4")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=357")
        || !row.lock14_status.contains("sk-v13-waveW4")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:golden_oracle"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_visual_functions_sota"
        || row.redress_entry != "REDRESS-132"
    {
        return Err(format!("{} has incomplete W4 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv13_css_at_rules_and_media_row(
    row: &SkV13CssAtRulesAndMediaRow,
    report: &SkV13CssAtRulesAndMediaReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W10.1 at-rules/media report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/at_rules_and_media/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:at_rules_and_media"
        || row.corpus_or_workload != "at_rules_and_media"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_at_rules_media_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W10.1 CSS identity", row.row_id));
    }
    if row.input_checksum != "234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958"
        || !row
            .generated_input_provenance
            .contains("sha256=234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958")
        || row.input_bytes != 85
        || row.generated_loc == 0
        || row.generated_loc > 950
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=950"
    {
        return Err(format!(
            "{} has invalid W10.1 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_at_rules_and_media_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_at_rules_and_media::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W10.1 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=golden=lightningcss"
        || row.lightningcss_sequence_status != "pass:typed-ast-media-keyframes-source-sidecar"
        || !row.track2_independence_status.contains("golden")
    {
        return Err(format!(
            "{} has invalid W10.1 measurement proof",
            row.row_id
        ));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W10.1 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("golden-fixture")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_at_rules_and_media")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W10.1 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w10_1")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w10_1")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=85")
        || !row.lock14_status.contains("sk-v13-waveW10.1")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:golden_oracle_plus_lightningcss_ast"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_at_rules_media_sota"
        || row.redress_entry != "REDRESS-133"
    {
        return Err(format!("{} has incomplete W10.1 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv13_css_vendor_custom_row(
    row: &SkV13CssVendorCustomRow,
    report: &SkV13CssVendorCustomReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W10.2 vendor/custom report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/vendor_and_custom_atrules/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:vendor_and_custom_atrules"
        || row.corpus_or_workload != "vendor_and_custom_atrules"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_vendor_custom_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W10.2 CSS identity", row.row_id));
    }
    if row.input_checksum != "367122942a2c937654b35a1065edc33ae85694a4bcd02b50d6ed50ea1631995f"
        || !row
            .generated_input_provenance
            .contains("sha256=367122942a2c937654b35a1065edc33ae85694a4bcd02b50d6ed50ea1631995f")
        || row.input_bytes != 162
        || row.generated_loc == 0
        || row.generated_loc > 1050
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=1050"
    {
        return Err(format!(
            "{} has invalid W10.2 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_vendor_and_custom_atrules_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_vendor_and_custom_atrules::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W10.2 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=golden=lightningcss"
        || row.lightningcss_sequence_status
            != "pass:typed-ast-custom-media-vendor-keyframes-source-sidecar"
        || !row.track2_independence_status.contains("golden")
    {
        return Err(format!(
            "{} has invalid W10.2 measurement proof",
            row.row_id
        ));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W10.2 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("golden-fixture")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_vendor_and_custom_atrules")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W10.2 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w10_2")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w10_2")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=162")
        || !row.lock14_status.contains("sk-v13-waveW10.2")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:golden_oracle_plus_lightningcss_ast"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_vendor_custom_sota"
        || row.redress_entry != "REDRESS-134"
    {
        return Err(format!("{} has incomplete W10.2 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv13_css_nested_layout_row(
    row: &SkV13CssNestedLayoutRow,
    report: &SkV13CssNestedLayoutReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "oracle_artifact_path" = row.oracle_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match W10.3 nested/layout report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/nested_layout/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:nested_layout"
        || row.corpus_or_workload != "nested_layout"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_nested_layout_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid W10.3 CSS identity", row.row_id));
    }
    if row.input_checksum != "5edcbfba1ba52af4dff689257aed8678a82f7d1cbbac36f5d0ae974384bddd2d"
        || !row
            .generated_input_provenance
            .contains("sha256=5edcbfba1ba52af4dff689257aed8678a82f7d1cbbac36f5d0ae974384bddd2d")
        || row.input_bytes != 351
        || row.generated_loc == 0
        || row.generated_loc > 1050
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=1050"
    {
        return Err(format!(
            "{} has invalid W10.3 generated-source proof",
            row.row_id
        ));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_nested_layout_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_nested_layout::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!("{} has invalid W10.3 runtime proof", row.row_id));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=golden=lightningcss"
        || row.lightningcss_sequence_status != "pass:typed-ast-nesting-layout-source-sidecar"
        || !row.track2_independence_status.contains("golden")
    {
        return Err(format!(
            "{} has invalid W10.3 measurement proof",
            row.row_id
        ));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
        || row.track1_mbps <= threshold
        || row.admission_status != "PASS-ADMIT-CANDIDATE"
        || row.outcome_id != "A"
        || row.verdict != "GO"
        || row.gate_status != "pass"
    {
        return Err(format!("{} has stale W10.3 threshold math", row.row_id));
    }
    if !row.track1_artifact.contains("track1-facts.txt")
        || !row.oracle_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row.track2_or_oracle_source_path.contains("golden-fixture")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_nested_layout")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!("{} has invalid W10.3 comparator proof", row.row_id));
    }
    if !row
        .measured_validation_path
        .contains("criterion:nonjson_css_l4_w10_3")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4_w10_3")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("bytes=351")
        || !row.lock14_status.contains("sk-v13-waveW10.3")
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:golden_oracle_plus_lightningcss_ast"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !row.json_guard_state.contains("guards-pass")
        || row.same_wave_consumer_class != "companion_gate_css_l4_nested_layout_sota"
        || row.redress_entry != "REDRESS-135"
    {
        return Err(format!("{} has incomplete W10.3 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv12_css_l4_sota_row(
    row: &SkV12CssL4SotaRow,
    report: &SkV12CssL4SotaReport,
) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "schema_id" = row.schema_id,
        "wave_id" = row.wave_id,
        "run_id" = row.run_id,
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "output_plane" = row.output_plane,
        "strictness" = row.strictness,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "gate_status" = row.gate_status,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "grammar_size_guard" = row.grammar_size_guard,
        "admission_status" = row.admission_status,
        "track1_artifact" = row.track1_artifact,
        "cssparser_artifact_path" = row.cssparser_artifact_path,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "lightningcss_command" = row.lightningcss_command,
        "lightningcss_artifact" = row.lightningcss_artifact,
        "lightningcss_fact_artifact_path" = row.lightningcss_fact_artifact_path,
        "fact_stream_sha256" = row.fact_stream_sha256,
        "strict_output_equality" = row.strict_output_equality,
        "three_way_equality" = row.three_way_equality,
        "lightningcss_sequence_status" = row.lightningcss_sequence_status,
        "track2_independence_status" = row.track2_independence_status,
        "measured_validation_path" = row.measured_validation_path,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "profile_artifact" = row.profile_artifact,
        "sample_cost" = row.sample_cost,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "redress_entry" = row.redress_entry,
    );
    if row.schema_id != report.schema_id
        || row.wave_id != report.wave_id
        || row.run_id != report.run_id
    {
        return Err(format!(
            "{} does not match CSS L4 report identity",
            row.row_id
        ));
    }
    if row.row_id != "css_l4/declaration_values/direct_to_struct/main"
        || row.grammar_id != "css_l4"
        || row.domain != "non_json_generated:css_l4:declaration_values"
        || row.corpus_or_workload != "declaration_values"
        || row.workload != "direct_to_struct"
        || row.output_plane != "css_l4_declaration_value_fact_stream"
        || row.strictness != "strict"
    {
        return Err(format!("{} has invalid CSS L4 SOTA identity", row.row_id));
    }
    if row.redress_entry != "REDRESS-125"
        || row.same_wave_consumer_class != "companion_gate_css_l4_lightningcss_sota"
        || row.gate_status != "pass"
        || row.verdict != "GO"
    {
        return Err(format!("{} has invalid CSS L4 gate context", row.row_id));
    }
    if row.input_checksum != "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374"
        || !row
            .generated_input_provenance
            .contains("sha256=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374")
        || row.input_bytes != 187
        || row.generated_loc == 0
        || row.generated_loc > 360
        || row.generated_module_bytes == 0
        || !is_lower_hex_64(&row.grammar_checksum)
        || row.grammar_size_guard != "pass:generated_loc<=360"
    {
        return Err(format!("{} has invalid generated-source proof", row.row_id));
    }
    if !row
        .generated_track1_source_path
        .contains("css_l4_declaration_values_templates/generated.rs")
        || !row
            .generated_runtime_path
            .contains("generated_css_l4_declaration_values::parser::parse")
        || row.generated_runtime_path.contains("generated_json")
    {
        return Err(format!(
            "{} has invalid generated runtime proof",
            row.row_id
        ));
    }
    if !positive_finite(row.track1_mbps)
        || !positive_finite(row.track2_or_oracle_mbps)
        || !positive_finite(row.lightningcss_mbps)
        || !positive_finite(row.threshold_mbps)
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.three_way_equality != "pass:track1=cssparser=lightningcss"
        || row.lightningcss_sequence_status != "pass:ast_projection_matches_source_sidecar"
        || row.track2_independence_status != "independent_verified"
    {
        return Err(format!(
            "{} has invalid CSS L4 measurement proof",
            row.row_id
        ));
    }
    let threshold = row.lightningcss_mbps + 1.0;
    let margin = row.track1_mbps - threshold;
    if (row.threshold_mbps - threshold).abs() > 0.01
        || (row.admission_margin_mbps - margin).abs() > 0.01
    {
        return Err(format!("{} has stale CSS L4 threshold math", row.row_id));
    }
    match row.admission_status.as_str() {
        "PASS-ADMIT-CANDIDATE" if row.track1_mbps > threshold && row.outcome_id == "A" => {}
        "PASS-MEASURED-BASELINE" if row.track1_mbps <= threshold && row.outcome_id == "C" => {}
        _ => {
            return Err(format!(
                "{} has invalid CSS L4 admission status",
                row.row_id
            ))
        }
    }
    if !row.track2_or_oracle_source_path.contains("cssparser-0.34")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_declaration_values")
        || !row
            .lightningcss_command
            .contains("lightningcss-1.0.0-alpha.71")
        || !row
            .lightningcss_command
            .contains("same-plane-source-sidecar")
        || !row.track1_artifact.contains("track1-facts.txt")
        || !row.cssparser_artifact_path.contains("oracle-facts.txt")
        || !row
            .lightningcss_fact_artifact_path
            .contains("lightningcss-facts.txt")
        || !row
            .lightningcss_artifact
            .contains("lightningcss-strict-equality.txt")
        || !is_lower_hex_64(&row.fact_stream_sha256)
    {
        return Err(format!(
            "{} has stale or coupled comparator proof",
            row.row_id
        ));
    }
    if !row.measured_validation_path.contains("strict-equality")
        || !row.benchmark_artifact_path.contains("nonjson_css_l4")
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("mean_ns=")
        || row.lock14_status != "pass:lock14_baseline::validate"
        || row.lock16_status != "n/a:no_simd_or_asm_claim"
        || row.scalar_reference_status != "pass:cssparser_oracle"
        || row.checkasm_or_parity_status != "pass:three_way_fact_stream"
        || !(row.json_guard_state == "not_refreshed:no_behavior_drift"
            || (row.json_guard_state.starts_with("refreshed:")
                && row.json_guard_state.contains("guards-pass")))
    {
        return Err(format!("{} has incomplete CSS L4 gate context", row.row_id));
    }
    Ok(())
}

fn validate_skv12_non_json_row(row: &SkV12NonJsonRow, run_id: &str) -> Result<(), String> {
    require_w1a_text!(
        row.row_id;
        "grammar_id" = row.grammar_id,
        "domain" = row.domain,
        "corpus_or_workload" = row.corpus_or_workload,
        "workload" = row.workload,
        "workload_class" = row.workload_class,
        "output_plane" = row.output_plane,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "strictness" = row.strictness,
        "generated_track1_source_path" = row.generated_track1_source_path,
        "generated_runtime_path" = row.generated_runtime_path,
        "generated_input_provenance" = row.generated_input_provenance,
        "grammar_checksum" = row.grammar_checksum,
        "input_checksum" = row.input_checksum,
        "track1_artifact" = row.track1_artifact,
        "track2_or_oracle_source_path" = row.track2_or_oracle_source_path,
        "track2_independence_status" = row.track2_independence_status,
        "strict_output_equality" = row.strict_output_equality,
        "oracle_status" = row.oracle_status,
        "baseline_row_id" = row.baseline_row_id,
        "host_triple" = row.host_triple,
        "feature_mask" = row.feature_mask,
        "build_flags" = row.build_flags,
        "sample_cost" = row.sample_cost,
        "benchmark_artifact_path" = row.benchmark_artifact_path,
        "measured_validation_path" = row.measured_validation_path,
        "profile_artifact" = row.profile_artifact,
        "grammar_size_guard" = row.grammar_size_guard,
        "lock14_status" = row.lock14_status,
        "lock16_status" = row.lock16_status,
        "scalar_reference_status" = row.scalar_reference_status,
        "checkasm_or_parity_status" = row.checkasm_or_parity_status,
        "json_guard_state" = row.json_guard_state,
        "redress_entry" = row.redress_entry,
        "same_wave_consumer_class" = row.same_wave_consumer_class,
        "gate_status" = row.gate_status,
    );
    let (grammar, corpus, workload) = parse_skv12_non_json_row_id(&row.row_id)?;
    if grammar != row.grammar_id || corpus != row.corpus_or_workload || workload != row.workload {
        return Err(format!("{} does not match row identity fields", row.row_id));
    }
    if row.grammar_id == "json"
        || !matches!(row.grammar_id.as_str(), "css_l4" | "sheets" | "bbnf_self")
    {
        return Err(format!(
            "{} has unsupported grammar {}",
            row.row_id, row.grammar_id
        ));
    }
    if !row.domain.starts_with("non_json_generated:") || !row.domain.contains(&row.grammar_id) {
        return Err(format!(
            "{} has unsupported domain {}",
            row.row_id, row.domain
        ));
    }
    let expected_plane = match row.workload.as_str() {
        "direct_to_struct" if row.row_id == "css_l4/declaration_values/direct_to_struct/main" => {
            "css_l4_declaration_value_fact_stream"
        }
        "direct_to_struct" => "direct_sink",
        "real_typed_struct" => "typed_direct",
        "parse_only" => return Err(format!("{} attempts parse_only admission", row.row_id)),
        _ => return Err(format!("{} has unsupported workload", row.row_id)),
    };
    if row.output_plane != expected_plane {
        return Err(format!("{} has output-plane mismatch", row.row_id));
    }
    gate::parse_outcome_id(&row.outcome_id)
        .ok_or_else(|| format!("{} has unsupported outcome {}", row.row_id, row.outcome_id))?;
    validate_skv12_generated_source(row)?;
    validate_skv12_oracle(row, run_id)?;
    validate_skv12_measurement(row)?;
    validate_skv12_gate_context(row)
}

fn parse_skv12_non_json_row_id(row_id: &str) -> Result<(&str, &str, &str), String> {
    let mut parts = row_id.split('/');
    let grammar = parts.next();
    let corpus = parts.next();
    let workload = parts.next();
    let suffix = parts.next();
    if parts.next().is_some() || corpus.is_none() || workload.is_none() || suffix != Some("main") {
        return Err(format!("{row_id} is not a valid SK-V12 non-JSON row id"));
    }
    Ok((
        grammar.unwrap_or_default(),
        corpus.unwrap(),
        workload.unwrap(),
    ))
}

fn validate_skv12_generated_source(row: &SkV12NonJsonRow) -> Result<(), String> {
    let forbidden = [
        "generated_json",
        "grammars/json",
        "sheets_witness",
        "w1a",
        "hand_only",
    ];
    for value in [
        &row.generated_track1_source_path,
        &row.generated_runtime_path,
        &row.generated_input_provenance,
    ] {
        if forbidden.iter().any(|needle| value.contains(needle)) || !value.contains(&row.grammar_id)
        {
            return Err(format!(
                "{} has stale or mismatched generated Track 1 evidence",
                row.row_id
            ));
        }
    }
    if row.row_id == "css_l4/declaration_values/direct_to_struct/main" {
        if !row
            .generated_track1_source_path
            .contains("css_l4_declaration_values_templates/generated.rs")
            || row
                .generated_runtime_path
                .contains("runtime::generated_json")
            || !row
                .generated_runtime_path
                .contains("generated_css_l4_declaration_values::parser::parse")
            || !row
                .generated_input_provenance
                .contains("sha256=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374")
            || row.input_checksum
                != "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374"
            || row.input_bytes != 187
            || row.generated_loc == 0
            || row.generated_loc > 360
            || row.generated_module_bytes == 0
            || row.grammar_checksum.len() != 64
            || row.grammar_size_guard != "pass:generated_loc<=360"
        {
            return Err(format!(
                "{} has incomplete CSS L4 generated-source evidence",
                row.row_id
            ));
        }
    }
    Ok(())
}

fn validate_skv12_oracle(row: &SkV12NonJsonRow, run_id: &str) -> Result<(), String> {
    if row.track2_independence_status != "independent_verified"
        || row.track2_or_oracle_source_path == row.generated_track1_source_path
        || row.track2_or_oracle_source_path == row.generated_runtime_path
        || row
            .track2_or_oracle_source_path
            .contains("runtime::generated_json::parse")
        || row
            .track2_or_oracle_source_path
            .contains("generated_css_l4_declaration_values")
        || row.track2_or_oracle_source_path.contains("track1")
        || !row.oracle_status.contains("same-plane")
        || !row.oracle_status.contains("strict")
        || !row.oracle_status.contains("independent")
        || !row.oracle_status.contains("fresh")
        || !row.track1_artifact.contains(run_id)
        || !row.benchmark_artifact_path.contains(run_id)
    {
        return Err(format!(
            "{} has coupled or stale oracle evidence",
            row.row_id
        ));
    }
    if row.row_id == "css_l4/declaration_values/direct_to_struct/main"
        && (!row.track2_or_oracle_source_path.contains("cssparser-0.34")
            || !row.oracle_status.contains("cssparser")
            || row.strict_output_equality != "pass"
            || row.scalar_reference_status != "pass:cssparser_oracle"
            || row.checkasm_or_parity_status != "pass:track1_equals_cssparser")
    {
        return Err(format!("{} is missing CSS parser oracle proof", row.row_id));
    }
    Ok(())
}

fn validate_skv12_measurement(row: &SkV12NonJsonRow) -> Result<(), String> {
    if row.track1_mbps < 1.0
        || !positive_finite(row.track1_mbps)
        || !row
            .track2_or_oracle_mbps
            .is_some_and(|value| value >= 1.0 && positive_finite(value))
        || row.sample_count < 30
        || row.strict_output_equality != "pass"
        || row.gate_status != "pass"
        || row.strictness != "strict"
        || row.verdict != "GO"
    {
        return Err(format!(
            "{} missing admissible SK-V12 measurement",
            row.row_id
        ));
    }
    if row.workload_class == "baseline" {
        if row.baseline_row_id != "none"
            || row.baseline_mbps.is_some()
            || row.threshold_mbps.is_some()
        {
            return Err(format!("{} has invalid baseline linkage", row.row_id));
        }
    } else if row.workload_class == "intervention" {
        let (Some(baseline), Some(threshold)) = (row.baseline_mbps, row.threshold_mbps) else {
            return Err(format!("{} missing intervention threshold", row.row_id));
        };
        if row.baseline_row_id == "none" || threshold < (baseline * 1.01).ceil() {
            return Err(format!("{} has invalid intervention threshold", row.row_id));
        }
    } else {
        return Err(format!("{} has unsupported workload class", row.row_id));
    }
    Ok(())
}

fn validate_skv12_gate_context(row: &SkV12NonJsonRow) -> Result<(), String> {
    let expected_consumer = match row.workload_class.as_str() {
        "baseline" if row.row_id == "css_l4/declaration_values/direct_to_struct/main" => {
            "companion_gate_generated_css_l4_baseline"
        }
        "baseline" => "companion_gate_generated_baseline",
        "intervention" => "companion_gate_generated_intervention",
        _ => unreachable!("workload class already validated"),
    };
    if row.same_wave_consumer_class != expected_consumer
        || !(row.json_guard_state == "not_refreshed:no_behavior_drift"
            || (row.json_guard_state.starts_with("refreshed:")
                && row.json_guard_state.contains("guards-pass")))
        || !row.host_triple.contains("arch=")
        || !row.feature_mask.contains("target_cpu=native")
        || !row.build_flags.contains("target-cpu=native")
        || !row.sample_cost.contains("ns_per_byte=")
        || row.measured_validation_path.trim().is_empty()
        || row.profile_artifact.trim().is_empty()
        || row.lock14_status != "pass:lock14_baseline::validate"
        || row.lock16_status.trim().is_empty()
    {
        return Err(format!(
            "{} has producer-only or incomplete gate context",
            row.row_id
        ));
    }
    Ok(())
}

fn validate_w1a_non_json_row(row: &NonJsonEvidenceRow) -> Result<(), String> {
    let t = &row.sk_v8;
    require_w1a_text!(
        t.row_id;
        "corpus" = row.corpus,
        "workload" = row.workload,
        "outcome_id" = row.outcome_id,
        "verdict" = row.verdict,
        "strictness" = row.strictness,
        "parse_utf8" = row.parse_utf8,
        "escape_complete" = row.escape_complete,
        "flaw_probe" = row.flaw_probe,
        "output_plane" = row.output_plane,
        "signal" = row.signal,
        "row_id" = t.row_id,
        "grammar_id" = t.grammar_id,
        "domain" = t.domain,
        "measured_validation_path" = t.measured_validation_path,
        "profile_artifact" = t.profile_artifact,
        "sample_cost" = t.sample_cost,
        "build_flags" = t.build_flags,
        "host_triple" = t.host_triple,
        "feature_mask" = t.feature_mask,
        "costfacts_rule_id" = t.costfacts_rule_id,
        "costfacts_chosen_shape" = t.costfacts_chosen_shape,
        "redress_entry" = t.redress_entry,
        "sk_v9_open_delta" = t.sk_v9_open_delta,
        "substrate_surface" = t.substrate_surface,
        "structural_projection_status" = t.structural_projection_status,
        "substrate_cardinality" = t.substrate_cardinality,
        "same_wave_consumer_class" = t.same_wave_consumer_class,
        "track2_independence_status" = t.track2_independence_status,
        "diagnostic_nonproducer_status" = t.diagnostic_nonproducer_status,
    );
    let (grammar, corpus, workload) = parse_w1a_non_json_row_id(&t.row_id)?;
    if grammar != t.grammar_id || corpus != row.corpus || workload != row.workload {
        return Err(format!("{} does not match row identity fields", t.row_id));
    }
    if t.domain != w1a_domain_for_grammar(&t.grammar_id)? {
        return Err(format!("{} has unsupported domain {}", t.row_id, t.domain));
    }
    if !matches!(
        (t.grammar_id.as_str(), row.corpus.as_str()),
        ("css_l4", "declaration_values") | ("sheets", "formula") | ("bbnf_self", "grammar")
    ) {
        return Err(format!("{} is not a W1a non-JSON row", t.row_id));
    }
    let expected_plane = match row.workload.as_str() {
        "direct" => "digest",
        "typed" => "typed direct",
        _ => return Err(format!("{} has unsupported workload", t.row_id)),
    };
    if row.outcome_id != "S" || row.verdict != "NO-GO" {
        return Err(format!("{} attempts W1a row admission", t.row_id));
    }
    if row.strictness != "strict"
        || row.parse_utf8 != "measured-row"
        || row.escape_complete != "yes"
        || row.output_plane != expected_plane
        || t.measured_validation_path != "schema-only"
        || t.same_wave_consumer_class != "non_json_gate_schema_only"
        || t.track2_independence_status != "independent_verified"
        || t.diagnostic_nonproducer_status != "pmu+cycles+profiles:nonproducer"
    {
        return Err(format!("{} has unsupported W1a semantics", t.row_id));
    }
    if !positive_optional(row.track1_mbps)
        || !positive_optional(row.track2_mbps)
        || t.sample_count == 0
        || !t.sample_cost.contains("ns_per_byte=")
    {
        return Err(format!("{} missing W1a measurement context", t.row_id));
    }
    validate_w1a_structured_context(row)?;
    validate_w1a_oracle(row)?;
    Ok(())
}

fn parse_w1a_non_json_row_id(row_id: &str) -> Result<(&str, &str, &str), String> {
    let mut parts = row_id.split('/');
    let grammar = parts.next();
    let corpus = parts.next();
    let workload = parts.next();
    let suffix = parts.next();
    if parts.next().is_some() || corpus.is_none() || workload.is_none() || suffix != Some("main") {
        return Err(format!("{row_id} is not a valid W1a row id"));
    }
    let grammar = grammar.unwrap_or_default();
    if !matches!(grammar, "css_l4" | "sheets" | "bbnf_self") {
        return Err(format!("{row_id} has unsupported grammar"));
    }
    Ok((grammar, corpus.unwrap(), workload.unwrap()))
}

fn w1a_domain_for_grammar(grammar: &str) -> Result<&'static str, String> {
    match grammar {
        "css_l4" => Ok("css_l4_bench"),
        "sheets" => Ok("sheets_bench"),
        "bbnf_self" => Ok("bbnf_self_bench"),
        _ => Err(format!("{grammar} is not a W1a grammar")),
    }
}

fn validate_w1a_oracle(row: &NonJsonEvidenceRow) -> Result<(), String> {
    let t = &row.sk_v8;
    let [oracle] = t.comparators.as_slice() else {
        return Err(format!("{} must carry one W1a oracle", t.row_id));
    };
    if oracle.comparator_id != "internal_oracle"
        || oracle.comparator_plane != row.output_plane
        || oracle.comparator_strictness != "strict"
        || oracle.comparator_freshness != "same-run-oracle"
        || oracle.sidecar_freshness != "n/a"
        || !oracle.value_mbps.is_some_and(positive_finite)
    {
        return Err(format!("{} has unsupported oracle evidence", t.row_id));
    }
    validate_w1a_oracle_source(row)
}

fn validate_w1a_oracle_source(row: &NonJsonEvidenceRow) -> Result<(), String> {
    let plane = row.output_plane.replace(' ', "_");
    let expected = format!(
        "oracle:w1a:{}:{}:{}:{}",
        row.sk_v8.grammar_id, row.corpus, row.workload, plane
    );
    let oracle = &row.sk_v8.comparators[0];
    if oracle.source_artifact == expected {
        Ok(())
    } else {
        Err(format!(
            "{} has non-independent oracle source",
            row.sk_v8.row_id
        ))
    }
}

fn validate_w1a_structured_context(row: &NonJsonEvidenceRow) -> Result<(), String> {
    let t = &row.sk_v8;
    if t.costfacts_rule_id != "none:w1a-schema"
        || t.costfacts_chosen_shape != "none:w1a-schema"
        || t.costfacts_rejected_alternative_ids != ["none:w1a-schema"]
        || t.redress_entry != "none:w1a-schema-only"
        || t.sk_v9_open_delta != "nonjson-schema-only"
        || t.substrate_surface != row.output_plane
        || t.structural_projection_status != "n/a"
        || t.substrate_cardinality != "zero_or_inert"
        || !t.profile_artifact.starts_with("fixture:w1a:")
        || !t.build_flags.contains("profile=bench")
        || !t.build_flags.contains("rustflags=-C target-cpu=native")
        || !t.build_flags.contains("target_cpu=native")
        || !t.host_triple.contains("arch=")
        || !t.host_triple.contains("cpu=")
        || !t.feature_mask.contains("arch=")
        || !t.feature_mask.contains("os=")
        || !t.feature_mask.contains("simd=")
        || !t.feature_mask.contains("target_cpu=native")
    {
        return Err(format!("{} has incomplete W1a context", t.row_id));
    }
    Ok(())
}

fn positive_optional(value: Option<f64>) -> bool {
    value.is_some_and(positive_finite)
}

fn positive_finite(value: f64) -> bool {
    value.is_finite() && value > 0.0
}

fn format_comparator_evidence(comparators: &[SkV8ComparatorEvidence]) -> String {
    comparators
        .iter()
        .map(|comparator| {
            format!(
                "{}[plane={},strictness={},freshness={},sidecar={},mbps={},source={}]",
                comparator.comparator_id,
                comparator.comparator_plane,
                comparator.comparator_strictness,
                comparator.comparator_freshness,
                comparator.sidecar_freshness,
                format_optional(comparator.value_mbps),
                comparator.source_artifact
            )
        })
        .collect::<Vec<_>>()
        .join("; ")
}

fn cell(value: &str) -> String {
    value.replace('|', "\\|").replace('\n', " ")
}

fn parse_signal(outcome: Outcome) -> String {
    match outcome.verdict() {
        Verdict::Go | Verdict::GoWithFocus => {
            format!("PASS parse gate classified {}", outcome.id())
        }
        Verdict::Conditional => {
            format!("NO-GO conditional parse gate classified {}", outcome.id())
        }
        Verdict::Invalid | Verdict::NoGo => {
            format!("NO-GO parse gate classified {}", outcome.id())
        }
    }
}

fn verdict_label(verdict: Verdict) -> &'static str {
    match verdict {
        Verdict::Go => "GO",
        Verdict::GoWithFocus => "GO with focus",
        Verdict::Conditional => "CONDITIONAL",
        Verdict::Invalid => "INVALID",
        Verdict::NoGo => "NO-GO",
    }
}

fn format_optional(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:.0}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn format_optional_precise(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:.2}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn format_ratio(numerator: Option<f64>, denominator: Option<f64>) -> String {
    match (numerator, denominator) {
        (Some(numerator), Some(denominator)) if denominator > 0.0 => {
            format!("{:.1}%", numerator / denominator * 100.0)
        }
        _ => "n/a".to_string(),
    }
}

fn format_delta(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:+.1}%"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn delta_ratio(candidate: Option<f64>, anchor: Option<f64>) -> Option<f64> {
    match (candidate, anchor) {
        (Some(candidate), Some(anchor)) if candidate > 0.0 && anchor > 0.0 => {
            Some((candidate / anchor - 1.0) * 100.0)
        }
        _ => None,
    }
}

fn ratio_to_track1(probe_ns: Option<f64>, track1_ns: Option<f64>) -> Option<f64> {
    match (probe_ns, track1_ns) {
        (Some(probe_ns), Some(track1_ns)) if probe_ns > 0.0 && track1_ns > 0.0 => {
            Some(track1_ns / probe_ns)
        }
        _ => None,
    }
}

fn track1_mbps_from_ratio(probe_mbps: Option<f64>, ratio: Option<f64>) -> Option<f64> {
    match (probe_mbps, ratio) {
        (Some(mbps), Some(ratio)) if ratio > 0.0 => Some(mbps / ratio),
        _ => None,
    }
}

fn throughput_mbps(bytes: u64, ns: Option<f64>) -> Option<f64> {
    if bytes == 0 {
        return None;
    }
    ns.filter(|ns| *ns > 0.0 && ns.is_finite())
        .map(|ns| bytes as f64 * 8_000.0 / ns)
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_SK_V9_OPEN_RUN_ID: &str = "SK-V14-open:criterion-fnv64-0123456789abcdef";

    fn comparators() -> ComparatorSet {
        ComparatorSet {
            sonic_strict_mbps: Some(11_915.0),
            sonic_lossy_mbps: Some(12_000.0),
            simdjson_dom_mbps: None,
            simdjson_ondemand_mbps: None,
            yyjson_default_mbps: None,
            asmjson_swar_mbps: None,
            asmjson_avx512_mbps: None,
            rapidjson_default_mbps: None,
            serde_json_mbps: Some(10_000.0),
        }
    }

    fn ns_for_mbps(bytes: u64, mbps: f64) -> f64 {
        bytes as f64 * 8_000.0 / mbps
    }

    fn w0_evidence(row_id: &str) -> Vec<SkV8ComparatorEvidence> {
        let (corpus, workload) = parse_row_id(row_id).unwrap();
        let sonic_bench = match workload {
            "parse_only" => "sonic_rs_skipper",
            "direct_to_struct" => "sonic_rs_direct_to_struct",
            "real_typed_struct" => "sonic_rs_real_typed_struct",
            _ => unreachable!(),
        };
        let serde_bench = match workload {
            "parse_only" => "serde_json",
            "direct_to_struct" => "serde_json_direct_to_struct",
            "real_typed_struct" => "serde_json_real_typed_struct",
            _ => unreachable!(),
        };
        let native_plane = match workload {
            "parse_only" => "parse_only/sonic_rs::Skipper",
            "direct_to_struct" => "direct strict product",
            "real_typed_struct" => "typed direct",
            _ => unreachable!(),
        };
        let serde_plane = match workload {
            "parse_only" => "DOM",
            _ => native_plane,
        };
        let mut evidence = vec![
            SkV8ComparatorEvidence {
                comparator_id: "sonic_rs_strict".into(),
                comparator_plane: native_plane.into(),
                comparator_strictness: "strict".into(),
                comparator_freshness: "same-run-native".into(),
                sidecar_freshness: "n/a".into(),
                value_mbps: Some(11_915.0),
                source_artifact: format!(
                    "criterion:json_{corpus}/{sonic_bench}/new/estimates.json"
                ),
            },
            SkV8ComparatorEvidence {
                comparator_id: "serde_json".into(),
                comparator_plane: serde_plane.into(),
                comparator_strictness: "strict".into(),
                comparator_freshness: "same-run-native".into(),
                sidecar_freshness: "n/a".into(),
                value_mbps: Some(10_000.0),
                source_artifact: format!(
                    "criterion:json_{corpus}/{serde_bench}/new/estimates.json"
                ),
            },
        ];
        for id in SK_V8_SIDECAR_COMPARATORS {
            evidence.push(SkV8ComparatorEvidence {
                comparator_id: (*id).into(),
                comparator_plane: "DOM".into(),
                comparator_strictness: "strict".into(),
                comparator_freshness: "absent:not-collected-for-test".into(),
                sidecar_freshness: "absent:not-collected-for-test".into(),
                value_mbps: None,
                source_artifact: format!("absence:w1:{corpus}:{workload}:{id}"),
            });
        }
        evidence
    }

    fn w0_telemetry(row_id: &str, _output_plane: &str) -> SkV8Telemetry {
        let (corpus, workload) = parse_row_id(row_id).unwrap();
        let (substrate_surface, structural_projection_status, substrate_cardinality) =
            w0_substrate_tuple(workload).unwrap();
        SkV8Telemetry {
            row_id: row_id.into(),
            grammar_id: "json".into(),
            domain: "json_bench".into(),
            measured_validation_path: "view-boundary".into(),
            profile_artifact: format!(
                "criterion-slope-profile:{}",
                expected_profile_path(row_id).unwrap()
            ),
            sample_cost: "ns_per_byte=1.000000;track1_ns=1.00;bytes=1".into(),
            sample_count: 100,
            build_flags: "profile=bench;rustflags=-C target-cpu=native;target_cpu=native".into(),
            host_triple: "aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max".into(),
            feature_mask: "arch=aarch64;os=macos;simd=Scalar;target_cpu=native".into(),
            costfacts_rule_id: "none:pre-W1".into(),
            costfacts_chosen_shape: "none:pre-W1".into(),
            costfacts_rejected_alternative_ids: vec!["none:pre-W1".into()],
            redress_entry: "none".into(),
            wave_id: "SK-V14-open".into(),
            run_id: TEST_SK_V9_OPEN_RUN_ID.into(),
            sk_v9_open_delta: "baseline".into(),
            track1_entry_point: skv14_track1_entry_point(workload).into(),
            track2_entry_point: skv14_track2_entry_point(workload).into(),
            comparator_plane: skv14_comparator_plane(corpus, workload),
            per_iter_equality: skv14_per_iter_equality(workload, 100),
            audit_overlay_verdict: skv14_audit_overlay_verdict(corpus, workload).into(),
            audit_overlay_reference: skv14_audit_overlay_reference(corpus, workload),
            sidecar_freshness: format!("absent:not-collected-for-{workload}"),
            substrate_target: skv14_substrate_target(workload).into(),
            retention_lifetime: skv14_retention_lifetime(workload).into(),
            policy_owner: skv14_policy_owner(workload).into(),
            sk_v14_open_delta: "baseline".into(),
            substrate_surface: substrate_surface.into(),
            structural_projection_status: structural_projection_status.into(),
            substrate_cardinality: substrate_cardinality.into(),
            same_wave_consumer_class: "gate_only".into(),
            track2_independence_status: "independent_verified".into(),
            diagnostic_nonproducer_status: "structural_scan+masking_probes+pmu+cycles:nonproducer"
                .into(),
            comparators: w0_evidence(row_id),
        }
    }

    fn w0_hot_leaf(row_id: &str) -> String {
        let profile = format!(
            "criterion-slope-profile:{}",
            expected_profile_path(row_id).unwrap()
        );
        format!("{profile};hot-leaf=criterion-slope-profile;row={row_id}")
    }

    fn w1a_report() -> NonJsonEvidenceReport {
        NonJsonEvidenceReport::from_json_str(include_str!(
            "../../../../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json"
        ))
        .unwrap()
    }

    fn w1a_reject(mut mutate: impl FnMut(&mut NonJsonEvidenceReport)) {
        let mut report = w1a_report();
        mutate(&mut report);
        assert!(report.validate_w1a_non_json_gate().is_err());
    }

    fn skv12_non_json_report() -> SkV12NonJsonReport {
        SkV12NonJsonReport {
            schema_id: SKV12_NON_JSON_REPORT_SCHEMA.into(),
            wave_id: "SK-V12-W1b-1".into(),
            run_id: "sk-v12-w1b-1:fixture-fnv64-0000000000000001".into(),
            rows: vec![SkV12NonJsonRow {
                row_id: "css_l4/declaration_values/direct_to_struct/main".into(),
                grammar_id: "css_l4".into(),
                domain: "non_json_generated:css_l4:declaration_values".into(),
                corpus_or_workload: "declaration_values".into(),
                workload: "direct_to_struct".into(),
                workload_class: "baseline".into(),
                output_plane: "css_l4_declaration_value_fact_stream".into(),
                outcome_id: "C".into(),
                verdict: "GO".into(),
                strictness: "strict".into(),
                generated_track1_source_path: "crates/codegen/src/css_l4_declaration_values_templates/generated.rs"
                    .into(),
                generated_runtime_path: "runtime::generated_css_l4_declaration_values::parser::parse".into(),
                generated_input_provenance:
                    "fixture:css_l4:declaration_values:sha256=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374".into(),
                grammar_checksum:
                    "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
                input_checksum:
                    "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374".into(),
                input_bytes: 187,
                track1_mbps: 12.0,
                track1_artifact: "criterion:sk-v12-w1b-1:fixture-fnv64-0000000000000001:target/criterion/nonjson_css_l4/track1_generated_css_l4_decl_values"
                    .into(),
                track2_or_oracle_source_path:
                    "cssparser-0.34:StyleSheetParser+RuleBodyParser:bench/nonjson_css_l4.rs".into(),
                track2_independence_status: "independent_verified".into(),
                track2_or_oracle_mbps: Some(10.0),
                strict_output_equality: "pass".into(),
                oracle_status: "same-plane:strict:independent:cssparser:fresh".into(),
                baseline_row_id: "none".into(),
                baseline_mbps: None,
                threshold_mbps: None,
                host_triple: "arch=aarch64;cpu=apple-m5-max".into(),
                feature_mask: "arch=aarch64;os=macos;simd=neon;target_cpu=native".into(),
                build_flags: "profile=bench;rustflags=-C target-cpu=native".into(),
                sample_count: 30,
                sample_cost: "ns_per_byte=0.83".into(),
                benchmark_artifact_path:
                    "criterion:sk-v12-w1b-1:fixture-fnv64-0000000000000001:target/criterion/nonjson_css_l4".into(),
                measured_validation_path: "track1-vs-cssparser-byte-identical-fact-stream".into(),
                profile_artifact: "profile:not_required_for_generated_baseline".into(),
                generated_loc: 120,
                generated_module_bytes: 4096,
                grammar_size_guard: "pass:generated_loc<=360".into(),
                lock14_status: "pass:lock14_baseline::validate".into(),
                lock16_status: "n/a:scalar-css-scaffold-no-simd".into(),
                scalar_reference_status: "pass:cssparser_oracle".into(),
                checkasm_or_parity_status: "pass:track1_equals_cssparser".into(),
                json_guard_state: "refreshed:sk-v12-w1b-1:guards-pass".into(),
                redress_entry: "none".into(),
                same_wave_consumer_class: "companion_gate_generated_css_l4_baseline".into(),
                gate_status: "pass".into(),
            }],
        }
    }

    fn skv12_reject(mut mutate: impl FnMut(&mut SkV12NonJsonReport)) {
        let mut report = skv12_non_json_report();
        mutate(&mut report);
        assert!(report.validate_gate().is_err());
    }

    fn skv12_css_l4_sota_report() -> SkV12CssL4SotaReport {
        SkV12CssL4SotaReport {
            schema_id: SKV12_CSS_L4_SOTA_REPORT_SCHEMA.into(),
            wave_id: "SK-V12-W1b-2b".into(),
            run_id: "sk-v12-w1b-2b:criterion-fnv64-27240148e5780a54".into(),
            rows: vec![SkV12CssL4SotaRow {
                schema_id: SKV12_CSS_L4_SOTA_REPORT_SCHEMA.into(),
                wave_id: "SK-V12-W1b-2b".into(),
                run_id: "sk-v12-w1b-2b:criterion-fnv64-27240148e5780a54".into(),
                row_id: "css_l4/declaration_values/direct_to_struct/main".into(),
                grammar_id: "css_l4".into(),
                domain: "non_json_generated:css_l4:declaration_values".into(),
                corpus_or_workload: "declaration_values".into(),
                workload: "direct_to_struct".into(),
                output_plane: "css_l4_declaration_value_fact_stream".into(),
                strictness: "strict".into(),
                outcome_id: "A".into(),
                verdict: "GO".into(),
                gate_status: "pass".into(),
                generated_track1_source_path:
                    "crates/codegen/src/css_l4_declaration_values_templates/generated.rs".into(),
                generated_runtime_path:
                    "runtime::generated_css_l4_declaration_values::parser::parse".into(),
                generated_input_provenance:
                    "fixture:css_l4:declaration_values:sha256=cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374".into(),
                grammar_checksum:
                    "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
                input_checksum:
                    "cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374".into(),
                input_bytes: 187,
                generated_loc: 287,
                generated_module_bytes: 9243,
                grammar_size_guard: "pass:generated_loc<=360".into(),
                track1_mbps: 429.34,
                track2_or_oracle_mbps: 217.43,
                lightningcss_mbps: 168.93,
                threshold_mbps: 169.93,
                admission_margin_mbps: 259.41,
                admission_status: "PASS-ADMIT-CANDIDATE".into(),
                track1_artifact:
                    "../restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt"
                        .into(),
                cssparser_artifact_path:
                    "../restart/skinny/tranches/sk-v12/research/w1b/artifacts/oracle-facts.txt"
                        .into(),
                track2_or_oracle_source_path:
                    "cssparser-0.34:StyleSheetParser+RuleBodyParser:bench/nonjson_css_l4.rs"
                        .into(),
                lightningcss_command:
                    "lightningcss-1.0.0-alpha.71:same-plane-source-sidecar".into(),
                lightningcss_artifact:
                    "../restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-strict-equality.txt"
                        .into(),
                lightningcss_fact_artifact_path:
                    "../restart/skinny/tranches/sk-v12/research/w1b/artifacts/lightningcss-facts.txt"
                        .into(),
                fact_stream_sha256:
                    "caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c".into(),
                strict_output_equality: "pass".into(),
                three_way_equality: "pass:track1=cssparser=lightningcss".into(),
                lightningcss_sequence_status: "pass:ast_projection_matches_source_sidecar".into(),
                track2_independence_status: "independent_verified".into(),
                measured_validation_path:
                    "../restart/skinny/tranches/sk-v12/research/w1b/artifacts/strict-equality.txt"
                        .into(),
                benchmark_artifact_path: "criterion:target/criterion/nonjson_css_l4".into(),
                profile_artifact: "n/a:w1b-2b-report-gate-consumes-w1b-2a-criterion".into(),
                sample_count: 30,
                sample_cost:
                    "track1_mean_ns=3484.383794;cssparser_mean_ns=6880.481226;lightningcss_mean_ns=8855.758871"
                        .into(),
                host_triple: "arch=aarch64;cpu=apple-m5-max".into(),
                feature_mask: "arch=aarch64;os=macos;simd=neon;target_cpu=native".into(),
                build_flags: "profile=bench;rustflags=-C target-cpu=native".into(),
                lock14_status: "pass:lock14_baseline::validate".into(),
                lock16_status: "n/a:no_simd_or_asm_claim".into(),
                scalar_reference_status: "pass:cssparser_oracle".into(),
                checkasm_or_parity_status: "pass:three_way_fact_stream".into(),
                json_guard_state: "refreshed:skv12-w1a-json-guard-criterion:guards-pass".into(),
                same_wave_consumer_class: "companion_gate_css_l4_lightningcss_sota".into(),
                redress_entry: "REDRESS-125".into(),
            }],
        }
    }

    fn skv12_css_l4_reject(mut mutate: impl FnMut(&mut SkV12CssL4SotaReport)) {
        let mut report = skv12_css_l4_sota_report();
        mutate(&mut report);
        assert!(report.validate_gate().is_err());
    }

    fn skv13_css_comparator_report() -> SkV13CssComparatorOracleReport {
        let mut rows = Vec::new();
        for feature in SKV13_CSS_FEATURES {
            let row_id = format!("css_l4/{feature}/direct_to_struct/main");
            if *feature == "declaration_values" {
                rows.push(SkV13CssFeatureCoverageRow {
                    row_id,
                    css_feature_id: feature.to_string(),
                    row_state: "admission_candidate".into(),
                    row_presence: "measured".into(),
                    css_feature_status: "ADMITTED-PARITY".into(),
                    planned_wave: "SK-V12-W1b-2b".into(),
                    absence_reason: "n/a".into(),
                    output_plane: "css_l4_declaration_value_fact_stream".into(),
                    feature_accept_count: 1,
                    feature_reject_count: 0,
                    feature_coverage_status: "pass:strict-equality".into(),
                    cssparser_or_golden_oracle: "cssparser-0.34".into(),
                    same_plane_fact_contract: "pass:track1=cssparser=lightningcss".into(),
                    admission_status: "PASS-MAINTAIN".into(),
                });
            } else {
                rows.push(SkV13CssFeatureCoverageRow {
                    row_id,
                    css_feature_id: feature.to_string(),
                    row_state: "open".into(),
                    row_presence: "absent_until_planned_wave".into(),
                    css_feature_status: "OPEN".into(),
                    planned_wave: "W10.N".into(),
                    absence_reason: "not-yet-generated".into(),
                    output_plane: "pending:same-plane-fact-stream".into(),
                    feature_accept_count: 0,
                    feature_reject_count: 0,
                    feature_coverage_status: "open:awaiting-row-wave".into(),
                    cssparser_or_golden_oracle: "pending".into(),
                    same_plane_fact_contract: "pending".into(),
                    admission_status: "not-admitted:absent".into(),
                });
            }
        }
        SkV13CssComparatorOracleReport {
            schema_id: SKV13_CSS_COMPARATOR_ORACLE_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W1".into(),
            run_id: "sk-v13-w1:coverage-fnv64-0000000000000001".into(),
            declaration_values_sota_report_path:
                "../restart/skinny/tranches/sk-v12/research/w1b/skv12-W1b-css-l4-sota.json".into(),
            coverage: SkV13CssCoverageSummary {
                feature_row_count: 24,
                measured_row_count: 1,
                open_absent_row_count: 23,
                admission_eligible_row_count: 1,
                admitted_row_count: 1,
                feature_accept_count: 1,
                feature_reject_count: 0,
            },
            rows,
        }
    }

    fn skv13_css_comparator_reject(mut mutate: impl FnMut(&mut SkV13CssComparatorOracleReport)) {
        let mut report = skv13_css_comparator_report();
        mutate(&mut report);
        assert!(report.validate_gate().is_err());
    }

    fn opening_report() -> Report {
        let mut report = Report::new("Skinny JSON Bench");
        for baseline in SK_V8_OPEN_BASELINE {
            let mut parts = baseline.row_id.split('/');
            let _grammar = parts.next().unwrap();
            let corpus = parts.next().unwrap();
            let workload = parts.next().unwrap();
            let bytes = 1_000_000u64;
            let track1_ns = Some(bytes as f64 * 8_000.0 / baseline.track1_mbps);
            let track2_ns = Some(bytes as f64 * 8_000.0 / baseline.track2_mbps);
            let output_plane = if workload == "parse_only" {
                "parse_only"
            } else if workload == "real_typed_struct" {
                "typed direct"
            } else {
                "digest"
            };
            let row = if workload == "parse_only" {
                TelemetryRow::parse(
                    corpus,
                    gate::parse_outcome_id(baseline.outcome_id).unwrap(),
                    bytes,
                    track1_ns,
                    track2_ns,
                    comparators(),
                    w0_hot_leaf(baseline.row_id),
                )
            } else {
                TelemetryRow::workload(
                    corpus,
                    workload,
                    (baseline.outcome_id != "A")
                        .then(|| gate::parse_outcome_id(baseline.outcome_id).unwrap()),
                    bytes,
                    track1_ns,
                    track2_ns,
                    comparators(),
                    output_plane,
                    "none",
                    "PASS",
                    w0_hot_leaf(baseline.row_id),
                )
            };
            report
                .rows
                .push(row.with_sk_v8(w0_telemetry(baseline.row_id, output_plane)));
        }
        report
    }

    fn admit_direct_contract(row: &mut TelemetryRow) {
        row.outcome_id = "A".into();
        row.verdict = "GO".into();
        row.track1_mbps = Some(12_000.0);
        row.track2_mbps = Some(12_000.0);
        row.strictness = "strict".into();
        row.parse_utf8 = "measured-row".into();
        row.output_plane = "digest".into();
        row.sk_v8.measured_validation_path = "measured-row".into();
        row.sk_v8.same_wave_consumer_class = "gate_json_direct_contract".into();
        row.sk_v8.redress_entry = "REDRESS-101".into();
        row.sk_v8.wave_id = "SK-V10-W2".into();
    }

    fn admit_parse_only_contract(row: &mut TelemetryRow, spec: &JsonParseOnlyAdmissionSpec) {
        row.outcome_id = "A".into();
        row.verdict = "GO".into();
        row.track1_mbps = Some(30_035.0);
        row.track2_mbps = Some(20_867.0);
        row.competitors.sonic_strict_mbps = Some(25_545.0);
        row.strictness = "strict".into();
        row.parse_utf8 = "measured-row".into();
        row.escape_complete = "yes".into();
        row.output_plane = "parse_only".into();
        row.flaw_probe = "generated Track 1 distinct parse_only contract vs independent hand Track 2/oracle; UTF-8 measured in row".into();
        row.sk_v8.measured_validation_path = "measured-row".into();
        row.sk_v8.same_wave_consumer_class = "generated_json_parse_only_contract".into();
        row.sk_v8.redress_entry = spec.redress_entry.into();
        row.sk_v8.wave_id = spec.wave_id.into();
        row.sk_v8.run_id =
            TEST_SK_V9_OPEN_RUN_ID.replacen(SK_V9_OPEN_RUN_ID_PREFIX, spec.run_id_prefix, 1);
        row.sk_v8.audit_overlay_verdict = "AUDIT-SUSTAINED".into();
        row.sk_v8.audit_overlay_reference = json_parse_only_audit_reference(spec).into();
        row.sk_v8.sk_v9_open_delta = json_parse_only_open_delta(spec).into();
        row.sk_v8.sk_v14_open_delta = json_parse_only_open_delta(spec).into();
    }

    fn w6_github_events_typed_row(track1_mbps: f64, track2_mbps: f64) -> TelemetryRow {
        let bytes = 1_000_000u64;
        let row_id = W6_GITHUB_EVENTS_TYPED_ROW_ID;
        let mut row = TelemetryRow::workload(
            "github_events",
            "real_typed_struct",
            None,
            bytes,
            Some(ns_for_mbps(bytes, track1_mbps)),
            Some(ns_for_mbps(bytes, track2_mbps)),
            comparators(),
            "typed direct",
            "generated Track 1 consumes host/API output schema; Track 2 is an independent typed oracle",
            "PASS W6 github_events typed admission",
            w0_hot_leaf(row_id),
        )
        .with_sk_v8(w0_telemetry(row_id, "typed direct"));
        admit_w6_typed_contract(&mut row);
        row
    }

    fn admit_w6_typed_contract(row: &mut TelemetryRow) {
        row.strictness = "strict".into();
        row.parse_utf8 = "measured-row".into();
        row.output_plane = "typed direct".into();
        row.sk_v8.measured_validation_path = "measured-row".into();
        row.sk_v8.same_wave_consumer_class = "gate_json_typed_contract".into();
        row.sk_v8.redress_entry = "REDRESS-105".into();
        row.sk_v8.wave_id = "SK-V10-W6".into();
        row.sk_v8.sk_v9_open_delta = "typed-row-added".into();
    }

    #[test]
    fn w1a_non_json_report_accepts_css_l4_schema_fixture() {
        assert!(w1a_report().validate_w1a_non_json_gate().is_ok());
    }

    #[test]
    fn w1a_non_json_report_rejects_identity_domain_and_row_id_mismatch() {
        w1a_reject(|report| report.rows[0].sk_v8.domain = "json_bench".into());
        w1a_reject(|report| {
            report.rows[0].sk_v8.row_id = "json/declaration_values/direct/main".into()
        });
        w1a_reject(|report| report.rows[0].corpus = "wrong".into());
    }

    #[test]
    fn w1a_non_json_report_rejects_missing_required_context() {
        w1a_reject(|report| report.run_id = "sk-v11-w1a:test".into());
        w1a_reject(|report| report.rows[0].sk_v8.sample_count = 0);
        w1a_reject(|report| report.rows[0].sk_v8.build_flags = "profile=bench".into());
    }

    #[test]
    fn w1a_non_json_report_rejects_oracle_plane_source_and_coupling() {
        w1a_reject(|report| {
            report.rows[0].sk_v8.comparators[0].comparator_plane = "typed direct".into()
        });
        w1a_reject(|report| {
            report.rows[0].sk_v8.track2_independence_status = "coupled_to_track1".into()
        });
        w1a_reject(|report| {
            report.rows[0].sk_v8.comparators[0].source_artifact =
                "criterion:json_twitter/track1_generated/new/estimates.json".into()
        });
        let coupled = NonJsonEvidenceReport::from_json_str(include_str!(
            "../../../../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-coupled.json"
        ))
        .unwrap();
        assert!(coupled.validate_w1a_non_json_gate().is_err());
        let shared_source = NonJsonEvidenceReport::from_json_str(include_str!(
            "../../../../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-track2-shared-source.json"
        ))
        .unwrap();
        assert!(shared_source.validate_w1a_non_json_gate().is_err());
    }

    #[test]
    fn w1a_non_json_report_rejects_gate_only_and_admission_claims() {
        w1a_reject(|report| report.rows[0].sk_v8.same_wave_consumer_class = "gate_only".into());
        let admission = NonJsonEvidenceReport::from_json_str(include_str!(
            "../../../../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-admission-claim.json"
        ))
        .unwrap();
        assert!(admission.validate_w1a_non_json_gate().is_err());
    }

    #[test]
    fn w1a_non_json_report_rejects_unknown_producer_fields() {
        assert!(NonJsonEvidenceReport::from_json_str(include_str!(
            "../../../../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-producer-only-extra-field.json"
        ))
        .is_err());
    }

    #[test]
    fn skv12_non_json_report_accepts_generated_baseline() {
        assert!(skv12_non_json_report().validate_gate().is_ok());
    }

    #[test]
    fn skv12_non_json_report_rejects_required_w0_failure_classes() {
        skv12_reject(|report| report.schema_id = W1A_NON_JSON_REPORT_SCHEMA.into());
        skv12_reject(|report| report.run_id = "sk-v11-w1a:fixture-fnv64-0000000000000001".into());
        skv12_reject(|report| report.rows[0].grammar_id = "json".into());
        skv12_reject(|report| report.rows[0].domain = "json_bench".into());
        skv12_reject(|report| report.rows[0].generated_track1_source_path = "".into());
        skv12_reject(|report| {
            report.rows[0].generated_runtime_path =
                "bbnf::runtime::grammars::sheets_witness::parse".into()
        });
        skv12_reject(|report| {
            report.rows[0].track2_or_oracle_source_path =
                report.rows[0].generated_track1_source_path.clone()
        });
        skv12_reject(|report| report.rows[0].same_wave_consumer_class = "gate_only".into());
        skv12_reject(|report| report.rows[0].gate_status = "".into());
        skv12_reject(|report| report.rows[0].track1_mbps = 0.5);
    }

    #[test]
    fn skv12_non_json_report_rejects_unknown_producer_fields() {
        let mut value = serde_json::to_value(skv12_non_json_report()).unwrap();
        value["rows"][0]["producer_only_field"] = serde_json::json!("not consumed");
        assert!(SkV12NonJsonReport::from_json_str(&value.to_string()).is_err());
    }

    #[test]
    fn skv12_css_l4_sota_report_accepts_admit_candidate() {
        assert!(skv12_css_l4_sota_report().validate_gate().is_ok());
    }

    #[test]
    fn skv12_css_l4_sota_report_rejects_required_fields() {
        skv12_css_l4_reject(|report| report.schema_id = SKV12_NON_JSON_REPORT_SCHEMA.into());
        skv12_css_l4_reject(|report| report.wave_id = "SK-V12-W1b-2a".into());
        skv12_css_l4_reject(|report| report.rows[0].row_id = "json/twitter/direct/main".into());
        skv12_css_l4_reject(|report| report.rows[0].input_bytes = 188);
        skv12_css_l4_reject(|report| {
            report.rows[0].track2_or_oracle_source_path =
                "generated_css_l4_declaration_values::parser::parse".into()
        });
        skv12_css_l4_reject(|report| report.rows[0].fact_stream_sha256 = "abc".into());
        skv12_css_l4_reject(|report| report.rows[0].same_wave_consumer_class = "gate_only".into());
    }

    #[test]
    fn skv12_css_l4_sota_report_rejects_stale_math_and_status() {
        skv12_css_l4_reject(|report| report.rows[0].threshold_mbps = 168.93);
        skv12_css_l4_reject(|report| report.rows[0].admission_margin_mbps = 1.0);
        skv12_css_l4_reject(|report| {
            report.rows[0].admission_status = "PASS-MEASURED-BASELINE".into()
        });
        let mut baseline = skv12_css_l4_sota_report();
        baseline.rows[0].track1_mbps = 169.93;
        baseline.rows[0].admission_margin_mbps = 0.0;
        baseline.rows[0].admission_status = "PASS-MEASURED-BASELINE".into();
        baseline.rows[0].outcome_id = "C".into();
        assert!(baseline.validate_gate().is_ok());
    }

    #[test]
    fn skv12_css_l4_sota_report_rejects_unknown_producer_fields() {
        let mut value = serde_json::to_value(skv12_css_l4_sota_report()).unwrap();
        value["rows"][0]["producer_only_field"] = serde_json::json!("not consumed");
        assert!(SkV12CssL4SotaReport::from_json_str(&value.to_string()).is_err());
    }

    #[test]
    fn skv13_css_comparator_report_accepts_full_matrix() {
        assert!(skv13_css_comparator_report().validate_gate().is_ok());
    }

    #[test]
    fn skv13_css_comparator_report_rejects_missing_or_stale_coverage() {
        skv13_css_comparator_reject(|report| {
            report.rows.retain(|row| row.css_feature_id != "flexbox")
        });
        skv13_css_comparator_reject(|report| report.coverage.open_absent_row_count = 22);
        skv13_css_comparator_reject(|report| report.rows[1].css_feature_status = "PARTIAL".into());
        skv13_css_comparator_reject(|report| {
            report.rows[1].admission_status = "PASS-ADMIT-CANDIDATE".into()
        });
        skv13_css_comparator_reject(|report| {
            report.rows[0].same_plane_fact_contract = "pass:track1=cssparser".into()
        });
    }

    #[test]
    fn skv13_css_comparator_report_rejects_unknown_producer_fields() {
        let mut value = serde_json::to_value(skv13_css_comparator_report()).unwrap();
        value["rows"][0]["producer_only_field"] = serde_json::json!("not consumed");
        assert!(SkV13CssComparatorOracleReport::from_json_str(&value.to_string()).is_err());
    }

    #[test]
    fn renders_schema_v3_header_and_parse_workload() {
        let mut report = Report::new("Skinny JSON Bench");
        report.push_row(
            "twitter",
            Outcome::ABeatAndParity,
            631_515,
            Some(390_000.0),
            Some(360_000.0),
            comparators(),
        );
        let markdown = report.render_markdown();
        assert!(markdown.contains(SCHEMA_V3_HEADER));
        assert!(markdown.contains("| twitter | parse_only | A | GO | deferred |"));
        assert!(report.validate_schema_v3().is_ok());
    }

    #[test]
    fn schema_v3_rejects_missing_required_comparator() {
        let mut report = Report::new("Skinny JSON Bench");
        let mut row = TelemetryRow::parse(
            "twitter",
            Outcome::ABeatAndParity,
            631_515,
            Some(390_000.0),
            Some(360_000.0),
            comparators(),
            "unprofiled",
        );
        row.competitors.sonic_strict_mbps = None;
        report.rows.push(row);
        assert!(report.validate_schema_v3().is_err());
    }

    #[test]
    fn renders_probe_rows() {
        let mut report = Report::new("Skinny JSON Bench");
        report.push_probe_row(
            "twitter",
            "host_call_eager_decode",
            631_515,
            Some(430_000.0),
            Some(390_000.0),
            "PASS",
        );
        let markdown = report.render_markdown();
        assert!(markdown.contains("## Masking Probes"));
        assert!(markdown
            .contains("| twitter | host_call_eager_decode | 11749 | 430000.00 | 90.7% | PASS |"));
    }

    #[test]
    fn w0_manifest_renders_required_fields() {
        let mut report = Report::new("Skinny JSON Bench");
        let row = TelemetryRow::parse(
            "twitter",
            Outcome::KSimdParityHashFail,
            631_515,
            Some(320_728.80),
            Some(410_427.35),
            comparators(),
            w0_hot_leaf("json/twitter/parse_only/main"),
        )
        .with_sk_v8(w0_telemetry(
            "json/twitter/parse_only/main",
            "borrowed_view_over_offset_tape",
        ));
        report.rows.push(row);
        let markdown = report.render_markdown();
        assert!(markdown.contains("## SK-V14 W0 Telemetry Manifest"));
        assert!(markdown.contains("json/twitter/parse_only/main"));
        assert!(markdown.contains("none:pre-W1"));
        assert!(markdown.contains("sonic_rs::Skipper"));
        assert!(markdown.contains("AUDIT-PENDING"));
        assert!(markdown.contains("absent:not-collected-for-test"));
    }

    #[test]
    fn w0_schema_rejects_missing_delta_or_profile() {
        let mut row = TelemetryRow::parse(
            "twitter",
            Outcome::KSimdParityHashFail,
            631_515,
            Some(320_728.80),
            Some(410_427.35),
            comparators(),
            w0_hot_leaf("json/twitter/parse_only/main"),
        )
        .with_sk_v8(w0_telemetry(
            "json/twitter/parse_only/main",
            "borrowed_view_over_offset_tape",
        ));
        row.sk_v8.sk_v9_open_delta.clear();
        assert!(row.validate_sk_v8_w0().is_err());
        row.sk_v8.sk_v9_open_delta = "baseline".into();
        row.sk_v8.profile_artifact = "unprofiled".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_malformed_sidecar_evidence() {
        let mut row = TelemetryRow::parse(
            "twitter",
            Outcome::KSimdParityHashFail,
            631_515,
            Some(320_728.80),
            Some(410_427.35),
            comparators(),
            w0_hot_leaf("json/twitter/parse_only/main"),
        )
        .with_sk_v8(w0_telemetry(
            "json/twitter/parse_only/main",
            "borrowed_view_over_offset_tape",
        ));
        let sidecar = row
            .sk_v8
            .comparators
            .iter_mut()
            .find(|entry| entry.comparator_id == "simdjson_dom")
            .unwrap();
        sidecar.value_mbps = Some(100.0);
        sidecar.sidecar_freshness = "absent:not-collected".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_row_id_rendered_identity_mismatch() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        row.sk_v8.row_id = "json/twitter/parse_only/main".into();
        row.sk_v8.profile_artifact =
            "criterion-slope-profile:json_twitter/track1_generated/new/estimates.json".into();
        row.hot_leaf = w0_hot_leaf("json/twitter/parse_only/main");
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_unsupported_outcome_and_strict_view_boundary_claim() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        row.outcome_id = "F-positive".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.outcome_id = "A".into();
        row.strictness = "strict".into();
        row.sk_v8.measured_validation_path = "measured-row".into();
        row.parse_utf8 = "view-boundary".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_deferred_validation_semantic_drift() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        assert!(row.validate_sk_v8_w0().is_ok());

        row.parse_utf8 = "none".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.parse_utf8 = "view-boundary".into();
        row.escape_complete = "n/a".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_native_comparator_source_mismatch() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        let sonic = row
            .sk_v8
            .comparators
            .iter_mut()
            .find(|entry| entry.comparator_id == "sonic_rs_strict")
            .unwrap();
        sonic.source_artifact = "criterion:json_twitter/sonic_rs_skipper/new/estimates.json".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_native_comparator_semantic_mismatch() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        let sonic_idx = row
            .sk_v8
            .comparators
            .iter()
            .position(|entry| entry.comparator_id == "sonic_rs_strict")
            .unwrap();
        row.sk_v8.comparators[sonic_idx].comparator_plane = "DOM".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.sk_v8.comparators[sonic_idx].comparator_plane = "digest".into();
        row.sk_v8.comparators[sonic_idx].comparator_freshness = "historical:old".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.sk_v8.comparators[sonic_idx].comparator_freshness = "same-run-native".into();
        row.sk_v8.comparators[sonic_idx].sidecar_freshness = "historical:old".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.sk_v8.comparators[sonic_idx].sidecar_freshness = "n/a".into();
        row.sk_v8.comparators[sonic_idx].value_mbps = None;
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_sidecar_source_and_freshness_mismatch() {
        let mut row = TelemetryRow::parse(
            "twitter",
            Outcome::KSimdParityHashFail,
            631_515,
            Some(320_728.80),
            Some(410_427.35),
            comparators(),
            w0_hot_leaf("json/twitter/parse_only/main"),
        )
        .with_sk_v8(w0_telemetry(
            "json/twitter/parse_only/main",
            "borrowed_view_over_offset_tape",
        ));
        let sidecar_idx = row
            .sk_v8
            .comparators
            .iter()
            .position(|entry| entry.comparator_id == "simdjson_dom")
            .unwrap();
        row.sk_v8.comparators[sidecar_idx].source_artifact =
            "absence:w1:wrong:parse_only:simdjson_dom".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.sk_v8.comparators[sidecar_idx].source_artifact =
            "absence:w1:twitter:parse_only:simdjson_dom".into();
        row.sk_v8.comparators[sidecar_idx].comparator_freshness = "sidecar-same-run".into();
        row.sk_v8.comparators[sidecar_idx].sidecar_freshness = "sidecar-same-run".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_unknown_comparator_strict_admission_shape() {
        let mut row = TelemetryRow::workload(
            "twitter",
            "direct_to_struct",
            None,
            1_000_000,
            Some(84_324.14),
            Some(101_204.33),
            comparators(),
            "digest",
            "none",
            "PASS",
            w0_hot_leaf("json/twitter/direct_to_struct/main"),
        )
        .with_sk_v8(w0_telemetry("json/twitter/direct_to_struct/main", "digest"));
        row.strictness = "strict".into();
        row.parse_utf8 = "measured-row".into();
        row.sk_v8.measured_validation_path = "measured-row".into();
        row.sk_v8.comparators.push(SkV8ComparatorEvidence {
            comparator_id: "unknown_sidecar".into(),
            comparator_plane: "digest".into(),
            comparator_strictness: "strict".into(),
            comparator_freshness: "sidecar-same-run".into(),
            sidecar_freshness: "sidecar-same-run".into(),
            value_mbps: Some(12_000.0),
            source_artifact: "sidecar-profile:unstructured".into(),
        });
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_rejects_profile_placeholder_shape() {
        let mut row = TelemetryRow::parse(
            "twitter",
            Outcome::KSimdParityHashFail,
            631_515,
            Some(320_728.80),
            Some(410_427.35),
            comparators(),
            "criterion:json_twitter/track1_generated/new/estimates.json;hot-leaf=criterion-slope;row=json/twitter/parse_only/main",
        )
        .with_sk_v8(w0_telemetry(
            "json/twitter/parse_only/main",
            "borrowed_view_over_offset_tape",
        ));
        row.sk_v8.profile_artifact =
            "criterion:json_twitter/track1_generated/new/estimates.json".into();
        assert!(row.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn direct_contract_accepts_complete_n_direct_movement() {
        let mut report = opening_report();
        let direct = report
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/apache_builds/direct_to_struct/main")
            .unwrap();
        admit_direct_contract(direct);
        assert_eq!(report.validate_sk_v8_w0(), Ok(()));
    }

    #[test]
    fn direct_contract_rejects_incomplete_movement() {
        let reject = |mutate: fn(&mut TelemetryRow)| {
            let mut report = opening_report();
            let direct = report
                .rows
                .iter_mut()
                .find(|row| row.sk_v8.row_id == "json/apache_builds/direct_to_struct/main")
                .unwrap();
            admit_direct_contract(direct);
            mutate(direct);
            assert!(report.validate_sk_v8_w0().is_err());
        };

        reject(|row| row.output_plane = "DOM".into());
        reject(|row| row.strictness = "deferred".into());
        reject(|row| row.parse_utf8 = "view-boundary".into());
        reject(|row| row.sk_v8.measured_validation_path = "view-boundary".into());
        reject(|row| row.sk_v8.same_wave_consumer_class = "gate_only".into());
        reject(|row| row.sk_v8.redress_entry = "none".into());
        reject(|row| row.sk_v8.wave_id = "SK-V9-open".into());
        reject(|row| row.sk_v8.track2_independence_status = "unverified".into());
        reject(|row| {
            let sonic = row
                .sk_v8
                .comparators
                .iter_mut()
                .find(|entry| entry.comparator_id == "sonic_rs_strict")
                .unwrap();
            sonic.comparator_plane = "DOM".into();
        });
        reject(|row| {
            let sonic = row
                .sk_v8
                .comparators
                .iter_mut()
                .find(|entry| entry.comparator_id == "sonic_rs_strict")
                .unwrap();
            sonic.source_artifact =
                "criterion:json_twitter/sonic_rs_skipper/new/estimates.json".into();
        });
    }

    #[test]
    fn direct_contract_rejects_floor_miss() {
        let mut report = opening_report();
        let direct = report
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/twitter/direct_to_struct/main")
            .unwrap();
        admit_direct_contract(direct);
        assert!(report.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn validate_sk_v8_w0_accepts_configured_parse_only_admission_rows() {
        for spec in JSON_PARSE_ONLY_ADMISSION_SPECS {
            let mut report = opening_report();
            let parse = report
                .rows
                .iter_mut()
                .find(|row| row.sk_v8.row_id == spec.row_id)
                .unwrap();
            admit_parse_only_contract(parse, spec);
            let manifest = parse.skv14_manifest_row();
            validate_skv14_manifest_row(&manifest).unwrap();
            validate_skv14_sustained_row(&manifest).unwrap();
        }
    }

    #[test]
    fn direct_contract_accepts_w11_3_mesh_track1_sota_reopen() {
        let mut report = opening_report();
        let direct = report
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/mesh/direct_to_struct/main")
            .unwrap();
        admit_direct_contract(direct);
        direct.track1_mbps = Some(9631.0);
        direct.track2_mbps = Some(7828.0);
        direct.competitors.sonic_strict_mbps = Some(9581.0);
        direct.sk_v8.same_wave_consumer_class = "direct_sink_stack_specialization".into();
        direct.sk_v8.redress_entry = "REDRESS-143".into();
        direct.sk_v8.wave_id = "SK-V13-W11.3".into();
        assert_eq!(report.validate_sk_v8_w0(), Ok(()));
    }

    #[test]
    fn w6_typed_contract_accepts_complete_github_events_row() {
        let mut report = opening_report();
        report
            .rows
            .push(w6_github_events_typed_row(12_000.0, 12_000.0));
        assert_eq!(report.validate_sk_v8_w0(), Ok(()));
    }

    #[test]
    fn w6_typed_contract_rejects_incomplete_github_events_row() {
        let reject = |mutate: fn(&mut TelemetryRow)| {
            let mut report = opening_report();
            report
                .rows
                .push(w6_github_events_typed_row(12_000.0, 12_000.0));
            let typed = report
                .rows
                .iter_mut()
                .find(|row| row.sk_v8.row_id == W6_GITHUB_EVENTS_TYPED_ROW_ID)
                .unwrap();
            mutate(typed);
            assert!(report.validate_sk_v8_w0().is_err());
        };

        reject(|row| row.output_plane = "DOM".into());
        reject(|row| row.strictness = "deferred".into());
        reject(|row| row.parse_utf8 = "view-boundary".into());
        reject(|row| row.sk_v8.measured_validation_path = "view-boundary".into());
        reject(|row| row.sk_v8.same_wave_consumer_class = "gate_only".into());
        reject(|row| row.sk_v8.redress_entry = "none".into());
        reject(|row| row.sk_v8.wave_id = "SK-V9-open".into());
        reject(|row| row.sk_v8.track2_independence_status = "unverified".into());
    }

    #[test]
    fn w6_typed_contract_rejects_track2_floor_miss() {
        let mut report = opening_report();
        report
            .rows
            .push(w6_github_events_typed_row(12_000.0, 10_000.0));
        assert!(report.validate_sk_v8_w0().is_err());
    }

    #[test]
    fn w0_report_accepts_exact_opening_baseline() {
        let report = opening_report();
        assert_eq!(report.validate_sk_v8_w0(), Ok(()));
        let mut fresh_throughput = report.clone();
        fresh_throughput.rows[0].track1_mbps = Some(SK_V8_OPEN_BASELINE[0].track1_mbps * 1.37);
        fresh_throughput.rows[0].track2_mbps = Some(SK_V8_OPEN_BASELINE[0].track2_mbps * 0.72);
        assert!(fresh_throughput.validate_sk_v8_w0().is_ok());

        let mut fresh_diagnostic_label = report.clone();
        let canada = fresh_diagnostic_label
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/canada/parse_only/main")
            .unwrap();
        canada.outcome_id = "S".into();
        canada.verdict = "NO-GO".into();
        assert!(fresh_diagnostic_label.validate_sk_v8_w0().is_ok());

        let mut bad_parse_outcome = report.clone();
        let parse = bad_parse_outcome
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/twitter/parse_only/main")
            .unwrap();
        parse.outcome_id = "K".into();
        assert!(bad_parse_outcome.validate_sk_v8_w0().is_err());

        let mut bad_direct_verdict = report.clone();
        let direct = bad_direct_verdict
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/twitter/direct_to_struct/main")
            .unwrap();
        direct.outcome_id = "A".into();
        direct.verdict = "GO".into();
        assert!(bad_direct_verdict.validate_sk_v8_w0().is_err());

        let mut bad_single_run_id = report.clone();
        bad_single_run_id.rows[0].sk_v8.run_id = "sk-v9-open:test".into();
        assert!(bad_single_run_id.validate_sk_v8_w0().is_err());

        let mut bad_uniform_run_id = report.clone();
        for row in &mut bad_uniform_run_id.rows {
            row.sk_v8.run_id = "sk-v9-open:test".into();
        }
        assert!(bad_uniform_run_id.validate_sk_v8_w0().is_err());

        let mut bad_mixed_valid_run_id = report.clone();
        bad_mixed_valid_run_id.rows[0].sk_v8.run_id =
            "sk-v9-open:criterion-fnv64-fedcba9876543210".into();
        assert!(bad_mixed_valid_run_id.validate_sk_v8_w0().is_err());

        let mut bad_strict_hard_failure = report.clone();
        let canada = bad_strict_hard_failure
            .rows
            .iter_mut()
            .find(|row| row.sk_v8.row_id == "json/canada/parse_only/main")
            .unwrap();
        canada.strictness = "strict".into();
        canada.parse_utf8 = "measured-row".into();
        canada.output_plane = "DOM".into();
        canada.sk_v8.measured_validation_path = "measured-row".into();
        assert!(bad_strict_hard_failure.validate_sk_v8_w0().is_err());

        let reject = |mutate: fn(&mut TelemetryRow)| {
            let mut bad = report.clone();
            mutate(&mut bad.rows[0]);
            assert!(bad.validate_sk_v8_w0().is_err());
        };
        reject(|row| row.sk_v8.costfacts_rule_id = "future:rule".into());
        reject(|row| row.sk_v8.costfacts_rejected_alternative_ids = vec!["other".into()]);
        reject(|row| row.sk_v8.redress_entry = "REDRESS-1".into());
        reject(|row| row.sk_v8.track2_independence_status = "unverified".into());
        reject(|row| {
            row.sk_v8.build_flags = "profile=bench;rustflags=<empty>;target_cpu=default".into()
        });
        reject(|row| row.sk_v8.host_triple = "aarch64-apple-darwin".into());
        reject(|row| row.sk_v8.host_triple = "aarch64-apple-darwin;arch=;cpu=".into());
        reject(|row| row.sk_v8.feature_mask = "arch=aarch64;simd=Scalar".into());
        reject(|row| row.sk_v8.feature_mask = "arch=;os=;simd=;target_cpu=native".into());
        reject(|row| row.sk_v8.substrate_surface = "side_substrate".into());
    }

    #[test]
    fn skv13_decision_regex_report_accepts_measured_block() {
        let report = SkV13DecisionRegexReport {
            schema_version: SKV13_DECISION_REGEX_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W5".into(),
            run_id: "sk-v13-w5:regex-facts-fnv64-0000000000000000".into(),
            regex_fact_source: "bbnf-regex::analyze".into(),
            regex_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w5/regex-facts.json".into(),
            regex_fact_sha256: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                .into(),
            regex_fact_consumer_path: vec![
                "ir::nullability".into(),
                "passes::recognizers".into(),
                "passes::extract".into(),
            ],
            generated_selection_path: "passes::recognizers::derive_backend_shape_with_diagnostics"
                .into(),
            hardcoded_regex_scan_status: "no-hardcoded-json-patterns".into(),
            feature_gate_status: "pass".into(),
            cascade_fallback_status: "fail-closed".into(),
            row_move_toward_sota_status: "measured_architectural_block".into(),
            block_id: Some("JSON-W5-REGEX-FACTS-NOT-CONSUMED-BY-GENERATED-DISPATCH".into()),
            material_differential: "REDRESS 119/120 did not extract grammar-neutral regex facts"
                .into(),
            redress_entry: "REDRESS-136".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut bad = report.clone();
        bad.row_move_toward_sota_status = "support_only".into();
        assert!(bad.validate_gate().is_err());
    }

    #[test]
    fn skv13_decision_active_cost_report_accepts_measured_block() {
        let report = SkV13DecisionActiveCostReport {
            schema_version: SKV13_DECISION_ACTIVE_COST_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W6".into(),
            run_id: "sk-v13-w6:active-cost-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W6-DECISION-ACTIVE-COST".into(),
            g_omega_status: "user-signed".into(),
            regex_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w5/regex-facts.json".into(),
            regex_fact_sha256: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                .into(),
            egraph_language_status: "pass".into(),
            rewrite_set_id: "sk-v13-w6-conservative-shape-v1".into(),
            egraph_node_count: 5,
            egraph_eclass_count: 1,
            egraph_iteration_count: 1,
            egraph_memory_peak_bytes: 1024,
            egraph_budget_status: "pass".into(),
            cost_function_source: "passes::backend_egraph::DecisionCostModel".into(),
            cost_formula_version: "sk-v13-w6-integer-v1".into(),
            candidate_total_count: 5,
            candidate_hard_pruned_count: 4,
            candidate_ranked_count: 1,
            candidate_stale_count: 0,
            candidate_cost_stale_rate: 0.0,
            selected_candidate_id: "rule-0-shape-OffsetTape-priority-P7OffsetTapeDefault".into(),
            selected_rule_id: "0".into(),
            selected_shape: "OffsetTape".into(),
            selected_cost_freshness: "fresh".into(),
            capacity_policy_cost_status: "pass".into(),
            determinism_replay_status: "pass".into(),
            rewrite_order_replay_count: 2,
            rewrite_order_variance_pct: 0.0,
            selection_trace_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            cost_facts_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w6/active-cost-facts.json".into(),
            cost_facts_sha256: "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
                .into(),
            generated_selection_path: "passes::recognizers::derive_backend_shape_with_diagnostics"
                .into(),
            same_wave_consumer_path: "codegen::lower::rust::lower_to_rust".into(),
            same_wave_consumer_class: "gate_json_decision_active_cost_contract".into(),
            row_move_toward_sota_status: "measured_architectural_block".into(),
            block_id: Some(
                "JSON-CSS-W6-EGRAPH-COST-CANDIDATE-NOT-CONSUMED-BY-GENERATED-RUNTIME".into(),
            ),
            cascade_fallback_status: "fail-closed".into(),
            abrogate_status: "not-triggered".into(),
            material_differential: "REDRESS 87 was passive cost evidence only".into(),
            redress_entry: "REDRESS-137".into(),
            csp_solve_ms: "n/a:w6-before-csp".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut bad = report.clone();
        bad.row_move_toward_sota_status = "support_only".into();
        assert!(bad.validate_gate().is_err());
        let mut stale = report;
        stale.candidate_ranked_count = 10;
        stale.candidate_stale_count = 4;
        stale.candidate_cost_stale_rate = 0.4;
        assert!(stale.validate_gate().is_err());
    }

    #[test]
    fn skv13_decision_csp_cascade_report_accepts_measured_block() {
        let report = SkV13DecisionCspCascadeReport {
            schema_version: SKV13_DECISION_CSP_CASCADE_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W7".into(),
            run_id: "sk-v13-w7:csp-cascade-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W7-DECISION-CSP-CASCADE".into(),
            g_omega_status: "user-signed".into(),
            regex_fact_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w5/regex-facts.json".into(),
            regex_fact_sha256: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                .into(),
            active_cost_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w6/active-cost-facts.json".into(),
            active_cost_sha256: "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
                .into(),
            selection_trace_sha256:
                "1111111111111111111111111111111111111111111111111111111111111111".into(),
            csp_problem_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w7/csp-problem.json".into(),
            csp_problem_sha256: "2222222222222222222222222222222222222222222222222222222222222222"
                .into(),
            csp_solution_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w7/csp-solution.json".into(),
            csp_solution_sha256: "3333333333333333333333333333333333333333333333333333333333333333"
                .into(),
            css_l4_witness_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w7/css-l4-witness.json".into(),
            css_l4_witness_sha256:
                "4444444444444444444444444444444444444444444444444444444444444444".into(),
            css_l4_witness_command: "cargo test -p codegen css_l4".into(),
            sheets_witness_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w7/sheets-witness.json".into(),
            sheets_witness_sha256:
                "5555555555555555555555555555555555555555555555555555555555555555".into(),
            sheets_witness_command: "cargo test -p codegen sheets".into(),
            bbnf_self_witness_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w7/bbnf-self-witness.json".into(),
            bbnf_self_witness_sha256:
                "6666666666666666666666666666666666666666666666666666666666666666".into(),
            bbnf_self_witness_command: "cargo test -p codegen bbnf_self".into(),
            scoped_witness_label: "css-l4+sheets+bbnf-self fail-closed generated-role".into(),
            csp_solver_source: "csp_solver::Csp<CostFiniteDomain>".into(),
            csp_solver_version: "0.1.0".into(),
            csp_status: "sat".into(),
            csp_variable_count: 1,
            csp_constraint_count: 6,
            csp_objective_count: 1,
            csp_named_grammars: vec!["json".into(), "css_l4".into()],
            csp_solve_ms: 0.2,
            csp_timeout_ms: 1_000,
            csp_node_budget: 10_000,
            csp_nodes_explored: 1,
            csp_budget_status: "pass".into(),
            selected_rule_count: 1,
            selected_candidate_id: "rule-0-shape-OffsetTape-priority-P7OffsetTapeDefault".into(),
            selected_shape: "OffsetTape".into(),
            parity_constraint_status: "pass".into(),
            recognizer_constraint_status: "pass".into(),
            substrate_constraint_status: "pass".into(),
            simd_constraint_status: "pass".into(),
            capacity_constraint_status: "pass".into(),
            resolver_output_piping: "regex_facts->egraph_active_cost->csp->compile_codegen".into(),
            fused_solver_status: "not-fused".into(),
            generated_selection_path: "passes::recognizers::derive_backend_shape_with_diagnostics"
                .into(),
            compile_consumer_path: "passes::compile".into(),
            same_wave_consumer_path: "codegen::lower::rust::lower_to_rust".into(),
            same_wave_consumer_class: "gate_json_decision_csp_cascade_contract".into(),
            cascade_retirement_status: "fail_closed".into(),
            choose_backend_shape_status: "csp-finalized".into(),
            priority_table_status: "evidence-only".into(),
            p1_p8_fallback_status: "non-admission".into(),
            legacy_cascade_admission_status: "blocked".into(),
            priority_data_role: "evidence-only".into(),
            priority_hard_prune_status: "not-used".into(),
            priority_objective_status: "not-used".into(),
            fallback_invoked: false,
            compat_fallback_status: "not-invoked".into(),
            static_css_provider_status: "static-template-blocker".into(),
            json_sink_only_status: "sink-only-static-blocker".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            sheets_fail_closed_status: "fail-closed-artifact".into(),
            bbnf_self_fail_closed_status: "fail-closed-artifact".into(),
            lock14_status: "pass".into(),
            generated_runtime_diff_status: "absent".into(),
            generated_runtime_diff_artifact_path: "".into(),
            generated_runtime_diff_sha256: "".into(),
            row_move_toward_sota_status: "measured_architectural_block".into(),
            affected_row_ids: vec!["json/twitter/direct_to_struct/main".into()],
            block_id: Some("JSON-CSS-W7-CSP-CASCADE-CONSUMED-BUT-NO-ROW-MOVEMENT".into()),
            abrogate_status: "not-triggered".into(),
            material_differential: "REDRESS 119/120 had no CSP resolver".into(),
            redress_entry: "REDRESS-138".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut bad = report.clone();
        bad.fallback_invoked = true;
        assert!(bad.validate_gate().is_err());
        let mut support_only = report.clone();
        support_only.row_move_toward_sota_status = "support_only".into();
        assert!(support_only.validate_gate().is_err());
        let mut no_command = report;
        no_command.css_l4_witness_command = "status-only".into();
        assert!(no_command.validate_gate().is_err());
    }

    #[test]
    fn skv13_per_grammar_policy_report_accepts_measured_block() {
        let report = SkV13PerGrammarPolicyReport {
            schema_version: SKV13_PER_GRAMMAR_POLICY_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W8".into(),
            run_id: "sk-v13-w8:per-grammar-policy-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W8-PER-GRAMMAR-POLICY".into(),
            g_omega_status: "user-signed".into(),
            json_consumer_row_id: "json/y_string_unicode/direct_to_struct/main".into(),
            json_consumer_path: "runtime::grammars::json::parse_direct".into(),
            css_consumer_row_id: "css_l4/declaration_values_extended/direct_to_struct/main".into(),
            css_consumer_path:
                "runtime::generated_css_l4_declaration_values_extended::generated::emit_fact_stream"
                    .into(),
            same_wave_consumer_class: "generated_json_and_css_policy_rows".into(),
            generic_storage_status: "stable".into(),
            public_grammar_config_status: "absent".into(),
            generic_json_sink_acceleration_status: "absent".into(),
            generic_json_policy_token_status: "absent".into(),
            json_flag_semantics_owner: "generated_json_config".into(),
            json_flag_physical_bit_status: "preserved".into(),
            css_policy_owner: "generated_css_config".into(),
            css_policy_consumer_status: "generated_scanner_and_sink".into(),
            json_strict_equality_status: "pass".into(),
            css_strict_equality_status: "pass".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            json_row_mbps_before: 1983.0,
            json_row_mbps_after: 1983.0,
            css_row_mbps_before: 429.34,
            css_row_mbps_after: 429.34,
            row_move_toward_sota_status: "measured_architectural_block".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            policy_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w8/policy-surface-facts.json".into(),
            policy_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec![
                "json/y_string_unicode/direct_to_struct/main".into(),
                "css_l4/declaration_values_extended/direct_to_struct/main".into(),
            ],
            block_id: Some("JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT".into()),
            material_differential: "REDRESS 121 was public GrammarConfig prose-only".into(),
            redress_entry: "REDRESS-139".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut support_only = report.clone();
        support_only.row_move_toward_sota_status = "support_only".into();
        assert!(support_only.validate_gate().is_err());
        let mut generic_policy = report.clone();
        generic_policy.generic_json_policy_token_status = "present".into();
        assert!(generic_policy.validate_gate().is_err());
        let mut wrong_block = report;
        wrong_block.block_id = Some("support-only".into());
        assert!(wrong_block.validate_gate().is_err());
    }

    #[test]
    fn skv13_same_substrate_union_report_accepts_measured_admit() {
        let report = SkV13SameSubstrateUnionReport {
            schema_version: SKV13_SAME_SUBSTRATE_UNION_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W9".into(),
            run_id: "sk-v13-w9:same-substrate-union-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W9-SAME-SUBSTRATE-UNION".into(),
            g_omega_status: "user-signed".into(),
            union_variant_id: "union-c1-per-rule-same-tape".into(),
            material_differential_status: "accepted".into(),
            prior_redress_citations: vec!["96".into(), "97".into(), "98".into()],
            substrate_cardinality: "one".into(),
            public_union_tape_status: "absent".into(),
            public_substrate_api_status: "absent".into(),
            backend_shape_expansion_status: "absent".into(),
            bir_directive_expansion_status: "absent".into(),
            class_column_status: "absent".into(),
            retained_structural_index_status: "absent".into(),
            sidecar_vector_status: "absent".into(),
            second_scan_status: "absent".into(),
            parser_owned_cursor_status: "absent".into(),
            bbnf_simd_touch_status: "read-only".into(),
            css_consumer_row_id: "css_l4/declaration_values_extended/direct_to_struct/main".into(),
            css_consumer_path:
                "runtime::generated_css_l4_declaration_values_extended::sink::FactSink::token"
                    .into(),
            same_wave_consumer_class:
                "generated_css_decl_values_extended_same_substrate_projection".into(),
            css_strict_equality_status: "pass".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            css_row_mbps_before: 265.657,
            css_row_mbps_after: 269.543,
            lightningcss_mbps: 132.141,
            threshold_mbps: 133.141,
            row_move_toward_sota_status: "admitted".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            union_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w9/same-substrate-union-facts.json"
                    .into(),
            union_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec![
                "css_l4/declaration_values_extended/direct_to_struct/main".into()
            ],
            block_id: None,
            material_differential: "REDRESS 96/97/98 used parse-plane side structures".into(),
            redress_entry: "REDRESS-140".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut no_move = report.clone();
        no_move.css_row_mbps_after = no_move.css_row_mbps_before;
        assert!(no_move.validate_gate().is_err());
        let mut support_only = report.clone();
        support_only.row_move_toward_sota_status = "support_only".into();
        assert!(support_only.validate_gate().is_err());
        let mut public_substrate = report;
        public_substrate.public_substrate_api_status = "present".into();
        assert!(public_substrate.validate_gate().is_err());
    }

    #[test]
    fn skv13_json_direct_reopen_report_accepts_numbers_admit() {
        let report = SkV13JsonDirectReopenReport {
            schema_version: SKV13_JSON_DIRECT_REOPEN_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W11.1".into(),
            run_id: "sk-v13-w11.1:numbers-direct-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W11.1-JSON-DIRECT-NUMBERS".into(),
            g_omega_status: "user-signed".into(),
            row_id: "json/numbers/direct_to_struct/main".into(),
            corpus: "numbers".into(),
            workload: "direct_to_struct".into(),
            output_plane: "digest".into(),
            route_id: "generated-json-direct-numeric-array-dispatch".into(),
            same_wave_consumer_path: "runtime::generated_json::generated::parse_array_direct"
                .into(),
            same_wave_consumer_class: "generated_json_direct_numeric_array_dispatch".into(),
            strict_equality_status: "pass".into(),
            track2_independence_status: "independent".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            track1_mbps_before: 12545.081,
            track1_mbps_after: 13798.591,
            track2_mbps_after: 12460.914,
            sonic_strict_mbps_after: 12937.655,
            serde_mbps_after: 8133.978,
            threshold_mbps: 12938.655,
            row_move_toward_sota_status: "admitted".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            measurement_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w11.1/numbers-direct-facts.json".into(),
            measurement_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec!["json/numbers/direct_to_struct/main".into()],
            block_id: None,
            prior_redress_citations: vec!["119".into(), "120".into()],
            material_differential:
                "REDRESS 119/120 did not remove generated array numeric redispatch".into(),
            redress_entry: "REDRESS-141".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut support_only = report.clone();
        support_only.row_move_toward_sota_status = "support_only".into();
        assert!(support_only.validate_gate().is_err());
        let mut below_sota = report.clone();
        below_sota.track1_mbps_after = below_sota.threshold_mbps;
        assert!(below_sota.validate_gate().is_err());
        let mut bad_threshold = report;
        bad_threshold.threshold_mbps += 0.5;
        assert!(bad_threshold.validate_gate().is_err());
    }

    #[test]
    fn skv13_json_direct_reopen_report_accepts_mesh_admit() {
        let report = SkV13JsonDirectReopenReport {
            schema_version: SKV13_JSON_DIRECT_REOPEN_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W11.3".into(),
            run_id: "sk-v13-w11.3:direct-sink-stack-fnv64-0000000000000000".into(),
            source_commit: "000000000000".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W11.3-JSON-DIRECT-SINK-STACK".into(),
            g_omega_status: "user-signed".into(),
            row_id: "json/mesh/direct_to_struct/main".into(),
            corpus: "mesh".into(),
            workload: "direct_to_struct".into(),
            output_plane: "digest".into(),
            route_id: "direct-sink-stack-specialization".into(),
            same_wave_consumer_path: "bbnf_bench::direct_struct::JsonDirectSink".into(),
            same_wave_consumer_class: "direct_sink_stack_specialization".into(),
            strict_equality_status: "pass".into(),
            track2_independence_status: "independent".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            track1_mbps_before: 8703.0,
            track1_mbps_after: 9657.892,
            track2_mbps_after: 6959.985,
            sonic_strict_mbps_after: 9569.599,
            serde_mbps_after: 7011.870,
            threshold_mbps: 9570.599,
            row_move_toward_sota_status: "admitted".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            measurement_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w11.3/direct-sink-stack-facts.json"
                    .into(),
            measurement_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec!["json/mesh/direct_to_struct/main".into()],
            block_id: None,
            prior_redress_citations: vec!["119".into(), "120".into(), "142".into()],
            material_differential:
                "REDRESS 119/120/142 did not specialize the direct sink stack parent access".into(),
            redress_entry: "REDRESS-143".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut bad_wave = report.clone();
        bad_wave.wave_id = "SK-V13-W11.2".into();
        assert!(bad_wave.validate_gate().is_err());
        let mut missing_citation = report;
        missing_citation
            .prior_redress_citations
            .retain(|entry| entry != "142");
        assert!(missing_citation.validate_gate().is_err());
    }

    #[test]
    fn skv14_json_parse_only_report_accepts_numbers_admit() {
        let report = SkV13JsonParseOnlyReport {
            schema_version: SKV14_JSON_PARSE_ONLY_REPORT_SCHEMA.into(),
            wave_id: "SK-V14-W10".into(),
            run_id: "SK-V14-W10:numbers-parse-fnv64-0000000000000000".into(),
            source_commit: "196a3f2a4".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-W10-JSON-PARSE-ONLY".into(),
            g_omega_status: "not-applicable:wave-implementation".into(),
            row_id: "json/numbers/parse_only/main".into(),
            corpus: "numbers".into(),
            workload: "parse_only".into(),
            output_plane: "parse_only".into(),
            route_id: "generated-json-parse-only-distinct-path".into(),
            same_wave_consumer_path: "runtime::generated_json::parse_only".into(),
            same_wave_consumer_class: "generated_json_parse_only_contract".into(),
            strict_equality_status: "pass".into(),
            strict_equality_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w14.1/numbers-parse-facts.json"
                    .into(),
            strict_equality_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            track2_independence_status: "independent".into(),
            measured_validation_path: "measured-row".into(),
            parse_utf8: "measured-row".into(),
            escape_complete: "yes".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            track1_mbps_before: 19110.0,
            track1_mbps_after: 19110.0,
            track2_mbps_after: 18277.0,
            sonic_strict_mbps_after: 13335.0,
            serde_mbps_after: 6290.0,
            threshold_mbps: 13336.0,
            admission_margin_mbps: 5774.0,
            row_move_toward_sota_status: "admitted".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            measurement_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w14.1/numbers-parse-facts.json"
                    .into(),
            measurement_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec!["json/numbers/parse_only/main".into()],
            block_id: None,
            prior_redress_citations: vec!["102".into()],
            material_differential:
                "REDRESS 102 classified parse-only as view-boundary; W10 admits distinct parse_only evidence"
                    .into(),
            redress_entry: "none:SK-V14-W10-admit".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut borrowed_view = report.clone();
        borrowed_view.output_plane = "borrowed view over offset tape vs DOM".into();
        assert!(borrowed_view.validate_gate().is_err());
        let mut support_only = report.clone();
        support_only.row_move_toward_sota_status = "support_only".into();
        assert!(support_only.validate_gate().is_err());
        let mut bad_margin = report.clone();
        bad_margin.admission_margin_mbps += 1.0;
        assert!(bad_margin.validate_gate().is_err());
        let mut missing_citation = report;
        missing_citation.prior_redress_citations.clear();
        assert!(missing_citation.validate_gate().is_err());
    }

    #[test]
    fn skv14_json_parse_only_report_accepts_configured_corpus_admit() {
        let report = SkV13JsonParseOnlyReport {
            schema_version: SKV14_JSON_PARSE_ONLY_REPORT_SCHEMA.into(),
            wave_id: "SK-V14-W10V".into(),
            run_id: "SK-V14-W10V:citm-catalog-current-head-fnv64-0000000000000000".into(),
            source_commit: "209fb0363".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native".into(),
            consumer_gate: "G-SK-V14-W10V-JSON-PARSE-ONLY-CURRENT-HEAD-RESWEEP".into(),
            g_omega_status: "not-applicable:wave-implementation".into(),
            row_id: "json/citm_catalog/parse_only/main".into(),
            corpus: "citm_catalog".into(),
            workload: "parse_only".into(),
            output_plane: "parse_only".into(),
            route_id: "generated-json-parse-only-current-head-resweep".into(),
            same_wave_consumer_path: "runtime::generated_json::parse_only".into(),
            same_wave_consumer_class: "generated_json_parse_only_contract".into(),
            strict_equality_status: "pass".into(),
            strict_equality_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w14.2/citm-catalog-parse-facts.json"
                    .into(),
            strict_equality_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            track2_independence_status: "independent".into(),
            measured_validation_path: "measured-row".into(),
            parse_utf8: "measured-row".into(),
            escape_complete: "yes".into(),
            json_guard_state: "maintain".into(),
            css_guard_state: "maintain".into(),
            track1_mbps_before: 8037.394,
            track1_mbps_after: 9079.838,
            track2_mbps_after: 13566.569,
            sonic_strict_mbps_after: 8335.772,
            serde_mbps_after: 5121.472,
            threshold_mbps: 8336.772,
            admission_margin_mbps: 743.066,
            row_move_toward_sota_status: "admitted".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            measurement_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w14.2/citm-catalog-parse-facts.json"
                    .into(),
            measurement_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            affected_row_ids: vec!["json/citm_catalog/parse_only/main".into()],
            block_id: None,
            prior_redress_citations: vec!["221".into()],
            material_differential:
                "REDRESS 221 abrogated the number-end scanner; W10V admits current-HEAD parse_only evidence without a source patch"
                    .into(),
            redress_entry: "none:SK-V14-W10V-admit".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut wrong_row = report.clone();
        wrong_row.row_id = "json/canada/parse_only/main".into();
        assert!(wrong_row.validate_gate().is_err());
        let mut wrong_gate = report;
        wrong_gate.consumer_gate = "G-W14.1-JSON-PARSE-NUMBERS".into();
        assert!(wrong_gate.validate_gate().is_err());
    }

    #[test]
    fn skv13_simd_asm_production_report_accepts_css_delimiter_admit() {
        let report = SkV13SimdAsmProductionReport {
            schema_version: SKV13_SIMD_ASM_PRODUCTION_REPORT_SCHEMA.into(),
            wave_id: "SK-V13-W12".into(),
            run_id: "sk-v13-w12:css-delimiter-simd-fnv64-0000000000000000".into(),
            source_commit: "20ff525da+w12-redress".into(),
            host_triple: "aarch64-apple-darwin".into(),
            build_flags: "RUSTFLAGS=-C target-cpu=native".into(),
            feature_mask: "arch=aarch64;target_cpu=native;simd=neon".into(),
            consumer_gate: "G-W12-SIMD-ASM-PRODUCTION".into(),
            g_omega_status: "user-signed".into(),
            route_id: "css-delimiter-ascii-set-member64".into(),
            selected_primitive: "bbnf_simd::find_ascii_set_member64".into(),
            primitive_source_paths: vec![
                "crates/bbnf-simd/src/lib.rs".into(),
                "crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs".into(),
            ],
            scalar_reference_status: "pass".into(),
            checkasm_status: "pass".into(),
            checkasm_command:
                "BBNF_SIMD_STRICT=1 RUSTFLAGS=\"-C target-cpu=native\" cargo run -p xtask --release -- primitive-checkasm"
                    .into(),
            checkasm_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w12/simd-production-facts.json"
                    .into(),
            checkasm_artifact_sha256:
                "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef".into(),
            corpus_parity_status: "pass".into(),
            consumer_row_id: "css_l4/declaration_values/direct_to_struct/main".into(),
            consumer_runtime_path:
                "runtime::generated_css_l4_declaration_values::generated::Scanner::scan_block"
                    .into(),
            consumer_bench_path: "bbnf-bench::nonjson_css_l4".into(),
            same_wave_consumer_class: "generated_css_l4_declaration_values_scan_block".into(),
            production_consumer_status: "wired".into(),
            track1_mbps_before: 434.131,
            track1_mbps_after: 444.208,
            lightningcss_mbps: 168.235,
            threshold_mbps: 169.235,
            criterion_delta_pct: 109.87,
            row_move_toward_sota_status: "admitted".into(),
            measurement_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w12/simd-production-facts.json"
                    .into(),
            measurement_artifact_sha256:
                "fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210"
                    .into(),
            orphan_count_before: 0,
            orphan_count_after: 0,
            orphan_inventory_artifact_path:
                "../restart/skinny/tranches/sk-v13/research/w12/orphan-inventory.json".into(),
            orphan_inventory_sha256:
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".into(),
            deleted_or_demoted_primitives: Vec::new(),
            json_guard_state: "maintain".into(),
            css_guard_state: "strict-equality-pass".into(),
            lock14_status: "pass".into(),
            lock14_owner_path_status: "pass".into(),
            lock14_generic_scan_status: "pass".into(),
            prior_redress_citations: vec![
                "88".into(),
                "89".into(),
                "90".into(),
                "122".into(),
                "126".into(),
            ],
            affected_row_ids: vec!["css_l4/declaration_values/direct_to_struct/main".into()],
            block_id: None,
            material_differential:
                "REDRESS 126 demoted primitives without a production CSS delimiter consumer".into(),
            redress_entry: "REDRESS-144".into(),
        };
        assert!(report.validate_gate().is_ok());
        let mut support_only = report.clone();
        support_only.production_consumer_status = "future_consumer".into();
        assert!(support_only.validate_gate().is_err());
        let mut orphan = report.clone();
        orphan.orphan_count_after = 1;
        assert!(orphan.validate_gate().is_err());
        let mut missing_citation = report;
        missing_citation
            .prior_redress_citations
            .retain(|entry| entry != "126");
        assert!(missing_citation.validate_gate().is_err());
    }
}
