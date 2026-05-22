use crate::gate::{self, Outcome, Verdict};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::io;
use std::path::Path;

pub const SCHEMA_V3_HEADER: &str = "| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Δ vs SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM | Δ vs yyjson | Hot leaf | Signal |";
const SCHEMA_V3_ALIGN: &str = "|---|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---|---|";

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
pub type NonJsonEvidenceRow = TelemetryRow;
pub type NonJsonOracleEvidence = SkV8ComparatorEvidence;

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
            "borrowed view over offset tape vs DOM",
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
        if telemetry.wave_id != "SK-V9-open" || telemetry.sk_v9_open_delta != "baseline" {
            return Err(format!(
                "{} is not marked as SK-V9-open baseline",
                telemetry.row_id
            ));
        }
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
                "{} has invalid SK-V9-open run_id {}",
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
            wave_id: "SK-V9-open".to_string(),
            run_id: "test-run".to_string(),
            sk_v9_open_delta: "baseline".to_string(),
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
        for row in &self.rows {
            let row_id = row.sk_v8.row_id.as_str();
            if !seen.insert(row_id) {
                return Err(format!("duplicate SK-V9 W0 row_id {row_id}"));
            }
            if row_id == W6_GITHUB_EVENTS_TYPED_ROW_ID {
                validate_w6_github_events_typed_row(row)?;
                w6_github_events_typed_seen = true;
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
            match &run_id {
                Some(expected) if expected != &row.sk_v8.run_id => {
                    return Err(format!(
                        "{row_id} run_id {} differs from report run_id {}",
                        row.sk_v8.run_id, expected
                    ));
                }
                Some(_) => {}
                None => run_id = Some(row.sk_v8.run_id.clone()),
            }
        }
        let expected_rows = SK_V8_OPEN_BASELINE.len() + usize::from(w6_github_events_typed_seen);
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
        Ok(())
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
            out.push_str("\n## SK-V9 W0 Telemetry Manifest\n\n");
            out.push_str("| Row id | Grammar | Domain | Wave | Run id | Validation | Profile artifact | Sample cost | Sample count | Build flags | Host triple | Feature mask | CostFacts | Redress | SK-V9-open delta | Substrate | Structural projection | Cardinality | Consumer | Track 2 | Diagnostic nonproducer | Comparator evidence |\n");
            out.push_str("|---|---|---|---|---|---|---|---|---:|---|---|---|---|---|---|---|---|---|---|---|---|---|\n");
            for row in &self.rows {
                let telemetry = &row.sk_v8;
                out.push_str(&format!(
                    "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
                    cell(&telemetry.row_id),
                    cell(&telemetry.grammar_id),
                    cell(&telemetry.domain),
                    cell(&telemetry.wave_id),
                    cell(&telemetry.run_id),
                    cell(&telemetry.measured_validation_path),
                    cell(&telemetry.profile_artifact),
                    cell(&telemetry.sample_cost),
                    telemetry.sample_count,
                    cell(&telemetry.build_flags),
                    cell(&telemetry.host_triple),
                    cell(&telemetry.feature_mask),
                    cell(&format!(
                        "{}:{}:{}",
                        telemetry.costfacts_rule_id,
                        telemetry.costfacts_chosen_shape,
                        telemetry.costfacts_rejected_alternative_ids.join(",")
                    )),
                    cell(&telemetry.redress_entry),
                    cell(&telemetry.sk_v9_open_delta),
                    cell(&telemetry.substrate_surface),
                    cell(&telemetry.structural_projection_status),
                    cell(&telemetry.substrate_cardinality),
                    cell(&telemetry.same_wave_consumer_class),
                    cell(&telemetry.track2_independence_status),
                    cell(&telemetry.diagnostic_nonproducer_status),
                    cell(&format_comparator_evidence(&telemetry.comparators))
                ));
            }
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

pub struct SkV8OpenBaseline {
    pub row_id: &'static str,
    pub outcome_id: &'static str,
    pub verdict: &'static str,
    pub track1_mbps: f64,
    pub track2_mbps: f64,
}

pub const SK_V9_OPEN_RUN_ID_PREFIX: &str = "sk-v9-open:criterion-fnv64-";

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
    if track1 < floor || track2 < floor {
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

fn validate_w0_profile_artifact(row_id: &str, profile_artifact: &str) -> Result<(), String> {
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
    for required in [
        "profile=bench",
        "rustflags=-C target-cpu=native",
        "target_cpu=native",
    ] {
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
        "parse_only" => (
            "borrowed_view_over_offset_tape",
            "discarded_after_capacity",
            "one",
        ),
        "direct_to_struct" => ("sink_only_digest", "n/a", "zero_or_inert"),
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
            "absence:w0:{corpus}:{workload}:{}",
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
        ("sonic_rs_strict", "parse_only") => ("sonic_rs_anchor", "DOM"),
        ("sonic_rs_strict", "direct_to_struct") => ("sonic_rs_direct_to_struct", "digest"),
        ("sonic_rs_strict", "real_typed_struct") => ("sonic_rs_real_typed_struct", "typed direct"),
        ("serde_json", "parse_only") => ("serde_json", "DOM"),
        ("serde_json", "direct_to_struct") => ("serde_json_direct_to_struct", "digest"),
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

    const TEST_SK_V9_OPEN_RUN_ID: &str = "sk-v9-open:criterion-fnv64-0123456789abcdef";

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
            "parse_only" => "sonic_rs_anchor",
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
            "parse_only" => "DOM",
            "direct_to_struct" => "digest",
            "real_typed_struct" => "typed direct",
            _ => unreachable!(),
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
                comparator_plane: native_plane.into(),
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
                source_artifact: format!("absence:w0:{corpus}:{workload}:{id}"),
            });
        }
        evidence
    }

    fn w0_telemetry(row_id: &str, _output_plane: &str) -> SkV8Telemetry {
        let (_, workload) = parse_row_id(row_id).unwrap();
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
            wave_id: "SK-V9-open".into(),
            run_id: TEST_SK_V9_OPEN_RUN_ID.into(),
            sk_v9_open_delta: "baseline".into(),
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
                "borrowed view over offset tape vs DOM"
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
        assert!(markdown.contains("## SK-V9 W0 Telemetry Manifest"));
        assert!(markdown.contains("json/twitter/parse_only/main"));
        assert!(markdown.contains("none:pre-W1"));
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
        sonic.source_artifact = "criterion:json_twitter/sonic_rs_anchor/new/estimates.json".into();
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
            "absence:w0:wrong:parse_only:simdjson_dom".into();
        assert!(row.validate_sk_v8_w0().is_err());

        row.sk_v8.comparators[sidecar_idx].source_artifact =
            "absence:w0:twitter:parse_only:simdjson_dom".into();
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
        assert!(report.validate_sk_v8_w0().is_ok());
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
                "criterion:json_twitter/sonic_rs_anchor/new/estimates.json".into();
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
    fn w6_typed_contract_accepts_complete_github_events_row() {
        let mut report = opening_report();
        report
            .rows
            .push(w6_github_events_typed_row(12_000.0, 12_000.0));
        assert!(report.validate_sk_v8_w0().is_ok());
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
        assert!(report.validate_sk_v8_w0().is_ok());
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
}
