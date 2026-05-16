use crate::gate::{Outcome, Verdict};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::Path;

pub const SCHEMA_V3_HEADER: &str = "| Corpus | Workload | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Output plane | Track 1 Mbps | Track 2 Mbps | sonic-rs strict Mbps | sonic-rs lossy Mbps | simdjson DOM Mbps | simdjson On Demand Mbps | yyjson default Mbps | asmjson SWAR Mbps | asmjson AVX-512 Mbps | RapidJSON default Mbps | serde_json Mbps | Δ vs SK-V6 | Δ vs sonic-strict | Δ vs simdjson DOM | Δ vs yyjson | Hot leaf | Signal |";
const SCHEMA_V3_ALIGN: &str = "|---|---|---:|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---|---|";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Report {
    pub title: String,
    pub rows: Vec<TelemetryRow>,
    pub probe_rows: Vec<ProbeReportRow>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
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
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProbeReportRow {
    pub corpus: String,
    pub probe: String,
    pub mbps: Option<f64>,
    pub ns_per_iter: Option<f64>,
    pub vs_track1: Option<f64>,
    pub signal: String,
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
        Self {
            corpus: corpus.into(),
            workload: workload.into(),
            outcome_id,
            verdict,
            strictness: strictness.into(),
            parse_utf8: parse_utf8.into(),
            escape_complete: escape_complete.into(),
            flaw_probe: flaw_probe.into(),
            output_plane: output_plane.into(),
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
}
