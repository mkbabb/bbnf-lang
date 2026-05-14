use crate::gate::{Outcome, Verdict};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Report {
    pub title: String,
    pub rows: Vec<ReportRow>,
    pub workload_rows: Vec<WorkloadReportRow>,
    pub probe_rows: Vec<ProbeReportRow>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ReportRow {
    pub corpus: String,
    pub outcome_id: String,
    pub verdict: String,
    pub bytes: u64,
    pub track1_mbps: Option<f64>,
    pub track2_mbps: Option<f64>,
    pub sonic_mbps: Option<f64>,
    pub simd_json_borrowed_mbps: Option<f64>,
    pub simd_json_owned_mbps: Option<f64>,
    pub fastest_anchor: Option<String>,
    pub fastest_anchor_mbps: Option<f64>,
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct WorkloadReportRow {
    pub corpus: String,
    pub workload: String,
    pub track1_mbps: Option<f64>,
    pub track2_mbps: Option<f64>,
    pub sonic_mbps: Option<f64>,
    pub serde_json_mbps: Option<f64>,
    pub track1_vs_sonic: Option<f64>,
    pub track2_vs_sonic: Option<f64>,
    pub correctness: String,
}

impl Report {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            rows: Vec::new(),
            workload_rows: Vec::new(),
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
        sonic_ns: Option<f64>,
        simd_json_borrowed_ns: Option<f64>,
        simd_json_owned_ns: Option<f64>,
        fastest_anchor: Option<impl Into<String>>,
        fastest_anchor_ns: Option<f64>,
    ) {
        self.rows.push(ReportRow {
            corpus: corpus.into(),
            outcome_id: outcome.id().to_string(),
            verdict: verdict_label(outcome.verdict()).to_string(),
            bytes,
            track1_mbps: throughput_mbps(bytes, track1_ns),
            track2_mbps: throughput_mbps(bytes, track2_ns),
            sonic_mbps: throughput_mbps(bytes, sonic_ns),
            simd_json_borrowed_mbps: throughput_mbps(bytes, simd_json_borrowed_ns),
            simd_json_owned_mbps: throughput_mbps(bytes, simd_json_owned_ns),
            fastest_anchor: fastest_anchor.map(Into::into),
            fastest_anchor_mbps: throughput_mbps(bytes, fastest_anchor_ns),
        });
    }

    pub fn push_workload_row(
        &mut self,
        corpus: impl Into<String>,
        workload: impl Into<String>,
        bytes: u64,
        track1_ns: Option<f64>,
        track2_ns: Option<f64>,
        sonic_ns: Option<f64>,
        serde_json_ns: Option<f64>,
        correctness: impl Into<String>,
    ) {
        self.workload_rows.push(WorkloadReportRow {
            corpus: corpus.into(),
            workload: workload.into(),
            track1_mbps: throughput_mbps(bytes, track1_ns),
            track2_mbps: throughput_mbps(bytes, track2_ns),
            sonic_mbps: throughput_mbps(bytes, sonic_ns),
            serde_json_mbps: throughput_mbps(bytes, serde_json_ns),
            track1_vs_sonic: speed_ratio(track1_ns, sonic_ns),
            track2_vs_sonic: speed_ratio(track2_ns, sonic_ns),
            correctness: correctness.into(),
        });
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

    pub fn render_markdown(&self) -> String {
        let mut out = String::new();
        out.push_str("# ");
        out.push_str(&self.title);
        out.push_str("\n\n");
        out.push_str("| Corpus | Outcome | Verdict | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | simd-json borrowed Mbps | simd-json owned Mbps | S anchor | S Mbps | Track 1 / S | Track 2 / S |\n");
        out.push_str(
            "|---|---:|---|---|---|---|---|---:|---:|---:|---:|---:|---|---:|---:|---:|\n",
        );
        for row in &self.rows {
            out.push_str(&format!(
                "| {} | {} | {} | deferred | view-boundary | yes | JSONTestSuite n_string_unescaped_ctrl_char rejected; i_string_invalid_utf8 rejected outside hot scan | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
                row.corpus,
                row.outcome_id,
                row.verdict,
                format_optional(row.track1_mbps),
                format_optional(row.track2_mbps),
                format_optional(row.sonic_mbps),
                format_optional(row.simd_json_borrowed_mbps),
                format_optional(row.simd_json_owned_mbps),
                row.fastest_anchor.as_deref().unwrap_or("n/a"),
                format_optional(row.fastest_anchor_mbps),
                format_ratio(row.track1_mbps, row.fastest_anchor_mbps),
                format_ratio(row.track2_mbps, row.fastest_anchor_mbps)
            ));
        }
        if !self.workload_rows.is_empty() {
            out.push_str("\n## Workloads\n\n");
            out.push_str("| Corpus | Workload | Strictness | parse_utf8 | escape_complete | flaw_probe | Track 1 Mbps | Track 2 Mbps | sonic-rs Mbps | serde_json Mbps | Track 1 / sonic | Track 2 / sonic | Signal |\n");
            out.push_str("|---|---|---|---|---|---|---:|---:|---:|---:|---:|---:|---|\n");
            for row in &self.workload_rows {
                out.push_str(&format!(
                    "| {} | {} | deferred | view-boundary | yes | generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary | {} | {} | {} | {} | {} | {} | {} |\n",
                    row.corpus,
                    row.workload,
                    format_optional(row.track1_mbps),
                    format_optional(row.track2_mbps),
                    format_optional(row.sonic_mbps),
                    format_optional(row.serde_json_mbps),
                    format_ratio_value(row.track1_vs_sonic),
                    format_ratio_value(row.track2_vs_sonic),
                    row.correctness
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

fn ratio_to_track1(probe_ns: Option<f64>, track1_ns: Option<f64>) -> Option<f64> {
    match (probe_ns, track1_ns) {
        (Some(probe_ns), Some(track1_ns)) if probe_ns > 0.0 && track1_ns > 0.0 => {
            Some(track1_ns / probe_ns)
        }
        _ => None,
    }
}

fn speed_ratio(candidate_ns: Option<f64>, anchor_ns: Option<f64>) -> Option<f64> {
    match (candidate_ns, anchor_ns) {
        (Some(candidate), Some(anchor)) if candidate > 0.0 && anchor > 0.0 => {
            Some(anchor / candidate)
        }
        _ => None,
    }
}

fn format_ratio_value(value: Option<f64>) -> String {
    value
        .map(|value| format!("{:.1}%", value * 100.0))
        .unwrap_or_else(|| "n/a".to_string())
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

    #[test]
    fn renders_report_skeleton_table() {
        let mut report = Report::new("Skinny JSON Bench");
        report.push_row(
            "twitter",
            Outcome::ABeatAndParity,
            631_515,
            Some(390_000.0),
            Some(360_000.0),
            Some(424_000.0),
            Some(424_000.0),
            Some(500_000.0),
            Some("simd-json borrowed"),
            Some(424_000.0),
        );
        let markdown = report.render_markdown();
        assert!(markdown.contains("Track 1 Mbps"));
        assert!(markdown.contains("| twitter | A | GO | deferred | view-boundary | yes |"));
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
