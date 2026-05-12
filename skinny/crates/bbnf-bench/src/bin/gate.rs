use bbnf_bench::gate::{self, ThresholdInput};
use bbnf_bench::materialization::track_stats;
use bbnf_bench::metadata::{RowMetadata, TrackTag};
use bbnf_bench::report::Report;
use serde_json::Value;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
    let criterion_root = workspace_root().join("target/criterion");
    let results_path = workspace_root().join("RESULTS.md");
    let fixtures = test_fixtures::load_available_bench_fixtures()?;
    let mut report = Report::new("Skinny JSON Bench Results");
    let mut outcomes = Vec::new();

    for fixture in fixtures {
        let group = criterion_root.join(format!("json_{}", fixture.name));
        let rows = read_metadata_rows(&group);
        let estimates = Estimates {
            track1: read_slope_ns(&group, "track1_generated"),
            track2: read_slope_ns(&group, "track2_handcoded"),
            sonic: read_slope_ns(&group, "sonic_rs_anchor"),
            simd_borrowed: read_slope_ns(&group, "simd_json_borrowed"),
            simd_owned: read_slope_ns(&group, "simd_json_owned"),
        };
        let input = std::str::from_utf8(&fixture.bytes)?;
        let parity_ok = bbnf_bench::parity::assert_parity(input).is_ok();
        let simd_parity_ok = bbnf_bench::scan::structural_offsets_scalar(&fixture.bytes)
            == bbnf_bench::scan::structural_offsets_simd(&fixture.bytes);
        let canada_scan_gbps =
            simd_canada_gbps(&criterion_root, &fixture.name, fixture.bytes.len());
        let outcome = gate::classify(&ThresholdInput {
            schema_ok: gate::validate_schema(&rows) && estimates.required_present(),
            parity_ok,
            simd_parity_ok,
            simd_canada_gbps: canada_scan_gbps,
            simd_floor_gbps: simd_floor_gbps(),
            track1_ns: estimates.track1.unwrap_or_default(),
            track2_ns: estimates.track2.unwrap_or_default(),
            sonic_rs_anchor_ns: estimates.sonic,
            simd_json_borrowed_ns: estimates.simd_borrowed,
            simd_json_owned_ns: estimates.simd_owned,
            readme_target_ns: readme_target_ns(&fixture.name),
            fastest_competitor_peak_rss: None,
            bbnf_peak_rss: None,
        });
        outcomes.push(outcome);
        report.push_row(
            fixture.name.clone(),
            outcome,
            fixture.bytes.len() as u64,
            estimates.track1,
            estimates.track2,
            estimates.sonic,
        );
        push_probe_rows(
            &mut report,
            &criterion_root,
            &fixture.name,
            fixture.bytes.len() as u64,
            estimates.track1,
        );
        if let Some(note) = arena_counter_note(&rows, &fixture.name) {
            report.notes.push(note);
        }
        if let Some(note) = materialization_note(input, &fixture.name) {
            report.notes.push(note);
        }
        if let Some(gbps) = canada_scan_gbps {
            let mbps = gbps * 8_000.0;
            let floor_mbps = simd_floor_gbps() * 8_000.0;
            report.notes.push(format!(
                "canada structural scan: {mbps:.0} Mbps; floor is {floor_mbps:.0} Mbps."
            ));
        }
    }

    if let Some(worst) = gate::worst_outcome(outcomes) {
        report.notes.push(format!(
            "Overall outcome {} / {:?}.",
            worst.id(),
            worst.verdict()
        ));
    }
    report.notes.push(
        "Track 1 is runtime::generated_json::parse; Track 2 is the independent hand-coded parser over runtime::tape."
            .to_string(),
    );
    report.write_markdown(&results_path)?;
    println!("{}", report.render_markdown());
    Ok(())
}

#[derive(Default)]
struct Estimates {
    track1: Option<f64>,
    track2: Option<f64>,
    sonic: Option<f64>,
    simd_borrowed: Option<f64>,
    simd_owned: Option<f64>,
}

impl Estimates {
    fn required_present(&self) -> bool {
        self.track1.is_some()
            && self.track2.is_some()
            && (self.sonic.is_some() || self.simd_borrowed.is_some() || self.simd_owned.is_some())
    }
}

fn read_metadata_rows(group: &Path) -> Vec<RowMetadata> {
    [
        "track1_generated",
        "track2_handcoded",
        "sonic_rs_anchor",
        "simd_json_borrowed",
        "simd_json_owned",
    ]
    .into_iter()
    .filter_map(|bench| fs::read_to_string(group.join(bench).join("metadata.toml")).ok())
    .filter_map(|text| toml::from_str::<RowMetadata>(&text).ok())
    .collect()
}

fn read_slope_ns(group: &Path, bench: &str) -> Option<f64> {
    let text = fs::read_to_string(group.join(bench).join("new/estimates.json")).ok()?;
    let value: Value = serde_json::from_str(&text).ok()?;
    value
        .pointer("/slope/point_estimate")
        .or_else(|| value.pointer("/mean/point_estimate"))
        .and_then(Value::as_f64)
        .filter(|value| value.is_finite() && *value > 0.0)
}

fn arena_counter_note(rows: &[RowMetadata], corpus: &str) -> Option<String> {
    let track1 = rows
        .iter()
        .find(|row| row.track == TrackTag::Track1Generated)
        .and_then(|row| Some((row.arena_writes?, row.payload_allocations?)))?;
    let track2 = rows
        .iter()
        .find(|row| row.track == TrackTag::Track2Handcoded)
        .and_then(|row| Some((row.arena_writes?, row.payload_allocations?)))?;
    Some(format!(
        "{corpus} payload arena counters: Track 1 {}/{} writes/allocations; Track 2 {}/{} writes/allocations.",
        track1.0, track1.1, track2.0, track2.1
    ))
}

fn materialization_note(input: &str, corpus: &str) -> Option<String> {
    let (track1, track2) = track_stats(input)?;
    if track1 == track2 {
        Some(track1.summary(corpus))
    } else {
        Some(format!(
            "{corpus} lazy tape materialization diverged: Track 1 {} offsets / {} bytes, Track 2 {} offsets / {} bytes.",
            track1.offset_count, track1.offset_bytes, track2.offset_count, track2.offset_bytes
        ))
    }
}

fn push_probe_rows(
    report: &mut Report,
    criterion_root: &Path,
    corpus: &str,
    bytes: u64,
    track1_ns: Option<f64>,
) {
    let group = criterion_root.join(format!("json_probes_{corpus}"));
    for probe in [
        "host_call_dispatch_overhead",
        "host_call_eager_decode",
        "alternate_scalar_plan",
        "alternate_dispatch_table_plan",
        "alternate_pext_mask_plan",
        "cold_first_parse",
    ] {
        if probe == "alternate_dispatch_table_plan" {
            report.push_probe_row(
                corpus,
                probe,
                bytes,
                None,
                None,
                "INVALID duplicate-probe disabled; real function-pointer table regressed",
            );
            continue;
        }
        let ns = read_slope_ns(&group, probe);
        let (probe_bytes, ratio_track1) = if probe == "host_call_dispatch_overhead" {
            (0, None)
        } else {
            (bytes, track1_ns)
        };
        report.push_probe_row(
            corpus,
            probe,
            probe_bytes,
            ns,
            ratio_track1,
            probe_signal(corpus, probe, ns, track1_ns),
        );
    }
}

fn probe_signal(
    corpus: &str,
    probe: &str,
    probe_ns: Option<f64>,
    track1_ns: Option<f64>,
) -> String {
    let Some(probe_ns) = probe_ns else {
        return "missing".to_string();
    };
    match probe {
        "host_call_dispatch_overhead" => {
            if probe_ns <= 50.0 {
                "PASS <=50ns".to_string()
            } else {
                "FAIL >50ns".to_string()
            }
        }
        "host_call_eager_decode" => {
            let Some(track1_ns) = track1_ns else {
                return "reported".to_string();
            };
            let max_ratio = match corpus {
                "twitter" => 1.15,
                "citm_catalog" => 1.08,
                "canada" => 1.02,
                _ => 1.10,
            };
            if probe_ns <= track1_ns * max_ratio {
                format!("PASS <={max_ratio:.2}x T1")
            } else {
                format!("MASKING >{max_ratio:.2}x T1")
            }
        }
        "cold_first_parse" => {
            let Some(track1_ns) = track1_ns else {
                return "reported".to_string();
            };
            if probe_ns <= track1_ns * 2.0 {
                "PASS <=2.00x T1".to_string()
            } else {
                "reported cold-sensitive".to_string()
            }
        }
        _ => "reported".to_string(),
    }
}

fn simd_canada_gbps(criterion_root: &Path, fixture: &str, bytes: usize) -> Option<f64> {
    if fixture != "canada" {
        return None;
    }
    let ns = read_slope_ns(&criterion_root.join("simd_structural_scan"), "canada_simd")?;
    Some(bytes as f64 / ns)
}

fn simd_floor_gbps() -> f64 {
    if cfg!(any(target_arch = "aarch64", target_arch = "arm")) {
        5.0
    } else {
        7.0
    }
}

fn readme_target_ns(name: &str) -> f64 {
    match name {
        "twitter" => 380_000.0,
        "citm_catalog" => 750_000.0,
        "canada" => 2_800_000.0,
        _ => f64::INFINITY,
    }
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}
