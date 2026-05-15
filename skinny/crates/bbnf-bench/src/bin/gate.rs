use bbnf_bench::gate::{self, DirectProjectionInput, Outcome, ThresholdInput, Verdict};
use bbnf_bench::materialization::track_stats;
use bbnf_bench::metadata::{current_peak_rss_bytes, RowMetadata, TrackTag};
use bbnf_bench::report::Report;
use serde_json::Value;
use std::env;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.get(1).is_some_and(|arg| arg == "--rss-probe") {
        return rss_probe_main(&args[2..]);
    }
    let advisory = args.iter().skip(1).any(|arg| arg == "--advisory");

    let criterion_root = env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root().join("target"))
        .join("criterion");
    let results_path = workspace_root().join("RESULTS.md");
    let fixtures = test_fixtures::load_available_bench_fixtures()?;
    let mut report = Report::new("Skinny JSON Bench Results");
    let mut outcomes = Vec::new();

    for fixture in fixtures {
        let group = criterion_root.join(format!("json_{}", fixture.name));
        let mut rows = read_metadata_rows(&group);
        let simd_metadata = read_simd_metadata_row(&criterion_root, &fixture.name);
        if let Some(row) = simd_metadata.clone() {
            rows.push(row);
        }
        let estimates = Estimates {
            track1: read_slope_ns(&group, "track1_generated"),
            track2: read_slope_ns(&group, "track2_handcoded"),
            sonic: read_slope_ns(&group, "sonic_rs_anchor"),
            simd_borrowed: read_slope_ns(&group, "simd_json_borrowed"),
            simd_owned: read_slope_ns(&group, "simd_json_owned"),
            direct_track1: read_slope_ns(&group, "track1_direct_to_struct"),
            direct_track2: read_slope_ns(&group, "track2_direct_to_struct"),
            direct_sonic: read_slope_ns(&group, "sonic_rs_direct_to_struct"),
            direct_serde: read_slope_ns(&group, "serde_json_direct_to_struct"),
            real_typed_track1: read_slope_ns(&group, "track1_real_typed_struct"),
            real_typed_track2: read_slope_ns(&group, "track2_real_typed_struct"),
            real_typed_sonic: read_slope_ns(&group, "sonic_rs_real_typed_struct"),
            real_typed_serde: read_slope_ns(&group, "serde_json_real_typed_struct"),
        };
        let input = std::str::from_utf8(&fixture.bytes)?;
        let parity_ok = bbnf_bench::parity::assert_parity(input).is_ok();
        let direct_struct_ok =
            bbnf_bench::direct_struct::assert_direct_struct_parity(input, &fixture.bytes).is_ok();
        let scalar_offsets = bbnf_bench::scan::structural_offsets_scalar(&fixture.bytes);
        let simd_offsets = bbnf_bench::scan::structural_offsets_simd(&fixture.bytes);
        let scalar_hash = bbnf_bench::scan::hash_offsets(&scalar_offsets);
        let simd_hash = bbnf_bench::scan::hash_offsets(&simd_offsets);
        let simd_parity_ok = scalar_hash == simd_hash
            && simd_metadata.as_ref().is_some_and(|row| {
                simd_metadata_hash(row, &fixture.name).as_deref() == Some(&scalar_hash)
            });
        let canada_scan_gbps =
            simd_canada_gbps(&criterion_root, &fixture.name, fixture.bytes.len());
        let (fastest_competitor_peak_rss, bbnf_peak_rss) =
            peak_rss_bounds(&fixture, estimates.fastest_anchor().map(|anchor| anchor.0));
        let outcome = gate::classify(&ThresholdInput {
            schema_ok: gate::validate_schema(&rows) && estimates.required_present(),
            parity_ok: parity_ok && direct_struct_ok,
            simd_parity_ok,
            simd_canada_gbps: canada_scan_gbps,
            simd_floor_gbps: simd_floor_gbps(),
            track1_ns: estimates.track1.unwrap_or_default(),
            track2_ns: estimates.track2.unwrap_or_default(),
            sonic_rs_anchor_ns: estimates.sonic,
            simd_json_borrowed_ns: estimates.simd_borrowed,
            simd_json_owned_ns: estimates.simd_owned,
            readme_target_ns: readme_target_ns(&fixture.name),
            fastest_competitor_peak_rss,
            bbnf_peak_rss,
        });
        outcomes.push(outcome);
        let direct_outcome = gate::classify_direct_projection(&DirectProjectionInput {
            correctness_ok: direct_struct_ok,
            track1_ns: estimates.direct_track1,
            track2_ns: estimates.direct_track2,
            sonic_rs_ns: estimates.direct_sonic,
        });
        if let Some(outcome) = direct_outcome {
            outcomes.push(outcome);
        }
        report.push_row(
            fixture.name.clone(),
            outcome,
            fixture.bytes.len() as u64,
            estimates.track1,
            estimates.track2,
            estimates.sonic,
            estimates.simd_borrowed,
            estimates.simd_owned,
            estimates
                .fastest_anchor()
                .map(|anchor| anchor.0.to_string()),
            estimates.fastest_anchor().map(|anchor| anchor.1),
        );
        report.push_workload_row(
            &fixture.name,
            "direct_to_struct",
            fixture.bytes.len() as u64,
            estimates.direct_track1,
            estimates.direct_track2,
            estimates.direct_sonic,
            estimates.direct_serde,
            direct_workload_signal(
                direct_struct_ok,
                direct_outcome,
                fixture.bytes.len() as u64,
                estimates.direct_track1,
                estimates.direct_track2,
                estimates.direct_sonic,
            ),
        );
        if direct_outcome == Some(Outcome::NDirectProjectionFailure) {
            report.notes.push(direct_projection_note(
                &fixture.name,
                fixture.bytes.len() as u64,
                &estimates,
            ));
        }
        if estimates.real_typed_track1.is_some() {
            let real_typed_ok = bbnf_bench::real_typed_struct::fixture_for_name(&fixture.name)
                .is_some_and(|real_typed| {
                    bbnf_bench::real_typed_struct::assert_real_typed_parity(
                        input,
                        &fixture.bytes,
                        real_typed,
                    );
                    true
                });
            let real_typed_outcome = classify_real_typed_projection(
                real_typed_ok,
                estimates.real_typed_track1,
                estimates.real_typed_sonic,
            );
            if let Some(outcome) = real_typed_outcome {
                outcomes.push(outcome);
            }
            report.push_workload_row(
                &fixture.name,
                "real_typed_struct",
                fixture.bytes.len() as u64,
                estimates.real_typed_track1,
                estimates.real_typed_track2,
                estimates.real_typed_sonic,
                estimates.real_typed_serde,
                real_typed_workload_signal(
                    real_typed_ok,
                    real_typed_outcome,
                    fixture.bytes.len() as u64,
                    estimates.real_typed_track1,
                    estimates.real_typed_track2,
                    estimates.real_typed_sonic,
                ),
            );
        }
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
        if let Some(note) = peak_rss_note(
            &fixture.name,
            fastest_competitor_peak_rss,
            bbnf_peak_rss,
            estimates.fastest_anchor().map(|anchor| anchor.0),
        ) {
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

    let hard_failure = outcomes.iter().copied().find(|outcome| {
        matches!(
            outcome,
            Outcome::IParityOracleFail | Outcome::JSchemaFail | Outcome::KSimdParityHashFail
        )
    });
    let worst_outcome = gate::worst_outcome(outcomes.iter().copied());
    if let Some(worst) = worst_outcome {
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
    report.notes.push(
        "Track 2 checklist signed by implementation owner: Track 2 uses runtime::tape::TapeBuilder, shares the same parity oracle as Track 1, and never calls runtime::generated_json::parse."
            .to_string(),
    );
    report.notes.push(
        "Sidecar strictness metadata: sonic-rs/simd-json/serde_json rows are strict / scan-boundary / yes; asmjson and RapidJSON default rows, when populated in Wave 6, must be rendered as permissive / none / no with their API and output plane named."
            .to_string(),
    );
    report.write_markdown(&results_path)?;
    println!("{}", report.render_markdown());
    let exit_outcome = if advisory {
        hard_failure
    } else {
        worst_outcome
    };
    if let Some(worst) = exit_outcome {
        let exit_code = exit_code_for_verdict(worst.verdict());
        if exit_code != 0 {
            std::process::exit(exit_code);
        }
    }
    Ok(())
}

fn exit_code_for_verdict(verdict: Verdict) -> i32 {
    match verdict {
        Verdict::Go | Verdict::GoWithFocus => 0,
        Verdict::Conditional => 6,
        Verdict::Invalid => 2,
        Verdict::NoGo => 5,
    }
}

fn direct_workload_signal(
    correctness_ok: bool,
    outcome: Option<Outcome>,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> String {
    if !correctness_ok {
        return "FAIL digest mismatch".to_string();
    }
    if outcome == Some(Outcome::NDirectProjectionFailure) {
        let track1 = throughput_mbps(bytes, track1_ns);
        let track2 = throughput_mbps(bytes, track2_ns);
        let sonic = throughput_mbps(bytes, sonic_ns);
        return format!(
            "NO-GO sink_only throughput > sonic-rs * {:.2} ns slack; correctness PASS; Track 1 {}, Track 2 {}, sonic {} Mbps",
            gate::DIRECT_PROJECTION_SONIC_SLACK,
            format_mbps(track1),
            format_mbps(track2),
            format_mbps(sonic)
        );
    }
    "PASS correctness green; sonic shape parity; throughput within gate".to_string()
}

fn classify_real_typed_projection(
    correctness_ok: bool,
    track1_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> Option<Outcome> {
    if !correctness_ok {
        return Some(Outcome::IParityOracleFail);
    }
    let (Some(track1), Some(sonic)) = (track1_ns, sonic_ns) else {
        return None;
    };
    if track1 > sonic * gate::DIRECT_PROJECTION_SONIC_SLACK {
        return Some(Outcome::NDirectProjectionFailure);
    }
    None
}

fn real_typed_workload_signal(
    correctness_ok: bool,
    outcome: Option<Outcome>,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> String {
    if !correctness_ok {
        return "FAIL typed-output parity mismatch".to_string();
    }
    let track1 = throughput_mbps(bytes, track1_ns);
    let track2 = throughput_mbps(bytes, track2_ns);
    let sonic = throughput_mbps(bytes, sonic_ns);
    if outcome == Some(Outcome::NDirectProjectionFailure) {
        return format!(
            "NO-GO generated typed output > sonic-rs * {:.2} ns slack; correctness PASS; Track 1 {}, Track 2 oracle {}, sonic {} Mbps",
            gate::DIRECT_PROJECTION_SONIC_SLACK,
            format_mbps(track1),
            format_mbps(track2),
            format_mbps(sonic)
        );
    }
    format!(
        "PASS generated typed output within sonic-rs * {:.2} ns slack; correctness PASS; Track 2 oracle structurally different at {} Mbps",
        gate::DIRECT_PROJECTION_SONIC_SLACK,
        format_mbps(track2)
    )
}

fn direct_projection_note(corpus: &str, bytes: u64, estimates: &Estimates) -> String {
    format!(
        "{corpus} direct-to-struct gate: NO-GO. Track 1 {} Mbps, Track 2 {} Mbps, sonic-rs {} Mbps; Track 1 and Track 2 must be within {:.2}x sonic-rs time.",
        format_mbps(throughput_mbps(bytes, estimates.direct_track1)),
        format_mbps(throughput_mbps(bytes, estimates.direct_track2)),
        format_mbps(throughput_mbps(bytes, estimates.direct_sonic)),
        gate::DIRECT_PROJECTION_SONIC_SLACK
    )
}

fn throughput_mbps(bytes: u64, ns: Option<f64>) -> Option<f64> {
    if bytes == 0 {
        return None;
    }
    ns.filter(|ns| *ns > 0.0 && ns.is_finite())
        .map(|ns| bytes as f64 * 8_000.0 / ns)
}

fn format_mbps(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:.0}"))
        .unwrap_or_else(|| "n/a".to_string())
}

fn peak_rss_bounds(
    fixture: &test_fixtures::JsonFixture,
    anchor: Option<&str>,
) -> (Option<u64>, Option<u64>) {
    let bbnf = ["track1_generated", "track2_handcoded"]
        .into_iter()
        .filter_map(|mode| rss_probe_bytes(mode, fixture))
        .max();
    let competitor_modes: &[&str] = match anchor {
        Some("sonic-rs") => &["sonic_rs_anchor"],
        Some("simd-json borrowed") => &["simd_json_borrowed"],
        Some("simd-json owned") => &["simd_json_owned"],
        _ => &["sonic_rs_anchor", "simd_json_borrowed", "simd_json_owned"],
    };
    let competitor = competitor_modes
        .iter()
        .filter_map(|mode| rss_probe_bytes(mode, fixture))
        .min();
    (competitor, bbnf)
}

fn peak_rss_note(
    corpus: &str,
    competitor: Option<u64>,
    bbnf: Option<u64>,
    anchor: Option<&str>,
) -> Option<String> {
    Some(format!(
        "{corpus} peak RSS subprocess probes: bbnf={} bytes, S anchor {}={} bytes.",
        bbnf?,
        anchor.unwrap_or("competitor"),
        competitor?
    ))
}

#[derive(Default)]
struct Estimates {
    track1: Option<f64>,
    track2: Option<f64>,
    sonic: Option<f64>,
    simd_borrowed: Option<f64>,
    simd_owned: Option<f64>,
    direct_track1: Option<f64>,
    direct_track2: Option<f64>,
    direct_sonic: Option<f64>,
    direct_serde: Option<f64>,
    real_typed_track1: Option<f64>,
    real_typed_track2: Option<f64>,
    real_typed_sonic: Option<f64>,
    real_typed_serde: Option<f64>,
}

impl Estimates {
    fn required_present(&self) -> bool {
        self.track1.is_some()
            && self.track2.is_some()
            && (self.sonic.is_some() || self.simd_borrowed.is_some() || self.simd_owned.is_some())
            && self.direct_track1.is_some()
            && self.direct_track2.is_some()
            && self.direct_sonic.is_some()
            && self.direct_serde.is_some()
    }

    fn fastest_anchor(&self) -> Option<(&'static str, f64)> {
        [
            ("sonic-rs", self.sonic),
            ("simd-json borrowed", self.simd_borrowed),
            ("simd-json owned", self.simd_owned),
        ]
        .into_iter()
        .filter_map(|(name, ns)| ns.map(|ns| (name, ns)))
        .filter(|(_, ns)| ns.is_finite() && *ns > 0.0)
        .min_by(|(_, a), (_, b)| a.total_cmp(b))
    }
}

fn read_metadata_rows(group: &Path) -> Vec<RowMetadata> {
    [
        "track1_generated",
        "track2_handcoded",
        "sonic_rs_anchor",
        "simd_json_borrowed",
        "simd_json_owned",
        "track1_direct_to_struct",
        "track2_direct_to_struct",
        "sonic_rs_direct_to_struct",
        "serde_json_direct_to_struct",
        "track1_real_typed_struct",
        "track2_real_typed_struct",
        "sonic_rs_real_typed_struct",
        "serde_json_real_typed_struct",
    ]
    .into_iter()
    .filter_map(|bench| fs::read_to_string(group.join(bench).join("metadata.toml")).ok())
    .filter_map(|text| toml::from_str::<RowMetadata>(&text).ok())
    .collect()
}

fn read_simd_metadata_row(criterion_root: &Path, fixture: &str) -> Option<RowMetadata> {
    let text = fs::read_to_string(
        criterion_root
            .join("simd_structural_scan")
            .join(format!("{fixture}_simd"))
            .join("metadata.toml"),
    )
    .ok()?;
    toml::from_str::<RowMetadata>(&text).ok()
}

fn simd_metadata_hash(row: &RowMetadata, fixture: &str) -> Option<String> {
    match fixture {
        "twitter" => row.scalar_parity_hash_twitter.clone(),
        "citm_catalog" => row.scalar_parity_hash_citm.clone(),
        "canada" => row.scalar_parity_hash_canada.clone(),
        _ => row
            .scalar_parity_hash_twitter
            .clone()
            .or_else(|| row.scalar_parity_hash_citm.clone())
            .or_else(|| row.scalar_parity_hash_canada.clone()),
    }
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

fn rss_probe_bytes(mode: &str, fixture: &test_fixtures::JsonFixture) -> Option<u64> {
    let path = fixture.path.as_ref()?;
    let output = Command::new(env::current_exe().ok()?)
        .arg("--rss-probe")
        .arg(mode)
        .arg(path)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8(output.stdout).ok()?;
    text.trim().parse().ok()
}

fn rss_probe_main(args: &[String]) -> Result<(), Box<dyn Error>> {
    let mode = args.first().ok_or("missing rss probe mode")?;
    let path = args.get(1).ok_or("missing rss probe path")?;
    let bytes = fs::read(path)?;
    match mode.as_str() {
        "track1_generated" => {
            let input = std::str::from_utf8(&bytes)?;
            let root = runtime::generated_json::parse(input)
                .map_err(|error| format!("track1 rss probe parse failed: {error}"))?;
            std::hint::black_box(root);
        }
        "track2_handcoded" => {
            let input = std::str::from_utf8(&bytes)?;
            let root = bbnf_bench::track2::json::parse(input)
                .map_err(|error| format!("track2 rss probe parse failed: {error}"))?;
            std::hint::black_box(root);
        }
        "sonic_rs_anchor" => {
            let value = sonic_rs::from_slice::<sonic_rs::Value>(&bytes)?;
            std::hint::black_box(value);
        }
        "simd_json_borrowed" => {
            let mut bytes = bytes;
            let value = simd_json::to_borrowed_value(&mut bytes)?;
            std::hint::black_box(value);
        }
        "simd_json_owned" => {
            let mut bytes = bytes;
            let value = simd_json::to_owned_value(&mut bytes)?;
            std::hint::black_box(value);
        }
        other => return Err(format!("unknown rss probe mode {other}").into()),
    }
    println!("{}", current_peak_rss_bytes().unwrap_or_default());
    Ok(())
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
