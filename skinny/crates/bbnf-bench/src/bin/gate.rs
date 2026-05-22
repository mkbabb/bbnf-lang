use bbnf_bench::gate::{self, DirectProjectionInput, Outcome, ThresholdInput, Verdict};
use bbnf_bench::lock14_baseline;
use bbnf_bench::materialization::track_stats;
use bbnf_bench::metadata::{current_peak_rss_bytes, RowMetadata, TrackTag};
use bbnf_bench::report::{
    sk_v8_open_baseline, ComparatorSet, NonJsonEvidenceReport, Report, SkV12CssL4SotaReport,
    SkV12NonJsonReport, SkV13CssComparatorOracleReport, SkV13CssDeclarationValuesExtendedReport,
    SkV13CssStylesheetSelectorsReport, SkV8ComparatorEvidence, SkV8Telemetry, TelemetryRow,
};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
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
    let update_results = args
        .iter()
        .skip(1)
        .any(|arg| matches!(arg.as_str(), "--update-results" | "--write-results"));
    let include_volatile_probes = args
        .iter()
        .skip(1)
        .any(|arg| arg == "--include-volatile-probes");
    if update_results && include_volatile_probes {
        return Err(
            "--include-volatile-probes cannot be combined with --update-results or --write-results"
                .into(),
        );
    }
    let workspace = workspace_root();
    if let Err(error) = lock14_baseline::validate(&workspace) {
        return Err(format!("Lock 14 baseline validation failed: {error}").into());
    }
    let has_explicit_json_check = companion_report_runs_json_check(&args[1..]);
    if let Some(path) = w1a_non_json_report_path(&args[1..])? {
        let text = fs::read_to_string(&path)?;
        let report = NonJsonEvidenceReport::from_json_str(&text)
            .and_then(|report| report.validate_w1a_non_json_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        println!("G-W1a-NONJSON-GATE PASS {}", path.display());
        drop(report);
        if !has_explicit_json_check {
            return Ok(());
        }
    }
    if let Some(path) = skv12_non_json_report_path(&args[1..])? {
        let text = fs::read_to_string(&path)?;
        let report = SkV12NonJsonReport::from_json_str(&text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        println!("G-W0-SK-V12-NONJSON-GATE PASS {}", path.display());
        drop(report);
        if !has_explicit_json_check {
            return Ok(());
        }
    }
    if let Some(path) = skv12_css_l4_sota_report_path(&args[1..])? {
        let text = fs::read_to_string(&path)?;
        let report = SkV12CssL4SotaReport::from_json_str(&text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        validate_skv12_css_l4_sota_report(&report, &criterion_root(), &workspace)
            .map_err(|error| format!("{}: {error}", path.display()))?;
        println!(
            "G-W1b-2b-CSS-L4-LIGHTNINGCSS-SOTA {} {}",
            report.rows[0].admission_status,
            path.display()
        );
        drop(report);
        if !has_explicit_json_check {
            return Ok(());
        }
    }
    if let Some(path) = skv13_css_comparator_oracle_report_path(&args[1..])? {
        if !has_explicit_json_check {
            return Err(format!("{} requires --check-results", path.display()).into());
        }
        let text = fs::read_to_string(&path)?;
        let report = SkV13CssComparatorOracleReport::from_json_str(&text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        let sota_path =
            resolve_workspace_path(&workspace, &report.declaration_values_sota_report_path);
        let sota_text = fs::read_to_string(&sota_path)?;
        let sota_report = SkV12CssL4SotaReport::from_json_str(&sota_text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", sota_path.display()))?;
        validate_skv12_css_l4_sota_report(&sota_report, &criterion_root(), &workspace)
            .map_err(|error| format!("{}: {error}", sota_path.display()))?;
        println!(
            "G-W1-CSS-COMPARATOR-ORACLE PASS {} feature_rows={}",
            path.display(),
            report.rows.len()
        );
        drop(report);
    }
    if let Some(path) = skv13_css_stylesheet_selectors_report_path(&args[1..])? {
        if !has_explicit_json_check {
            return Err(format!("{} requires --check-results", path.display()).into());
        }
        let text = fs::read_to_string(&path)?;
        let report = SkV13CssStylesheetSelectorsReport::from_json_str(&text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        validate_skv13_css_stylesheet_selectors_report(&report, &criterion_root(), &workspace)
            .map_err(|error| format!("{}: {error}", path.display()))?;
        println!(
            "G-W2-CSS-STYLESHEET-SELECTORS {} {} feature_rows={}",
            report.rows[0].admission_status,
            path.display(),
            report.covered_feature_rows.len()
        );
        drop(report);
    }
    if let Some(path) = skv13_css_declaration_values_extended_report_path(&args[1..])? {
        if !has_explicit_json_check {
            return Err(format!("{} requires --check-results", path.display()).into());
        }
        let text = fs::read_to_string(&path)?;
        let report = SkV13CssDeclarationValuesExtendedReport::from_json_str(&text)
            .and_then(|report| report.validate_gate().map(|_| report))
            .map_err(|error| format!("{}: {error}", path.display()))?;
        validate_skv13_css_declaration_values_extended_report(
            &report,
            &criterion_root(),
            &workspace,
        )
        .map_err(|error| format!("{}: {error}", path.display()))?;
        println!(
            "G-W3-CSS-DECLARATION-VALUES-EXTENDED {} {} feature_rows={}",
            report.rows[0].admission_status,
            path.display(),
            report.covered_feature_rows.len()
        );
        drop(report);
    }

    let criterion_root = criterion_root();
    let results_path = workspace.join("RESULTS.md");
    let fixtures = test_fixtures::load_available_bench_fixtures()?;
    let fixture_names = fixtures
        .iter()
        .map(|fixture| fixture.name.as_str())
        .collect::<BTreeSet<_>>();
    let run_facts = RunFacts::probe(&criterion_root, &fixture_names);
    let mut report = Report::new("Skinny JSON Bench Results");
    let mut outcomes = Vec::new();
    let mut report_capture_identity = None;

    for fixture in fixtures {
        let group = criterion_root.join(format!("json_{}", fixture.name));
        let mut rows = read_metadata_rows(&group)?;
        validate_w0_capture_metadata(
            &fixture.name,
            &fixture.sha256,
            fixture.bytes.len() as u64,
            w0_real_typed_metadata_expected(&fixture.name),
            &rows,
        )
        .map_err(|error| format!("{} metadata invalid: {error}", fixture.name))?;
        let main_capture = rows
            .first()
            .expect("validated W0 capture metadata rows are nonempty");
        validate_report_capture_identity(
            &mut report_capture_identity,
            &fixture.name,
            main_capture,
        )?;
        let scalar_offsets = bbnf_bench::scan::structural_offsets_scalar(&fixture.bytes);
        let simd_offsets = bbnf_bench::scan::structural_offsets_simd(&fixture.bytes);
        let scalar_hash = bbnf_bench::scan::hash_offsets(&scalar_offsets);
        let simd_hash = bbnf_bench::scan::hash_offsets(&simd_offsets);
        let simd_metadata = read_simd_metadata_row(&criterion_root, &fixture.name)?;
        validate_w0_simd_metadata(
            &fixture.name,
            &fixture.sha256,
            fixture.bytes.len() as u64,
            &scalar_hash,
            main_capture,
            &simd_metadata,
        )
        .map_err(|error| format!("{} SIMD metadata invalid: {error}", fixture.name))?;
        rows.push(simd_metadata.clone());
        let estimates = Estimates {
            track1: read_slope_ns(&group, "track1_generated"),
            track2: read_slope_ns(&group, "track2_handcoded"),
            sonic: read_slope_ns(&group, "sonic_rs_anchor"),
            sonic_lossy: read_slope_ns(&group, "sonic_rs_lossy"),
            simd_borrowed: read_slope_ns(&group, "simd_json_borrowed"),
            simd_owned: read_slope_ns(&group, "simd_json_owned"),
            serde_json: read_slope_ns(&group, "serde_json"),
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
        let simd_parity_ok = scalar_hash == simd_hash
            && simd_metadata_hash(&simd_metadata, &fixture.name).as_deref() == Some(&scalar_hash);
        let canada_scan_gbps =
            simd_canada_gbps(&criterion_root, &fixture.name, fixture.bytes.len());
        let (fastest_competitor_peak_rss, bbnf_peak_rss) = if include_volatile_probes {
            peak_rss_bounds(&fixture, estimates.fastest_anchor().map(|anchor| anchor.0))
        } else {
            (None, None)
        };
        let classified_outcome = gate::classify(&ThresholdInput {
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
        let outcome = w0_parse_non_admission(classified_outcome);
        outcomes.push(outcome);
        let classified_direct_outcome = gate::classify_direct_projection(&DirectProjectionInput {
            correctness_ok: direct_struct_ok,
            track1_ns: estimates.direct_track1,
            track2_ns: estimates.direct_track2,
            sonic_rs_ns: estimates.direct_sonic,
        });
        let direct_decision = direct_row_decision(
            &fixture.name,
            classified_direct_outcome,
            fixture.bytes.len() as u64,
            estimates.direct_track1,
            estimates.direct_track2,
        );
        let direct_outcome = direct_decision.outcome;
        if let Some(outcome) = direct_outcome {
            outcomes.push(outcome);
        }
        let parse_comparators =
            parse_comparators(fixture.bytes.len() as u64, &fixture.name, &estimates);
        let parse_hot_leaf = w0_hot_leaf(&fixture.name, "track1_generated");
        let parse_telemetry = w0_telemetry(
            &fixture.name,
            "parse_only",
            "borrowed view over offset tape vs DOM",
            fixture.bytes.len() as u64,
            estimates.track1,
            &parse_comparators,
            &rows,
            &run_facts,
            "track1_generated",
        );
        report.rows.push(
            TelemetryRow::parse(
                fixture.name.clone(),
                outcome,
                fixture.bytes.len() as u64,
                estimates.track1,
                estimates.track2,
                parse_comparators,
                parse_hot_leaf,
            )
            .with_sk_v8(parse_telemetry),
        );
        let direct_comparators = direct_comparators(fixture.bytes.len() as u64, &estimates);
        let direct_signal = direct_workload_signal(
            direct_struct_ok,
            direct_outcome,
            direct_decision.w0_clamped,
            direct_decision.w2_reclaimed,
            direct_decision.w10_residual,
            fixture.bytes.len() as u64,
            estimates.direct_track1,
            estimates.direct_track2,
            estimates.direct_sonic,
        );
        let direct_telemetry = w0_telemetry(
            &fixture.name,
            "direct_to_struct",
            "digest",
            fixture.bytes.len() as u64,
            estimates.direct_track1,
            &direct_comparators,
            &rows,
            &run_facts,
            "track1_direct_to_struct",
        );
        let mut direct_row = TelemetryRow::workload(
            &fixture.name,
            "direct_to_struct",
            direct_outcome,
            fixture.bytes.len() as u64,
            estimates.direct_track1,
            estimates.direct_track2,
            direct_comparators,
            "digest",
            "generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 remains view-boundary",
            direct_signal,
            w0_hot_leaf(&fixture.name, "track1_direct_to_struct"),
        )
        .with_sk_v8(direct_telemetry);
        if direct_decision.w2_reclaimed {
            mark_w2_direct_reclamation(&mut direct_row);
        } else if direct_decision.w10_residual {
            mark_w10_direct_residual(&mut direct_row);
        }
        report.rows.push(direct_row);
        if direct_outcome == Some(Outcome::NDirectProjectionFailure) {
            report.notes.push(direct_projection_note(
                &fixture.name,
                fixture.bytes.len() as u64,
                &estimates,
            ));
        }
        if w0_real_typed_metadata_expected(&fixture.name) && estimates.real_typed_track1.is_some() {
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
            let typed_decision = real_typed_row_decision(
                &fixture.name,
                real_typed_outcome,
                fixture.bytes.len() as u64,
                estimates.real_typed_track1,
                estimates.real_typed_track2,
                estimates.real_typed_sonic,
            );
            if let Some(outcome) = typed_decision.outcome {
                outcomes.push(outcome);
            }
            let typed_comparators = real_typed_comparators(fixture.bytes.len() as u64, &estimates);
            let typed_signal = real_typed_workload_signal(
                real_typed_ok,
                typed_decision.outcome,
                fixture.bytes.len() as u64,
                estimates.real_typed_track1,
                estimates.real_typed_track2,
                estimates.real_typed_sonic,
            );
            let typed_telemetry = w0_telemetry(
                &fixture.name,
                "real_typed_struct",
                "typed direct",
                fixture.bytes.len() as u64,
                estimates.real_typed_track1,
                &typed_comparators,
                &rows,
                &run_facts,
                "track1_real_typed_struct",
            );
            let mut typed_row = TelemetryRow::workload(
                &fixture.name,
                "real_typed_struct",
                typed_decision.outcome,
                fixture.bytes.len() as u64,
                estimates.real_typed_track1,
                estimates.real_typed_track2,
                typed_comparators,
                "typed direct",
                "generated Track 1 consumes host/API output schema; Track 2 is a structural oracle, not the SOTA gate; UTF-8 remains view-boundary",
                typed_signal,
                w0_hot_leaf(&fixture.name, "track1_real_typed_struct"),
            )
            .with_sk_v8(typed_telemetry);
            if typed_decision.w6_github_events_added {
                mark_w6_github_events_typed_admission(&mut typed_row);
            }
            report.rows.push(typed_row);
        }
        if include_volatile_probes {
            push_probe_rows(
                &mut report,
                &criterion_root,
                &fixture.name,
                fixture.bytes.len() as u64,
                estimates.track1,
            );
        }
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
        "SK-V9 W0 telemetry: gate-json consumes the manifest below; native Rust comparators are same-run, C++ sidecars are historical or explicitly absent and never strict anchors in W0."
            .to_string(),
    );
    if let Err(error) = report
        .validate_schema_v3()
        .and_then(|_| report.validate_sk_v8_w0())
    {
        report
            .notes
            .push(format!("Schema/W0 validation failure: {error}."));
        println!("{}", report.render_markdown());
        std::process::exit(exit_code_for_verdict(Verdict::Invalid));
    }
    let existing_results = fs::read_to_string(&results_path).ok();
    let rendered =
        merge_retained_non_json_results(&report.render_markdown(), existing_results.as_deref());
    if update_results {
        fs::write(&results_path, &rendered)?;
    } else if existing_results.as_deref() != Some(rendered.as_str()) {
        eprintln!(
            "{} is stale; rerun `cargo xtask gate-json --update-results{}` to rewrite it.",
            results_path.display(),
            if advisory { " --advisory" } else { "" }
        );
        std::process::exit(exit_code_for_verdict(Verdict::Invalid));
    }
    println!("{rendered}");
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

fn merge_retained_non_json_results(json_rendered: &str, existing_results: Option<&str>) -> String {
    let Some(existing_results) = existing_results else {
        return ensure_trailing_newline(json_rendered);
    };
    if json_rendered.contains("| css_l4/") {
        return ensure_trailing_newline(json_rendered);
    }

    let retained_rows = existing_results
        .lines()
        .filter(|line| line.starts_with("| css_l4/"))
        .collect::<Vec<_>>();
    if retained_rows.is_empty() {
        return ensure_trailing_newline(json_rendered);
    }
    let retained_notes = existing_results
        .lines()
        .filter(|line| line.starts_with("- SK-V12 campaign close:"))
        .collect::<Vec<_>>();

    let mut merged = json_rendered.trim_end().to_string();
    let retained_block = format!("\n\n{}", retained_rows.join("\n"));
    if let Some(notes_index) = merged.find("\n## Notes") {
        merged.insert_str(notes_index, &retained_block);
    } else {
        merged.push_str(&retained_block);
    }
    for note in retained_notes {
        if !merged.contains(note) {
            merged.push('\n');
            merged.push_str(note);
        }
    }
    ensure_trailing_newline(&merged)
}

fn ensure_trailing_newline(text: &str) -> String {
    if text.ends_with('\n') {
        text.to_string()
    } else {
        format!("{text}\n")
    }
}

fn exit_code_for_verdict(verdict: Verdict) -> i32 {
    match verdict {
        Verdict::Go | Verdict::GoWithFocus => 0,
        Verdict::Conditional => 6,
        Verdict::Invalid => 2,
        Verdict::NoGo => 5,
    }
}

fn w1a_non_json_report_path(args: &[String]) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--w1a-non-json-report")
}

fn skv12_non_json_report_path(args: &[String]) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--skv12-non-json-report")
}

fn skv12_css_l4_sota_report_path(args: &[String]) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--skv12-css-l4-sota-report")
}

fn skv13_css_comparator_oracle_report_path(
    args: &[String],
) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--skv13-css-comparator-oracle-report")
}

fn skv13_css_stylesheet_selectors_report_path(
    args: &[String],
) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--skv13-css-stylesheet-selectors-report")
}

fn skv13_css_declaration_values_extended_report_path(
    args: &[String],
) -> Result<Option<PathBuf>, Box<dyn Error>> {
    companion_report_path(args, "--skv13-css-declaration-values-extended-report")
}

fn companion_report_path(args: &[String], flag: &str) -> Result<Option<PathBuf>, Box<dyn Error>> {
    let flag_positions = args
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| (arg == flag).then_some(index))
        .collect::<Vec<_>>();
    if flag_positions.is_empty() {
        return Ok(None);
    };
    if flag_positions.len() != 1 {
        return Err(format!("{flag} expects exactly one path argument").into());
    }
    let flag_index = flag_positions[0];
    if args.iter().any(|arg| {
        matches!(
            arg.as_str(),
            "--update-results" | "--write-results" | "--include-volatile-probes"
        )
    }) {
        return Err(
            format!("{flag} cannot be combined with JSON result update/probe flags").into(),
        );
    }
    if flag_index + 1 >= args.len() || args[flag_index + 1].starts_with("--") {
        return Err(format!("{flag} expects exactly one path argument").into());
    }
    let mut index = 0;
    while index < args.len() {
        if index == flag_index {
            index += 2;
            continue;
        }
        let arg = args[index].as_str();
        if matches!(arg, "--advisory" | "--check-results" | "--with-cost-facts") {
            index += 1;
            continue;
        }
        if is_companion_report_flag(arg) {
            if index + 1 >= args.len() || args[index + 1].starts_with("--") {
                return Err(format!("{arg} expects exactly one path argument").into());
            }
            index += 2;
            continue;
        }
        return Err(format!("{flag} cannot be combined with {}", args[index]).into());
    }
    Ok(Some(PathBuf::from(&args[flag_index + 1])))
}

fn is_companion_report_flag(arg: &str) -> bool {
    matches!(
        arg,
        "--w1a-non-json-report"
            | "--skv12-non-json-report"
            | "--skv12-css-l4-sota-report"
            | "--skv13-css-comparator-oracle-report"
            | "--skv13-css-stylesheet-selectors-report"
            | "--skv13-css-declaration-values-extended-report"
    )
}

fn companion_report_runs_json_check(args: &[String]) -> bool {
    args.iter()
        .any(|arg| matches!(arg.as_str(), "--check-results" | "--with-cost-facts"))
}

struct CssL4Lane {
    mbps: f64,
    samples: u64,
}

fn validate_skv12_css_l4_sota_report(
    report: &SkV12CssL4SotaReport,
    criterion_root: &Path,
    workspace: &Path,
) -> Result<(), String> {
    let row = &report.rows[0];
    let track1 = read_css_l4_lane(criterion_root, "track1_generated_css_l4_decl_values")?;
    let cssparser = read_css_l4_lane(criterion_root, "track2_cssparser_oracle")?;
    let lightningcss = read_css_l4_lane(criterion_root, "lightningcss_same_plane_fact_stream")?;
    require_close("track1_mbps", row.track1_mbps, track1.mbps)?;
    require_close(
        "track2_or_oracle_mbps",
        row.track2_or_oracle_mbps,
        cssparser.mbps,
    )?;
    require_close(
        "lightningcss_mbps",
        row.lightningcss_mbps,
        lightningcss.mbps,
    )?;
    require_close(
        "threshold_mbps",
        row.threshold_mbps,
        lightningcss.mbps + 1.0,
    )?;
    require_close(
        "admission_margin_mbps",
        row.admission_margin_mbps,
        track1.mbps - (lightningcss.mbps + 1.0),
    )?;
    if row.sample_count != track1.samples
        || row.sample_count != cssparser.samples
        || row.sample_count != lightningcss.samples
    {
        return Err("CSS L4 sample count does not match Criterion lanes".to_string());
    }
    validate_css_l4_retained_artifacts(row, workspace)?;
    validate_lightningcss_source_isolation(workspace)?;
    Ok(())
}

fn validate_skv13_css_stylesheet_selectors_report(
    report: &SkV13CssStylesheetSelectorsReport,
    criterion_root: &Path,
    workspace: &Path,
) -> Result<(), String> {
    let row = &report.rows[0];
    let track1 = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w2",
        "track1_generated_css_l4_stylesheet_selectors",
        117,
    )?;
    let golden = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w2",
        "track2_golden_stylesheet_selectors_oracle",
        117,
    )?;
    let lightningcss = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w2",
        "lightningcss_stylesheet_selectors_same_plane_fact_stream",
        117,
    )?;
    require_close("track1_mbps", row.track1_mbps, track1.mbps)?;
    require_close(
        "track2_or_oracle_mbps",
        row.track2_or_oracle_mbps,
        golden.mbps,
    )?;
    require_close(
        "lightningcss_mbps",
        row.lightningcss_mbps,
        lightningcss.mbps,
    )?;
    require_close(
        "threshold_mbps",
        row.threshold_mbps,
        lightningcss.mbps + 1.0,
    )?;
    require_close(
        "admission_margin_mbps",
        row.admission_margin_mbps,
        track1.mbps - (lightningcss.mbps + 1.0),
    )?;
    if row.sample_count != track1.samples
        || row.sample_count != golden.samples
        || row.sample_count != lightningcss.samples
    {
        return Err("W2 CSS sample count does not match Criterion lanes".to_string());
    }
    validate_css_l4_stylesheet_selectors_retained_artifacts(row, workspace)?;
    validate_stylesheet_selectors_lightningcss_source_isolation(workspace)?;
    Ok(())
}

fn validate_skv13_css_declaration_values_extended_report(
    report: &SkV13CssDeclarationValuesExtendedReport,
    criterion_root: &Path,
    workspace: &Path,
) -> Result<(), String> {
    let row = &report.rows[0];
    let track1 = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w3",
        "track1_generated_css_l4_decl_values_extended",
        305,
    )?;
    let cssparser = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w3",
        "track2_cssparser_decl_values_extended_oracle",
        305,
    )?;
    let lightningcss = read_css_l4_lane_in_group(
        criterion_root,
        "nonjson_css_l4_w3",
        "lightningcss_decl_values_extended_same_plane_fact_stream",
        305,
    )?;
    require_close("track1_mbps", row.track1_mbps, track1.mbps)?;
    require_close(
        "track2_or_oracle_mbps",
        row.track2_or_oracle_mbps,
        cssparser.mbps,
    )?;
    require_close(
        "lightningcss_mbps",
        row.lightningcss_mbps,
        lightningcss.mbps,
    )?;
    require_close(
        "threshold_mbps",
        row.threshold_mbps,
        lightningcss.mbps + 1.0,
    )?;
    require_close(
        "admission_margin_mbps",
        row.admission_margin_mbps,
        track1.mbps - (lightningcss.mbps + 1.0),
    )?;
    if row.sample_count != track1.samples
        || row.sample_count != cssparser.samples
        || row.sample_count != lightningcss.samples
    {
        return Err("W3 CSS sample count does not match Criterion lanes".to_string());
    }
    validate_css_l4_declaration_values_extended_retained_artifacts(row, workspace)?;
    validate_declaration_values_extended_lightningcss_source_isolation(workspace)?;
    Ok(())
}

fn read_css_l4_lane(criterion_root: &Path, lane: &str) -> Result<CssL4Lane, String> {
    read_css_l4_lane_in_group(criterion_root, "nonjson_css_l4", lane, 187)
}

fn read_css_l4_lane_in_group(
    criterion_root: &Path,
    group: &str,
    lane: &str,
    expected_bytes: u64,
) -> Result<CssL4Lane, String> {
    let lane_root = criterion_root.join(group).join(lane).join("new");
    let benchmark: Value = serde_json::from_str(
        &fs::read_to_string(lane_root.join("benchmark.json"))
            .map_err(|error| format!("{lane} benchmark.json missing: {error}"))?,
    )
    .map_err(|error| format!("{lane} benchmark.json invalid: {error}"))?;
    if benchmark
        .pointer("/throughput/Bytes")
        .and_then(Value::as_u64)
        != Some(expected_bytes)
    {
        return Err(format!(
            "{group}/{lane} does not report throughput.Bytes == {expected_bytes}"
        ));
    }
    let estimates: Value = serde_json::from_str(
        &fs::read_to_string(lane_root.join("estimates.json"))
            .map_err(|error| format!("{lane} estimates.json missing: {error}"))?,
    )
    .map_err(|error| format!("{lane} estimates.json invalid: {error}"))?;
    let mean_ns = estimates
        .pointer("/mean/point_estimate")
        .and_then(Value::as_f64)
        .filter(|value| value.is_finite() && *value > 0.0)
        .ok_or_else(|| format!("{lane} missing finite positive mean.point_estimate"))?;
    let sample: Value = serde_json::from_str(
        &fs::read_to_string(lane_root.join("sample.json"))
            .map_err(|error| format!("{lane} sample.json missing: {error}"))?,
    )
    .map_err(|error| format!("{lane} sample.json invalid: {error}"))?;
    let samples = sample
        .pointer("/iters")
        .and_then(Value::as_array)
        .map(|iters| iters.len() as u64)
        .ok_or_else(|| format!("{lane} missing sample iters"))?;
    if samples < 30 {
        return Err(format!("{lane} sample count {samples} below 30"));
    }
    Ok(CssL4Lane {
        mbps: expected_bytes as f64 * 8_000.0 / mean_ns,
        samples,
    })
}

fn validate_css_l4_retained_artifacts(
    row: &bbnf_bench::report::SkV12CssL4SotaRow,
    workspace: &Path,
) -> Result<(), String> {
    let track1_path = resolve_workspace_path(workspace, &row.track1_artifact);
    let cssparser_path = resolve_workspace_path(workspace, &row.cssparser_artifact_path);
    let lightning_path = resolve_workspace_path(workspace, &row.lightningcss_fact_artifact_path);
    let equality_dir = track1_path
        .parent()
        .ok_or_else(|| "CSS L4 Track 1 artifact has no parent directory".to_string())?;
    let strict_path = equality_dir.join("strict-equality.txt");
    let lightning_eq_path = equality_dir.join("lightningcss-strict-equality.txt");
    let track1 = fs::read(&track1_path)
        .map_err(|error| format!("failed to read {}: {error}", track1_path.display()))?;
    let cssparser = fs::read(&cssparser_path)
        .map_err(|error| format!("failed to read {}: {error}", cssparser_path.display()))?;
    let lightning = fs::read(&lightning_path)
        .map_err(|error| format!("failed to read {}: {error}", lightning_path.display()))?;
    if track1 != cssparser || track1 != lightning {
        return Err("CSS L4 retained fact streams differ".to_string());
    }
    if sha256_hex(&track1) != row.fact_stream_sha256 {
        return Err("CSS L4 fact_stream_sha256 mismatch".to_string());
    }
    let fact_text = std::str::from_utf8(&track1)
        .map_err(|error| format!("fact stream is not UTF-8: {error}"))?;
    for needle in [
        "row\tid=css_l4/declaration_values/direct_to_struct/main\tplane=css_l4_declaration_value_fact_stream",
        "source\tinput_fnv64=27240148e5780a54\tinput_bytes=187",
        "end\tdecls=7\ttokens=13\tstream_fnv64=285dd62f19dea4a8",
    ] {
        if !fact_text.contains(needle) {
            return Err(format!("CSS L4 fact stream missing `{needle}`"));
        }
    }
    validate_css_l4_equality_artifact(&strict_path, false)?;
    validate_css_l4_equality_artifact(&lightning_eq_path, true)?;
    if resolve_workspace_path(workspace, &row.lightningcss_artifact) != lightning_eq_path {
        return Err("CSS L4 lightningcss equality path mismatch".to_string());
    }
    Ok(())
}

fn validate_css_l4_stylesheet_selectors_retained_artifacts(
    row: &bbnf_bench::report::SkV13CssStylesheetSelectorsRow,
    workspace: &Path,
) -> Result<(), String> {
    let track1_path = resolve_workspace_path(workspace, &row.track1_artifact);
    let oracle_path = resolve_workspace_path(workspace, &row.oracle_artifact_path);
    let lightning_path = resolve_workspace_path(workspace, &row.lightningcss_fact_artifact_path);
    let equality_dir = track1_path
        .parent()
        .ok_or_else(|| "W2 CSS Track 1 artifact has no parent directory".to_string())?;
    let strict_path = equality_dir.join("strict-equality.txt");
    let lightning_eq_path = equality_dir.join("lightningcss-strict-equality.txt");
    let track1 = fs::read(&track1_path)
        .map_err(|error| format!("failed to read {}: {error}", track1_path.display()))?;
    let oracle = fs::read(&oracle_path)
        .map_err(|error| format!("failed to read {}: {error}", oracle_path.display()))?;
    let lightning = fs::read(&lightning_path)
        .map_err(|error| format!("failed to read {}: {error}", lightning_path.display()))?;
    if track1 != oracle || track1 != lightning {
        return Err("W2 CSS retained fact streams differ".to_string());
    }
    if sha256_hex(&track1) != row.fact_stream_sha256 {
        return Err("W2 CSS fact_stream_sha256 mismatch".to_string());
    }
    let fact_text = std::str::from_utf8(&track1)
        .map_err(|error| format!("W2 fact stream is not UTF-8: {error}"))?;
    for needle in [
        "row\tid=css_l4/stylesheet_and_selectors/direct_to_struct/main\tplane=css_l4_stylesheet_selector_fact_stream",
        "source\tinput_fnv64=b6ac6a6f4f0f0960\tinput_bytes=117",
        "end\trules=1\tselector_lists=1\tselectors=2\tselector_items=16\tdeclarations=1\tstream_fnv64=5ec8b16c78e94737",
    ] {
        if !fact_text.contains(needle) {
            return Err(format!("W2 CSS fact stream missing `{needle}`"));
        }
    }
    validate_css_l4_named_equality_artifact(
        &strict_path,
        "css_l4/stylesheet_and_selectors/direct_to_struct/main",
        "sk-v13-w2:fixture-fnv64-b6ac6a6f4f0f0960",
        false,
    )?;
    validate_css_l4_named_equality_artifact(
        &lightning_eq_path,
        "css_l4/stylesheet_and_selectors/direct_to_struct/main",
        "sk-v13-w2:fixture-fnv64-b6ac6a6f4f0f0960",
        true,
    )?;
    if resolve_workspace_path(workspace, &row.lightningcss_artifact) != lightning_eq_path {
        return Err("W2 CSS lightningcss equality path mismatch".to_string());
    }
    Ok(())
}

fn validate_css_l4_declaration_values_extended_retained_artifacts(
    row: &bbnf_bench::report::SkV13CssDeclarationValuesExtendedRow,
    workspace: &Path,
) -> Result<(), String> {
    let track1_path = resolve_workspace_path(workspace, &row.track1_artifact);
    let oracle_path = resolve_workspace_path(workspace, &row.oracle_artifact_path);
    let lightning_path = resolve_workspace_path(workspace, &row.lightningcss_fact_artifact_path);
    let equality_dir = track1_path
        .parent()
        .ok_or_else(|| "W3 CSS Track 1 artifact has no parent directory".to_string())?;
    let strict_path = equality_dir.join("strict-equality.txt");
    let lightning_eq_path = equality_dir.join("lightningcss-strict-equality.txt");
    let track1 = fs::read(&track1_path)
        .map_err(|error| format!("failed to read {}: {error}", track1_path.display()))?;
    let oracle = fs::read(&oracle_path)
        .map_err(|error| format!("failed to read {}: {error}", oracle_path.display()))?;
    let lightning = fs::read(&lightning_path)
        .map_err(|error| format!("failed to read {}: {error}", lightning_path.display()))?;
    if track1 != oracle || track1 != lightning {
        return Err("W3 CSS retained fact streams differ".to_string());
    }
    if sha256_hex(&track1) != row.fact_stream_sha256 {
        return Err("W3 CSS fact_stream_sha256 mismatch".to_string());
    }
    let fact_text = std::str::from_utf8(&track1)
        .map_err(|error| format!("W3 fact stream is not UTF-8: {error}"))?;
    for needle in [
        "row\tid=css_l4/declaration_values_extended/direct_to_struct/main\tplane=css_l4_declaration_value_extended_fact_stream",
        "source\tinput_fnv64=ffbf6baa300b8f39\tinput_bytes=305",
        "tok\tdecl=2\tidx=0\tdepth=1\tkind=function\tlexeme_hex=766172\tflags=normalized",
        "tok\tdecl=3\tidx=0\tdepth=0\tkind=function\tlexeme_hex=636f6c6f722d6d6978\tflags=normalized",
        "tok\tdecl=5\tidx=0\tdepth=0\tkind=url\tlexeme_hex=2f6173736574732f6d61736b2e737667\tflags=normalized",
        "end\tdecls=7\ttokens=43\tmax_depth=2\tstream_fnv64=364efb675d132e91",
    ] {
        if !fact_text.contains(needle) {
            return Err(format!("W3 CSS fact stream missing `{needle}`"));
        }
    }
    validate_css_l4_named_equality_artifact(
        &strict_path,
        "css_l4/declaration_values_extended/direct_to_struct/main",
        "sk-v13-w3:fixture-fnv64-ffbf6baa300b8f39",
        false,
    )?;
    validate_css_l4_named_equality_artifact(
        &lightning_eq_path,
        "css_l4/declaration_values_extended/direct_to_struct/main",
        "sk-v13-w3:fixture-fnv64-ffbf6baa300b8f39",
        true,
    )?;
    if resolve_workspace_path(workspace, &row.lightningcss_artifact) != lightning_eq_path {
        return Err("W3 CSS lightningcss equality path mismatch".to_string());
    }
    Ok(())
}

fn validate_css_l4_equality_artifact(path: &Path, lightningcss: bool) -> Result<(), String> {
    validate_css_l4_named_equality_artifact(
        path,
        "css_l4/declaration_values/direct_to_struct/main",
        "sk-v12-w1b-1:fixture-fnv64-27240148e5780a54",
        lightningcss,
    )
}

fn validate_css_l4_named_equality_artifact(
    path: &Path,
    row_id: &str,
    run_id: &str,
    lightningcss: bool,
) -> Result<(), String> {
    let text = fs::read_to_string(path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    let needles = [
        "status=pass".to_string(),
        format!("row_id={row_id}"),
        format!("run_id={run_id}"),
    ];
    for needle in needles {
        if !text.contains(&needle) {
            return Err(format!("{} missing `{needle}`", path.display()));
        }
    }
    if lightningcss
        && !text.contains("comparator=lightningcss-1.0.0-alpha.71:same-plane-source-sidecar")
    {
        return Err(format!(
            "{} missing lightningcss comparator",
            path.display()
        ));
    }
    Ok(())
}

fn validate_lightningcss_source_isolation(workspace: &Path) -> Result<(), String> {
    let source_path = workspace.join("crates/bbnf-bench/src/nonjson_css_l4.rs");
    let source = fs::read_to_string(&source_path)
        .map_err(|error| format!("failed to read {}: {error}", source_path.display()))?;
    let start = source
        .find("pub fn lightningcss_facts")
        .ok_or_else(|| "lightningcss_facts function not found".to_string())?;
    let rest = &source[start..];
    let end = rest
        .find("\npub fn assert_strict_equality")
        .ok_or_else(|| "lightningcss_facts function end not found".to_string())?;
    let body = &rest[..end];
    for forbidden in [
        " oracle_facts(",
        "ParserInput",
        "Parser::new",
        "StyleSheetParser",
        "cssparser::",
    ] {
        if body.contains(forbidden) {
            return Err(format!(
                "lightningcss_facts is coupled to cssparser via `{forbidden}`"
            ));
        }
    }
    Ok(())
}

fn validate_stylesheet_selectors_lightningcss_source_isolation(
    workspace: &Path,
) -> Result<(), String> {
    let source_path = workspace.join("crates/bbnf-bench/src/nonjson_css_l4.rs");
    let source = fs::read_to_string(&source_path)
        .map_err(|error| format!("failed to read {}: {error}", source_path.display()))?;
    let start = source
        .find("pub fn stylesheet_selectors_lightningcss_facts")
        .ok_or_else(|| "stylesheet_selectors_lightningcss_facts function not found".to_string())?;
    let rest = &source[start..];
    let end = rest
        .find("\npub fn assert_strict_equality")
        .ok_or_else(|| "stylesheet selectors lightningcss function end not found".to_string())?;
    let body = &rest[..end];
    for forbidden in [
        "stylesheet_selectors_track1_facts(",
        "runtime::generated_css_l4_stylesheet_selectors",
        "generated_css_l4_stylesheet_selectors",
    ] {
        if body.contains(forbidden) {
            return Err(format!(
                "stylesheet selectors lightningcss facts are coupled to Track 1 via `{forbidden}`"
            ));
        }
    }
    Ok(())
}

fn validate_declaration_values_extended_lightningcss_source_isolation(
    workspace: &Path,
) -> Result<(), String> {
    let source_path = workspace.join("crates/bbnf-bench/src/nonjson_css_l4.rs");
    let source = fs::read_to_string(&source_path)
        .map_err(|error| format!("failed to read {}: {error}", source_path.display()))?;
    let start = source
        .find("pub fn declaration_values_extended_lightningcss_facts")
        .ok_or_else(|| {
            "declaration_values_extended_lightningcss_facts function not found".to_string()
        })?;
    let rest = &source[start..];
    let end = rest
        .find("\npub fn assert_strict_equality")
        .ok_or_else(|| {
            "declaration-values-extended lightningcss function end not found".to_string()
        })?;
    let body = &rest[..end];
    for forbidden in [
        "declaration_values_extended_track1_facts(",
        "runtime::generated_css_l4_declaration_values_extended",
        "generated_css_l4_declaration_values_extended",
    ] {
        if body.contains(forbidden) {
            return Err(format!(
                "declaration-values-extended lightningcss facts are coupled to Track 1 via `{forbidden}`"
            ));
        }
    }
    Ok(())
}

fn resolve_workspace_path(workspace: &Path, value: &str) -> PathBuf {
    let path = PathBuf::from(value);
    if path.is_absolute() {
        path
    } else {
        workspace.join(path)
    }
}

fn require_close(name: &str, actual: f64, expected: f64) -> Result<(), String> {
    if !actual.is_finite() || (actual - expected).abs() > 0.01 {
        return Err(format!(
            "CSS L4 {name} mismatch: report {actual:.6}, criterion {expected:.6}"
        ));
    }
    Ok(())
}

fn sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{byte:02x}");
    }
    out
}

fn w0_parse_non_admission(outcome: Outcome) -> Outcome {
    match outcome {
        Outcome::IParityOracleFail
        | Outcome::JSchemaFail
        | Outcome::KSimdParityHashFail
        | Outcome::LSimdThroughputFail
        | Outcome::MMemoryResidencyFail => outcome,
        _ => Outcome::SSubstrateGuardNonAdmission,
    }
}

#[derive(Debug, Clone)]
struct RunFacts {
    run_id: String,
    host_triple: String,
    build_flags: String,
    feature_mask: String,
}

impl RunFacts {
    fn probe(criterion_root: &Path, fixture_names: &BTreeSet<&str>) -> Self {
        let host_triple = rustc_host_triple()
            .unwrap_or_else(|| format!("{}-{}", std::env::consts::ARCH, std::env::consts::OS));
        let rustflags = env::var("RUSTFLAGS").unwrap_or_default();
        let target_cpu = parse_target_cpu(&rustflags).unwrap_or_else(|| "default".to_string());
        Self {
            run_id: format!(
                "sk-v9-open:criterion-fnv64-{}",
                criterion_fingerprint(criterion_root, fixture_names)
            ),
            host_triple: host_triple.clone(),
            build_flags: format!(
                "profile={};rustflags={};target_cpu={target_cpu}",
                env::var("PROFILE").unwrap_or_else(|_| "bench".to_string()),
                if rustflags.is_empty() {
                    "<empty>"
                } else {
                    &rustflags
                }
            ),
            feature_mask: format!(
                "arch={};os={};simd={:?};target_cpu={target_cpu}",
                std::env::consts::ARCH,
                std::env::consts::OS,
                bbnf_simd::active_backend()
            ),
        }
    }
}

fn w0_telemetry(
    corpus: &str,
    workload: &str,
    output_plane: &str,
    bytes: u64,
    track1_ns: Option<f64>,
    competitors: &ComparatorSet,
    rows: &[RowMetadata],
    run_facts: &RunFacts,
    bench_name: &str,
) -> SkV8Telemetry {
    let row_id = format!("json/{corpus}/{workload}/main");
    let metadata = rows
        .iter()
        .find(|row| row.workload == workload && row.track == TrackTag::Track1Generated);
    let build_flags = metadata
        .map(|row| {
            format!(
                "profile={};rustflags={};target_cpu={}",
                row.profile,
                if row.rustflags.is_empty() {
                    "<empty>"
                } else {
                    row.rustflags.as_str()
                },
                row.target_cpu
            )
        })
        .unwrap_or_else(|| run_facts.build_flags.clone());
    let host_triple = metadata
        .map(|row| {
            format!(
                "{};arch={};cpu={}",
                run_facts.host_triple, row.cpu_arch, row.cpu_model
            )
        })
        .unwrap_or_else(|| run_facts.host_triple.clone());
    let feature_mask = metadata
        .map(|row| {
            if row.feature_mask == "n/a" {
                run_facts.feature_mask.clone()
            } else {
                row.feature_mask.clone()
            }
        })
        .unwrap_or_else(|| run_facts.feature_mask.clone());
    let sample_count = metadata
        .map(|row| row.sample_size as u64)
        .unwrap_or_default();
    let sample_cost = track1_ns
        .filter(|ns| bytes > 0 && ns.is_finite() && *ns > 0.0)
        .map(|ns| {
            format!(
                "ns_per_byte={:.6};track1_ns={ns:.2};bytes={bytes}",
                ns / bytes as f64
            )
        })
        .unwrap_or_else(|| "n/a".to_string());
    let (substrate_surface, structural_projection_status, substrate_cardinality) =
        substrate_facts(workload);
    SkV8Telemetry {
        row_id: row_id.clone(),
        grammar_id: "json".to_string(),
        domain: "json_bench".to_string(),
        measured_validation_path: "view-boundary".to_string(),
        profile_artifact: w0_profile_artifact(corpus, bench_name),
        sample_cost,
        sample_count,
        build_flags,
        host_triple,
        feature_mask,
        costfacts_rule_id: "none:pre-W1".to_string(),
        costfacts_chosen_shape: "none:pre-W1".to_string(),
        costfacts_rejected_alternative_ids: vec!["none:pre-W1".to_string()],
        redress_entry: "none".to_string(),
        wave_id: "SK-V9-open".to_string(),
        run_id: run_facts.run_id.clone(),
        sk_v9_open_delta: "baseline".to_string(),
        substrate_surface: substrate_surface.to_string(),
        structural_projection_status: structural_projection_status.to_string(),
        substrate_cardinality: substrate_cardinality.to_string(),
        same_wave_consumer_class: "gate_only".to_string(),
        track2_independence_status: "independent_verified".to_string(),
        diagnostic_nonproducer_status: "structural_scan+masking_probes+pmu+cycles:nonproducer"
            .to_string(),
        comparators: w0_comparator_evidence(corpus, workload, output_plane, competitors),
    }
}

fn w0_comparator_evidence(
    corpus: &str,
    workload: &str,
    output_plane: &str,
    comparators: &ComparatorSet,
) -> Vec<SkV8ComparatorEvidence> {
    let native_plane = if workload == "parse_only" {
        "DOM"
    } else {
        output_plane
    };
    let (sonic_bench, serde_bench, lossy_bench) = match workload {
        "parse_only" => ("sonic_rs_anchor", "serde_json", Some("sonic_rs_lossy")),
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
        _ => ("sonic_rs_anchor", "serde_json", None),
    };
    let mut evidence = vec![
        comparator_evidence(
            "sonic_rs_strict",
            native_plane,
            "strict",
            "same-run-native",
            "n/a",
            comparators.sonic_strict_mbps,
            &format!("criterion:json_{corpus}/{sonic_bench}/new/estimates.json"),
        ),
        comparator_evidence(
            "serde_json",
            native_plane,
            "strict",
            "same-run-native",
            "n/a",
            comparators.serde_json_mbps,
            &format!("criterion:json_{corpus}/{serde_bench}/new/estimates.json"),
        ),
    ];
    if let Some(lossy_bench) = lossy_bench.filter(|_| comparators.sonic_lossy_mbps.is_some()) {
        evidence.push(comparator_evidence(
            "sonic_rs_lossy",
            native_plane,
            "permissive",
            "same-run-native",
            "n/a",
            comparators.sonic_lossy_mbps,
            &format!("criterion:json_{corpus}/{lossy_bench}/new/estimates.json"),
        ));
    }
    for (id, value) in [
        ("simdjson_dom", comparators.simdjson_dom_mbps),
        ("simdjson_ondemand", comparators.simdjson_ondemand_mbps),
        ("yyjson_default", comparators.yyjson_default_mbps),
        ("asmjson_swar", comparators.asmjson_swar_mbps),
        ("asmjson_avx512", comparators.asmjson_avx512_mbps),
        ("rapidjson_default", comparators.rapidjson_default_mbps),
    ] {
        let (freshness, source) = if value.is_some() {
            (
                "historical:sk-v7-sidecar-profile".to_string(),
                format!("sidecar-profile:sk-v7-cpp:{corpus}:{id}"),
            )
        } else {
            (
                format!("absent:not-collected-for-{workload}"),
                format!("absence:w0:{corpus}:{workload}:{id}"),
            )
        };
        evidence.push(comparator_evidence(
            id, "DOM", "strict", &freshness, &freshness, value, &source,
        ));
    }
    evidence
}

fn comparator_evidence(
    comparator_id: &str,
    comparator_plane: &str,
    comparator_strictness: &str,
    comparator_freshness: &str,
    sidecar_freshness: &str,
    value_mbps: Option<f64>,
    source_artifact: &str,
) -> SkV8ComparatorEvidence {
    SkV8ComparatorEvidence {
        comparator_id: comparator_id.to_string(),
        comparator_plane: comparator_plane.to_string(),
        comparator_strictness: comparator_strictness.to_string(),
        comparator_freshness: comparator_freshness.to_string(),
        sidecar_freshness: sidecar_freshness.to_string(),
        value_mbps,
        source_artifact: source_artifact.to_string(),
    }
}

fn substrate_facts(workload: &str) -> (&'static str, &'static str, &'static str) {
    match workload {
        "parse_only" => (
            "borrowed_view_over_offset_tape",
            "discarded_after_capacity",
            "one",
        ),
        "direct_to_struct" => ("sink_only_digest", "n/a", "zero_or_inert"),
        "real_typed_struct" => ("typed_direct_projection", "n/a", "zero_or_inert"),
        _ => ("unknown", "unknown", "unknown"),
    }
}

fn w0_profile_artifact(corpus: &str, bench_name: &str) -> String {
    format!("criterion-slope-profile:json_{corpus}/{bench_name}/new/estimates.json")
}

fn w0_hot_leaf(corpus: &str, bench_name: &str) -> String {
    format!(
        "{};hot-leaf=criterion-slope-profile;row=json/{corpus}/{}/main",
        w0_profile_artifact(corpus, bench_name),
        workload_for_bench(bench_name)
    )
}

fn workload_for_bench(bench_name: &str) -> &str {
    match bench_name {
        "track1_direct_to_struct" => "direct_to_struct",
        "track1_real_typed_struct" => "real_typed_struct",
        _ => "parse_only",
    }
}

fn rustc_host_triple() -> Option<String> {
    command_output("rustc", &["-vV"]).and_then(|text| {
        text.lines()
            .find_map(|line| line.strip_prefix("host:"))
            .map(str::trim)
            .map(str::to_string)
    })
}

fn command_output(program: &str, args: &[&str]) -> Option<String> {
    let output = Command::new(program).args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    String::from_utf8(output.stdout)
        .ok()
        .map(|text| text.trim().to_string())
}

fn parse_target_cpu(rustflags: &str) -> Option<String> {
    let mut words = rustflags.split_whitespace();
    while let Some(word) = words.next() {
        if word == "-C" {
            if let Some(value) = words
                .next()
                .and_then(|next| next.strip_prefix("target-cpu="))
            {
                return Some(value.to_string());
            }
        }
        if let Some(value) = word.strip_prefix("-Ctarget-cpu=") {
            return Some(value.to_string());
        }
    }
    None
}

fn criterion_fingerprint(root: &Path, fixture_names: &BTreeSet<&str>) -> String {
    let mut files = Vec::new();
    collect_criterion_inputs(root, root, fixture_names, &mut files);
    files.sort();
    let mut hash = FNV_OFFSET_BASIS;
    for relative in files {
        hash = fnv1a(hash, relative.as_os_str().to_string_lossy().as_bytes());
        hash = fnv1a(hash, b"\0");
        if let Ok(bytes) = fs::read(root.join(&relative)) {
            hash = fnv1a(hash, &bytes);
        }
        hash = fnv1a(hash, b"\0");
    }
    format!("{hash:016x}")
}

fn collect_criterion_inputs(
    root: &Path,
    dir: &Path,
    fixture_names: &BTreeSet<&str>,
    files: &mut Vec<PathBuf>,
) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_criterion_inputs(root, &path, fixture_names, files);
            continue;
        }
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if matches!(name, "estimates.json" | "metadata.toml") {
            if let Ok(relative) = path.strip_prefix(root) {
                if is_w0_criterion_input(relative, fixture_names) {
                    files.push(relative.to_path_buf());
                }
            }
        }
    }
}

fn is_w0_criterion_input(relative: &Path, fixture_names: &BTreeSet<&str>) -> bool {
    let parts: Vec<_> = relative
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect();
    match parts.as_slice() {
        ["simd_structural_scan", bench, "metadata.toml"] => {
            bench.strip_suffix("_simd").is_some_and(|corpus| {
                fixture_names.contains(corpus)
                    && sk_v8_open_baseline(&format!("json/{corpus}/parse_only/main")).is_some()
            })
        }
        ["simd_structural_scan", "canada_simd", "new", "estimates.json"] => {
            fixture_names.contains("canada")
                && sk_v8_open_baseline("json/canada/parse_only/main").is_some()
        }
        [group, bench, "metadata.toml"] | [group, bench, "new", "estimates.json"] => {
            group.strip_prefix("json_").is_some_and(|corpus| {
                fixture_names.contains(corpus)
                    && w0_workload_for_bench(bench).is_some_and(|workload| {
                        sk_v8_open_baseline(&format!("json/{corpus}/{workload}/main")).is_some()
                    })
            })
        }
        _ => false,
    }
}

fn w0_workload_for_bench(bench: &str) -> Option<&'static str> {
    match bench {
        "track1_generated" | "track2_handcoded" | "sonic_rs_anchor" | "sonic_rs_lossy"
        | "simd_json_borrowed" | "simd_json_owned" | "serde_json" => Some("parse_only"),
        "track1_direct_to_struct"
        | "track2_direct_to_struct"
        | "sonic_rs_direct_to_struct"
        | "serde_json_direct_to_struct" => Some("direct_to_struct"),
        "track1_real_typed_struct"
        | "track2_real_typed_struct"
        | "sonic_rs_real_typed_struct"
        | "serde_json_real_typed_struct" => Some("real_typed_struct"),
        _ => None,
    }
}

const FNV_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
const FNV_PRIME: u64 = 0x100000001b3;

fn fnv1a(mut hash: u64, bytes: &[u8]) -> u64 {
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

fn direct_workload_signal(
    correctness_ok: bool,
    outcome: Option<Outcome>,
    w0_no_admission_clamp: bool,
    w2_reclaimed: bool,
    w10_residual: bool,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> String {
    if !correctness_ok {
        return "FAIL digest mismatch".to_string();
    }
    if w2_reclaimed {
        let track1 = throughput_mbps(bytes, track1_ns);
        let track2 = throughput_mbps(bytes, track2_ns);
        let sonic = throughput_mbps(bytes, sonic_ns);
        return format!(
            "PASS W2 direct row reclamation; strict measured-row contract; Track 1 {}, Track 2 {}, sonic {} Mbps",
            format_mbps(track1),
            format_mbps(track2),
            format_mbps(sonic)
        );
    }
    if w10_residual {
        let track1 = throughput_mbps(bytes, track1_ns);
        let track2 = throughput_mbps(bytes, track2_ns);
        let sonic = throughput_mbps(bytes, sonic_ns);
        return format!(
            "PASS W10 direct residual admission; strict measured-row contract; Track 1 {}, Track 2 {}, sonic {} Mbps",
            format_mbps(track1),
            format_mbps(track2),
            format_mbps(sonic)
        );
    }
    if w0_no_admission_clamp {
        let track1 = throughput_mbps(bytes, track1_ns);
        let track2 = throughput_mbps(bytes, track2_ns);
        let sonic = throughput_mbps(bytes, sonic_ns);
        return format!(
            "NO-GO W0 no-admission clamp; fresh direct guard passes are not behavior evidence in W0; Track 1 {}, Track 2 {}, sonic {} Mbps",
            format_mbps(track1),
            format_mbps(track2),
            format_mbps(sonic)
        );
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DirectRowDecision {
    outcome: Option<Outcome>,
    w0_clamped: bool,
    w2_reclaimed: bool,
    w10_residual: bool,
}

fn direct_row_decision(
    corpus: &str,
    classified: Option<Outcome>,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
) -> DirectRowDecision {
    let row_id = format!("json/{corpus}/direct_to_struct/main");
    if sk_v8_open_baseline(&row_id).is_some_and(|baseline| baseline.verdict == "NO-GO") {
        if classified.is_none() && w2_direct_reclamation_passes(corpus, bytes, track1_ns, track2_ns)
        {
            return DirectRowDecision {
                outcome: None,
                w0_clamped: false,
                w2_reclaimed: true,
                w10_residual: false,
            };
        }
        if matches!(classified, None | Some(Outcome::NDirectProjectionFailure))
            && w10_direct_residual_passes(corpus, bytes, track1_ns, track2_ns)
        {
            return DirectRowDecision {
                outcome: None,
                w0_clamped: false,
                w2_reclaimed: false,
                w10_residual: true,
            };
        }
        if classified.is_none() {
            return DirectRowDecision {
                outcome: Some(Outcome::NDirectProjectionFailure),
                w0_clamped: true,
                w2_reclaimed: false,
                w10_residual: false,
            };
        }
    }
    DirectRowDecision {
        outcome: classified,
        w0_clamped: false,
        w2_reclaimed: false,
        w10_residual: false,
    }
}

fn w2_direct_reclamation_passes(
    corpus: &str,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
) -> bool {
    let Some(floor) = w2_direct_reclamation_floor(corpus) else {
        return false;
    };
    direct_row_passes_floor(bytes, track1_ns, track2_ns, floor)
}

fn w10_direct_residual_passes(
    corpus: &str,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
) -> bool {
    let Some(floor) = w10_direct_residual_floor(corpus) else {
        return false;
    };
    direct_row_passes_floor(bytes, track1_ns, track2_ns, floor)
}

fn direct_row_passes_floor(
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    floor: f64,
) -> bool {
    let (Some(track1), Some(track2)) = (
        throughput_mbps(bytes, track1_ns),
        throughput_mbps(bytes, track2_ns),
    ) else {
        return false;
    };
    track1 >= floor && track2 >= floor
}

fn w2_direct_reclamation_floor(corpus: &str) -> Option<f64> {
    match corpus {
        "apache_builds" => Some(10_020.0),
        "numbers" => Some(11_788.0),
        _ => None,
    }
}

fn w10_direct_residual_floor(corpus: &str) -> Option<f64> {
    match corpus {
        "instruments" => Some(11_086.0),
        _ => None,
    }
}

fn mark_w2_direct_reclamation(row: &mut TelemetryRow) {
    row.strictness = "strict".to_string();
    row.parse_utf8 = "measured-row".to_string();
    row.flaw_probe =
        "generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 measured in row"
            .to_string();
    row.sk_v8.measured_validation_path = "measured-row".to_string();
    row.sk_v8.same_wave_consumer_class = "gate_json_direct_contract".to_string();
    row.sk_v8.redress_entry = "REDRESS-101".to_string();
    row.sk_v8.wave_id = "SK-V10-W2".to_string();
    row.sk_v8.sk_v9_open_delta = "direct-reclaimed".to_string();
}

fn mark_w10_direct_residual(row: &mut TelemetryRow) {
    row.strictness = "strict".to_string();
    row.parse_utf8 = "measured-row".to_string();
    row.flaw_probe =
        "generated Track 1 SinkOnly vs independent hand Track 2 SinkOnly; UTF-8 measured in row"
            .to_string();
    row.sk_v8.measured_validation_path = "measured-row".to_string();
    row.sk_v8.same_wave_consumer_class = "gate_json_direct_contract".to_string();
    row.sk_v8.redress_entry = "REDRESS-109".to_string();
    row.sk_v8.wave_id = "SK-V10-W10".to_string();
    row.sk_v8.sk_v9_open_delta = "direct-residual".to_string();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RealTypedRowDecision {
    outcome: Option<Outcome>,
    w6_github_events_added: bool,
}

fn real_typed_row_decision(
    corpus: &str,
    classified: Option<Outcome>,
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> RealTypedRowDecision {
    if corpus == "github_events" {
        if classified.is_none()
            && w6_github_events_typed_passes(bytes, track1_ns, track2_ns, sonic_ns)
        {
            return RealTypedRowDecision {
                outcome: None,
                w6_github_events_added: true,
            };
        }
        return RealTypedRowDecision {
            outcome: Some(classified.unwrap_or(Outcome::NDirectProjectionFailure)),
            w6_github_events_added: false,
        };
    }
    RealTypedRowDecision {
        outcome: classified,
        w6_github_events_added: false,
    }
}

fn w6_github_events_typed_passes(
    bytes: u64,
    track1_ns: Option<f64>,
    track2_ns: Option<f64>,
    sonic_ns: Option<f64>,
) -> bool {
    let track1 = throughput_mbps(bytes, track1_ns);
    let track2 = throughput_mbps(bytes, track2_ns);
    let sonic = throughput_mbps(bytes, sonic_ns);
    let (Some(track1), Some(track2), Some(sonic)) = (track1, track2, sonic) else {
        return false;
    };
    let floor = (sonic / 1.10).ceil();
    track1 >= floor && track2 >= floor
}

fn mark_w6_github_events_typed_admission(row: &mut TelemetryRow) {
    row.strictness = "strict".to_string();
    row.parse_utf8 = "measured-row".to_string();
    row.flaw_probe =
        "generated Track 1 typed product vs independent serde Track 2/oracle; UTF-8 measured in row"
            .to_string();
    row.signal = format!(
        "PASS W6 github_events root typed admission; Track 1 {}, Track 2 oracle {}, sonic {} Mbps",
        format_mbps(row.track1_mbps),
        format_mbps(row.track2_mbps),
        format_mbps(row.competitors.sonic_strict_mbps)
    );
    row.sk_v8.measured_validation_path = "measured-row".to_string();
    row.sk_v8.same_wave_consumer_class = "gate_json_typed_contract".to_string();
    row.sk_v8.redress_entry = "REDRESS-105".to_string();
    row.sk_v8.wave_id = "SK-V10-W6".to_string();
    row.sk_v8.sk_v9_open_delta = "typed-row-added".to_string();
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
            "NO-GO generated typed output or Track 2 oracle missed sonic-rs / 1.10 floor; correctness PASS; Track 1 {}, Track 2 oracle {}, sonic {} Mbps",
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

fn parse_comparators(bytes: u64, corpus: &str, estimates: &Estimates) -> ComparatorSet {
    let sidecar = sidecar_comparators(corpus);
    ComparatorSet {
        sonic_strict_mbps: throughput_mbps(bytes, estimates.sonic),
        sonic_lossy_mbps: throughput_mbps(bytes, estimates.sonic_lossy),
        simdjson_dom_mbps: sidecar.simdjson_dom_mbps,
        simdjson_ondemand_mbps: sidecar.simdjson_ondemand_mbps,
        yyjson_default_mbps: sidecar.yyjson_default_mbps,
        asmjson_swar_mbps: sidecar.asmjson_swar_mbps,
        asmjson_avx512_mbps: sidecar.asmjson_avx512_mbps,
        rapidjson_default_mbps: sidecar.rapidjson_default_mbps,
        serde_json_mbps: throughput_mbps(bytes, estimates.serde_json),
    }
}

fn direct_comparators(bytes: u64, estimates: &Estimates) -> ComparatorSet {
    ComparatorSet {
        sonic_strict_mbps: throughput_mbps(bytes, estimates.direct_sonic),
        serde_json_mbps: throughput_mbps(bytes, estimates.direct_serde),
        ..ComparatorSet::default()
    }
}

fn real_typed_comparators(bytes: u64, estimates: &Estimates) -> ComparatorSet {
    ComparatorSet {
        sonic_strict_mbps: throughput_mbps(bytes, estimates.real_typed_sonic),
        serde_json_mbps: throughput_mbps(bytes, estimates.real_typed_serde),
        ..ComparatorSet::default()
    }
}

fn sidecar_comparators(corpus: &str) -> ComparatorSet {
    let mut comparators = ComparatorSet::default();
    comparators.simdjson_dom_mbps = sidecar_mib_to_mbps(match corpus {
        "twitter" => Some(2923.0),
        "citm_catalog" => Some(4270.0),
        "canada" => Some(1370.0),
        "apache_builds" => Some(4292.9),
        "github_events" => Some(4725.3),
        "update_center" => Some(3646.7),
        "mesh" => Some(1122.2),
        "random" => Some(2460.1),
        "distinct_values" => Some(2720.7),
        "unicode_basic" => Some(1940.1),
        "unicode_escapes" => Some(671.9),
        "unicode_mixed" => Some(1567.5),
        "y_string_unicode" => Some(1624.3),
        _ => None,
    });
    comparators.yyjson_default_mbps = sidecar_mib_to_mbps(match corpus {
        "twitter" => Some(3687.0),
        "citm_catalog" => Some(2498.0),
        "canada" => Some(1550.0),
        "apache_builds" => Some(1940.0),
        "github_events" => Some(2554.0),
        "update_center" => Some(2210.0),
        _ => None,
    });
    comparators.rapidjson_default_mbps = sidecar_mib_to_mbps(match corpus {
        "twitter" => Some(479.2),
        "citm_catalog" => Some(805.8),
        "canada" => Some(618.3),
        "apache_builds" => Some(470.2),
        "instruments" => Some(891.3),
        "random" => Some(420.3),
        _ => None,
    });
    comparators
}

fn sidecar_mib_to_mbps(value: Option<f64>) -> Option<f64> {
    value.map(|mib_per_sec| mib_per_sec / 0.1192)
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
        "{corpus} peak RSS subprocess probes: bbnf<={}, S anchor {}<={}.",
        format_rss_mib(bbnf?),
        anchor.unwrap_or("competitor"),
        format_rss_mib(competitor?)
    ))
}

fn format_rss_mib(bytes: u64) -> String {
    let bucket_bytes = 2 * 1_048_576;
    let bucket_mib = bytes.div_ceil(bucket_bytes) * 2;
    format!("{bucket_mib} MiB")
}

#[derive(Default)]
struct Estimates {
    track1: Option<f64>,
    track2: Option<f64>,
    sonic: Option<f64>,
    sonic_lossy: Option<f64>,
    simd_borrowed: Option<f64>,
    simd_owned: Option<f64>,
    serde_json: Option<f64>,
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
            && self.sonic_lossy.is_some()
            && self.serde_json.is_some()
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

fn read_metadata_rows(group: &Path) -> Result<Vec<RowMetadata>, String> {
    let mut rows = Vec::new();
    for bench in [
        "track1_generated",
        "track2_handcoded",
        "sonic_rs_anchor",
        "sonic_rs_lossy",
        "simd_json_borrowed",
        "simd_json_owned",
        "serde_json",
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
    {
        let path = group.join(bench).join("metadata.toml");
        if !path.exists() {
            continue;
        }
        let text = fs::read_to_string(&path)
            .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
        let row = toml::from_str::<RowMetadata>(&text)
            .map_err(|error| format!("malformed {}: {error}", path.display()))?;
        rows.push(row);
    }
    Ok(rows)
}

#[derive(Clone, Copy)]
struct MetadataSpec {
    label: &'static str,
    track: TrackTag,
    workload: &'static str,
    materialisation: &'static str,
    competitor_crate: Option<&'static str>,
    competitor_version: Option<&'static str>,
    strictness: &'static str,
    output_plane: &'static str,
}

fn validate_w0_capture_metadata(
    fixture: &str,
    input_sha256: &str,
    input_bytes: u64,
    real_typed_expected: bool,
    rows: &[RowMetadata],
) -> Result<(), String> {
    if rows.is_empty() {
        return Err("missing Criterion metadata rows".to_string());
    }
    let capture = CaptureMetadata::from_row(&rows[0]);
    for row in rows {
        if !row.required_fields_present() {
            return Err(format!(
                "{} has missing required metadata fields",
                row.api_symbol
            ));
        }
        if row.input_sha256 != input_sha256 {
            return Err(format!(
                "{} {} metadata has input hash {}, expected {}",
                fixture, row.api_symbol, row.input_sha256, input_sha256
            ));
        }
        if row.input_bytes != input_bytes {
            return Err(format!(
                "{} {} metadata has {} input bytes, expected {}",
                fixture, row.api_symbol, row.input_bytes, input_bytes
            ));
        }
        if row.profile != "bench"
            || row.rustflags != "-C target-cpu=native"
            || row.target_cpu != "native"
        {
            return Err(format!(
                "{} has unsupported native W0 capture policy",
                row.api_symbol
            ));
        }
        capture.validate_same_capture(row)?;
    }
    for spec in required_metadata_specs(real_typed_expected) {
        if !rows.iter().any(|row| metadata_matches_spec(row, spec)) {
            return Err(format!("missing coherent metadata for {}", spec.label));
        }
    }
    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ReportCaptureIdentity {
    cpu_model: String,
    cpu_arch: String,
    os_kernel: String,
    rustflags: String,
    target_cpu: String,
    profile: String,
    bbnf_commit: String,
}

impl ReportCaptureIdentity {
    fn from_row(row: &RowMetadata) -> Self {
        Self {
            cpu_model: row.cpu_model.clone(),
            cpu_arch: row.cpu_arch.clone(),
            os_kernel: row.os_kernel.clone(),
            rustflags: row.rustflags.clone(),
            target_cpu: row.target_cpu.clone(),
            profile: row.profile.clone(),
            bbnf_commit: row.bbnf_commit.clone(),
        }
    }
}

fn validate_report_capture_identity(
    expected: &mut Option<ReportCaptureIdentity>,
    fixture: &str,
    row: &RowMetadata,
) -> Result<(), String> {
    let current = ReportCaptureIdentity::from_row(row);
    match expected {
        Some(expected) if expected != &current => Err(format!(
            "{fixture} metadata is from a different report-wide capture"
        )),
        Some(_) => Ok(()),
        slot @ None => {
            *slot = Some(current);
            Ok(())
        }
    }
}

fn w0_real_typed_metadata_expected(fixture: &str) -> bool {
    sk_v8_open_baseline(&format!("json/{fixture}/real_typed_struct/main")).is_some()
        || fixture == "github_events"
}

#[derive(Clone)]
struct CaptureMetadata<'a> {
    cpu_model: &'a str,
    cpu_arch: &'a str,
    os_kernel: &'a str,
    rustflags: &'a str,
    target_cpu: &'a str,
    profile: &'a str,
    bbnf_commit: &'a str,
    warmup_samples: u32,
    warmup_time_s: f64,
    sample_size: u32,
    measurement_time_s: f64,
    confidence_interval: f64,
    outlier_rejection: &'a str,
    statistical_method: &'a str,
}

impl<'a> CaptureMetadata<'a> {
    fn from_row(row: &'a RowMetadata) -> Self {
        Self {
            cpu_model: &row.cpu_model,
            cpu_arch: &row.cpu_arch,
            os_kernel: &row.os_kernel,
            rustflags: &row.rustflags,
            target_cpu: &row.target_cpu,
            profile: &row.profile,
            bbnf_commit: &row.bbnf_commit,
            warmup_samples: row.warmup_samples,
            warmup_time_s: row.warmup_time_s,
            sample_size: row.sample_size,
            measurement_time_s: row.measurement_time_s,
            confidence_interval: row.confidence_interval,
            outlier_rejection: &row.outlier_rejection,
            statistical_method: &row.statistical_method,
        }
    }

    fn validate_same_capture(&self, row: &RowMetadata) -> Result<(), String> {
        if self.cpu_model != row.cpu_model
            || self.cpu_arch != row.cpu_arch
            || self.os_kernel != row.os_kernel
            || self.rustflags != row.rustflags
            || self.target_cpu != row.target_cpu
            || self.profile != row.profile
            || self.bbnf_commit != row.bbnf_commit
            || self.warmup_samples != row.warmup_samples
            || !same_f64(self.warmup_time_s, row.warmup_time_s)
            || self.sample_size != row.sample_size
            || !same_f64(self.measurement_time_s, row.measurement_time_s)
            || !same_f64(self.confidence_interval, row.confidence_interval)
            || self.outlier_rejection != row.outlier_rejection
            || self.statistical_method != row.statistical_method
        {
            return Err(format!(
                "{} metadata is from a mixed Criterion capture",
                row.api_symbol
            ));
        }
        Ok(())
    }
}

fn same_f64(left: f64, right: f64) -> bool {
    left.is_finite() && right.is_finite() && (left - right).abs() <= f64::EPSILON
}

fn required_metadata_specs(real_typed_expected: bool) -> Vec<MetadataSpec> {
    let mut specs = vec![
        spec(
            "track1_generated",
            TrackTag::Track1Generated,
            "parse_only",
            "typed_root_over_tape",
            None,
            None,
            "deferred",
            "borrowed view over offset tape",
        ),
        spec(
            "track2_handcoded",
            TrackTag::Track2Handcoded,
            "parse_only",
            "typed_root_over_tape",
            None,
            None,
            "deferred",
            "borrowed view over offset tape",
        ),
        spec(
            "sonic_rs_anchor",
            TrackTag::Competitor,
            "parse_only",
            "eager_typed",
            Some("sonic-rs"),
            Some("0.5.8"),
            "strict",
            "DOM",
        ),
        spec(
            "sonic_rs_lossy",
            TrackTag::Competitor,
            "parse_only",
            "eager_typed_lossy",
            Some("sonic-rs"),
            Some("0.5.8"),
            "permissive",
            "DOM",
        ),
        spec(
            "simd_json_borrowed",
            TrackTag::Competitor,
            "parse_only",
            "borrowed",
            Some("simd-json"),
            Some("0.13.11"),
            "strict",
            "DOM",
        ),
        spec(
            "simd_json_owned",
            TrackTag::Competitor,
            "parse_only",
            "owned",
            Some("simd-json"),
            Some("0.13.11"),
            "strict",
            "DOM",
        ),
        spec(
            "serde_json",
            TrackTag::Competitor,
            "parse_only",
            "eager_owned",
            Some("serde_json"),
            Some("workspace"),
            "strict",
            "DOM",
        ),
        spec(
            "track1_direct_to_struct",
            TrackTag::Track1Generated,
            "direct_to_struct",
            "direct_to_struct",
            None,
            None,
            "deferred",
            "digest",
        ),
        spec(
            "track2_direct_to_struct",
            TrackTag::Track2Handcoded,
            "direct_to_struct",
            "direct_to_struct",
            None,
            None,
            "deferred",
            "digest",
        ),
        spec(
            "sonic_rs_direct_to_struct",
            TrackTag::Competitor,
            "direct_to_struct",
            "direct_to_struct",
            Some("sonic-rs"),
            Some("0.5.8"),
            "strict",
            "digest",
        ),
        spec(
            "serde_json_direct_to_struct",
            TrackTag::Competitor,
            "direct_to_struct",
            "direct_to_struct",
            Some("serde_json"),
            Some("workspace"),
            "strict",
            "digest",
        ),
    ];
    if real_typed_expected {
        specs.extend([
            spec(
                "track1_real_typed_struct",
                TrackTag::Track1Generated,
                "real_typed_struct",
                "real_typed_struct",
                None,
                None,
                "deferred",
                "typed direct",
            ),
            spec(
                "track2_real_typed_struct",
                TrackTag::Track2Handcoded,
                "real_typed_struct",
                "real_typed_struct",
                None,
                None,
                "deferred",
                "typed direct",
            ),
            spec(
                "sonic_rs_real_typed_struct",
                TrackTag::Competitor,
                "real_typed_struct",
                "real_typed_struct",
                Some("sonic-rs"),
                Some("0.5.8"),
                "strict",
                "typed direct",
            ),
            spec(
                "serde_json_real_typed_struct",
                TrackTag::Competitor,
                "real_typed_struct",
                "real_typed_struct",
                Some("serde_json"),
                Some("workspace"),
                "strict",
                "typed direct",
            ),
        ]);
    }
    specs
}

fn spec(
    label: &'static str,
    track: TrackTag,
    workload: &'static str,
    materialisation: &'static str,
    competitor_crate: Option<&'static str>,
    competitor_version: Option<&'static str>,
    strictness: &'static str,
    output_plane: &'static str,
) -> MetadataSpec {
    MetadataSpec {
        label,
        track,
        workload,
        materialisation,
        competitor_crate,
        competitor_version,
        strictness,
        output_plane,
    }
}

fn metadata_matches_spec(row: &RowMetadata, spec: MetadataSpec) -> bool {
    row.track == spec.track
        && row.workload == spec.workload
        && row.materialisation == spec.materialisation
        && row.competitor_crate.as_deref() == spec.competitor_crate
        && row.competitor_version.as_deref() == spec.competitor_version
        && row.strictness == spec.strictness
        && row.output_plane == spec.output_plane
}

fn read_simd_metadata_row(criterion_root: &Path, fixture: &str) -> Result<RowMetadata, String> {
    let path = criterion_root
        .join("simd_structural_scan")
        .join(format!("{fixture}_simd"))
        .join("metadata.toml");
    let text = fs::read_to_string(&path)
        .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
    toml::from_str::<RowMetadata>(&text)
        .map_err(|error| format!("malformed {}: {error}", path.display()))
}

fn validate_w0_simd_metadata(
    fixture: &str,
    input_sha256: &str,
    input_bytes: u64,
    scalar_hash: &str,
    main_capture: &RowMetadata,
    row: &RowMetadata,
) -> Result<(), String> {
    if !row.required_fields_present() {
        return Err("missing required metadata fields".to_string());
    }
    if row.input_sha256 != input_sha256 {
        return Err(format!(
            "{} SIMD metadata has input hash {}, expected {}",
            fixture, row.input_sha256, input_sha256
        ));
    }
    if row.input_bytes != input_bytes {
        return Err(format!(
            "{} SIMD metadata has {} input bytes, expected {}",
            fixture, row.input_bytes, input_bytes
        ));
    }
    if row.track != TrackTag::SimdScan
        || row.workload != "cycles_per_byte"
        || row.materialisation != "structural_offsets"
        || row.strictness != "strict"
        || row.output_plane != "offset bitmap"
        || row.parse_mode != "simd_scan"
    {
        return Err("SIMD metadata has unsupported bench semantics".to_string());
    }
    if row.cpu_model != main_capture.cpu_model
        || row.cpu_arch != main_capture.cpu_arch
        || row.os_kernel != main_capture.os_kernel
        || row.rustflags != main_capture.rustflags
        || row.target_cpu != main_capture.target_cpu
        || row.profile != main_capture.profile
        || row.bbnf_commit != main_capture.bbnf_commit
    {
        return Err("SIMD metadata is from a different capture".to_string());
    }
    if row.profile != "bench"
        || row.rustflags != "-C target-cpu=native"
        || row.target_cpu != "native"
        || row.warmup_samples != 3
        || !same_f64(row.warmup_time_s, 3.0)
        || row.sample_size != 100
        || !same_f64(row.measurement_time_s, 5.0)
        || !same_f64(row.confidence_interval, 0.95)
        || row.outlier_rejection != "iqr"
        || row.statistical_method != "bootstrap"
    {
        return Err("SIMD metadata has unsupported capture policy".to_string());
    }
    if simd_metadata_hash(row, fixture).as_deref() != Some(scalar_hash) {
        return Err("SIMD metadata parity hash does not match scalar scan".to_string());
    }
    Ok(())
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

fn criterion_root() -> PathBuf {
    if let Some(path) = env::var_os("CRITERION_HOME") {
        return PathBuf::from(path);
    }
    env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root().join("target"))
        .join("criterion")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn w1a_non_json_report_arg_extracts_single_path() {
        let args = vec![
            "--w1a-non-json-report".to_string(),
            "../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json"
                .to_string(),
        ];
        assert_eq!(
            w1a_non_json_report_path(&args).unwrap(),
            Some(PathBuf::from(
                "../restart/skinny/tranches/sk-v11/research/w1a/fixtures/nonjson-pass-css-l4.json"
            ))
        );
    }

    #[test]
    fn w1a_non_json_report_arg_rejects_update_results_combination() {
        let args = vec![
            "--w1a-non-json-report".to_string(),
            "nonjson-pass-css-l4.json".to_string(),
            "--update-results".to_string(),
        ];
        assert!(w1a_non_json_report_path(&args).is_err());
    }

    #[test]
    fn skv12_non_json_report_arg_extracts_single_path_and_allows_read_only_companions() {
        let args = vec![
            "--skv12-non-json-report".to_string(),
            "skv12-nonjson-pass.json".to_string(),
        ];
        assert_eq!(
            skv12_non_json_report_path(&args).unwrap(),
            Some(PathBuf::from("skv12-nonjson-pass.json"))
        );
        let mixed = vec![
            "--w1a-non-json-report".to_string(),
            "w1a.json".to_string(),
            "--skv12-non-json-report".to_string(),
            "skv12.json".to_string(),
        ];
        assert_eq!(
            skv12_non_json_report_path(&mixed).unwrap(),
            Some(PathBuf::from("skv12.json"))
        );
    }

    #[test]
    fn skv12_non_json_report_arg_allows_no_write_json_check_flags() {
        let args = vec![
            "--skv12-non-json-report".to_string(),
            "skv12-nonjson-pass.json".to_string(),
            "--advisory".to_string(),
            "--check-results".to_string(),
        ];
        assert_eq!(
            skv12_non_json_report_path(&args).unwrap(),
            Some(PathBuf::from("skv12-nonjson-pass.json"))
        );
        assert!(companion_report_runs_json_check(&args));
    }

    #[test]
    fn skv12_css_l4_sota_report_arg_extracts_single_path_and_allows_read_only_companions() {
        let args = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
        ];
        assert_eq!(
            skv12_css_l4_sota_report_path(&args).unwrap(),
            Some(PathBuf::from("skv12-css-l4-sota.json"))
        );
        let mixed = vec![
            "--skv12-non-json-report".to_string(),
            "skv12-nonjson-pass.json".to_string(),
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
        ];
        assert_eq!(
            skv12_css_l4_sota_report_path(&mixed).unwrap(),
            Some(PathBuf::from("skv12-css-l4-sota.json"))
        );
    }

    #[test]
    fn skv12_css_l4_sota_report_arg_rejects_write_probe_and_flag_paths() {
        let write = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
            "--write-results".to_string(),
        ];
        assert!(skv12_css_l4_sota_report_path(&write).is_err());
        let probe = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
            "--include-volatile-probes".to_string(),
        ];
        assert!(skv12_css_l4_sota_report_path(&probe).is_err());
        let flag_path = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "--advisory".to_string(),
        ];
        assert!(skv12_css_l4_sota_report_path(&flag_path).is_err());
    }

    #[test]
    fn skv12_css_l4_sota_report_arg_allows_no_write_json_check_flags() {
        let args = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
            "--advisory".to_string(),
            "--check-results".to_string(),
        ];
        assert_eq!(
            skv12_css_l4_sota_report_path(&args).unwrap(),
            Some(PathBuf::from("skv12-css-l4-sota.json"))
        );
        assert!(companion_report_runs_json_check(&args));
    }

    #[test]
    fn skv13_css_comparator_oracle_report_arg_allows_json_check_only() {
        let args = vec![
            "--skv13-css-comparator-oracle-report".to_string(),
            "skv13-css-comparator.json".to_string(),
            "--advisory".to_string(),
            "--check-results".to_string(),
        ];
        assert_eq!(
            skv13_css_comparator_oracle_report_path(&args).unwrap(),
            Some(PathBuf::from("skv13-css-comparator.json"))
        );
        assert!(companion_report_runs_json_check(&args));
    }

    #[test]
    fn skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports() {
        let mixed = vec![
            "--skv12-css-l4-sota-report".to_string(),
            "skv12-css-l4-sota.json".to_string(),
            "--skv13-css-comparator-oracle-report".to_string(),
            "skv13-css-comparator.json".to_string(),
            "--skv13-css-stylesheet-selectors-report".to_string(),
            "skv13-css-w2.json".to_string(),
            "--skv13-css-declaration-values-extended-report".to_string(),
            "skv13-css-w3.json".to_string(),
        ];
        assert_eq!(
            skv13_css_comparator_oracle_report_path(&mixed).unwrap(),
            Some(PathBuf::from("skv13-css-comparator.json"))
        );
        assert_eq!(
            skv13_css_stylesheet_selectors_report_path(&mixed).unwrap(),
            Some(PathBuf::from("skv13-css-w2.json"))
        );
        assert_eq!(
            skv13_css_declaration_values_extended_report_path(&mixed).unwrap(),
            Some(PathBuf::from("skv13-css-w3.json"))
        );
        let write = vec![
            "--skv13-css-comparator-oracle-report".to_string(),
            "skv13-css-comparator.json".to_string(),
            "--write-results".to_string(),
        ];
        assert!(skv13_css_comparator_oracle_report_path(&write).is_err());
    }

    #[test]
    fn w0_parse_non_admission_preserves_hard_failures() {
        assert_eq!(
            w0_parse_non_admission(Outcome::IParityOracleFail),
            Outcome::IParityOracleFail
        );
        assert_eq!(
            w0_parse_non_admission(Outcome::JSchemaFail),
            Outcome::JSchemaFail
        );
        assert_eq!(
            w0_parse_non_admission(Outcome::KSimdParityHashFail),
            Outcome::KSimdParityHashFail
        );
        assert_eq!(
            w0_parse_non_admission(Outcome::LSimdThroughputFail),
            Outcome::LSimdThroughputFail
        );
        assert_eq!(
            w0_parse_non_admission(Outcome::MMemoryResidencyFail),
            Outcome::MMemoryResidencyFail
        );
    }

    #[test]
    fn w0_parse_non_admission_demotes_admission_capable_parse_outcomes() {
        assert_eq!(
            w0_parse_non_admission(Outcome::ABeatAndParity),
            Outcome::SSubstrateGuardNonAdmission
        );
        assert_eq!(
            w0_parse_non_admission(Outcome::GSubstrateFailure),
            Outcome::SSubstrateGuardNonAdmission
        );
    }

    #[test]
    fn w2_direct_reclamation_admits_only_floor_pass_clamped_rows() {
        assert_eq!(
            direct_row_decision(
                "apache_builds",
                None,
                1_000_000,
                Some(750_000.0),
                Some(760_000.0)
            ),
            DirectRowDecision {
                outcome: None,
                w0_clamped: false,
                w2_reclaimed: true,
                w10_residual: false,
            }
        );
        assert_eq!(
            direct_row_decision("twitter", None, 1_000_000, Some(800_000.0), Some(850_000.0)),
            DirectRowDecision {
                outcome: Some(Outcome::NDirectProjectionFailure),
                w0_clamped: true,
                w2_reclaimed: false,
                w10_residual: false,
            }
        );
        assert_eq!(
            direct_row_decision(
                "citm_catalog",
                None,
                1_000_000,
                Some(800_000.0),
                Some(850_000.0)
            ),
            DirectRowDecision {
                outcome: None,
                w0_clamped: false,
                w2_reclaimed: false,
                w10_residual: false,
            }
        );
        assert_eq!(
            direct_row_decision(
                "apache_builds",
                Some(Outcome::IParityOracleFail),
                1_000_000,
                Some(800_000.0),
                Some(850_000.0)
            ),
            DirectRowDecision {
                outcome: Some(Outcome::IParityOracleFail),
                w0_clamped: false,
                w2_reclaimed: false,
                w10_residual: false,
            }
        );
    }

    #[test]
    fn w10_direct_residual_admits_only_instruments_floor_pass_clamped_rows() {
        assert_eq!(
            direct_row_decision(
                "instruments",
                Some(Outcome::NDirectProjectionFailure),
                1_000_000,
                Some(700_000.0),
                Some(720_000.0)
            ),
            DirectRowDecision {
                outcome: None,
                w0_clamped: false,
                w2_reclaimed: false,
                w10_residual: true,
            }
        );
        assert_eq!(
            direct_row_decision(
                "instruments",
                None,
                1_000_000,
                Some(700_000.0),
                Some(730_000.0)
            ),
            DirectRowDecision {
                outcome: Some(Outcome::NDirectProjectionFailure),
                w0_clamped: true,
                w2_reclaimed: false,
                w10_residual: false,
            }
        );
        assert_eq!(
            direct_row_decision("random", None, 1_000_000, Some(700_000.0), Some(700_000.0)),
            DirectRowDecision {
                outcome: Some(Outcome::NDirectProjectionFailure),
                w0_clamped: true,
                w2_reclaimed: false,
                w10_residual: false,
            }
        );
    }

    #[test]
    fn w6_github_events_typed_admits_only_track1_track2_floor_pass() {
        assert_eq!(
            real_typed_row_decision(
                "github_events",
                None,
                1_000_000,
                Some(500_000.0),
                Some(520_000.0),
                Some(550_000.0),
            ),
            RealTypedRowDecision {
                outcome: None,
                w6_github_events_added: true,
            }
        );
        assert_eq!(
            real_typed_row_decision(
                "github_events",
                None,
                1_000_000,
                Some(500_000.0),
                Some(700_000.0),
                Some(550_000.0),
            ),
            RealTypedRowDecision {
                outcome: Some(Outcome::NDirectProjectionFailure),
                w6_github_events_added: false,
            }
        );
        assert_eq!(
            real_typed_row_decision(
                "github_events",
                Some(Outcome::IParityOracleFail),
                1_000_000,
                Some(500_000.0),
                Some(520_000.0),
                Some(550_000.0),
            ),
            RealTypedRowDecision {
                outcome: Some(Outcome::IParityOracleFail),
                w6_github_events_added: false,
            }
        );
    }

    #[test]
    fn w0_capture_metadata_accepts_coherent_required_rows() {
        let rows = metadata_rows(false);
        validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).unwrap();
    }

    #[test]
    fn w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures() {
        assert!(w0_real_typed_metadata_expected("twitter"));
        assert!(w0_real_typed_metadata_expected("update_center"));
        assert!(w0_real_typed_metadata_expected("apache_builds"));
        assert!(w0_real_typed_metadata_expected("citm_catalog"));
        assert!(w0_real_typed_metadata_expected("github_events"));
    }

    #[test]
    fn w0_capture_metadata_rejects_fixture_mismatch() {
        let mut rows = metadata_rows(false);
        rows[0].input_sha256 = "other".into();
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());

        let mut rows = metadata_rows(false);
        rows[0].input_bytes = 13;
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());
    }

    #[test]
    fn w0_capture_metadata_rejects_mixed_capture() {
        let mut rows = metadata_rows(false);
        rows[1].bbnf_commit = "other-commit".into();
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());

        let mut rows = metadata_rows(false);
        rows[1].target_cpu = "other-cpu".into();
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());

        let mut rows = metadata_rows(false);
        rows[0].rustflags.clear();
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());
    }

    #[test]
    fn w0_report_capture_identity_rejects_cross_fixture_drift() {
        let row = metadata_rows(false).remove(0);
        let mut identity = None;
        validate_report_capture_identity(&mut identity, "fixture-a", &row).unwrap();
        validate_report_capture_identity(&mut identity, "fixture-b", &row).unwrap();

        let mut other = row.clone();
        other.bbnf_commit = "other-commit".into();
        assert!(validate_report_capture_identity(&mut identity, "fixture-c", &other).is_err());
    }

    #[test]
    fn w0_capture_metadata_rejects_missing_required_bench() {
        let mut rows = metadata_rows(false);
        rows.retain(|row| row.materialisation != "eager_typed");
        assert!(validate_w0_capture_metadata("fixture", "hash", 12, false, &rows).is_err());
    }

    #[test]
    fn w0_simd_metadata_rejects_capture_and_hash_mismatch() {
        let main = metadata_rows(false).remove(0);
        let mut simd = simd_metadata_row(&main);
        assert!(
            validate_w0_simd_metadata("fixture", "hash", 12, "scan-hash", &main, &simd).is_ok()
        );

        simd.bbnf_commit = "other-commit".into();
        assert!(
            validate_w0_simd_metadata("fixture", "hash", 12, "scan-hash", &main, &simd).is_err()
        );

        let mut simd = simd_metadata_row(&main);
        simd.scalar_parity_hash_twitter = Some("other-hash".into());
        assert!(
            validate_w0_simd_metadata("fixture", "hash", 12, "scan-hash", &main, &simd).is_err()
        );

        let mut simd = simd_metadata_row(&main);
        simd.sample_size = 50;
        assert!(
            validate_w0_simd_metadata("fixture", "hash", 12, "scan-hash", &main, &simd).is_err()
        );
    }

    #[test]
    fn w0_criterion_fingerprint_excludes_derendered_probe_estimates() {
        let root = test_temp_root("criterion-fingerprint");
        let fixture_names = BTreeSet::from(["twitter", "canada"]);
        write_test_file(
            &root.join("json_twitter/track1_generated/new/estimates.json"),
            b"main-estimate-a",
        );
        write_test_file(
            &root.join("json_twitter/track1_generated/metadata.toml"),
            b"main-metadata",
        );
        let before_probe = criterion_fingerprint(&root, &fixture_names);
        write_test_file(
            &root.join("json_probes_twitter/host_call_dispatch_overhead/new/estimates.json"),
            b"volatile-probe",
        );
        assert_eq!(before_probe, criterion_fingerprint(&root, &fixture_names));

        write_test_file(
            &root.join("json_unvalidated_future/track1_generated/new/estimates.json"),
            b"unvalidated-future-estimate",
        );
        assert_eq!(before_probe, criterion_fingerprint(&root, &fixture_names));

        write_test_file(
            &root.join("json_canada/sonic_rs_real_typed_struct/new/estimates.json"),
            b"valid-fixture-unvalidated-row",
        );
        assert_eq!(before_probe, criterion_fingerprint(&root, &fixture_names));

        write_test_file(
            &root.join("json_twitter/track1_generated/new/estimates.json"),
            b"main-estimate-b",
        );
        assert_ne!(before_probe, criterion_fingerprint(&root, &fixture_names));
        let _ = fs::remove_dir_all(root);
    }

    #[test]
    fn update_results_retains_css_l4_rows() {
        let json = "# Skinny JSON Bench Results\n\n| Corpus | Workload |\n|---|---|\n| twitter | parse_only |\n\n## Notes\n\n- JSON note.\n";
        let existing = "# Skinny JSON Bench Results\n\n| css_l4/declaration_values/direct_to_struct/main | css_l4 | evidence |\n\n## Notes\n\n- SK-V12 campaign close: PASS-ADMIT via css_l4/declaration_values/direct_to_struct/main.\n";
        let merged = merge_retained_non_json_results(json, Some(existing));
        assert!(merged.contains("| css_l4/declaration_values/direct_to_struct/main |"));
        assert!(merged.contains("- SK-V12 campaign close: PASS-ADMIT"));
        assert!(merged.find("| css_l4/").unwrap() < merged.find("## Notes").unwrap());
    }

    fn metadata_rows(real_typed: bool) -> Vec<RowMetadata> {
        required_metadata_specs(real_typed)
            .into_iter()
            .map(metadata_row)
            .collect()
    }

    fn metadata_row(spec: MetadataSpec) -> RowMetadata {
        RowMetadata {
            schema_version: bbnf_bench::metadata::SCHEMA_VERSION.into(),
            cpu_model: "cpu".into(),
            cpu_arch: "arch".into(),
            os_kernel: "kernel".into(),
            rustflags: "-C target-cpu=native".into(),
            target_cpu: "native".into(),
            profile: "bench".into(),
            input_sha256: "hash".into(),
            input_bytes: 12,
            competitor_crate: spec.competitor_crate.map(str::to_string),
            competitor_version: spec.competitor_version.map(str::to_string),
            bbnf_commit: "commit".into(),
            warmup_samples: 3,
            warmup_time_s: 3.0,
            sample_size: 100,
            measurement_time_s: 5.0,
            confidence_interval: 0.95,
            outlier_rejection: "iqr".into(),
            statistical_method: "bootstrap".into(),
            track: spec.track,
            workload: spec.workload.into(),
            strictness: spec.strictness.into(),
            parse_utf8: if spec.track == TrackTag::Competitor {
                "scan-boundary".into()
            } else {
                "view-boundary".into()
            },
            escape_complete: "yes".into(),
            flaw_probe: "none".into(),
            output_plane: spec.output_plane.into(),
            feature_mask: "feature".into(),
            api_symbol: spec.label.into(),
            sidecar_freshness: "same-run".into(),
            primitive_status: "test".into(),
            hot_leaf: "leaf".into(),
            materialisation: spec.materialisation.into(),
            parse_mode: "mode".into(),
            source_ownership: "borrowed".into(),
            allocator: "mimalloc".into(),
            plan_variant: "variant".into(),
            host_call_mode: "none".into(),
            arena_writes: None,
            payload_allocations: None,
            scalar_parity_hash_twitter: None,
            scalar_parity_hash_citm: None,
            scalar_parity_hash_canada: None,
            peak_rss_bytes: Some(1),
            cold_cache_mode: "warm".into(),
        }
    }

    fn simd_metadata_row(main: &RowMetadata) -> RowMetadata {
        let mut row = main.clone();
        row.track = TrackTag::SimdScan;
        row.workload = "cycles_per_byte".into();
        row.strictness = "strict".into();
        row.parse_utf8 = "none".into();
        row.escape_complete = "n/a".into();
        row.flaw_probe = "structural scan parity probe".into();
        row.output_plane = "offset bitmap".into();
        row.feature_mask = "Scalar".into();
        row.api_symbol = "bbnf_bench::scan::structural_offsets_simd".into();
        row.sidecar_freshness = "same-run".into();
        row.primitive_status = "checkasm-backed primitive".into();
        row.hot_leaf = "unprofiled in W0b".into();
        row.materialisation = "structural_offsets".into();
        row.parse_mode = "simd_scan".into();
        row.sample_size = 100;
        row.measurement_time_s = 5.0;
        row.scalar_parity_hash_twitter = Some("scan-hash".into());
        row.arena_writes = None;
        row.payload_allocations = None;
        row
    }

    fn test_temp_root(label: &str) -> PathBuf {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = env::temp_dir().join(format!("skv8-{label}-{}-{nanos}", std::process::id()));
        fs::create_dir_all(&root).unwrap();
        root
    }

    fn write_test_file(path: &Path, bytes: &[u8]) {
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
}
