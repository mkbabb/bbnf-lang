use anyhow::{bail, Context, Result};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;

mod real_typed_schema;

const USAGE: &str = "usage: cargo xtask <regen-json|check-json|regen-real-typed|check-real-typed|check-conformance|lint-loc|bench-json|gate-json|primitive-checkasm>";

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let Some(command) = args.next() else {
        bail!("{USAGE}");
    };

    let root = workspace_root()?;
    match command.as_str() {
        "regen-json" => regen_json(&root),
        "check-json" => check_json(&root),
        "regen-real-typed" => regen_real_typed(&root),
        "check-real-typed" => check_real_typed(&root),
        "check-conformance" => check_conformance(),
        "lint-loc" => lint_loc(&root),
        "bench-json" => bench_json(&root, args.collect()),
        "gate-json" => gate_json(&root, args.collect()),
        "primitive-checkasm" => primitive_checkasm(&root),
        "help" | "--help" | "-h" => {
            eprintln!("{USAGE}");
            Ok(())
        }
        other => bail!("unknown xtask command `{other}`"),
    }
}

fn check_conformance() -> Result<()> {
    let suite = test_fixtures::load_json_suite()?;
    let mut failures = Vec::new();
    let mut valid_count = 0usize;
    let mut invalid_count = 0usize;

    for fixture in suite
        .embedded_valid
        .iter()
        .chain(suite.corpus.iter().filter_map(|status| match status {
            test_fixtures::FixtureStatus::Available(fixture) => Some(fixture),
            test_fixtures::FixtureStatus::Unavailable(_) => None,
        }))
    {
        valid_count += 1;
        let skinny = runtime::generated_json::parse_bytes(&fixture.bytes);
        let serde = serde_json::from_slice::<serde_json::Value>(&fixture.bytes);
        if skinny.is_err() || serde.is_err() {
            failures.push(format!(
                "valid fixture {} failed: skinny={:?} serde={:?}",
                fixture.name,
                skinny.err().map(|error| error.to_string()),
                serde.err().map(|error| error.to_string())
            ));
            continue;
        }
        check_float_bits(&fixture.name, skinny.unwrap().value(), &mut failures);
    }

    for fixture in suite.embedded_invalid {
        invalid_count += 1;
        let skinny_ok = runtime::generated_json::parse_bytes(&fixture.bytes).is_ok();
        let serde_ok = serde_json::from_slice::<serde_json::Value>(&fixture.bytes).is_ok();
        if skinny_ok {
            failures.push(format!(
                "invalid fixture {} accepted: skinny_ok={skinny_ok} serde_ok={serde_ok}",
                fixture.name
            ));
        }
    }

    println!(
        "conformance: {valid_count} valid fixtures accepted; {invalid_count} invalid fixtures rejected"
    );
    if failures.is_empty() {
        Ok(())
    } else {
        bail!("{}", failures.join("; "))
    }
}

fn check_float_bits(
    name: &str,
    value: runtime::generated_json::JsonValue<'_, '_>,
    failures: &mut Vec<String>,
) {
    match value {
        runtime::generated_json::JsonValue::Object(object) => {
            for pair in object.pairs() {
                check_float_bits(name, pair.value(), failures);
            }
        }
        runtime::generated_json::JsonValue::Array(array) => {
            for value in array.values() {
                check_float_bits(name, value, failures);
            }
        }
        runtime::generated_json::JsonValue::Number(number) => {
            let raw = number.raw();
            if raw.contains('.') || raw.contains('e') || raw.contains('E') || raw == "-0" {
                let skinny = number.as_f64().map(f64::to_bits);
                let serde = serde_json::from_str::<serde_json::Number>(raw)
                    .ok()
                    .and_then(|number| number.as_f64())
                    .map(f64::to_bits);
                if skinny != serde {
                    failures.push(format!(
                        "float bit mismatch in {name}: literal={raw} skinny={skinny:?} serde={serde:?}"
                    ));
                }
            }
        }
        _ => {}
    }
}

fn regen_json(root: &Path) -> Result<()> {
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let emitted = codegen::emit_from_source("json", &source)?;
    emitted.write_to_dir(root.join("crates/runtime/src/grammars/json"))?;
    Ok(())
}

fn check_json(root: &Path) -> Result<()> {
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let emitted = codegen::emit_from_source("json", &source)?;
    emitted
        .check_dir(root.join("crates/runtime/src/grammars/json"))
        .context("generated JSON runtime is stale; run `cargo xtask regen-json`")
}

fn regen_real_typed(root: &Path) -> Result<()> {
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let schema = real_typed_schema::schema();
    let emitted = codegen::emit_typed_from_source("json", &source, &schema)?;
    emitted.write_to_dir(root.join("crates/bbnf-bench/src"))?;
    Ok(())
}

fn check_real_typed(root: &Path) -> Result<()> {
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let schema = real_typed_schema::schema();
    let emitted = codegen::emit_typed_from_source("json", &source, &schema)?;
    emitted
        .check_dir(root.join("crates/bbnf-bench/src"))
        .context(
            "generated real typed DirectBuild module is stale; run `cargo xtask regen-real-typed`",
        )
}

fn lint_loc(root: &Path) -> Result<()> {
    let budgets = [
        ("crates/bbnf", 600usize),
        ("crates/grammar", 3500),
        ("crates/ir", 2500),
        ("crates/passes", 6000),
        ("crates/codegen", 4500),
        ("crates/runtime", 4000),
        ("crates/parse-that-regex", 4000),
        ("crates/bbnf-simd", 3500),
        ("crates/bbnf-bench", 3300),
        ("crates/test-fixtures", 800),
        ("xtask", 650),
    ];
    let mut failures = Vec::new();
    for (path, budget) in budgets {
        let loc = rust_loc(&root.join(path))?;
        println!("{path}: {loc}/{budget} LOC");
        if path == "crates/bbnf-bench" && (3250..=budget).contains(&loc) {
            println!(
                "warning: BBNF-BUDGET-CLIFF {path} has {loc}/{budget} LOC; post-iteration headroom is nearly exhausted"
            );
        }
        if loc > budget {
            failures.push(format!("{path} has {loc} LOC over budget {budget}"));
        }
    }

    let generated_dir = root.join("crates/runtime/src/grammars/json");
    if generated_dir.exists() {
        let generated = rust_loc(&generated_dir)?;
        println!("generated runtime json: {generated}/4000 LOC");
        if generated > 4000 {
            failures.push(format!(
                "generated runtime json has {generated} LOC over budget 4000"
            ));
        }
    }

    let track2 = rust_loc(&root.join("crates/bbnf-bench/src/track2/json.rs"))?;
    println!("track2 handwritten json: {track2}/500 LOC");
    if track2 > 500 {
        failures.push(format!(
            "track2 handwritten json has {track2} LOC over budget 500"
        ));
    }

    if failures.is_empty() {
        Ok(())
    } else {
        bail!("{}", failures.join("; "))
    }
}

fn bench_json(root: &Path, passthrough: Vec<String>) -> Result<()> {
    let advisory = passthrough.iter().any(|arg| arg == "--advisory");
    let criterion_args: Vec<String> = passthrough
        .into_iter()
        .filter(|arg| arg != "--advisory")
        .collect();
    let full_run = criterion_args.is_empty();
    let mut command = Command::new("cargo");
    command
        .current_dir(root)
        .arg("bench")
        .arg("-p")
        .arg("bbnf-bench");
    apply_bench_output_env(&mut command, root);
    if !full_run {
        command.arg("--").args(&criterion_args);
    }
    let status = command.status().context("failed to spawn cargo bench")?;
    if status.success() {
        if full_run {
            let mut gate_args = vec!["--update-results".to_string()];
            if advisory {
                gate_args.push("--advisory".to_string());
            }
            gate_json(root, gate_args)
        } else {
            Ok(())
        }
    } else {
        bail!("cargo bench failed with status {status}")
    }
}

fn gate_json(root: &Path, passthrough: Vec<String>) -> Result<()> {
    if passthrough.iter().any(|arg| arg == "--with-cost-facts") {
        return gate_json_cost_facts(root, passthrough);
    }
    if passthrough.iter().any(|arg| arg == "--check-results") {
        validate_w0_results_snapshot(root)?;
    }
    validate_gate_json_passthrough(&passthrough)?;
    let mut command = Command::new("cargo");
    command
        .current_dir(root)
        .args(["run", "-p", "bbnf-bench", "--bin", "gate"])
        .arg("--")
        .args(passthrough);
    apply_bench_output_env(&mut command, root);
    let status = command.status().context("failed to spawn bench gate")?;
    if status.success() {
        Ok(())
    } else {
        bail!("bench gate failed with status {status}")
    }
}

fn validate_gate_json_passthrough(args: &[String]) -> Result<()> {
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--advisory"
            | "--check-results"
            | "--update-results"
            | "--write-results"
            | "--include-volatile-probes" => index += 1,
            "--w1a-non-json-report"
            | "--skv12-non-json-report"
            | "--skv12-css-l4-sota-report"
            | "--skv13-css-comparator-oracle-report"
            | "--skv13-css-stylesheet-selectors-report"
            | "--skv13-css-declaration-values-extended-report"
            | "--skv13-css-visual-functions-report"
            | "--skv13-css-at-rules-media-report"
            | "--skv13-css-vendor-custom-report"
            | "--skv13-css-nested-layout-report"
            | "--skv13-decision-regex-report"
            | "--skv13-decision-active-cost-report"
            | "--skv13-decision-csp-cascade-report"
            | "--skv13-per-grammar-policy-report"
            | "--skv13-same-substrate-union-report" => {
                if index + 1 >= args.len() {
                    bail!("{} expects one path argument", args[index]);
                }
                index += 2;
            }
            other => bail!("gate-json got unsupported argument {other}"),
        }
    }
    Ok(())
}

fn apply_bench_output_env(command: &mut Command, root: &Path) {
    if let Some(target_dir) = normalized_env_path(root, "CARGO_TARGET_DIR") {
        command.env("CARGO_TARGET_DIR", &target_dir);
        if std::env::var_os("CRITERION_HOME").is_none() {
            command.env("CRITERION_HOME", target_dir.join("criterion"));
        }
    }
    if let Some(criterion_home) = normalized_env_path(root, "CRITERION_HOME") {
        command.env("CRITERION_HOME", criterion_home);
    }
}

fn normalized_env_path(root: &Path, key: &str) -> Option<PathBuf> {
    let path = PathBuf::from(std::env::var_os(key)?);
    if path.is_absolute() {
        Some(path)
    } else {
        Some(root.join(path))
    }
}

fn gate_json_cost_facts(root: &Path, passthrough: Vec<String>) -> Result<()> {
    let check_results = validate_cost_facts_flags(&passthrough)?;
    if check_results {
        validate_w0_results_snapshot(root)?;
    }
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let snapshot = codegen::cost_facts_from_source("json", &source)?;
    let report = cost_facts_gate_report(&snapshot)?;
    validate_cost_facts_gate_report(&report)?;
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn validate_cost_facts_flags(passthrough: &[String]) -> Result<bool> {
    let mut check_results = false;
    let mut unexpected = Vec::new();
    for arg in passthrough {
        match arg.as_str() {
            "--with-cost-facts" => {}
            "--advisory" => {}
            "--check-results" => check_results = true,
            _ => unexpected.push(arg.clone()),
        }
    }
    if !unexpected.is_empty() {
        bail!("gate-json --with-cost-facts accepts only --advisory and --check-results; got {unexpected:?}");
    }
    Ok(check_results)
}

fn validate_w0_results_snapshot(root: &Path) -> Result<()> {
    let text = std::fs::read_to_string(root.join("RESULTS.md"))
        .context("gate-json --with-cost-facts --check-results requires RESULTS.md")?;
    let rolling_path = root
        .parent()
        .context("skinny workspace has no parent")?
        .join("restart/skinny/ROLLING-SOTA-DELTA.md");
    validate_skv13_rolling_delta(&text, &rolling_path).with_context(|| {
        format!(
            "{} is not a valid SK-V13 rolling delta",
            rolling_path.display()
        )
    })?;
    Ok(())
}

const SKV13_JSON_CORPORA: &[&str] = &[
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

const SKV13_JSON_WORKLOADS: &[&str] = &["parse_only", "direct_to_struct", "real_typed_struct"];

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

#[derive(Debug, Clone)]
struct RollingDeltaRow {
    row_id: String,
    plane: String,
    t1_current: String,
    t1_sota: String,
    margin: String,
    tranche_admitted: String,
}

#[derive(Debug, Clone, Copy)]
struct ResultsMetric {
    track1_threshold_mbps: f64,
    threshold_mbps: f64,
}

fn validate_skv13_rolling_delta(results_text: &str, rolling_path: &Path) -> Result<()> {
    let rolling_text = std::fs::read_to_string(rolling_path).with_context(|| {
        format!(
            "gate-json --check-results requires {}",
            rolling_path.display()
        )
    })?;
    for required in [
        "schema_version: sk-v13-rolling-sota-delta-v1",
        "run_id: SK-V13-open",
        "g_omega_status: signed",
        "consumer_gate: cargo xtask gate-json --check-results",
    ] {
        if !rolling_text.contains(required) {
            bail!("ROLLING-SOTA-DELTA.md missing `{required}`");
        }
    }

    let json_rows = parse_rolling_rows(&rolling_text, "json/")?;
    let css_rows = parse_rolling_rows(&rolling_text, "css_l4/")?;
    if json_rows.len() != SKV13_JSON_CORPORA.len() * SKV13_JSON_WORKLOADS.len() {
        bail!(
            "ROLLING-SOTA-DELTA.md expected 51 JSON rows, saw {}",
            json_rows.len()
        );
    }
    if css_rows.len() != SKV13_CSS_FEATURES.len() {
        bail!(
            "ROLLING-SOTA-DELTA.md expected 24 CSS feature rows, saw {}",
            css_rows.len()
        );
    }

    let result_metrics = parse_results_metrics(results_text)?;
    let mut seen = BTreeSet::new();
    for corpus in SKV13_JSON_CORPORA {
        for workload in SKV13_JSON_WORKLOADS {
            let row_id = format!("json/{corpus}/{workload}/main");
            let row = find_rolling_row(&json_rows, &row_id)?;
            if !seen.insert(row_id.clone()) {
                bail!("duplicate rolling row {row_id}");
            }
            if row.plane != *workload {
                bail!(
                    "{row_id} rolling plane {} does not match {workload}",
                    row.plane
                );
            }
            validate_rolling_status(row)?;
            if let Some(metric) = result_metrics.get(&row_id) {
                validate_numeric_rolling_row(row, *metric)?;
            } else {
                validate_absent_rolling_row(row)?;
            }
        }
    }

    let mut seen_css = BTreeSet::new();
    let css_metrics = parse_css_results_metrics(results_text)?;
    for feature in SKV13_CSS_FEATURES {
        let row_id = format!("css_l4/{feature}/direct_to_struct/main");
        let row = find_rolling_row(&css_rows, &row_id)?;
        if !seen_css.insert(row_id.clone()) {
            bail!("duplicate CSS rolling row {row_id}");
        }
        if row.plane != "css_l4_parity" {
            bail!("{row_id} rolling plane {} is not css_l4_parity", row.plane);
        }
        validate_rolling_status(row)?;
        if let Some(metric) = css_metrics.get(&row_id) {
            validate_numeric_rolling_row(row, *metric)?;
            if row.tranche_admitted != "ADMITTED" {
                bail!(
                    "{row_id} has numeric CSS evidence but is {}",
                    row.tranche_admitted
                );
            }
        } else {
            validate_absent_rolling_row(row)?;
            if row.tranche_admitted != "OPEN" {
                bail!(
                    "{row_id} open CSS target has status {}",
                    row.tranche_admitted
                );
            }
        }
    }
    Ok(())
}

fn parse_rolling_rows(text: &str, prefix: &str) -> Result<Vec<RollingDeltaRow>> {
    let mut rows = Vec::new();
    for line in text.lines() {
        let cells = markdown_cells(line);
        if cells.len() != 6 || !cells[0].starts_with(prefix) {
            continue;
        }
        rows.push(RollingDeltaRow {
            row_id: cells[0].clone(),
            plane: cells[1].clone(),
            t1_current: cells[2].clone(),
            t1_sota: cells[3].clone(),
            margin: cells[4].clone(),
            tranche_admitted: cells[5].clone(),
        });
    }
    Ok(rows)
}

fn find_rolling_row<'a>(rows: &'a [RollingDeltaRow], row_id: &str) -> Result<&'a RollingDeltaRow> {
    rows.iter()
        .find(|row| row.row_id == row_id)
        .with_context(|| format!("ROLLING-SOTA-DELTA.md missing {row_id}"))
}

fn validate_rolling_status(row: &RollingDeltaRow) -> Result<()> {
    match row.tranche_admitted.as_str() {
        "ADMITTED" | "OPEN" | "MISSING" | "ARCHITECTURAL-BLOCK" | "OUT_OF_SCOPE" => Ok(()),
        other => bail!("{} has unsupported tranche_admitted {other}", row.row_id),
    }
}

fn validate_numeric_rolling_row(row: &RollingDeltaRow, metric: ResultsMetric) -> Result<()> {
    let current = parse_delta_number(&row.row_id, "T1_current", &row.t1_current)?;
    let threshold = parse_delta_number(&row.row_id, "T1_sota", &row.t1_sota)?;
    let margin = parse_delta_number(&row.row_id, "margin", &row.margin)?;
    require_close_delta(
        &row.row_id,
        "T1_current",
        current,
        metric.track1_threshold_mbps,
    )?;
    require_close_delta(&row.row_id, "T1_sota", threshold, metric.threshold_mbps)?;
    require_close_delta(&row.row_id, "margin", margin, current - threshold)?;
    if row.tranche_admitted == "ADMITTED" && margin <= 0.0 {
        bail!(
            "{} is ADMITTED with non-positive margin {margin}",
            row.row_id
        );
    }
    Ok(())
}

fn validate_absent_rolling_row(row: &RollingDeltaRow) -> Result<()> {
    for (field, value) in [
        ("T1_current", &row.t1_current),
        ("T1_sota", &row.t1_sota),
        ("margin", &row.margin),
    ] {
        let Some(reason) = value.strip_prefix("absent:") else {
            bail!(
                "{} missing row must use absent:<reason> for {field}",
                row.row_id
            );
        };
        if reason.trim().is_empty() {
            bail!("{} has empty absent reason for {field}", row.row_id);
        }
    }
    if row.tranche_admitted == "ADMITTED" {
        bail!("{} cannot be ADMITTED while absent", row.row_id);
    }
    Ok(())
}

fn parse_delta_number(row_id: &str, field: &str, value: &str) -> Result<f64> {
    if value.starts_with("absent:") {
        bail!("{row_id} has absent {field} where numeric evidence exists");
    }
    value
        .parse::<f64>()
        .with_context(|| format!("{row_id} has malformed {field} `{value}`"))
}

fn require_close_delta(row_id: &str, field: &str, observed: f64, expected: f64) -> Result<()> {
    if (observed - expected).abs() > 0.02 {
        bail!("{row_id} {field} {observed:.2} does not match expected {expected:.2}");
    }
    Ok(())
}

fn parse_results_metrics(
    results_text: &str,
) -> Result<std::collections::BTreeMap<String, ResultsMetric>> {
    let mut metrics = std::collections::BTreeMap::new();
    for line in results_text.lines() {
        let cells = markdown_cells(line);
        if cells.len() < 12 {
            continue;
        }
        let corpus = cells[0].as_str();
        let workload = cells[1].as_str();
        if !SKV13_JSON_CORPORA.contains(&corpus) || !SKV13_JSON_WORKLOADS.contains(&workload) {
            continue;
        }
        let track1 = parse_results_number(corpus, workload, "Track 1 Mbps", &cells[9])?;
        let sonic = parse_results_number(corpus, workload, "sonic-rs strict Mbps", &cells[11])?;
        let row_id = format!("json/{corpus}/{workload}/main");
        metrics.insert(
            row_id,
            ResultsMetric {
                track1_threshold_mbps: track1,
                threshold_mbps: sonic + 1.0,
            },
        );
    }
    if metrics.len() < 41 {
        bail!(
            "RESULTS.md expected at least 41 extant JSON rows before missing typed surfaces, saw {}",
            metrics.len()
        );
    }
    Ok(metrics)
}

fn parse_css_results_metrics(
    results_text: &str,
) -> Result<std::collections::BTreeMap<String, ResultsMetric>> {
    let mut metrics = std::collections::BTreeMap::new();
    for line in results_text.lines() {
        let cells = markdown_cells(line);
        if cells.is_empty()
            || !cells[0].starts_with("css_l4/")
            || !cells[0].ends_with("/direct_to_struct/main")
        {
            continue;
        }
        let row_id = &cells[0];
        let track1 = extract_mbps(line, "track1_generated")?;
        let lightningcss = extract_mbps(line, "lightningcss_strict")?;
        let lightningcss_segment = extract_comparator_segment(line, "lightningcss_strict")?;
        let threshold = extract_keyed_f64(lightningcss_segment, "threshold_mbps=")?;
        require_close_delta(
            row_id,
            "lightningcss threshold",
            threshold,
            lightningcss + 1.0,
        )?;
        metrics.insert(
            row_id.clone(),
            ResultsMetric {
                track1_threshold_mbps: track1,
                threshold_mbps: threshold,
            },
        );
    }
    if !metrics.contains_key("css_l4/declaration_values/direct_to_struct/main") {
        bail!("RESULTS.md missing SK-V12 CSS declaration-values manifest row");
    }
    Ok(metrics)
}

fn parse_results_number(corpus: &str, workload: &str, field: &str, value: &str) -> Result<f64> {
    value
        .parse::<f64>()
        .with_context(|| format!("RESULTS.md {corpus}/{workload} has malformed {field} `{value}`"))
}

fn extract_mbps(line: &str, comparator: &str) -> Result<f64> {
    let comparator_tail = extract_comparator_segment(line, comparator)?;
    extract_keyed_f64(comparator_tail, "mbps=")
}

fn extract_comparator_segment<'a>(line: &'a str, comparator: &str) -> Result<&'a str> {
    let marker = format!("{comparator}[");
    let start = line
        .find(&marker)
        .with_context(|| format!("RESULTS.md CSS row missing {comparator} comparator"))?;
    let comparator_tail = &line[start..];
    let end = comparator_tail
        .find(']')
        .context("RESULTS.md CSS comparator segment is unterminated")?;
    Ok(&comparator_tail[..=end])
}

fn extract_keyed_f64(text: &str, key: &str) -> Result<f64> {
    let start = text
        .find(key)
        .with_context(|| format!("missing numeric key {key}"))?
        + key.len();
    let tail = &text[start..];
    let end = tail
        .find(|ch: char| !(ch.is_ascii_digit() || ch == '.'))
        .unwrap_or(tail.len());
    tail[..end]
        .parse::<f64>()
        .with_context(|| format!("malformed numeric key {key}"))
}

fn markdown_cells(line: &str) -> Vec<String> {
    if !line.trim_start().starts_with('|') {
        return Vec::new();
    }
    line.trim()
        .trim_matches('|')
        .split('|')
        .map(|cell| cell.trim().to_string())
        .collect()
}

fn cost_facts_gate_report(snapshot: &codegen::CostFactsSnapshot) -> Result<serde_json::Value> {
    let diagnostics = snapshot
        .diagnostics
        .iter()
        .map(|diagnostic| {
            serde_json::json!({
                "code": diagnostic.code.as_str(),
                "rule": diagnostic.rule.map(|rule| rule.0),
                "message": diagnostic.message.as_str(),
            })
        })
        .collect::<Vec<_>>();
    let mut manifest = Vec::new();
    for (rule_key, facts) in &snapshot.cost_facts {
        if rule_key != &facts.rule_id.0.to_string() {
            bail!(
                "CostFacts rule key {rule_key} does not match rule id {}",
                facts.rule_id.0
            );
        }
        if facts.rejected.len() < 4 {
            bail!("CostFacts rule {rule_key} has fewer than four rejected alternatives");
        }
        let mut evidence_sources = BTreeSet::new();
        let mut redress_refs = BTreeSet::new();
        let mut rejected_alternative_ids = Vec::new();
        for alternative in &facts.rejected {
            rejected_alternative_ids.push(format!("{:?}", alternative.shape));
            if alternative.evidence.is_empty() {
                evidence_sources.insert("StaticAnalysis".to_string());
                redress_refs.insert("REDRESS-87".to_string());
            } else {
                for measurement in &alternative.evidence {
                    if measurement.source_ref.trim().is_empty() {
                        bail!(
                            "CostFacts rule {rule_key} rejected {:?} has empty source_ref",
                            alternative.shape
                        );
                    }
                    evidence_sources.insert(format!("{:?}", measurement.source));
                    if measurement.source_ref.starts_with("REDRESS-") {
                        redress_refs.insert(
                            measurement
                                .source_ref
                                .split(';')
                                .next()
                                .unwrap_or(measurement.source_ref.as_str())
                                .to_string(),
                        );
                    } else {
                        redress_refs.insert("REDRESS-87".to_string());
                    }
                }
            }
        }
        if redress_refs.is_empty() {
            bail!("CostFacts rule {rule_key} has no REDRESS reference");
        }
        manifest.push(serde_json::json!({
            "grammar": snapshot.grammar.as_str(),
            "rule_id": rule_key,
            "chosen_shape": format!("{:?}", facts.chosen),
            "rejected_alternative_ids": rejected_alternative_ids,
            "evidence_sources": evidence_sources.into_iter().collect::<Vec<_>>(),
            "redress_refs": redress_refs.into_iter().collect::<Vec<_>>(),
            "wave_id": "SK-V8-W1",
        }));
    }
    let report = serde_json::json!({
        "schema": "sk-v8-costfacts-v1",
        "grammar": snapshot.grammar.as_str(),
        "wave_id": "SK-V8-W1",
        "manifest": manifest,
        "cost_facts": &snapshot.cost_facts,
        "diagnostics": [],
        "producer_diagnostics": diagnostics,
    });
    validate_cost_facts_gate_report(&report)?;
    Ok(report)
}

fn validate_cost_facts_gate_report(report: &serde_json::Value) -> Result<()> {
    if report.get("schema").and_then(|value| value.as_str()) != Some("sk-v8-costfacts-v1") {
        bail!("CostFacts gate report has unsupported schema");
    }
    if report.get("wave_id").and_then(|value| value.as_str()) != Some("SK-V8-W1") {
        bail!("CostFacts gate report missing SK-V8-W1 wave id");
    }
    let manifest = report
        .get("manifest")
        .and_then(|value| value.as_array())
        .context("CostFacts gate report missing manifest")?;
    if manifest.is_empty() {
        bail!("CostFacts gate report manifest is empty");
    }
    for entry in manifest {
        let rule_id = nonempty_json_str(entry, "rule_id")?;
        nonempty_json_str(entry, "chosen_shape")?;
        if nonempty_json_array(entry, "rejected_alternative_ids")?.len() < 4 {
            bail!("CostFacts rule {rule_id} missing rejected alternatives");
        }
        nonempty_json_array(entry, "evidence_sources")?;
        nonempty_json_array(entry, "redress_refs")?;
        if entry.get("wave_id").and_then(|value| value.as_str()) != Some("SK-V8-W1") {
            bail!("CostFacts rule {rule_id} missing SK-V8-W1 wave id");
        }
    }
    if let Some(diagnostics) = report.get("diagnostics").and_then(|value| value.as_array()) {
        for diagnostic in diagnostics {
            if diagnostic.get("code").and_then(|value| value.as_str())
                == Some("BBNF-COSTFACTS-MISSING-EVIDENCE")
            {
                bail!("CostFacts gate report contains BBNF-COSTFACTS-MISSING-EVIDENCE");
            }
        }
    }
    Ok(())
}

fn nonempty_json_str<'a>(entry: &'a serde_json::Value, field: &str) -> Result<&'a str> {
    let value = entry
        .get(field)
        .and_then(|value| value.as_str())
        .with_context(|| format!("CostFacts manifest entry missing {field}"))?;
    if value.trim().is_empty() || value == "none:pre-W1" {
        bail!("CostFacts manifest entry has invalid {field}");
    }
    Ok(value)
}

fn nonempty_json_array<'a>(
    entry: &'a serde_json::Value,
    field: &str,
) -> Result<&'a Vec<serde_json::Value>> {
    let values = entry
        .get(field)
        .and_then(|value| value.as_array())
        .with_context(|| format!("CostFacts manifest entry missing {field}"))?;
    if values.is_empty()
        || values
            .iter()
            .any(|value| value.as_str().is_some_and(|text| text.trim().is_empty()))
        || values
            .iter()
            .any(|value| value.as_str() == Some("none:pre-W1"))
    {
        bail!("CostFacts manifest entry has invalid {field}");
    }
    Ok(values)
}

fn primitive_checkasm(root: &Path) -> Result<()> {
    for test in [
        "checkasm_byte_class_from_eq_set_64",
        "checkasm_byte_class_from_table_64",
        "checkasm_bulk_emit_positions_64",
        "checkasm_structural_terminator_64",
        "checkasm_bitmap_prefix_xor_64",
        "checkasm_bitmap_next_set_bit",
        "checkasm_eob_pad_clamp",
        "checkasm_parity",
        "checkasm_utf8_block",
    ] {
        let status = Command::new("cargo")
            .current_dir(root)
            .env("BBNF_SIMD_STRICT", "1")
            .env_remove("BBNF_SIMD_INJECT_BUG")
            .args(["test", "-p", "bbnf-simd", "--release", "--test", test])
            .status()
            .with_context(|| format!("failed to spawn bbnf-simd primitive checkasm gate {test}"))?;
        if !status.success() {
            bail!("bbnf-simd primitive checkasm gate {test} failed with status {status}");
        }
    }
    Ok(())
}

fn rust_loc(path: &Path) -> Result<usize> {
    let mut total = 0;
    for file in rust_files(path)? {
        let source = std::fs::read_to_string(&file)?;
        total += source
            .lines()
            .filter(|line| {
                let line = line.trim();
                !line.is_empty() && !line.starts_with("//")
            })
            .count();
    }
    Ok(total)
}

fn rust_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    if path.is_file() {
        if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
            files.push(path.to_path_buf());
        }
        return Ok(files);
    }

    let mut stack = vec![path.to_path_buf()];
    while let Some(dir) = stack.pop() {
        for entry in std::fs::read_dir(&dir)
            .with_context(|| format!("failed to read directory {}", dir.display()))?
        {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().and_then(|ext| ext.to_str()) == Some("rs") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn workspace_root() -> Result<PathBuf> {
    let mut dir = std::env::current_dir()?;
    loop {
        let cargo_toml = dir.join("Cargo.toml");
        if cargo_toml.exists() {
            let manifest = std::fs::read_to_string(&cargo_toml)?;
            let parsed: toml::Value = toml::from_str(&manifest)?;
            if has_skinny_workspace_metadata(&parsed) {
                return Ok(dir);
            }
        }
        if !dir.pop() {
            bail!("could not find skinny workspace root");
        }
    }
}

fn has_skinny_workspace_metadata(manifest: &toml::Value) -> bool {
    manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("metadata"))
        .and_then(|metadata| metadata.get("bbnf"))
        .and_then(|bbnf| bbnf.get("grammars"))
        .and_then(|grammars| grammars.get("json"))
        .is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    const JSON_GRAMMAR: &str = include_str!("../../grammars/json.bbnf");

    fn report() -> serde_json::Value {
        let snapshot = codegen::cost_facts_from_source("json", JSON_GRAMMAR).unwrap();
        cost_facts_gate_report(&snapshot).unwrap()
    }

    #[test]
    fn gate_json_passthrough_accepts_skv12_non_json_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv12-non-json-report".into(),
            "skv12-nonjson-pass.json".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv12-css-l4-sota-report".into(),
            "skv12-css-pass.json".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-comparator-oracle-report".into(),
            "skv13-css-comparator.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-stylesheet-selectors-report".into(),
            "skv13-css-w2.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-declaration-values-extended-report".into(),
            "skv13-css-w3.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-visual-functions-report".into(),
            "skv13-css-w4.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-at-rules-media-report".into(),
            "skv13-css-w10-1.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-vendor-custom-report".into(),
            "skv13-css-w10-2.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-css-nested-layout-report".into(),
            "skv13-css-w10-3.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-decision-regex-report".into(),
            "skv13-w5.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-decision-active-cost-report".into(),
            "skv13-w6.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-decision-csp-cascade-report".into(),
            "skv13-w7.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-per-grammar-policy-report".into(),
            "skv13-w8.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        validate_gate_json_passthrough(&[
            "--skv13-same-substrate-union-report".into(),
            "skv13-w9.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
        assert!(validate_gate_json_passthrough(&["--skv12-non-json-report".into()]).is_err());
        assert!(validate_gate_json_passthrough(&["--unknown".into()]).is_err());
    }

    #[test]
    fn gate_json_passthrough_accepts_skv13_decision_regex_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv13-decision-regex-report".into(),
            "skv13-w5.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
    }

    #[test]
    fn gate_json_passthrough_accepts_skv13_decision_active_cost_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv13-decision-active-cost-report".into(),
            "skv13-w6.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
    }

    #[test]
    fn gate_json_passthrough_accepts_skv13_decision_csp_cascade_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv13-decision-csp-cascade-report".into(),
            "skv13-w7.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
    }

    #[test]
    fn gate_json_passthrough_accepts_skv13_per_grammar_policy_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv13-per-grammar-policy-report".into(),
            "skv13-w8.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
    }

    #[test]
    fn gate_json_passthrough_accepts_skv13_same_substrate_union_report_flag() {
        validate_gate_json_passthrough(&[
            "--skv13-same-substrate-union-report".into(),
            "skv13-w9.json".into(),
            "--check-results".into(),
        ])
        .unwrap();
    }

    #[test]
    fn w1_costfacts_accepts_and_rejects_required_manifest_fields() {
        assert!(validate_cost_facts_flags(&[
            "--with-cost-facts".into(),
            "--advisory".into(),
            "--check-results".into(),
        ])
        .unwrap());
        assert!(validate_cost_facts_flags(&[
            "--with-cost-facts".into(),
            "--update-results".into(),
        ])
        .is_err());

        let report = report();
        validate_cost_facts_gate_report(&report).unwrap();
        assert_eq!(report["manifest"].as_array().unwrap().len(), 15);

        let manifest_fields = [
            "rule_id",
            "chosen_shape",
            "wave_id",
            "rejected_alternative_ids",
            "evidence_sources",
            "redress_refs",
        ];
        for field in manifest_fields {
            let mut bad = report.clone();
            let entry = bad["manifest"][0].as_object_mut().unwrap();
            if entry[field].is_array() {
                entry.insert(field.into(), serde_json::json!([]));
            } else {
                entry.remove(field);
            }
            assert!(validate_cost_facts_gate_report(&bad).is_err());
        }

        for mutate in [
            |bad: &mut serde_json::Value| bad.as_object_mut().unwrap().remove("wave_id").is_some(),
            |bad: &mut serde_json::Value| {
                bad["manifest"][0]
                    .as_object_mut()
                    .unwrap()
                    .insert("rule_id".into(), serde_json::json!("none:pre-W1"));
                true
            },
        ] {
            let mut bad = report.clone();
            mutate(&mut bad);
            assert!(validate_cost_facts_gate_report(&bad).is_err());
        }
    }

    #[test]
    fn skv13_rolling_delta_accepts_full_json_and_css_universe() {
        let root = std::env::temp_dir().join(format!(
            "skv13-rolling-delta-{}-{}",
            std::process::id(),
            "results"
        ));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();

        let mut results = Vec::new();
        let mut rolling = vec![
            "# Rolling SOTA Delta".to_string(),
            "".to_string(),
            "schema_version: sk-v13-rolling-sota-delta-v1".to_string(),
            "run_id: SK-V13-open".to_string(),
            "g_omega_status: signed".to_string(),
            "consumer_gate: cargo xtask gate-json --check-results".to_string(),
            "".to_string(),
            "| row | plane | T1_current | T1_sota | margin | tranche_admitted |".to_string(),
            "|---|---|---:|---:|---:|---|".to_string(),
        ];
        for corpus in SKV13_JSON_CORPORA {
            for workload in SKV13_JSON_WORKLOADS {
                results.push(format!(
                    "| {corpus} | {workload} | A | GO | strict | measured-row | yes | probe | {workload} | 200.00 | 190.00 | 100.00 |"
                ));
                rolling.push(format!(
                    "| json/{corpus}/{workload}/main | {workload} | 200.00 | 101.00 | 99.00 | ADMITTED |"
                ));
            }
        }
        results.push(
            "| css_l4/declaration_values/direct_to_struct/main | css_l4 | non_json_generated:css_l4:declaration_values | SK-V12-W1b-2b | run | equality | gate | samples | 30 | flags | host | features | schema | REDRESS-125 | delta | generated_css_l4_declaration_values | css_l4_declaration_value_fact_stream | one | companion_gate_css_l4_lightningcss_sota | independent | parity | track1_generated[plane=css_l4_declaration_value_fact_stream,strictness=strict,freshness=same-run-native,sidecar=n/a,mbps=429.34,source=track1]; lightningcss_strict[plane=css_l4_declaration_value_fact_stream,strictness=strict,freshness=same-run-native,sidecar=same-plane-source-sidecar,mbps=168.93,threshold_mbps=169.93,source=lightningcss] |".to_string(),
        );
        rolling.push(
            "| css_l4/declaration_values/direct_to_struct/main | css_l4_parity | 429.34 | 169.93 | 259.41 | ADMITTED |".to_string(),
        );
        for feature in SKV13_CSS_FEATURES
            .iter()
            .filter(|feature| **feature != "declaration_values")
        {
            rolling.push(format!(
                "| css_l4/{feature}/direct_to_struct/main | css_l4_parity | absent:not-yet-generated | absent:not-yet-generated | absent:not-yet-generated | OPEN |"
            ));
        }
        let rolling_path = root.join("ROLLING-SOTA-DELTA.md");
        std::fs::write(&rolling_path, rolling.join("\n")).unwrap();

        validate_skv13_rolling_delta(&results.join("\n"), &rolling_path).unwrap();

        let malformed = std::fs::read_to_string(&rolling_path).unwrap().replace(
            "| json/twitter/parse_only/main | parse_only | 200.00 | 101.00 | 99.00 | ADMITTED |",
            "| json/twitter/parse_only/main | parse_only | 200.00 | 101.00 | 98.00 | ADMITTED |",
        );
        let bad_path = root.join("ROLLING-SOTA-DELTA.bad.md");
        std::fs::write(&bad_path, malformed).unwrap();
        assert!(
            validate_skv13_rolling_delta(&results.join("\n"), &bad_path).is_err(),
            "rolling delta must reject malformed margins"
        );

        let stale_threshold = results
            .join("\n")
            .replace("threshold_mbps=169.93", "threshold_mbps=170.93");
        assert!(
            validate_skv13_rolling_delta(&stale_threshold, &rolling_path).is_err(),
            "rolling delta must reject stale CSS threshold math"
        );

        let _ = std::fs::remove_dir_all(&root);
    }
}
