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
    let unexpected = passthrough
        .iter()
        .filter(|arg| {
            !matches!(
                arg.as_str(),
                "--advisory"
                    | "--check-results"
                    | "--update-results"
                    | "--write-results"
                    | "--include-volatile-probes"
            )
        })
        .collect::<Vec<_>>();
    if !unexpected.is_empty() {
        bail!("gate-json got unsupported arguments {unexpected:?}");
    }
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
    const SK_V10_OPENING_MANIFEST_ROWS: usize = 40;

    let text = std::fs::read_to_string(root.join("RESULTS.md"))
        .context("gate-json --with-cost-facts --check-results requires RESULTS.md")?;
    let manifest_rows = text
        .lines()
        .filter(|line| line.starts_with("| json/"))
        .count();
    if manifest_rows != SK_V10_OPENING_MANIFEST_ROWS {
        bail!(
            "RESULTS.md SK-V10 opening manifest row count moved from {} to {manifest_rows}",
            SK_V10_OPENING_MANIFEST_ROWS
        );
    }
    for required in ["SK-V9-open", "none:pre-W1:none:pre-W1:none:pre-W1"] {
        if !text.contains(required) {
            bail!("RESULTS.md missing W0 snapshot marker {required}");
        }
    }
    if !text.contains("structural_scan+masking_probes+pmu+cycles:nonproducer") {
        bail!("RESULTS.md missing W0 diagnostic nonproducer marker");
    }
    let mut run_ids = BTreeSet::new();
    for line in text.lines().filter(|line| line.starts_with("| json/")) {
        let Some(offset) = line.find("sk-v9-open:criterion-fnv64-") else {
            bail!("RESULTS.md manifest row missing SK-V9 run id");
        };
        let end = offset + "sk-v9-open:criterion-fnv64-".len() + 16;
        let Some(run_id) = line.get(offset..end) else {
            bail!("RESULTS.md manifest row has truncated SK-V9 run id");
        };
        if !run_id
            .strip_prefix("sk-v9-open:criterion-fnv64-")
            .is_some_and(|suffix| {
                suffix.len() == 16
                    && suffix
                        .bytes()
                        .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
            })
        {
            bail!("RESULTS.md manifest row has malformed SK-V9 run id `{run_id}`");
        }
        run_ids.insert(run_id);
    }
    if run_ids.len() != 1 {
        bail!("RESULTS.md SK-V9 manifest run id is not uniform: {run_ids:?}");
    }
    Ok(())
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
}
