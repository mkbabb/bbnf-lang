use anyhow::{bail, Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;

const USAGE: &str = "usage: cargo xtask <regen-json|check-json|check-conformance|lint-loc|bench-json|gate-json|primitive-checkasm>";

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let Some(command) = args.next() else {
        bail!("{USAGE}");
    };

    let root = workspace_root()?;
    match command.as_str() {
        "regen-json" => regen_json(&root),
        "check-json" => check_json(&root),
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
    let emitted = codegen::emit_json_from_source(&source)?;
    emitted.write_to_dir(root.join("crates/runtime/src/grammars/json"))?;
    Ok(())
}

fn check_json(root: &Path) -> Result<()> {
    let source = std::fs::read_to_string(root.join("grammars/json.bbnf"))?;
    let emitted = codegen::emit_json_from_source(&source)?;
    emitted
        .check_dir(root.join("crates/runtime/src/grammars/json"))
        .context("generated JSON runtime is stale; run `cargo xtask regen-json`")
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
    let full_run = passthrough.is_empty();
    let mut command = Command::new("cargo");
    command
        .current_dir(root)
        .arg("bench")
        .arg("-p")
        .arg("bbnf-bench");
    if !full_run {
        command.arg("--").args(passthrough);
    }
    let status = command.status().context("failed to spawn cargo bench")?;
    if status.success() {
        if full_run {
            gate_json(root, Vec::new())
        } else {
            Ok(())
        }
    } else {
        bail!("cargo bench failed with status {status}")
    }
}

fn gate_json(root: &Path, passthrough: Vec<String>) -> Result<()> {
    let status = Command::new("cargo")
        .current_dir(root)
        .args(["run", "-p", "bbnf-bench", "--bin", "gate"])
        .arg("--")
        .args(passthrough)
        .status()
        .context("failed to spawn bench gate")?;
    if status.success() {
        Ok(())
    } else {
        bail!("bench gate failed with status {status}")
    }
}

fn primitive_checkasm(root: &Path) -> Result<()> {
    for test in ["checkasm_parity", "checkasm_utf8_block"] {
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
