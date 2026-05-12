use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let Some(command) = args.next() else {
        print_help();
        bail!("missing xtask command");
    };

    let root = workspace_root()?;
    match command.as_str() {
        "regen-json" => regen_json(&root),
        "check-json" => check_json(&root),
        "lint-loc" => lint_loc(&root),
        "bench-json" => bench_json(&root, args.collect()),
        "gate-json" => gate_json(&root),
        "help" | "--help" | "-h" => {
            print_help();
            Ok(())
        }
        other => bail!("unknown xtask command `{other}`"),
    }
}

fn print_help() {
    eprintln!("usage: cargo xtask <regen-json|check-json|lint-loc|bench-json|gate-json>");
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
        ("crates/simd-scan", 3500),
        ("crates/bbnf-bench", 2000),
        ("crates/test-fixtures", 800),
        ("xtask", 350),
    ];
    let mut failures = Vec::new();
    for (path, budget) in budgets {
        let loc = rust_loc(&root.join(path))?;
        println!("{path}: {loc}/{budget} LOC");
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
            gate_json(root)
        } else {
            Ok(())
        }
    } else {
        bail!("cargo bench failed with status {status}")
    }
}

fn gate_json(root: &Path) -> Result<()> {
    let status = Command::new("cargo")
        .current_dir(root)
        .args(["run", "-p", "bbnf-bench", "--bin", "gate"])
        .status()
        .context("failed to spawn bench gate")?;
    if status.success() {
        Ok(())
    } else {
        bail!("bench gate failed with status {status}")
    }
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

#[allow(dead_code)]
fn metadata_table(manifest: &toml::Value) -> BTreeMap<String, String> {
    manifest
        .get("workspace")
        .and_then(|workspace| workspace.get("metadata"))
        .and_then(|metadata| metadata.get("bbnf"))
        .and_then(|bbnf| bbnf.as_table())
        .map(|table| {
            table
                .iter()
                .map(|(key, value)| (key.clone(), value.to_string()))
                .collect()
        })
        .unwrap_or_default()
}
