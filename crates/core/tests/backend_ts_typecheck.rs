//! Tempdir TS typecheck harness for the four representative grammars
//! (JSON, CSS L4, Sheets, BBNF). Emits the TS source for each grammar
//! into a temporary directory and invokes the system `tsc` compiler in
//! `--noEmit --strict` mode; the test passes when each emitted file is
//! both syntactically valid and type-correct.
//!
//! AZ-IV.W1.4 hard-gate: build-time correctness only — Node execution
//! lives in W5. The harness is gated on the presence of `tsc` so that
//! environments without TypeScript installed still build/run the rest
//! of the suite.

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use std::path::{Path, PathBuf};
use std::process::Command;

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

/// Compile a grammar entry path through the TS pipeline and return the
/// emitted source. Imports under the entry path resolve via
/// `compile_paths_request`'s normal import-graph traversal.
fn compile_grammar_to_ts(entry: &Path) -> String {
    let paths = vec![entry.to_path_buf()];
    match compile_paths_request(&paths, &ts_request()).expect("ts compile") {
        CompileOutput::Ts(source) => source,
        other => panic!("expected TS output, got {other:?}"),
    }
}

/// Locate `tsc` on PATH. Returns `None` when the TypeScript compiler
/// is unavailable so the harness short-circuits gracefully on
/// minimal CI images.
fn locate_tsc() -> Option<PathBuf> {
    let probe = Command::new("tsc").arg("--version").output().ok()?;
    if probe.status.success() {
        Some(PathBuf::from("tsc"))
    } else {
        None
    }
}

/// Run `tsc --noEmit --strict --target es2020 --lib es2020,dom <file>`
/// and panic with the captured diagnostics on failure.
fn run_tsc(tsc: &Path, file: &Path) {
    let output = Command::new(tsc)
        .args([
            "--noEmit",
            "--strict",
            "--target",
            "es2020",
            "--lib",
            "es2020,dom",
            "--moduleDetection",
            "force",
            "--skipLibCheck",
        ])
        .arg(file)
        .output()
        .expect("invoke tsc");
    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let source = std::fs::read_to_string(file).unwrap_or_default();
        panic!(
            "tsc rejected emitted TS for {}:\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}\n--- source ---\n{source}",
            file.display(),
        );
    }
}

/// Emit the four representative grammars into a tempdir and typecheck
/// each via the system `tsc`. Skipped when `tsc` is missing.
#[test]
fn ts_tempdir_typecheck_representative_grammars() {
    let Some(tsc) = locate_tsc() else {
        eprintln!("tsc not found on PATH; skipping ts_tempdir_typecheck");
        return;
    };

    let workspace = workspace_root();
    let dir = tempfile::tempdir().expect("create tempdir");

    let grammars: &[(&str, &str)] = &[
        ("json", "grammar/json/json.bbnf"),
        ("css_l4", "grammar/css/l4/stylesheet.bbnf"),
        ("google_sheets", "grammar/google-sheets/google-sheets.bbnf"),
        ("bbnf", "grammar/bbnf/bbnf.bbnf"),
    ];

    for (ident, rel) in grammars {
        let entry = workspace.join(rel);
        let ts = compile_grammar_to_ts(&entry);
        let out = dir.path().join(format!("{ident}.ts"));
        std::fs::write(&out, &ts).expect("write emitted ts");
        run_tsc(&tsc, &out);
    }
}

/// Resolve the workspace root from the cargo manifest dir of the
/// `bbnf` crate. Tests run with `cwd = crates/core/`, so the grammar/
/// directory lives two levels up.
fn workspace_root() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .parent()
        .and_then(|p| p.parent())
        .map(Path::to_path_buf)
        .expect("workspace root")
}
