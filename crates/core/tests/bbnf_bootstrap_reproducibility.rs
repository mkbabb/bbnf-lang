//! AZ-II.cutover.B — permanent CI gate for BBNF bootstrap reproducibility.
//!
//! Encodes the Stage A / Stage B byte-equal property as a repeatable
//! test: regenerate the BBNF grammar twice in succession into a fresh
//! tempdir and assert the two outputs are byte-identical. The test
//! catches non-idempotent regen drift before it ships — any failure
//! surfaces a regen pass that depends on hidden mutable state across
//! invocations (cache pollution, monotonic counters, hash-randomised
//! iteration order, etc.).
//!
//! # Mechanism
//!
//! The test invokes `cargo xtask regen --grammar bbnf --output <dir>`
//! twice, once each into two sibling subdirectories of a single
//! tempdir. The `--output` flag (added in cutover.B alongside this
//! test) routes the regen emission to the supplied path instead of
//! the checked-in `crates/core/src/grammar/generated/` tree, so the
//! workspace is left untouched. Comparing the two emitted files byte-
//! by-byte exercises the property.
//!
//! # Why two stages
//!
//! AZ-II.cutover targets the BBNF self-host: the bootstrap parser
//! that compiles every grammar in the workspace is itself produced
//! by the bootstrap parser. A non-idempotent regen pass would mean
//! that running cutover twice produces different bytes — and would
//! mean the bootstrap is not a fixed point. The byte-equal property
//! is the axiomatic discipline for any self-hosted compiler.
//!
//! # Failure surface
//!
//! On byte-divergence the test prints a unified diff snippet covering
//! the first mismatching region (~120 surrounding lines) so the
//! offending regen pass is locatable from CI logs alone.

use std::path::Path;
use std::process::Command;

/// Locate the workspace root. Walk upward from the test binary's
/// location until a directory containing `Cargo.toml` with a
/// `[workspace]` table appears. Stops at filesystem root with a
/// panic — the test is workspace-bound.
fn workspace_root() -> std::path::PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let mut p = Path::new(manifest_dir).to_path_buf();
    loop {
        let candidate = p.join("Cargo.toml");
        if candidate.exists() {
            let bytes = std::fs::read_to_string(&candidate)
                .expect("workspace Cargo.toml read");
            if bytes.contains("[workspace]") {
                return p;
            }
        }
        if !p.pop() {
            panic!(
                "bbnf_bootstrap_reproducibility: workspace root not found \
                 (started from {manifest_dir:?})"
            );
        }
    }
}

/// Invoke `cargo xtask regen --grammar <ident> --output <dest_dir>`
/// from `workspace_root`. Panics with the captured stderr on
/// non-zero exit so the test failure surfaces actionable detail.
fn run_xtask_regen(workspace_root: &Path, dest_dir: &Path, grammar: &str) {
    let status = Command::new(env!("CARGO"))
        .current_dir(workspace_root)
        .args([
            "xtask",
            "regen",
            "--grammar",
            grammar,
            "--output",
        ])
        .arg(dest_dir)
        .output()
        .expect("spawn cargo xtask regen");
    if !status.status.success() {
        panic!(
            "cargo xtask regen --grammar {grammar} --output {dest:?} failed:\n\
             ── stdout ──\n{stdout}\n── stderr ──\n{stderr}",
            dest = dest_dir,
            stdout = String::from_utf8_lossy(&status.stdout),
            stderr = String::from_utf8_lossy(&status.stderr),
        );
    }
}

/// Compute the first divergent byte position between `a` and `b`,
/// returning `Some(idx)` on mismatch and `None` on equality. Used by
/// the diff snippet builder to focus the failure surface on the
/// offending region instead of dumping the full multi-megabyte
/// emission.
fn first_divergence(a: &[u8], b: &[u8]) -> Option<usize> {
    a.iter().zip(b).position(|(x, y)| x != y).or_else(|| {
        if a.len() != b.len() {
            Some(a.len().min(b.len()))
        } else {
            None
        }
    })
}

/// Build a diff snippet anchored at `divergence`. Prints up to
/// `surrounding` bytes on each side of the divergence, with column
/// pointers under the differing positions. The snippet is meant for
/// CI log readability — full diffs live in the captured stage files
/// the test writes to its tempdir on failure.
fn diff_snippet(stage_a: &[u8], stage_b: &[u8], divergence: usize) -> String {
    const WINDOW: usize = 240;
    let lo = divergence.saturating_sub(WINDOW);
    let hi_a = (divergence + WINDOW).min(stage_a.len());
    let hi_b = (divergence + WINDOW).min(stage_b.len());

    let a_slice = String::from_utf8_lossy(&stage_a[lo..hi_a]);
    let b_slice = String::from_utf8_lossy(&stage_b[lo..hi_b]);

    format!(
        "byte divergence at offset {divergence} \
         (stage-a len {a_len}, stage-b len {b_len})\n\
         ── stage-a (offset {lo}..{hi_a}) ──\n{a_slice}\n\
         ── stage-b (offset {lo}..{hi_b}) ──\n{b_slice}\n",
        a_len = stage_a.len(),
        b_len = stage_b.len(),
    )
}

/// Run the byte-equal idempotence check for a single grammar by
/// regenerating it twice into sibling subdirectories of a fresh
/// tempdir and comparing the two emissions byte-by-byte.
fn assert_grammar_regen_idempotent(grammar: &str, output_filename: &str) {
    let workspace = workspace_root();
    let tmpdir = tempfile::tempdir().expect("create tempdir for regen stages");

    let stage_a_dir = tmpdir.path().join("stage-a");
    let stage_b_dir = tmpdir.path().join("stage-b");
    std::fs::create_dir_all(&stage_a_dir).expect("create stage-a dir");
    std::fs::create_dir_all(&stage_b_dir).expect("create stage-b dir");

    run_xtask_regen(&workspace, &stage_a_dir, grammar);
    let stage_a_path = stage_a_dir.join(output_filename);
    let stage_a_bytes = std::fs::read(&stage_a_path)
        .unwrap_or_else(|err| panic!("read stage-a {output_filename}: {err}"));

    run_xtask_regen(&workspace, &stage_b_dir, grammar);
    let stage_b_path = stage_b_dir.join(output_filename);
    let stage_b_bytes = std::fs::read(&stage_b_path)
        .unwrap_or_else(|err| panic!("read stage-b {output_filename}: {err}"));

    if let Some(divergence) = first_divergence(&stage_a_bytes, &stage_b_bytes) {
        panic!(
            "{grammar} regen is NOT idempotent: stage-a and stage-b emissions \
             diverge.\n\n{}",
            diff_snippet(&stage_a_bytes, &stage_b_bytes, divergence),
        );
    }

    assert!(
        !stage_a_bytes.is_empty(),
        "stage-a {output_filename} is empty — xtask regen produced no output"
    );
    assert_eq!(
        stage_a_bytes.len(),
        stage_b_bytes.len(),
        "stage-a / stage-b byte-length mismatch despite divergence-search returning None"
    );
}

#[test]
fn bbnf_regen_is_idempotent() {
    assert_grammar_regen_idempotent("bbnf", "bbnf.rs");
}

/// AZ-II.cutover.K Phase 1 — extend the reproducibility CI gate to
/// the JSON grammar. cutover.J's diagnosis surfaced a regression
/// that hid under the BBNF-only gate: typed-leaf projection drops
/// source text, so the value-expression lower passes that classify
/// `mapped_factor` mappings by source text panic on a clean regen
/// of any grammar with `-> Bool` / `-> 0u8` annotations. Pinning
/// JSON's regen idempotence catches the regression at the test
/// boundary instead of waiting for emitter divergence.
#[test]
fn json_regen_is_idempotent() {
    assert_grammar_regen_idempotent("json", "json.rs");
}
