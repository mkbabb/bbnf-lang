use std::collections::VecDeque;
use std::path::{Path, PathBuf};

/// Build script for bbnf-derive.
///
/// Emits `cargo:rerun-if-changed=...` directives over the source trees that
/// affect generated parser output, so cargo re-runs the proc-macro whenever
/// the derive crate, shared pipeline, or IR/codegen support changes.
///
/// This script is a cargo-reload signal ONLY. It does NOT export any env var
/// that feeds the proc-macro cache key — cache invalidation across schema
/// changes is handled by the manually-bumped `BBNF_SCHEMA_VERSION` const in
/// `src/lib.rs`. Splitting these two concerns means unrelated edits to the
/// derive/core/ir source trees no longer bust the on-disk `.bbnf-cache`.
fn main() {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    // Only the subtrees that actually influence `#[derive(Parser)]`
    // emission output are tracked here. The proc-macro cache in
    // `src/lib.rs` is content-keyed on the grammar bytes + attrs +
    // schema version, so unrelated bbnf source edits under `runtime/`,
    // `backend/wasm/`, `backend/ts/`, `imports/`, `graph/`, or at
    // `lib.rs`/`types.rs` do NOT change derive output — tracking them
    // here only fires a spurious fingerprint cascade that re-rehydrates
    // 60+ cross-crate derive TokenStreams for zero output delta.
    let tracked_roots = [
        manifest_dir.join("Cargo.toml"),
        manifest_dir.join("build.rs"),
        manifest_dir.join("src"),
        manifest_dir.join("../core/Cargo.toml"),
        manifest_dir.join("../core/src/backend/rust"),
        manifest_dir.join("../core/src/generate"),
        manifest_dir.join("../core/src/grammar"),
        manifest_dir.join("../core/src/pipeline"),
        manifest_dir.join("../core/src/pipeline.rs"),
        manifest_dir.join("../core/src/lower"),
        manifest_dir.join("../ir/Cargo.toml"),
        manifest_dir.join("../ir/src"),
    ];

    let mut files = Vec::new();
    for root in tracked_roots {
        collect_files(&root, &mut files);
    }
    files.sort();

    for file in files {
        println!("cargo:rerun-if-changed={}", file.display());
    }
}

fn collect_files(root: &Path, out: &mut Vec<PathBuf>) {
    if !root.exists() {
        return;
    }
    if root.is_file() {
        out.push(root.to_path_buf());
        return;
    }

    let mut queue = VecDeque::from([root.to_path_buf()]);
    while let Some(dir) = queue.pop_front() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                queue.push_back(path);
            } else if path.is_file() {
                out.push(path);
            }
        }
    }
}
