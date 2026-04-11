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
    let tracked_roots = [
        manifest_dir.join("Cargo.toml"),
        manifest_dir.join("build.rs"),
        manifest_dir.join("src"),
        manifest_dir.join("../core/Cargo.toml"),
        manifest_dir.join("../core/src"),
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
