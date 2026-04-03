use std::collections::VecDeque;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

/// Build script for bbnf-derive.
///
/// Emits a `BBNF_DERIVE_BUILD_ID` derived from the source trees that affect
/// generated parser output. This makes the proc-macro cache key change when
/// the derive crate, shared pipeline, or IR/codegen support changes, even if
/// the on-disk `.bbnf-cache` survives between builds.
fn main() {
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let tracked_roots = [
        manifest_dir.join("Cargo.toml"),
        manifest_dir.join("build.rs"),
        manifest_dir.join("src"),
        manifest_dir.join("../bbnf/Cargo.toml"),
        manifest_dir.join("../bbnf/src"),
        manifest_dir.join("../bbnf-ir/Cargo.toml"),
        manifest_dir.join("../bbnf-ir/src"),
    ];

    let mut files = Vec::new();
    for root in tracked_roots {
        collect_files(&root, &mut files);
    }
    files.sort();

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for file in files {
        println!("cargo:rerun-if-changed={}", file.display());
        file.hash(&mut hasher);
        if let Ok(bytes) = std::fs::read(&file) {
            bytes.hash(&mut hasher);
        }
    }

    println!(
        "cargo:rustc-env=BBNF_DERIVE_BUILD_ID={:016x}",
        hasher.finish()
    );
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
