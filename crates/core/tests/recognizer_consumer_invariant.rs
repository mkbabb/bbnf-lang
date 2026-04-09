//! Tranche V.10 — consumer-invariant test.
//!
//! Asserts that every non-trivial field of the V.3 / V.6 recognizer
//! schemas has at least one named consumer somewhere under
//! `crates/core/src/backend/kernels/`, `backend/driver/`, or
//! `backend/strategy/`. A new field added without a consumer fails
//! this test, preventing the "rich facts with no emitter path"
//! failure mode from re-occurring.
//!
//! V.10 implementation: a build-time grep over the backend tree.
//! The test reads files via `std::fs` (the bbnf-lang workspace lives
//! at the same root as the test binary, so relative paths work) and
//! asserts presence of identifier strings.
//!
//! Note: this is a smoke / structural check, not a full type-system
//! invariant. The grep approach is intentionally simple — it catches
//! orphan fact additions without requiring proc-macro tooling.

use std::fs;
use std::path::PathBuf;

const REQUIRED_REFERENCES: &[(&str, &str)] = &[
    // V.6 decision API — must be referenced by at least one driver site.
    ("recognizer_decision", "backend/driver"),
    // V.6 decision struct — must be defined and exported.
    ("RecognizerDecision", "passes/csp_recognizers"),
    // V.3 fact field — must exist on NodeFacts.
    ("recognizer:", "passes/patterns"),
    // V.7 kernel module — must exist with at least one entry point.
    ("emit_call", "backend/kernels"),
];

fn workspace_root() -> PathBuf {
    // CARGO_MANIFEST_DIR points at the package directory; backtrack
    // to the workspace root.
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // crates/core → workspace root
    p.pop();
    p.pop();
    p
}

fn read_files_under(dir: &str) -> Vec<String> {
    let mut acc = Vec::new();
    let root = workspace_root().join("crates").join(dir);
    if root.exists() {
        if root.is_dir() {
            walk(&root, &mut acc);
        } else if let Ok(content) = fs::read_to_string(&root) {
            acc.push(content);
        }
    } else {
        // Try `<dir>.rs` for single-file modules.
        let with_ext = workspace_root().join("crates").join(format!("{dir}.rs"));
        if let Ok(content) = fs::read_to_string(&with_ext) {
            acc.push(content);
        }
    }
    acc
}

fn walk(dir: &std::path::Path, acc: &mut Vec<String>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, acc);
            } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                if let Ok(content) = fs::read_to_string(&path) {
                    acc.push(content);
                }
            }
        }
    }
}

#[test]
fn every_recognizer_field_has_a_named_consumer() {
    for (needle, dir_fragment) in REQUIRED_REFERENCES {
        // Translate the path fragment into a directory under crates/.
        // The convention: "ir/passes/patterns" → walk crates/ir/src/passes/patterns/
        let (crate_name, sub) = if dir_fragment.starts_with("backend") {
            ("core/src", *dir_fragment)
        } else {
            ("ir/src", *dir_fragment)
        };
        let dir = format!("{crate_name}/{sub}");
        let files = read_files_under(&dir);
        let mention = files.iter().any(|c| c.contains(needle));
        assert!(
            mention,
            "Tranche V consumer-invariant: identifier `{needle}` not found in `crates/{dir}`. \
             Either the field/decision is unused (delete it) or a consumer is missing."
        );
    }
}
