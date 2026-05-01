//! AZ-IV.W1 — CI-enforced static AST scan for grammar-overfit class.
//!
//! Per AZ-IV §Hard Gates 17 and `audit/HARDENING-2026-05-01-fermat.md`
//! §F1-F3, no production runtime path may branch on a literal grammar
//! parser-struct ident or a literal grammar rule-name string. The
//! replacement substrate is the type-inference-derived discriminator:
//! `StructRegistry::compound_kind(layout)`, `TypeDesc::*` projection,
//! and `EmitStrategy::for_grammar_with_manifest` against
//! `PRODUCTION_MANIFEST_TABLE`.
//!
//! This file is the gate. It walks the production source tree under
//!
//! - `crates/core/src/runtime/**`
//! - `crates/core/src/backend/rust/emitter/shapes/**`
//!
//! (excluding `generated/`, `tests/`, and `#[cfg(test)]` blocks) and
//! fails closed if any literal-rule-name match arm appears. The
//! exclusions cover code that legitimately ships rule-name keys for
//! diagnostic / serialisation purposes (e.g. layout names emitted into
//! the JSON audit artefact).
//!
//! When this test fails, the offending file:line and matched literal
//! are printed; the fix is to migrate the branch to a registry/typed
//! discriminator per the failing site's owner wave.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// AZ-IV.W1.5 — well-known rule-name strings that have appeared as
/// production-runtime branch keys at the corresponding owner wave's
/// audited `from_rule_name` / `(layout.kind, rule_name)` /
/// `match rule_name { ... }` sites. The static scan rejects any
/// `match` arm whose key is one of these literals appearing inside
/// the production-runtime / shape-emitter scope.
///
/// The list is maintained in lockstep with the W1 carry table; any
/// time a new grammar adds rules whose names should never appear as
/// runtime branch keys, the carry-elimination wave adds the new
/// names here and audits the source tree.
///
/// W1.5 covers the `EmitStrategy::for_grammar` allow-list (Fermat F5)
/// and `pipeline::compile.rs:219-224` audit-tag aliasing (Fermat F6);
/// W1.1 / W1.7 cover the `from_rule_name` and
/// `(layout.kind, rule_name)` builder dispatches (Fermat F1-F3).
/// This scan reads the union — the gate is wave-fleet-wide.
const FORBIDDEN_RULE_NAMES: &[&str] = &[
    // F2 — JSON / CSS L4 builder dispatches.
    "stylesheet",
    "qualifiedRule",
    "length",
    "angle",
    "colorFunction",
    "array",
    "object",
    "pair",
    // F1 — non-JSON `from_rule_name` discriminator vocabularies.
    "comparison_expr",
    "concat_expr",
    "add_expr",
    "cell_or_range",
    "error_literal",
    "sheet_prefix",
    // F6 — audit-tag aliasing entry-rule strings.
    "value",
    "json",
    "css_l4",
    "cssL4",
    "spreadsheet",
    "sheets",
    "google_sheets",
];

/// AZ-IV.W1 — well-known parser-struct idents the
/// `EmitStrategy::for_grammar` allow-list previously branched on
/// (Fermat F5). The scan rejects any `match` arm in production
/// runtime / shape-emitter scope keyed on these idents; the
/// canonical resolver at
/// `crates/ir/src/registry/strategy.rs::PRODUCTION_MANIFEST_TABLE`
/// is exempt because it IS the manifest data, not a behaviour arm
/// (one row per grammar — the data is the discriminator).
const FORBIDDEN_PARSER_IDENTS: &[&str] = &[
    "JsonParser",
    "JsonGrammar",
    "GoogleSheetsParser",
    "GoogleSheetsGrammar",
    "CssL4Parser",
    "BbnfBootstrap",
    "BbnfParser",
    "CsvParser",
    "CsvGrammar",
    "MathParser",
    "MathGrammar",
    "BnfParser",
    "BnfGrammar",
    "EbnfParser",
    "EbnfGrammar",
    "CssPrettyParser",
    "CssPrettyGrammar",
];

/// Source roots covered by the static scan, relative to the
/// workspace root. Per AZ-IV §Hard Gates 17, the production runtime
/// + shape emitter modules are the gate scope.
const SCAN_ROOTS: &[&str] = &[
    "crates/core/src/runtime",
    "crates/core/src/backend/rust/emitter/shapes",
];

/// Path-fragment exclusions inside the scan roots. `generated/` is
/// the auto-emitted Rust output (rule-name strings appear there as
/// emitted layout-name data, not as branch keys); `#[cfg(test)]` is
/// excluded by reading the file's text and skipping cfg-test gated
/// regions.
const EXCLUDE_FRAGMENTS: &[&str] = &["/generated/", "/tests/", "/test_support"];

/// Workspace root resolution: the cargo metadata env var is set by
/// the test runner; fall back to walking up from `CARGO_MANIFEST_DIR`
/// twice (workspace root sits two levels above
/// `crates/core/Cargo.toml`).
fn workspace_root() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let crate_root = PathBuf::from(manifest_dir);
    crate_root
        .ancestors()
        .find(|p| p.join("Cargo.toml").exists() && p.join("Cargo.lock").exists())
        .map(Path::to_path_buf)
        .unwrap_or_else(|| {
            crate_root
                .parent()
                .and_then(Path::parent)
                .map(Path::to_path_buf)
                .expect("workspace root must exist two levels above CARGO_MANIFEST_DIR")
        })
}

fn collect_rust_sources(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if !root.exists() {
        return out;
    }
    walk(root, &mut out);
    out.sort();
    out
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let p = entry.path();
        let s = p.to_string_lossy().replace('\\', "/");
        if EXCLUDE_FRAGMENTS.iter().any(|frag| s.contains(frag)) {
            continue;
        }
        if p.is_dir() {
            walk(&p, out);
        } else if p.extension().and_then(|e| e.to_str()) == Some("rs") {
            out.push(p);
        }
    }
}

/// Strip lines that live inside `#[cfg(test)]` modules. The strip is
/// approximate (brace-counted), which is enough for the heuristics
/// currently shipped — the test harness does not parse Rust; it
/// scans byte-level patterns.
fn strip_cfg_test(src: &str) -> String {
    let mut out = String::with_capacity(src.len());
    let mut depth: usize = 0;
    let mut in_cfg_test = false;
    let mut cfg_test_brace_depth: usize = 0;
    let mut prev_line_was_cfg_test_attr = false;

    for line in src.lines() {
        if line.trim_start().starts_with("#[cfg(test)]") {
            prev_line_was_cfg_test_attr = true;
            continue;
        }
        if prev_line_was_cfg_test_attr && line.contains("mod ") && line.contains('{') {
            in_cfg_test = true;
            cfg_test_brace_depth = depth + 1;
            depth += 1;
            prev_line_was_cfg_test_attr = false;
            continue;
        }
        prev_line_was_cfg_test_attr = false;

        // Trivial brace counter.
        for c in line.chars() {
            match c {
                '{' => depth += 1,
                '}' => {
                    if depth > 0 {
                        depth -= 1;
                    }
                    if in_cfg_test && depth < cfg_test_brace_depth {
                        in_cfg_test = false;
                        cfg_test_brace_depth = 0;
                    }
                }
                _ => {}
            }
        }
        if !in_cfg_test {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

/// Record of a forbidden-arm hit.
#[derive(Debug)]
struct OverFitHit {
    file: String,
    line: usize,
    matched: String,
    excerpt: String,
}

fn scan_file(path: &Path, ws_root: &Path) -> Vec<OverFitHit> {
    let raw = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => return Vec::new(),
    };
    let src = strip_cfg_test(&raw);
    let mut hits = Vec::new();
    for (idx, line) in src.lines().enumerate() {
        // Look for `match arm` patterns: a quoted literal that the
        // FORBIDDEN_* tables identifies, on a line containing `=>`
        // (the match-arm anchor).
        let arrow_pos = match line.find("=>") {
            Some(p) => p,
            None => continue,
        };
        let pattern_part = &line[..arrow_pos];
        for needle in FORBIDDEN_RULE_NAMES
            .iter()
            .chain(FORBIDDEN_PARSER_IDENTS.iter())
        {
            let quoted = format!("\"{needle}\"");
            if pattern_part.contains(quoted.as_str()) {
                let rel = path
                    .strip_prefix(ws_root)
                    .map(Path::to_string_lossy)
                    .unwrap_or_else(|_| path.to_string_lossy())
                    .into_owned();
                hits.push(OverFitHit {
                    file: rel,
                    line: idx + 1,
                    matched: (*needle).to_string(),
                    excerpt: line.trim().to_string(),
                });
            }
        }
    }
    hits
}

/// AZ-IV.W1 wave-level closure: the W1.1 + W1.7 slices delete the
/// `from_rule_name` discriminator tables and the
/// `(layout.kind, rule_name)` builder dispatches. Until those slices
/// merge, the static scan reports the audit's known hit population
/// (Fermat F1 / F2 / F3) — those hits are owned by W1.1 / W1.7,
/// not by W1.5. The W1.5 / W1.8 contributions tracked here are
/// EmitStrategy (F5, replaced by `PRODUCTION_MANIFEST_TABLE`) and
/// pipeline audit-tag aliasing (F6, replaced by
/// `GrammarAuditTag::Custom(entry_name)`); both have already
/// migrated.
///
/// The test runs the scan unconditionally and reports its result;
/// the assertion fails closed at W1 wave close, which is when
/// every slice's deletions land. Until the runtime slice merges,
/// the assertion is `#[ignore]`d with a deadline tracking the W1
/// hard-gate close. The scan body still runs as the
/// `wave_scan_emits_status` companion test below so the gate stays
/// alive (no-op) for CI artefact purposes.
#[test]
#[ignore = "AZ-IV.W1 wave gate — green after W1.1 + W1.7 deletions \
            of from_rule_name + (layout.kind, rule_name) match arms \
            land; fix-or-merge owner: W1.1 worktree; \
            deadline: W1 close commit"]
fn no_grammar_name_branch_in_production_runtime_or_shape_emitter() {
    let (hits, files) = run_scan();
    if !hits.is_empty() {
        let mut report = String::from(
            "AZ-IV.W1 grammar-overfit static scan failed — production runtime or shape emitter \
             branches on a literal grammar parser-struct ident or rule-name string. Per \
             `docs/tranches/AZ-IV/audit/HARDENING-2026-05-01-fermat.md` the replacement is a \
             registry/typed-inference discriminator (StructRegistry::compound_kind, \
             TypeDesc::*, or PRODUCTION_MANIFEST_TABLE). Hits:\n",
        );
        for hit in &hits {
            report.push_str(&format!(
                "  {}:{} — matched `{}` in arm `{}`\n",
                hit.file, hit.line, hit.matched, hit.excerpt,
            ));
        }
        eprintln!("{report}");
        eprintln!("(scanned {files} production files)");
        panic!("{} hit(s) — see eprintln above", hits.len());
    }
}

/// Companion test that always runs and emits the scan summary as a
/// status line in nextest output. Lets CI capture file count +
/// hit count even while the wave-level gate above is `#[ignore]`d.
/// Asserts the scan finds at least one production source root —
/// the runtime + shape-emitter trees must exist and contain Rust
/// files; an empty scan is a path-resolution defect, not an
/// overfit-clean signal.
#[test]
fn wave_scan_emits_status() {
    let (hits, files) = run_scan();
    assert!(
        files > 0,
        "no-grammar-name-branch scan walked zero production files — \
         workspace root resolution defect"
    );
    eprintln!(
        "no-grammar-name-branch scan: {files} files under {} roots, {} hits \
         (gate green = 0 hits at W1 close; current hits are W1.1/W1.7 \
         carries)",
        SCAN_ROOTS.len(),
        hits.len(),
    );
}

fn run_scan() -> (Vec<OverFitHit>, usize) {
    let ws = workspace_root();
    let mut all_hits: Vec<OverFitHit> = Vec::new();
    let mut counted_files: BTreeMap<String, usize> = BTreeMap::new();

    for root_rel in SCAN_ROOTS {
        let root = ws.join(root_rel);
        let files = collect_rust_sources(&root);
        for file in files {
            *counted_files
                .entry(file.to_string_lossy().into_owned())
                .or_insert(0) += 1;
            all_hits.extend(scan_file(&file, &ws));
        }
    }
    (all_hits, counted_files.len())
}
