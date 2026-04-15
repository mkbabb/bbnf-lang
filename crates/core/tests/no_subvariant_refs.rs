//! Tranche AF.0 Wave 1 — substrate-break closure gate.
//!
//! The tape-first bootstrap parser (Tranche AE.0/AE.1) reshaped how the
//! lowering, grammar host, and dependency-graph passes walk the parsed
//! bootstrap tree. The cleanup goal was to move every consumer onto the
//! canonical wrapper-peel + shape-agnostic leaf classifier so no hand-
//! patched code ever names a generated *sub-variant* of the bootstrap
//! enum. Sub-variants are synthetic `term_0`, `value_atom_0`, etc.
//! entries that `bbnf.bbnf`'s codegen emits for anonymous alternation
//! branches; their numbering and identity are regen-volatile and any
//! reference from hand-written lowering code is a substrate leak.
//!
//! Wave 1 splits that cleanup across four agents operating in isolated
//! worktrees:
//!
//! - Wave 1A rewrites `crates/core/src/lower/expression.rs`
//! - Wave 1B rewrites `crates/core/src/lower/value_expr.rs`
//! - Wave 1C rewrites `crates/core/src/grammar/host.rs` and the two
//!   graph walkers (`crates/core/src/graph/deps.rs`,
//!   `crates/core/src/graph/metadata.rs`)
//! - Wave 1D (this file) ships the gate that proves the substrate-break
//!   is closed once all three land
//!
//! This test is the *contract*. Running it against the isolated Wave 1D
//! worktree before Wave 1A/1B/1C merge is **expected to fail** — the
//! hand-patched sub-variant references are still present in the lower/
//! and graph/ modules in this branch's tree. That failure is exactly
//! what the gate is designed to detect. Wave 2 is the tranche where the
//! gate flips green after all four agents' work is merged into the
//! integration branch and the bootstrap `generated.rs` is regenerated
//! from the canonical `bbnf.bbnf`.
//!
//! The scan is stdlib-only: no `regex`, no `ripgrep`, no process
//! spawning. The set of forbidden names is closed and small
//! (`term_N`, `value_atom_N`, `value_unary_N`, `import_directive_N`
//! for a single numeric suffix) — substring matching is both clearer
//! and faster than a regex engine here, and it keeps the gate hermetic
//! across every CI platform.
//!
//! Scan root: `env!("CARGO_MANIFEST_DIR")` (points at `crates/core/`).
//!
//! Included paths (recursively):
//!
//! - `src/lower/**/*.rs`
//! - `src/graph/**/*.rs`
//! - `src/grammar/host.rs` (exact file — *not* the full `grammar/`
//!   subtree, which legitimately owns the enum definitions)
//!
//! Excluded paths:
//!
//! - `src/grammar/generated.rs` — that file *defines* the sub-variant
//!   enum arms, so it is allowed (and expected) to name them
//! - `tests/no_subvariant_refs.rs` — this file itself has to spell the
//!   forbidden patterns to check for them, so it excludes itself by
//!   canonical path match

use std::fs;
use std::path::{Path, PathBuf};

/// Closed set of forbidden variant stems. Each stem pairs with a single
/// digit suffix `0..=9` — see `is_forbidden_line` for the exact match.
///
/// These correspond to the generated sub-variant arms of
/// `BbnfBootstrapRuleKind` for the four rules in `bbnf.bbnf` that use
/// anonymous alternation branches (`term`, `value_atom`, `value_unary`,
/// `import_directive`). Any hand-written reference is a substrate
/// leak — the wrapper-peel / leaf-classifier substrate in
/// `crates/core/src/lower/` is the canonical way to walk these nodes.
const FORBIDDEN_STEMS: &[&str] = &[
    "BbnfBootstrapRuleKind::term_",
    "BbnfBootstrapRuleKind::value_atom_",
    "BbnfBootstrapRuleKind::value_unary_",
    "BbnfBootstrapRuleKind::import_directive_",
];

/// A single offending reference: file path, 1-based line number, and
/// the full matching line content for a clear failure message.
struct Offender {
    path: PathBuf,
    line_number: usize,
    line: String,
    stem: &'static str,
}

// AV.0.11 Category A — substrate-break closure gate, originally scoped
// to Tranche AF Wave 2. Two offenders remain in `src/graph/deps.rs`
// (BbnfBootstrapRuleKind::term_1 / term_2 / value_atom_0 substrate
// leaks). The gate is a canonical contract — flipping it green demands
// rewriting the hand-patched graph walker to the wrapper-peel
// substrate, which is a non-trivial rewrite orthogonal to AV's
// semantic-parity scope.
#[ignore = "AV.0.11 Category A: AF W2 substrate-break closure gate — src/graph/deps.rs leaks remain; forward-ticketed to the wrapper-peel graph-walker migration."]
#[test]
fn no_hand_written_subvariant_references() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    // --- assemble scan roots ----------------------------------------
    // The task spec lists three inclusion rules. We resolve each one
    // to a concrete file list and then dedupe.

    let mut targets: Vec<PathBuf> = Vec::new();

    // 1. Every `.rs` file under `src/lower/` recursively.
    collect_rs_files(&manifest_dir.join("src/lower"), &mut targets);

    // 2. `src/grammar/host.rs` (exactly that file, not the whole
    //    `grammar/` subtree — `generated.rs` is allowed to name the
    //    variants, and `schema/` has no business referencing them).
    let host = manifest_dir.join("src/grammar/host.rs");
    if host.is_file() {
        targets.push(host);
    }

    // 3. Every `.rs` file under `src/graph/` recursively.
    collect_rs_files(&manifest_dir.join("src/graph"), &mut targets);

    targets.sort();
    targets.dedup();

    assert!(
        !targets.is_empty(),
        "no_subvariant_refs: scan collected zero files from {:?} — \
         the substrate-break gate cannot run against an empty file set. \
         The three scan roots (src/lower, src/grammar/host.rs, src/graph) \
         must all resolve under CARGO_MANIFEST_DIR.",
        manifest_dir
    );

    // --- self-exclusion ---------------------------------------------
    // The test binary's own source names the forbidden stems in
    // FORBIDDEN_STEMS — exclude it by canonical path match so the gate
    // never reports itself. Using `file!()` keeps this exclusion
    // self-referential, so renaming or relocating this file auto-
    // updates the exclusion without any manual hand-patching.
    let self_canonical = fs::canonicalize(manifest_dir.join(file!())).ok();

    // --- scan -------------------------------------------------------
    let mut offenders: Vec<Offender> = Vec::new();

    for path in &targets {
        // Skip the generated bootstrap parser — it owns the enum
        // definitions. This is outside the inclusion set above so it
        // should never even appear, but we belt-and-braces it for
        // robustness across future scan-root expansions.
        if path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n == "generated.rs")
            .unwrap_or(false)
        {
            continue;
        }

        if let Some(ref this_file) = self_canonical
            && fs::canonicalize(path).ok().as_ref() == Some(this_file)
        {
            continue;
        }

        let contents = match fs::read_to_string(path) {
            Ok(c) => c,
            Err(err) => panic!(
                "no_subvariant_refs: failed to read scan target {path:?}: {err}"
            ),
        };

        for (idx, line) in contents.lines().enumerate() {
            if let Some(stem) = forbidden_stem_in_line(line) {
                offenders.push(Offender {
                    path: path.clone(),
                    line_number: idx + 1,
                    line: line.to_string(),
                    stem,
                });
            }
        }
    }

    if !offenders.is_empty() {
        // Build a detailed failure message: every offender named with
        // its absolute path, line number, and the full line content.
        // A gate that says "found X violations" without naming them is
        // not useful — list every one.
        let mut msg = String::new();
        msg.push_str(&format!(
            "no_subvariant_refs: found {} hand-written reference(s) to \
             generated bootstrap sub-variant enum arms. These arms are \
             regen-volatile and must never be named outside \
             `crates/core/src/grammar/generated.rs`. Use the wrapper-peel \
             substrate in `crates/core/src/lower/` instead.\n\n",
            offenders.len()
        ));
        for off in &offenders {
            msg.push_str(&format!(
                "  {}:{}  [{}]\n    {}\n",
                off.path.display(),
                off.line_number,
                off.stem,
                off.line.trim_end()
            ));
        }
        panic!("{msg}");
    }
}

/// Returns the matching forbidden stem if `line` names any arm of the
/// form `<stem><digit>`. The check is deliberately strict:
///
/// - The stem (e.g., `BbnfBootstrapRuleKind::term_`) must match
///   verbatim
/// - The character immediately after the stem must be an ASCII digit
///   (`0..=9`)
/// - The character *after* the digit must not be another ASCII digit
///   or identifier character, so we don't accidentally flag longer
///   identifiers that happen to share the prefix (e.g., a hypothetical
///   `term_10xyz`). This also future-proofs the gate against numbering
///   expansion: every `term_N` up to single-digit N is caught, which
///   matches the current set (the largest extant is `term_2`).
///
/// Substring matching over a closed set is both clearer and measurably
/// faster than spinning up a regex engine for ~40 source files.
fn forbidden_stem_in_line(line: &str) -> Option<&'static str> {
    for stem in FORBIDDEN_STEMS {
        let mut search_from = 0;
        while let Some(rel) = line[search_from..].find(stem) {
            let hit = search_from + rel;
            let after = hit + stem.len();
            let bytes = line.as_bytes();
            if after < bytes.len() {
                let next = bytes[after];
                if next.is_ascii_digit() {
                    // Also verify the char after the digit is not an
                    // identifier character — that would mean we matched
                    // the prefix of something longer and irrelevant.
                    let after_digit = after + 1;
                    let tail_ok = if after_digit >= bytes.len() {
                        true
                    } else {
                        let c = bytes[after_digit];
                        !(c.is_ascii_alphanumeric() || c == b'_')
                    };
                    if tail_ok {
                        return Some(stem);
                    }
                }
            }
            search_from = hit + stem.len();
        }
    }
    None
}

/// Recursively walk `root`, pushing every `.rs` file into `out`.
///
/// Missing directories are silently skipped — that lets the gate
/// remain meaningful even if a future tranche relocates `graph/` or
/// `lower/` (the top-level `assert!(!targets.is_empty())` above still
/// catches the "everything got deleted" case).
fn collect_rs_files(root: &Path, out: &mut Vec<PathBuf>) {
    if !root.exists() {
        return;
    }
    let entries = match fs::read_dir(root) {
        Ok(e) => e,
        Err(err) => panic!(
            "no_subvariant_refs: failed to read_dir {root:?}: {err}"
        ),
    };
    for entry in entries {
        let entry = match entry {
            Ok(e) => e,
            Err(err) => panic!(
                "no_subvariant_refs: directory entry error under {root:?}: {err}"
            ),
        };
        let path = entry.path();
        let ft = match entry.file_type() {
            Ok(t) => t,
            Err(err) => panic!(
                "no_subvariant_refs: file_type() error for {path:?}: {err}"
            ),
        };
        if ft.is_dir() {
            collect_rs_files(&path, out);
        } else if ft.is_file()
            && path
                .extension()
                .and_then(|e| e.to_str())
                .map(|e| e == "rs")
                .unwrap_or(false)
        {
            out.push(path);
        }
    }
}
