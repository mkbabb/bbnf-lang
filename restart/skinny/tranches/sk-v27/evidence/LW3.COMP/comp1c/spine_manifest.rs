//! LW3.COMP-1c falsifier: the spine reads the language.
//!
//! Every `grammar/MANIFEST` entry is compiled by the spine
//! (`compile_instance`, which runs `compile_validated_instance`) and its
//! verdict is compared with the verdict `DIVERGENCE.tsv` rules: ACCEPT for a
//! file no `D-*` row names; for a named file the `rust` column, except where
//! the row's fate is `CURED-RUST` (the Rust arm was the defect, so the
//! `ts_base` column is the ruled verdict).
//!
//! The grammar instance each entry is compiled under is derived from the
//! grammar text by the rule this test states (ask COMP1C-2 asks who owns
//! that derivation): `entry` is the first rule written; `trivia_regex` is
//! the `@ws` pattern when the grammar writes `@ws`, the six-byte `?w` set
//! (`docs/bbnf/observation.md`) when it writes `?w`, otherwise null; no
//! Pratt tables.
//!
//! The test prints one line per entry and a summary, and fails while any
//! entry's verdict differs from the recorded one.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use bbnf::{CompileOptions, GrammarInstanceInput, StageResult, compile_instance};

const DEFAULT_TRIVIA: &str = "[ \\t\\n\\x0B\\x0C\\r]*";

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root resolves")
}

fn manifest_entries(root: &Path) -> Vec<String> {
    std::fs::read_to_string(root.join("grammar/MANIFEST"))
        .expect("grammar/MANIFEST is readable")
        .lines()
        .filter_map(|line| line.split_whitespace().nth(1).map(str::to_owned))
        .collect()
}

/// `files -> ruled verdict` for every `D-*` row of `DIVERGENCE.tsv`.
fn recorded_rust_verdicts(root: &Path) -> BTreeMap<String, bool> {
    let text = std::fs::read_to_string(
        root.join("restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv"),
    )
    .expect("DIVERGENCE.tsv is readable");
    let mut verdicts = BTreeMap::new();
    for line in text.lines() {
        let cells: Vec<&str> = line.split('\t').collect();
        if cells.len() < 8 || !cells[0].starts_with("D-") {
            continue;
        }
        let column = if cells[7].starts_with("CURED-RUST") { 3 } else { 2 };
        let accept = match cells[column] {
            "ACCEPT" => true,
            "REJECT" => false,
            other => panic!("DIVERGENCE {}: unknown verdict {other:?}", cells[0]),
        };
        for file in cells[1].split(',') {
            verdicts.insert(file.trim().to_owned(), accept);
        }
    }
    verdicts
}

/// Strip `//` and `/* */` comments and string/regex bodies are kept as is;
/// only used to find the first rule name and the `@ws` / `?w` uses.
fn strip_comments(source: &str) -> String {
    let bytes = source.as_bytes();
    let mut out = String::with_capacity(source.len());
    let mut i = 0;
    let mut quote: Option<u8> = None;
    while i < bytes.len() {
        let b = bytes[i];
        if let Some(q) = quote {
            out.push(b as char);
            if b == b'\\' && i + 1 < bytes.len() {
                out.push(bytes[i + 1] as char);
                i += 2;
                continue;
            }
            if b == q {
                quote = None;
            }
            i += 1;
            continue;
        }
        if b == b'"' || b == b'\'' || b == b'`' {
            quote = Some(b);
            out.push(b as char);
            i += 1;
        } else if bytes[i..].starts_with(b"//") {
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
        } else if bytes[i..].starts_with(b"/*") {
            i += 2;
            while i < bytes.len() && !bytes[i..].starts_with(b"*/") {
                i += 1;
            }
            i += 2;
        } else {
            out.push(b as char);
            i += 1;
        }
    }
    out
}

fn derived_instance(logical_path: &str, source: &str) -> Vec<u8> {
    let text = strip_comments(source);
    let entry = text
        .split(';')
        .filter_map(|statement| {
            let statement = statement.trim_start();
            if statement.starts_with('@') {
                return None;
            }
            let (lhs, _) = statement.split_once('=')?;
            let lhs = lhs.trim();
            (!lhs.is_empty()
                && lhs
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-'))
            .then(|| lhs.to_owned())
        })
        .next()
        .unwrap_or_else(|| "root".to_owned());
    let trivia = text
        .find("@ws")
        .and_then(|at| {
            let rest = &text[at + 3..];
            let open = rest.find('/')?;
            let body = &rest[open + 1..];
            let close = body.find("/ ").or_else(|| body.find("/;"))?;
            Some(body[..close].to_owned())
        })
        .or_else(|| text.contains("?w").then(|| DEFAULT_TRIVIA.to_owned()));
    serde_json::to_vec(&serde_json::json!({
        "schema": "bbnf.grammar-instance.v1",
        "logical_path": logical_path,
        "entry": entry,
        "trivia_regex": trivia,
        "pratt_tables": [],
    }))
    .expect("instance serializes")
}

#[test]
fn every_manifest_grammar_gets_its_recorded_verdict_from_the_spine() {
    let root = repo_root();
    let recorded = recorded_rust_verdicts(&root);
    let entries = manifest_entries(&root);
    assert!(!entries.is_empty(), "grammar/MANIFEST lists no entry");

    let mut wrong = Vec::new();
    let mut accepted = 0usize;
    let mut first_codes: BTreeMap<String, usize> = BTreeMap::new();
    for path in &entries {
        let source = std::fs::read_to_string(root.join(path))
            .unwrap_or_else(|error| panic!("{path}: unreadable: {error}"));
        let instance = derived_instance(path, &source);
        let expected = recorded.get(path).copied().unwrap_or(true);
        let result = compile_instance(
            GrammarInstanceInput::new(&instance, source.as_bytes()),
            CompileOptions::default(),
        );
        let (got, detail) = match &result {
            StageResult::Accepted { .. } => (true, String::new()),
            StageResult::Rejected { diagnostics } => {
                let first = diagnostics
                    .iter()
                    .next()
                    .map(ToString::to_string)
                    .unwrap_or_default();
                let code = diagnostics
                    .iter()
                    .next()
                    .map(|d| format!("{:?}", d.code()))
                    .unwrap_or_default();
                *first_codes.entry(code).or_default() += 1;
                (false, first.replace('\n', " "))
            }
        };
        accepted += usize::from(got);
        let mark = if got == expected { "ok  " } else { "DIFF" };
        let verdict = |accept: bool| if accept { "ACCEPT" } else { "REJECT" };
        println!(
            "{mark} {path} expected={} spine={} {detail}",
            verdict(expected),
            verdict(got)
        );
        if got != expected {
            wrong.push(path.clone());
        }
    }
    println!(
        "spine_manifest: entries={} accepted={} refused={} wrong={} first-diagnostic-codes={:?}",
        entries.len(),
        accepted,
        entries.len() - accepted,
        wrong.len(),
        first_codes
    );
    assert!(
        wrong.is_empty(),
        "{} of {} MANIFEST entries do not get their recorded verdict from the spine: {wrong:?}",
        wrong.len(),
        entries.len()
    );
}
