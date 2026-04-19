//! Shared helpers for per-grammar tape_parity tests — AX.W0a.2.e.
//!
//! Included via `#[path = "tape_parity_common/mod.rs"] mod common;`
//! in each `tape_parity_<grammar>.rs` binary. The split exists to
//! avoid a single rustc process linking five `#[derive(Parser)]`
//! sites; the W0a.2.d re-plan measured 26 GB RSS for the aggregate
//! and killed a test build pre-OOM.
//!
//! Per-grammar binaries each link ONE grammar and compile in ~11s
//! under `CARGO_BUILD_JOBS=4`.

#![allow(dead_code)]

use std::fs;
use std::path::PathBuf;

use bbnf::runtime::Root;
use bbnf::runtime::tape::{TapeCursor, TapeKind};

/// Search a small candidate list for an input file.
pub fn load(relpath: &str) -> String {
    let candidates = [
        format!("../../data/{}", relpath),
        format!("../data/{}", relpath),
        format!("data/{}", relpath),
        relpath.to_string(),
    ];
    for path in &candidates {
        if let Ok(contents) = fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find data file '{}'; tried: {:?}",
        relpath, candidates
    );
}

/// Load a grammar sample file from the `grammar/` tree.
pub fn load_grammar_sample(relpath: &str) -> String {
    let candidates = [
        format!("../../grammar/{}", relpath),
        format!("../grammar/{}", relpath),
        format!("grammar/{}", relpath),
    ];
    for path in &candidates {
        if let Ok(contents) = fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find grammar file '{}'; tried: {:?}",
        relpath, candidates
    );
}

pub trait HasCursor<'tape> {
    fn cursor_of(&self) -> TapeCursor<'tape>;
}

pub trait ParseGrammar: Root + Sized {
    fn parse_input(input: &str) -> Result<bbnf::runtime::Parsed<'_, Self>, bbnf::runtime::ParseErr>;
}

pub fn kind_name(k: TapeKind) -> &'static str {
    match k {
        TapeKind::None => "None",
        TapeKind::Span => "Span",
        TapeKind::Epsilon => "Epsilon",
        TapeKind::Literal => "Literal",
        TapeKind::Regex => "Regex",
        TapeKind::Seq => "Seq",
        TapeKind::Alt => "Alt",
        TapeKind::Repeat => "Repeat",
        TapeKind::Rule => "Rule",
        TapeKind::VariantTag => "VariantTag",
        TapeKind::MapValue => "MapValue",
        TapeKind::TokenDispatch => "TokenDispatch",
        TapeKind::Recovered => "Recovered",
        TapeKind::KvPair => "KvPair",
        TapeKind::ShapeRef => "ShapeRef",
        TapeKind::Reserved => "Reserved",
        TapeKind::Scanned => "Scanned",
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TapeSummary {
    pub root_kind: &'static str,
    pub root_variant_idx: u8,
    pub root_children_count: usize,
    pub total_records: usize,
}

impl TapeSummary {
    pub fn from_cursor(cursor: TapeCursor<'_>, total_records: usize) -> Self {
        let root_kind = kind_name(cursor.kind());
        let root_variant_idx = cursor.variant_idx();
        let root_children_count = cursor.children().count();
        TapeSummary {
            root_kind,
            root_variant_idx,
            root_children_count,
            total_records,
        }
    }

    pub fn to_json(&self) -> String {
        format!(
            "{{\n  \"root_kind\": \"{}\",\n  \"root_variant_idx\": {},\n  \"root_children_count\": {},\n  \"total_records\": {}\n}}\n",
            self.root_kind, self.root_variant_idx, self.root_children_count, self.total_records,
        )
    }

    pub fn from_json(raw: &str) -> Option<Self> {
        fn extract_str<'a>(s: &'a str, key: &str) -> Option<&'a str> {
            let needle = format!("\"{}\": \"", key);
            let start = s.find(&needle)? + needle.len();
            let end = start + s[start..].find('"')?;
            Some(&s[start..end])
        }
        fn extract_num(s: &str, key: &str) -> Option<u64> {
            let needle = format!("\"{}\":", key);
            let start = s.find(&needle)? + needle.len();
            let rest = s[start..].trim_start();
            let end = rest.find(|c: char| !c.is_ascii_digit())?;
            rest[..end].parse().ok()
        }
        let root_kind_str = extract_str(raw, "root_kind")?;
        let root_kind: &'static str = match root_kind_str {
            "None" => "None",
            "Span" => "Span",
            "Epsilon" => "Epsilon",
            "Literal" => "Literal",
            "Regex" => "Regex",
            "Seq" => "Seq",
            "Alt" => "Alt",
            "Repeat" => "Repeat",
            "Rule" => "Rule",
            "VariantTag" => "VariantTag",
            "MapValue" => "MapValue",
            "TokenDispatch" => "TokenDispatch",
            "Recovered" => "Recovered",
            "KvPair" => "KvPair",
            "Reserved" => "Reserved",
            _ => return None,
        };
        Some(TapeSummary {
            root_kind,
            root_variant_idx: extract_num(raw, "root_variant_idx")? as u8,
            root_children_count: extract_num(raw, "root_children_count")? as usize,
            total_records: extract_num(raw, "total_records")? as usize,
        })
    }
}

pub fn golden_path(grammar: &str, input: &str) -> PathBuf {
    let base_candidates = [
        PathBuf::from("tests/fixtures/tape_golden"),
        PathBuf::from("crates/core/tests/fixtures/tape_golden"),
    ];
    for base in &base_candidates {
        if base.exists() {
            return base.join(grammar).join(format!("{}.json", input));
        }
    }
    PathBuf::from("tests/fixtures/tape_golden")
        .join(grammar)
        .join(format!("{}.json", input))
}

pub fn assert_tape_parity<G>(grammar: &'static str, input_name: &str, input: &str)
where
    G: ParseGrammar,
    for<'tape> <G as Root>::View<'tape>: HasCursor<'tape>,
{
    let parsed = match G::parse_input(input) {
        Ok(p) => p,
        Err(e) => panic!(
            "{}/{}: parse failed with {:?}",
            grammar, input_name, e
        ),
    };
    let total_records = parsed.tape().len();
    let view = parsed.view();
    let cursor: TapeCursor<'_> = view.cursor_of();
    assert_ne!(
        cursor.kind(),
        TapeKind::None,
        "{}/{}: root cursor reported TapeKind::None",
        grammar,
        input_name
    );
    let summary = TapeSummary::from_cursor(cursor, total_records);

    let path = golden_path(grammar, input_name);
    if let Some(parent) = path.parent() {
        if !parent.exists() {
            fs::create_dir_all(parent).expect("failed to create golden dir");
        }
    }
    match fs::read_to_string(&path) {
        Ok(raw) => {
            let golden = TapeSummary::from_json(&raw).unwrap_or_else(|| {
                panic!(
                    "{}/{}: corrupt golden at {} — delete it to regenerate",
                    grammar,
                    input_name,
                    path.display()
                )
            });
            assert_eq!(
                summary, golden,
                "{}/{}: tape summary diverged from golden at {}",
                grammar,
                input_name,
                path.display()
            );
        }
        Err(_) => {
            fs::write(&path, summary.to_json()).expect("failed to write golden");
            eprintln!(
                "[tape_parity] wrote new golden {} for {}/{}",
                path.display(),
                grammar,
                input_name
            );
        }
    }
}
