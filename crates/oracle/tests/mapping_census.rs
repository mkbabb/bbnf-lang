//! sk-v27 LW3 HM pre-work, LAYOUT-HM-V2 §11 item 1: the `->` census as a gate.
//!
//! Reads every `.bbnf` under `grammar/`, finds each `->` mapping outside
//! comments, string literals and regex literals, extracts its right side and
//! classifies it into the seven forms of LAYOUT-HM-V2 §4. It fails on a site
//! that falls outside the seven forms, and on any drift of the per-form,
//! per-file counts from the committed baseline `crates/oracle/mapping-census.tsv`.
//!
//! The classification is syntactic and asserts no semantics: form 7 is "a path
//! called with an annotation"; whether the path is bound by the grammar or the
//! prelude is a question for the typer, not for this census. When the corpus
//! moves on purpose, the failure prints the new table to commit.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The seven forms of LAYOUT-HM-V2 §4, with form 1 split by its suffix.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Form {
    /// Form 1: a suffixed integer or hex literal; the label is `u8`, `hex u32`, ...
    Literal(String),
    /// Form 2: `true` or `false`.
    Bool,
    /// Form 3: bare `Span`.
    Span,
    /// Form 4: a bare numeric base type name.
    Conv(String),
    /// Form 5: `input : Span`.
    InputSpan,
    /// Form 6: `input : T`, T not a base type.
    HostAscribe,
    /// Form 7: a call of a path with `: T`.
    HostCall,
}

impl Form {
    fn number(&self) -> u8 {
        match self {
            Form::Literal(_) => 1,
            Form::Bool => 2,
            Form::Span => 3,
            Form::Conv(_) => 4,
            Form::InputSpan => 5,
            Form::HostAscribe => 6,
            Form::HostCall => 7,
        }
    }

    fn key(&self) -> String {
        match self {
            Form::Literal(s) => format!("1 literal {s}"),
            Form::Bool => "2 bool".into(),
            Form::Span => "3 span".into(),
            Form::Conv(b) => format!("4 conv {b}"),
            Form::InputSpan => "5 input-span".into(),
            Form::HostAscribe => "6 host-ascribe".into(),
            Form::HostCall => "7 host-call".into(),
        }
    }
}

/// The eleven numeric members of `B` (LAYOUT-HM-V2 §1.2).
const NUMERIC: [&str; 11] = ["u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "f32", "f64"];
/// The four non-numeric members of `B`.
const NON_NUMERIC: [&str; 4] = ["Unit", "Span", "Bool", "Str"];
/// Integer suffixes a form-1 literal may carry.
const INT_SUFFIX: [&str; 9] = ["u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64"];

fn is_base(name: &str) -> bool {
    NUMERIC.contains(&name) || NON_NUMERIC.contains(&name)
}

fn is_ident(s: &str) -> bool {
    let mut cs = s.chars();
    matches!(cs.next(), Some(c) if c == '_' || c.is_ascii_alphabetic())
        && cs.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

fn is_path(s: &str) -> bool {
    !s.is_empty() && s.split("::").all(is_ident)
}

fn literal_form(rhs: &str) -> Option<Form> {
    let suffix = INT_SUFFIX.iter().filter(|s| rhs.ends_with(**s)).max_by_key(|s| s.len())?;
    let digits = &rhs[..rhs.len() - suffix.len()];
    if let Some(hex) = digits.strip_prefix("0x").or_else(|| digits.strip_prefix("0X")) {
        (!hex.is_empty() && hex.chars().all(|c| c.is_ascii_hexdigit())).then(|| Form::Literal(format!("hex {suffix}")))
    } else {
        (!digits.is_empty() && digits.chars().all(|c| c.is_ascii_digit())).then(|| Form::Literal((*suffix).to_string()))
    }
}

/// `path(args) : T` with balanced parentheses and an identifier or path `T`.
fn host_call(rhs: &str) -> bool {
    let Some((call, ty)) = rhs.rsplit_once(" : ") else { return false };
    let Some(open) = call.find('(') else { return false };
    if !call.ends_with(')') || !is_path(&call[..open]) || &call[..open] == "input" || !is_path(ty) {
        return false;
    }
    let mut depth = 0i32;
    for (i, c) in call[open..].char_indices() {
        depth += match c {
            '(' => 1,
            ')' => -1,
            _ => 0,
        };
        if depth == 0 && open + i != call.len() - 1 {
            return false;
        }
    }
    depth == 0
}

/// Classify a whitespace-normalised right side, or `None` for an eighth form.
fn classify(rhs: &str) -> Option<Form> {
    if let Some(f) = literal_form(rhs) {
        return Some(f);
    }
    match rhs {
        "true" | "false" => return Some(Form::Bool),
        "Span" => return Some(Form::Span),
        "input : Span" => return Some(Form::InputSpan),
        _ => {}
    }
    if NUMERIC.contains(&rhs) {
        return Some(Form::Conv(rhs.to_string()));
    }
    if let Some(t) = rhs.strip_prefix("input : ") {
        return (is_path(t) && !is_base(t)).then_some(Form::HostAscribe);
    }
    host_call(rhs).then_some(Form::HostCall)
}

/// One `->` site: its 1-based line and its normalised right side.
struct Site {
    line: usize,
    rhs: String,
}

/// Skip a quoted literal or a regex starting at `i` (the opening delimiter);
/// returns the index just past the closing delimiter.
fn skip_delimited(b: &[u8], mut i: usize, close: u8) -> usize {
    i += 1;
    while i < b.len() && b[i] != close {
        i += if b[i] == b'\\' { 2 } else { 1 };
    }
    i + 1
}

/// Skip a `//` or `/*` comment at `i`, if there is one.
fn skip_comment(b: &[u8], i: usize) -> Option<usize> {
    match b.get(i + 1) {
        _ if b[i] != b'/' => None,
        Some(b'/') => Some(b[i..].iter().position(|&c| c == b'\n').map_or(b.len(), |p| i + p)),
        Some(b'*') => Some(b[i + 2..].windows(2).position(|w| w == b"*/").map_or(b.len(), |p| i + 2 + p + 2)),
        _ => None,
    }
}

/// The right side of a mapping starting at `i` (just past `->`): up to the
/// first `;`, `|`, `,`, `)`, `]`, `}` or terminating `.` at depth zero, or a
/// comment. Returns the end index and the whitespace-normalised text.
fn right_side(b: &[u8], mut i: usize) -> (usize, String) {
    let start = i;
    let mut depth = 0i32;
    while i < b.len() {
        if skip_comment(b, i).is_some() {
            break;
        }
        match b[i] {
            b'"' | b'\'' | b'`' => {
                i = skip_delimited(b, i, b[i]);
                continue;
            }
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' if depth > 0 => depth -= 1,
            b';' | b'|' | b',' | b')' | b']' | b'}' if depth == 0 => break,
            b'.' if depth == 0 && b.get(i + 1).is_none_or(|c| c.is_ascii_whitespace()) => break,
            _ => {}
        }
        i += 1;
    }
    let text = String::from_utf8_lossy(&b[start..i]);
    (i, text.split_whitespace().collect::<Vec<_>>().join(" "))
}

/// Every `->` site in one grammar source, outside comments, literals and regexes.
fn sites(src: &str) -> Vec<Site> {
    let b = src.as_bytes();
    let line_of = |i: usize| 1 + b[..i].iter().filter(|&&c| c == b'\n').count();
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
        if let Some(j) = skip_comment(b, i) {
            i = j;
            continue;
        }
        match b[i] {
            b'"' | b'\'' | b'`' | b'/' => i = skip_delimited(b, i, b[i]),
            b'-' if b.get(i + 1) == Some(&b'>') => {
                let (end, rhs) = right_side(b, i + 2);
                out.push(Site { line: line_of(i), rhs });
                i = end;
            }
            _ => i += 1,
        }
    }
    out
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().expect("repo root")
}

fn grammar_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir).expect("read grammar dir").map(|e| e.expect("entry").path()).collect();
    entries.sort();
    for p in entries {
        if p.is_dir() {
            grammar_files(&p, out);
        } else if p.extension().is_some_and(|e| e == "bbnf") {
            out.push(p);
        }
    }
}

/// The census: (form key, file) → count, plus every unclassified site.
fn census() -> (usize, BTreeMap<(String, String), usize>, Vec<String>) {
    let root = repo_root();
    let mut files = Vec::new();
    grammar_files(&root.join("grammar"), &mut files);
    let mut table = BTreeMap::new();
    let mut unclassified = Vec::new();
    for path in &files {
        let rel = path.strip_prefix(&root).expect("under root").to_string_lossy().into_owned();
        let src = std::fs::read_to_string(path).expect("read grammar");
        for site in sites(&src) {
            match classify(&site.rhs) {
                Some(form) => *table.entry((form.key(), rel.clone())).or_insert(0) += 1,
                None => unclassified.push(format!("{rel}:{}: -> {}", site.line, site.rhs)),
            }
        }
    }
    (files.len(), table, unclassified)
}

fn render(files: usize, table: &BTreeMap<(String, String), usize>) -> String {
    let mut s = format!("# LAYOUT-HM-V2 §4 mapping census; regenerate from the failure text of crates/oracle/tests/mapping_census.rs\nfiles\t{files}\n");
    for ((form, file), n) in table {
        s.push_str(&format!("{form}\t{file}\t{n}\n"));
    }
    s
}

#[test]
fn every_mapping_in_the_grammars_is_one_of_the_seven_forms_and_the_counts_hold() {
    let (files, table, unclassified) = census();
    let mut per_form: BTreeMap<u8, usize> = BTreeMap::new();
    for ((form, _), n) in &table {
        *per_form.entry(form.as_bytes()[0] - b'0').or_insert(0) += n;
    }
    let total: usize = per_form.values().sum();
    eprintln!("census: {files} files, {total} classified sites, per form {per_form:?}");
    assert!(unclassified.is_empty(), "{} `->` site(s) outside the seven forms of LAYOUT-HM-V2 §4:\n{}", unclassified.len(), unclassified.join("\n"));

    let actual = render(files, &table);
    let baseline_path = repo_root().join("crates/oracle/mapping-census.tsv");
    let baseline = std::fs::read_to_string(&baseline_path).unwrap_or_default();
    assert!(
        actual == baseline,
        "the mapping census drifted from {}; if the corpus moved on purpose, commit this table:\n{actual}",
        baseline_path.display()
    );
}

#[test]
fn the_classifier_refuses_what_is_not_one_of_the_seven_forms() {
    for (rhs, form) in [
        ("0u8", Some(1)),
        ("0xF0F8FFFFu32", Some(1)),
        ("true", Some(2)),
        ("Span", Some(3)),
        ("f64", Some(4)),
        ("input : Span", Some(5)),
        ("input : Color", Some(6)),
        ("crate::css_types::parse_hex_color(input) : u32", Some(7)),
        ("decode_json_string_to_arena(input) : String", Some(7)),
        ("1", None),
        ("1.5f64", None),
        ("nil()", None),
        ("input", None),
        ("input : f64", None),
        ("Bool", None),
        ("f(input)", None),
        ("f(a) (b) : T", None),
        ("", None),
    ] {
        assert_eq!(classify(rhs).as_ref().map(Form::number), form, "classify({rhs:?})");
    }
}

#[test]
fn comments_strings_and_regexes_hide_their_arrows() {
    let src = "// a -> b\n/* c -> d */ r = \"->\" , '->' , `->` , /->/ -> 1u8 ;\ns = x\n  -> f(input) : T ; t = y -> Span .\n";
    let got: Vec<_> = sites(src).into_iter().map(|s| (s.line, s.rhs)).collect();
    assert_eq!(got, vec![(2, "1u8".to_string()), (4, "f(input) : T".to_string()), (4, "Span".to_string())]);
}
