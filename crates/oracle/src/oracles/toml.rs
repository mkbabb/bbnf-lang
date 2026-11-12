//! TOML's oracle: toml-test (at its pin) with its typed-value comparator.
//!
//! The held-out grammar is TOML's syntax. TOML's semantics (key uniqueness,
//! table definition rules, inline-table immutability, the date and time
//! ranges, Unicode scalar escapes, the 64-bit integer range) are applied here,
//! by a decoder over an arm's rule-labelled observation tree, in the way
//! serde_json is JSON's typed comparator. A `valid/` file must be accepted and
//! decode to the value toml-test's `.json` file holds; an `invalid/` file must
//! be rejected, by the grammar or by the decoder. A case outside toml-test's
//! TOML 1.0.0 list never reaches here (the corpus is enumerated from it).

use std::collections::BTreeMap;

use serde_json::Value as J;

use super::{Judgement, Oracle};
use crate::arm::{Case, Outcome};
use crate::obs::{Obs, normalise};
use crate::registry::Grammar;

pub struct TomlTest;

impl Oracle for TomlTest {
    fn name(&self) -> &'static str {
        "toml-test"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "toml"
    }

    fn expected_verdict(&self, _g: &Grammar, _i: usize, case: &Case) -> Option<bool> {
        Some(case.id.contains("/tests/valid/"))
    }

    fn judge(&self, _g: &Grammar, _i: usize, case: &Case, out: &Outcome) -> Judgement {
        let valid = case.id.contains("/tests/valid/");
        match out {
            Outcome::GrammarRefused(m) => {
                Judgement::Diverge(format!("arm refused the grammar: {m}"))
            }
            Outcome::Crash(m) => Judgement::Diverge(format!("arm crashed: {m}")),
            Outcome::Reject(_) if valid => {
                Judgement::Diverge("valid file rejected by the grammar".into())
            }
            Outcome::Reject(_) => Judgement::Agree,
            Outcome::Accept { obs, .. } => match (decode(obs), valid) {
                (Err(e), true) => {
                    Judgement::Diverge(format!("valid file fails TOML semantics: {e}"))
                }
                (Err(_), false) => Judgement::Agree,
                (Ok(_), false) => Judgement::Diverge("invalid file accepted and decoded".into()),
                (Ok(doc), true) => {
                    let want_path = case.path.with_extension("json");
                    let Ok(bytes) = std::fs::read(&want_path) else {
                        return Judgement::Diverge(format!(
                            "no expected value at {}",
                            want_path.display()
                        ));
                    };
                    match serde_json::from_slice::<J>(&bytes) {
                        Ok(want) => match compare(&doc, &want, "") {
                            Ok(()) => Judgement::Agree,
                            Err(e) => Judgement::Diverge(format!("value differs: {e}")),
                        },
                        Err(e) => Judgement::Diverge(format!("expected value unreadable: {e}")),
                    }
                }
            },
        }
    }
}

// ── The observation tree, read by rule label ────────────────────────────────

fn label(n: &Obs) -> &str {
    n.label().unwrap_or("")
}

fn kids(n: &Obs) -> &[Obs] {
    match n {
        Obs::Node { children, .. } => children,
        Obs::Leaf { .. } => &[],
    }
}

fn text(n: &Obs) -> String {
    match n {
        Obs::Leaf { text, .. } => text.clone(),
        Obs::Node { children, .. } => children.iter().map(text).collect(),
    }
}

/// The nodes labelled `want` reachable from `n` without entering a node
/// labelled `want` or any label in `stop`.
fn find<'a>(n: &'a Obs, want: &str, stop: &[&str], out: &mut Vec<&'a Obs>) {
    for c in kids(n) {
        if label(c) == want {
            out.push(c);
        } else if !stop.contains(&label(c)) {
            find(c, want, stop, out);
        }
    }
}

/// Follows single-child chains down to the first node whose label is in `set`.
fn innermost<'a>(mut n: &'a Obs, set: &[&str]) -> Option<&'a Obs> {
    loop {
        if set.contains(&label(n)) {
            return Some(n);
        }
        match kids(n) {
            [only] => n = only,
            _ => return None,
        }
    }
}

// ── Values ──────────────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
enum Val {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    /// (toml-test type, normalised text)
    Time(&'static str, DateTime),
    Array(Vec<Val>),
    Table(Table),
}

#[derive(Clone, Debug, PartialEq)]
struct DateTime {
    date: Option<(u32, u32, u32)>,
    /// (h, m, s, fraction digits without trailing zeros)
    time: Option<(u32, u32, u32, String)>,
    /// minutes east of UTC; `Some(0)` for Z
    offset: Option<i32>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Kind {
    /// Created on the way to a header's last key.
    Implicit,
    /// Named by a `[header]`.
    Header,
    /// Created by a dotted key.
    Dotted,
    /// An inline table (and everything inside it): frozen.
    Inline,
}

#[derive(Clone, Debug)]
enum Item {
    Val(Val),
    Table(Table),
    Aot(Vec<Table>),
}

#[derive(Clone, Debug)]
struct Table {
    kind: Kind,
    map: BTreeMap<String, Item>,
}

impl Table {
    fn new(kind: Kind) -> Self {
        Table {
            kind,
            map: BTreeMap::new(),
        }
    }
}

const SCALARS: &[&str] = &[
    "basic-string",
    "ml-basic-string",
    "literal-string",
    "ml-literal-string",
    "dec-int",
    "hex-int",
    "oct-int",
    "bin-int",
    "float",
    "special-float",
    "boolean",
    "offset-date-time",
    "local-date-time",
    "local-date",
    "local-time",
    "array",
    "inline-table",
];

fn decode_val(n: &Obs) -> Result<Val, String> {
    let n = innermost(n, SCALARS).ok_or_else(|| format!("no value under {:?}", n.label()))?;
    let t = text(n);
    Ok(match label(n) {
        "basic-string" => Val::Str(unescape(&t[1..t.len() - 1])?),
        "ml-basic-string" => Val::Str(unescape_ml(&t[3..t.len() - 3])?),
        "literal-string" => Val::Str(t[1..t.len() - 1].to_string()),
        "ml-literal-string" => Val::Str(strip_first_newline(&t[3..t.len() - 3]).to_string()),
        "dec-int" => Val::Int(
            t.replace('_', "")
                .parse::<i64>()
                .map_err(|e| format!("integer {t}: {e}"))?,
        ),
        "hex-int" => Val::Int(radix(&t[2..], 16)?),
        "oct-int" => Val::Int(radix(&t[2..], 8)?),
        "bin-int" => Val::Int(radix(&t[2..], 2)?),
        "float" | "special-float" => Val::Float(float(&t)?),
        "boolean" => Val::Bool(t == "true"),
        "offset-date-time" => Val::Time("datetime", datetime(&t)?),
        "local-date-time" => Val::Time("datetime-local", datetime(&t)?),
        "local-date" => Val::Time("date-local", datetime(&t)?),
        "local-time" => Val::Time("time-local", datetime(&t)?),
        "array" => {
            let mut vals = Vec::new();
            find(n, "val", &[], &mut vals);
            Val::Array(vals.into_iter().map(decode_val).collect::<Result<_, _>>()?)
        }
        "inline-table" => {
            let mut t = Table::new(Kind::Inline);
            let mut kvs = Vec::new();
            find(n, "keyval", &["val"], &mut kvs);
            for kv in kvs {
                insert_keyval(&mut t, kv)?;
            }
            Val::Table(t)
        }
        other => return Err(format!("unknown value rule {other}")),
    })
}

fn radix(digits: &str, r: u32) -> Result<i64, String> {
    i64::from_str_radix(&digits.replace('_', ""), r).map_err(|e| format!("integer {digits}: {e}"))
}

fn float(t: &str) -> Result<f64, String> {
    let s = t.replace('_', "");
    match s.trim_start_matches(['+', '-']) {
        "inf" => Ok(if s.starts_with('-') {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        }),
        "nan" => Ok(f64::NAN),
        _ => s.parse::<f64>().map_err(|e| format!("float {t}: {e}")),
    }
}

fn strip_first_newline(s: &str) -> &str {
    s.strip_prefix("\r\n")
        .or_else(|| s.strip_prefix('\n'))
        .unwrap_or(s)
}

fn scalar(code: u32) -> Result<char, String> {
    char::from_u32(code).ok_or_else(|| format!("escape U+{code:X} is not a Unicode scalar value"))
}

fn unescape(s: &str) -> Result<String, String> {
    let mut out = String::with_capacity(s.len());
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match it.next() {
            Some('"') => out.push('"'),
            Some('\\') => out.push('\\'),
            Some('b') => out.push('\u{8}'),
            Some('f') => out.push('\u{c}'),
            Some('n') => out.push('\n'),
            Some('r') => out.push('\r'),
            Some('t') => out.push('\t'),
            Some(u @ ('u' | 'U')) => {
                let n = if u == 'u' { 4 } else { 8 };
                let hex: String = (0..n).filter_map(|_| it.next()).collect();
                let code =
                    u32::from_str_radix(&hex, 16).map_err(|e| format!("escape \\{u}{hex}: {e}"))?;
                out.push(scalar(code)?);
            }
            other => return Err(format!("bad escape \\{other:?}")),
        }
    }
    Ok(out)
}

fn unescape_ml(body: &str) -> Result<String, String> {
    let body = strip_first_newline(body);
    // A line-ending backslash trims the newline and the white space after it.
    let mut out = String::new();
    let mut rest = body;
    while let Some(i) = rest.find('\\') {
        out.push_str(&rest[..i]);
        let after = &rest[i + 1..];
        let trimmed = after.trim_start_matches([' ', '\t']);
        if trimmed.starts_with('\n') || trimmed.starts_with("\r\n") {
            rest = trimmed.trim_start_matches([' ', '\t', '\n', '\r']);
        } else {
            // An ordinary escape: decode it with the single-line rules.
            let len = match after.chars().next() {
                Some('u') => 5,
                Some('U') => 9,
                Some(c) => c.len_utf8(),
                None => return Err("trailing backslash".into()),
            };
            let end = (1 + len).min(rest.len() - i);
            out.push_str(&unescape(&rest[i..i + end])?);
            rest = &rest[i + end..];
        }
    }
    out.push_str(rest);
    Ok(out)
}

fn num(s: &str) -> Result<u32, String> {
    s.parse::<u32>().map_err(|e| format!("{s}: {e}"))
}

fn days_in(y: u32, m: u32) -> u32 {
    match m {
        2 if (y.is_multiple_of(4) && !y.is_multiple_of(100)) || y.is_multiple_of(400) => 29,
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}

/// Reads the RFC 3339 profiles TOML allows, with the ranges checked.
fn datetime(t: &str) -> Result<DateTime, String> {
    let mut dt = DateTime {
        date: None,
        time: None,
        offset: None,
    };
    let mut rest = t;
    if rest.len() >= 10 && rest.as_bytes()[4] == b'-' {
        let (y, m, d) = (num(&rest[0..4])?, num(&rest[5..7])?, num(&rest[8..10])?);
        if !(1..=12).contains(&m) || d == 0 || d > days_in(y, m) {
            return Err(format!("date {} out of range", &rest[..10]));
        }
        dt.date = Some((y, m, d));
        rest = &rest[10..];
        if rest.is_empty() {
            return Ok(dt);
        }
        // T, t or space; the space is trivia, so the semantic projection
        // the decoder reads may have dropped it.
        if matches!(rest.as_bytes()[0], b'T' | b't' | b' ') {
            rest = &rest[1..];
        }
    }
    let (h, mi, s) = (num(&rest[0..2])?, num(&rest[3..5])?, num(&rest[6..8])?);
    if h > 23 || mi > 59 || s > 60 {
        return Err(format!("time {} out of range", &rest[..8]));
    }
    rest = &rest[8..];
    let mut frac = String::new();
    if let Some(r) = rest.strip_prefix('.') {
        let n = r.bytes().take_while(u8::is_ascii_digit).count();
        frac = r[..n].trim_end_matches('0').to_string();
        rest = &r[n..];
    }
    dt.time = Some((h, mi, s, frac));
    if rest.eq_ignore_ascii_case("z") {
        dt.offset = Some(0);
    } else if !rest.is_empty() {
        let sign = if rest.starts_with('-') { -1 } else { 1 };
        let (oh, om) = (num(&rest[1..3])?, num(&rest[4..6])?);
        if oh > 23 || om > 59 {
            return Err(format!("offset {rest} out of range"));
        }
        dt.offset = Some(sign * (oh as i32 * 60 + om as i32));
    }
    Ok(dt)
}

// ── The document: TOML 1.0.0's table rules ──────────────────────────────────

fn key_parts(k: &Obs) -> Result<Vec<String>, String> {
    let mut parts = Vec::new();
    find(k, "simple-key", &[], &mut parts);
    parts
        .into_iter()
        .map(|p| {
            let inner = innermost(p, &["unquoted-key", "basic-string", "literal-string"])
                .ok_or_else(|| "a key with no key text".to_string())?;
            let t = text(inner);
            match label(inner) {
                "basic-string" => unescape(&t[1..t.len() - 1]),
                "literal-string" => Ok(t[1..t.len() - 1].to_string()),
                _ => Ok(t),
            }
        })
        .collect()
}

fn insert_keyval(table: &mut Table, kv: &Obs) -> Result<(), String> {
    let key = kids(kv)
        .iter()
        .find(|c| label(c) == "key")
        .ok_or("keyval with no key")?;
    let val = kids(kv)
        .iter()
        .find(|c| label(c) == "val")
        .ok_or("keyval with no value")?;
    let parts = key_parts(key)?;
    let (last, prefix) = parts.split_last().ok_or("empty key")?;
    let mut t = table;
    // A dotted key extends only tables dotted keys made, in the same table
    // body; an inline table's value (Kind::Inline) is frozen everywhere,
    // including by the dotted keys of the inline table that holds it.
    for p in prefix {
        let entry = t
            .map
            .entry(p.clone())
            .or_insert_with(|| Item::Table(Table::new(Kind::Dotted)));
        match entry {
            Item::Table(sub) if sub.kind == Kind::Dotted => {}
            Item::Table(sub) => {
                return Err(format!(
                    "dotted key {p} reaches into a {:?} table",
                    sub.kind
                ));
            }
            _ => return Err(format!("dotted key {p} reaches into a non-table")),
        }
        t = match entry {
            Item::Table(sub) => sub,
            _ => unreachable!(),
        };
    }
    if t.map.contains_key(last) {
        return Err(format!("duplicate key {last}"));
    }
    let v = match decode_val(val)? {
        Val::Table(tbl) => Item::Table(tbl),
        other => Item::Val(other),
    };
    t.map.insert(last.clone(), v);
    Ok(())
}

/// Walks to a header's table, creating implicit tables on the way.
fn header_prefix<'a>(root: &'a mut Table, prefix: &[String]) -> Result<&'a mut Table, String> {
    let mut t = root;
    for p in prefix {
        let entry = t
            .map
            .entry(p.clone())
            .or_insert_with(|| Item::Table(Table::new(Kind::Implicit)));
        t = match entry {
            Item::Table(sub) if sub.kind != Kind::Inline => sub,
            Item::Table(_) => return Err(format!("header reaches into inline table {p}")),
            Item::Aot(v) => v.last_mut().ok_or("empty array of tables")?,
            Item::Val(_) => return Err(format!("header reaches through value {p}")),
        };
    }
    Ok(t)
}

/// Decodes a whole document. `Err` is a TOML semantic (or decoding) error.
fn decode(obs: &Obs) -> Result<Table, String> {
    let doc = normalise(obs);
    let mut root = Table::new(Kind::Header);
    // The current table, as a path; each step names a table or the last
    // element of an array of tables.
    let mut current: Vec<String> = Vec::new();
    let mut exprs = Vec::new();
    collect_exprs(&doc, &mut exprs);
    for e in exprs {
        match label(e) {
            "keyval" => {
                let t = header_prefix(&mut root, &current)?;
                insert_keyval(t, e)?;
            }
            "std-table" => {
                let key = kids(e)
                    .iter()
                    .find(|c| label(c) == "key")
                    .ok_or("header with no key")?;
                let parts = key_parts(key)?;
                let (last, prefix) = parts.split_last().ok_or("empty header")?;
                let t = header_prefix(&mut root, prefix)?;
                match t.map.get_mut(last) {
                    None => {
                        t.map
                            .insert(last.clone(), Item::Table(Table::new(Kind::Header)));
                    }
                    Some(Item::Table(sub)) if sub.kind == Kind::Implicit => sub.kind = Kind::Header,
                    Some(_) => return Err(format!("table {} defined twice", parts.join("."))),
                }
                current = parts;
            }
            "array-table" => {
                let key = kids(e)
                    .iter()
                    .find(|c| label(c) == "key")
                    .ok_or("header with no key")?;
                let parts = key_parts(key)?;
                let (last, prefix) = parts.split_last().ok_or("empty header")?;
                let t = header_prefix(&mut root, prefix)?;
                match t.map.get_mut(last) {
                    None => {
                        t.map
                            .insert(last.clone(), Item::Aot(vec![Table::new(Kind::Header)]));
                    }
                    Some(Item::Aot(v)) => v.push(Table::new(Kind::Header)),
                    Some(_) => {
                        return Err(format!("{} is not an array of tables", parts.join(".")));
                    }
                }
                current = parts;
            }
            _ => {}
        }
    }
    Ok(root)
}

fn collect_exprs<'a>(n: &'a Obs, out: &mut Vec<&'a Obs>) {
    for c in kids(n) {
        match label(c) {
            "keyval" | "std-table" | "array-table" => out.push(c),
            "val" => {}
            _ => collect_exprs(c, out),
        }
    }
}

// ── Comparison with toml-test's tagged JSON ─────────────────────────────────

fn compare(t: &Table, want: &J, at: &str) -> Result<(), String> {
    let J::Object(w) = want else {
        return Err(format!("{at}: expected a table"));
    };
    if w.len() != t.map.len() {
        return Err(format!("{at}: {} keys, expected {}", t.map.len(), w.len()));
    }
    for (k, item) in &t.map {
        let sub = w
            .get(k)
            .ok_or_else(|| format!("{at}.{k}: unexpected key"))?;
        let path = format!("{at}.{k}");
        match item {
            Item::Table(tt) => compare(tt, sub, &path)?,
            Item::Aot(v) => {
                let J::Array(ws) = sub else {
                    return Err(format!("{path}: expected an array"));
                };
                if ws.len() != v.len() {
                    return Err(format!("{path}: {} tables, expected {}", v.len(), ws.len()));
                }
                for (i, (tt, ww)) in v.iter().zip(ws).enumerate() {
                    compare(tt, ww, &format!("{path}[{i}]"))?;
                }
            }
            Item::Val(v) => compare_val(v, sub, &path)?,
        }
    }
    Ok(())
}

fn compare_val(v: &Val, want: &J, at: &str) -> Result<(), String> {
    if let Val::Table(t) = v {
        return compare(t, want, at);
    }
    if let Val::Array(xs) = v {
        let J::Array(ws) = want else {
            return Err(format!("{at}: expected an array"));
        };
        if xs.len() != ws.len() {
            return Err(format!(
                "{at}: {} elements, expected {}",
                xs.len(),
                ws.len()
            ));
        }
        for (i, (x, w)) in xs.iter().zip(ws).enumerate() {
            compare_val(x, w, &format!("{at}[{i}]"))?;
        }
        return Ok(());
    }
    let (Some(ty), Some(val)) = (
        want.get("type").and_then(J::as_str),
        want.get("value").and_then(J::as_str),
    ) else {
        return Err(format!("{at}: expected a tagged scalar"));
    };
    let ok = match v {
        Val::Str(s) => ty == "string" && s == val,
        Val::Int(i) => ty == "integer" && val.parse::<i64>().ok() == Some(*i),
        Val::Float(f) => {
            ty == "float"
                && match float(val) {
                    Ok(g) => (f.is_nan() && g.is_nan()) || *f == g,
                    Err(_) => false,
                }
        }
        Val::Bool(b) => ty == "bool" && val == if *b { "true" } else { "false" },
        Val::Time(kind, dt) => *kind == ty && datetime(val).ok().as_ref() == Some(dt),
        Val::Array(_) | Val::Table(_) => unreachable!(),
    };
    if ok {
        Ok(())
    } else {
        Err(format!("{at}: {v:?} against {ty} {val:?}"))
    }
}
