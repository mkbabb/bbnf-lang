//! R7RS datum syntax's oracle: lexpr 0.2.7 (ruling H-1), and the corpus.
//!
//! lexpr is an S-expression reader with R6RS-style strings and characters; it
//! does not read the whole R7RS datum syntax (no `#|…|#` or `#;` comments, no
//! `#true`/`#false`, no datum labels, no `|…|` identifiers, no rationals,
//! complexes or exactness prefixes), and it reads some forms R7RS does not
//! define (`[ ]` as list brackets, `#nil`, `#vu8(`, R6RS character names).
//! Its authority is therefore declared, not assumed: it judges the cases under
//! `r7rs/cases/lexpr/`, which are written in the intersection of the two
//! syntaxes, by verdict and, where both accept, by datum value. The cases
//! under `r7rs/cases/spec/` exercise R7RS forms outside lexpr's reader; they
//! have no independent oracle and are listed as a gap, never as a pass.

use std::fmt::Write as _;

use lexpr::Value;

use super::{Judgement, Oracle};
use crate::arm::{Case, Outcome};
use crate::obs::{Obs, normalise};
use crate::registry::Grammar;
use crate::sample::Rng;

pub struct Lexpr;

/// lexpr's reading of a whole input: every datum, or the error.
pub fn read_all(bytes: &[u8]) -> Result<Vec<Value>, String> {
    let mut p = lexpr::Parser::from_slice_custom(bytes, lexpr::parse::Options::new());
    let mut out = Vec::new();
    loop {
        match p.next_value() {
            Ok(Some(v)) => out.push(v),
            Ok(None) => return Ok(out),
            Err(e) => return Err(e.to_string()),
        }
    }
}

impl Oracle for Lexpr {
    fn name(&self) -> &'static str {
        "lexpr"
    }

    fn applies(&self, g: &Grammar) -> bool {
        g.id == "r7rs_datum"
    }

    fn expected_verdict(&self, _g: &Grammar, _i: usize, case: &Case) -> Option<bool> {
        case.id
            .starts_with("r7rs/cases/lexpr/")
            .then(|| read_all(&case.bytes()).is_ok())
    }

    fn judge(&self, _g: &Grammar, _i: usize, case: &Case, out: &Outcome) -> Judgement {
        if !case.id.starts_with("r7rs/cases/lexpr/") {
            return Judgement::NotApplicable;
        }
        let want = read_all(&case.bytes());
        match (want, out) {
            (_, Outcome::GrammarRefused(m)) => {
                Judgement::Diverge(format!("arm refused the grammar: {m}"))
            }
            (_, Outcome::Crash(m)) => Judgement::Diverge(format!("arm crashed: {m}")),
            (Err(_), Outcome::Reject(_)) => Judgement::Agree,
            (Err(e), Outcome::Accept { .. }) => {
                Judgement::Diverge(format!("lexpr rejects ({e}), arm accepted"))
            }
            (Ok(_), Outcome::Reject(_)) => Judgement::Diverge("lexpr accepts, arm rejected".into()),
            (Ok(vals), Outcome::Accept { obs, .. }) => match decode(obs) {
                Ok(got) if got == vals => Judgement::Agree,
                Ok(got) => Judgement::Diverge(format!(
                    "data differ: arm {} lexpr {}",
                    got.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                    vals.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )),
                Err(e) => Judgement::Diverge(format!("arm tree not decodable: {e}")),
            },
        }
    }
}

// ── Decoding an arm's rule-labelled tree into lexpr values ───────────────────

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

/// The data of a whole input, in order (comments and atmosphere skipped).
fn decode(obs: &Obs) -> Result<Vec<Value>, String> {
    let t = normalise(obs);
    kids(&t)
        .iter()
        .filter(|c| label(c) == "datum")
        .map(datum)
        .collect()
}

fn datums(n: &Obs) -> Vec<&Obs> {
    kids(n).iter().filter(|c| label(c) == "datum").collect()
}

fn first_labelled<'a>(n: &'a Obs, set: &[&str]) -> Option<&'a Obs> {
    if set.contains(&label(n)) {
        return Some(n);
    }
    kids(n).iter().find_map(|c| first_labelled(c, set))
}

const FORMS: &[&str] = &[
    "boolean",
    "number",
    "character",
    "string",
    "symbol",
    "bytevector",
    "list",
    "vector",
    "abbreviation",
    "labelled",
];

fn datum(n: &Obs) -> Result<Value, String> {
    let f = first_labelled(n, FORMS).ok_or("a datum with no form")?;
    let t = text(f);
    Ok(match label(f) {
        "boolean" => Value::Bool(t.eq_ignore_ascii_case("#t") || t.eq_ignore_ascii_case("#true")),
        "number" => number(&t)?,
        "character" => Value::Char(character(&t)?),
        "string" => Value::string(string(&t[1..t.len() - 1])?),
        "symbol" => Value::symbol(t),
        "bytevector" => {
            let bytes = kids(f)
                .iter()
                .filter(|c| label(c) == "byte")
                .map(|b| {
                    text(b)
                        .parse::<u8>()
                        .map_err(|e| format!("byte {}: {e}", text(b)))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Value::bytes(bytes)
        }
        "vector" => Value::vector(
            datums(f)
                .into_iter()
                .map(datum)
                .collect::<Result<Vec<_>, _>>()?,
        ),
        "list" => {
            let ds: Vec<Value> = datums(f).into_iter().map(datum).collect::<Result<_, _>>()?;
            let dotted = kids(f)
                .iter()
                .any(|c| matches!(c, Obs::Leaf { text, .. } if text == "."));
            if dotted {
                let (tail, init) = ds.split_last().ok_or("dotted list with no tail")?;
                Value::append(init.to_vec(), tail.clone())
            } else {
                Value::list(ds)
            }
        }
        "abbreviation" => {
            let prefix = kids(f)
                .iter()
                .find(|c| label(c) == "abbrev-prefix")
                .map(text)
                .unwrap_or_default();
            let name = match prefix.as_str() {
                "'" => "quote",
                "`" => "quasiquote",
                ",@" => "unquote-splicing",
                _ => "unquote",
            };
            let inner = datums(f)
                .into_iter()
                .next()
                .ok_or("abbreviation with no datum")?;
            Value::list(vec![Value::symbol(name), datum(inner)?])
        }
        other => return Err(format!("{other} is outside lexpr's reader")),
    })
}

fn number(t: &str) -> Result<Value, String> {
    let lower = t.to_ascii_lowercase();
    let (radix, body) = match lower.get(..2) {
        Some("#x") => (16, &lower[2..]),
        Some("#o") => (8, &lower[2..]),
        Some("#b") => (2, &lower[2..]),
        Some("#d") => (10, &lower[2..]),
        _ => (10, lower.as_str()),
    };
    if let Ok(i) = i64::from_str_radix(body.trim_start_matches('+'), radix) {
        return Ok(Value::Number(i.into()));
    }
    if radix == 10
        && let Ok(f) = body.parse::<f64>()
    {
        return Ok(Value::Number(f.into()));
    }
    Err(format!("number {t} is outside lexpr's reader"))
}

fn character(t: &str) -> Result<char, String> {
    let body = &t[2..];
    Ok(match body {
        "alarm" => '\u{7}',
        "backspace" => '\u{8}',
        "delete" => '\u{7f}',
        "newline" => '\n',
        "return" => '\r',
        "space" => ' ',
        "tab" => '\t',
        _ if body.len() > 1 && body.starts_with('x') => {
            char::from_u32(u32::from_str_radix(&body[1..], 16).map_err(|e| e.to_string())?)
                .ok_or("not a scalar")?
        }
        _ => body.chars().next().ok_or("empty character")?,
    })
}

fn string(s: &str) -> Result<String, String> {
    let mut out = String::new();
    let mut it = s.chars().peekable();
    while let Some(c) = it.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match it.next().ok_or("trailing backslash")? {
            'a' => out.push('\u{7}'),
            'b' => out.push('\u{8}'),
            't' => out.push('\t'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            '"' => out.push('"'),
            '\\' => out.push('\\'),
            '|' => out.push('|'),
            'x' => {
                let hex: String = it.by_ref().take_while(|c| *c != ';').collect();
                out.push(
                    char::from_u32(u32::from_str_radix(&hex, 16).map_err(|e| e.to_string())?)
                        .ok_or("not a scalar")?,
                );
            }
            other => return Err(format!("escape \\{other} is outside lexpr's reader")),
        }
    }
    Ok(out)
}

// ── The corpus (written once, then held in custody; never regenerated to fit) ─

const SYMBOLS: &[&str] = &[
    "a",
    "foo",
    "list->vector",
    "set!",
    "null?",
    "x1",
    "*count*",
    "<=?",
    "make-point",
    "+",
    "-",
    "string-ref",
];
const STRINGS: &[&str] = &[
    "",
    "hello",
    "a\\nb",
    "tab\\there",
    "q\\\"uote",
    "back\\\\slash",
    "bell\\a",
    "\\x41;BC",
    "multi word",
];
const CHARS: &[&str] = &[
    "#\\a",
    "#\\Z",
    "#\\space",
    "#\\newline",
    "#\\tab",
    "#\\x41",
    "#\\(",
    "#\\alarm",
    "#\\delete",
    "#\\return",
    "#\\backspace",
];

fn gen_datum(rng: &mut Rng, depth: u32, out: &mut String) {
    let leaf = depth == 0 || rng.chance(1, 2);
    if leaf {
        match rng.below(6) {
            0 => out.push_str(rng.pick(SYMBOLS)),
            1 => {
                let n = rng.below(100_000) as i64 - 50_000;
                let _ = write!(out, "{n}");
            }
            2 => {
                let _ = write!(out, "{}.{}", rng.below(1000), rng.below(1000));
            }
            3 => {
                let _ = write!(out, "\"{}\"", rng.pick(STRINGS));
            }
            4 => out.push_str(rng.pick(CHARS)),
            _ => out.push_str(if rng.chance(1, 2) { "#t" } else { "#f" }),
        }
        return;
    }
    match rng.below(5) {
        0 | 1 => {
            out.push('(');
            let n = rng.below(4);
            for i in 0..n {
                if i > 0 {
                    out.push(' ');
                }
                gen_datum(rng, depth - 1, out);
            }
            if n > 0 && rng.chance(1, 4) {
                out.push_str(" . ");
                gen_datum(rng, depth - 1, out);
            }
            out.push(')');
        }
        2 => {
            out.push_str("#(");
            for i in 0..rng.below(4) {
                if i > 0 {
                    out.push(' ');
                }
                gen_datum(rng, depth - 1, out);
            }
            out.push(')');
        }
        3 => {
            out.push_str("#u8(");
            for i in 0..rng.below(4) {
                if i > 0 {
                    out.push(' ');
                }
                let _ = write!(out, "{}", rng.below(256));
            }
            out.push(')');
        }
        _ => {
            out.push_str(rng.pick(&["'", "`", ",", ",@"]));
            gen_datum(rng, depth - 1, out);
        }
    }
}

/// In lexpr's domain but not data: each is rejected by both syntaxes.
const LEXPR_INVALID: &[&str] = &[
    "(",
    ")",
    "(a b",
    "a)",
    "(a . )",
    "( . a)",
    "(a . b c)",
    "\"unterminated",
    "\"bad \\q escape\"",
    "#",
    "#(1 2",
    "#u8(1 2",
    "#\\",
    "(a . b . c)",
    "#t#f",
    "'",
    "(1 2 . )",
    "#( . 1)",
    "#u8(a)",
];

/// R7RS forms outside lexpr's reader: valid R7RS data, then invalid R7RS text.
const SPEC_VALID: &[&str] = &[
    "#true",
    "#false",
    "#|block comment|# a",
    "#| nested #| inner |# outer |# b",
    "#;(skipped datum) c",
    "#0=(a . #0#)",
    "|hello world|",
    "|a\\|b|",
    "1/2",
    "#e1.5",
    "#i3/4",
    "#x-1F/3",
    "+inf.0",
    "-nan.0",
    "1+2i",
    "-i",
    "+5i",
    "1@2",
    ".5",
    "1.",
    "1e10",
    "#d#e12",
    "#!fold-case ABC",
    "#\\null",
    "#\\escape",
    "#\\x3bb",
    "\"line\\\n   continued\"",
    "\"\\x3bb;\"",
    "(#u8(1 2) #(a))",
    "`(a ,b ,@c)",
];
const SPEC_INVALID: &[&str] = &[
    "#|unterminated",
    "1/",
    "#e",
    "1a",
    "#truex",
    "#\\abc",
    "|unterminated",
    "1@",
    "#!unknown",
    "#u8(256 . 1)",
    "#true#false",
];

/// Writes the corpus files under `dir` (`lexpr/` and `spec/`), returning their
/// relative paths. Deterministic: seed 20261111.
pub fn write_corpus(dir: &std::path::Path) -> anyhow::Result<Vec<String>> {
    let mut rels = Vec::new();
    let mut put = |rel: String, body: &str| -> anyhow::Result<()> {
        let p = dir.join(&rel);
        std::fs::create_dir_all(p.parent().unwrap())?;
        std::fs::write(&p, body)?;
        rels.push(rel);
        Ok(())
    };
    let mut rng = Rng::new(20_261_111);
    for i in 0..400 {
        let mut body = String::new();
        if rng.chance(1, 5) {
            body.push_str("; a line comment\n");
        }
        for j in 0..1 + rng.below(4) {
            if j > 0 {
                body.push_str(if rng.chance(1, 3) { "\n" } else { " " });
            }
            gen_datum(&mut rng, 4, &mut body);
        }
        body.push('\n');
        put(format!("r7rs/cases/lexpr/gen-{i:04}.scm"), &body)?;
    }
    for (i, s) in LEXPR_INVALID.iter().enumerate() {
        put(format!("r7rs/cases/lexpr/invalid-{i:02}.scm"), s)?;
    }
    for (i, s) in SPEC_VALID.iter().enumerate() {
        put(format!("r7rs/cases/spec/valid-{i:02}.scm"), s)?;
    }
    for (i, s) in SPEC_INVALID.iter().enumerate() {
        put(format!("r7rs/cases/spec/invalid-{i:02}.scm"), s)?;
    }
    Ok(rels)
}
