//! The oracle of the EBNF, BNF and Math grammars of record (ruling ORC-2,
//! 2026-11-12a): peggy 5.1.1, pinned by sha256 in `ts/pin.json` (set
//! "peggy"), reading a hand transcription of each grammar into PEG
//! (`crates/oracle/peg/<id>.peggy`), driven by `peg/peggy.mjs`; and the
//! custody corpora those grammars are read on (`g1 corpus-gen <id>`).
//!
//! The three grammars are dialects defined only by their `.bbnf` text: no
//! external reader of them exists. The oracle is therefore an independent
//! reader of that text: a second transcription by hand into a PEG engine that
//! shares no front end, generator or runtime with bbnf-lang, under the
//! semantics docs/bbnf states (each transcription's header gives the reading
//! of every operator it uses). A transcription names the grammar file and the
//! sha256 it was written from, and the oracle refuses to judge when the file
//! has moved since: an edited grammar needs its transcription read again.
//!
//! The oracle judges verdicts. The corpora are generated deterministically
//! (seed below) from a hand model of each language, with seeded one-byte
//! mutations of every generated case, so both verdicts are well represented;
//! which cases are in the language is decided by the oracle, never by the
//! generator.

use std::path::Path;

use super::{Judgement, Oracle, verdict_judgement};
use crate::arm::{Case, Outcome};
use crate::arms::ts_package::run_node;
use crate::registry::{Grammar, repo_root};
use crate::sample::Rng;

/// The grammars this oracle reads, with their transcription and start rule.
pub const GRAMMARS: [(&str, &str); 3] = [("ebnf", "grammar"), ("bnf", "grammar"), ("math", "number")];

/// The seed of every generated corpus (`g1 corpus-gen ebnf|bnf|math`).
pub const SEED: u64 = 20_261_112;

#[derive(Default)]
pub struct Peggy {
    verdicts: Vec<Option<bool>>,
}

fn start_rule(id: &str) -> Option<&'static str> {
    GRAMMARS.iter().find(|(g, _)| *g == id).map(|(_, s)| *s)
}

/// Checks that a transcription was written from the grammar file as it is.
pub fn check_transcription(g: &Grammar) -> anyhow::Result<std::path::PathBuf> {
    let root = repo_root();
    let peg = root.join("crates/oracle/peg").join(format!("{}.peggy", g.id));
    let text = std::fs::read_to_string(&peg)?;
    let line = text
        .lines()
        .find_map(|l| l.strip_prefix("// transcribes: "))
        .ok_or_else(|| anyhow::anyhow!("{}: no `// transcribes:` line", peg.display()))?;
    let (path, sha) = line
        .split_once(" sha256 ")
        .ok_or_else(|| anyhow::anyhow!("{}: malformed `// transcribes:` line", peg.display()))?;
    anyhow::ensure!(
        path == g.path,
        "{}: transcribes {path}, the registry reads {}",
        peg.display(),
        g.path
    );
    let now = crate::npm::sha256_file(&root.join(path))?;
    anyhow::ensure!(
        now == sha,
        "{path} moved since its transcription was written (sha256 {now}, transcription {sha}): read the grammar again into {}",
        peg.display()
    );
    Ok(peg)
}

/// peggy's verdict on each case: `Some(true)` accept, `Some(false)` reject,
/// `None` when the input is not UTF-8 (no oracle has authority over it).
pub fn run(g: &Grammar, cases: &[Case]) -> anyhow::Result<Vec<Option<bool>>> {
    let peg = check_transcription(g)?;
    let start = start_rule(g.id).ok_or_else(|| anyhow::anyhow!("peggy: no start rule for {}", g.id))?;
    let pkg = crate::npm::ensure("peggy")?;
    let req = serde_json::json!({
        "package": pkg,
        "grammar": peg,
        "start": start,
        "inputs": cases.iter().map(|c| c.path.clone()).collect::<Vec<_>>(),
    });
    let lines = run_node(&repo_root().join("crates/oracle/peg/peggy.mjs"), &req)?;
    anyhow::ensure!(
        lines.len() == cases.len(),
        "peggy returned {} lines for {} cases",
        lines.len(),
        cases.len()
    );
    Ok(lines
        .iter()
        .map(|l| match l["verdict"].as_str() {
            Some("accept") => Some(true),
            Some("reject") => Some(false),
            _ => None,
        })
        .collect())
}

impl Oracle for Peggy {
    fn name(&self) -> &'static str {
        "peggy"
    }

    fn applies(&self, g: &Grammar) -> bool {
        start_rule(g.id).is_some()
    }

    fn prepare(&mut self, g: &Grammar, cases: &[Case]) -> anyhow::Result<bool> {
        self.verdicts = run(g, cases)?;
        Ok(!cases.is_empty())
    }

    fn expected_verdict(&self, _g: &Grammar, i: usize, _case: &Case) -> Option<bool> {
        self.verdicts.get(i).copied().flatten()
    }

    fn judge(&self, _g: &Grammar, i: usize, _case: &Case, out: &Outcome) -> Judgement {
        match self.verdicts.get(i).copied().flatten() {
            Some(expect) => verdict_judgement(expect, out),
            None => Judgement::NotApplicable,
        }
    }
}

// ── The corpora ────────────────────────────────────────────────────────────

/// One seeded edit of a case: a byte deleted, a byte inserted from the
/// language's alphabet, or a byte replaced by one. Applied at a char boundary.
fn mutate(rng: &mut Rng, s: &str, alphabet: &[&str]) -> String {
    let bounds: Vec<usize> = s
        .char_indices()
        .map(|(i, _)| i)
        .chain(std::iter::once(s.len()))
        .collect();
    let at = bounds[rng.below(bounds.len() as u64) as usize];
    let next = bounds.iter().copied().find(|&b| b > at).unwrap_or(s.len());
    let ins = *rng.pick(alphabet);
    match rng.below(3) {
        0 if at < s.len() => format!("{}{}", &s[..at], &s[next..]),
        1 => format!("{}{}{}", &s[..at], ins, &s[at..]),
        _ if at < s.len() => format!("{}{}{}", &s[..at], ins, &s[next..]),
        _ => format!("{s}{ins}"),
    }
}

const EBNF_WS: [&str; 6] = [" ", " ", "\n", "\t", "\r\n", "\x0c"];

fn ebnf_ident(rng: &mut Rng) -> String {
    const L: &[u8] = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const T: &[u8] = b"abcxyzQ019_";
    let mut s = String::new();
    s.push(L[rng.below(L.len() as u64) as usize] as char);
    for _ in 0..rng.below(6) {
        s.push(T[rng.below(T.len() as u64) as usize] as char);
    }
    s
}

fn ebnf_terminal(rng: &mut Rng) -> String {
    // The `character` set: letters, digits, the symbols, `_` and space.
    const C: &[&str] = &[
        "a", "Z", "7", "_", " ", "[", "]", "{", "}", "(", ")", "<", ">", "=", "|", ".", ",", ";",
        "-", "+", "*", "?", "\n", "\t", "\r", "\x0c", "\x08", "\\", "'", "\"",
    ];
    let q = if rng.chance(1, 2) { "'" } else { "\"" };
    let mut s = q.to_string();
    for _ in 0..1 + rng.below(5) {
        let c = *rng.pick(C);
        s.push_str(if c == q { "x" } else { c });
    }
    s.push_str(q);
    s
}

fn ebnf_ws(rng: &mut Rng) -> &'static str {
    if rng.chance(1, 3) { "" } else { rng.pick(&EBNF_WS) }
}

fn ebnf_term(rng: &mut Rng, depth: u32) -> String {
    match if depth == 0 { rng.below(2) } else { rng.below(5) } {
        0 => ebnf_ident(rng),
        1 => ebnf_terminal(rng),
        k => {
            let (o, c) = [("(", ")"), ("[", "]"), ("{", "}")][(k - 2) as usize];
            format!("{o}{}{}{}{c}", ebnf_ws(rng), ebnf_rhs(rng, depth - 1), ebnf_ws(rng))
        }
    }
}

fn ebnf_factor(rng: &mut Rng, depth: u32) -> String {
    let t = ebnf_term(rng, depth);
    match rng.below(6) {
        0 => format!("{t}{}?", ebnf_ws(rng)),
        1 => format!("{t}{}*", ebnf_ws(rng)),
        2 => format!("{t}{}+", ebnf_ws(rng)),
        3 => format!("{t}{}-{}{}", ebnf_ws(rng), ebnf_ws(rng), ebnf_term(rng, depth)),
        _ => t,
    }
}

fn ebnf_rhs(rng: &mut Rng, depth: u32) -> String {
    let mut alts = Vec::new();
    for _ in 0..1 + rng.below(3) {
        let mut cat = Vec::new();
        for _ in 0..1 + rng.below(3) {
            cat.push(ebnf_factor(rng, depth));
        }
        // The comma is optional in this dialect's concatenation.
        let sep = if rng.chance(1, 6) { " " } else { " , " };
        alts.push(cat.join(sep));
    }
    let sep = if rng.chance(1, 8) { " " } else { " | " };
    alts.join(sep)
}

fn ebnf_grammar(rng: &mut Rng) -> String {
    let mut s = String::new();
    for _ in 0..rng.below(5) {
        s.push_str(ebnf_ws(rng));
        s.push_str(&ebnf_ident(rng));
        s.push_str(ebnf_ws(rng));
        s.push('=');
        s.push_str(ebnf_ws(rng));
        s.push_str(&ebnf_rhs(rng, 2));
        s.push_str(ebnf_ws(rng));
        s.push_str(if rng.chance(1, 4) { "." } else { ";" });
        s.push_str(if rng.chance(1, 2) { "\n" } else { ebnf_ws(rng) });
    }
    s
}

const BNF_GAP: [&str; 4] = ["", " ", "\t", "  "];
/// The default `?w` set, plus characters JavaScript's `\s` reads and it does not.
const BNF_W: [&str; 9] = ["\n", "\n\n", " ", "\t", "\r\n", "\x0b", "\x0c", "\u{a0}", "\u{2028}"];

fn bnf_ident(rng: &mut Rng) -> String {
    const L: &[u8] = b"abcXYZ_";
    const T: &[u8] = b"abc019_-";
    let mut s = String::new();
    s.push(L[rng.below(L.len() as u64) as usize] as char);
    for _ in 0..rng.below(6) {
        s.push(T[rng.below(T.len() as u64) as usize] as char);
    }
    s
}

fn bnf_term(rng: &mut Rng) -> String {
    if rng.chance(1, 2) {
        format!("<{}>", bnf_ident(rng))
    } else {
        const C: &[&str] = &["a", "Z", " ", "<", ">", "|", ":", "=", "'", "\\\"", "\\\\", "\\n", "\\t", "é"];
        let mut s = "\"".to_string();
        for _ in 0..rng.below(5) {
            s.push_str(rng.pick(C));
        }
        s.push('"');
        s
    }
}

fn bnf_grammar(rng: &mut Rng) -> String {
    let mut s = String::new();
    for _ in 0..rng.below(5) {
        if rng.chance(1, 3) {
            s.push_str(rng.pick(&BNF_W));
        }
        s.push_str(&format!("<{}>", bnf_ident(rng)));
        s.push_str(rng.pick(&BNF_GAP));
        s.push_str("::=");
        s.push_str(rng.pick(&BNF_GAP));
        for a in 0..1 + rng.below(3) {
            if a > 0 {
                s.push_str(rng.pick(&BNF_GAP));
                s.push('|');
                s.push_str(rng.pick(&BNF_GAP));
            }
            for t in 0..1 + rng.below(3) {
                if t > 0 {
                    s.push_str(rng.pick(&BNF_GAP));
                }
                s.push_str(&bnf_term(rng));
            }
        }
        s.push_str(rng.pick(&BNF_GAP));
        s.push_str(if rng.chance(1, 5) { "" } else { "\n" });
    }
    if rng.chance(1, 3) {
        s.push_str(rng.pick(&BNF_W));
    }
    s
}

fn math_number(rng: &mut Rng) -> String {
    let digits = |rng: &mut Rng, n: u64| -> String {
        (0..n).map(|_| char::from(b'0' + rng.below(10) as u8)).collect()
    };
    let mut s = String::new();
    if rng.chance(3, 4) {
        let n = 1 + rng.below(4);
        s.push_str(&digits(rng, n));
    }
    if rng.chance(1, 2) {
        s.push('.');
        let n = 1 + rng.below(3);
        s.push_str(&digits(rng, n));
    }
    if rng.chance(1, 3) {
        s.push(if rng.chance(1, 2) { 'e' } else { 'E' });
        if rng.chance(1, 2) {
            s.push(if rng.chance(1, 2) { '+' } else { '-' });
        }
        let n = 1 + rng.below(3);
        s.push_str(&digits(rng, n));
    }
    s
}

/// One language's corpus recipe: its sentence generator, the alphabet its
/// one-edit mutations draw from, and its hand cases.
type CorpusRecipe = (fn(&mut Rng) -> String, &'static [&'static str], &'static [&'static str]);

/// Writes the corpus of one grammar under `dir/<id>/cases/` and returns the
/// relative paths written. Each language gets 300 generated cases, a seeded
/// mutation of each, and the hand cases below.
pub fn write_corpus(id: &str, dir: &Path) -> anyhow::Result<Vec<String>> {
    let (make, alphabet, hand): CorpusRecipe = match id {
        "ebnf" => (
            ebnf_grammar,
            &["'", "\"", "=", ";", ".", ",", "|", "(", ")", "[", "]", "{", "}", "-", "?", "*", "+", " ", "\n", "a", "_", "1"],
            &[
                "",
                "a = b ;",
                "a = 'x' | \"y\" , { z } .",
                "a=b;b=c.",
                "digit = '0' | '1' ;\nnumber = digit , { digit } ;\n",
                "a = ( b | [ c ] ) - d ;",
                "a = 'x' ? , 'y' * , 'z' + ;",
                "a = ;",
                "a = b",
                "= b ;",
                "a = '' ;",
                "a = 'it''s' ;",
                "1a = b ;",
                "a_b = c ;",
                "a = b ;;",
                "\x0ca = b ;\x0c",
                "\u{a0}a = b ;",
            ],
        ),
        "bnf" => (
            bnf_grammar,
            &["<", ">", "\"", "|", ":", "=", " ", "\t", "\n", "a", "-", "\\"],
            &[
                "",
                "<a> ::= \"x\"",
                "<a> ::= \"x\"\n",
                "<a> ::= <b> | \"c\"\n<b> ::= \"d\"\n",
                "<a>::=\"x\"|\"y\"\n",
                "<a> ::= \"\\\"\"\n",
                "<a> ::= \"\"\n",
                "<a> ::= \n",
                "<a> := \"x\"\n",
                "<1a> ::= \"x\"\n",
                "<a-b> ::= <c_d>\n",
                "\n\n<a> ::= \"x\"\n\n",
                "<a> ::= \"x\"\x0b<b> ::= \"y\"",
                "<a> ::= \"x\"\x0c\n<b> ::= \"y\"",
                "<a> ::= \"x\"\u{a0}<b> ::= \"y\"",
                "<a> ::= \"x\" <b> ::= \"y\"",
            ],
        ),
        "math" => (
            math_number,
            &["0", "1", "9", ".", "e", "E", "+", "-", " ", "x", "("],
            &[
                "", "0", "00", "1.5", ".5", "1.", ".", "1e3", "1E+3", "1e-3", "1e", "1e+", "e3",
                ".5e1", "1.5.5", " 1", "1 ", "+1", "-1", "1+2", "(1)", "1e3e3", "١",
            ],
        ),
        other => anyhow::bail!("corpus-gen: no generated corpus for {other}"),
    };
    let mut rels = Vec::new();
    let mut put = |rel: String, body: &str| -> anyhow::Result<()> {
        let p = dir.join(&rel);
        std::fs::create_dir_all(p.parent().unwrap())?;
        std::fs::write(&p, body)?;
        rels.push(rel);
        Ok(())
    };
    let mut rng = Rng::new(SEED ^ id.bytes().fold(0u64, |h, b| h.wrapping_mul(131).wrapping_add(b as u64)));
    for i in 0..300 {
        let s = make(&mut rng);
        let m = mutate(&mut rng, &s, alphabet);
        put(format!("{id}/cases/gen-{i:04}.txt"), &s)?;
        put(format!("{id}/cases/mut-{i:04}.txt"), &m)?;
    }
    for (i, s) in hand.iter().enumerate() {
        put(format!("{id}/cases/hand-{i:02}.txt"), s)?;
    }
    Ok(rels)
}
