//! The root arm: root `backend/rust`'s generated parsers for the 9 grammars
//! (`bbnf::grammar::generated::*`), observed through the grammar-agnostic
//! `RuntimeView` surface. Root is the second oracle until LW4.COMP-1 (quarry
//! law X-1); it shares the front end with the spine, so it is never counted as
//! an independent oracle.
//!
//! Each parse runs in a child process (`g1 __root <grammar>`) on a thread
//! with a 1 GiB stack, so JSONTestSuite's deepest nesting files
//! (`n_structure_100000_opening_arrays`) reach the parser's own verdict, not
//! the default thread stack; the parent bounds the child's memory, so a parse
//! whose memory grows without bound is a recorded crash, not a lost run.

use bbnf::grammar::generated::{
    bbnf::BbnfBootstrap, bnf::BnfParser, css_l4::CssL4Parser, css_pretty::CssPrettyParser,
    csv::CsvParser, ebnf::EbnfParser, google_sheets::GoogleSheetsParser, json::JsonParser,
    math::MathParser,
};
use bbnf::runtime::{JsonDocument, JsonNumber, JsonValue, ParseErr, RuntimeView};

use crate::arm::{Arm, Case, Outcome};
use crate::obs::{FailureContract, Obs};
use crate::registry::Grammar;

const STACK: usize = 1 << 30;

pub struct Root;

fn reject(e: ParseErr) -> Outcome {
    match e {
        ParseErr::Syntax { offset, .. } => Outcome::Reject(FailureContract::at(offset)),
    }
}

fn walk<'p, V: RuntimeView<'p>>(v: V, base: usize) -> Obs {
    let label = format!("{:?}", v.kind());
    let span = v.span().map(|s| {
        let start = (s.as_ptr() as usize).wrapping_sub(base) as u32;
        (start, start + s.len() as u32)
    });
    let children: Vec<Obs> = v.children().map(|c| walk(c, base)).collect();
    if children.is_empty() {
        Obs::Leaf {
            label: Some(label),
            span,
            text: v.span().unwrap_or("").to_string(),
        }
    } else {
        Obs::Node {
            label: Some(label),
            span,
            children,
        }
    }
}

fn json_obs(doc: &JsonDocument<'_>, v: &JsonValue<'_>) -> Obs {
    match v {
        JsonValue::Null => Obs::leaf(Some("null"), None, "null"),
        JsonValue::Bool(b) => Obs::leaf(Some("bool"), None, b.to_string()),
        JsonValue::Number(n) => Obs::leaf(Some("number"), None, number_text(n)),
        JsonValue::String(s) => Obs::leaf(Some("string"), None, *s),
        JsonValue::Array(id) => Obs::node(
            Some("array"),
            None,
            doc.array(*id).iter().map(|x| json_obs(doc, x)).collect(),
        ),
        JsonValue::Object(id) => Obs::node(
            Some("object"),
            None,
            doc.object(*id)
                .iter()
                .map(|p| {
                    Obs::node(
                        Some("pair"),
                        None,
                        vec![
                            Obs::leaf(Some("string"), None, p.key),
                            json_obs(doc, &p.value),
                        ],
                    )
                })
                .collect(),
        ),
    }
}

fn number_text(n: &JsonNumber) -> String {
    match n {
        JsonNumber::Int(i) => i.to_string(),
        JsonNumber::UInt(u) => u.to_string(),
        JsonNumber::Float(f) => format!("{f:?}"),
    }
}

/// Root's JSON document in the JSON data model, for the serde_json comparator.
fn json_value(doc: &JsonDocument<'_>, v: &JsonValue<'_>) -> serde_json::Value {
    use serde_json::Value as V;
    match v {
        JsonValue::Null => V::Null,
        JsonValue::Bool(b) => V::Bool(*b),
        JsonValue::Number(n) => match n {
            JsonNumber::Int(i) => V::from(*i),
            JsonNumber::UInt(u) => V::from(*u),
            JsonNumber::Float(f) => serde_json::Number::from_f64(*f)
                .map(V::Number)
                .unwrap_or(V::Null),
        },
        JsonValue::String(s) => V::String((*s).to_string()),
        JsonValue::Array(id) => {
            V::Array(doc.array(*id).iter().map(|x| json_value(doc, x)).collect())
        }
        JsonValue::Object(id) => {
            // Pairs in source order; a repeated key keeps its last value, as
            // serde_json's map does.
            let mut m = serde_json::Map::new();
            for p in doc.object(*id) {
                m.insert(p.key.to_string(), json_value(doc, &p.value));
            }
            V::Object(m)
        }
    }
}

macro_rules! generic {
    ($parser:ty, $src:expr) => {{
        let src: &str = $src;
        match <$parser>::parse(src) {
            Ok(doc) => Outcome::Accept {
                obs: walk(doc.view(), src.as_ptr() as usize),
                value: None,
            },
            Err(e) => reject(e),
        }
    }};
}

fn parse_one(id: &str, bytes: Vec<u8>) -> Outcome {
    let src = match std::str::from_utf8(&bytes) {
        Ok(s) => s,
        Err(_) => return Outcome::not_utf8(&bytes),
    };
    match id {
        "json" => match JsonParser::parse(src) {
            Ok(doc) => Outcome::Accept {
                obs: json_obs(&doc, doc.root()),
                value: Some(json_value(&doc, doc.root())),
            },
            Err(e) => reject(e),
        },
        "bbnf" => generic!(BbnfBootstrap, src),
        "css_l4" => generic!(CssL4Parser, src),
        "css_pretty" => generic!(CssPrettyParser, src),
        "google_sheets" => generic!(GoogleSheetsParser, src),
        "ebnf" => generic!(EbnfParser, src),
        "bnf" => generic!(BnfParser, src),
        "csv" => generic!(CsvParser, src),
        "math" => generic!(MathParser, src),
        other => Outcome::GrammarRefused(format!(
            "root backend/rust has no generated parser for {other}"
        )),
    }
}

/// The memory bound one root child may reach before the case in flight is
/// recorded as a crash (`BBNF_ORACLE_ROOT_RSS_MB`, default 4096).
fn rss_budget_kb() -> u64 {
    std::env::var("BBNF_ORACLE_ROOT_RSS_MB")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(4096)
        * 1024
}

fn rss_kb(pid: u32) -> Option<u64> {
    let out = std::process::Command::new("ps")
        .args(["-o", "rss=", "-p", &pid.to_string()])
        .output()
        .ok()?;
    String::from_utf8_lossy(&out.stdout).trim().parse().ok()
}

/// The child half: reads case paths (one per line) on stdin and writes one
/// [`Outcome`] per line on stdout, flushed after each, parsing on a thread
/// with a 1 GiB stack. `g1 __root <grammar>` calls this.
pub fn child_main(grammar: &str) -> anyhow::Result<()> {
    use std::io::{BufRead, Write};
    let grammar = grammar.to_string();
    std::thread::Builder::new()
        .stack_size(STACK)
        .spawn(move || -> anyhow::Result<()> {
            let stdin = std::io::stdin();
            let mut stdout = std::io::stdout().lock();
            for line in stdin.lock().lines() {
                let path = line?;
                let bytes = std::fs::read(&path)?;
                let out =
                    std::panic::catch_unwind(|| parse_one(&grammar, bytes)).unwrap_or_else(|p| {
                        let msg = p
                            .downcast_ref::<String>()
                            .cloned()
                            .or_else(|| p.downcast_ref::<&str>().map(|s| s.to_string()))
                            .unwrap_or_default();
                        Outcome::Crash(format!("panic: {msg}"))
                    });
                serde_json::to_writer(&mut stdout, &out)?;
                stdout.write_all(b"\n")?;
                stdout.flush()?;
            }
            Ok(())
        })?
        .join()
        .map_err(|_| anyhow::anyhow!("root child thread panicked"))?
}

/// One outcome line from the child; its trees nest as deep as the input.
fn decode(line: &str) -> anyhow::Result<Outcome> {
    use serde::Deserialize;
    let mut de = serde_json::Deserializer::from_str(line);
    de.disable_recursion_limit();
    Ok(Outcome::deserialize(&mut de)?)
}

/// Runs `cases[from..]` in one child until it finishes or dies; returns the
/// outcomes it delivered and, when it died early, why.
fn run_child(grammar: &str, cases: &[Case]) -> anyhow::Result<(Vec<Outcome>, Option<String>)> {
    use std::io::{BufRead, BufReader, Write};
    use std::process::{Command, Stdio};
    use std::sync::mpsc;
    let exe = std::env::current_exe()?;
    let mut child = Command::new(exe)
        .args(["__root", grammar])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;
    let pid = child.id();
    let mut stdin = child.stdin.take().expect("piped");
    let paths: String = cases
        .iter()
        .map(|c| format!("{}\n", c.path.display()))
        .collect();
    let feeder = std::thread::spawn(move || {
        let _ = stdin.write_all(paths.as_bytes());
    });
    let stdout = child.stdout.take().expect("piped");
    let (tx, rx) = mpsc::channel::<String>();
    let reader = std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines() {
            match line {
                Ok(l) => {
                    if tx.send(l).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });
    let budget = rss_budget_kb();
    let mut outs = Vec::new();
    let mut killed: Option<String> = None;
    loop {
        match rx.recv_timeout(std::time::Duration::from_millis(50)) {
            Ok(l) => {
                outs.push(decode(&l)?);
                if outs.len() == cases.len() {
                    break;
                }
            }
            Err(mpsc::RecvTimeoutError::Disconnected) => break,
            Err(mpsc::RecvTimeoutError::Timeout) => {
                if let Some(kb) = rss_kb(pid)
                    && kb > budget
                {
                    let _ = child.kill();
                    killed = Some(format!(
                        "exceeded the root arm's {} MiB memory bound",
                        budget / 1024
                    ));
                    break;
                }
            }
        }
    }
    let status = child.wait()?;
    let _ = feeder.join();
    while let Ok(l) = rx.try_recv() {
        if outs.len() < cases.len() && killed.is_none() {
            outs.push(decode(&l)?);
        }
    }
    let _ = reader.join();
    if outs.len() == cases.len() {
        return Ok((outs, None));
    }
    Ok((
        outs,
        Some(killed.unwrap_or_else(|| format!("the parse process died ({status})"))),
    ))
}

impl Arm for Root {
    fn name(&self) -> &'static str {
        "root"
    }

    fn supports(&self, g: &Grammar) -> bool {
        crate::registry::NINE.contains(&g.id)
    }

    /// Each batch runs in a child process (`g1 __root`), watched for memory:
    /// a case that kills the child (stack overflow abort, or the memory bound)
    /// is recorded as a crash and the child restarts after it.
    fn run(&mut self, g: &Grammar, cases: &[Case]) -> anyhow::Result<Vec<Outcome>> {
        let mut outs: Vec<Outcome> = Vec::with_capacity(cases.len());
        while outs.len() < cases.len() {
            let (got, died) = run_child(g.id, &cases[outs.len()..])?;
            outs.extend(got);
            if let Some(why) = died {
                outs.push(Outcome::Crash(why));
            }
        }
        Ok(outs)
    }
}
