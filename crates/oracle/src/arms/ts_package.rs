//! The TS-package arm: the published `@mkbabb/bbnf-lang` 0.3.0, unpacked from
//! its npm tarball at a pinned sha256 (`ts/pin.json`; never `typescript/`
//! HEAD), driven by `ts/driver.mjs` under node.
//!
//! 0.3.0 predates reading `->` mappings. The specification says a mapping
//! never changes what the factor matches and that a front end that does not
//! emit Rust keeps the expression as text (`docs/bbnf/grammar-syntax.md`), so
//! the arm is handed each grammar file with its mappings erased. The mapping
//! spans come from the grammar of record (`grammar/bbnf/bbnf.bbnf`), through
//! its generated parser: no second recognizer of BBNF is written here. A file
//! the grammar of record cannot read is passed unchanged.

use std::collections::BTreeMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, ensure};
use bbnf::grammar::generated::bbnf::BbnfBootstrap;
use bbnf::runtime::{BbnfCompoundKind, BbnfView, RuntimeView};
use serde::Deserialize;

use crate::arm::{Arm, Case, Outcome};
use crate::obs::{FailureContract, Obs};
use crate::registry::{Grammar, repo_root};

pub struct TsPackage {
    /// Erase `->` mappings before handing a grammar to 0.3.0.
    pub erase_mappings: bool,
}

impl Default for TsPackage {
    fn default() -> Self {
        Self {
            erase_mappings: true,
        }
    }
}

/// The byte ranges of every mapping (`-> expr [: Type]`) in a BBNF text, by
/// the grammar of record's parse. `None` when that parser refuses the text.
pub fn mapping_ranges(text: &str) -> Option<Vec<(usize, usize)>> {
    let doc = BbnfBootstrap::parse(text).ok()?;
    let mut out = Vec::new();
    collect(doc.view(), &mut out);
    out.sort();
    Some(out)
}

fn collect(v: BbnfView<'_, '_>, out: &mut Vec<(usize, usize)>) {
    if v.compound_kind() == Some(BbnfCompoundKind::MappedFactor)
        && let (Some(factor), Some((_, end))) = (v.child(0), v.byte_span())
    {
        if let Some((_, fend)) = factor.byte_span()
            && end > fend
        {
            out.push((fend as usize, end as usize));
        }
        // A mapping's factor may itself hold mapped factors (a group).
        collect(factor, out);
        return;
    }
    for c in v.children() {
        collect(c, out);
    }
}

/// `text` with each mapping replaced by spaces (offsets and line numbers kept).
pub fn erase_mappings(text: &str) -> Option<String> {
    let ranges = mapping_ranges(text)?;
    let mut bytes = text.as_bytes().to_vec();
    for (s, e) in ranges {
        for b in &mut bytes[s..e] {
            if *b != b'\n' {
                *b = b' ';
            }
        }
    }
    // Only ASCII bytes were replaced inside a mapping's span when the span is
    // ASCII; a non-ASCII mapping text is blanked byte-wise, which keeps the
    // result valid UTF-8 only if whole scalars were replaced, so check.
    String::from_utf8(bytes).ok()
}

/// Every grammar file under `grammar/` and the held-out set, erased.
fn sources() -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    let mut stack = vec![repo_root().join("grammar")];
    while let Some(d) = stack.pop() {
        let Ok(rd) = std::fs::read_dir(&d) else {
            continue;
        };
        for e in rd.flatten() {
            let p = e.path();
            if p.is_dir() {
                stack.push(p);
            } else if p.extension().is_some_and(|x| x == "bbnf")
                && let Ok(text) = std::fs::read_to_string(&p)
                && let Some(erased) = erase_mappings(&text)
                && erased != text
            {
                out.insert(p.to_string_lossy().into_owned(), erased);
            }
        }
    }
    out
}

#[derive(Deserialize)]
struct Line {
    verdict: String,
    #[serde(default)]
    obs: Option<Obs>,
    #[serde(default)]
    furthest: Option<u32>,
    #[serde(default)]
    expected: Vec<String>,
    #[serde(default)]
    message: Option<String>,
}

/// Runs a node script with one JSON request on stdin; one JSON line per result.
pub fn run_node(
    script: &Path,
    request: &serde_json::Value,
) -> anyhow::Result<Vec<serde_json::Value>> {
    let mut child = Command::new("node")
        .arg(script)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("spawn node (the JS oracles need node on PATH)")?;
    child
        .stdin
        .take()
        .unwrap()
        .write_all(serde_json::to_vec(request)?.as_slice())?;
    let out = child.wait_with_output()?;
    ensure!(
        out.status.success(),
        "{} failed ({}): {}",
        script.display(),
        out.status,
        String::from_utf8_lossy(&out.stderr)
            .chars()
            .take(2000)
            .collect::<String>()
    );
    String::from_utf8(out.stdout)?
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            // Trees nest as deep as the inputs: no recursion limit.
            use serde::Deserialize;
            let mut de = serde_json::Deserializer::from_str(l);
            de.disable_recursion_limit();
            serde_json::Value::deserialize(&mut de)
                .with_context(|| format!("driver line: {}", &l[..l.len().min(200)]))
        })
        .collect()
}

pub fn driver() -> PathBuf {
    repo_root().join("crates/oracle/ts/driver.mjs")
}

impl TsPackage {
    /// Runs one job per (grammar, mode) and returns the raw lines in case order.
    pub fn request(
        &self,
        mode: &str,
        g: &Grammar,
        cases: &[Case],
    ) -> anyhow::Result<Vec<serde_json::Value>> {
        let pkg = crate::npm::ensure("ts-package")?;
        let mut req = serde_json::json!({
            "package": pkg,
            "jobs": [{
                "id": g.id,
                "mode": mode,
                "grammar": repo_root().join(g.path),
                "entry": g.entry,
                "inputs": cases.iter().map(|c| c.path.clone()).collect::<Vec<_>>(),
            }],
        });
        if self.erase_mappings {
            req["sources"] = serde_json::to_value(sources())?;
        }
        let lines = run_node(&driver(), &req)?;
        ensure!(
            lines.len() == cases.len(),
            "driver returned {} lines for {} cases",
            lines.len(),
            cases.len()
        );
        Ok(lines)
    }
}

impl Arm for TsPackage {
    fn name(&self) -> &'static str {
        "ts-package"
    }

    fn supports(&self, _g: &Grammar) -> bool {
        true
    }

    fn run(&mut self, g: &Grammar, cases: &[Case]) -> anyhow::Result<Vec<Outcome>> {
        let lines = self.request("parse", g, cases)?;
        lines
            .into_iter()
            .zip(cases)
            .map(|(v, c)| {
                let l: Line = serde_json::from_value(v)?;
                Ok(match l.verdict.as_str() {
                    "accept" => Outcome::Accept {
                        obs: l.obs.unwrap_or(Obs::node(None, None, vec![])),
                        value: None,
                    },
                    "reject" => {
                        let mut e = l.expected;
                        e.sort();
                        e.dedup();
                        Outcome::Reject(FailureContract::new(l.furthest.unwrap_or(0), e)?)
                    }
                    "unreadable" => Outcome::not_utf8(&c.bytes()),
                    _ => Outcome::GrammarRefused(l.message.unwrap_or_default()),
                })
            })
            .collect()
    }
}
