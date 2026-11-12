//! The value.js leaf corpus: the 115 distinct regex leaves of value.js's six
//! CSS grammars (135 occurrences; `rx/LW1-RX-RECEIPT.md:50` retracts PLAN's
//! 121), each with its OR-7 cases and the V8/JSC answers recorded at LW1.RX-1
//! (`rx/or7/records/or7-{corpus,answers}.json.gz`, custody `rx/or7/records/SHA256`).
//!
//! The arm is the regex engine root `backend/rust` prints for a leaf at the
//! parse-that pin bbnf-lang builds against: `parse_that_regex::Dfa`, a sticky
//! match at offset 0 with the leaf's flags as inline flags. A case diverges
//! when the engine's match end (UTF-8 bytes) differs from the oracle's, or the
//! engine refuses the leaf. A divergence is ruled only by a row of
//! `crates/oracle/ruled.tsv` naming the leaf and the unit that owns its cure.

use std::collections::{BTreeMap, BTreeSet};
use std::process::Command;

use anyhow::{Context, ensure};
use serde::Deserialize;

use super::{Judgement, Oracle};
use crate::arm::{Case, Outcome};
use crate::registry::{Grammar, repo_root};

const RECORDS: &str = "restart/skinny/tranches/sk-v27/rx/or7/records";
const VALUEJS: &str = "value.js@8d7bb5b8";

/// Counted in `--list-oracles` for the CSS grammars; judged by [`run`].
pub struct ValueJsLeavesMarker;

impl Oracle for ValueJsLeavesMarker {
    fn name(&self) -> &'static str {
        "valuejs-leaves"
    }

    fn applies(&self, g: &Grammar) -> bool {
        matches!(g.id, "css_l4" | "css_pretty")
    }

    fn judge(&self, _g: &Grammar, _i: usize, _c: &Case, _o: &Outcome) -> Judgement {
        Judgement::NotApplicable
    }
}

#[derive(Deserialize)]
struct CorpusFile {
    cases: Vec<LeafCase>,
}

#[derive(Deserialize, Clone)]
pub struct LeafCase {
    pub id: String,
    pub pattern: String,
    pub flags: String,
    pub input: String,
}

#[derive(Deserialize)]
struct Answer {
    status: String,
    #[serde(default)]
    end8: Option<usize>,
}

fn gunzip_json<T: for<'de> Deserialize<'de>>(rel: &str) -> anyhow::Result<T> {
    let p = repo_root().join(RECORDS).join(rel);
    let out = Command::new("gzip")
        .arg("-dc")
        .arg(&p)
        .output()
        .with_context(|| format!("gzip -dc {}", p.display()))?;
    ensure!(out.status.success(), "gzip -dc {} failed", p.display());
    Ok(serde_json::from_slice(&out.stdout)?)
}

/// The leaf a case belongs to (`value.js@8d7bb5b8#12|59` → `value.js@8d7bb5b8#12`).
pub fn leaf_of(case_id: &str) -> &str {
    case_id.split('|').next().unwrap_or(case_id)
}

pub struct LeafReport {
    pub leaves: usize,
    pub cases: usize,
    pub refused_leaves: BTreeSet<String>,
    /// leaf → (diverging cases, first reason)
    pub diverging: BTreeMap<String, (usize, String)>,
}

/// Runs every value.js leaf case through the engine root prints.
pub fn run() -> anyhow::Result<LeafReport> {
    let corpus: CorpusFile = gunzip_json("or7-corpus.json.gz")?;
    let answers: BTreeMap<String, Answer> = gunzip_json("or7-answers.json.gz")?;
    let cases: Vec<LeafCase> = corpus
        .cases
        .into_iter()
        .filter(|c| c.id.starts_with(VALUEJS))
        .collect();
    let mut by_leaf: BTreeMap<String, Vec<LeafCase>> = BTreeMap::new();
    for c in &cases {
        by_leaf
            .entry(leaf_of(&c.id).to_string())
            .or_default()
            .push(c.clone());
    }
    let mut rep = LeafReport {
        leaves: by_leaf.len(),
        cases: cases.len(),
        refused_leaves: BTreeSet::new(),
        diverging: BTreeMap::new(),
    };
    for (leaf, cs) in &by_leaf {
        let first = &cs[0];
        let mut pattern = String::new();
        if first.flags.contains('i') {
            pattern.push_str("(?i)");
        }
        if first.flags.contains('s') {
            pattern.push_str("(?s)");
        }
        pattern.push_str(&first.pattern);
        let dfa = parse_that_regex::parse(&pattern)
            .ok()
            .and_then(|_| parse_that_regex::Dfa::compile(&pattern));
        let Some(dfa) = dfa else {
            rep.refused_leaves.insert(leaf.clone());
            rep.diverging.insert(
                leaf.clone(),
                (cs.len(), "the engine refuses the leaf".into()),
            );
            continue;
        };
        for c in cs {
            let Some(a) = answers.get(&c.id) else {
                continue;
            };
            let want = if a.status == "MATCH" { a.end8 } else { None };
            let got = dfa.find_at(c.input.as_bytes(), 0);
            if got != want {
                let e = rep
                    .diverging
                    .entry(leaf.clone())
                    .or_insert((0, format!("case {}: oracle {want:?} engine {got:?}", c.id)));
                e.0 += 1;
            }
        }
    }
    Ok(rep)
}
