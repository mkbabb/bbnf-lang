//! The one harness: grammar × input × arm → `obs()` plus the failure
//! contract, judged by every independent oracle that has authority over the
//! grammar (LW3.COMP-3a).
//!
//! Before an arm's results count, the harness checks the arm itself: an arm
//! whose outcomes do not depend on its input is refused. Concretely, over a
//! grammar's cases an arm must (a) report at least two distinct outcomes
//! when at least two inputs differ, and (b) not give one verdict to every case
//! when the oracles expect both accepts and rejects among them.
//!
//! A divergence is ruled only by a row of `crates/oracle/ruled.tsv` or of
//! `SK/grammar/DIVERGENCE.tsv` (for the `bbnf` grammar against the TS front
//! end); every other divergence is unruled. A row's arm `*` rules the case
//! for every arm. A row names the unit or ruling that owns the divergence and
//! the authority it rests on; a row never makes a divergence agree.

use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};

use crate::arm::{Arm, Case, Outcome};
use crate::oracles::{Judgement, Oracle};
use crate::registry::{Grammar, repo_root};

#[derive(Clone, Debug)]
pub struct Ruling {
    pub arm: String,
    pub grammar: String,
    pub oracle: String,
    /// A case id (or, for the leaf oracle, a leaf id).
    pub case: String,
    pub owner: String,
    pub authority: String,
}

/// Every ruled divergence of record.
pub fn rulings() -> anyhow::Result<Vec<Ruling>> {
    let mut out = Vec::new();
    let text = std::fs::read_to_string(repo_root().join("crates/oracle/ruled.tsv"))?;
    for line in text
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .skip(1)
    {
        let f: Vec<&str> = line.split('\t').collect();
        anyhow::ensure!(f.len() == 6, "ruled.tsv: 6 columns expected: {line}");
        out.push(Ruling {
            arm: f[0].into(),
            grammar: f[1].into(),
            oracle: f[2].into(),
            case: f[3].into(),
            owner: f[4].into(),
            authority: f[5].into(),
        });
    }
    // The TS<->Rust front-end divergences ruled under R-b (LW2.PAR-1): each
    // row is the TS front end's own reading of a grammar file, so it rules
    // that file for every arm the TS front end judges (`*`).
    let div = std::fs::read_to_string(
        repo_root().join("restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv"),
    )?;
    let mut rows = div.lines().filter(|l| !l.is_empty() && !l.starts_with('#'));
    let head: Vec<&str> = rows.next().unwrap_or("").split('\t').collect();
    let col = |n: &str| head.iter().position(|h| *h == n);
    let (Some(id), Some(files), Some(fate)) = (col("id"), col("files"), col("fate")) else {
        anyhow::bail!("DIVERGENCE.tsv: header lacks id/files/fate");
    };
    for r in rows {
        let f: Vec<&str> = r.split('\t').collect();
        for file in f[files].split(',') {
            out.push(Ruling {
                arm: "*".into(),
                grammar: "bbnf".into(),
                oracle: "ts-frontend".into(),
                case: format!("in-tree:{file}"),
                owner: f[fate].into(),
                authority: format!("SK/grammar/DIVERGENCE.tsv {}", f[id]),
            });
        }
    }
    Ok(out)
}

fn ruled(rs: &[Ruling], arm: &str, g: &str, oracle: &str, case: &str) -> Option<String> {
    rs.iter()
        .find(|r| {
            (r.arm == arm || r.arm == "*") && r.grammar == g && r.oracle == oracle && r.case == case
        })
        .map(|r| format!("{} ({})", r.owner, r.authority))
}

#[derive(Debug, Default)]
pub struct OracleTally {
    pub judged: usize,
    pub agree: usize,
    pub diverge: usize,
    pub ruled: usize,
}

#[derive(Debug)]
pub struct Report {
    pub grammar: String,
    pub arm: String,
    pub inputs: usize,
    pub verdicts: BTreeMap<&'static str, usize>,
    pub oracles: BTreeMap<String, OracleTally>,
    pub divergences: usize,
    pub unruled: Vec<String>,
    pub self_check: Result<(), String>,
}

fn fingerprint(o: &Outcome) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    match o {
        Outcome::Accept { obs, value } => {
            0u8.hash(&mut h);
            obs.hash(&mut h);
            value.as_ref().map(|v| v.to_string()).hash(&mut h);
        }
        Outcome::Reject(c) => {
            1u8.hash(&mut h);
            c.hash(&mut h);
        }
        Outcome::GrammarRefused(m) => {
            2u8.hash(&mut h);
            m.hash(&mut h);
        }
        Outcome::Crash(m) => {
            3u8.hash(&mut h);
            m.hash(&mut h);
        }
    }
    h.finish()
}

/// The arm self-check (see the module doc).
pub fn self_check(
    cases: &[Case],
    outs: &[Outcome],
    expected: &[Option<bool>],
) -> Result<(), String> {
    if outs.iter().all(|o| matches!(o, Outcome::GrammarRefused(_))) {
        return Ok(());
    }
    let distinct_inputs: BTreeSet<Vec<u8>> = cases.iter().take(256).map(Case::bytes).collect();
    let distinct_outs: BTreeSet<u64> = outs.iter().map(fingerprint).collect();
    if distinct_inputs.len() >= 2 && distinct_outs.len() < 2 {
        return Err(format!(
            "{} distinct inputs, one outcome: the arm returns a constant",
            cases.len()
        ));
    }
    let want_both = expected.contains(&Some(true)) && expected.contains(&Some(false));
    let verdicts: BTreeSet<Option<bool>> = outs.iter().map(Outcome::accepted).collect();
    if want_both && verdicts.len() < 2 {
        return Err(
            "the oracles expect accepts and rejects; the arm gives one verdict to every case"
                .into(),
        );
    }
    Ok(())
}

/// Runs one arm over one grammar and judges it.
pub fn read(
    arm: &mut dyn Arm,
    g: &Grammar,
    cases: &[Case],
    oracles: &mut [Box<dyn Oracle>],
) -> anyhow::Result<Report> {
    let rs = rulings()?;
    let outs = arm.run(g, cases)?;
    anyhow::ensure!(
        outs.len() == cases.len(),
        "{} returned {} outcomes for {} cases",
        arm.name(),
        outs.len(),
        cases.len()
    );
    let mut rep = Report {
        grammar: g.id.into(),
        arm: arm.name().into(),
        inputs: cases.len(),
        verdicts: BTreeMap::new(),
        oracles: BTreeMap::new(),
        divergences: 0,
        unruled: Vec::new(),
        self_check: Ok(()),
    };
    for o in &outs {
        *rep.verdicts.entry(o.verdict()).or_default() += 1;
    }
    let mut expected: Vec<Option<bool>> = vec![None; cases.len()];
    for oracle in oracles.iter_mut() {
        if !oracle.applies(g) || oracle.name() == arm.name() {
            continue;
        }
        if !oracle.prepare(g, cases)? {
            continue;
        }
        let tally = rep.oracles.entry(oracle.name().to_string()).or_default();
        for (i, (c, o)) in cases.iter().zip(&outs).enumerate() {
            if expected[i].is_none() {
                expected[i] = oracle.expected_verdict(g, i, c);
            }
            match oracle.judge(g, i, c, o) {
                Judgement::NotApplicable => {}
                Judgement::Agree => {
                    tally.judged += 1;
                    tally.agree += 1;
                }
                Judgement::Diverge(why) => {
                    tally.judged += 1;
                    tally.diverge += 1;
                    rep.divergences += 1;
                    match ruled(&rs, arm.name(), g.id, oracle.name(), &c.id) {
                        Some(_) => tally.ruled += 1,
                        None => rep
                            .unruled
                            .push(format!("{} [{}]: {why}", c.id, oracle.name())),
                    }
                }
            }
        }
    }
    rep.self_check = self_check(cases, &outs, &expected);
    Ok(rep)
}

impl Report {
    pub fn line(&self) -> String {
        let v = |k: &str| self.verdicts.get(k).copied().unwrap_or(0);
        format!(
            "grammar={} arm={} oracles={} inputs={} accept={} reject={} refused={} crash={} divergences={} unruled={} self-check={}",
            self.grammar,
            self.arm,
            self.oracles.len(),
            self.inputs,
            v("accept"),
            v("reject"),
            v("grammar-refused"),
            v("crash"),
            self.divergences,
            self.unruled.len(),
            match &self.self_check {
                Ok(()) => "ok".to_string(),
                Err(e) => format!("REFUSED ({e})"),
            }
        )
    }
}
