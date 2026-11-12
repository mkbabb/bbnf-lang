//! `g1`: the G1 harness (sk-v27 LW3.COMP-3a; LW3-ORDER amendment 4).
//!
//! ```text
//! g1 [--arms root,ts-package] [--all] [--heldout grammar/heldout] [--grammar <id>,...]
//!    [--manifest grammar/MANIFEST] [--case <substring>,...]
//! g1 --list-oracles [--grammar <id>,...]
//! g1 corpus-gen r7rs --out <dir>          # write the R7RS held-out corpus
//! ```
//!
//! `--dump` prints each case's outcome as JSON instead of judging it.
//! `--case` narrows a reading to the cases whose id contains one of the
//! substrings (a diagnostic aid; a G1 reading never passes it).
//! `--all` reads the 9 grammars, `--heldout` the held-out 3. `--manifest`
//! prints the MANIFEST's digest the reading is taken at and, when the
//! `ts-package` arm is read, runs that arm over every MANIFEST grammar (a
//! grammar it compiles runs on its corpus, or on the empty input when it has
//! none).
//! ```text
//! ```
//!
//! Prints one line per grammar per arm (`oracles=N … divergences=D unruled=U
//! self-check=…`), then each unruled divergence indented below it. The value.js
//! leaf oracle prints its own line for each CSS grammar. Exit status: 0 when
//! every reading is clean, 1 when an unruled divergence exists, 2 when an arm
//! fails its self-check or the harness fails.

use std::collections::BTreeMap;
use std::process::ExitCode;

use bbnf_oracle::arm::{Arm, Case};
use bbnf_oracle::arms::{self, ts_package::TsPackage};
use bbnf_oracle::harness;
use bbnf_oracle::oracles::{self, leaves};
use bbnf_oracle::registry::{self, Grammar, HELDOUT, NINE, repo_root};

fn arg_values(args: &[String], flag: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut it = args.iter();
    while let Some(a) = it.next() {
        if a == flag
            && let Some(v) = it.next()
        {
            out.extend(v.split(',').map(str::to_owned));
        }
    }
    out
}

fn leaf_lines(rs: &[harness::Ruling], g: &str, rep: &leaves::LeafReport) -> (Vec<String>, usize) {
    let mut unruled = Vec::new();
    let mut ruled = 0;
    let mut cases = 0;
    for (leaf, (n, why)) in &rep.diverging {
        cases += n;
        match rs.iter().find(|r| {
            r.arm == "root" && r.grammar == g && r.oracle == "valuejs-leaves" && &r.case == leaf
        }) {
            Some(_) => ruled += 1,
            None => unruled.push(format!("    {leaf}: {n} case(s); {why}")),
        }
    }
    let n = unruled.len();
    let mut lines = vec![format!(
        "grammar={g} arm=root oracle=valuejs-leaves leaves={} cases={} refused-leaves={} diverging-leaves={} diverging-cases={cases} ruled-leaves={ruled} unruled={n}",
        rep.leaves,
        rep.cases,
        rep.refused_leaves.len(),
        rep.diverging.len(),
    )];
    lines.extend(unruled);
    (lines, n)
}

fn list_oracles(gs: &[Grammar]) -> anyhow::Result<()> {
    for g in gs {
        let cases = registry::cases(g)?;
        let mut names = Vec::new();
        for mut o in oracles::all() {
            if o.applies(g) && o.prepare(g, &cases)? {
                names.push(o.name());
            }
        }
        println!(
            "grammar={} oracles={} [{}] inputs={}",
            g.id,
            names.len(),
            names.join(","),
            cases.len()
        );
    }
    Ok(())
}

fn manifest() -> anyhow::Result<()> {
    let root = repo_root();
    let text = std::fs::read_to_string(root.join("grammar/MANIFEST"))?;
    let registered: BTreeMap<&str, Grammar> = registry::grammars()
        .into_iter()
        .map(|g| (g.path, g))
        .collect();
    let empty = bbnf_oracle::npm::cache_root().join("empty-input");
    std::fs::create_dir_all(empty.parent().unwrap())?;
    std::fs::write(&empty, "")?;
    let (mut compiled, mut refused) = (0, 0);
    for line in text.lines().filter(|l| !l.trim().is_empty()) {
        let Some((_, file)) = line.split_once(char::is_whitespace) else {
            continue;
        };
        let file: &'static str = Box::leak(file.trim().to_string().into_boxed_str());
        let (g, cases) = match registered.get(file) {
            Some(g) => (g.clone(), registry::cases(g)?),
            None => (
                Grammar {
                    id: file,
                    path: file,
                    entry: None,
                    heldout: false,
                    sources: &[],
                },
                vec![Case {
                    id: "empty-input".into(),
                    path: empty.clone(),
                }],
            ),
        };
        let cases = if cases.is_empty() {
            vec![Case {
                id: "empty-input".into(),
                path: empty.clone(),
            }]
        } else {
            cases
        };
        let outs = TsPackage::default().run(&g, &cases)?;
        let n = |v: &str| outs.iter().filter(|o| o.verdict() == v).count();
        if n("grammar-refused") == outs.len() {
            refused += 1;
        } else {
            compiled += 1;
        }
        println!(
            "manifest={file} arm=ts-package compiled={} inputs={} accept={} reject={}",
            if n("grammar-refused") == outs.len() {
                "no"
            } else {
                "yes"
            },
            cases.len(),
            n("accept"),
            n("reject")
        );
    }
    println!(
        "manifest-total arm=ts-package grammars={} compiled={compiled} refused={refused}",
        compiled + refused
    );
    Ok(())
}

fn run() -> anyhow::Result<u8> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.first().map(String::as_str) == Some("__root") {
        arms::root::child_main(args.get(1).map(String::as_str).unwrap_or(""))?;
        return Ok(0);
    }
    if args.first().map(String::as_str) == Some("corpus-gen") {
        let what = args.get(1).map(String::as_str).unwrap_or("");
        let out = arg_values(&args, "--out")
            .pop()
            .ok_or_else(|| anyhow::anyhow!("--out <dir>"))?;
        anyhow::ensure!(
            what == "r7rs",
            "corpus-gen: only r7rs is generated here (graphql: heldout/graphql-corpus.mjs)"
        );
        for rel in oracles::r7rs::write_corpus(std::path::Path::new(&out))? {
            println!("{rel}");
        }
        return Ok(0);
    }
    let named = arg_values(&args, "--grammar");
    let all = args.iter().any(|a| a == "--all");
    let heldout = args.iter().any(|a| a == "--heldout");
    let ids: Vec<&str> = if !named.is_empty() {
        NINE.iter()
            .chain(HELDOUT.iter())
            .copied()
            .filter(|id| named.iter().any(|n| n == id))
            .collect()
    } else {
        let mut v: Vec<&str> = if all { NINE.to_vec() } else { Vec::new() };
        if heldout {
            v.extend(HELDOUT);
        }
        v
    };
    let gs: Vec<Grammar> = ids
        .iter()
        .map(|id| registry::find(id).expect("registered"))
        .collect();
    let manifest_path = arg_values(&args, "--manifest").pop();
    if let Some(m) = &manifest_path {
        let digest = bbnf_oracle::npm::sha256_file(&repo_root().join(m))?;
        println!("manifest={m} sha256={digest}");
    }
    if args.iter().any(|a| a == "--list-oracles") {
        let gs: Vec<Grammar> = if gs.is_empty() {
            NINE.iter()
                .chain(HELDOUT.iter())
                .map(|id| registry::find(id).unwrap())
                .collect()
        } else {
            gs
        };
        list_oracles(&gs)?;
        return Ok(0);
    }
    let mut arm_names = arg_values(&args, "--arms");
    if arm_names.is_empty() {
        arm_names = vec!["root".into(), "ts-package".into()];
    }
    let only = arg_values(&args, "--case");
    let rs = harness::rulings()?;
    let mut status = 0u8;
    let mut leaf_report: Option<leaves::LeafReport> = None;
    for g in &gs {
        let mut cases = registry::cases(g)?;
        if !only.is_empty() {
            cases.retain(|c| only.iter().any(|o| c.id.contains(o.as_str())));
        }
        for name in &arm_names {
            let mut arm: Box<dyn Arm> =
                arms::by_name(name).ok_or_else(|| anyhow::anyhow!("no arm {name}"))?;
            if !arm.supports(g) {
                println!("grammar={} arm={name} unsupported", g.id);
                continue;
            }
            if args.iter().any(|a| a == "--dump") {
                for (c, o) in cases.iter().zip(arm.run(g, &cases)?) {
                    println!(
                        "dump grammar={} arm={name} case={} {}",
                        g.id,
                        c.id,
                        serde_json::to_string(&o)?
                    );
                }
                continue;
            }
            let mut os = oracles::all();
            let rep = harness::read(arm.as_mut(), g, &cases, &mut os)?;
            println!("{}", rep.line());
            for (o, t) in &rep.oracles {
                println!(
                    "  oracle={o} judged={} agree={} diverge={} ruled={}",
                    t.judged, t.agree, t.diverge, t.ruled
                );
            }
            for u in &rep.unruled {
                println!("    {u}");
            }
            if rep.self_check.is_err() {
                status = 2;
            } else if !rep.unruled.is_empty() && status == 0 {
                status = 1;
            }
        }
        if matches!(g.id, "css_l4" | "css_pretty") && arm_names.iter().any(|a| a == "root") {
            let rep = match &leaf_report {
                Some(r) => r,
                None => leaf_report.insert(leaves::run()?),
            };
            let (lines, unruled) = leaf_lines(&rs, g.id, rep);
            for l in lines {
                println!("{l}");
            }
            if unruled > 0 && status == 0 {
                status = 1;
            }
        }
    }
    if manifest_path.is_some() && arm_names.iter().any(|a| a == "ts-package") {
        manifest()?;
    }
    Ok(status)
}

fn main() -> ExitCode {
    // Observation trees nest as deep as the inputs (JSONTestSuite reaches
    // 100000 levels): comparing, hashing and dropping them recurses, so the
    // reading runs on a 1 GiB stack.
    let reading = std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(run)
        .expect("spawn the reading thread");
    let result = reading
        .join()
        .unwrap_or_else(|_| Err(anyhow::anyhow!("the reading thread panicked")));
    match result {
        Ok(s) => ExitCode::from(s),
        Err(e) => {
            eprintln!("g1: {e:#}");
            ExitCode::from(2)
        }
    }
}
