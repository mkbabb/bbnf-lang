//! RXEF-BENCH attribution probe (RULINGS 2026-11-15a). Evidence only: copied into
//! crates/core/examples/ with `probe.diff` applied to the Sheets builder, run, removed.
//! For each input line: parse with the generated GoogleSheetsParser, print the final
//! tree's compound kinds (counted), and the rule-entry counts (`begin_compound` calls
//! by rule name, including attempts later rolled back) that `probe.diff` records.
use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;
use bbnf::runtime::google_sheets::{SheetsArena, SheetsValue};
use std::collections::BTreeMap;

fn walk(a: &SheetsArena<'_>, v: &SheetsValue<'_>, tree: &mut BTreeMap<String, u64>) {
    if let SheetsValue::Compound(id) = v {
        let c = a.compound(*id);
        *tree.entry(format!("{:?}", c.kind)).or_default() += 1;
        for ch in c.children {
            walk(a, ch, tree);
        }
    }
}

fn sexpr(a: &SheetsArena<'_>, v: &SheetsValue<'_>) -> String {
    match v {
        SheetsValue::Compound(id) => {
            let c = a.compound(*id);
            let kids: Vec<String> = c.children.iter().map(|ch| sexpr(a, ch)).collect();
            format!("({:?} {})", c.kind, kids.join(" "))
        }
        other => format!("{other:?}"),
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mode = args[1].as_str(); // "file" <path> | "tree" <input>
    let mut tree = BTreeMap::new();
    let mut lines = 0u64;
    let mut failed = 0u64;
    let inputs: Vec<String> = match mode {
        "file" => std::fs::read_to_string(&args[2]).unwrap().lines().filter(|l| !l.trim().is_empty()).map(str::to_owned).collect(),
        _ => vec![args[2].clone()],
    };
    bbnf::runtime::google_sheets::builder::__probe_take_entries();
    for l in &inputs {
        lines += 1;
        match GoogleSheetsParser::parse(l) {
            Ok(doc) => {
                walk(doc.arena(), doc.root(), &mut tree);
                if mode == "tree" {
                    println!("tree {}", sexpr(doc.arena(), doc.root()));
                }
            }
            Err(e) => {
                failed += 1;
                println!("parse-error line={lines} {e:?}");
            }
        }
    }
    let entries = bbnf::runtime::google_sheets::builder::__probe_take_entries();
    println!("lines={lines} failed={failed}");
    for (k, n) in &tree {
        println!("tree\t{k}\t{n}");
    }
    for (k, n) in &entries {
        println!("entry\t{k}\t{n}");
    }
}
