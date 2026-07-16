//! LOCK-8 · pass-3 · 8C · PROTO-2 — three-way hazard-triple differential.
//!
//! Column 1 (JS `RegExp`) is supplied out-of-band by `js-oracle.mjs`; this
//! binary produces columns 2 (current leftmost-longest `Dfa`) and 3 (Route-A
//! leftmost-first `Pdfa`), plus the R1 state-count comparison over the real
//! consumer corpus and the R2 Hopcroft on/off parity check.

use bbnf_regex::automata::dfa::Dfa;
use bbnf_regex::automata::pfa::{MatchKind, Pdfa};

fn longest(pat: &str, input: &str) -> String {
    match Dfa::compile(pat) {
        None => "REJECTED".into(),
        Some(d) => match d.find_at(input.as_bytes(), 0) {
            None => "no-match".into(),
            Some(end) => format!("{:?}", &input[..end]),
        },
    }
}

fn priority(pat: &str, input: &str, minimize: bool) -> String {
    let nfa = match bbnf_regex::automata::nfa::Nfa::from_pattern(pat) {
        None => return "REJECTED".into(),
        Some(n) => n,
    };
    match Pdfa::from_nfa(&nfa, MatchKind::LeftmostFirst, minimize) {
        None => "REJECTED".into(),
        Some(p) => match p.find_at(input.as_bytes(), 0) {
            None => "no-match".into(),
            Some(end) => format!("{:?}", &input[..end]),
        },
    }
}

fn main() {
    // (hazard, pattern, input) — JS expected computed by js-oracle.mjs
    let triples = [
        ("H1 ordered-alt shorter-first", "a|ab", "ab"),
        ("H2 greedy-extent", r"\d+", "123"),
        ("H3 lazy", r"\d+?", "123"),
        ("H4 backref", r"(a)\1", "aa"),
        ("H5 lookaround", r"a(?=b)", "ab"),
    ];

    println!("=== HAZARD TRIPLES: col2 longest-DFA | col3 priority-DFA (leftmost-first) ===");
    println!(
        "{:<32} {:<10} {:<8} {:<12} {:<12} {:<12}",
        "hazard", "pattern", "input", "longestDFA", "priorityDFA", "prio(no-min)"
    );
    for (name, pat, input) in triples {
        println!(
            "{:<32} {:<10} {:<8} {:<12} {:<12} {:<12}",
            name,
            pat,
            format!("{:?}", input),
            longest(pat, input),
            priority(pat, input, true),
            priority(pat, input, false),
        );
    }

    // ── R1: state-count blowup over the REAL consumer corpus + hazards ──
    // Patterns harvested by the P4 census that the engine can actually compile
    // (greedy / disjoint-prefix / anchor-free). H4/H5 are excluded — they do
    // not compile in either engine.
    let corpus = [
        // parse-that first-party leaves
        (r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", "json-number"),
        (r#""(?:[^"\\]|\\(?:["\\/bfnrt]|u[0-9a-fA-F]{4}))*""#, "json-string"),
        (r"\s*", "ws-star"),
        (r"[^,]+", "csv-field"),
        // value.js @6abef80 src/parsing
        (r"[a-zA-Z][a-zA-Z0-9-]*", "ident"),
        (r"#[0-9a-fA-F]{3,8}", "hex-color"),
        (r"--[a-zA-Z_][a-zA-Z0-9_-]*", "custom-prop"),
        (r"\s*,\s*|\s+", "sep"),
        (r"-?(?:\d+\.?\d*|\.\d+)(?:%|[a-zA-Z]+)?", "css-number"),
        (r"-?\d+", "integer"),
        // latex src/grammar (lookaround-free subset)
        (r"%[^\n]*\n?", "latex-comment"),
        (r"\\[vh]space\*?", "vspace"),
        (r"[a-zA-Z@]+\*?", "cmd-name"),
        // hazards that DO compile
        ("a|ab", "H1"),
        (r"\d+", "H2"),
        (r"\d+?", "H3"),
    ];

    println!("\n=== R1 STATE-COUNT: longest vs priority (both minimized) ===");
    println!(
        "{:<14} {:<44} {:>8} {:>8} {:>7}",
        "name", "pattern", "longest", "priority", "ratio"
    );
    let mut worst = 0.0f64;
    let mut r2_fail = 0;
    for (pat, name) in corpus {
        let l = Dfa::compile(pat).map(|d| d.state_count());
        let nfa = bbnf_regex::automata::nfa::Nfa::from_pattern(pat);
        let p = nfa
            .as_ref()
            .and_then(|n| Pdfa::from_nfa(n, MatchKind::LeftmostFirst, true))
            .map(|p| p.state_count());
        let (ls, ps, ratio) = match (l, p) {
            (Some(l), Some(p)) => {
                let r = p as f64 / l as f64;
                if r > worst {
                    worst = r;
                }
                (l.to_string(), p.to_string(), format!("{:.2}x", r))
            }
            _ => ("-".into(), "-".into(), "-".into()),
        };
        // R2: minimize-on vs minimize-off must give the SAME match on a probe.
        if let Some(n) = &nfa {
            let probe = "0"; // benign probe byte present in most classes
            let a = Pdfa::from_nfa(n, MatchKind::LeftmostFirst, true)
                .and_then(|p| p.find_at(probe.as_bytes(), 0));
            let b = Pdfa::from_nfa(n, MatchKind::LeftmostFirst, false)
                .and_then(|p| p.find_at(probe.as_bytes(), 0));
            if a != b {
                r2_fail += 1;
            }
        }
        println!("{:<14} {:<44} {:>8} {:>8} {:>7}", name, pat, ls, ps, ratio);
    }
    println!("\nR1 worst priority/longest state ratio = {:.2}x", worst);
    println!("R2 minimize on/off match divergences   = {}", r2_fail);
}
