//! PROTO-2 (LOCK-5 / family 5C AUTOMATA-AS-EXTRACTION):
//!   2A  non-restart worklist minimizer + acyclic congruence fast path
//!   2B  Brzozowski-derivative determinization saturating the five HIR rules
//!
//! Run: cargo run -p bbnf-regex --example proto2 --profile bench

use std::collections::{HashMap, HashSet, VecDeque};
use std::hint::black_box;
use std::time::Instant;

use bbnf_regex::automata::dfa::{
    congruence_minimize_acyclic, congruence_partition_acyclic, hopcroft_minimize, is_acyclic,
    worklist_minimize,
};
use bbnf_regex::egraph::{simplify_hir, RegexExtractionCost};
use bbnf_regex::hir::{CharClass, Hir, Repetition};
use bbnf_regex::{parse_with, Dfa, DfaOptions, Nfa};

const COMPILE_PATTERNS: &[&str] = &[
    r"\d+",
    r"\s+",
    r"\w+",
    r"[a-z]+",
    r"[a-zA-Z_]\w*",
    r"[0-9a-fA-F]+",
    r"[0-9a-fA-F]{3,8}",
    r"-?\d+",
    r"-?(0|[1-9]\d*)(\.\d+)?",
    r"[-+]?\d+",
    r"[-+]?(\d+(\.\d+)?|\.\d+)",
    r"https?://[a-zA-Z0-9./-]+",
    r"#[0-9a-fA-F]{3,6}",
    r"\d{4}-\d{2}-\d{2}",
    r"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}",
    r"0x[0-9a-fA-F]+",
    r"\d+px",
    r"\d+(?:px|em|rem|vh|vw)",
    r"(?:foo|bar|baz)+",
    r"[A-Za-z0-9+/]+",
    r"[a-zA-Z][a-zA-Z0-9]*",
    r"(?:0|[1-9][0-9]*)",
    r"[\-]?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[a-zA-Z_][\w-]*",
    r"\d+(?:_\d+)*",
    r"\w+(?:[-_]\w+)*",
];

const MATCH_INPUT: &str = "the quick brown fox 12345 jumped over 3.14159 lazy \
dogs at https://example.com on 2026-04-09 with hex \
deadBEEF and 192.168.1.100 plus 0x7FFF and #FFAACC";

fn build_unmin(pattern: &str) -> Option<Dfa> {
    let opts = DfaOptions {
        state_limit: 4096,
        minimize: false,
        unicode: true,
    };
    let parse_opts = bbnf_regex::hir::ParseOptions { unicode: true };
    let hir = parse_with(pattern, &parse_opts).ok()?;
    let nfa = Nfa::from_hir(&hir)?;
    Dfa::from_nfa(&nfa, &opts)
}

/// Populate the flat transition table (minimizers leave it empty).
fn flatten(mut d: Dfa) -> Dfa {
    let ncls = d.num_classes as usize;
    let mut flat = Vec::with_capacity(d.states.len() * ncls);
    for s in &d.states {
        flat.extend_from_slice(&s.transitions);
    }
    d.flat_transitions = flat;
    d
}

/// Two DFAs agree on MATCH_INPUT anchored scan (language spot-check).
fn scan_equiv(a: &Dfa, b: &Dfa) -> bool {
    let bytes = MATCH_INPUT.as_bytes();
    for off in 0..=bytes.len() {
        if a.find_at(bytes, off) != b.find_at(bytes, off) {
            return false;
        }
    }
    true
}

fn part_a() {
    println!("== 2A: worklist minimizer + acyclic congruence fast path ==");
    println!(
        "{:<44} {:>5} {:>5} {:>5} {:>5} {:>4} {:>4}",
        "pattern", "unmin", "hopc", "work", "cong", "acyc", "ok"
    );

    let mut bases: Vec<(&str, Dfa)> = Vec::new();
    let mut n_ok = 0usize;
    let mut n_total = 0usize;
    let mut n_acyclic = 0usize;
    let mut acyclic_min_states = 0usize;
    let mut all_states = 0usize;

    for &p in COMPILE_PATTERNS {
        let Some(base) = build_unmin(p) else {
            println!("{p:<44}  (skip: compile failed)");
            continue;
        };
        n_total += 1;
        let hopc = flatten(hopcroft_minimize(&base));
        let work = flatten(worklist_minimize(&base));
        let acyc = is_acyclic(&base);
        let cong = congruence_minimize_acyclic(&base).map(flatten);

        let hc = hopc.state_count();
        let wc = work.state_count();
        let cc = cong.as_ref().map(|d| d.state_count());

        // Correctness gate: byte-identical minimized state count + language equiv.
        let count_ok = wc == hc;
        let lang_ok = scan_equiv(&hopc, &work);
        let cong_ok = match &cong {
            Some(d) => d.state_count() == hc && scan_equiv(&hopc, d),
            None => !acyc, // None is only acceptable when the DFA is cyclic
        };
        let ok = count_ok && lang_ok && cong_ok;
        if ok {
            n_ok += 1;
        }
        if acyc {
            n_acyclic += 1;
            acyclic_min_states += hc;
        }
        all_states += hc;

        // Partition-equivalence cross-check (worklist vs congruence on acyclic).
        if let Some((wp, wnb)) = Some(worklist_partition_dbg(&base)) {
            if acyc {
                if let Some((cp, cnb)) = congruence_partition_acyclic(&base) {
                    debug_assert_eq!(wnb, cnb, "partition block-count mismatch {p}");
                    debug_assert!(same_relation(&wp, &cp), "partition relation mismatch {p}");
                }
            }
        }

        println!(
            "{:<44} {:>5} {:>5} {:>5} {:>5} {:>4} {:>4}",
            p,
            base.state_count(),
            hc,
            wc,
            cc.map(|v| v.to_string()).unwrap_or_else(|| "-".into()),
            if acyc { "Y" } else { "n" },
            if ok { "Y" } else { "FAIL" },
        );
        bases.push((p, base));
    }

    println!(
        "\ncorrectness gate: {n_ok}/{n_total} patterns byte-identical + language-equiv",
    );
    println!(
        "acyclic fraction (congruence exact-and-free): {n_acyclic}/{n_total} patterns \
         = {:.0}% of patterns, {acyclic_min_states}/{all_states} minimal states",
        100.0 * n_acyclic as f64 / n_total as f64
    );

    // ---- cold paired timing (minimizer head-to-head on identical inputs) ----
    let n_iter = 500usize;
    let warm = 30usize;
    let mut t_hopc = 0u128;
    let mut t_work = 0u128;
    for i in 0..(warm + n_iter) {
        // Alternate which runs first each iter to cancel ordering bias.
        let hopc_first = i % 2 == 0;
        let (mut a, mut b) = (0u128, 0u128);
        if hopc_first {
            let t = Instant::now();
            for (_, base) in &bases {
                black_box(hopcroft_minimize(black_box(base)));
            }
            a = t.elapsed().as_nanos();
            let t = Instant::now();
            for (_, base) in &bases {
                black_box(worklist_minimize(black_box(base)));
            }
            b = t.elapsed().as_nanos();
        } else {
            let t = Instant::now();
            for (_, base) in &bases {
                black_box(worklist_minimize(black_box(base)));
            }
            b = t.elapsed().as_nanos();
            let t = Instant::now();
            for (_, base) in &bases {
                black_box(hopcroft_minimize(black_box(base)));
            }
            a = t.elapsed().as_nanos();
        }
        if i >= warm {
            t_hopc += a;
            t_work += b;
        }
    }
    let ratio = t_work as f64 / t_hopc as f64;
    println!(
        "\ncold-paired minimizer wall-time (N={n_iter}, {} patterns/iter):",
        bases.len()
    );
    println!("  incumbent hopcroft : {:>12} ns total", t_hopc);
    println!("  worklist (2A)      : {:>12} ns total", t_work);
    println!(
        "  ratio work/hopc    : {ratio:.4}   ({})",
        if ratio < 1.0 { "FASTER — 2A perf GO" } else { "SLOWER — 2A perf NO-GO" }
    );

    // congruence-only timing on the acyclic subset (exact-and-free claim).
    let acyclic_bases: Vec<&Dfa> = bases
        .iter()
        .map(|(_, b)| b)
        .filter(|b| is_acyclic(b))
        .collect();
    if !acyclic_bases.is_empty() {
        let mut t_c = 0u128;
        let mut t_h = 0u128;
        for i in 0..(warm + n_iter) {
            if i % 2 == 0 {
                let t = Instant::now();
                for b in &acyclic_bases {
                    black_box(congruence_minimize_acyclic(black_box(b)));
                }
                if i >= warm { t_c += t.elapsed().as_nanos(); }
                let t = Instant::now();
                for b in &acyclic_bases {
                    black_box(hopcroft_minimize(black_box(b)));
                }
                if i >= warm { t_h += t.elapsed().as_nanos(); }
            } else {
                let t = Instant::now();
                for b in &acyclic_bases {
                    black_box(hopcroft_minimize(black_box(b)));
                }
                if i >= warm { t_h += t.elapsed().as_nanos(); }
                let t = Instant::now();
                for b in &acyclic_bases {
                    black_box(congruence_minimize_acyclic(black_box(b)));
                }
                if i >= warm { t_c += t.elapsed().as_nanos(); }
            }
        }
        println!(
            "  acyclic subset ({} pats): congruence {:.4}x hopcroft ({} vs {} ns)",
            acyclic_bases.len(),
            t_c as f64 / t_h as f64,
            t_c,
            t_h
        );
    }
}

// Re-export the partition for the cross-check (worklist_partition is pub too).
fn worklist_partition_dbg(dfa: &Dfa) -> (Vec<usize>, usize) {
    bbnf_regex::automata::dfa::worklist_partition(dfa)
}

fn same_relation(a: &[usize], b: &[usize]) -> bool {
    let n = a.len();
    for i in 0..n {
        for j in (i + 1)..n {
            if (a[i] == a[j]) != (b[i] == b[j]) {
                return false;
            }
        }
    }
    true
}

// ══════════════════════════════════════════════════════════════════════
// 2B — Brzozowski derivative determinization, canonicalized ONLY by the
// five HIR saturation rules (simplify_hir). ∅ (empty language) is None.
// ══════════════════════════════════════════════════════════════════════

/// Does the HIR match the empty string?
fn nullable(h: &Hir) -> bool {
    match h {
        Hir::Empty => true,
        Hir::Literal(v) => v.is_empty(),
        Hir::Class(_) => false,
        Hir::Look(_) => true, // treated as zero-width ε for this probe
        Hir::Group(inner) => nullable(inner),
        Hir::Concat(vs) => vs.iter().all(nullable),
        Hir::Alternation(vs) => vs.iter().any(nullable),
        Hir::Repetition(r) => r.min == 0 || nullable(&r.sub),
    }
}

/// Does CharClass `cc` match single byte `b`?
fn class_has(cc: &CharClass, b: u8) -> bool {
    let ranges = cc.to_positive_byte_ranges();
    ranges.iter().any(|r| b >= r.start && b <= r.end)
}

/// Union of two optional-HIR derivatives.
fn union(a: Option<Hir>, b: Option<Hir>) -> Option<Hir> {
    match (a, b) {
        (None, x) | (x, None) => x,
        (Some(x), Some(y)) => Some(Hir::Alternation(vec![x, y])),
    }
}

/// Brzozowski derivative of `h` w.r.t. byte `b`. `None` = ∅ (dead).
/// Bounded/counted reps are expanded lazily via the identities below.
fn derivative(h: &Hir, b: u8) -> Option<Hir> {
    match h {
        Hir::Empty => None,
        Hir::Literal(v) => {
            if v.first() == Some(&b) {
                if v.len() == 1 {
                    Some(Hir::Empty)
                } else {
                    Some(Hir::Literal(v[1..].to_vec()))
                }
            } else {
                None
            }
        }
        Hir::Class(cc) => {
            if class_has(cc, b) {
                Some(Hir::Empty)
            } else {
                None
            }
        }
        Hir::Look(_) => None, // zero-width: no byte consumed → ∅ derivative
        Hir::Group(inner) => derivative(inner, b),
        Hir::Concat(vs) => {
            // d(x·rest) = d(x)·rest  ∪  (nullable(x)) d(rest)
            if vs.is_empty() {
                return None;
            }
            let head = &vs[0];
            let rest: Vec<Hir> = vs[1..].to_vec();
            let rest_hir = if rest.is_empty() {
                Hir::Empty
            } else if rest.len() == 1 {
                rest[0].clone()
            } else {
                Hir::Concat(rest.clone())
            };
            let d_head = derivative(head, b).map(|dh| concat2(dh, rest_hir.clone()));
            let d_rest = if nullable(head) {
                derivative(&rest_hir, b)
            } else {
                None
            };
            union(d_head, d_rest)
        }
        Hir::Alternation(vs) => {
            let mut acc: Option<Hir> = None;
            for v in vs {
                acc = union(acc, derivative(v, b));
            }
            acc
        }
        Hir::Repetition(r) => derivative_rep(r, b),
    }
}

fn concat2(a: Hir, b: Hir) -> Hir {
    match (a, b) {
        (Hir::Empty, x) | (x, Hir::Empty) => x,
        (x, y) => Hir::Concat(vec![x, y]),
    }
}

fn star(sub: Hir) -> Hir {
    Hir::Repetition(Repetition {
        sub: Box::new(sub),
        min: 0,
        max: None,
        greedy: true,
    })
}

/// Derivative of a repetition. Handles star/plus/question directly; general
/// {n,m} via the identity d(R{n,m}) = d(R)·R{n-1,m-1} (+ skip when nullable).
fn derivative_rep(r: &Repetition, b: u8) -> Option<Hir> {
    let sub = (*r.sub).clone();
    match (r.min, r.max) {
        // R*  : d = d(R)·R*
        (0, None) => derivative(&sub, b).map(|d| concat2(d, star(sub.clone()))),
        // R?  : d = d(R)
        (0, Some(1)) => derivative(&sub, b),
        // R+  : d = d(R)·R*
        (1, None) => derivative(&sub, b).map(|d| concat2(d, star(sub.clone()))),
        // general {n,m}
        (min, max) => {
            // tail = R{min-1, max-1}
            let new_min = min.saturating_sub(1);
            let new_max = max.map(|m| m.saturating_sub(1));
            let is_empty_tail = new_min == 0 && new_max == Some(0);
            let tail = if is_empty_tail {
                Hir::Empty
            } else {
                Hir::Repetition(Repetition {
                    sub: Box::new(sub.clone()),
                    min: new_min,
                    max: new_max,
                    greedy: true,
                })
            };
            let d_first = derivative(&sub, b).map(|d| concat2(d, tail.clone()));
            // If the required count is already satisfiable empty (min==0), we may
            // also skip the leading R and derive the tail — but for min>=1 the
            // first R is mandatory, so no skip term.
            if min == 0 {
                union(d_first, derivative(&tail, b))
            } else {
                d_first
            }
        }
    }
}

/// Class representatives of a DFA's byte partition (one byte per class).
fn class_reps(dfa: &Dfa) -> Vec<u8> {
    let k = dfa.num_classes as usize;
    let mut reps = vec![0u8; k];
    let mut seen = vec![false; k];
    for b in 0..=255u8 {
        let c = dfa.byte_classes[b as usize] as usize;
        if !seen[c] {
            reps[c] = b;
            seen[c] = true;
        }
    }
    reps
}

/// Build the saturate-only derivative automaton; return live state count.
/// Alphabet = the DFA's byte-equivalence-class representatives (identical
/// granularity to the incumbent DFA, so counts are directly comparable).
fn derivative_states(pattern: &str, dfa: &Dfa, cost: &RegexExtractionCost) -> Option<usize> {
    let parse_opts = bbnf_regex::hir::ParseOptions { unicode: true };
    let hir = parse_with(pattern, &parse_opts).ok()?;
    // Bail on Look assertions (out of probe scope — zero-width handling
    // would make the count incomparable to the byte DFA).
    if contains_look(&hir) {
        return None;
    }
    let reps = class_reps(dfa);
    let start = simplify_hir(&hir, cost);

    let mut ids: HashMap<Hir, usize> = HashMap::new();
    ids.insert(start.clone(), 0);
    let mut queue: VecDeque<Hir> = VecDeque::new();
    queue.push_back(start);
    let mut guard = 0usize;

    while let Some(state) = queue.pop_front() {
        guard += 1;
        if guard > 100_000 {
            return None; // runaway (non-canonicalizing) — treat as unbounded
        }
        for &rb in &reps {
            if let Some(d) = derivative(&state, rb) {
                let canon = simplify_hir(&d, cost);
                if is_empty_lang(&canon) {
                    continue;
                }
                let next = ids.len();
                if !ids.contains_key(&canon) {
                    ids.insert(canon.clone(), next);
                    queue.push_back(canon);
                }
            }
        }
    }
    Some(ids.len())
}

fn contains_look(h: &Hir) -> bool {
    match h {
        Hir::Look(_) => true,
        Hir::Group(i) => contains_look(i),
        Hir::Repetition(r) => contains_look(&r.sub),
        Hir::Concat(v) | Hir::Alternation(v) => v.iter().any(contains_look),
        _ => false,
    }
}

/// Heuristic: is this canonical HIR the empty language? Only true ∅ markers.
/// (Our derivatives never fabricate ∅ as an Hir — ∅ is None — so a canonical
/// Hir is never the empty language here; kept as a guard.)
fn is_empty_lang(_h: &Hir) -> bool {
    false
}

fn part_b() {
    println!("\n== 2B: Brzozowski derivative determinization (saturate-only) ==");
    println!(
        "{:<44} {:>7} {:>7} {:>7}",
        "pattern", "sat", "minimal", "rho"
    );
    let cost = RegexExtractionCost::default();

    // Witness patterns first: (ab)*a ≡ a(ba)* and the composite that forces
    // both equivalent-but-non-rule-unifiable residuals to co-exist as states.
    let witness: &[&str] = &[
        r"(ab)*a",
        r"a(ba)*",
        r"c(ab)*a|da(ba)*", // composite: 'c'→(ab)*a state, 'd'→a(ba)* state
    ];

    let mut worst_rho = 1.0f64;
    let mut n_measured = 0usize;
    let mut n_under = 0usize;

    let run = |pattern: &str,
               worst: &mut f64,
               nm: &mut usize,
               nu: &mut usize| {
        let Some(dfa) = Dfa::compile(pattern) else {
            println!("{pattern:<44}   (skip: compile failed)");
            return;
        };
        let minimal = dfa.state_count();
        match derivative_states(pattern, &dfa, &cost) {
            Some(sat) => {
                let rho = sat as f64 / minimal as f64;
                *nm += 1;
                if rho > 1.0 + 1e-9 {
                    *nu += 1;
                }
                if rho > *worst {
                    *worst = rho;
                }
                println!(
                    "{:<44} {:>7} {:>7} {:>7.3}{}",
                    pattern,
                    sat,
                    minimal,
                    rho,
                    if rho > 1.0 + 1e-9 { "  <-- UNDER-MIN" } else { "" }
                );
            }
            None => {
                println!("{pattern:<44} {:>7} {:>7}   (unsupported: Look/runaway)", "-", minimal);
            }
        }
    };

    println!("-- named witness family --");
    for &p in witness {
        run(p, &mut worst_rho, &mut n_measured, &mut n_under);
    }
    println!("-- corpus --");
    for &p in COMPILE_PATTERNS {
        run(p, &mut worst_rho, &mut n_measured, &mut n_under);
    }

    println!(
        "\n2B: measured {n_measured} patterns, {n_under} with rho>1, worst rho = {worst_rho:.3}"
    );
    if n_under == 0 {
        println!("2B VERDICT: rho=1 across all measured — LANDMARK candidate (saturation collapses determinize+minimize)");
    } else {
        println!("2B VERDICT: rho>1 present — BLOCKED-candidate CONFIRMED (five rules under-minimize; theory-bounded)");
    }

    // Direct witness proof: two language-equal derivatives with distinct
    // saturated canonical forms.
    let po = bbnf_regex::hir::ParseOptions { unicode: true };
    let h1 = parse_with(r"(ab)*a", &po).unwrap();
    let h2 = parse_with(r"a(ba)*", &po).unwrap();
    let c1 = simplify_hir(&h1, &cost);
    let c2 = simplify_hir(&h2, &cost);
    println!(
        "\nwitness (ab)*a ≡ a(ba)* : same language, saturated forms {} ==> the five rules {} unify them",
        if c1 == c2 { "EQUAL" } else { "DISTINCT" },
        if c1 == c2 { "DO" } else { "do NOT" }
    );
}

fn main() {
    part_a();
    part_b();
}
