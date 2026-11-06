//! LOCK-5 pass-2 PROTO-1 (family 5B LAWFUL-FUNCTOR) — probes (a) + (b).
//!
//! (b) Cross-domain round-trip oracle: for every pattern in the
//!     `regex_hir_egraph` corpus (62 patterns), the functor path
//!     `parse -> insert_hir -> saturate -> extract_hir` (== `simplify_hir`,
//!     which runs the SAME five HIR rewrite rules the grammar tier would
//!     reuse) must produce a HIR whose minimized DFA is LANGUAGE-EQUIVALENT
//!     to the original pattern's minimized DFA. Oracle = product-BFS
//!     symmetric-difference over all 256 bytes (canonical/minimize on).
//!
//! (a) Structural fusion vs string fusion: the grammar tier fuses
//!     `[a-c] | [d-f]` with `parts.join("|")` -> "[a-c]|[d-f]". The functor
//!     rebuilds it structurally (Alternation -> union-merge -> `[a-f]`).
//!     Both must compile to language-equivalent DFAs.

use bbnf_regex::egraph::{RegexExtractionCost, simplify_hir};
use bbnf_regex::hir::{Hir, ParseOptions};
use bbnf_regex::{DEAD, Dfa, DfaOptions, Nfa, StateId, parse_with};

const PATTERNS: &[&str] = &[
    r#"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"#,
    r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    r"[a-zA-Z_][\w-]*",
    r"-?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[\-]?[a-zA-Z_][\w-]*|--[\w-]+",
    r"[-+]?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?",
    r"[-+]?\d+",
    r"(?s)(?:\s|/\*.*?\*/)*",
    r"[0-9a-fA-F]{3,8}",
    r"[0-9a-fA-F]+",
    r"[iIsS]",
    r"[^\]\\]",
    r"[^\\()\[\]{}|*+?.^$]",
    r#""(?:[^"\\]|\\[\s\S])*"|'(?:[^'\\]|\\[\s\S])*'"#,
    r"\d+",
    r"\s+",
    r"\w+",
    r"[a-z]+",
    r".",
    r"\\",
    r"(?:foo|bar|baz)+",
    r"(a|b)(c|d)",
    r"a*",
    r"a+?",
    r"a{3,5}",
    r"\n\r\t",
    r"\.",
    r"",
    r"(?:-?[a-zA-Z_]|\\[^\n])(?:[\w-]|\\[^\n])*",
    r"[-+]?\d*n",
    r"https?://[a-zA-Z0-9./-]+",
    r"(?:[a-z]+\.)+[a-z]+",
    r"#[0-9a-fA-F]{3,6}",
    r"\d+px",
    r"\d+%",
    r"\d+(?:px|em|rem|vh|vw)",
    r"\d{4}-\d{2}-\d{2}",
    r"\d{2}:\d{2}:\d{2}",
    r"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}",
    r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
    r"0x[0-9a-fA-F]+",
    r"0b[01]+",
    r"0[0-7]+",
    r"\d+\.\d+",
    r"\d+(?:_\d+)*",
    r"[A-Za-z0-9+/]+",
    r"\$[a-zA-Z_][a-zA-Z0-9_]*",
    r"[a-zA-Z][a-zA-Z0-9]*",
    r"\w+(?:[-_]\w+)*",
    r"(?:foo|bar)*",
    r"(?:[a-z]{2,4})+",
    r"(?:0|[1-9][0-9]*)",
    r"[a-zA-Z_]\w*\s*=\s*\d+",
    r"\([^)]*\)",
    r"[a-c]|[d-f]|[g-i]",
    r"[a-z]|[a-c]",
    r"a|b|a|c|b",
    r"a+a*",
    r"(?:(?:a|b)|c|(?:d|e))",
];

fn dfa_of(hir: &Hir) -> Option<Dfa> {
    let nfa = Nfa::from_hir(hir)?;
    let opts = DfaOptions {
        minimize: true,
        ..Default::default()
    };
    Dfa::from_nfa(&nfa, &opts)
}

fn accept(dfa: &Dfa, s: StateId) -> bool {
    s != DEAD && (s as usize) < dfa.states.len() && dfa.states[s as usize].is_accept
}

fn step(dfa: &Dfa, s: StateId, byte: u8) -> StateId {
    if s == DEAD || (s as usize) >= dfa.states.len() {
        return DEAD;
    }
    let class = dfa.byte_classes[byte as usize] as usize;
    let idx = s as usize * dfa.num_classes as usize + class;
    if idx >= dfa.flat_transitions.len() {
        return DEAD;
    }
    dfa.flat_transitions[idx]
}

/// Rigorous language-equivalence: BFS the product automaton over all 256
/// input bytes from the paired start states (0,0); the languages differ
/// iff any reachable product state disagrees on acceptance. Both DFAs are
/// Hopcroft-minimized, so this is a complete decision procedure.
fn dfa_equivalent(a: &Dfa, b: &Dfa) -> bool {
    use std::collections::{HashSet, VecDeque};
    let mut seen: HashSet<(StateId, StateId)> = HashSet::new();
    let mut q: VecDeque<(StateId, StateId)> = VecDeque::new();
    let start = (0u32, 0u32);
    seen.insert(start);
    q.push_back(start);
    while let Some((sa, sb)) = q.pop_front() {
        if accept(a, sa) != accept(b, sb) {
            return false;
        }
        for byte in 0u16..256 {
            let na = step(a, sa, byte as u8);
            let nb = step(b, sb, byte as u8);
            if seen.insert((na, nb)) {
                q.push_back((na, nb));
            }
        }
    }
    true
}

#[test]
fn probe_b_corpus_roundtrip_dfa_equivalent() {
    let opts = ParseOptions::byte_mode();
    let cost = RegexExtractionCost::default();

    let mut parsed = 0usize;
    let mut checked = 0usize;
    let mut equivalent = 0usize;
    let mut non_equiv: Vec<&str> = Vec::new();
    let mut uncompilable: Vec<&str> = Vec::new();

    for &p in PATTERNS {
        let Ok(hir_orig) = parse_with(p, &opts) else {
            continue; // parse failures are out of scope for the functor
        };
        parsed += 1;
        let hir_simplified = simplify_hir(&hir_orig, &cost);

        let (Some(d_orig), Some(d_simp)) = (dfa_of(&hir_orig), dfa_of(&hir_simplified)) else {
            uncompilable.push(p);
            continue;
        };
        checked += 1;
        if dfa_equivalent(&d_orig, &d_simp) {
            equivalent += 1;
        } else {
            non_equiv.push(p);
        }
    }

    eprintln!(
        "PROBE-B corpus: total={} parsed={} dfa_checked={} equivalent={} non_equiv={} uncompilable={}",
        PATTERNS.len(),
        parsed,
        checked,
        equivalent,
        non_equiv.len(),
        uncompilable.len()
    );
    if !non_equiv.is_empty() {
        eprintln!("  NON-EQUIVALENT: {:?}", non_equiv);
    }
    if !uncompilable.is_empty() {
        eprintln!("  UNCOMPILABLE:   {:?}", uncompilable);
    }

    assert!(
        non_equiv.is_empty(),
        "functor round-trip changed the language on {} patterns: {:?}",
        non_equiv.len(),
        non_equiv
    );
    assert_eq!(
        equivalent, checked,
        "not every compiled pattern was DFA-equivalent"
    );
    assert!(checked >= 55, "too few patterns exercised: {}", checked);
}

#[test]
fn probe_a_structural_fusion_equals_string_fusion() {
    // String fusion (grammar tier `parts.join("|")`): "[a-c]|[d-f]".
    let string_fused = Dfa::compile("[a-c]|[d-f]").expect("string-fused pattern compiles");

    // Functor fusion: build the Alternation HIR, run the regex-side
    // union/flatten/superset rules, extract. Result is `[a-f]`.
    let opts = ParseOptions::byte_mode();
    let cost = RegexExtractionCost::default();
    let hir = parse_with("[a-c]|[d-f]", &opts).expect("parses");
    let functor_hir = simplify_hir(&hir, &cost);
    let functor_dfa = dfa_of(&functor_hir).expect("functor HIR compiles");

    assert!(
        dfa_equivalent(&string_fused, &functor_dfa),
        "functor fusion DFA differs from string-fusion DFA"
    );

    // And the functor genuinely simplified (not identity): the extracted
    // form is a single merged class, not a two-branch alternation.
    assert!(
        matches!(functor_hir, Hir::Class(_)),
        "expected functor to merge to a single Class, got {:?}",
        functor_hir
    );
}
