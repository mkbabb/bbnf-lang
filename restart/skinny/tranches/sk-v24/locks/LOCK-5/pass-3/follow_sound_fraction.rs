//! LOCK-5 pass-3 PROTO-1 (family 5D GENERATOR-PATH) — probe G-5D.4.
//!
//! QUESTION. Of the regex leaves in the live grammar corpus, what fraction are
//! FOLLOW-sound for straight-line one-pass lowering — i.e.
//!
//!     first(C) ∩ FOLLOW(leaf) = ∅
//!
//! — and which leaves REQUIRE `Seq[Repeat, FOLLOW-guard]`? A high sound fraction
//! makes 5D's direct class-scan the default generator path and retreats the DFA
//! to fallback; a low fraction (the Ragel nondeterminism trap) keeps the DFA
//! central.
//!
//! METHOD.
//!   (1) Build the grammar FIRST/NULLABLE/FOLLOW closures as fixed-points with a
//!       Changed-flag convergence (NOT a structural hash), dual to the existing
//!       `first_bytes` at `skinny/crates/passes/src/lib.rs:911`. FOLLOW is
//!       computed per-`ExprId` so every `Regex` leaf gets its position-specific
//!       follow-set; `Ref` sites bridge FOLLOW back into the referenced rule.
//!   (2) For each `ExprKind::Regex` leaf, compute first(C) via
//!       `bbnf_regex::analyze(pattern).first` (→ `ByteSet256`) and test
//!       disjointness against FOLLOW(leaf).
//!   (3) Report `disjoint=N/M frac=X%` and NAME every non-disjoint leaf.
//!
//! SOUNDNESS CONTRAST. The in-tree `check_one_pass_eligible` (`one_pass.rs:64`)
//! is UNSOUND: its `Repetition` arm recurses into the sub and never checks the
//! repeated class against its own FOLLOW, so `[a-z]+z` on `"abz"` returns
//! one-pass=`true` though a straight-line lowering rejects a matching string
//! (greedy give-back). The injected counterexample
//!
//!     word = /[a-z]+/ ;  sentence = word "and" word ;
//!
//! is the grammar-level image of that flaw: `first([a-z]) = {a..z}` contains
//! `'a' = first("and") ∈ FOLLOW(word)`, so the greedy run STEALS "and". PASS
//! requires this leaf to fall in the NON-disjoint (guard-required) partition —
//! the FOLLOW predicate must be SOUND where the recursive predicate is not.

use std::collections::{BTreeMap, BTreeSet};

use bbnf_regex::{analyze, FirstSet};
use ir::{ExprId, ExprKind, GrammarIr, RuleId};

// ── Corpus ───────────────────────────────────────────────────────────────────

/// The injected counterexample: the grammar-level image of the unsound
/// `Repetition` arm. The greedy `[a-z]+` must be shown to steal "and".
const COUNTEREXAMPLE: &str = r#"
word     = /[a-z]+/ ;
sentence = word "and" word ;
"#;

fn manifest_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// `skinny/grammars/json.bbnf` — the in-tree engine corpus.
fn load_json() -> Option<GrammarIr> {
    let path = manifest_dir().join("../../grammars/json.bbnf");
    let src = std::fs::read_to_string(&path).ok()?;
    match grammar::parse_grammar("json", &src) {
        Ok(mut g) => {
            g.resolve_refs().ok()?;
            Some(g)
        }
        Err(e) => {
            eprintln!("  [corpus] json.bbnf did not parse under skinny: {e}");
            None
        }
    }
}

/// `grammar/google-sheets/google-sheets.bbnf` — the second in-tree engine
/// corpus. Full BBNF; parsed under the skinny dialect on a best-effort basis
/// and reported honestly if it does not load.
fn load_sheets() -> Option<GrammarIr> {
    let path = manifest_dir().join("../../../grammar/google-sheets/google-sheets.bbnf");
    let src = std::fs::read_to_string(&path).ok()?;
    match grammar::parse_grammar("sheets", &src) {
        Ok(mut g) => match g.resolve_refs() {
            Ok(()) => Some(g),
            Err(e) => {
                eprintln!("  [corpus] google-sheets.bbnf refs unresolved under skinny: {e:?}");
                None
            }
        },
        Err(e) => {
            eprintln!("  [corpus] google-sheets.bbnf did not parse under skinny: {e}");
            None
        }
    }
}

fn load_counterexample() -> GrammarIr {
    let mut g = grammar::parse_grammar("counterexample", COUNTEREXAMPLE)
        .expect("counterexample grammar must parse");
    g.resolve_refs().expect("counterexample refs resolve");
    g
}

// ── Byte-set closures: FIRST / NULLABLE / FOLLOW ──────────────────────────────
//
// Every set is a `BTreeSet<u8>` so disjointness is `is_disjoint`. All three
// closures converge via a Changed-flag round loop (`while changed { … }`) — the
// LLVM-style fixed point the brief mandates, NOT a structural hash. A leaf whose
// FIRST cannot be proven (regex `FirstSet::Unknown`) is over-approximated to the
// full byte alphabet, keeping FOLLOW (hence the soundness verdict) conservative.

fn all_bytes() -> BTreeSet<u8> {
    (0u8..=255).collect()
}

/// Regex first(C) as the brief defines it: `analyze(pattern).first` → bytes.
/// `Unknown` ⇒ unprovable ⇒ full alphabet (sound over-approximation).
fn regex_first(pattern: &str) -> BTreeSet<u8> {
    match analyze(pattern).first {
        FirstSet::Exact(set) => set.to_vec().into_iter().collect(),
        FirstSet::Unknown => all_bytes(),
    }
}

fn regex_nullable(pattern: &str) -> bool {
    analyze(pattern).nullable
}

/// NULLABLE(expr) for every `ExprId`, converged with a Changed flag.
fn nullable_closure(g: &GrammarIr) -> Vec<bool> {
    let mut null = vec![false; g.exprs.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for (idx, expr) in g.exprs.iter().enumerate() {
            let v = match &expr.kind {
                ExprKind::Seq(children) => children.iter().all(|c| null[c.0]),
                ExprKind::Alt { branches, .. } => branches.iter().any(|b| null[b.0]),
                ExprKind::Repeat { body, min, .. } => *min == 0 || null[body.0],
                ExprKind::Optional(_) => true,
                ExprKind::Literal { bytes, .. } => bytes.is_empty(),
                ExprKind::Regex { pattern } => regex_nullable(pattern),
                ExprKind::Ref { target: Some(t), .. } => null[g.rule(*t).unwrap().body.0],
                ExprKind::Ref { target: None, .. } => false,
                // Annotations (`?w`) match nothing structural → treat as nullable.
                ExprKind::Annotation { .. } => true,
                ExprKind::Project { body, .. } => null[body.0],
                ExprKind::Discard { left, right, .. } => null[left.0] && null[right.0],
            };
            if v && !null[idx] {
                null[idx] = true;
                changed = true;
            }
        }
    }
    null
}

/// FIRST(expr) for every `ExprId`, converged with a Changed flag.
fn first_closure(g: &GrammarIr, null: &[bool]) -> Vec<BTreeSet<u8>> {
    let mut first: Vec<BTreeSet<u8>> = vec![BTreeSet::new(); g.exprs.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for (idx, expr) in g.exprs.iter().enumerate() {
            let add: BTreeSet<u8> = match &expr.kind {
                ExprKind::Seq(children) => {
                    let mut acc = BTreeSet::new();
                    for c in children {
                        acc.extend(first[c.0].iter().copied());
                        if !null[c.0] {
                            break;
                        }
                    }
                    acc
                }
                ExprKind::Alt { branches, .. } => {
                    let mut acc = BTreeSet::new();
                    for b in branches {
                        acc.extend(first[b.0].iter().copied());
                    }
                    acc
                }
                ExprKind::Repeat { body, .. } => first[body.0].clone(),
                ExprKind::Optional(body) => first[body.0].clone(),
                ExprKind::Literal { bytes, .. } => bytes.first().copied().into_iter().collect(),
                ExprKind::Regex { pattern } => regex_first(pattern),
                ExprKind::Ref { target: Some(t), .. } => first[g.rule(*t).unwrap().body.0].clone(),
                ExprKind::Ref { target: None, .. } => all_bytes(),
                ExprKind::Annotation { .. } => BTreeSet::new(),
                ExprKind::Project { body, .. } => first[body.0].clone(),
                ExprKind::Discard { left, right, .. } => {
                    let mut acc = first[left.0].clone();
                    if null[left.0] {
                        acc.extend(first[right.0].iter().copied());
                    }
                    acc
                }
            };
            let before = first[idx].len();
            first[idx].extend(add);
            if first[idx].len() != before {
                changed = true;
            }
        }
    }
    first
}

/// FOLLOW(expr) for every `ExprId`, converged with a Changed flag. Roots seed
/// EOF (⊘ bytes — never intersects any first-set). `Ref` sites bridge FOLLOW of
/// the reference into the referenced rule's body; that inter-rule edge is what
/// forces the fixed point.
fn follow_closure(g: &GrammarIr, null: &[bool], first: &[BTreeSet<u8>]) -> Vec<BTreeSet<u8>> {
    // FIRST of the concatenation of `rest`, folding in FOLLOW-of-parent when the
    // whole tail is nullable.
    let first_of_tail = |rest: &[ExprId], parent_follow: &BTreeSet<u8>| -> BTreeSet<u8> {
        let mut acc = BTreeSet::new();
        let mut all_null = true;
        for e in rest {
            acc.extend(first[e.0].iter().copied());
            if !null[e.0] {
                all_null = false;
                break;
            }
        }
        if all_null {
            acc.extend(parent_follow.iter().copied());
        }
        acc
    };

    let mut follow: Vec<BTreeSet<u8>> = vec![BTreeSet::new(); g.exprs.len()];
    let mut changed = true;
    while changed {
        changed = false;
        // Snapshot parent follow-sets read this round.
        let prev = follow.clone();
        let mut push = |target: usize, bytes: &BTreeSet<u8>, changed: &mut bool| {
            let before = follow[target].len();
            follow[target].extend(bytes.iter().copied());
            if follow[target].len() != before {
                *changed = true;
            }
        };

        for expr in &g.exprs {
            let parent_follow = prev[expr.id.0].clone();
            match &expr.kind {
                ExprKind::Seq(children) => {
                    for (i, child) in children.iter().enumerate() {
                        let tail = first_of_tail(&children[i + 1..], &parent_follow);
                        push(child.0, &tail, &mut changed);
                    }
                }
                ExprKind::Alt { branches, .. } => {
                    for b in branches {
                        push(b.0, &parent_follow, &mut changed);
                    }
                }
                ExprKind::Repeat { body, .. } => {
                    // The loop may re-enter: FOLLOW(body) ⊇ FIRST(body) (another
                    // iteration) ∪ FOLLOW(repeat) (loop exit).
                    let mut b = first[body.0].clone();
                    b.extend(parent_follow.iter().copied());
                    push(body.0, &b, &mut changed);
                }
                ExprKind::Optional(body) | ExprKind::Project { body, .. } => {
                    push(body.0, &parent_follow, &mut changed);
                }
                ExprKind::Discard { left, right, .. } => {
                    let tail = first_of_tail(&[*right], &parent_follow);
                    push(left.0, &tail, &mut changed);
                    push(right.0, &parent_follow, &mut changed);
                }
                ExprKind::Ref { target: Some(t), .. } => {
                    // Bridge: FOLLOW(rule body) ⊇ FOLLOW(this reference).
                    let body = g.rule(*t).unwrap().body.0;
                    push(body, &parent_follow, &mut changed);
                }
                ExprKind::Ref { target: None, .. }
                | ExprKind::Literal { .. }
                | ExprKind::Regex { .. }
                | ExprKind::Annotation { .. } => {}
            }
        }
    }
    follow
}

/// Rules that no other rule references are roots; their body follow-set carries
/// EOF (no bytes). Nothing to seed at the byte level — included for clarity.
fn root_rules(g: &GrammarIr) -> Vec<RuleId> {
    let mut referenced = BTreeSet::new();
    for expr in &g.exprs {
        if let ExprKind::Ref { target: Some(t), .. } = &expr.kind {
            referenced.insert(t.0);
        }
    }
    g.rules
        .iter()
        .filter(|r| !referenced.contains(&r.id.0))
        .map(|r| r.id)
        .collect()
}

// ── The probe ─────────────────────────────────────────────────────────────────

/// Which rule an expr lives in (for naming leaves).
fn owning_rule(g: &GrammarIr) -> BTreeMap<usize, String> {
    // Walk each rule body; attribute every reachable expr to that rule name.
    let mut owner = BTreeMap::new();
    for rule in &g.rules {
        let mut stack = vec![rule.body];
        while let Some(id) = stack.pop() {
            if owner.contains_key(&id.0) {
                continue;
            }
            owner.insert(id.0, rule.name.clone());
            match &g.expr(id).kind {
                ExprKind::Seq(cs) => stack.extend(cs.iter().copied()),
                ExprKind::Alt { branches, .. } => stack.extend(branches.iter().copied()),
                ExprKind::Repeat { body, .. }
                | ExprKind::Optional(body)
                | ExprKind::Project { body, .. } => stack.push(*body),
                ExprKind::Discard { left, right, .. } => {
                    stack.push(*left);
                    stack.push(*right);
                }
                _ => {}
            }
        }
    }
    owner
}

struct LeafVerdict {
    grammar: String,
    rule: String,
    pattern: String,
    first_unknown: bool,
    nullable: bool,
    disjoint: bool,
    /// Give-back is possible ONLY through a trailing unbounded run: a leaf with
    /// no unescaped top-level `*`/`+` matches a bounded-length language and is
    /// one-pass BY CONSTRUCTION, so any raw first∩FOLLOW flag on it is a
    /// leading-first artifact, not a real give-back. This flag is SOUND: a leaf
    /// it marks bounded genuinely cannot give back.
    give_back_prone: bool,
    overlap: Vec<u8>,
}

/// Does the pattern contain an unescaped, non-class `*` or `+`? If not, the
/// language is bounded-length and the leaf cannot give back.
fn has_unbounded_rep(pattern: &str) -> bool {
    let bytes = pattern.as_bytes();
    let mut escaped = false;
    let mut in_class = false;
    for &b in bytes {
        if escaped {
            escaped = false;
            continue;
        }
        match b {
            b'\\' => escaped = true,
            b'[' => in_class = true,
            b']' => in_class = false,
            b'*' | b'+' if !in_class => return true,
            _ => {}
        }
    }
    false
}

fn adjudicate(name: &str, g: &GrammarIr, out: &mut Vec<LeafVerdict>) {
    let null = nullable_closure(g);
    let first = first_closure(g, &null);
    let follow = follow_closure(g, &null, &first);
    let _roots = root_rules(g);
    let owner = owning_rule(g);

    for expr in &g.exprs {
        let ExprKind::Regex { pattern } = &expr.kind else {
            continue;
        };
        let facts = analyze(pattern);
        let first_unknown = matches!(facts.first, FirstSet::Unknown);
        let fc = regex_first(pattern);
        let fol = &follow[expr.id.0];
        let overlap: Vec<u8> = fc.intersection(fol).copied().collect();
        out.push(LeafVerdict {
            grammar: name.to_string(),
            rule: owner
                .get(&expr.id.0)
                .cloned()
                .unwrap_or_else(|| "<unrooted>".into()),
            pattern: pattern.clone(),
            first_unknown,
            nullable: facts.nullable,
            disjoint: overlap.is_empty(),
            give_back_prone: has_unbounded_rep(pattern),
            overlap,
        });
    }
}

fn show_bytes(bytes: &[u8]) -> String {
    let rendered: Vec<String> = bytes
        .iter()
        .map(|b| match b {
            b' ' => "SP".to_string(),
            b'\t' => "\\t".to_string(),
            b'\n' => "\\n".to_string(),
            b'\r' => "\\r".to_string(),
            0x20..=0x7e => (*b as char).to_string(),
            other => format!("0x{other:02x}"),
        })
        .collect();
    format!("[{}]", rendered.join(","))
}

#[test]
fn follow_sound_fraction() {
    let mut verdicts: Vec<LeafVerdict> = Vec::new();

    eprintln!("=== corpus load ===");
    if let Some(g) = load_json() {
        eprintln!("  json:    {} rules loaded", g.rules.len());
        adjudicate("json", &g, &mut verdicts);
    }
    if let Some(g) = load_sheets() {
        eprintln!("  sheets:  {} rules loaded", g.rules.len());
        adjudicate("sheets", &g, &mut verdicts);
    }
    let cx = load_counterexample();
    eprintln!("  counterexample: {} rules loaded", cx.rules.len());
    adjudicate("counterexample", &cx, &mut verdicts);

    // ── Report ────────────────────────────────────────────────────────────────
    let total = verdicts.len();
    let disjoint = verdicts.iter().filter(|v| v.disjoint).count();
    let frac = if total == 0 {
        0.0
    } else {
        100.0 * disjoint as f64 / total as f64
    };

    eprintln!("\n=== per-leaf verdicts (grammar :: rule) ===");
    for v in &verdicts {
        let tag = if v.disjoint {
            "SOUND     "
        } else if !v.give_back_prone {
            "BOUNDED*  " // raw-flagged but give-back impossible (leading-first artifact)
        } else {
            "GUARD-REQ "
        };
        let flags = {
            let mut f = Vec::new();
            if v.nullable {
                f.push("nullable");
            }
            if !v.give_back_prone {
                f.push("bounded");
            }
            if v.first_unknown {
                f.push("first=UNKNOWN");
            }
            if f.is_empty() {
                String::new()
            } else {
                format!(" ({})", f.join(","))
            }
        };
        eprintln!(
            "  {tag}  {}::{:<14} /{}/{}",
            v.grammar, v.rule, v.pattern, flags
        );
        if !v.disjoint {
            eprintln!(
                "             first(C) ∩ FOLLOW = {}  ← greedy give-back / guard required",
                show_bytes(&v.overlap)
            );
        }
    }

    let non_disjoint: Vec<&LeafVerdict> = verdicts.iter().filter(|v| !v.disjoint).collect();

    eprintln!("\n=== NON-DISJOINT (require Seq[Repeat, FOLLOW-guard]) ===");
    for v in &non_disjoint {
        eprintln!(
            "  {}::{}  /{}/  overlap={}",
            v.grammar,
            v.rule,
            v.pattern,
            show_bytes(&v.overlap)
        );
    }

    // Layered reading. The raw predicate is SOUND but not COMPLETE: it flags
    // bounded leaves (no trailing unbounded run) purely on their leading-first,
    // though give-back is impossible there. Separate the two populations.
    let bounded = verdicts.iter().filter(|v| !v.give_back_prone).count();
    let prone: Vec<&LeafVerdict> = verdicts.iter().filter(|v| v.give_back_prone).collect();
    let prone_sound = prone.iter().filter(|v| v.disjoint).count();
    let prone_total = prone.len();
    // "Effectively one-pass": bounded-by-construction ∪ give-back-prone-sound.
    let eff_one_pass = bounded + prone_sound;
    let eff_frac = 100.0 * eff_one_pass as f64 / total as f64;
    let prone_frac = if prone_total == 0 {
        0.0
    } else {
        100.0 * prone_sound as f64 / prone_total as f64
    };

    eprintln!("\n================ RESULT ================");
    eprintln!("[brief-literal] first(C)∩FOLLOW=∅ : disjoint={disjoint}/{total} frac={frac:.1}%");
    eprintln!(
        "[bounded, give-back impossible by construction] : {bounded}/{total}"
    );
    eprintln!(
        "[give-back-prone (trailing unbounded run)] : {prone_total}/{total}; of those FOLLOW-sound={prone_sound}/{prone_total} ({prone_frac:.1}%)"
    );
    eprintln!(
        "[effectively one-pass = bounded ∪ prone-sound] : {eff_one_pass}/{total} ({eff_frac:.1}%)"
    );
    eprintln!("=======================================");

    // ── PASS/FAIL gate ──────────────────────────────────────────────────────────
    // The counterexample leaf MUST be classified NON-disjoint: the FOLLOW
    // predicate must be SOUND exactly where `one_pass.rs:64` is not.
    let cx_leaf = verdicts
        .iter()
        .find(|v| v.grammar == "counterexample")
        .expect("counterexample leaf present");
    assert!(
        !cx_leaf.disjoint,
        "SOUNDNESS FAILURE: word=/[a-z]+/ classified FOLLOW-sound though the greedy \
         run steals \"and\"; the predicate is unsound (the `one_pass.rs:64` bug)."
    );
    assert!(
        cx_leaf.overlap.contains(&b'a'),
        "counterexample overlap must contain 'a' = first(\"and\"); got {:?}",
        cx_leaf.overlap
    );

    assert!(total > 0, "empty corpus — nothing measured");
    eprintln!(
        "PASS: fraction reported ({disjoint}/{total} = {frac:.1}%) and the word/\"and\" \
         counterexample is NON-disjoint (predicate SOUND)."
    );
}
