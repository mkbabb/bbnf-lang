//! Universal e-graph rewrites (AY.W2.3) — G1, G2, and G4.
//!
//! Semantics-preserving canonicalisations that fire as
//! IR-canonicalisation passes before emit. Narrow, low-risk, and
//! compositionally composable with the existing regex-algebra +
//! common-suffix rule set.
//!
//! - **G1** [`AltOfSingle`]     — `Alt([x])     ≡ x`.
//! - **G2** [`RepeatOfSingle`]  — `Repeat { inner: x, lo: 1, hi: 1 } ≡ x`.
//! - **G4** [`ConcatLiterals`]  — `Seq([.., Literal(a), Literal(b), ..])
//!   ≡ Seq([.., Literal(ab), ..])` for every adjacent literal run
//!   inside a Seq.

use egraph::{Analysis, EGraph, Id, Rewrite};

use crate::StringId;
use crate::egraph::interner::SharedStrings;
use crate::egraph::node::GrammarENode;

// ── G1: Alt([x]) ≡ x ─────────────────────────────────────────────────────────

/// Collapse a single-branch Alt onto its sole child. Preserves
/// semantics — an `Alt` with one branch degenerates to that branch.
/// Canonical-form cleanup; fires whenever prior saturation (e.g.
/// `SupersetAbsorbAlt` / `DeduplicateAltBranches`) leaves a lone
/// survivor.
pub struct AltOfSingle;

#[derive(Clone, Debug)]
pub struct AltOfSingleMatch {
    /// The surviving child's canonical id.
    pub sole: Id,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for AltOfSingle {
    type Match = AltOfSingleMatch;

    fn name(&self) -> &str {
        "alt-of-single"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, _dispatch) = node else {
                    continue;
                };
                if children.len() == 1 {
                    matches.push((
                        class.id,
                        AltOfSingleMatch {
                            sole: egraph.find_ref(children[0]),
                        },
                    ));
                    break;
                }
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        egraph.union(class_id, m.sole);
    }
}

// ── G2: Repeat { lo: 1, hi: 1 } ≡ inner ──────────────────────────────────────

/// Collapse a `Repeat { lo: 1, hi: 1 }` onto its inner expression.
/// The bounded single-repetition case is a pure identity — no
/// loop, no optionality — and the `Repeat` wrapper disappears under
/// extraction.
pub struct RepeatOfSingle;

#[derive(Clone, Debug)]
pub struct RepeatOfSingleMatch {
    pub inner: Id,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for RepeatOfSingle {
    type Match = RepeatOfSingleMatch;

    fn name(&self) -> &str {
        "repeat-of-single"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Repeat { inner, lo, hi } = node else {
                    continue;
                };
                if *lo == 1 && *hi == 1 {
                    matches.push((
                        class.id,
                        RepeatOfSingleMatch {
                            inner: egraph.find_ref(*inner),
                        },
                    ));
                    break;
                }
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        egraph.union(class_id, m.inner);
    }
}

// ── G4: adjacent-literal concatenation inside Seq ────────────────────────────

/// Fuse adjacent runs of `Literal` children inside a `Seq` into one
/// combined literal. Preserves semantics — parsing `"ab"` is the
/// same as parsing `"a"` then `"b"` — while reducing the node count
/// by `run_len - 1` per run and making downstream prefix analysis
/// (FIRST sets, dispatch tables) tighter.
///
/// # Shape
///
/// Matches any `Seq` whose child list contains two or more
/// consecutive entries whose canonical classes carry a `Literal`
/// e-node. Multi-run `Seq`s (e.g. `Lit('a'), Lit('b'), Ref, Lit('c'),
/// Lit('d')`) fuse each run independently in one apply.
///
/// # Interning
///
/// The combined string is interned through `SharedStrings` so the
/// pool round-trips into `GrammarIR::strings` at `write_back` time.
/// The new `Literal(fused_sid)` is added inside `apply` under a
/// mutable egraph borrow.
pub struct ConcatLiterals {
    pool: SharedStrings,
}

impl ConcatLiterals {
    pub fn new(pool: SharedStrings) -> Self {
        Self { pool }
    }
}

/// One fused run within a Seq: the range `[start, start+len)` of the
/// original child list is replaced by a single `Literal(fused_sid)`.
#[derive(Clone, Debug)]
pub struct LiteralRun {
    pub start: usize,
    pub len: usize,
    pub fused_sid: StringId,
}

#[derive(Clone, Debug)]
pub struct ConcatLiteralsMatch {
    /// The original Seq children — copied into the match so `apply`
    /// doesn't need to re-resolve the matched e-node.
    pub original_children: Box<[Id]>,
    /// Fused runs, sorted by `start`, non-overlapping.
    pub runs: Box<[LiteralRun]>,
}

fn class_literal<A: Analysis<GrammarENode>>(
    egraph: &EGraph<GrammarENode, A>,
    id: Id,
) -> Option<StringId> {
    egraph.class(id).iter().find_map(|n| match n {
        GrammarENode::Literal(sid) => Some(*sid),
        _ => None,
    })
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for ConcatLiterals {
    type Match = ConcatLiteralsMatch;

    fn name(&self) -> &str {
        "concat-literals"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Seq(children) = node else {
                    continue;
                };
                if children.len() < 2 {
                    continue;
                }

                // Scan the child list collecting adjacent literal-class
                // runs of length ≥ 2.
                let mut runs: Vec<LiteralRun> = Vec::new();
                let mut current_start: Option<usize> = None;
                let mut current_lits: Vec<StringId> = Vec::new();

                let flush = |runs: &mut Vec<LiteralRun>,
                             current_start: &mut Option<usize>,
                             current_lits: &mut Vec<StringId>,
                             pool: &SharedStrings| {
                    if current_lits.len() >= 2 {
                        let combined: String =
                            current_lits.iter().map(|&sid| pool.get(sid)).collect();
                        let fused_sid = pool.intern(&combined);
                        runs.push(LiteralRun {
                            start: current_start.unwrap(),
                            len: current_lits.len(),
                            fused_sid,
                        });
                    }
                    current_lits.clear();
                    *current_start = None;
                };

                for (i, &child_id) in children.iter().enumerate() {
                    match class_literal(egraph, child_id) {
                        Some(sid) => {
                            if current_start.is_none() {
                                current_start = Some(i);
                            }
                            current_lits.push(sid);
                        }
                        None => {
                            flush(&mut runs, &mut current_start, &mut current_lits, &self.pool);
                        }
                    }
                }
                flush(&mut runs, &mut current_start, &mut current_lits, &self.pool);

                if runs.is_empty() {
                    continue;
                }

                matches.push((
                    class.id,
                    ConcatLiteralsMatch {
                        original_children: children.clone(),
                        runs: runs.into_boxed_slice(),
                    },
                ));
                break;
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        // Reconstruct the child list: each run's range is replaced by
        // one fresh `Literal(fused_sid)` node; non-run entries pass
        // through unchanged.
        let mut new_children: Vec<Id> = Vec::with_capacity(m.original_children.len());
        let mut run_iter = m.runs.iter().peekable();
        let mut i = 0usize;
        while i < m.original_children.len() {
            match run_iter.peek() {
                Some(run) if run.start == i => {
                    let fused_id = egraph.add(GrammarENode::Literal(run.fused_sid));
                    new_children.push(fused_id);
                    i += run.len;
                    run_iter.next();
                }
                _ => {
                    new_children.push(m.original_children[i]);
                    i += 1;
                }
            }
        }
        let new_id = if new_children.len() == 1 {
            new_children[0]
        } else {
            egraph.add(GrammarENode::Seq(new_children.into_boxed_slice()))
        };
        egraph.union(class_id, new_id);
    }
}
