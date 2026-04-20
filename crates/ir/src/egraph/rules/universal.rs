//! Universal e-graph rewrites (AY.W2.3) — G1-G2.
//!
//! Semantics-preserving canonicalisations that fire as
//! IR-canonicalisation passes before emit. Narrow, low-risk, and
//! compositionally composable with the existing regex-algebra +
//! common-suffix rule set.
//!
//! - **G1** [`AltOfSingle`]     — `Alt([x])     ≡ x`.
//! - **G2** [`RepeatOfSingle`]  — `Repeat { inner: x, lo: 1, hi: 1 } ≡ x`.

use egraph::{Analysis, EGraph, Id, Rewrite};

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
