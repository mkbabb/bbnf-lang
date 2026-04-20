//! Universal e-graph rewrites (AY.W2.3) — G1-G3.
//!
//! Semantics-preserving canonicalisations that fire as
//! IR-canonicalisation passes before emit. Narrow, low-risk, and
//! compositionally composable with the existing regex-algebra +
//! common-suffix rule set.
//!
//! - **G1** [`AltOfSingle`]     — `Alt([x])     ≡ x`.
//! - **G2** [`RepeatOfSingle`]  — `Repeat { inner: x, lo: 1, hi: 1 } ≡ x`.
//! - **G3** [`WrapOfEpsilonScalar`] — `Alt([leaf, ε]) ≡ leaf` when
//!   `leaf`'s class projects to a scalar `TypeDesc`. **PRIMARY LEVER**
//!   for JSON wrap-compound elision (AY.md prop 2 / invariant 23 part 2).

use rustc_hash::FxHashMap;

use egraph::{Analysis, EGraph, Id, Rewrite};

use crate::egraph::node::GrammarENode;
use crate::{RuleId, TypeDesc};

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

// ── G3: Alt([leaf, ε]) ≡ leaf  (scalar projection) ───────────────────────────

/// **PRIMARY LEVER** (AY.md prop 2 / invariant 23 part 2).
///
/// Collapse a two-branch `Alt` where one branch is a scalar-
/// projecting form (Literal, Regex, or `Ref` to a rule whose
/// `TypeDesc` is a scalar payload) and the other branch is
/// `Epsilon`. The outer `Alt` is equivalent to the leaf branch
/// alone: on non-empty input the leaf matches; on empty input
/// either branch matches (ordered alternation picks epsilon, but
/// the scalar branch is cost-cheapest and structurally cleaner).
///
/// The rewrite targets JSON's `value`-wrap family + CSS transparent
/// `… | /* epsilon */` dispatchers. Post-G3 the rule body collapses
/// to the leaf; the `wrap.rs` shape emitter consults the canonical
/// form to skip its outer `push_compound` emission (AY.W2.6).
///
/// # Precondition
///
/// 1. The node is `Alt` with exactly 2 branches.
/// 2. Exactly one branch's canonical class contains `Epsilon`.
/// 3. The other branch's canonical class projects to a scalar
///    `TypeDesc`. Literal / Regex branches are scalar by
///    construction (`TypeDesc::Span`); `Ref` branches resolve
///    through the per-rule `TypeDesc` side-table.
pub struct WrapOfEpsilonScalar {
    /// Per-rule `TypeDesc` side-table — cloned out of
    /// `GrammarIR::types` at rule registration so `search` can
    /// resolve a branch's projection without a live IR borrow.
    rule_types: FxHashMap<RuleId, TypeDesc>,
}

impl WrapOfEpsilonScalar {
    pub fn new(rule_types: FxHashMap<RuleId, TypeDesc>) -> Self {
        Self { rule_types }
    }

    /// Whether a `TypeDesc` projection is a scalar payload
    /// (`Span`, `F64`, `Bool`, `U8`, …). Composite types (`Vec`,
    /// `Tuple`, `Map`, `Named`) return false — eliding the wrap
    /// would lose the outer Rule-compound structure the downstream
    /// projection needs.
    fn is_scalar(ty: &TypeDesc) -> bool {
        match ty {
            TypeDesc::Option(inner) => Self::is_scalar(inner),
            t => t.is_scalar_payload(),
        }
    }

    /// Resolve a branch class to its scalar-ness.
    ///
    /// - `Literal` / `Regex` leaves: scalar Span.
    /// - `Ref(rule)` with a scalar `TypeDesc`: scalar.
    /// - Otherwise: abstain.
    fn branch_is_scalar<A: Analysis<GrammarENode>>(
        &self,
        egraph: &EGraph<GrammarENode, A>,
        id: Id,
    ) -> bool {
        for node in egraph.class(id).iter() {
            match node {
                GrammarENode::Literal(_) | GrammarENode::Regex(_) => return true,
                GrammarENode::Ref(rid) => {
                    if let Some(ty) = self.rule_types.get(rid) {
                        if Self::is_scalar(ty) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn class_has_epsilon<A: Analysis<GrammarENode>>(
        &self,
        egraph: &EGraph<GrammarENode, A>,
        id: Id,
    ) -> bool {
        egraph
            .class(id)
            .iter()
            .any(|n| matches!(n, GrammarENode::Epsilon))
    }
}

#[derive(Clone, Debug)]
pub struct WrapOfEpsilonMatch {
    /// Canonical class of the scalar branch. Unioned into the outer
    /// Alt's class so cost-guided extraction picks it as cheapest.
    pub leaf: Id,
}

impl<A: Analysis<GrammarENode>> Rewrite<GrammarENode, A> for WrapOfEpsilonScalar {
    type Match = WrapOfEpsilonMatch;

    fn name(&self) -> &str {
        "wrap-of-epsilon-scalar"
    }

    fn search(&self, egraph: &EGraph<GrammarENode, A>) -> Vec<(Id, Self::Match)> {
        let mut matches = Vec::new();
        for class in egraph.classes() {
            for node in class.iter() {
                let GrammarENode::Alt(children, _dispatch) = node else {
                    continue;
                };
                if children.len() != 2 {
                    continue;
                }
                let a = egraph.find_ref(children[0]);
                let b = egraph.find_ref(children[1]);
                let leaf = match (
                    self.class_has_epsilon(egraph, a),
                    self.class_has_epsilon(egraph, b),
                ) {
                    (false, true) => a,
                    (true, false) => b,
                    // Both-epsilon is degenerate (G1-dedup territory);
                    // neither-epsilon is out of scope for G3.
                    _ => continue,
                };
                if !self.branch_is_scalar(egraph, leaf) {
                    continue;
                }
                matches.push((class.id, WrapOfEpsilonMatch { leaf }));
                break;
            }
        }
        matches
    }

    fn apply(&self, egraph: &mut EGraph<GrammarENode, A>, class_id: Id, m: Self::Match) {
        egraph.union(class_id, m.leaf);
    }
}
