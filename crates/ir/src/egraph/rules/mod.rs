//! Default rewrite rules for grammar e-graph equality saturation.
//!
//! Each rule is a non-destructive equivalence — it adds a new form
//! to an e-class rather than replacing the existing one. Cost-guided
//! extraction (`GrammarCostModel`) picks the preferred form after
//! saturation converges.
//!
//! # Rule retention policy
//!
//! Rules in `default_rules()` justify themselves by one of three
//! criteria (see `docs/four-layer-optimizer.md` / the plan file):
//!
//! 1. **Ordering-independent equivalence**: catches rewrites the
//!    structural normalizer's fixed pass order can miss, by matching
//!    on e-class canonical identity rather than tree shape.
//! 2. **Cost-guided canonical form selection**: multiple equivalent
//!    shapes exist in the class and extraction chooses the cheapest
//!    per the shared cost model.
//! 3. **Regex algebra family**: peephole rewrites that are
//!    equality-saturation-native (superset absorption, charclass
//!    union, literal/regex fusion, branch dedup).
//!
//! The epsilon/singleton/literal-merging/prefix-factoring/inline
//! families were pruned — the structural normalizer covers their
//! territory with the iterative cross-rule cascading (inline→merge
//! →factor→inline) that one-pass saturation cannot express.

mod regex;
mod suffix;
mod universal;

pub use regex::{DeduplicateAltBranches, FuseAltRegexBranches, SupersetAbsorbAlt, UnionMergeAlt};
pub use suffix::CommonSuffixFactor;
pub use universal::{AltOfSingle, ConcatLiterals, RepeatOfSingle, WrapOfEpsilonScalar};

use rustc_hash::FxHashMap;

use parse_that_egraph::{Analysis, Id, RewriteFn};

use super::interner::SharedStrings;
use super::node::GrammarENode;
use crate::{GrammarIR, RuleId, TypeDesc};

/// Default rule set.
///
/// Regex-algebra + structural equivalences that the structural
/// normalizer cannot express in its fixed pass order, plus the
/// AY.W2.3 universal canonicalisations (G1-G2 so far).
///
/// Regex-algebra family:
/// - [`DeduplicateAltBranches`] — collapse structurally-equal branches
///   visible only after e-class canonicalization.
/// - [`SupersetAbsorbAlt`] — byte-set subset absorption in regex
///   alternations.
/// - [`UnionMergeAlt`] — charclass union merging.
/// - [`FuseAltRegexBranches`] — mixed `Alt([Regex, Literal, Regex])`
///   fusion into a single combined pattern, selected by cost when
///   dispatch tables aren't available.
///
/// Structural family:
/// - [`CommonSuffixFactor`] — dual of the normalizer's prefix-factoring
///   pass. Lifts shared trailing sub-expressions out of Alt branches:
///   `Alt([Seq([A, x]), Seq([B, x])]) ≡ Seq([Alt([A, B]), x])`.
///   (Tranche Y.11)
///
/// Universal family (AY.W2.3):
/// - [`AltOfSingle`] — `Alt([x]) ≡ x` (G1).
/// - [`RepeatOfSingle`] — `Repeat { lo: 1, hi: 1 } ≡ inner` (G2).
/// - [`WrapOfEpsilonScalar`] — `Alt([leaf, ε]) ≡ leaf` when `leaf`'s
///   projection is scalar (G3; **PRIMARY LEVER** per AY.md prop 2 for
///   JSON wrap-compound elision).
/// - [`ConcatLiterals`] — `Seq([.., Literal(a), Literal(b), ..])
///   ≡ Seq([.., Literal(ab), ..])` (G4).
///
/// The list depends on the backend's regex leaf ([`LeafMatch`]):
/// [`FuseAltRegexBranches`] is in it only for a leftmost-first leaf.
pub fn default_rules<A: Analysis<GrammarENode> + 'static>(
    ir: &GrammarIR,
    pool: &SharedStrings,
    _rule_body_ids: FxHashMap<RuleId, Id>,
    leaf: LeafMatch,
) -> Vec<Box<dyn RewriteFn<GrammarENode, A>>> {
    // AY.W2.3 G3 — snapshot the per-rule `TypeDesc` side-table so the
    // rewrite's search path can resolve a `Ref` branch's projection
    // without holding a live IR borrow.
    let rule_types: FxHashMap<RuleId, TypeDesc> = ir
        .types
        .iter()
        .map(|(rid, ty)| (*rid, ty.clone()))
        .collect();
    let mut rules: Vec<Box<dyn RewriteFn<GrammarENode, A>>> = vec![
        Box::new(DeduplicateAltBranches),
        Box::new(SupersetAbsorbAlt::new(pool.clone())),
        Box::new(UnionMergeAlt::new(pool.clone())),
    ];
    // `Alt(/a/, /ab/)` and `/a|ab/` are the same parse only under a
    // leftmost-first leaf, which the grammar's ordered choice is. A
    // leftmost-longest leaf reads the fused `/a|ab/` on "ab" as 2 bytes
    // where the ordered choice reads 1, so the rule is not in that list.
    // It returns for the DFA leaf with the leftmost-first priority cut and
    // its `L_cut` certificate (sk-v27 LW3.RX-1).
    if leaf == LeafMatch::LeftmostFirst {
        rules.push(Box::new(FuseAltRegexBranches::new(pool.clone())));
    }
    rules.extend([
        Box::new(CommonSuffixFactor) as Box<dyn RewriteFn<GrammarENode, A>>,
        Box::new(AltOfSingle),
        Box::new(RepeatOfSingle),
        Box::new(WrapOfEpsilonScalar::new(rule_types)),
        Box::new(ConcatLiterals::new(pool.clone())),
    ]);
    rules
}

/// How a backend's regex leaf resolves an alternation: it decides which
/// grammar-tier regex rewrites preserve the parse.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LeafMatch {
    /// ECMAScript `RegExp` (the TS backend): the first alternative that
    /// matches wins, as in the grammar's ordered choice.
    LeftmostFirst,
    /// parse-that-regex's DFA (the Rust, VM and WASM backends): the longest
    /// match wins.
    LeftmostLongest,
}
