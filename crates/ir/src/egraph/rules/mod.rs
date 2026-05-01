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

use egraph::{Analysis, Id, RewriteFn};

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
pub fn default_rules<A: Analysis<GrammarENode> + 'static>(
    ir: &GrammarIR,
    pool: &SharedStrings,
    _rule_body_ids: FxHashMap<RuleId, Id>,
) -> Vec<Box<dyn RewriteFn<GrammarENode, A>>> {
    // AY.W2.3 G3 — snapshot the per-rule `TypeDesc` side-table so the
    // rewrite's search path can resolve a `Ref` branch's projection
    // without holding a live IR borrow.
    let rule_types: FxHashMap<RuleId, TypeDesc> = ir
        .types
        .iter()
        .map(|(rid, ty)| (*rid, ty.clone()))
        .collect();
    vec![
        Box::new(DeduplicateAltBranches),
        Box::new(SupersetAbsorbAlt::new(pool.clone())),
        Box::new(UnionMergeAlt::new(pool.clone())),
        Box::new(FuseAltRegexBranches::new(pool.clone())),
        Box::new(CommonSuffixFactor),
        Box::new(AltOfSingle),
        Box::new(RepeatOfSingle),
        Box::new(WrapOfEpsilonScalar::new(rule_types)),
        Box::new(ConcatLiterals::new(pool.clone())),
    ]
}
