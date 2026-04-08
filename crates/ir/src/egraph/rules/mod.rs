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

pub use regex::{
    DeduplicateAltBranches, FuseAltRegexBranches, SupersetAbsorbAlt, UnionMergeAlt,
};

use std::collections::HashMap;

use egraph::{Id, RewriteFn};

use super::analysis::GrammarAnalysis;
use super::interner::SharedStrings;
use super::node::GrammarENode;
use crate::{GrammarIR, RuleId};

/// Default rule set: regex-algebra equivalences that the structural
/// normalizer cannot express in its fixed pass order.
///
/// - [`DeduplicateAltBranches`] — collapse structurally-equal branches
///   visible only after e-class canonicalization.
/// - [`SupersetAbsorbAlt`] — byte-set subset absorption in regex
///   alternations.
/// - [`UnionMergeAlt`] — charclass union merging.
/// - [`FuseAltRegexBranches`] — mixed `Alt([Regex, Literal, Regex])`
///   fusion into a single combined pattern, selected by cost when
///   dispatch tables aren't available.
pub fn default_rules(
    _ir: &GrammarIR,
    pool: &SharedStrings,
    _rule_body_ids: HashMap<RuleId, Id>,
) -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(DeduplicateAltBranches),
        Box::new(SupersetAbsorbAlt::new(pool.clone())),
        Box::new(UnionMergeAlt::new(pool.clone())),
        Box::new(FuseAltRegexBranches::new(pool.clone())),
    ]
}
