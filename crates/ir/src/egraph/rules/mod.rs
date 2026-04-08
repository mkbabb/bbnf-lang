//! Default rewrite rules for grammar optimization.
//!
//! Each rule is a non-destructive equivalence — it adds a new form to the
//! e-class rather than replacing the existing one. The extraction cost
//! model picks the preferred form at the end.

mod inline;
mod normalize;
mod prefix;
mod regex;
mod structural;

pub use inline::{INLINE_SIZE_THRESHOLD, InlineEligibleRef};
pub use normalize::MergeLiterals;
pub use prefix::{FactorLiteralByteTrie, FactorSharedSeqPrefix};
pub use regex::{
    DeduplicateAltBranches, FuseAltRegexBranches, SupersetAbsorbAlt, UnionMergeAlt,
};
pub use structural::{CanonicalizeAlias, build_alias_map};

use std::collections::HashMap;

use egraph::{Id, RewriteFn};

use super::analysis::GrammarAnalysis;
use super::interner::SharedStrings;
use super::node::GrammarENode;
use crate::{GrammarIR, RuleId};

/// Default rule set: normalization (epsilon + singleton unwrap + literal
/// merging) + regex algebra (dedupe + superset absorb + union merge +
/// all-regex fusion) + prefix factoring (shared Seq leader + literal
/// byte trie) + ref inlining (acyclic-and-small + single-use) +
/// grammar-aware structural rewrites (alias canonicalization).
///
/// `ir` supplies pre-computed metadata (alias chains, ref counts, SCC);
/// `pool` is the shared string interner used by rules that produce new
/// literals or new regex patterns; `rule_body_ids` maps `RuleId` to
/// the e-graph root of that rule's body (populated by the build phase).
pub fn default_rules(
    ir: &GrammarIR,
    pool: &SharedStrings,
    rule_body_ids: HashMap<RuleId, Id>,
) -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
        Box::new(normalize::MergeLiterals::new(pool.clone())),
        Box::new(DeduplicateAltBranches),
        Box::new(SupersetAbsorbAlt::new(pool.clone())),
        Box::new(UnionMergeAlt::new(pool.clone())),
        Box::new(FuseAltRegexBranches::new(pool.clone())),
        Box::new(FactorSharedSeqPrefix),
        Box::new(FactorLiteralByteTrie::new(pool.clone())),
        Box::new(InlineEligibleRef::new(ir, rule_body_ids)),
        Box::new(CanonicalizeAlias::new(ir)),
    ]
}

/// Subset of `default_rules` with only the grammar-metadata-free rules.
/// Useful for tests that don't need an `ir` but still want normalization.
pub fn normalize_rules(
    pool: &SharedStrings,
) -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
        Box::new(normalize::MergeLiterals::new(pool.clone())),
    ]
}
