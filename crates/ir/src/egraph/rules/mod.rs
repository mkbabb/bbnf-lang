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
pub use normalize::{
    EliminateEpsilon, EliminateEpsilonInAlt, EliminateEpsilonInRepeat,
    EliminateEpsilonInSkipNext, MergeLiterals, UnwrapSingletonAlt, UnwrapSingletonSeq,
};
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
    // `rule_body_ids` is retained in the signature for future reuse
    // (e.g. a cost model that consults per-rule ref counts). The
    // only rule that previously used it — InlineEligibleRef — is
    // excluded from the default set; see the comment below.
    let _ = rule_body_ids;
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::EliminateEpsilonInAlt),
        Box::new(normalize::EliminateEpsilonInRepeat),
        Box::new(normalize::EliminateEpsilonInSkipNext),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
        Box::new(normalize::MergeLiterals::new(pool.clone())),
        Box::new(DeduplicateAltBranches),
        Box::new(SupersetAbsorbAlt::new(pool.clone())),
        Box::new(UnionMergeAlt::new(pool.clone())),
        Box::new(FuseAltRegexBranches::new(pool.clone())),
        Box::new(FactorSharedSeqPrefix),
        Box::new(FactorLiteralByteTrie::new(pool.clone())),
        // Rule inlining (inline_acyclic + fuse_single_use) is NOT
        // an e-graph rewrite: it's inherently cross-rule — it
        // dissolves a rule boundary and exposes the inlined body as
        // part of the caller. The e-graph cost model doesn't favor
        // inlined forms over cheap Ref indirections, so an
        // InlineEligibleRef rule can't fire productively via
        // extraction. Inlining stays as a post-extraction pass
        // running *after* `write_back_optimized` and *before* the
        // analysis phases.
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
        Box::new(normalize::EliminateEpsilonInAlt),
        Box::new(normalize::EliminateEpsilonInRepeat),
        Box::new(normalize::EliminateEpsilonInSkipNext),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
        Box::new(normalize::MergeLiterals::new(pool.clone())),
    ]
}
