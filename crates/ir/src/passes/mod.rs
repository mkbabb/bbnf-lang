//! IR transformation passes.
//!
//! Each pass is an independent function `GrammarIR → GrammarIR` (or `&mut GrammarIR`).
//! Passes can be composed in any order (though some orderings are more efficient).

pub mod context;
pub mod csp_domains;
pub mod csp_strategy;
pub mod lr;
pub mod materialization;
pub mod metadata;
pub mod patterns;
pub mod prefix;
pub mod recognizers;
pub mod regex_info;
pub mod sets;
pub mod span;
pub mod transform;
pub mod types;

// NOTE: The `regex` submodule (simplify_regex_algebra + merge_regex_alts)
// was deleted in Tranche H-7. Every rewrite it performed is now handled
// by the retained grammar-tier e-graph rules
// (DeduplicateAltBranches, SupersetAbsorbAlt, UnionMergeAlt,
// FuseAltRegexBranches in crate::egraph::rules::regex) plus the HIR
// e-graph saturation in bbnf-regex. See commit message for details.

pub use lr::{eliminate_direct_lr, eliminate_indirect_lr};
pub use materialization::{MaterializationClass, classify_materialization, mat_join};
pub use metadata::{compute_aliases, compute_transparent};
pub use prefix::factor_common_prefixes;
pub use sets::{
    compute_first_sets, compute_follow_sets, compute_rule_deps, compute_scc,
    factor_regex_with_lookahead, generate_dispatch_tables, sort_alt_branches,
};
pub use span::{compute_sp_method_rules, refine_span_eligibility};
pub use transform::{
    canonicalize_aliases, eliminate_epsilon, fuse_single_use, fuse_token_dispatch, inline_acyclic,
    merge_literals, prune_unreachable,
};
pub use csp_strategy::{
    extract_regex_engine_decisions, solve_strategy_decisions, AltMode, RecognizerDecision,
    RecognizerDecisionMap, RegexEngine, StrategyDomain, StrategyValue, WrapMode,
};
pub use recognizers::mine_recognizers;
pub use regex_info::compute_regex_info;
pub use types::project_types;
pub use types::{TypeMap, try_flatten_pair};
