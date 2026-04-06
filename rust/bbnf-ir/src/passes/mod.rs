//! IR transformation passes.
//!
//! Each pass is an independent function `GrammarIR → GrammarIR` (or `&mut GrammarIR`).
//! Passes can be composed in any order (though some orderings are more efficient).

pub mod lr;
pub mod metadata;
pub mod prefix;
pub mod regex;
pub mod sets;
pub mod span;
pub mod transform;
pub mod types;

pub use lr::{eliminate_direct_lr, eliminate_indirect_lr};
pub use metadata::{compute_aliases, compute_transparent};
pub use prefix::factor_common_prefixes;
pub use regex::{merge_regex_alts, simplify_regex_algebra};
pub use sets::{
    compute_first_sets, compute_follow_sets, compute_rule_deps, compute_scc,
    factor_regex_with_lookahead, generate_dispatch_tables, sort_alt_branches,
};
pub use span::{compute_sp_method_rules, refine_span_eligibility};
pub use transform::{
    canonicalize_aliases, eliminate_epsilon, fuse_single_use, fuse_token_dispatch, inline_acyclic,
    merge_literals, prune_unreachable,
};
pub use types::project_types;
pub use types::{TypeMap, try_flatten_pair};
