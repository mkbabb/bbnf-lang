//! IR transformation passes.
//!
//! Each pass is an independent function `GrammarIR → GrammarIR` (or `&mut GrammarIR`).
//! Passes can be composed in any order (though some orderings are more efficient).

pub mod alias;
pub mod dispatch;
pub mod factor_lookahead;
pub mod follow;
pub mod fuse;
pub mod fuse_token;
pub mod inline;
pub mod lr;
pub mod merge_regex;
pub mod regex_algebra;
pub mod metadata;
pub mod optimize;
pub mod prefix;
pub mod prune;
pub mod sort;
pub mod span;
pub mod types;

pub use alias::canonicalize_aliases;
pub use dispatch::generate_dispatch_tables;
pub use factor_lookahead::factor_regex_with_lookahead;
pub use follow::compute_follow_sets;
pub use fuse::fuse_single_use;
pub use fuse_token::fuse_token_dispatch;
pub use inline::inline_acyclic;
pub use lr::{eliminate_direct_lr, eliminate_indirect_lr};
pub use merge_regex::merge_regex_alts;
pub use regex_algebra::simplify_regex_algebra;
pub use metadata::{compute_aliases, compute_transparent};
pub use optimize::{eliminate_epsilon, merge_literals};
pub use prefix::factor_common_prefixes;
pub use prune::prune_unreachable;
pub use sort::sort_alt_branches;
pub use span::{compute_sp_method_rules, refine_span_eligibility};
pub use types::project_types;
pub use types::{ProjectionCtx, ProjectionRules, TypeMap, TypeRecorder, project_node, project_node_in_vec};
