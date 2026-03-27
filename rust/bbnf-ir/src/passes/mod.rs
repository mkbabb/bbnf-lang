//! IR transformation passes.
//!
//! Each pass is an independent function `GrammarIR → GrammarIR` (or `&mut GrammarIR`).
//! Passes can be composed in any order (though some orderings are more efficient).

pub mod alias;
pub mod dispatch;
pub mod factor_lookahead;
pub mod follow;
pub mod force_inline;
pub mod fuse_token;
pub mod fuse;
pub mod inline;
pub mod memo;
pub mod merge_regex;
pub mod optimize;
pub mod prefix;
pub mod prune;
pub mod span;
pub mod types;

pub use alias::canonicalize_aliases;
pub use dispatch::generate_dispatch_tables;
pub use factor_lookahead::factor_regex_with_lookahead;
pub use follow::compute_follow_sets;
pub use force_inline::force_inline;
pub use fuse::fuse_single_use;
pub use fuse_token::fuse_token_dispatch;
pub use inline::inline_acyclic;
pub use memo::refine_memo_strategies;
pub use merge_regex::merge_regex_alts;
pub use optimize::{eliminate_epsilon, merge_literals};
pub use prefix::factor_common_prefixes;
pub use prune::prune_unreachable;
pub use span::{refine_span_eligibility, compute_sp_method_rules};
pub use types::infer_types;
pub use types::{infer_node, infer_node_in_vec, InferCtx};
