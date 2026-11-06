//! Structural normalizer — the primary cross-rule optimizer.
//!
//! Destructive tree rewrites iterated to fixed point: alias
//! canonicalization, pruning, inlining, fusing, epsilon elimination,
//! literal merging, and prefix factoring. This layer handles the
//! iterative cross-rule cascading feedback
//! (inline→merge→factor→inline) directly and deterministically.

mod alias;
mod fuse;
mod fuse_token;
mod inline;
mod optimize;
pub mod pattern_dedup;
mod prune;

pub use alias::canonicalize_aliases;
pub use fuse::fuse_single_use;
pub use fuse_token::fuse_token_dispatch;
pub use inline::inline_acyclic;
pub use optimize::{eliminate_epsilon, merge_literals};
pub use pattern_dedup::{
    MAX_PATTERN_NODES, MIN_OCCURRENCES, MIN_PATTERN_NODES, hoist_recurring_patterns,
};
pub use prune::prune_unreachable;
