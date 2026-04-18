//! Structural normalizer — the primary cross-rule optimizer.
//!
//! Destructive tree rewrites iterated to fixed point: alias
//! canonicalization, pruning, inlining, fusing, epsilon elimination,
//! literal merging, and prefix factoring. This layer handles the
//! iterative cross-rule cascading feedback
//! (inline→merge→factor→inline) that equality saturation
//! architecturally cannot express in a single pass.
//!
//! The grammar e-graph (`crate::egraph`) runs once **after**
//! normalizer convergence as a permanent secondary equivalence
//! discoverer, catching ordering-independent rewrites and regex
//! algebra the normalizer's fixed pass order can miss. The two
//! layers are genuinely complementary: the normalizer does the
//! iterative cross-rule work; the e-graph does cost-guided
//! canonical-form selection on the normalized IR.

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
pub use pattern_dedup::{hoist_recurring_patterns, MAX_PATTERN_NODES, MIN_OCCURRENCES, MIN_PATTERN_NODES};
pub use prune::prune_unreachable;
