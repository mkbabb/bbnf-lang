//! Structural graph rewrites: inlining, fusing, pruning, alias resolution,
//! epsilon elimination, and literal merging.

mod alias;
mod fuse;
mod fuse_token;
mod inline;
mod optimize;
mod prune;

pub use alias::canonicalize_aliases;
pub use fuse::fuse_single_use;
pub use fuse_token::fuse_token_dispatch;
pub use inline::inline_acyclic;
pub use optimize::{eliminate_epsilon, merge_literals};
pub use prune::prune_unreachable;
