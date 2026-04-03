//! Set-analysis passes: FIRST/FOLLOW computation, dispatch table generation,
//! lookahead factoring, and branch sorting.
//!
//! All passes in this module share `CharSet128`, `regex_first`, and rule-level
//! FIRST/nullable metadata. Shared utilities live here in `mod.rs`.

mod dispatch;
mod factor_lookahead;
mod follow;
mod sort;

pub use dispatch::generate_dispatch_tables;
pub use factor_lookahead::factor_regex_with_lookahead;
pub use follow::compute_follow_sets;
pub use sort::sort_alt_branches;
