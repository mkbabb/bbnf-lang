//! Set-analysis passes: dependency analysis, SCC, FIRST/FOLLOW computation,
//! dispatch table generation, lookahead factoring, and branch sorting.
//!
//! Foundation passes (`compute_scc`, `compute_first_sets`) run first in the
//! pipeline, populating `RuleMeta` fields that all subsequent passes depend on.

pub mod deps;
mod dispatch;
mod factor_lookahead;
pub mod first_sets;
mod follow;
pub mod scc;
mod sort;
pub mod structural_alphabet;

pub use deps::compute_rule_deps;
pub use dispatch::generate_dispatch_tables;
pub use factor_lookahead::factor_regex_with_lookahead;
pub use first_sets::compute_first_sets;
pub use follow::compute_follow_sets;
pub use scc::compute_scc;
pub use sort::sort_alt_branches;
pub use structural_alphabet::{StructuralAlphabet, compute_structural_alphabet};
