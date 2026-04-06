//! Grammar analysis passes: SCC detection, FIRST sets, conflict detection, and more.

pub mod charset;
pub mod deps;
pub mod dispatch;
pub mod first_sets;
mod metadata;
pub mod scc;

pub use charset::*;
pub use deps::*;
pub use dispatch::FirstSetConflict;
pub use dispatch::find_first_set_conflicts;
pub use first_sets::*;
pub use metadata::{compute_ref_counts, find_aliases};
pub use scc::*;

/// Extract the set of possible first bytes from a regex pattern.
///
/// Delegates to `bbnf_ir::regex_first::regex_first_chars` and converts the
/// result from `CharSet128` (`[u64; 2]`) to `CharSet` (`[u32; 4]`).
pub fn regex_first_chars(pattern: &str) -> Option<CharSet> {
    let cs128 = bbnf_ir::regex_first::regex_first_chars(pattern)?;
    Some(CharSet {
        bits: cs128.to_u32x4(),
    })
}
