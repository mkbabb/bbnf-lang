//! Grammar analysis passes: SCC detection, FIRST sets, conflict detection, and more.

pub mod deps;
pub mod scc;
pub mod charset;
pub mod first_sets;
pub mod dispatch;
pub mod metadata;

pub use deps::*;
pub use scc::*;
pub use charset::*;
pub use first_sets::*;
pub use dispatch::find_first_set_conflicts;
pub use dispatch::FirstSetConflict;
pub use metadata::*;

/// Extract the set of possible first bytes from a regex pattern.
///
/// Delegates to `bbnf_ir::regex_first::regex_first_chars` and converts the
/// result from `CharSet128` (`[u64; 2]`) to `CharSet` (`[u32; 4]`).
pub fn regex_first_chars(pattern: &str) -> Option<CharSet> {
    let cs128 = bbnf_ir::regex_first::regex_first_chars(pattern)?;
    Some(CharSet { bits: cs128.to_u32x4() })
}
