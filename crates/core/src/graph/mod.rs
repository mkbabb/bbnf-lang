//! Grammar dependency graph: SCC detection, topological sort, alias detection.

pub mod deps;
mod metadata;
pub mod scc;

pub use deps::*;
pub use metadata::find_aliases;
pub use scc::*;
