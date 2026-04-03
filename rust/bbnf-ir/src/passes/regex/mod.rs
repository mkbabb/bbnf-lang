//! Regex-level IR transforms: algebraic rewrite rules and alternation merging.
//!
//! These passes operate on IR graph nodes containing regex pattern strings ---
//! they do NOT parse or compile regexes; they manipulate IR structure.

mod algebra;
mod merge;

pub use algebra::simplify_regex_algebra;
pub use merge::merge_regex_alts;
