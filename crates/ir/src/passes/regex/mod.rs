//! Regex-level IR transforms: algebraic rewrite rules and
//! alternation merging at the grammar-IR layer.
//!
//! Part of the structural normalizer (primary cross-rule
//! optimizer). These passes operate on IR graph nodes containing
//! regex pattern strings — they do NOT parse or compile regexes;
//! they manipulate IR structure.
//!
//! The grammar e-graph has complementary regex-algebra rewrites
//! in `crate::egraph::rules::regex` — dedup, superset absorption,
//! charclass union, and alt fusion — which run after normalizer
//! convergence and catch equivalences the destructive passes'
//! fixed order misses. Deeper HIR-level saturation (repetition
//! absorption, dead-anchor elimination, NFA-vs-DFA shape
//! selection) lives in the bbnf-regex HIR e-graph (Tranche H)
//! and shares the same `GrammarCostModel`.

pub mod algebra;
pub mod merge;

pub use algebra::simplify_regex_algebra;
pub use merge::merge_regex_alts;
