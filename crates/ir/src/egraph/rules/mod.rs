//! Default rewrite rules for grammar optimization.
//!
//! Each rule is a non-destructive equivalence — it adds a new form to the
//! e-class rather than replacing the existing one. The extraction cost
//! model picks the preferred form at the end.

mod normalize;
mod structural;

pub use structural::{CanonicalizeAlias, build_alias_map};

use egraph::RewriteFn;

use super::analysis::GrammarAnalysis;
use super::node::GrammarENode;
use crate::GrammarIR;

/// Default rule set: normalization (epsilon + singleton unwrap) + grammar-aware
/// structural rewrites (alias canonicalization). Takes `ir` because the
/// grammar-aware rules carry pre-computed metadata snapshots.
pub fn default_rules(
    ir: &GrammarIR,
) -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
        Box::new(CanonicalizeAlias::new(ir)),
    ]
}

/// Subset of `default_rules` with only the grammar-metadata-free rules.
/// Useful for tests that don't need an `ir` but still want normalization.
pub fn normalize_rules() -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
    ]
}
