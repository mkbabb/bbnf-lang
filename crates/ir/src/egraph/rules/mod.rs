//! Default rewrite rules for grammar optimization.
//!
//! Each rule is a non-destructive equivalence — it adds a new form to the
//! e-class rather than replacing the existing one. The extraction cost
//! model picks the preferred form at the end.

mod normalize;

use egraph::RewriteFn;

use super::analysis::GrammarAnalysis;
use super::node::GrammarENode;

/// Default rule set: epsilon elimination, singleton unwrap, literal merge.
pub fn default_rules() -> Vec<Box<dyn RewriteFn<GrammarENode, GrammarAnalysis>>> {
    vec![
        Box::new(normalize::EliminateEpsilon),
        Box::new(normalize::UnwrapSingletonSeq),
        Box::new(normalize::UnwrapSingletonAlt),
    ]
}
