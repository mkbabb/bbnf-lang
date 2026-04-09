//! Context facts — propagated structural information about each node's
//! role within its containing rule/grammar.
//!
//! Unlike `NodeFacts` (which answers "what shape is this node?"),
//! `ContextFacts` answers "what does this node mean in context?". The
//! facts are computed via monotone fixed-point propagation over the
//! grammar DAG.
//!
//! Phase 5 introduces the module scaffolding with a minimal fact set.
//! Future phases extend it with discrimination strength, scan safety,
//! and recovery-friendliness propagation.

mod facts;
mod propagate;

pub use facts::{ContextFacts, ContextFactsMap, DiscriminationStrength, ScanSafety};
pub use propagate::compute_context_facts;
