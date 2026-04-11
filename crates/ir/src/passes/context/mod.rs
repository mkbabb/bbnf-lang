//! Context facts — propagated structural information about each node's
//! role within its containing rule/grammar.
//!
//! Unlike `NodeFacts` (which answers "what shape is this node?"),
//! `ContextFacts` answers "what does this node mean in context?".
//!
//! The facts are produced by
//! [`passes::recognizers::ContextFactsMiner`] as part of the unified
//! single-walk recognizer mining pass — see the AF.1 fold notes in
//! `passes::recognizers::mod`. This module owns only the pure type
//! definitions consumed by the miner and by every downstream reader
//! (`ir.context_facts`).

mod facts;

pub use facts::{ContextFacts, ContextFactsMap, DiscriminationStrength, ScanSafety};
