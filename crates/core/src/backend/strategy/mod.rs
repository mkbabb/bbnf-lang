//! Strategy domain — per-node decision types consumed by the driver.
//!
//! Each node kind has its own strategy enum capturing the backend-agnostic
//! decision of *what* to emit (as opposed to *how*, which the per-target
//! emitters handle). Pre-solving strategies at `prepare_grammar` time lets
//! the driver dispatch on the resolved form instead of re-detecting
//! patterns inline.
//!
//! Strategy classifiers take `NodeFacts` + `ContextFacts` + cost model as
//! input and produce a strategy per node. Consumers read the strategy via
//! `DriverState`.
//!
//! Isomorphic with the regex pipeline's `RegexStrategy`/`RegexTier`
//! — both pipelines share the same facts-to-strategy-to-emission shape.

pub mod alt_strategy;
pub mod repeat_strategy;
pub mod ref_strategy;
pub mod seq_strategy;
pub mod wrap_strategy;

pub use alt_strategy::AltStrategy;
pub use repeat_strategy::RepeatStrategy;
pub use ref_strategy::RefStrategy;
pub use seq_strategy::SeqStrategy;
pub use wrap_strategy::WrapStrategy;

/// Unified per-node strategy enum spanning all node kinds.
///
/// Produced by `solve_node_strategies` and stored keyed by `NodeId` (or
/// pointer, during the transition). Consumers dispatch on the variant.
#[derive(Clone, Debug)]
pub enum NodeStrategy {
    Alt(AltStrategy),
    Seq(SeqStrategy),
    Repeat(RepeatStrategy),
    Ref(RefStrategy),
    Wrap(WrapStrategy),
    /// Leaf node (Literal/Regex/Epsilon) — no strategy needed.
    Leaf,
}
