//! Shared extraction cost weights for any e-graph consumer.
//!
//! A single source of truth for the cost weights whose meaning is
//! identical across e-graph consumers — per-node structural cost,
//! alternation-branch penalty, and the dispatch bonus that rewards
//! forms enabling constant-time dispatch at emission time.
//!
//! Domain-specific cost models (e.g., `bbnf_ir::egraph::GrammarCostModel`,
//! `bbnf_regex::egraph::RegexExtractionCost`) embed this struct and
//! layer their own per-variant knobs on top. Splitting the shared
//! weights across domains creates drift and is forbidden.
//!
//! ```text
//!   ┌────────────────────────────┐
//!   │  egraph::CostWeights       │  ← shared knobs (structural, alt, dispatch)
//!   └─────────────┬──────────────┘
//!                 │ embedded by
//!   ┌─────────────┼──────────────────────┐
//!   ▼             ▼                      ▼
//!   GrammarCostModel       RegexExtractionCost       (future tiers…)
//!   + literal_cost         + literal_per_byte
//!   + regex_cost           + class_cost
//!   + ref_cost             + repeat_cost
//!   + seq_per_child        + merged_bonus
//! ```
//!
//! Both grammar-tier and regex-HIR-tier extraction use the *same*
//! `weights.alt_per_branch`, `weights.dispatch_bonus`, and
//! `weights.structural` for decisions that belong at the structural
//! layer (branch factoring, dispatch eligibility, overall node
//! count). Domain-specific knobs — things like "how expensive is a
//! single regex literal byte" — stay in the wrapper.

/// Shared extraction cost weights embedded by every domain-specific
/// cost model. Knobs live here only if their meaning is the same for
/// every e-graph consumer.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CostWeights {
    /// Per-node structural cost. Applied to every e-node regardless
    /// of variant — equivalent to `AstSize` when the other knobs
    /// default to zero. Higher values bias extraction toward smaller
    /// trees.
    pub structural: f64,

    /// Per-branch penalty applied to alternation nodes. Higher
    /// values encourage prefix factoring and branch dedup.
    pub alt_per_branch: f64,

    /// Bonus applied to nodes whose shape enables constant-time
    /// dispatch at emission time (grammar-tier: `AltDispatch`
    /// present; regex-tier: literal prefix with fixed continuation
    /// letting the DFA emit a memchr accelerator). Negative values
    /// reward dispatch-eligible forms.
    pub dispatch_bonus: f64,
}

impl Default for CostWeights {
    fn default() -> Self {
        Self {
            structural: 1.0,
            alt_per_branch: 1.5,
            dispatch_bonus: -2.0,
        }
    }
}
