//! Grammar-tier cost / scheduling configuration.
//!
//! Wraps the shared [`egraph::CostConfig`] with grammar-specific knobs
//! consumed by the grammar-tier extraction cost model
//! (`crate::egraph::GrammarCostModel`) and by the strategy CSP
//! introduced in Tranche W phase 3b
//! (`crate::passes::csp_strategy`).
//!
//! `GrammarIR::cost_config` is the **single per-compile source of
//! truth**. Every cost model, scheduler, and strategy CSP reads from
//! it; no pass calls `Default::default()` on a cost model directly.
//! Per-grammar tunability lands in a future tranche via a `@cost`
//! directive that updates this struct on the parsed `GrammarIR`.

use egraph::CostConfig as EgraphCostConfig;

/// Grammar-tier cost / scheduling configuration. Embeds
/// [`EgraphCostConfig`] (the cross-tier substrate) and layers
/// grammar-specific knobs on top.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CostConfig {
    /// Cross-tier extraction weights + scheduler caps shared with the
    /// regex HIR tier.
    pub egraph: EgraphCostConfig,

    // ── Grammar e-graph cost-model knobs ────────────────────────────
    /// Grammar-tier `Literal` per-node cost.
    pub literal_cost: f64,
    /// Grammar-tier `Regex` per-node cost.
    pub regex_cost: f64,
    /// Grammar-tier `Ref` per-node cost.
    pub ref_cost: f64,
    /// Grammar-tier `Seq` per-child cost surcharge.
    pub seq_per_child: f64,

    // ── HIR e-graph cost-model knobs ────────────────────────────────
    // The grammar pipeline owns the HIR cost as well: `compute_regex_info`
    // builds a `RegexExtractionCost` from these knobs and passes it
    // explicitly to `RegexInfo::analyze_with_cost`.
    /// Per-byte HIR literal cost (lower = reward literal runs).
    pub hir_literal_per_byte: f64,
    /// HIR character class cost.
    pub hir_class_cost: f64,
    /// HIR repetition cost.
    pub hir_repeat_cost: f64,
    /// HIR class-merge bonus (negative = reward).
    pub hir_merged_bonus: f64,

    // ── Strategy CSP weights (consumed in Tranche W phase 3b) ───────
    /// Bonus when an `Alt` resolves to a token-dispatch strategy.
    pub strategy_dispatch_bonus: f64,
    /// Penalty for emission strategies that require a lookahead.
    pub strategy_lookahead_penalty: f64,
    /// Bonus for hoisting shared sub-recognizers across rules.
    pub strategy_hoist_savings: f64,
    /// Bound on inline-unrolling repetitions during strategy synthesis.
    pub strategy_unroll_bound: u32,
}

impl Default for CostConfig {
    fn default() -> Self {
        Self {
            egraph: EgraphCostConfig::default(),
            // Grammar-tier defaults — match the previous
            // `GrammarCostModel::default` field values exactly so the
            // substrate is a no-op semantically.
            literal_cost: 1.0,
            regex_cost: 2.0,
            ref_cost: 0.5,
            seq_per_child: 1.0,
            // HIR-tier defaults — match
            // `RegexExtractionCost::default`.
            hir_literal_per_byte: 0.25,
            hir_class_cost: 1.5,
            hir_repeat_cost: 1.0,
            hir_merged_bonus: -1.0,
            // Strategy CSP defaults — these are seed values for
            // Tranche W phase 3b; the empirical sweep happens after
            // the strategy CSP is wired into the backend.
            strategy_dispatch_bonus: -2.0,
            strategy_lookahead_penalty: 0.5,
            strategy_hoist_savings: 1.0,
            strategy_unroll_bound: 8,
        }
    }
}

impl CostConfig {
    /// Build a `CostConfig` from environment variables.
    ///
    /// Recognized variables (in addition to those handled by
    /// [`EgraphCostConfig::from_env`]):
    /// - `BBNF_COST_LITERAL`
    /// - `BBNF_COST_REGEX`
    /// - `BBNF_COST_REF`
    /// - `BBNF_COST_SEQ_PER_CHILD`
    /// - `BBNF_COST_HIR_LITERAL_PER_BYTE`
    /// - `BBNF_COST_HIR_CLASS`
    /// - `BBNF_COST_HIR_REPEAT`
    /// - `BBNF_COST_HIR_MERGED_BONUS`
    /// - `BBNF_COST_STRATEGY_DISPATCH_BONUS`
    /// - `BBNF_COST_STRATEGY_LOOKAHEAD_PENALTY`
    /// - `BBNF_COST_STRATEGY_HOIST_SAVINGS`
    /// - `BBNF_COST_STRATEGY_UNROLL_BOUND`
    pub fn from_env() -> Self {
        let mut c = Self {
            egraph: EgraphCostConfig::from_env(),
            ..Self::default()
        };

        if let Ok(s) = std::env::var("BBNF_COST_LITERAL") {
            if let Ok(v) = s.parse() {
                c.literal_cost = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_REGEX") {
            if let Ok(v) = s.parse() {
                c.regex_cost = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_REF") {
            if let Ok(v) = s.parse() {
                c.ref_cost = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_SEQ_PER_CHILD") {
            if let Ok(v) = s.parse() {
                c.seq_per_child = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_HIR_LITERAL_PER_BYTE") {
            if let Ok(v) = s.parse() {
                c.hir_literal_per_byte = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_HIR_CLASS") {
            if let Ok(v) = s.parse() {
                c.hir_class_cost = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_HIR_REPEAT") {
            if let Ok(v) = s.parse() {
                c.hir_repeat_cost = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_HIR_MERGED_BONUS") {
            if let Ok(v) = s.parse() {
                c.hir_merged_bonus = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_STRATEGY_DISPATCH_BONUS") {
            if let Ok(v) = s.parse() {
                c.strategy_dispatch_bonus = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_STRATEGY_LOOKAHEAD_PENALTY") {
            if let Ok(v) = s.parse() {
                c.strategy_lookahead_penalty = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_STRATEGY_HOIST_SAVINGS") {
            if let Ok(v) = s.parse() {
                c.strategy_hoist_savings = v;
            }
        }
        if let Ok(s) = std::env::var("BBNF_COST_STRATEGY_UNROLL_BOUND") {
            if let Ok(v) = s.parse() {
                c.strategy_unroll_bound = v;
            }
        }

        c
    }
}
