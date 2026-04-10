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
//!
//! # Tranche Z.6 — universal strategy cost knob
//!
//! Pre-Z, this struct held four `strategy_*` knobs that were grammar-
//! tier only. Three of them (`strategy_lookahead_penalty`,
//! `strategy_hoist_savings`, `strategy_unroll_bound`) were ghosts —
//! defined and env-var-loaded but never read by any production
//! consumer. The fourth (`strategy_dispatch_bonus`) was a numeric
//! duplicate of the existing cross-tier
//! [`egraph::CostWeights::dispatch_bonus`]: same default value, same
//! semantics, just stored twice.
//!
//! Z.6 deletes all four. The strategy CSP now reads
//! `cfg.egraph.weights.dispatch_bonus` directly — the same field the
//! `RegexExtractionCost` HIR tier already consumes via
//! [`hir_extraction_cost`]. The universal cost model is therefore
//! truly universal: one source of truth at `egraph::CostWeights`,
//! both tiers read it.

use egraph::CostConfig as EgraphCostConfig;

/// Grammar-tier cost / scheduling configuration. Embeds
/// [`EgraphCostConfig`] (the cross-tier substrate) and layers
/// grammar-specific knobs on top.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CostConfig {
    /// Cross-tier extraction weights + scheduler caps shared with the
    /// regex HIR tier. The strategy CSP reads
    /// `egraph.weights.dispatch_bonus` here directly (Z.6).
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
        }
    }
}

impl CostConfig {
    /// Tranche Y.6a: build an authoritative HIR-tier extraction cost
    /// from this config.
    ///
    /// This is the single construction site for
    /// [`bbnf_regex::egraph::RegexExtractionCost`] in production — every
    /// call site that used to construct the struct field-by-field (or
    /// call `RegexExtractionCost::default()`) now goes through this
    /// method. The cross-tier commitment from Tranche H-5 is that the
    /// grammar tier and the HIR tier see identical weights, and that
    /// every HIR knob in `CostConfig.hir_*` flows through to the
    /// extractor. Both halves are honored here.
    ///
    /// Lives on `bbnf_ir::CostConfig` (not `bbnf_regex`) because the
    /// dependency direction is `bbnf-ir → bbnf-regex`, not the reverse.
    /// Attaching the constructor here keeps `bbnf-regex` oblivious to
    /// its grammar-tier consumer.
    pub fn hir_extraction_cost(&self) -> bbnf_regex::egraph::RegexExtractionCost {
        bbnf_regex::egraph::RegexExtractionCost {
            weights: self.egraph.weights,
            literal_per_byte: self.hir_literal_per_byte,
            class_cost: self.hir_class_cost,
            repeat_cost: self.hir_repeat_cost,
            merged_bonus: self.hir_merged_bonus,
        }
    }

    /// Build a `CostConfig` from environment variables.
    ///
    /// Recognized variables (in addition to those handled by
    /// [`EgraphCostConfig::from_env`] — notably
    /// `BBNF_COST_DISPATCH_BONUS`, which now flows directly into
    /// `egraph.weights.dispatch_bonus` after the Z.6 ghost-knob
    /// deletion):
    /// - `BBNF_COST_LITERAL`
    /// - `BBNF_COST_REGEX`
    /// - `BBNF_COST_REF`
    /// - `BBNF_COST_SEQ_PER_CHILD`
    /// - `BBNF_COST_HIR_LITERAL_PER_BYTE`
    /// - `BBNF_COST_HIR_CLASS`
    /// - `BBNF_COST_HIR_REPEAT`
    /// - `BBNF_COST_HIR_MERGED_BONUS`
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

        c
    }
}
