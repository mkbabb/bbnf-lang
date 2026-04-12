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
/// every e-graph consumer. Tranche AF.2 added the CSP / backend-
/// driver dimensions so every cost computation in the compiler reads
/// from a single source of truth.
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

    // ── Call / inline decision weights (Tranche AF.2) ──────────────
    //
    // These knobs drive the backend driver's inline-vs-call decision
    // and the AF.3–AF.5 follow-ups (cross-rule CSP, per-rule
    // `EmissionTier` decoder, Tier B emitter). Tranche Z.6 already
    // consolidated the CSP strategy solver's `dispatch_bonus` read
    // into the shared `CostWeights`; AF.2 extends the struct with
    // the remaining call / inline / dispatch / tape-push / prettify
    // / cross-module dimensions so every cost computation in the
    // compiler reads from a single source of truth.

    /// Fixed cost of a rule-function call. Applied to every
    /// `CallStrategy::DirectCall` site; does not scale with body
    /// size. Higher values push more call sites toward inlining.
    pub call_overhead: f64,

    /// Per-IR-node cost of inlining a rule body at a call site.
    /// Scales with the body's node count — inlining a 20-node body
    /// pays `20 * inline_body_size_penalty` at the call site versus
    /// a fixed `call_overhead` for calling. Balances body growth
    /// against call overhead.
    pub inline_body_size_penalty: f64,

    /// Cost of a single `TapeBuilder::push_*` invocation. Applied
    /// to every node that materializes to a tape record
    /// (`MustTape` / `TapeSpanOnly`); elided for `TransparentElide`
    /// nodes. Drives the Tier B vs Tier A decision in AF.3–AF.5.
    pub tape_push: f64,

    /// Cost of a `BumpSlab` allocation — a typed scratch allocation
    /// used by the legacy slab emitter path. Mostly zero under
    /// tape-first emission; retained as a dimension so the HIR and
    /// VM tiers that still use slab allocation can share the weight.
    pub slab_alloc: f64,

    /// Per-arm cost of a dispatch-branch (byte-match → jump). The
    /// CSP strategy solver compares `N * dispatch_branch` against
    /// `alt_per_branch * N + dispatch_table` to decide sequential
    /// trial vs. table dispatch.
    pub dispatch_branch: f64,

    /// Fixed setup cost of a dispatch-table (128-entry byte → branch
    /// map). Amortized across the arm count; higher values bias
    /// toward sequential trial on small Alts.
    pub dispatch_table: f64,

    /// Per-node cost of prettify emission (`@pretty`-pinned rules
    /// that run the FmtBuilder path). Prettify-pinned subtrees pay
    /// this cost during CSP cost accounting; used by the
    /// `TierFollowsMaterialization` constraint in AF.3.
    pub prettify_emission: f64,

    /// Cost of coercing a Tier B direct value into a tape record at
    /// a Tape parent → Direct child call boundary. Pre-seed for
    /// AG's module substrate (`@import` boundary coercion is the
    /// same operation); AG consumes this dimension without adding
    /// a new one.
    pub cross_module_coercion: f64,

    /// Bonus applied to e-nodes whose body shape enables Direct
    /// emission (Tier B) — leaves (Literal, Regex, Epsilon), non-
    /// closure Maps, short Seqs, and Skip with a leaf kept side.
    /// Negative values reward Direct-eligible forms so the
    /// extractor prefers them over equivalent but harder-to-emit
    /// alternatives.
    pub emission_tier_bonus: f64,
}

impl Default for CostWeights {
    fn default() -> Self {
        Self {
            structural: 1.0,
            alt_per_branch: 1.5,
            dispatch_bonus: -2.0,

            // AF.2 defaults — calibrated to preserve pre-AF.2
            // behavior at the dimension level. Every consumer that
            // reads these values should see the same decisions it
            // made before the weights were consolidated.
            call_overhead: 4.0,
            inline_body_size_penalty: 0.5,
            tape_push: 1.0,
            slab_alloc: 1.0,
            dispatch_branch: 0.5,
            dispatch_table: 3.0,
            prettify_emission: 2.0,
            cross_module_coercion: 1.5,
            emission_tier_bonus: -1.5,
        }
    }
}
