//! Inline-CSP cost budgets derived from [`egraph::CostWeights`].
//!
//! Tranche AF.2: every numeric threshold the inline CSP consults is a
//! projection of the shared [`CostWeights`]. Per-node estimates use
//! `call_overhead` / `inline_body_size_penalty` as dimensional anchors;
//! CSP thresholds and shape-guard thresholds scale `call_overhead`.
//! At the default [`CostWeights`] values, every projection here
//! reproduces the pre-AF.2 hardcoded constants.

use egraph::CostWeights;

/// Structural ceiling on alternation fan-out. Not a cost weight — a
/// pragmatic gate on dispatch-table complexity that every backend
/// respects regardless of the cost model. Tracked here next to the
/// inline CSP so the two structural gates live together.
pub(crate) const MAX_ALT_BRANCHES: usize = 32;

/// Structural budget derived from the shared [`CostWeights`]. Every
/// threshold the inline CSP consults lives here — the numeric
/// constants that used to be hardcoded in the inline cost-budget
/// constraint and shape-guard predicate are all projections of the same
/// `call_overhead` / `inline_body_size_penalty` dimensions that the
/// CSP strategy solver (`crates/ir/src/passes/csp_strategy/`) reads
/// for its own decisions.
///
/// At [`CostWeights::default`] values these projections reproduce the
/// pre-AF.2 hardcoded constants (`MAX_LOCAL_COST=80`,
/// `MAX_TOTAL_BUDGET=4096`, shape-guard thresholds 48/1024/1536,
/// leaf/ref/alt/repeat per-node costs 2/8/5/10).
#[derive(Clone, Copy, Debug)]
pub(crate) struct CostBudgets {
    // Per-node expansion-cost estimates. The `estimate_expansion_cost`
    // walker turns these into a single `usize` body-size number that
    // the budget thresholds below compare against.
    pub leaf_cost: usize,
    pub ref_cost: usize,
    pub alt_branch_cost: usize,
    pub repeat_cost: usize,
    pub negate_cost: usize,

    // CSP inline-vs-call thresholds.
    pub max_local_cost: usize,
    pub max_total_budget: usize,

    // Shape-guard thresholds for `should_force_direct_call`.
    pub high_ref_local_cost: usize,
    pub wrapper_heavy_total_budget: usize,
    pub control_heavy_total_budget: usize,
}

impl CostBudgets {
    /// Derive every inline-CSP budget from the shared [`CostWeights`].
    ///
    /// The per-node estimates use `call_overhead` / `inline_body_size_penalty`
    /// as dimensional anchors:
    /// - Leaf / Map / OptionalWhitespace: `4 * inline_body_size_penalty`
    ///   (= 2 at default weights — the baseline "tiny structural node" cost).
    /// - `Ref`: `2 * call_overhead` (= 8 at defaults — a direct call
    ///   pays `call_overhead`; inlining a call site is two call-equivalents
    ///   worth of work under the pre-AF.2 cost model).
    /// - `Alt` per-branch: `call_overhead + 2 * inline_body_size_penalty`
    ///   (= 5 at defaults — one call-worth plus a small per-branch structural
    ///   surcharge).
    /// - `Repeat`: `2 * call_overhead + 4 * inline_body_size_penalty`
    ///   (= 10 at defaults — two calls plus loop overhead).
    /// - `Negate`: `0.5 * call_overhead + 2 * inline_body_size_penalty`
    ///   (= 3 at defaults — half a call plus a small structural surcharge).
    ///
    /// The CSP thresholds scale `call_overhead`:
    /// - `max_local_cost = call_overhead * 20` (= 80 — at most ~20
    ///   call-equivalents of inlined body per rule).
    /// - `max_total_budget = call_overhead * 1024` (= 4096 — at most
    ///   ~1024 call-equivalents summed across all inline sites).
    /// - Shape guards: `call_overhead * 12 / 256 / 384` (= 48 / 1024 /
    ///   1536 at defaults).
    pub fn from_weights(w: &CostWeights) -> Self {
        let call = w.call_overhead;
        let body = w.inline_body_size_penalty;

        // Per-node costs (f64 → usize conversion with round-to-nearest
        // via `as usize`, which truncates but is stable for the
        // non-negative weights the CostWeights contract guarantees).
        let leaf_cost = (body * 4.0) as usize;
        let ref_cost = (call * 2.0) as usize;
        let alt_branch_cost = (call + body * 2.0) as usize;
        let repeat_cost = (call * 2.0 + body * 4.0) as usize;
        let negate_cost = (call * 0.5 + body * 2.0) as usize;

        // CSP thresholds.
        let max_local_cost = (call * 20.0) as usize;
        let max_total_budget = (call * 1024.0) as usize;

        // Shape-guard thresholds.
        let high_ref_local_cost = (call * 12.0) as usize;
        let wrapper_heavy_total_budget = (call * 256.0) as usize;
        let control_heavy_total_budget = (call * 384.0) as usize;

        Self {
            leaf_cost,
            ref_cost,
            alt_branch_cost,
            repeat_cost,
            negate_cost,
            max_local_cost,
            max_total_budget,
            high_ref_local_cost,
            wrapper_heavy_total_budget,
            control_heavy_total_budget,
        }
    }
}
