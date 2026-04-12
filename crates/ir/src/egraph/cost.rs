//! Grammar-tier extraction cost model.
//!
//! Embeds the shared `egraph::CostWeights` substrate (per-node
//! structural cost, alt-branch penalty, dispatch bonus) and layers
//! grammar-specific knobs on top — literal/regex/ref costs plus a
//! per-child Seq penalty. The shared weights are the single source
//! of truth for decisions that belong at the structural layer; the
//! regex-tier `RegexExtractionCost` (bbnf-regex, Tranche H) embeds
//! the same `CostWeights` so dispatch bonuses and alt penalties
//! stay consistent across tiers. Per-domain knobs do not leak.
//!
//! Tranche AI.3 adds an **emission-tier bonus** that rewards body
//! shapes enabling Direct (Tier B) emission — leaves, non-closure
//! Maps, short Seqs, and Skip with a leaf kept side. The bonus is
//! additive (`weights.emission_tier_bonus`, negative = reward) and
//! steers the extractor toward forms the backend can emit without
//! tape/slab overhead.

use egraph::{CostModel, CostWeights, Id};

use super::node::GrammarENode;
use crate::FnDescriptor;

/// Cost model for grammar extraction: minimize total node count
/// with a slight penalty for `Alt` branches (encourages prefix
/// factoring) and a reward for nodes already carrying an
/// `AltDispatch` (extraction keeps dispatch-eligible structures
/// intact). Shared weights live in `weights`; grammar-specific
/// knobs layer on top. The emission-tier bonus (Tranche AI.3)
/// additionally rewards Direct-eligible body shapes.
#[derive(Clone, Copy, Debug)]
pub struct GrammarCostModel {
    /// Shared cost weights (`structural`, `alt_per_branch`,
    /// `dispatch_bonus`, `emission_tier_bonus`) used by every
    /// e-graph consumer.
    pub weights: CostWeights,
    pub literal_cost: f64,
    pub regex_cost: f64,
    pub ref_cost: f64,
    pub seq_per_child: f64,

    /// Pointer to the host function table (`GrammarIR::fns`).
    /// Used by the emission-tier bonus to distinguish non-closure
    /// Maps (Direct-eligible) from `FnDescriptor::Expr` closures.
    /// Null when the cost model is constructed without an IR
    /// reference (e.g., `from_config`); in that case the Map bonus
    /// is conservatively skipped.
    fns_ptr: *const FnDescriptor,
    fns_len: usize,
}

impl Default for GrammarCostModel {
    fn default() -> Self {
        Self::from_config(&crate::CostConfig::default())
    }
}

// Safety: the raw `fns_ptr` points into `GrammarIR::fns` which is
// immutable for the lifetime of the cost model. The cost model is
// only used during extraction which borrows the IR immutably.
unsafe impl Send for GrammarCostModel {}
unsafe impl Sync for GrammarCostModel {}

impl GrammarCostModel {
    /// Build a `GrammarCostModel` from the per-compile
    /// [`crate::CostConfig`]. This is the gestalt entry point — every
    /// production call site reads from `ir.cost_config` instead of
    /// calling `Default::default` directly. The fns table is not
    /// available here; call [`with_fns`] to attach it for full
    /// emission-tier bonus precision on Map nodes.
    pub fn from_config(cfg: &crate::CostConfig) -> Self {
        Self {
            weights: cfg.egraph.weights,
            literal_cost: cfg.literal_cost,
            regex_cost: cfg.regex_cost,
            // `Ref` is cheap: the structural normalizer runs first
            // and has already inlined the acyclic/small/single-use
            // rules the extractor would otherwise try to unfold.
            // After normalizer convergence, any surviving `Ref` is
            // deliberately load-bearing (cyclic, shared, or
            // identity-preserving) and its indirection is the
            // desired form.
            ref_cost: cfg.ref_cost,
            seq_per_child: cfg.seq_per_child,
            fns_ptr: std::ptr::null(),
            fns_len: 0,
        }
    }

    /// Attach the host function table for precise emission-tier
    /// bonus computation on `Map` nodes. The returned cost model
    /// distinguishes non-closure Maps (Direct-eligible, bonus
    /// applied) from `FnDescriptor::Expr` closures (no bonus).
    ///
    /// The caller must ensure the slice outlives the cost model.
    pub fn with_fns(mut self, fns: &[FnDescriptor]) -> Self {
        self.fns_ptr = fns.as_ptr();
        self.fns_len = fns.len();
        self
    }

    /// Recover the fns slice from the stored raw pointer + length.
    /// Returns an empty slice when constructed without [`with_fns`].
    fn fns(&self) -> &[FnDescriptor] {
        if self.fns_ptr.is_null() {
            &[]
        } else {
            // Safety: `with_fns` stores a valid pointer+length from
            // a borrowed slice that outlives this cost model.
            unsafe { std::slice::from_raw_parts(self.fns_ptr, self.fns_len) }
        }
    }
}

impl CostModel<GrammarENode> for GrammarCostModel {
    type Cost = f64;

    fn cost(&self, node: &GrammarENode, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost {
        let structural = self.weights.structural;
        let tier_bonus = self.weights.emission_tier_bonus;

        match node {
            // ── Direct-eligible leaves: full emission-tier bonus ────
            GrammarENode::Literal(_) => self.literal_cost + tier_bonus,
            GrammarENode::Regex(_) => self.regex_cost + tier_bonus,
            GrammarENode::Epsilon => 0.5 + tier_bonus,

            GrammarENode::Ref(_) => self.ref_cost,

            // ── Short Seqs (<=3 children): discounted bonus ────────
            GrammarENode::Seq(children) => {
                let child_sum: f64 = children.iter().map(|&id| child_cost(id)).sum();
                let base = structural + self.seq_per_child * children.len() as f64 + child_sum;
                if children.len() <= 3 {
                    base + tier_bonus * 0.5
                } else {
                    base
                }
            }

            GrammarENode::Alt(children, dispatch) => {
                let child_sum: f64 = children.iter().map(|&id| child_cost(id)).sum();
                let base =
                    structural + self.weights.alt_per_branch * children.len() as f64 + child_sum;
                if dispatch.is_some() {
                    base + self.weights.dispatch_bonus
                } else {
                    base
                }
            }

            GrammarENode::Repeat { inner, .. } => structural + child_cost(*inner),

            // ── Skip: Direct-eligible (leaf kept side) ─────────────
            GrammarENode::Skip([a, b]) => {
                structural + child_cost(*a) + child_cost(*b) + tier_bonus
            }
            GrammarENode::Next([a, b])
            | GrammarENode::Minus([a, b]) => structural + child_cost(*a) + child_cost(*b),

            GrammarENode::Negate(inner) | GrammarENode::OptionalWhitespace(inner) => {
                structural + child_cost(*inner)
            }

            // ── Map: bonus for non-closure (non-Expr) descriptors ──
            GrammarENode::Map { inner, fn_id } => {
                let base = structural + child_cost(*inner);
                let fns = self.fns();
                if !fns.is_empty() {
                    // Precise path: look up the FnDescriptor to
                    // distinguish compiler-internal variants
                    // (EnumWrap, BoxWrap, NumberConvert, etc.) from
                    // user-facing Expr closures.
                    match fns.get(*fn_id as usize) {
                        Some(FnDescriptor::Expr { .. }) => base,
                        _ => base + tier_bonus,
                    }
                } else {
                    // No fns table available — conservatively skip
                    // the bonus rather than misclassify.
                    base
                }
            }

            GrammarENode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                let arms_sum: f64 = arms.iter().map(|a| child_cost(a.continuation)).sum();
                structural + child_cost(*token) + child_cost(*fallback) + arms_sum
            }
        }
    }
}
