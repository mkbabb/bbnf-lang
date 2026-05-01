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

use egraph::{CostModel, CostWeights, Id};

use super::node::GrammarENode;
use crate::FnDescriptor;

/// Cost model for grammar extraction: minimize total node count
/// with a slight penalty for `Alt` branches (encourages prefix
/// factoring) and a reward for nodes already carrying an
/// `AltDispatch` (extraction keeps dispatch-eligible structures
/// intact). Shared weights live in `weights`; grammar-specific
/// knobs layer on top.
#[derive(Clone, Copy, Debug)]
pub struct GrammarCostModel {
    /// Shared cost weights (`structural`, `alt_per_branch`,
    /// `dispatch_bonus`) used by every e-graph consumer.
    pub weights: CostWeights,
    pub literal_cost: f64,
    pub regex_cost: f64,
    pub ref_cost: f64,
    pub seq_per_child: f64,

    /// Pointer to the host function table (`GrammarIR::fns`).
    /// Used for precise cost accounting on Map nodes.
    /// Null when the cost model is constructed without an IR
    /// reference (e.g., `from_config`).
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

    /// Attach the host function table for precise cost computation
    /// on `Map` nodes.
    ///
    /// The caller must ensure the slice outlives the cost model.
    pub fn with_fns(mut self, fns: &[FnDescriptor]) -> Self {
        self.fns_ptr = fns.as_ptr();
        self.fns_len = fns.len();
        self
    }
}

impl CostModel<GrammarENode> for GrammarCostModel {
    type Cost = f64;

    fn cost(&self, node: &GrammarENode, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost {
        let structural = self.weights.structural;

        match node {
            GrammarENode::Literal(_) => self.literal_cost,
            GrammarENode::Regex(_) => self.regex_cost,
            GrammarENode::Epsilon => 0.5,

            GrammarENode::Ref(_) => self.ref_cost,

            GrammarENode::Seq(children) => {
                let child_sum: f64 = children.iter().map(|&id| child_cost(id)).sum();
                structural + self.seq_per_child * children.len() as f64 + child_sum
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

            GrammarENode::Skip([a, b])
            | GrammarENode::Next([a, b])
            | GrammarENode::Minus([a, b]) => structural + child_cost(*a) + child_cost(*b),

            GrammarENode::Negate(inner) | GrammarENode::OptionalWhitespace(inner) => {
                structural + child_cost(*inner)
            }

            // `Map { fn_id, inner }` is a semantic wrapper, not a
            // structural shape — the host fn it carries cannot be
            // recovered by extracting any non-Map equivalent in the
            // same e-class. AZ-III W3c.1 traced this exactly: a class
            // unioned its `Map`-wrapped form with an equivalent bare
            // form, and the cost-cheapest pick was the bare form. The
            // wrapper disappeared; named-color emission never
            // activated at runtime.
            //
            // The fix bonds Map to a hard preserve. Every non-Map
            // e-node carries at least Epsilon's `0.5` of structural
            // mass, so a fixed bonus of `1e6` keeps the wrapper below
            // any sibling that would elide it. `child_cost(*inner)`
            // remains the dominant term so distinct `Map` variants
            // (different fn_ids, different inner classes) stay
            // distinguishable when more than one shares a class. The
            // floor at `f64::MIN / 2.0` keeps repeated nesting
            // numerically safe without saturating to a non-finite
            // cost.
            GrammarENode::Map { inner, fn_id: _ } => {
                const MAP_PRESERVE_BONUS: f64 = 1.0e6;
                (child_cost(*inner) - MAP_PRESERVE_BONUS).max(f64::MIN / 2.0)
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
