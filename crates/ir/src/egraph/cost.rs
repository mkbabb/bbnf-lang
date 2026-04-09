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

use egraph::{CostModel, CostWeights, Id};

use super::node::GrammarENode;

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
}

impl Default for GrammarCostModel {
    fn default() -> Self {
        Self {
            weights: CostWeights::default(),
            literal_cost: 1.0,
            regex_cost: 2.0,
            // `Ref` is cheap: the structural normalizer runs first
            // and has already inlined the acyclic/small/single-use
            // rules the extractor would otherwise try to unfold.
            // After normalizer convergence, any surviving `Ref` is
            // deliberately load-bearing (cyclic, shared, or
            // identity-preserving) and its indirection is the
            // desired form.
            ref_cost: 0.5,
            seq_per_child: 1.0,
        }
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
            GrammarENode::Map { inner, .. } => structural + child_cost(*inner),
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
