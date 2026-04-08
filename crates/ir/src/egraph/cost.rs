//! Shared cost model for equality-saturation extraction.
//!
//! `GrammarCostModel` is the single source of truth for extraction
//! cost across:
//!
//! - the grammar-level e-graph (this crate)
//! - the bbnf-regex HIR e-graph (Tranche H, via shared import)
//! - CSP/COP strategy solvers that need a cost function
//!
//! Splitting this model creates parallel cost sources that drift
//! out of sync — forbidden. Per-domain solvers pull their objective
//! weights from here so a rewrite's "price" is consistent whether
//! it fires at grammar compile time or inside the regex engine's
//! own saturation pass.

use egraph::{CostModel, Id};

use super::node::GrammarENode;

/// Cost model for grammar extraction: minimize total node count
/// with a slight penalty for `Alt` branches (encourages prefix
/// factoring) and a reward for nodes already carrying an
/// `AltDispatch` (extraction keeps dispatch-eligible structures
/// intact).
#[derive(Clone, Copy, Debug)]
pub struct GrammarCostModel {
    pub literal_cost: f64,
    pub regex_cost: f64,
    pub ref_cost: f64,
    pub seq_per_child: f64,
    pub alt_per_branch: f64,
    pub dispatch_bonus: f64,
}

impl Default for GrammarCostModel {
    fn default() -> Self {
        Self {
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
            alt_per_branch: 1.5,
            // Negative cost: reward dispatch-eligible Alt nodes.
            dispatch_bonus: -2.0,
        }
    }
}

impl CostModel<GrammarENode> for GrammarCostModel {
    type Cost = f64;

    fn cost(&self, node: &GrammarENode, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost {
        match node {
            GrammarENode::Literal(_) => self.literal_cost,
            GrammarENode::Regex(_) => self.regex_cost,
            GrammarENode::Epsilon => 0.5,
            GrammarENode::Ref(_) => self.ref_cost,
            GrammarENode::Seq(children) => {
                let child_sum: f64 = children.iter().map(|&id| child_cost(id)).sum();
                1.0 + self.seq_per_child * children.len() as f64 + child_sum
            }
            GrammarENode::Alt(children, dispatch) => {
                let child_sum: f64 = children.iter().map(|&id| child_cost(id)).sum();
                let base = 1.0 + self.alt_per_branch * children.len() as f64 + child_sum;
                if dispatch.is_some() {
                    base + self.dispatch_bonus
                } else {
                    base
                }
            }
            GrammarENode::Repeat { inner, .. } => 1.0 + child_cost(*inner),
            GrammarENode::Skip([a, b])
            | GrammarENode::Next([a, b])
            | GrammarENode::Minus([a, b]) => 1.0 + child_cost(*a) + child_cost(*b),
            GrammarENode::Negate(inner) | GrammarENode::OptionalWhitespace(inner) => {
                1.0 + child_cost(*inner)
            }
            GrammarENode::Map { inner, .. } => 1.0 + child_cost(*inner),
            GrammarENode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                let arms_sum: f64 = arms.iter().map(|a| child_cost(a.continuation)).sum();
                1.0 + child_cost(*token) + child_cost(*fallback) + arms_sum
            }
        }
    }
}
