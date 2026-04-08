//! Per-class lattice analysis for the grammar e-graph.
//!
//! Tracks light structural facts needed by rewrite rules and extraction:
//! node count (used by the AstSize cost model) and nullability.

use egraph::{Analysis, EGraph, Id};

use super::node::GrammarENode;

/// Per-e-class data: monotone lattice values refined during saturation.
#[derive(Clone, Debug, Default)]
pub struct ClassData {
    /// Minimum node count among all representatives of this class.
    pub min_size: u32,
    /// Whether the class contains a node that matches the empty string.
    pub nullable: Option<bool>,
}

/// Grammar analysis — implements `egraph::Analysis<GrammarENode>`.
#[derive(Clone, Copy, Debug, Default)]
pub struct GrammarAnalysis;

impl Analysis<GrammarENode> for GrammarAnalysis {
    type Data = ClassData;

    fn make(egraph: &EGraph<GrammarENode, Self>, node: &GrammarENode) -> Self::Data {
        let child_size = |id: Id| egraph.class(id).data.min_size as usize;
        let size_of = |children: &[Id]| children.iter().map(|&id| child_size(id)).sum::<usize>();

        let min_size = match node {
            GrammarENode::Literal(_)
            | GrammarENode::Regex(_)
            | GrammarENode::Epsilon
            | GrammarENode::Ref(_) => 1,
            GrammarENode::Seq(children) => 1 + size_of(children),
            GrammarENode::Alt(children, _) => 1 + size_of(children),
            GrammarENode::Repeat { inner, .. }
            | GrammarENode::Negate(inner)
            | GrammarENode::OptionalWhitespace(inner)
            | GrammarENode::Map { inner, .. } => 1 + child_size(*inner),
            GrammarENode::Skip(a, b)
            | GrammarENode::Next(a, b)
            | GrammarENode::Minus(a, b) => 1 + child_size(*a) + child_size(*b),
            GrammarENode::TokenDispatch {
                token,
                arms,
                fallback,
            } => {
                let arms_size: usize = arms.iter().map(|a| child_size(a.continuation)).sum();
                1 + child_size(*token) + child_size(*fallback) + arms_size
            }
        };

        let nullable = match node {
            GrammarENode::Epsilon => Some(true),
            GrammarENode::Literal(_) | GrammarENode::Regex(_) => Some(false),
            GrammarENode::Repeat { lo, .. } => Some(*lo == 0),
            _ => None,
        };

        ClassData {
            min_size: u32::try_from(min_size.min(u32::MAX as usize)).unwrap_or(u32::MAX),
            nullable,
        }
    }

    fn merge(a: &mut Self::Data, b: Self::Data) -> bool {
        let mut changed = false;
        if b.min_size < a.min_size {
            a.min_size = b.min_size;
            changed = true;
        }
        if a.nullable != b.nullable {
            a.nullable = match (a.nullable, b.nullable) {
                (Some(x), Some(y)) if x == y => Some(x),
                (Some(_), Some(_)) => None, // conflicting — treat as unknown
                (Some(x), None) => Some(x),
                (None, Some(y)) => {
                    changed = true;
                    Some(y)
                }
                (None, None) => None,
            };
        }
        changed
    }
}
