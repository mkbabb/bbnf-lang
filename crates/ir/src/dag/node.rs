//! `NodeId` and `DagNode` — the canonical interned node type.

use crate::{AltDispatch, FnId, RuleId, StringId};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Stable, serializable identifier for a node in `GrammarDag`.
///
/// Indexes into `GrammarDag::nodes`. Survives serialization, consistent
/// across pipeline runs on the same grammar, and suitable as a map key.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct NodeId(pub u32);

impl NodeId {
    /// Sentinel value for "no node" / "unassigned".
    pub const INVALID: Self = Self(u32::MAX);

    /// Construct from a raw u32.
    pub const fn new(n: u32) -> Self {
        Self(n)
    }

    /// Construct from a usize. Debug-asserts the value fits in u32.
    #[inline]
    pub fn from_usize(n: usize) -> Self {
        debug_assert!(n <= u32::MAX as usize);
        Self(n as u32)
    }

    /// Raw u32 value.
    #[inline]
    pub fn index(self) -> u32 {
        self.0
    }

    /// Convert to usize for indexing `Vec<T>`.
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

/// A single interned node in the grammar DAG.
///
/// Mirrors `IrNode` variant-for-variant, but recursive positions carry
/// `NodeId` instead of `Box<IrNode>` / `Vec<IrNode>`. This makes the DAG
/// flat, hash-consed, and serializable.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub enum DagNode {
    // ── Leaves ──────────────────────────────────────────────────────────
    Literal(StringId),
    Regex(StringId),
    Epsilon,

    // ── N-ary combinators ───────────────────────────────────────────────
    Seq(Vec<NodeId>),
    Alt(Vec<NodeId>, Option<AltDispatch>),

    Repeat {
        inner: NodeId,
        lo: u32,
        hi: u32,
    },

    Ref(RuleId),

    // ── Binary ──────────────────────────────────────────────────────────
    Skip(NodeId, NodeId),
    Next(NodeId, NodeId),
    Minus(NodeId, NodeId),

    // ── Lookahead ───────────────────────────────────────────────────────
    Negate(NodeId),

    // ── Host integration ────────────────────────────────────────────────
    Map {
        inner: NodeId,
        fn_id: FnId,
    },

    // ── Whitespace ──────────────────────────────────────────────────────
    OptionalWhitespace(NodeId),

    // ── Lexer-Parser Fusion ────────────────────────────────────────────
    TokenDispatch {
        token: NodeId,
        arms: Vec<DagTokenDispatchArm>,
        fallback: NodeId,
    },
}

/// `TokenDispatchArm` projected into DAG form.
#[derive(Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct DagTokenDispatchArm {
    pub patterns: Vec<StringId>,
    pub guard_byte: Option<u8>,
    pub continuation: NodeId,
    pub map_fn: Option<FnId>,
}

impl DagNode {
    /// Iterate over the direct child `NodeId`s of this node.
    pub fn children(&self) -> ChildIter<'_> {
        ChildIter::new(self)
    }

    /// Whether this node is a leaf (no children).
    pub fn is_leaf(&self) -> bool {
        matches!(
            self,
            DagNode::Literal(_) | DagNode::Regex(_) | DagNode::Epsilon | DagNode::Ref(_)
        )
    }
}

/// Iterator over the direct `NodeId` children of a `DagNode`.
pub struct ChildIter<'a> {
    node: &'a DagNode,
    pos: usize,
}

impl<'a> ChildIter<'a> {
    fn new(node: &'a DagNode) -> Self {
        Self { node, pos: 0 }
    }
}

impl Iterator for ChildIter<'_> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let result = match self.node {
            DagNode::Literal(_) | DagNode::Regex(_) | DagNode::Epsilon | DagNode::Ref(_) => None,
            DagNode::Seq(children) | DagNode::Alt(children, _) => children.get(self.pos).copied(),
            DagNode::Repeat { inner, .. }
            | DagNode::Negate(inner)
            | DagNode::OptionalWhitespace(inner)
            | DagNode::Map { inner, .. } => {
                if self.pos == 0 {
                    Some(*inner)
                } else {
                    None
                }
            }
            DagNode::Skip(a, b) | DagNode::Next(a, b) | DagNode::Minus(a, b) => match self.pos {
                0 => Some(*a),
                1 => Some(*b),
                _ => None,
            },
            DagNode::TokenDispatch {
                token,
                arms,
                fallback,
            } => match self.pos {
                0 => Some(*token),
                i if (1..=arms.len()).contains(&i) => Some(arms[i - 1].continuation),
                i if i == arms.len() + 1 => Some(*fallback),
                _ => None,
            },
        };
        if result.is_some() {
            self.pos += 1;
        }
        result
    }
}
