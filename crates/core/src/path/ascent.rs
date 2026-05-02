//! Ascent strategies for parent-pointer queries on a parsed-document
//! node graph.
//!
//! Ascent — walking from a child node up to its parent — is the
//! reverse direction of a normal recursive descent. Three impls cover
//! the design space:
//!
//! - [`RootTraversal`] — no per-node bloat, descent cost on every
//!   ascent (re-walks the tree from the root to find the child's
//!   parent).
//! - [`InStructPointer`] — stores a parent `NodeId` on every node
//!   (8 B per node, O(1) ascent — node bloat even when the feature is
//!   unused).
//! - [`HybridSidecar`] — keeps a parallel `Vec<NodeId>` indexed by
//!   depth-first order, populated only when ascent is requested. Default.
//!
//! The trait is the **reversal seam** flagged at the AZ-IV.W2 design
//! lane: alternative impls plug in for a future tranche if benchmark
//! evidence forces inversion. The W2.5 micro-bench at
//! `crates/core/benches/path_ascent.rs` lands the default pick;
//! [`DefaultAscent`] re-aliases the chosen impl so consumer code names
//! one trait object regardless of which strategy wins.
//!
//! `NodeId` here is [`bbnf_ir::dag::NodeId`] — the same flat 32-bit
//! handle the IR uses for DAG nodes, reused as the parsed-document
//! node identifier so a future executor can share the type with the
//! IR rewrites that descend the same tree.

use bbnf_ir::dag::NodeId;

/// Reversal-seam abstraction for parent-pointer lookups on a parsed
/// node graph.
///
/// An impl stores whatever data it needs — a parallel sidecar, a
/// per-node field, or a re-walk hook — and answers
/// [`AscentStrategy::ascend`] in `Option<NodeId>`. The trait is
/// intentionally narrow: every impl has the same surface; only the
/// space/time trade-off differs.
pub trait AscentStrategy {
    /// Return the parent of `node`, if any. The root node ascends to
    /// `None`. An invalid `node` (e.g. [`NodeId::INVALID`]) also
    /// ascends to `None`; callers do not distinguish the two cases.
    fn ascend(&self, node: NodeId) -> Option<NodeId>;

    /// Storage cost of the strategy in bytes per node, amortised.
    /// Used by the W2.5 micro-bench to label each lane in the
    /// `W2-ascent-microbench.json` artefact.
    fn bytes_per_node(&self) -> usize;

    /// Human-friendly strategy name (`"root_traversal"`,
    /// `"in_struct_pointer"`, `"hybrid_sidecar"`). Stable; the
    /// micro-bench artefact keys on it.
    fn name(&self) -> &'static str;
}

/// Ascend by re-walking the tree from the root. Zero per-node bloat;
/// O(N) per ascent in the worst case.
///
/// Holds a borrowed `child → parent` callback so the strategy stays
/// tree-shape-agnostic — the parsed-document executor (W3) supplies a
/// callback that traverses the tape, while the W2.5 bench harness
/// supplies a `HashMap` lookup over a synthetic tree. The callback
/// receives the queried child id and returns the resolved parent (or
/// `None` for the root).
pub struct RootTraversal<F>
where
    F: Fn(NodeId) -> Option<NodeId>,
{
    walker: F,
}

impl<F> RootTraversal<F>
where
    F: Fn(NodeId) -> Option<NodeId>,
{
    /// Build a root-traversal strategy from a parent-resolver callback.
    pub fn new(walker: F) -> Self {
        Self { walker }
    }
}

impl<F> AscentStrategy for RootTraversal<F>
where
    F: Fn(NodeId) -> Option<NodeId>,
{
    #[inline]
    fn ascend(&self, node: NodeId) -> Option<NodeId> {
        if node == NodeId::INVALID {
            return None;
        }
        (self.walker)(node)
    }

    #[inline]
    fn bytes_per_node(&self) -> usize {
        0
    }

    #[inline]
    fn name(&self) -> &'static str {
        "root_traversal"
    }
}

/// Ascend through a parent-id slot stored on every node. Eight bytes
/// per node, O(1) per ascent — the cost lands at construction time on
/// every node, even when the consumer never calls `ascend`.
///
/// The strategy stores a `Vec<NodeId>` indexed by `node.as_usize()`;
/// `parents[i]` is the parent of node `i`. Out-of-bounds queries and
/// [`NodeId::INVALID`] queries return `None`.
pub struct InStructPointer {
    parents: Vec<NodeId>,
}

impl InStructPointer {
    /// Build the strategy from a fully-populated parent vector.
    /// `parents[i]` must be the parent of node `i`; the root's slot
    /// must be [`NodeId::INVALID`].
    pub fn from_parents(parents: Vec<NodeId>) -> Self {
        Self { parents }
    }

    /// Number of nodes the strategy has parent slots for.
    #[inline]
    pub fn len(&self) -> usize {
        self.parents.len()
    }

    /// True iff the strategy carries no parent slots.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.parents.is_empty()
    }
}

impl AscentStrategy for InStructPointer {
    #[inline]
    fn ascend(&self, node: NodeId) -> Option<NodeId> {
        if node == NodeId::INVALID {
            return None;
        }
        let idx = node.as_usize();
        let parent = self.parents.get(idx)?;
        if *parent == NodeId::INVALID {
            None
        } else {
            Some(*parent)
        }
    }

    #[inline]
    fn bytes_per_node(&self) -> usize {
        // 4 B for the NodeId itself; the strategy holds a single Vec
        // header (24 B amortised across all nodes — negligible at
        // realistic counts).
        core::mem::size_of::<NodeId>()
    }

    #[inline]
    fn name(&self) -> &'static str {
        "in_struct_pointer"
    }
}

/// Default. A parallel `Vec<NodeId>` indexed by depth-first order.
///
/// Construction cost is paid once, lazily, the first time a consumer
/// calls [`HybridSidecar::populate_from_walker`]. Until then the
/// strategy carries a single empty vector — zero bloat. Once
/// populated, ascent cost matches [`InStructPointer`] (one `Vec`
/// indexed access).
///
/// The "hybrid" framing comes from the construction lane: callers that
/// will ascend frequently pay the populate cost up front; callers that
/// never ascend pay nothing. The default selection (recorded in
/// `W2-ascent-microbench.json`) reflects the real-world parser
/// workload, where wildcard expansion is the dominant ascent driver
/// and the population step amortises across many subsequent queries.
pub struct HybridSidecar {
    parents: Vec<NodeId>,
    populated: bool,
}

impl HybridSidecar {
    /// Build an unpopulated sidecar. [`HybridSidecar::ascend`] returns
    /// `None` for every query until [`HybridSidecar::populate_from_walker`]
    /// runs.
    pub fn new() -> Self {
        Self {
            parents: Vec::new(),
            populated: false,
        }
    }

    /// Populate the sidecar by walking `node_count` nodes through a
    /// resolver callback. The callback receives every node id and
    /// returns its parent (or `None` for the root). Idempotent — the
    /// first call materialises the vector; subsequent calls are
    /// no-ops.
    pub fn populate_from_walker<F>(&mut self, node_count: usize, walker: F)
    where
        F: Fn(NodeId) -> Option<NodeId>,
    {
        if self.populated {
            return;
        }
        self.parents.clear();
        self.parents.reserve_exact(node_count);
        for i in 0..node_count {
            let id = NodeId::from_usize(i);
            self.parents.push(walker(id).unwrap_or(NodeId::INVALID));
        }
        self.populated = true;
    }

    /// True once the sidecar has been populated.
    #[inline]
    pub fn is_populated(&self) -> bool {
        self.populated
    }

    /// Number of nodes the sidecar carries parent slots for. Zero when
    /// unpopulated.
    #[inline]
    pub fn len(&self) -> usize {
        self.parents.len()
    }

    /// True iff the sidecar carries no parent slots (either
    /// unpopulated or populated against an empty graph).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.parents.is_empty()
    }
}

impl Default for HybridSidecar {
    fn default() -> Self {
        Self::new()
    }
}

impl AscentStrategy for HybridSidecar {
    #[inline]
    fn ascend(&self, node: NodeId) -> Option<NodeId> {
        if !self.populated || node == NodeId::INVALID {
            return None;
        }
        let parent = self.parents.get(node.as_usize())?;
        if *parent == NodeId::INVALID {
            None
        } else {
            Some(*parent)
        }
    }

    #[inline]
    fn bytes_per_node(&self) -> usize {
        if self.populated {
            core::mem::size_of::<NodeId>()
        } else {
            0
        }
    }

    #[inline]
    fn name(&self) -> &'static str {
        "hybrid_sidecar"
    }
}

/// Default ascent strategy. Selected by the W2.5 micro-bench artefact
/// (`docs/tranches/AZ-IV/audit/W2-ascent-microbench.json`); a future
/// tranche reassigns this alias if benchmark evidence forces a different
/// pick.
pub type DefaultAscent = HybridSidecar;
