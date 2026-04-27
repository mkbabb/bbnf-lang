//! [`TypeMap`] — precomputed sub-expression types per IR node.
//!
//! Built during `project_types` (CSP solver) and consumed by codegen.
//! Eliminates codegen re-inference — lookups are O(1) HashMap access.
//! Carries normal-context types, structural (pre-collapse) types, vec-
//! context types, per-Seq child + result types, and `preserve_spans`
//! flags.
//!
//! Also exposes [`try_flatten_pair`] — the `(T, Vec<T>)` /
//! `(Vec<T>, T)` flattening helper consumed by the CSP solver's revise
//! step + by codegen.

use std::collections::HashMap;

use crate::dag::NodeId;
use crate::TypeDesc;

// ── TypeMap: precomputed sub-expression types ────────────────────────────────

/// Precomputed type information for every IR node, keyed by stable
/// `NodeId` from the durable `GrammarDag` substrate.
///
/// Built during `project_types` (CSP solver) and consumed by codegen.
/// Eliminates codegen re-inference — lookups are O(1) HashMap access.
/// Callers resolve `&IrNode → NodeId` via `GrammarIR::dag.node_for()`
/// before calling the getters here; convenience wrappers on
/// [`GrammarIR`](crate::GrammarIR) chain both steps.
#[derive(Default, Debug, Clone)]
pub struct TypeMap {
    /// TypeDesc from normal-context projection for each visited node.
    /// May include parse-optimizing collapses (`Optional(Span)→Span`,
    /// Seq compression, etc.).
    node_types: HashMap<NodeId, TypeDesc>,
    /// Structural types: pre-collapse types that reflect the actual
    /// runtime topology. Only populated where structural differs from
    /// collapsed. Used by emit codegen.
    structural_types: HashMap<NodeId, TypeDesc>,
    /// TypeDesc from vec-context projection for each visited node.
    vec_elem_types: HashMap<NodeId, TypeDesc>,
    /// Per-Seq effective child types after span-method override
    /// (pre-compression). Keyed by the Seq node's `NodeId`.
    seq_child_types: HashMap<NodeId, Vec<TypeDesc>>,
    /// Per-Seq result type (post-compression, post-flattening — the
    /// return of `project_seq`). Keyed by the Seq node's `NodeId`.
    seq_result_types: HashMap<NodeId, TypeDesc>,
    /// Per-Seq `preserve_spans` flag: true when the Seq preserved
    /// individual Span identity (skipped compression). Keyed by the
    /// Seq node's `NodeId`.
    seq_preserve_spans: HashMap<NodeId, bool>,
    /// Distinct Vec element types for scratch Vec generation in
    /// codegen. Collected from both `vec_elem_types` values and
    /// `ir.types` Vec inners.
    scratch_types: Vec<TypeDesc>,
}

impl TypeMap {
    /// Look up the type of a node (from normal-context projection).
    #[inline]
    pub fn node_type(&self, id: NodeId) -> Option<&TypeDesc> {
        self.node_types.get(&id)
    }

    /// Look up the structural (pre-collapse) type of a node for
    /// emission. Returns the structural type if it differs from the
    /// collapsed type, otherwise falls back to the collapsed type.
    #[inline]
    pub fn structural_type(&self, id: NodeId) -> Option<&TypeDesc> {
        self.structural_types
            .get(&id)
            .or_else(|| self.node_types.get(&id))
    }

    /// Look up the effective child types for a Seq by the Seq node's
    /// `NodeId`.
    #[inline]
    pub fn seq_child_types(&self, id: NodeId) -> Option<&[TypeDesc]> {
        self.seq_child_types.get(&id).map(|v| v.as_slice())
    }

    /// Look up the result type of a Seq (post-compression,
    /// post-flattening) by the Seq node's `NodeId`.
    #[inline]
    pub fn seq_result_type(&self, id: NodeId) -> Option<&TypeDesc> {
        self.seq_result_types.get(&id)
    }

    /// Look up whether a Seq preserved individual Span identity.
    #[inline]
    pub fn seq_preserve_spans(&self, id: NodeId) -> bool {
        self.seq_preserve_spans.get(&id).copied().unwrap_or(false)
    }

    /// Get the distinct scratch types for codegen scratch Vec
    /// generation.
    pub fn scratch_types(&self) -> &[TypeDesc] {
        &self.scratch_types
    }

    /// Set the scratch types (called after correction pass).
    pub fn set_scratch_types(&mut self, types: Vec<TypeDesc>) {
        self.scratch_types = types;
    }

    /// Override the vec_elem_type for a node (used by correction
    /// pass).
    pub fn set_vec_elem_type(&mut self, id: NodeId, ty: TypeDesc) {
        self.vec_elem_types.insert(id, ty);
    }

    /// Number of entries in the `node_types` map.
    pub fn node_types_len(&self) -> usize {
        self.node_types.len()
    }

    // ── Iterators used by Tranche AA.1 for interning ────────────────────

    /// Iterate every `(NodeId, &TypeDesc)` in the normal-context map.
    pub fn iter_node_types(&self) -> impl Iterator<Item = (NodeId, &TypeDesc)> {
        self.node_types.iter().map(|(id, ty)| (*id, ty))
    }

    /// Iterate every `(NodeId, &TypeDesc)` in the vec-context map.
    pub fn iter_vec_elem_types(&self) -> impl Iterator<Item = (NodeId, &TypeDesc)> {
        self.vec_elem_types.iter().map(|(id, ty)| (*id, ty))
    }

    /// Iterate every `(NodeId, &TypeDesc)` in the structural type map.
    pub fn iter_structural_types(&self) -> impl Iterator<Item = (NodeId, &TypeDesc)> {
        self.structural_types.iter().map(|(id, ty)| (*id, ty))
    }

    /// Iterate every `(NodeId, &TypeDesc)` in the Seq result type map.
    pub fn iter_seq_result_types(&self) -> impl Iterator<Item = (NodeId, &TypeDesc)> {
        self.seq_result_types.iter().map(|(id, ty)| (*id, ty))
    }

    /// Iterate every `(NodeId, &[TypeDesc])` in the Seq child type map.
    pub fn iter_seq_child_types(&self) -> impl Iterator<Item = (NodeId, &[TypeDesc])> {
        self.seq_child_types
            .iter()
            .map(|(id, v)| (*id, v.as_slice()))
    }

    /// Look up the Vec-element type of a node (from vec-context
    /// projection).
    #[inline]
    pub fn vec_elem_type(&self, id: NodeId) -> Option<&TypeDesc> {
        self.vec_elem_types.get(&id)
    }

    // ── Builder methods for CSP export ──────────────────────────────────

    /// Insert a node type (normal context).
    pub(super) fn insert_node_type(&mut self, id: NodeId, ty: TypeDesc) {
        self.node_types.insert(id, ty);
    }

    /// Insert a structural (pre-collapse) type for a node. Only call
    /// when the structural type DIFFERS from the collapsed type.
    pub(super) fn insert_structural_type(&mut self, id: NodeId, ty: TypeDesc) {
        self.structural_types.insert(id, ty);
    }

    /// Insert a vec-element type.
    pub(super) fn insert_vec_elem_type(&mut self, id: NodeId, ty: TypeDesc) {
        self.vec_elem_types.insert(id, ty);
    }

    /// Insert Seq child types (pre-compression), keyed by the Seq
    /// node's `NodeId`.
    pub(super) fn insert_seq_child_types(&mut self, id: NodeId, types: Vec<TypeDesc>) {
        self.seq_child_types.insert(id, types);
    }

    /// Insert Seq result type (post-compression, post-flattening),
    /// keyed by the Seq node's `NodeId`.
    pub(super) fn insert_seq_result_type(&mut self, id: NodeId, ty: TypeDesc) {
        self.seq_result_types.insert(id, ty);
    }

    /// Insert Seq `preserve_spans` flag, keyed by the Seq node's
    /// `NodeId`.
    pub(super) fn insert_seq_preserve_spans(&mut self, id: NodeId, preserve: bool) {
        self.seq_preserve_spans.insert(id, preserve);
    }
}

/// Try to flatten a 2-element tuple where one is `T` and the other
/// is `Vec<T>`. Only flattens same-type pairs `(A, Vec<A>)` or
/// `(Vec<A>, A)`.
pub fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
    }
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
    }
    None
}
