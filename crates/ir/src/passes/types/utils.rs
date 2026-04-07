//! Helper types and utility functions for type projection.

use std::collections::HashMap;

use crate::{IrNode, TypeDesc};

// ── TypeMap: precomputed sub-expression types ────────────────────────────────

/// Precomputed type information for every IR node, keyed by raw pointer.
///
/// Built during `project_types` (CSP solver) and consumed by codegen. Eliminates
/// all codegen re-inference — lookups are O(1) HashMap access.
///
/// Not serializable (pointer keys). Only valid within the process that
/// ran `project_types`. For WASM, the codegen doesn't run (only the VM).
#[derive(Default, Debug, Clone)]
pub struct TypeMap {
    /// TypeDesc from normal-context projection for each visited node.
    /// May include parse-optimizing collapses (Optional(Span)→Span, Seq compression, etc.)
    node_types: HashMap<usize, TypeDesc>,
    /// Structural types: pre-collapse types that reflect the actual runtime topology.
    /// Only populated where structural differs from collapsed. Used by emit codegen.
    structural_types: HashMap<usize, TypeDesc>,
    /// TypeDesc from vec-context projection for each visited node.
    vec_elem_types: HashMap<usize, TypeDesc>,
    /// Per-Seq effective child types after span-method override (pre-compression).
    /// Keyed by Seq children slice pointer.
    seq_child_types: HashMap<usize, Vec<TypeDesc>>,
    /// Per-Seq result type (post-compression, post-flattening — the return of project_seq).
    /// Keyed by Seq children slice pointer.
    seq_result_types: HashMap<usize, TypeDesc>,
    /// Per-Seq preserve_spans flag: true when the Seq preserved individual Span
    /// identity (skipped compression).
    /// Keyed by Seq children slice pointer.
    seq_preserve_spans: HashMap<usize, bool>,
    /// Distinct Vec element types for scratch Vec generation in codegen.
    /// Collected from both `vec_elem_types` values and `ir.types` Vec inners.
    scratch_types: Vec<TypeDesc>,
}

impl TypeMap {
    /// Look up the type of a node (from normal-context projection).
    #[inline]
    pub fn node_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        self.node_types.get(&(node as *const IrNode as usize))
    }

    /// Look up the structural (pre-collapse) type of a node for emission.
    /// Returns the structural type if it differs from the collapsed type,
    /// otherwise falls back to the collapsed type.
    #[inline]
    pub fn structural_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        let ptr = node as *const IrNode as usize;
        self.structural_types.get(&ptr).or_else(|| self.node_types.get(&ptr))
    }

    /// Look up the effective child types for a Seq by its children slice pointer.
    #[inline]
    pub fn seq_child_types_by_ptr(&self, children_ptr: usize) -> Option<&[TypeDesc]> {
        self.seq_child_types
            .get(&children_ptr)
            .map(|v| v.as_slice())
    }

    /// Look up the result type of a Seq (post-compression, post-flattening).
    #[inline]
    pub fn seq_result_type(&self, children_ptr: usize) -> Option<&TypeDesc> {
        self.seq_result_types.get(&children_ptr)
    }

    /// Look up whether a Seq preserved individual Span identity.
    #[inline]
    pub fn seq_preserve_spans(&self, children_ptr: usize) -> bool {
        self.seq_preserve_spans
            .get(&children_ptr)
            .copied()
            .unwrap_or(false)
    }

    /// Get the distinct scratch types for codegen scratch Vec generation.
    pub fn scratch_types(&self) -> &[TypeDesc] {
        &self.scratch_types
    }

    /// Set the scratch types (called after correction pass).
    pub fn set_scratch_types(&mut self, types: Vec<TypeDesc>) {
        self.scratch_types = types;
    }

    /// Override the vec_elem_type for a node (used by correction pass).
    pub fn set_vec_elem_type(&mut self, node: &IrNode, ty: TypeDesc) {
        self.vec_elem_types
            .insert(node as *const IrNode as usize, ty);
    }

    /// Number of entries in the node_types map.
    pub fn node_types_len(&self) -> usize {
        self.node_types.len()
    }

    /// Look up the Vec-element type of a node (from vec-context projection).
    #[inline]
    pub fn vec_elem_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        self.vec_elem_types.get(&(node as *const IrNode as usize))
    }

    // ── Builder methods for CSP export ──────────────────────────────────

    /// Insert a node type (normal context).
    pub(super) fn insert_node_type(&mut self, node_id: usize, ty: TypeDesc) {
        self.node_types.insert(node_id, ty);
    }

    /// Insert a structural (pre-collapse) type for a node.
    /// Only call when the structural type DIFFERS from the collapsed type.
    pub(super) fn insert_structural_type(&mut self, node_id: usize, ty: TypeDesc) {
        self.structural_types.insert(node_id, ty);
    }

    /// Insert a vec-element type.
    pub(super) fn insert_vec_elem_type(&mut self, node_id: usize, ty: TypeDesc) {
        self.vec_elem_types.insert(node_id, ty);
    }

    /// Insert seq child types (pre-compression).
    pub(super) fn insert_seq_child_types(&mut self, children_ptr: usize, types: Vec<TypeDesc>) {
        self.seq_child_types.insert(children_ptr, types);
    }

    /// Insert seq result type (post-compression, post-flattening).
    pub(super) fn insert_seq_result_type(&mut self, children_ptr: usize, ty: TypeDesc) {
        self.seq_result_types.insert(children_ptr, ty);
    }

    /// Insert seq preserve_spans flag.
    pub(super) fn insert_seq_preserve_spans(&mut self, children_ptr: usize, preserve: bool) {
        self.seq_preserve_spans.insert(children_ptr, preserve);
    }
}

/// Try to flatten a 2-element tuple where one is `T` and the other is `Vec<T>`.
/// Only flattens same-type pairs (A, Vec<A)) or (Vec<A), A).
pub fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    // (T, Vec<T)) → Vec<T)
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
    }
    // (Vec<T), T) → Vec<T)
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
    }
    None
}
