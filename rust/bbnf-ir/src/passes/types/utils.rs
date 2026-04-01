//! Helper types and utility functions for type inference.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId, TypeDesc};

// ── InferMap: precomputed sub-expression types ──────────────────────────────

/// Precomputed type information for every IR node, keyed by raw pointer.
///
/// Built during `infer_types` and consumed by codegen. Eliminates all
/// codegen re-inference — lookups are O(1) HashMap access.
///
/// Not serializable (pointer keys). Only valid within the process that
/// ran `infer_types`. For WASM, the codegen doesn't run (only the VM).
#[derive(Default, Debug, Clone)]
pub struct InferMap {
    /// TypeDesc from `infer_node(node, ctx)` for each visited node.
    node_types: HashMap<usize, TypeDesc>,
    /// TypeDesc from `infer_node_in_vec(node, ctx)` for each visited node.
    vec_elem_types: HashMap<usize, TypeDesc>,
    /// Per-Seq effective child types after B.1 override + Span compression.
    /// Keyed by Seq node pointer.
    seq_child_types: HashMap<usize, Vec<TypeDesc>>,
}

impl InferMap {
    /// Look up the type of a node (from `infer_node`).
    #[inline]
    pub fn node_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        self.node_types.get(&(node as *const IrNode as usize))
    }

    /// Look up the effective child types for a Seq by its children slice pointer.
    #[inline]
    pub fn seq_child_types_by_ptr(&self, children_ptr: usize) -> Option<&[TypeDesc]> {
        self.seq_child_types
            .get(&children_ptr)
            .map(|v| v.as_slice())
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

    /// Look up the Vec-element type of a node (from `infer_node_in_vec`).
    #[inline]
    pub fn vec_elem_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        self.vec_elem_types.get(&(node as *const IrNode as usize))
    }
}

/// Mutable recorder used during `infer_types` to populate an `InferMap`.
/// Uses `RefCell` for interior mutability (the inference functions take `&self`).
#[derive(Default)]
pub struct InferRecorder {
    node_types: RefCell<HashMap<usize, TypeDesc>>,
    vec_elem_types: RefCell<HashMap<usize, TypeDesc>>,
    seq_child_types: RefCell<HashMap<usize, Vec<TypeDesc>>>,
}

impl InferRecorder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a node's infer_node result.
    #[inline]
    pub fn record_node(&self, node: &IrNode, ty: &TypeDesc) {
        self.node_types
            .borrow_mut()
            .insert(node as *const IrNode as usize, ty.clone());
    }

    /// Record a node's infer_node result only if not already present.
    #[inline]
    pub fn record_node_if_absent(&self, node: &IrNode, ty: &TypeDesc) {
        let key = node as *const IrNode as usize;
        let mut map = self.node_types.borrow_mut();
        map.entry(key).or_insert_with(|| ty.clone());
    }

    /// Record a node's infer_node_in_vec result.
    #[inline]
    pub fn record_vec_elem(&self, node: &IrNode, ty: &TypeDesc) {
        self.vec_elem_types
            .borrow_mut()
            .insert(node as *const IrNode as usize, ty.clone());
    }

    /// Record a node's infer_node_in_vec result only if not already present.
    #[inline]
    pub fn record_vec_elem_if_absent(&self, node: &IrNode, ty: &TypeDesc) {
        let key = node as *const IrNode as usize;
        let mut map = self.vec_elem_types.borrow_mut();
        map.entry(key).or_insert_with(|| ty.clone());
    }

    /// Record the effective child types for a Seq node.
    #[inline]
    pub fn record_seq_children(&self, seq_node: &[IrNode], types: &[TypeDesc]) {
        self.seq_child_types
            .borrow_mut()
            .insert(seq_node.as_ptr() as usize, types.to_vec());
    }

    /// Consume into an immutable InferMap.
    pub fn into_map(self) -> InferMap {
        InferMap {
            node_types: self.node_types.into_inner(),
            vec_elem_types: self.vec_elem_types.into_inner(),
            seq_child_types: self.seq_child_types.into_inner(),
        }
    }
}

// ── InferCtx ────────────────────────────────────────────────────────────────

/// Context for type inference — avoids threading many parameters.
pub struct InferCtx<'a> {
    pub ir: &'a GrammarIR,
    pub cache: &'a HashMap<RuleId, TypeDesc>,
    pub acyclic_rules: &'a std::collections::HashSet<RuleId>,
    /// Whether the current rule being inferred is cyclic (for B.4).
    pub cyclic_context: bool,
    /// Consumable flag for @pretty tuple preservation (B.2).
    /// Only applies to the first (top-level) Seq encountered.
    pub pretty_preserve: bool,
    /// Optional recorder for building an InferMap during the pass.
    pub recorder: Option<&'a InferRecorder>,
}

impl InferCtx<'_> {
    /// Return a copy with pretty_preserve consumed (set to false).
    pub fn consumed(&self) -> InferCtx<'_> {
        InferCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            pretty_preserve: false,
            recorder: self.recorder,
        }
    }

    /// Return a copy with recorder disabled (for recording-pass re-inference).
    pub fn with_no_recorder(&self) -> InferCtx<'_> {
        InferCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            pretty_preserve: self.pretty_preserve,
            recorder: None,
        }
    }
}

/// Try to flatten a 2-element tuple where one is `T` and the other is `Vec<T>`.
/// Only flattens same-type pairs (A, Vec<A>) or (Vec<A>, A).
pub fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    // (T, Vec<T>) → Vec<T>
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
    }
    // (Vec<T>, T) → Vec<T>
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
    }
    None
}
