//! Helper types and utility functions for type projection.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::{GrammarIR, IrNode, RuleId, TypeDesc};

// ── TypeMap: precomputed sub-expression types ────────────────────────────────

/// Precomputed type information for every IR node, keyed by raw pointer.
///
/// Built during `project_types` and consumed by codegen. Eliminates all
/// codegen re-inference — lookups are O(1) HashMap access.
///
/// Not serializable (pointer keys). Only valid within the process that
/// ran `project_types`. For WASM, the codegen doesn't run (only the VM).
#[derive(Default, Debug, Clone)]
pub struct TypeMap {
    /// TypeDesc from `project_node(node, ctx)` for each visited node.
    node_types: HashMap<usize, TypeDesc>,
    /// TypeDesc from `project_node_in_vec(node, ctx)` for each visited node.
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
    /// Look up the type of a node (from `project_node`).
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

    /// Look up the Vec-element type of a node (from `project_node_in_vec`).
    #[inline]
    pub fn vec_elem_type(&self, node: &IrNode) -> Option<&TypeDesc> {
        self.vec_elem_types.get(&(node as *const IrNode as usize))
    }
}

/// Mutable recorder used during `project_types` to populate a `TypeMap`.
/// Uses `RefCell` for interior mutability (the projection functions take `&self`).
#[derive(Default)]
pub struct TypeRecorder {
    node_types: RefCell<HashMap<usize, TypeDesc>>,
    vec_elem_types: RefCell<HashMap<usize, TypeDesc>>,
    seq_child_types: RefCell<HashMap<usize, Vec<TypeDesc>>>,
    seq_result_types: RefCell<HashMap<usize, TypeDesc>>,
    seq_preserve_spans: RefCell<HashMap<usize, bool>>,
}

impl TypeRecorder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a node's project_node result.
    #[inline]
    pub fn record_node(&self, node: &IrNode, ty: &TypeDesc) {
        self.node_types
            .borrow_mut()
            .insert(node as *const IrNode as usize, ty.clone());
    }

    /// Record a node's project_node result only if not already present.
    #[inline]
    pub fn record_node_if_absent(&self, node: &IrNode, ty: &TypeDesc) {
        let key = node as *const IrNode as usize;
        let mut map = self.node_types.borrow_mut();
        map.entry(key).or_insert_with(|| ty.clone());
    }

    /// Record a node's project_node_in_vec result.
    #[inline]
    pub fn record_vec_elem(&self, node: &IrNode, ty: &TypeDesc) {
        self.vec_elem_types
            .borrow_mut()
            .insert(node as *const IrNode as usize, ty.clone());
    }

    /// Record a node's project_node_in_vec result only if not already present.
    #[inline]
    pub fn record_vec_elem_if_absent(&self, node: &IrNode, ty: &TypeDesc) {
        let key = node as *const IrNode as usize;
        let mut map = self.vec_elem_types.borrow_mut();
        map.entry(key).or_insert_with(|| ty.clone());
    }

    /// Check if seq_child_types are already recorded for a Seq.
    pub fn has_seq_children(&self, seq_node: &[IrNode]) -> bool {
        self.seq_child_types
            .borrow()
            .contains_key(&(seq_node.as_ptr() as usize))
    }

    /// Record the effective child types for a Seq node.
    #[inline]
    pub fn record_seq_children(&self, seq_node: &[IrNode], types: &[TypeDesc]) {
        self.seq_child_types
            .borrow_mut()
            .insert(seq_node.as_ptr() as usize, types.to_vec());
    }

    /// Record the result type of a Seq (post-compression, post-flattening).
    #[inline]
    pub fn record_seq_result(&self, seq_node: &[IrNode], ty: &TypeDesc) {
        self.seq_result_types
            .borrow_mut()
            .insert(seq_node.as_ptr() as usize, ty.clone());
    }

    /// Record whether a Seq preserved individual Span identity.
    #[inline]
    pub fn record_seq_preserve_spans(&self, seq_node: &[IrNode], preserve: bool) {
        self.seq_preserve_spans
            .borrow_mut()
            .insert(seq_node.as_ptr() as usize, preserve);
    }

    /// Consume into an immutable TypeMap.
    pub fn into_map(self) -> TypeMap {
        TypeMap {
            node_types: self.node_types.into_inner(),
            vec_elem_types: self.vec_elem_types.into_inner(),
            seq_child_types: self.seq_child_types.into_inner(),
            seq_result_types: self.seq_result_types.into_inner(),
            seq_preserve_spans: self.seq_preserve_spans.into_inner(),
            scratch_types: Vec::new(),
        }
    }
}

// ── ProjectionCtx ──────────────────────────────────────────────────────────

/// Rules that govern type projection behavior.
#[derive(Clone, Debug, Default)]
pub struct ProjectionRules {
    /// Preserve individual Span identity in the top-level Seq (skip consecutive-
    /// Span compression). Consumed after the first Seq so nested Seqs are
    /// unaffected. Currently sourced from `@pretty` directives.
    pub preserve_spans: bool,
}

/// Context for type projection — avoids threading many parameters.
pub struct ProjectionCtx<'a> {
    pub ir: &'a GrammarIR,
    pub cache: &'a HashMap<RuleId, TypeDesc>,
    pub acyclic_rules: &'a std::collections::HashSet<RuleId>,
    /// Whether the current rule being projected is cyclic (for cyclic-context boxing).
    pub cyclic_context: bool,
    /// Projection rules (preserve_spans, etc.).
    /// Only applies to the first (top-level) Seq encountered.
    pub rules: ProjectionRules,
    /// Optional recorder for building a TypeMap during the pass.
    pub recorder: Option<&'a TypeRecorder>,
}

impl ProjectionCtx<'_> {
    /// Return a copy with preserve_spans consumed (only applies to top-level Seq).
    pub fn consumed(&self) -> ProjectionCtx<'_> {
        ProjectionCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            rules: ProjectionRules::default(),
            recorder: self.recorder,
        }
    }

    /// Return a copy with recorder disabled (for recording-pass re-inference).
    pub fn with_no_recorder(&self) -> ProjectionCtx<'_> {
        ProjectionCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            rules: ProjectionRules {
                preserve_spans: self.rules.preserve_spans,
            },
            recorder: None,
        }
    }
}

/// Try to flatten a 2-element tuple where one is `T` and the other is `Vec<T>`.
/// Only flattens same-type pairs (A, Vec<A>) or (Vec<A>, A).
pub fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    // (T, Vec<T)) → Vec<T>
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
    }
    // (Vec<T>, T) → Vec<T)
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
    }
    None
}
