//! Shared decision layer: pure type-resolution functions.
//!
//! These functions query the TypeMap to resolve structural decisions about IR nodes.
//! Both the parse driver (`driver.rs`) and the emit plan builder (`generate/emit/`)
//! consume these decisions — one source of truth for all type classification.
//!
//! ## Design
//!
//! Each `decide_*` function takes an IR node (or slice) + the GrammarIR (for TypeMap
//! access) + parent context (ValuePlacement) and returns a decision struct describing
//! the resolved types, child classification, and structural strategy.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use crate::backend::types::{FlattenStrategy, ValuePlacement};

// ═════════════════════════════════════════════════════════════════════════════
// Decision types
// ═════════════════════════════════════════════════════════════════════════════

/// Resolved Seq decomposition.
#[derive(Debug)]
pub struct SeqDecision {
    /// Post-compression, post-flatten result type.
    pub result_type: TypeDesc,
    /// All children are Span → pure structural.
    pub all_span: bool,
    /// Per-child resolved types (post Tuple-element override, may have span-method overrides).
    pub child_types: Vec<TypeDesc>,
    /// Per-child raw types (from node_type, NO span-method overrides).
    /// Used by emit plan for compression alignment.
    pub raw_child_types: Vec<TypeDesc>,
    /// Flatten strategy for `(T, Vec<T>) → Vec<T)`.
    pub flatten: Option<FlattenStrategy>,
}

/// Per-child alloc decision (computed from the child's TypeDesc + parent alloc).
pub fn child_alloc(ty: &TypeDesc, parent_alloc: ValuePlacement) -> ValuePlacement {
    match ty {
        TypeDesc::BoxedEnum => ValuePlacement::Alloc,
        TypeDesc::Enum if parent_alloc == ValuePlacement::Alloc => ValuePlacement::Alloc,
        TypeDesc::Vec(inner) if **inner != TypeDesc::Span => ValuePlacement::Alloc,
        _ => ValuePlacement::Inline,
    }
}

// ═════════════════════════════════════════════════════════════════════════════
// Decision functions
// ═════════════════════════════════════════════════════════════════════════════

/// Resolve Seq type decomposition from TypeMap.
///
/// Mirrors the decision logic in `driver.rs:compile_seq` lines 375-475.
pub fn decide_seq(children: &[IrNode], ir: &GrammarIR) -> SeqDecision {
    let type_map = ir.type_map.as_ref();

    // Step 1: child types from seq_child_types (pointer-based lookup).
    let child_types: Vec<TypeDesc> = type_map
        .and_then(|tm| {
            tm.seq_child_types_by_ptr(children.as_ptr() as usize)
                .map(|s| s.to_vec())
        })
        .unwrap_or_else(|| {
            children
                .iter()
                .map(|c| {
                    type_map
                        .and_then(|tm| tm.node_type(c).cloned())
                        .unwrap_or(TypeDesc::Span)
                })
                .collect()
        });

    // Step 2: result type.
    let result_type = type_map
        .and_then(|tm| tm.seq_result_type(children.as_ptr() as usize).cloned())
        .unwrap_or(TypeDesc::Span);

    // Step 3: all-Span check.
    let all_span = match &result_type {
        TypeDesc::Span => true,
        _ if child_types.iter().all(|t| *t == TypeDesc::Span) && !type_map.is_some() => true,
        _ => result_type == TypeDesc::Span,
    };

    // Step 4: override child types from result Tuple elements.
    let child_types = if let TypeDesc::Tuple(ref result_elems) = result_type {
        if result_elems.len() == child_types.len() {
            result_elems.clone()
        } else {
            child_types
        }
    } else {
        child_types
    };

    // Step 5: raw child types (from node_type, no seq_child_types override).
    let raw_child_types: Vec<TypeDesc> = children
        .iter()
        .map(|c| {
            type_map
                .and_then(|tm| tm.node_type(c).cloned())
                .unwrap_or(TypeDesc::Span)
        })
        .collect();

    // Step 6: flatten detection.
    let flatten = detect_flatten(&result_type, &child_types);

    SeqDecision {
        result_type,
        all_span,
        child_types,
        raw_child_types,
        flatten,
    }
}

// ═════════════════════════════════════════════════════════════════════════════
// Helpers
// ═════════════════════════════════════════════════════════════════════════════

/// Detect sep_by pattern: `Skip(element, Repeat(separator, 0, 1))`.
pub fn detect_sep_by(inner: &IrNode) -> Option<(&IrNode, &IrNode)> {
    if let IrNode::Skip(element, opt_sep) = inner {
        if let IrNode::Repeat {
            inner: separator,
            lo: 0,
            hi: 1,
        } = opt_sep.as_ref()
        {
            return Some((element.as_ref(), separator.as_ref()));
        }
    }
    None
}

/// Detect `(T, Vec<T>)` or `(Vec<T>, T)` flattening.
pub fn detect_flatten(
    result_type: &TypeDesc,
    child_types: &[TypeDesc],
) -> Option<FlattenStrategy> {
    let TypeDesc::Vec(_) = result_type else {
        return None;
    };
    if child_types.len() != 2 {
        return None;
    }
    match (&child_types[0], &child_types[1]) {
        (_, TypeDesc::Vec(_)) => Some(FlattenStrategy::HeadThenVec),
        (TypeDesc::Vec(_), _) => Some(FlattenStrategy::VecThenTail),
        _ => None,
    }
}

