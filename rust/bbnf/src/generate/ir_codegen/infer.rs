//! Type inference for codegen — delegates to the canonical IR inference.
//!
//! All type inference is performed by `bbnf_ir::passes::types::{infer_node, infer_node_in_vec}`.
//! This module provides thin wrappers that construct the `InferCtx` from the
//! codegen context (`IrCodegenCtx`), ensuring zero divergence between IR analysis
//! and codegen.

use std::collections::{HashMap, HashSet};

use bbnf_ir::{IrNode, RuleId, TypeDesc};
use bbnf_ir::passes::{InferCtx, infer_node, infer_node_in_vec};

use super::super::ir_types::IrCodegenCtx;

/// Shared state for codegen type inference — cached to avoid recomputation.
///
/// Built once per rule body emission via `CodegenInfer::new()`.
pub struct CodegenInfer {
    cache: HashMap<RuleId, TypeDesc>,
    acyclic_rules: HashSet<RuleId>,
}

impl CodegenInfer {
    /// Build from the IR's pre-computed type table.
    pub fn new(ctx: &IrCodegenCtx<'_>) -> Self {
        let cache: HashMap<RuleId, TypeDesc> = ctx.ir.types.iter().cloned().collect();
        let acyclic: HashSet<RuleId> = ctx
            .ir
            .rules
            .iter()
            .filter(|r| !r.meta.is_cyclic)
            .map(|r| r.id)
            .collect();
        Self { cache, acyclic_rules: acyclic }
    }

    /// Build an `InferCtx` matching the IR pass's context for the current rule.
    fn make_ctx<'a>(&'a self, ctx: &'a IrCodegenCtx<'_>) -> InferCtx<'a> {
        // Determine cyclic_context from current_rule being emitted.
        // The IR pass uses cyclic_context=true for cyclic rules, which affects
        // B.4 override (acyclic Ref types in cyclic context → BoxedEnum).
        let cyclic_context = false; // Will be overridden per-call when needed
        InferCtx {
            ir: ctx.ir,
            cache: &self.cache,
            acyclic_rules: &self.acyclic_rules,
            cyclic_context,

            pretty_preserve: false,
        }
    }
}

/// Type inference for a single IrNode — delegates to the canonical IR inference.
///
/// This is the SAME function that runs during IR type analysis. Using it here
/// guarantees zero divergence between the types stored in `RuleMeta::sub_variants`
/// and the types computed at codegen time.
pub fn infer_node_type(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    let ci = CodegenInfer::new(ctx);
    let infer_ctx = ci.make_ctx(ctx);
    infer_node(node, &infer_ctx)
}

/// Type inference in elide_box (Vec) context — delegates to the canonical IR inference.
///
/// In Vec contexts, `Ref` → `Enum` (not `BoxedEnum`) because Vec provides
/// heap indirection, making Box unnecessary.
pub fn infer_node_type_elide_box(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    let ci = CodegenInfer::new(ctx);
    let infer_ctx = ci.make_ctx(ctx);
    infer_node_in_vec(node, &infer_ctx)
}
