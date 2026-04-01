//! Type inference for codegen — delegates to the canonical IR inference.
//!
//! Uses `ctx.codegen_type_cache` for Ref lookups. For arena mode, this cache
//! is built from scratch with consistent flags. For owned mode, it's ir.types.
//!
//! Always uses cyclic_context=false, pretty_preserve=false to match the
//! cache. This ensures exact agreement between enum variant types and body types.

use std::collections::HashSet;

use bbnf_ir::passes::{InferCtx, infer_node, infer_node_in_vec};
use bbnf_ir::{IrNode, RuleId, TypeDesc};

use super::ir_types::IrCodegenCtx;

/// Type inference for a single IrNode.
pub fn infer_node_type(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    let acyclic: HashSet<RuleId> = ctx
        .ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();
    let infer_ctx = InferCtx {
        ir: ctx.ir,
        cache: &ctx.codegen_type_cache,
        acyclic_rules: &acyclic,
        cyclic_context: false,
        pretty_preserve: false,
    };
    infer_node(node, &infer_ctx)
}

/// Type inference in elide_box (Vec) context.
pub fn infer_node_type_elide_box(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    let acyclic: HashSet<RuleId> = ctx
        .ir
        .rules
        .iter()
        .filter(|r| !r.meta.is_cyclic)
        .map(|r| r.id)
        .collect();
    let infer_ctx = InferCtx {
        ir: ctx.ir,
        cache: &ctx.codegen_type_cache,
        acyclic_rules: &acyclic,
        cyclic_context: false,
        pretty_preserve: false,
    };
    infer_node_in_vec(node, &infer_ctx)
}
