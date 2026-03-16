//! Quick type inference for IrNode trees (used by codegen for Span detection).

use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};

use super::super::ir_types::IrCodegenCtx;

/// Quick type inference for a single IrNode (used by codegen for Span detection).
/// This is a simplified version that doesn't need the full InferCtx.
pub fn infer_node_type(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) => TypeDesc::Span,
        IrNode::Epsilon => TypeDesc::Tuple(vec![]),
        // Always BoxedEnum: emit_ref wraps with .map(Box::new), matching the
        // AST type inference (type_inference.rs:122 returns boxed_enum_type for
        // all nonterminal references).
        IrNode::Ref(_) => TypeDesc::BoxedEnum,
        IrNode::Seq(children) => {
            // B.1: Override Ref to rules with _sp() methods with Span.
            // Matches emit_seq's sp_method_rules override and infer_types:infer_seq.
            let child_types: Vec<TypeDesc> = children
                .iter()
                .map(|c| {
                    if let IrNode::Ref(id) = c {
                        let rule = &ctx.ir.rules[*id as usize];
                        if rule.meta.has_sp_method && !rule.meta.is_transparent {
                            return TypeDesc::Span;
                        }
                    }
                    infer_node_type(c, ctx)
                })
                .collect();

            // All-Span guard: if all children would be Span after override,
            // don't apply (emit_seq also has this guard).
            let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
            let tys = if all_span {
                children
                    .iter()
                    .map(|c| infer_node_type(c, ctx))
                    .collect::<Vec<_>>()
            } else {
                child_types
            };

            // Span compression.
            let mut compressed: Vec<TypeDesc> = Vec::new();
            let mut in_span = false;
            for ty in &tys {
                if *ty == TypeDesc::Span {
                    if !in_span {
                        compressed.push(TypeDesc::Span);
                        in_span = true;
                    }
                } else {
                    compressed.push(ty.clone());
                    in_span = false;
                }
            }

            // (T, Vec<T>) flattening — matches emit_seq.
            if compressed.len() == 2 {
                if let TypeDesc::Vec(inner) = &compressed[1] {
                    if **inner == compressed[0] {
                        return compressed[1].clone();
                    }
                }
                if let TypeDesc::Vec(inner) = &compressed[0] {
                    if **inner == compressed[1] {
                        return compressed[0].clone();
                    }
                }
            }

            if compressed.len() == 1 {
                compressed.into_iter().next().unwrap()
            } else {
                TypeDesc::Tuple(compressed)
            }
        }
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let first = infer_node_type(&branches[0].node, ctx);
            if branches[1..]
                .iter()
                .all(|b| infer_node_type(&b.node, ctx) == first)
            {
                first
            } else {
                TypeDesc::BoxedEnum
            }
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_ty = infer_node_type(inner, ctx);
            if *lo == 0 && *hi == 1 {
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else if inner_ty == TypeDesc::Span {
                TypeDesc::Span
            } else {
                TypeDesc::Vec(Box::new(inner_ty))
            }
        }
        IrNode::Skip(left, _) => infer_node_type(left, ctx),
        IrNode::Next(_, right) => infer_node_type(right, ctx),
        IrNode::Minus(left, _) => infer_node_type(left, ctx),
        IrNode::Negate(_) => TypeDesc::Tuple(vec![]),
        IrNode::OptionalWhitespace(inner) => infer_node_type(inner, ctx),
        IrNode::Map { fn_id, .. } => match &ctx.ir.fns[*fn_id as usize] {
            FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
            FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
            FnDescriptor::Custom {
                return_type,
                source,
            } => return_type.clone().unwrap_or(TypeDesc::Named(*source)),
        },
    }
}
