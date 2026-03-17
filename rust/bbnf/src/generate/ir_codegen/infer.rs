//! Quick type inference for IrNode trees (used by codegen for Span detection).

use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};

use super::super::ir_types::IrCodegenCtx;

/// Quick type inference for a single IrNode (used by codegen for Span detection).
/// This is a simplified version that doesn't need the full InferCtx.
pub fn infer_node_type(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) => TypeDesc::Span,
        IrNode::Epsilon => TypeDesc::Tuple(vec![]),
        // Non-transparent refs get boxed by emit_ref → BoxedEnum.
        // Transparent refs also return BoxedEnum (Box<Enum>).
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
                    // (BoxedEnum, Vec<Enum>) → Vec<Enum>
                    if **inner == TypeDesc::Enum && compressed[0] == TypeDesc::BoxedEnum {
                        return compressed[1].clone();
                    }
                }
                if let TypeDesc::Vec(inner) = &compressed[0] {
                    if **inner == compressed[1] {
                        return compressed[0].clone();
                    }
                    // (Vec<Enum>, BoxedEnum) → Vec<Enum>
                    if **inner == TypeDesc::Enum && compressed[1] == TypeDesc::BoxedEnum {
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
            if *lo == 0 && *hi == 1 {
                let inner_ty = infer_node_type(inner, ctx);
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else {
                // Vec-producing: use in_vec inference (Ref → Enum, not BoxedEnum).
                let inner_ty = infer_node_type_in_vec(inner, ctx);
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    TypeDesc::Vec(Box::new(inner_ty))
                }
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

/// Quick type inference for a single IrNode in a Vec context.
///
/// Identical to `infer_node_type` except `Ref` returns `Enum` for non-transparent
/// rules (since Vec provides heap indirection, Box is unnecessary). Transparent
/// rules still return `BoxedEnum` since they box internally.
///
/// Propagates through Skip (left), Next (right), Minus (left), Map, OW — matching
/// the codegen `in_vec` propagation.
pub fn infer_node_type_in_vec(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    match node {
        // In Vec context, ALL refs return Enum (no boxing needed).
        // Non-transparent: codegen emits Self::rule() without Box.
        // Transparent: codegen emits Self::rule_unboxed() which returns Enum directly.
        IrNode::Ref(_) => TypeDesc::Enum,
        IrNode::Skip(left, _) => infer_node_type_in_vec(left, ctx),
        IrNode::Next(_, right) => infer_node_type_in_vec(right, ctx),
        IrNode::Minus(left, _) => infer_node_type_in_vec(left, ctx),
        IrNode::OptionalWhitespace(inner) => infer_node_type_in_vec(inner, ctx),
        IrNode::Map { fn_id, .. } => {
            // Map determines its own type from FnDescriptor.
            match &ctx.ir.fns[*fn_id as usize] {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                FnDescriptor::Custom { return_type, source } => {
                    return_type.clone().unwrap_or(TypeDesc::Named(*source))
                }
            }
        }
        // Alt: try in_vec inference. Only apply if branches are homogeneous
        // with in_vec (otherwise coercion produces BoxedEnum, defeating in_vec).
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let first = infer_node_type_in_vec(&branches[0].node, ctx);
            let all_same = branches[1..]
                .iter()
                .all(|b| infer_node_type_in_vec(&b.node, ctx) == first);
            if all_same {
                first
            } else {
                // Heterogeneous even with in_vec — fall back to standard inference.
                infer_node_type(node, ctx)
            }
        }
        _ => infer_node_type(node, ctx),
    }
}
