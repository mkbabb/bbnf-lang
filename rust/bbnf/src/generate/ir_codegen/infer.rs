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

            // (T, Vec<T>) flattening — matches emit_seq (same-type only).
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
            if *lo == 0 && *hi == 1 {
                let inner_ty = infer_node_type(inner, ctx);
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    // Transparent Ref nodes get unboxed in Optional context.
                    // Codegen emits _unboxed().opt() → Option<Enum>.
                    // Non-transparent Refs keep arena ref: rule().opt() → Option<&'a Enum>.
                    if let IrNode::Ref(rule_id) = inner.as_ref() {
                        let rule = &ctx.ir.rules[*rule_id as usize];
                        if rule.meta.is_transparent {
                            return TypeDesc::Option(Box::new(TypeDesc::Enum));
                        }
                        // Non-transparent falls through to default: Option<BoxedEnum>
                    }
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else {
                // Vec-producing: use elide_box inference (Ref → Enum, not BoxedEnum).
                let inner_ty = infer_node_type_elide_box(inner, ctx);
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

/// Quick type inference for a single IrNode in an elide_box context.
///
/// Identical to `infer_node_type` except `Ref` returns `Enum` for ALL rules
/// (since the parent provides heap indirection, Box is unnecessary), and
/// `BoxWrap` returns `Enum` instead of `BoxedEnum`.
///
/// Propagates through Skip (left), Next (right), Minus (left), Map, OW — matching
/// the codegen `elide_box` propagation.
pub fn infer_node_type_elide_box(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    match node {
        // In elide_box context, ALL refs return Enum (no boxing needed).
        // Codegen emits Self::rule_unboxed() which returns Enum directly.
        IrNode::Ref(_) => TypeDesc::Enum,
        IrNode::Skip(left, _) => infer_node_type_elide_box(left, ctx),
        IrNode::Next(_, right) => infer_node_type_elide_box(right, ctx),
        IrNode::Minus(left, _) => infer_node_type_elide_box(left, ctx),
        IrNode::OptionalWhitespace(inner) => infer_node_type_elide_box(inner, ctx),
        IrNode::Map { fn_id, .. } => {
            // Map determines its own type from FnDescriptor.
            match &ctx.ir.fns[*fn_id as usize] {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                // BoxWrap is elided in elide_box context — returns Enum, not BoxedEnum.
                FnDescriptor::BoxWrap => TypeDesc::Enum,
                FnDescriptor::Custom {
                    return_type,
                    source,
                } => return_type.clone().unwrap_or(TypeDesc::Named(*source)),
            }
        }
        // Alt: try elide_box inference. Only apply if branches are homogeneous
        // with elide_box (otherwise coercion produces BoxedEnum, defeating elide_box).
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let first = infer_node_type_elide_box(&branches[0].node, ctx);
            let all_same = branches[1..]
                .iter()
                .all(|b| infer_node_type_elide_box(&b.node, ctx) == first);
            if all_same {
                first
            } else {
                // Heterogeneous even with elide_box — fall back to standard inference.
                infer_node_type(node, ctx)
            }
        }
        _ => infer_node_type(node, ctx),
    }
}
