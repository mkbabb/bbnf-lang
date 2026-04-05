//! Core recursive type projection logic for IR nodes.

use crate::{FnDescriptor, IrNode, TypeDesc};

use super::utils::{ProjectionCtx, try_flatten_pair};

/// Project the output type of a single IR node.
pub fn project_node(node: &IrNode, ctx: &ProjectionCtx<'_>) -> TypeDesc {
    let ty = project_node_inner(node, ctx);
    if let Some(rec) = ctx.recorder {
        rec.record_node(node, &ty);
    }
    ty
}

fn project_node_inner(node: &IrNode, ctx: &ProjectionCtx<'_>) -> TypeDesc {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) => TypeDesc::Span,

        // Epsilon produces an empty Span in monolithic codegen:
        // `Span::new(state.offset, state.offset, state.src)`.
        // Must match codegen output so sub-variant enum types are correct.
        IrNode::Epsilon => TypeDesc::Span,

        IrNode::Ref(_id) => {
            // BoxedEnum: emit_ref wraps non-transparent calls with Box::new.
            // Transparent refs also return Box<Enum>.
            // The insert_recursion_boxing post-pass converts Vec<BoxedEnum>
            // → Vec<Enum> where Vec provides sufficient heap indirection.
            TypeDesc::BoxedEnum
        }

        IrNode::Seq(children) => {
            // Seq projection with span-method override and span preservation.
            project_seq(children, ctx)
        }

        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let consumed = ctx.consumed();
            let first = project_node(&branches[0].node, &consumed);
            let all_same = branches[1..]
                .iter()
                .all(|b| project_node(&b.node, &consumed) == first);
            if all_same { first } else { TypeDesc::BoxedEnum }
        }

        IrNode::Repeat { inner, lo, hi } => {
            // Nested: consume preserve_spans.
            let consumed = ctx.consumed();

            if *lo == 0 && *hi == 1 {
                // Optional.
                let inner_ty = project_node(inner, &consumed);
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    // Phase 1a: transparent refs get unboxed in Optional context.
                    // Codegen emits _unboxed().opt() → Option<Enum>.
                    if let IrNode::Ref(rule_id) = inner.as_ref() {
                        let rule = &ctx.ir.rules[*rule_id as usize];
                        if rule.meta.is_transparent {
                            return TypeDesc::Option(Box::new(TypeDesc::Enum));
                        }
                    }
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else {
                // Many / Many1: use in_vec projection for inner elements.
                // Vec provides heap indirection, so Box is unnecessary.
                let inner_ty = project_node_in_vec(inner, &consumed);
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    TypeDesc::Vec(Box::new(inner_ty))
                }
            }
        }

        IrNode::Skip(left, _) => {
            let consumed = ctx.consumed();
            project_node(left, &consumed)
        }
        IrNode::Next(_, right) => {
            let consumed = ctx.consumed();
            project_node(right, &consumed)
        }
        IrNode::Minus(left, _) => {
            let consumed = ctx.consumed();
            project_node(left, &consumed)
        }

        IrNode::Negate(_) => TypeDesc::Tuple(vec![]),

        IrNode::OptionalWhitespace(inner) => project_node(inner, ctx),

        IrNode::Map { inner: _, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                FnDescriptor::NumberConvert => TypeDesc::F64,
                FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                FnDescriptor::SpanCapture => TypeDesc::Span,
                FnDescriptor::Expr { return_type, .. } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        // No return type annotation — falls back to Span (raw parse result).
                        TypeDesc::Span
                    }
                }
            }
        }

        // TokenDispatch is a heterogeneous alternation dispatched by token value.
        IrNode::TokenDispatch { .. } => TypeDesc::BoxedEnum,
    }
}

/// Project the output type of a single IR node in a Vec context.
///
/// Identical to `project_node` except `Ref` returns `Enum` for non-transparent rules
/// (since Vec provides heap indirection, Box is unnecessary). Transparent rules
/// still return `BoxedEnum` since they box internally.
///
/// The `in_vec` context propagates through Skip (left), Next (right), Minus (left),
/// Map, and OptionalWhitespace — the same nodes that propagate `in_vec` in codegen.
/// It does NOT propagate into Seq children (multi-element Seq produces a tuple),
/// Alt branches (they produce compound types), or Repeat (which starts its own context).
pub fn project_node_in_vec(node: &IrNode, ctx: &ProjectionCtx<'_>) -> TypeDesc {
    let ty = project_node_in_vec_inner(node, ctx);
    if let Some(rec) = ctx.recorder {
        rec.record_vec_elem(node, &ty);
    }
    ty
}

fn project_node_in_vec_inner(node: &IrNode, ctx: &ProjectionCtx<'_>) -> TypeDesc {
    match node {
        // In Vec context, ALL refs return Enum (no boxing needed).
        // Non-transparent: codegen emits Self::rule() without Box.
        // Transparent: codegen emits Self::rule_unboxed() which returns Enum directly.
        IrNode::Ref(_) => TypeDesc::Enum,
        IrNode::Skip(left, _) => {
            let consumed = ctx.consumed();
            project_node_in_vec(left, &consumed)
        }
        IrNode::Next(_, right) => {
            let consumed = ctx.consumed();
            project_node_in_vec(right, &consumed)
        }
        IrNode::Minus(left, _) => {
            let consumed = ctx.consumed();
            project_node_in_vec(left, &consumed)
        }
        IrNode::OptionalWhitespace(inner) => project_node_in_vec(inner, ctx),
        IrNode::Map { inner: _, fn_id } => {
            // Map determines its own type from FnDescriptor, not from inner.
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
                FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
                FnDescriptor::NumberConvert => TypeDesc::F64,
                FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                FnDescriptor::SpanCapture => TypeDesc::Span,
                FnDescriptor::Expr { return_type, .. } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Span
                    }
                }
            }
        }
        // Alt: try in_vec projection. Only apply if branches are homogeneous
        // with in_vec (otherwise coercion produces BoxedEnum, defeating in_vec).
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let consumed = ctx.consumed();
            let first = project_node_in_vec(&branches[0].node, &consumed);
            let all_same = branches[1..]
                .iter()
                .all(|b| project_node_in_vec(&b.node, &consumed) == first);
            if all_same {
                first
            } else {
                // Heterogeneous even with in_vec — fall back to standard projection.
                project_node(node, ctx)
            }
        }
        // For all other nodes (Seq, Repeat, Literal, Regex, Epsilon, Negate),
        // delegate to project_node.
        _ => project_node(node, ctx),
    }
}

/// Project the output type of a Seq (concatenation) node.
///
/// Applies:
/// - Span-method override (with safety guard)
/// - Span preservation (consumed at top-level Seq)
/// - Consecutive-Span compression
/// - `(T, Vec<T>)` flattening
fn project_seq(children: &[IrNode], ctx: &ProjectionCtx<'_>) -> TypeDesc {
    if children.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    if children.len() == 1 {
        return project_node(&children[0], ctx);
    }

    // Span-method override: Ref to rules with _sp() methods produce Span.
    // Matches emit_seq's dispatch: refs to rules with _sp() methods get their
    // _sp() method called (producing Span) instead of the normal parser
    // (producing BoxedEnum). Transparent rules are excluded because the codegen
    // doesn't override them.
    let child_types: Vec<TypeDesc> = children
        .iter()
        .map(|c| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    // Record the overridden type so codegen can look it up.
                    if let Some(rec) = ctx.recorder {
                        rec.record_node(c, &TypeDesc::Span);
                    }
                    return TypeDesc::Span;
                }
            }
            let consumed = ctx.consumed();
            project_node(c, &consumed)
        })
        .collect();

    // Span-method safety guard: when all children are Span after the override,
    // decide whether to keep the override (collapsing the whole Seq to Span) or
    // revert it.
    //
    // Keep override when: every child is EITHER a naturally-Span leaf (Literal/
    // Regex/Epsilon/inlined) OR a span-method overridden Ref, AND !preserve_spans.
    // This limits the optimization to simple Seqs like `(propertyName, ":", value)`
    // where compression to Span is unambiguous.
    //
    // Revert override when: any child is Span through projection of a complex
    // expression (Repeat, Skip, etc.) — these have different compression behavior
    // between IR and codegen, causing type mismatches.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    // Check if every child is Span through simple, unambiguous means:
    // either naturally Span (leaf), span-method overridden Ref, or projects to
    // Span through the same logic the codegen will use.
    let all_simple_span = all_span
        && ctx.ir.collapse_simple_spans
        && !ctx.rules.preserve_spans
        && children.iter().zip(child_types.iter()).all(|(c, ty)| {
            // Optional(Span) produces Option<Span> at runtime, not Span —
            // exclude from simple-Span collapse to match codegen behavior in seq.rs.
            if let IrNode::Repeat { lo: 0, hi: 1, .. } = c {
                return false;
            }
            // Span-method overridden Ref — codegen will call _sp().
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    return true;
                }
            }
            // Naturally Span — leaf or collapsed expression.
            // Only safe when the child ALWAYS projects to Span regardless of context.
            *ty == TypeDesc::Span
        });
    let effective_types = if all_span && !all_simple_span {
        children
            .iter()
            .map(|c| {
                let consumed = ctx.consumed();
                project_node(c, &consumed)
            })
            .collect::<Vec<_>>()
    } else {
        child_types
    };

    // Consume preserve_spans flag (only applies to top-level Seq).
    let preserve_spans =
        ctx.rules.preserve_spans && effective_types.iter().all(|t| *t == TypeDesc::Span);

    // Record effective child types BEFORE compression so codegen can look them up.
    // Also record whether preserve_spans was applied for this Seq.
    //
    // When the safety guard reverts the span-method override, re-record per-node
    // types for children whose override was reverted. Without this, the per-node
    // TypeMap retains the overridden Span type, but seq_child_types returns the
    // correct non-override type, causing divergence between the two lookup paths.
    if let Some(rec) = ctx.recorder {
        // Re-record per-node types to match effective_types (fixes span-method revert divergence).
        for (c, ty) in children.iter().zip(effective_types.iter()) {
            rec.record_node(c, ty);
        }
        rec.record_seq_children(children, &effective_types);
        rec.record_seq_preserve_spans(children, preserve_spans);
    }

    // Consecutive Span compression (skipped when preserve_spans is set).
    let compressed = if preserve_spans {
        effective_types
    } else {
        let mut result: Vec<TypeDesc> = Vec::new();
        let mut in_span_run = false;
        for ty in &effective_types {
            if *ty == TypeDesc::Span {
                if !in_span_run {
                    result.push(TypeDesc::Span);
                    in_span_run = true;
                }
            } else {
                result.push(ty.clone());
                in_span_run = false;
            }
        }
        result
    };

    // Compute final result type.
    let result = if compressed.len() == 1 {
        // Single-element unwrap.
        compressed
            .into_iter()
            .next()
            .expect("compressed Seq verified to have exactly one element")
    } else if compressed.len() == 2 {
        // (T, Vec<T>) → Vec<T> flattening.
        try_flatten_pair(&compressed[0], &compressed[1])
            .unwrap_or_else(|| TypeDesc::Tuple(compressed))
    } else {
        TypeDesc::Tuple(compressed)
    };

    // Record the Seq's final result type for codegen flattening decisions.
    if let Some(rec) = ctx.recorder {
        rec.record_seq_result(children, &result);
    }

    result
}
