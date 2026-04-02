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
            // B.1 + B.2: Seq projection with sp_method_rules override and preserve_spans.
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
                // B.3: Use parsed return type if available.
                FnDescriptor::Custom {
                    return_type,
                    source,
                } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*source)
                    }
                }
                FnDescriptor::NumberConvert => TypeDesc::F64,
                FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                FnDescriptor::Constant { return_type, value } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*value)
                    }
                }
                FnDescriptor::SpanCapture => TypeDesc::Span,
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
                FnDescriptor::Custom {
                    return_type,
                    source,
                } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*source)
                    }
                }
                FnDescriptor::NumberConvert => TypeDesc::F64,
                FnDescriptor::HexConvert { .. } => TypeDesc::U32,
                FnDescriptor::Constant { return_type, value } => {
                    if let Some(rt) = return_type {
                        rt.clone()
                    } else {
                        TypeDesc::Named(*value)
                    }
                }
                FnDescriptor::SpanCapture => TypeDesc::Span,
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
/// - B.1: sp_method_rules Span override (with all-Span guard)
/// - B.2: @pretty tuple preservation (consume flag)
/// - Consecutive-Span compression
/// - `(T, Vec<T>)` flattening
fn project_seq(children: &[IrNode], ctx: &ProjectionCtx<'_>) -> TypeDesc {
    if children.is_empty() {
        return TypeDesc::Tuple(vec![]);
    }
    if children.len() == 1 {
        return project_node(&children[0], ctx);
    }

    // B.1: Override Ref to rules with _sp() methods with Span type.
    // Matches emit_seq's sp_method_rules override: refs to rules with _sp()
    // methods get their _sp() method called (producing Span) instead of the
    // normal parser (producing BoxedEnum). Transparent rules are excluded
    // because the codegen doesn't override them.
    let child_types: Vec<TypeDesc> = children
        .iter()
        .map(|c| {
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                if rule.meta.has_sp_method && !rule.meta.is_transparent {
                    // B.1 override: record the overridden type so codegen can look it up.
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

    // B.1 guard: when all children are Span after B.1 override, decide whether
    // to keep the override (collapsing the whole Seq to Span) or undo it.
    //
    // Keep B.1 when: every child is EITHER a naturally-Span leaf (Literal/Regex/
    // Epsilon/inlined) OR a B.1-overridden Ref, AND !preserve_spans.
    // This limits the optimization to simple Seqs like `(propertyName, ":", value)`
    // where compression to Span is unambiguous.
    //
    // Undo B.1 when: any child is Span through projection of a complex expression
    // (Repeat, Skip, etc.) — these have different compression behavior between
    // IR and codegen, causing type mismatches.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    // Check if every child is Span through simple, unambiguous means:
    // either naturally Span (leaf), B.1-overridden Ref, or projects to Span
    // through the same logic the codegen will use.
    let all_simple_span = all_span
        && ctx.ir.b1_span_collapse
        && !ctx.rules.preserve_spans
        && children.iter().zip(child_types.iter()).all(|(c, ty)| {
            // Optional(Span) produces Option<Span> at runtime, not Span —
            // exclude from B.1 collapse to match codegen behavior in seq.rs.
            if let IrNode::Repeat { lo: 0, hi: 1, .. } = c {
                return false;
            }
            // B.1-overridden Ref — codegen will call _sp().
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

    // B.2: Consume preserve_spans flag. Only the top-level Seq preserves all-Span tuples.
    let preserve_spans =
        ctx.rules.preserve_spans && effective_types.iter().all(|t| *t == TypeDesc::Span);

    // Record effective child types BEFORE compression so codegen can look them up.
    // Also record whether preserve_spans was applied for this Seq.
    //
    // When the all-Span guard undoes B.1, re-record per-node types for children
    // whose B.1 override was reverted. Without this, the per-node TypeMap retains
    // the B.1 Span type, but seq_child_types returns the correct non-B.1 type,
    // causing a disagreement between the two lookup paths in codegen.
    if let Some(rec) = ctx.recorder {
        // Re-record per-node types to match effective_types (fixes B.1-undo divergence).
        for (c, ty) in children.iter().zip(effective_types.iter()) {
            rec.record_node(c, ty);
        }
        rec.record_seq_children(children, &effective_types);
        rec.record_seq_preserve_spans(children, preserve_spans);
    }

    // Consecutive Span compression (skip if preserve_spans).
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
