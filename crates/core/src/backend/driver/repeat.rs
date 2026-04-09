//! Repeat node compilation.
//!
//! Decisions (shared across all backends):
//! - sep_by pattern detection (`Repeat(Skip(element, Repeat(sep, 0, 1)), lo, MAX)`)
//! - optional (`0..1`) vs many (`0+`, `1+`)

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::patterns::decisions;
use crate::backend::{Emitter, SepByConfig, ValuePlacement};

/// Compile a Repeat node.
pub(super) fn compile_repeat<E: Emitter>(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let type_map = ir.type_map.as_ref();

    // sep_by: Repeat(Skip(element, Repeat(separator, 0, 1)), lo, MAX).
    if hi == u32::MAX {
        if let Some((element, separator)) = decisions::detect_sep_by(inner) {
            let elem_type = type_map
                .and_then(|tm| tm.vec_elem_type(element).cloned())
                .or_else(|| {
                    type_map.and_then(|tm| {
                        let ty = tm.node_type(element).cloned()?;
                        Some(if ty == TypeDesc::BoxedEnum {
                            TypeDesc::Enum
                        } else {
                            ty
                        })
                    })
                })
                .unwrap_or(TypeDesc::Span);

            let elem_alloc = if elem_type == TypeDesc::BoxedEnum {
                ValuePlacement::Alloc
            } else {
                ValuePlacement::Inline
            };
            let element_out = compile_node(element, elem_alloc, ir, dstate, emitter, ctx);
            let sep_out =
                compile_node(separator, ValuePlacement::Inline, ir, dstate, emitter, ctx);

            let config = SepByConfig {
                ws: false,
                lo,
                terminator_bytes: None,
            };

            return emitter.emit_sep_by(element_out, sep_out, &config, &elem_type, ctx);
        }
    }

    // Optional (0..1) vs many.
    if lo == 0 && hi == 1 {
        let inner_type = type_map
            .and_then(|tm| tm.node_type(inner).cloned())
            .unwrap_or(TypeDesc::Span);
        // BoxedEnum optionals need Alloc so the inner Ref produces &'a Enum.
        let inner_alloc = if matches!(inner_type, TypeDesc::BoxedEnum) {
            ValuePlacement::Alloc
        } else {
            ValuePlacement::Inline
        };
        let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
        return emitter.emit_repeat_optional(body, &inner_type, alloc, ctx);
    }

    // Use vec_elem_type for repeat-many: element type for scratch Vec
    // collection, mapping BoxedEnum → Enum since scratch Vecs store
    // unboxed values.
    let elem_type = type_map
        .and_then(|tm| tm.vec_elem_type(inner).cloned())
        .or_else(|| {
            type_map.and_then(|tm| {
                let ty = tm.node_type(inner).cloned()?;
                Some(if ty == TypeDesc::BoxedEnum {
                    TypeDesc::Enum
                } else {
                    ty
                })
            })
        })
        .unwrap_or_else(|| match inner {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => TypeDesc::Span,
            IrNode::Ref(_) => TypeDesc::Enum,
            _ => TypeDesc::BoxedEnum,
        });
    // Override: when parent forces Alloc (Vec(non-Span) expected), prevent
    // Span compression even if elem_type fell back to Span from TypeMap.
    let elem_type = if alloc == ValuePlacement::Alloc && elem_type == TypeDesc::Span {
        TypeDesc::Enum
    } else {
        elem_type
    };
    let inner_alloc = if elem_type == TypeDesc::BoxedEnum {
        ValuePlacement::Alloc
    } else {
        ValuePlacement::Inline
    };
    let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
    emitter.emit_repeat_many(body, lo, hi, &elem_type, ctx)
}
