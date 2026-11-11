//! Repeat node compilation.
//!
//! Decisions (shared across all backends):
//! - the strict separated list `(element (sep element)*)?` lowers through
//!   `emit_sep_by` (`bbnf_ir::passes::inspect::sep_list`). A
//!   `( element << sep ? ) *` repetition reads literally (ruling S1-B,
//!   R-b: spec text first) and compiles as the repeat it is.
//! - optional (`0..1`) vs many (`0+`, `1+`)
//!
//! # `TypeDesc` plumbing (Tranche AF.1)
//!
//! `emit_sep_by` / `emit_repeat_optional` / `emit_repeat_many` all
//! destructure their respective `elem_type` / `inner_type` parameter
//! as `_elem_type` / `_inner_type` across every backend under
//! tape-first emission. AF.1 keeps the driver's classification
//! (variant inspection for `child_alloc` routing) working against
//! live borrows out of `TypeMap`, then hands the emitter a
//! [`PLACEHOLDER_TY`] reference so the dead parameter never forces
//! a clone or an allocation.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};

use super::DriverState;
use super::node::compile_node;
use crate::backend::{Emitter, SepByConfig, ValuePlacement};

/// Shared unit-variant stand-in for the dead `elem_type` /
/// `inner_type` emitter parameters. See the module doc comment for
/// background.
static PLACEHOLDER_TY: TypeDesc = TypeDesc::Span;

/// Classify a Repeat's element type as a lightweight variant tag
/// drawn from the live `TypeMap` without cloning. Collapses the full
/// `TypeDesc` space to the four variants the downstream alloc
/// routing actually distinguishes:
///
/// - `Span`  — span-compressible children (the TypeMap reports `Span`
///   either directly or implicitly when the node has no entry and
///   the kind is `Literal`/`Regex`/`Epsilon`).
/// - `Enum`  — unboxed enum references (the TypeMap reports `Enum`
///   directly, or the fallback kicks in on `Ref(_)` with no type).
/// - `BoxedEnum` — needs `ValuePlacement::Alloc` so the inner `Ref`
///   produces `&'a Enum` from a slab-allocated `&mut Enum`.
/// - `Other` — everything else (`Vec<_>`, `Tuple<_>`, `Option<_>`,
///   `Named(_)`, `F64`, `U32`). The backends don't inspect these on
///   the repeat path; the driver only cares that the child isn't
///   one of the span/enum fast-paths.
#[derive(Clone, Copy, PartialEq, Eq)]
enum RepeatElemKind {
    Span,
    Enum,
    BoxedEnum,
    Other,
}

impl RepeatElemKind {
    #[inline]
    fn from_type(ty: &TypeDesc) -> Self {
        match ty {
            TypeDesc::Span => Self::Span,
            TypeDesc::Enum => Self::Enum,
            TypeDesc::BoxedEnum => Self::BoxedEnum,
            _ => Self::Other,
        }
    }
}

/// The `lo..=hi` iteration bounds of an `IrNode::Repeat`
/// (`hi == u32::MAX` is unbounded).
pub(super) struct RepeatBounds {
    pub lo: u32,
    pub hi: u32,
}

/// Compile a Repeat node.
pub(super) fn compile_repeat<E: Emitter>(
    inner: &IrNode,
    RepeatBounds { lo, hi }: RepeatBounds,
    alloc: ValuePlacement,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    // `(x (sep x)*)?` -- the zero-or-more strict separated list
    // (ruling S1-B). Its loop accepts exactly the literal reading.
    if lo == 0
        && hi == 1
        && let Some(list) = bbnf_ir::passes::inspect::sep_list(inner)
    {
        return compile_sep_list(
            list.element,
            list.separator,
            0,
            false,
            None,
            ir,
            dstate,
            emitter,
            ctx,
        );
    }

    // Optional (0..1) vs many.
    if lo == 0 && hi == 1 {
        // BoxedEnum optionals need Alloc so the inner Ref produces &'a Enum.
        let inner_alloc = match ir.node_type(inner) {
            Some(TypeDesc::BoxedEnum) => ValuePlacement::Alloc,
            _ => ValuePlacement::Inline,
        };
        let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
        return emitter.emit_repeat_optional(body, &PLACEHOLDER_TY, alloc, ctx);
    }

    // Use vec_elem_type for repeat-many: element type for scratch
    // Vec collection, mapping BoxedEnum → Enum since scratch Vecs
    // store unboxed values. Structural fallback (when neither map
    // carries a type) distinguishes span-compressible leaves from
    // refs from compound children.
    let elem_kind = match (ir.vec_elem_type(inner), ir.node_type(inner)) {
        (Some(t), _) => RepeatElemKind::from_type(t),
        (None, Some(t)) if *t == TypeDesc::BoxedEnum => RepeatElemKind::Enum,
        (None, Some(t)) => RepeatElemKind::from_type(t),
        (None, None) => match inner {
            IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => RepeatElemKind::Span,
            IrNode::Ref(_) => RepeatElemKind::Enum,
            _ => RepeatElemKind::BoxedEnum,
        },
    };
    // Override: when parent forces Alloc (Vec(non-Span) expected), prevent
    // Span compression even if elem_kind fell back to Span from TypeMap.
    let elem_kind = if alloc == ValuePlacement::Alloc && elem_kind == RepeatElemKind::Span {
        RepeatElemKind::Enum
    } else {
        elem_kind
    };
    let inner_alloc = if elem_kind == RepeatElemKind::BoxedEnum {
        ValuePlacement::Alloc
    } else {
        ValuePlacement::Inline
    };
    let body = compile_node(inner, inner_alloc, ir, dstate, emitter, ctx);
    emitter.emit_repeat_many(body, lo, hi, &PLACEHOLDER_TY, ctx)
}

/// Element type tag handed to `emit_sep_by` / `emit_repeat_many`: the
/// TS backend collects non-span elements into an array.
static ENUM_TY: TypeDesc = TypeDesc::Enum;

/// Compile a strict separated list `element (separator element)*`
/// with at least `lo` elements through `emit_sep_by`. `ws` and
/// `terminator_bytes` carry the delimited-wrap context (`wrap.rs`).
#[allow(clippy::too_many_arguments)]
pub(super) fn compile_sep_list<E: Emitter>(
    element: &IrNode,
    separator: &IrNode,
    lo: u32,
    ws: bool,
    terminator_bytes: Option<Vec<u8>>,
    ir: &GrammarIR,
    dstate: &mut DriverState,
    emitter: &mut E,
    ctx: &mut E::Ctx,
) -> E::Output {
    let elem_kind = match ir.vec_elem_type(element) {
        Some(t) => RepeatElemKind::from_type(t),
        None => match ir.node_type(element) {
            Some(t) if *t == TypeDesc::BoxedEnum => RepeatElemKind::Enum,
            Some(t) => RepeatElemKind::from_type(t),
            None => RepeatElemKind::Span,
        },
    };
    let elem_alloc = if elem_kind == RepeatElemKind::BoxedEnum {
        ValuePlacement::Alloc
    } else {
        ValuePlacement::Inline
    };
    let element_out = compile_node(element, elem_alloc, ir, dstate, emitter, ctx);
    let sep_out = compile_node(separator, ValuePlacement::Inline, ir, dstate, emitter, ctx);
    let config = SepByConfig {
        ws,
        lo,
        terminator_bytes,
    };
    let elem_ty = if elem_kind == RepeatElemKind::Span {
        &PLACEHOLDER_TY
    } else {
        &ENUM_TY
    };
    emitter.emit_sep_by(element_out, sep_out, &config, elem_ty, ctx)
}
