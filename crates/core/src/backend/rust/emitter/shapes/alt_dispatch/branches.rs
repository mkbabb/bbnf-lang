//! Per-branch tape-path emission: dispatch-arm assembly, structural
//! branch-body routing, and exact emitted-payload planning.

use bbnf_ir::{
    AltBranch, FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, NumericDomain, TypeDesc,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::emit_ref_call_shape;

/// Exact scalar carried by a grammar map.
///
/// `MapExpr::IntLit` preserves signed syntax and `MapExpr::UIntLit` preserves
/// exact unsigned syntax. The descriptor's declared return type remains the
/// authority for scalar width. Centralising that interpretation prevents the
/// individual shape emitters from reinterpreting signs or silently truncating
/// narrow constants.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(in crate::backend::rust::emitter::shapes) enum ScalarPayload {
    U8(u8),
    I64(i64),
    U64(u64),
    F64(f64),
    Bool(bool),
}

/// Decode a constant scalar map without truncation.
///
/// `Ok(None)` means the descriptor is not a scalar constant. `Err` means the
/// value contradicts its declared scalar type; callers emit a hard compile
/// error instead of fabricating a value.
pub(in crate::backend::rust::emitter::shapes) fn scalar_payload_from_fn(
    descriptor: &FnDescriptor,
) -> Result<Option<ScalarPayload>, &'static str> {
    let FnDescriptor::Expr { expr, return_type } = descriptor else {
        return Ok(None);
    };
    let mut folded = expr.clone();
    checked_fold_map_expr(&mut folded, return_type.as_ref())?;
    let payload = match &folded {
        MapExpr::IntLit(value) => match return_type.as_ref() {
            Some(TypeDesc::U8) => ScalarPayload::U8(
                u8::try_from(*value).map_err(|_| "u8 map constant is out of range")?,
            ),
            Some(TypeDesc::I8) => ScalarPayload::I64(
                i8::try_from(*value).map_err(|_| "i8 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I16) => ScalarPayload::I64(
                i16::try_from(*value).map_err(|_| "i16 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I32) => ScalarPayload::I64(
                i32::try_from(*value).map_err(|_| "i32 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I64) => ScalarPayload::I64(*value),
            Some(TypeDesc::U16) => ScalarPayload::U64(
                u16::try_from(*value).map_err(|_| "u16 map constant is out of range")? as u64,
            ),
            Some(TypeDesc::U32) => ScalarPayload::U64(
                u32::try_from(*value).map_err(|_| "u32 map constant is out of range")? as u64,
            ),
            Some(TypeDesc::U64) => ScalarPayload::U64(
                u64::try_from(*value).map_err(|_| "u64 map constant is negative")?,
            ),
            // `IntLit` is signed by construction, including when untyped.
            None => ScalarPayload::I64(*value),
            _ => return Err("integer map constant has a non-integer return type"),
        },
        MapExpr::UIntLit(value) => match return_type.as_ref() {
            Some(TypeDesc::U8) => ScalarPayload::U8(
                u8::try_from(*value).map_err(|_| "u8 map constant is out of range")?,
            ),
            Some(TypeDesc::I8) => ScalarPayload::I64(
                i8::try_from(*value).map_err(|_| "i8 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I16) => ScalarPayload::I64(
                i16::try_from(*value).map_err(|_| "i16 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I32) => ScalarPayload::I64(
                i32::try_from(*value).map_err(|_| "i32 map constant is out of range")? as i64,
            ),
            Some(TypeDesc::I64) => ScalarPayload::I64(
                i64::try_from(*value).map_err(|_| "i64 map constant is out of range")?,
            ),
            Some(TypeDesc::U16) => ScalarPayload::U64(
                u16::try_from(*value).map_err(|_| "u16 map constant is out of range")? as u64,
            ),
            Some(TypeDesc::U32) => ScalarPayload::U64(
                u32::try_from(*value).map_err(|_| "u32 map constant is out of range")? as u64,
            ),
            Some(TypeDesc::U64) | None => ScalarPayload::U64(*value),
            _ => return Err("unsigned map constant has a non-integer return type"),
        },
        MapExpr::FloatLit(value) => {
            if !value.is_finite() {
                return Err("float map constant is not finite");
            }
            match return_type.as_ref() {
                Some(TypeDesc::F64) | None => ScalarPayload::F64(*value),
                _ => return Err("float map constant has a non-float return type"),
            }
        }
        MapExpr::BoolLit(value) => match return_type.as_ref() {
            Some(TypeDesc::Bool) | None => ScalarPayload::Bool(*value),
            _ => return Err("boolean map constant has a non-boolean return type"),
        },
        MapExpr::BinOp { .. } | MapExpr::UnaryOp { .. } if folded.is_constant() => {
            return Err("constant map expression could not be folded exactly");
        }
        _ => return Ok(None),
    };
    Ok(Some(payload))
}

fn checked_fold_map_expr(
    expr: &mut MapExpr,
    return_type: Option<&TypeDesc>,
) -> Result<(), &'static str> {
    let domain = return_type.and_then(NumericDomain::from_type_desc);
    expr.checked_fold(domain).map_err(|error| error.message())
}

/// Materialise a scalar payload on the trait-qualified builder surface.
pub(in crate::backend::rust::emitter::shapes) fn scalar_payload_push(
    payload: ScalarPayload,
) -> TokenStream {
    match payload {
        ScalarPayload::U8(value) => quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_u8(builder, #value);
        },
        ScalarPayload::I64(value) => quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_i64(builder, #value);
        },
        ScalarPayload::U64(value) => quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_u64(builder, #value);
        },
        ScalarPayload::F64(value) => quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_f64(builder, #value);
        },
        ScalarPayload::Bool(value) => quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_bool(builder, #value);
        },
    }
}

/// Resolve one descriptor to the exact semantic builder operation it owns.
///
/// This is the sole Rust-shape authority for deciding whether a map emits a
/// real payload. Scalar expressions are folded before decoding; string
/// literals resolve through the grammar string table and emit their semantic
/// value, rather than the source slice matched by the map's inner parser.
pub(in crate::backend::rust::emitter::shapes) fn payload_push_from_fn(
    descriptor: &FnDescriptor,
    ir: &GrammarIR,
) -> Result<Option<TokenStream>, &'static str> {
    let FnDescriptor::Expr { expr, return_type } = descriptor else {
        return Ok(None);
    };
    let mut folded = expr.clone();
    checked_fold_map_expr(&mut folded, return_type.as_ref())?;
    if let MapExpr::StringLit(sid) = folded {
        if !matches!(return_type, None | Some(TypeDesc::Span)) {
            return Err("string map constant has a non-string return type");
        }
        let Some(value) = ir.strings.get(sid as usize) else {
            return Err("string map constant references a missing string");
        };
        return Ok(Some(quote! {
            <_ as crate::runtime::StructBuilder>::push_leaf_with_str(builder, #value);
        }));
    }
    scalar_payload_from_fn(descriptor).map(|payload| payload.map(scalar_payload_push))
}

/// Exact extrema of committed payloads over every successful emitted path.
///
/// `Range { min, max: Some(n) }` retains concrete counts; an exact arity is
/// represented only when `min == n == max`. `max: None` means a proven
/// unbounded repetition, not a qualitative "many" guess. `Pending` exists only
/// during the recursive-rule fixed point. Any path for which the active shape
/// emitter has no proof becomes `Unsupported` and poisons its enclosing plan.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(in crate::backend::rust::emitter::shapes) enum EmittedPayloadArity {
    Pending,
    Unsupported,
    Range { min: u32, max: Option<u32> },
}

impl EmittedPayloadArity {
    const ZERO: Self = Self::Range {
        min: 0,
        max: Some(0),
    };
    const ONE: Self = Self::Range {
        min: 1,
        max: Some(1),
    };

    fn exact(value: u32) -> Self {
        Self::Range {
            min: value,
            max: Some(value),
        }
    }

    fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unsupported, _) | (_, Self::Unsupported) => Self::Unsupported,
            (Self::Pending, rhs) => rhs,
            (lhs, Self::Pending) => lhs,
            (
                Self::Range {
                    min: lhs_min,
                    max: lhs_max,
                },
                Self::Range {
                    min: rhs_min,
                    max: rhs_max,
                },
            ) => Self::Range {
                min: lhs_min.min(rhs_min),
                max: match (lhs_max, rhs_max) {
                    (Some(lhs), Some(rhs)) => Some(lhs.max(rhs)),
                    _ => None,
                },
            },
        }
    }

    fn then(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unsupported, _) | (_, Self::Unsupported) => Self::Unsupported,
            (Self::Pending, _) | (_, Self::Pending) => Self::Pending,
            (
                Self::Range {
                    min: lhs_min,
                    max: lhs_max,
                },
                Self::Range {
                    min: rhs_min,
                    max: rhs_max,
                },
            ) => {
                let Some(min) = lhs_min.checked_add(rhs_min) else {
                    return Self::Unsupported;
                };
                let max = match (lhs_max, rhs_max) {
                    (Some(lhs), Some(rhs)) => match lhs.checked_add(rhs) {
                        Some(value) => Some(value),
                        None => return Self::Unsupported,
                    },
                    _ => None,
                };
                Self::Range { min, max }
            }
        }
    }

    fn repeat(self, lo: u32, hi: u32) -> Self {
        if hi < lo {
            return Self::Unsupported;
        }
        if hi == 0 {
            return Self::ZERO;
        }
        match self {
            Self::Pending | Self::Unsupported => self,
            Self::Range { min, max } => {
                let Some(min) = min.checked_mul(lo) else {
                    return Self::Unsupported;
                };
                let max = if hi == u32::MAX {
                    match max {
                        Some(0) => Some(0),
                        _ => None,
                    }
                } else {
                    match max {
                        Some(value) => match value.checked_mul(hi) {
                            Some(product) => Some(product),
                            None => return Self::Unsupported,
                        },
                        None => None,
                    }
                };
                Self::Range { min, max }
            }
        }
    }

    pub(in crate::backend::rust::emitter::shapes) fn is_exactly(self, expected: u32) -> bool {
        matches!(
            self,
            Self::Range {
                min,
                max: Some(max),
            } if min == expected && max == expected
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(in crate::backend::rust::emitter::shapes) struct FlatPayloadLayoutPlan {
    pub(in crate::backend::rust::emitter::shapes) admitted: bool,
    pub(in crate::backend::rust::emitter::shapes) synthesize_span: bool,
}

fn descriptor_payload_status(fn_id: u32, ir: &GrammarIR) -> Result<bool, ()> {
    let descriptor = ir.fns.get(fn_id as usize).ok_or(())?;
    payload_push_from_fn(descriptor, ir)
        .map(|payload| payload.is_some())
        .map_err(|_| ())
}

fn ref_boundary_arity(rule_id: u32, rule_arities: &[EmittedPayloadArity]) -> EmittedPayloadArity {
    rule_arities
        .get(rule_id as usize)
        .copied()
        .unwrap_or(EmittedPayloadArity::Unsupported)
}

fn arity_with_rules(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => EmittedPayloadArity::ZERO,
        IrNode::Ref(rule_id) => ref_boundary_arity(*rule_id, rule_arities),
        IrNode::Map { inner, fn_id } => {
            let recognition = arity_with_rules(inner, ir, rule_arities);
            if matches!(
                recognition,
                EmittedPayloadArity::Pending | EmittedPayloadArity::Unsupported
            ) {
                return recognition;
            }
            match descriptor_payload_status(*fn_id, ir) {
                Ok(true) => EmittedPayloadArity::ONE,
                // The generic structural walkers do not implement dynamic
                // FnDescriptor evaluation. Passing their inner payload through
                // would claim a semantic replacement that was never emitted.
                Ok(false) | Err(()) => EmittedPayloadArity::Unsupported,
            }
        }
        IrNode::Seq(children) => children
            .iter()
            .fold(EmittedPayloadArity::ZERO, |arity, child| {
                arity.then(arity_with_rules(child, ir, rule_arities))
            }),
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return EmittedPayloadArity::Unsupported;
            }
            branches
                .iter()
                .fold(EmittedPayloadArity::Pending, |arity, branch| {
                    arity.union(arity_with_rules(&branch.node, ir, rule_arities))
                })
        }
        IrNode::Repeat { inner, lo, hi } => {
            arity_with_rules(inner, ir, rule_arities).repeat(*lo, *hi)
        }
        IrNode::Negate(inner) => match arity_with_rules(inner, ir, rule_arities) {
            EmittedPayloadArity::Range { .. } => EmittedPayloadArity::ZERO,
            other => other,
        },
        IrNode::Minus(lhs, rhs) => {
            let primary = arity_with_rules(lhs, ir, rule_arities);
            match arity_with_rules(rhs, ir, rule_arities) {
                EmittedPayloadArity::Range { .. } => primary,
                other => other,
            }
        }
        IrNode::Skip(kept, discarded) => {
            let kept = arity_with_rules(kept, ir, rule_arities);
            match arity_with_rules(discarded, ir, rule_arities) {
                EmittedPayloadArity::Range { .. } => kept,
                other => other,
            }
        }
        IrNode::Next(discarded, kept) => {
            let kept = arity_with_rules(kept, ir, rule_arities);
            match arity_with_rules(discarded, ir, rule_arities) {
                EmittedPayloadArity::Range { .. } => kept,
                other => other,
            }
        }
        IrNode::OptionalWhitespace(inner) => arity_with_rules(inner, ir, rule_arities),
        // Flat delegates TokenDispatch back to the root dispatcher and the
        // shared inline walker rejects it. Neither path proves target identity,
        // replacement, or arity, so code generation must fail closed.
        IrNode::TokenDispatch { .. } => EmittedPayloadArity::Unsupported,
    }
}

fn scalar_boundary_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    match node {
        IrNode::OptionalWhitespace(inner) => scalar_boundary_arity(inner, ir, rule_arities),
        IrNode::Map { inner, fn_id } => {
            let recognition = scalar_boundary_arity(inner, ir, rule_arities);
            if matches!(
                recognition,
                EmittedPayloadArity::Pending | EmittedPayloadArity::Unsupported
            ) {
                return recognition;
            }
            match descriptor_payload_status(*fn_id, ir) {
                Ok(true) => EmittedPayloadArity::ONE,
                Ok(false) | Err(()) => EmittedPayloadArity::Unsupported,
            }
        }
        IrNode::Literal(sid) => match ir.strings.get(*sid as usize).map(String::as_str) {
            Some("null" | "true" | "false") => EmittedPayloadArity::ONE,
            Some(_) => EmittedPayloadArity::ZERO,
            None => EmittedPayloadArity::Unsupported,
        },
        IrNode::Ref(target) => ref_boundary_arity(*target, rule_arities),
        _ => EmittedPayloadArity::Unsupported,
    }
}

fn keyword_branch_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let mut current = node;
    let mut has_map = false;
    loop {
        match current {
            IrNode::OptionalWhitespace(inner) => current = inner,
            IrNode::Map { inner, fn_id } => {
                if descriptor_payload_status(*fn_id, ir) != Ok(true) {
                    return EmittedPayloadArity::Unsupported;
                }
                has_map = true;
                current = inner;
            }
            _ => break,
        }
    }
    match current {
        IrNode::Literal(_) => EmittedPayloadArity::ONE,
        IrNode::Ref(rule_id) if !has_map => ref_boundary_arity(*rule_id, rule_arities),
        IrNode::Seq(_) | IrNode::Skip(_, _) | IrNode::Next(_, _) if !has_map => {
            match arity_with_rules(current, ir, rule_arities) {
                EmittedPayloadArity::Range { .. } => EmittedPayloadArity::ONE,
                other => other,
            }
        }
        _ => EmittedPayloadArity::Unsupported,
    }
}

fn keyword_boundary_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let mut current = node;
    while let IrNode::OptionalWhitespace(inner) = current {
        current = inner;
    }
    match current {
        IrNode::Literal(_) | IrNode::Map { .. } => keyword_branch_arity(current, ir, rule_arities),
        IrNode::Alt(branches, _) if !branches.is_empty() => branches
            .iter()
            .fold(EmittedPayloadArity::Pending, |arity, branch| {
                arity.union(keyword_branch_arity(&branch.node, ir, rule_arities))
            }),
        _ => EmittedPayloadArity::Unsupported,
    }
}

fn hregex_pattern(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Regex(sid) => Some(*sid),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => hregex_pattern(inner),
        _ => None,
    }
}

fn hregex_map_fn(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Map { inner, fn_id } if hregex_pattern(inner).is_some() => Some(*fn_id),
        IrNode::OptionalWhitespace(inner) => hregex_map_fn(inner),
        _ => None,
    }
}

fn hregex_map_directly_wraps_regex(node: &IrNode) -> bool {
    match node {
        IrNode::Map { inner, .. } => {
            let mut inner = inner.as_ref();
            while let IrNode::OptionalWhitespace(next) = inner {
                inner = next;
            }
            matches!(inner, IrNode::Regex(_))
        }
        IrNode::OptionalWhitespace(inner) => hregex_map_directly_wraps_regex(inner),
        _ => false,
    }
}

fn hregex_boundary_arity(node: &IrNode, ir: &GrammarIR) -> EmittedPayloadArity {
    if hregex_pattern(node).is_none() {
        return EmittedPayloadArity::Unsupported;
    }
    let map_fn = hregex_map_fn(node);
    let descriptor = match map_fn {
        Some(fn_id) => match ir.fns.get(fn_id as usize) {
            Some(descriptor) => Some(descriptor),
            None => return EmittedPayloadArity::Unsupported,
        },
        None => None,
    };
    if map_fn.is_some() && !hregex_map_directly_wraps_regex(node) {
        return match descriptor.and_then(|descriptor| {
            matches!(payload_push_from_fn(descriptor, ir), Ok(Some(_))).then_some(())
        }) {
            Some(()) => EmittedPayloadArity::ONE,
            None => EmittedPayloadArity::Unsupported,
        };
    }
    match descriptor {
        None
        | Some(
            FnDescriptor::NumberConvert { .. }
            | FnDescriptor::SpanCapture
            | FnDescriptor::EnumWrap { .. }
            | FnDescriptor::BoxWrap,
        ) => EmittedPayloadArity::ONE,
        Some(FnDescriptor::HexConvert { fn_path }) => {
            let valid_path = ir
                .strings
                .get(*fn_path as usize)
                .is_some_and(|path| syn::parse_str::<syn::Path>(path).is_ok());
            if valid_path {
                EmittedPayloadArity::ONE
            } else {
                EmittedPayloadArity::Unsupported
            }
        }
        Some(FnDescriptor::Expr {
            expr: MapExpr::Input,
            return_type:
                Some(
                    TypeDesc::F64
                    | TypeDesc::I8
                    | TypeDesc::U8
                    | TypeDesc::I16
                    | TypeDesc::U16
                    | TypeDesc::I32
                    | TypeDesc::U32
                    | TypeDesc::I64
                    | TypeDesc::U64,
                ),
        }) => EmittedPayloadArity::ONE,
        Some(descriptor @ FnDescriptor::Expr { .. }) => {
            match payload_push_from_fn(descriptor, ir) {
                Ok(Some(_)) => EmittedPayloadArity::ONE,
                Ok(None) | Err(_) => EmittedPayloadArity::Unsupported,
            }
        }
    }
}

fn struct_direct_branch_boundary_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let mut emitted_root = node;
    while let IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) = emitted_root {
        emitted_root = inner;
    }
    if !matches!(
        emitted_root,
        IrNode::Ref(_)
            | IrNode::Literal(_)
            | IrNode::Regex(_)
            | IrNode::Seq(_)
            | IrNode::Next(_, _)
            | IrNode::Skip(_, _)
    ) {
        return EmittedPayloadArity::Unsupported;
    }
    match arity_with_rules(node, ir, rule_arities) {
        EmittedPayloadArity::Pending => EmittedPayloadArity::Pending,
        EmittedPayloadArity::Unsupported => EmittedPayloadArity::Unsupported,
        arity if arity.is_exactly(0) || arity.is_exactly(1) => EmittedPayloadArity::ONE,
        _ => EmittedPayloadArity::Unsupported,
    }
}

fn alt_dispatch_boundary_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let mut emitted_root = node;
    while let IrNode::OptionalWhitespace(inner) = emitted_root {
        emitted_root = inner;
    }
    let IrNode::Alt(branches, _) = emitted_root else {
        return EmittedPayloadArity::Unsupported;
    };
    if branches.is_empty() {
        return EmittedPayloadArity::Unsupported;
    }
    branches
        .iter()
        .fold(EmittedPayloadArity::Pending, |arity, branch| {
            arity.union(struct_direct_branch_boundary_arity(
                &branch.node,
                ir,
                rule_arities,
            ))
        })
}

fn wrap_boundary_arity(
    node: &IrNode,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let mut emitted_root = node;
    while let IrNode::OptionalWhitespace(inner) = emitted_root {
        emitted_root = inner;
    }
    let IrNode::Alt(branches, _) = emitted_root else {
        if let IrNode::Ref(rule_id) = emitted_root {
            return ref_boundary_arity(*rule_id, rule_arities);
        }
        return EmittedPayloadArity::Unsupported;
    };
    if branches.is_empty() {
        return EmittedPayloadArity::Unsupported;
    }
    branches
        .iter()
        .fold(EmittedPayloadArity::Pending, |arity, branch| {
            arity.union(struct_direct_branch_boundary_arity(
                &branch.node,
                ir,
                rule_arities,
            ))
        })
}

fn wrap_rule_boundary_arity(
    rule: &IrRule,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    let boundary = wrap_boundary_arity(&rule.body, ir, rule_arities);
    if rule.meta.is_transparent {
        boundary
    } else {
        match boundary {
            EmittedPayloadArity::Range { .. } => EmittedPayloadArity::ONE,
            other => other,
        }
    }
}

fn layout_plan_for_arity(
    rule: &IrRule,
    ir: &GrammarIR,
    arity: EmittedPayloadArity,
) -> FlatPayloadLayoutPlan {
    let Some(layout) = ir.struct_registry.layout(rule.id) else {
        return FlatPayloadLayoutPlan {
            admitted: arity.is_exactly(0) || arity.is_exactly(1),
            synthesize_span: arity.is_exactly(0),
        };
    };
    if matches!(layout.rule_type, TypeDesc::Vec(_)) {
        return FlatPayloadLayoutPlan {
            admitted: matches!(arity, EmittedPayloadArity::Range { .. }),
            synthesize_span: false,
        };
    }
    if !matches!(
        layout.kind,
        bbnf_ir::registry::LayoutKind::Struct | bbnf_ir::registry::LayoutKind::NewtypeWrapper
    ) {
        return FlatPayloadLayoutPlan {
            admitted: false,
            synthesize_span: false,
        };
    }
    let Ok(expected) = u32::try_from(layout.field_count()) else {
        return FlatPayloadLayoutPlan {
            admitted: false,
            synthesize_span: false,
        };
    };
    FlatPayloadLayoutPlan {
        admitted: arity.is_exactly(expected) || (expected == 1 && arity.is_exactly(0)),
        synthesize_span: expected == 1 && arity.is_exactly(0),
    }
}

fn rule_boundary_arity(
    rule: &IrRule,
    ir: &GrammarIR,
    rule_arities: &[EmittedPayloadArity],
) -> EmittedPayloadArity {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    if !super::super::shape_driver_emits_rule(rule, ir) {
        return EmittedPayloadArity::Unsupported;
    }
    match ir.shape_assignments.get(rule.id) {
        ShapeTag::Scalar => scalar_boundary_arity(&rule.body, ir, rule_arities),
        ShapeTag::Keyword => keyword_boundary_arity(&rule.body, ir, rule_arities),
        ShapeTag::Wrap => wrap_rule_boundary_arity(rule, ir, rule_arities),
        ShapeTag::Flat => {
            let body = arity_with_rules(&rule.body, ir, rule_arities);
            match body {
                EmittedPayloadArity::Pending | EmittedPayloadArity::Unsupported => body,
                _ if layout_plan_for_arity(rule, ir, body).admitted => EmittedPayloadArity::ONE,
                _ => EmittedPayloadArity::Unsupported,
            }
        }
        // The Pratt emitter creates a dynamic operator/RHS sequence whose exact
        // internal layout is not checked by this authority. TokenDispatch has
        // no owned structural emission at all. Neither may masquerade as an
        // exact-one Ref boundary.
        ShapeTag::Pratt => EmittedPayloadArity::Unsupported,
        ShapeTag::Object
        | ShapeTag::Array
        | ShapeTag::String
        | ShapeTag::Number
        | ShapeTag::Unordered
        | ShapeTag::ArgList => EmittedPayloadArity::ONE,
        ShapeTag::AltDispatch => {
            alt_dispatch_boundary_arity(&rule.body, ir, rule_arities)
        }
        ShapeTag::HRegex => hregex_boundary_arity(&rule.body, ir),
        ShapeTag::None => EmittedPayloadArity::Unsupported,
    }
}

fn solve_rule_arities(ir: &GrammarIR) -> Vec<EmittedPayloadArity> {
    let mut arities = vec![EmittedPayloadArity::Pending; ir.rules.len()];
    let iteration_limit = ir.rules.len().saturating_mul(4).saturating_add(1);
    let mut converged = false;
    for _ in 0..iteration_limit {
        let next: Vec<_> = ir
            .rules
            .iter()
            .map(|rule| rule_boundary_arity(rule, ir, &arities))
            .collect();
        if next == arities {
            converged = true;
            break;
        }
        arities = next;
    }
    if !converged {
        arities.fill(EmittedPayloadArity::Unsupported);
        return arities;
    }
    if arities
        .iter()
        .any(|arity| *arity == EmittedPayloadArity::Pending)
    {
        for arity in &mut arities {
            if *arity == EmittedPayloadArity::Pending {
                *arity = EmittedPayloadArity::Unsupported;
            }
        }
        let mut fallback_converged = false;
        for _ in 0..iteration_limit {
            let next: Vec<_> = ir
                .rules
                .iter()
                .map(|rule| rule_boundary_arity(rule, ir, &arities))
                .collect();
            if next == arities {
                fallback_converged = true;
                break;
            }
            arities = next;
        }
        if !fallback_converged {
            // A post-widening cycle or still-growing recursive range has no
            // exact fixed point within the finite proof budget. Do not leak its
            // last approximation as an emitted-path fact.
            arities.fill(EmittedPayloadArity::Unsupported);
        }
    }
    arities
}

/// One solved emitted-payload authority for a shape-emission boundary.
///
/// Constructing the planner runs the recursive rule fixed point exactly once.
/// Callers pass the same planner through every branch and nested structural
/// walk they lower for that emitted rule.
pub struct PayloadPlanner<'ir> {
    ir: &'ir GrammarIR,
    rule_arities: Vec<EmittedPayloadArity>,
}

impl<'ir> PayloadPlanner<'ir> {
    pub fn new(ir: &'ir GrammarIR) -> Self {
        Self {
            ir,
            rule_arities: solve_rule_arities(ir),
        }
    }

    /// Return the exact emitted-path payload extrema for `node`.
    pub(in crate::backend::rust::emitter::shapes) fn node_arity(
        &self,
        node: &IrNode,
    ) -> EmittedPayloadArity {
        arity_with_rules(node, self.ir, &self.rule_arities)
    }

    /// Whether one shape emitter owns every successful path through `rule`.
    ///
    /// Direct standalone emitter tests need not mutate the fixture's assigned
    /// tag: the requested shape selects the same boundary proof that the
    /// grammar-wide fixed point uses in production.
    pub(in crate::backend::rust::emitter::shapes) fn admits_shape_path(
        &self,
        rule: &IrRule,
        shape: bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag,
    ) -> bool {
        use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

        let arity = match shape {
            ShapeTag::AltDispatch => {
                alt_dispatch_boundary_arity(&rule.body, self.ir, &self.rule_arities)
            }
            ShapeTag::Keyword => {
                keyword_boundary_arity(&rule.body, self.ir, &self.rule_arities)
            }
            ShapeTag::Wrap => wrap_rule_boundary_arity(rule, self.ir, &self.rule_arities),
            _ => EmittedPayloadArity::Unsupported,
        };
        matches!(arity, EmittedPayloadArity::Range { .. })
    }

    /// Compare a Flat body against its solved layout using exact field arity.
    ///
    /// A fixed Struct/Newtype layout admits only its exact `field_count`. The
    /// sole repair is a zero-body/one-field span synthesis. Vec layouts admit
    /// a proven range, including empty and unbounded repetition, because their
    /// one registry field describes the repeated element rather than a fixed
    /// child count.
    pub(in crate::backend::rust::emitter::shapes) fn flat_layout_plan(
        &self,
        rule: &IrRule,
    ) -> FlatPayloadLayoutPlan {
        layout_plan_for_arity(rule, self.ir, self.node_arity(&rule.body))
    }
}

fn mapped_payload_push(branch_root: &IrNode, ir: &GrammarIR) -> Option<TokenStream> {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }

    let descriptor = ir.fns.get(find_map_fn(branch_root)? as usize)?;
    match payload_push_from_fn(descriptor, ir) {
        Ok(Some(payload)) => Some(payload),
        Ok(None) => None,
        Err(message) => Some(quote! { compile_error!(#message); }),
    }
}
/// AZ-I.W2.RB — emit the dispatch body for an AltDispatch rule under
/// [`bbnf_ir::registry::EmitStrategy::StructDirect`].
///
/// Each branch attempt routes through the per-Ref struct-direct shape
/// fn (or — for non-Ref leaf branches — emits a typed `builder.push_*`
/// for the leaf payload) and records the matched branch tag via
/// `builder.select_alt_branch(AltBranchId(idx))` before the per-shape body fires. No
/// `tape.*` calls are emitted under this path.
///
/// JSON does not exercise AltDispatch (its `value` rule is `Wrap`); this
/// path is reachable only when CSS L4 (W3) or Sheets (W2.B) flips a
/// rule with AltDispatch shape into StructDirect. Until then it remains
/// dead code at codegen but the emission contract holds (zero
/// `tape.push` per the W2-EMITTER-REWIRE plan §3 hard gate).
pub(super) fn emit_dispatch_arms_struct_direct(
    branches: &[AltBranch],
    grammar_suffix: &str,
    _rule: &IrRule,
    ir: &GrammarIR,
    payload_planner: &PayloadPlanner<'_>,
) -> TokenStream {
    let mut arms: Vec<TokenStream> = Vec::new();
    for (branch_idx, branch) in branches.iter().enumerate() {
        // The IR pipeline rejects Alt counts above AltBranchId::MAX_COUNT
        // both before and after rewriting; therefore every enumerated index
        // is representable. Keep the checked conversion here so emitter
        // misuse cannot silently truncate a structural branch identity.
        let idx_u16 =
            u16::try_from(branch_idx).expect("Alt branch count gate guarantees a u16 branch index");
        let inner = unwrap_trivia(&branch.node);
        // AZ-III.W3c.1 — typed-payload constant-fold pulled from the
        // branch's outermost `Map { fn_id }` chain. Mirrors Wrap's
        // `wrap_branch_payload_push`: `FnDescriptor::Expr { expr, .. }`
        // with `MapExpr::IntLit(n)` produces a `push_leaf_with_u64(n)`
        // call so CSS L4's `namedColor` / hex-color branches deposit
        // the declared `0xRRGGBBAAu32` instead of unit. Non-constant
        // maps (or no Map wrapper) fall back to `push_leaf_with_unit`
        // — the branch still records the discriminator without
        // fabricating typed payload the grammar didn't declare.
        let payload_push = branch_payload_push(&branch.node, ir);
        let mapped_continuation = mapped_payload_push(&branch.node, ir);
        let unmapped_fallback = if mapped_continuation.is_none() {
            let arity = payload_planner.node_arity(&branch.node);
            if arity.is_exactly(0) {
                quote! {
                    <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(builder);
                }
            } else if arity.is_exactly(1) {
                quote! {}
            } else {
                quote! {
                    compile_error!(
                        "AltDispatch branch does not emit exactly one \
                         committed semantic payload"
                    );
                }
            }
        } else {
            quote! {}
        };
        let mapped_replacement_start = mapped_continuation.as_ref().map(|_| {
            quote! {
                let __map_builder_checkpoint = builder.checkpoint();
            }
        });
        let mapped_replacement_finish = mapped_continuation.as_ref().map(|payload| {
            quote! {
                builder.rollback(__map_builder_checkpoint);
                #payload
            }
        });
        let body = match inner {
            IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
                Some(call) => quote! {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        #mapped_replacement_start
                        match #call {
                            Ok(_) => {
                                #mapped_replacement_finish
                                #unmapped_fallback
                                builder.select_alt_branch(
                                    ::bbnf_ir::registry::AltBranchId::new(#idx_u16),
                                );
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                },
                None => quote! {},
            },
            // AZ-II.cutover.M Phase 3 — Literal-led Alt branches under
            // struct-direct emit the byte-comparison + push_leaf pair
            // mirroring `emit_literal_attempt`'s tape-path body, plus
            // the structural `select_alt_branch(AltBranchId(idx))` that
            // the AltDispatch frame's TaggedEnum layout consumes when
            // the branch fires. EBNF's `letter`, `digit`, `symbol`,
            // `terminator` and BNF's `terminal` are the canonical
            // Alt-of-literal AltDispatch rules; pre-cutover.M these
            // emitted empty placeholders, dropping every literal
            // candidate on the floor.
            //
            // AZ-III.W3c.1 — typed-payload activation: the previous
            // unconditional `push_leaf_with_unit()` call is replaced
            // by `payload_push`, which constant-folds `Map(Literal)`
            // branches' `-> NN` projections (CSS L4 `namedColor`'s
            // 150 `"name" -> 0xRRGGBBAAu32` mappings) into the typed
            // `push_leaf_with_u64(value)` call the
            // `CssStructBuilder::Color::Hex` reader consumes.
            IrNode::Literal(sid) => {
                let bytes = ir.get_string(*sid).as_bytes();
                let len = bytes.len();
                let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
                quote! {
                    {
                        let at = *p;
                        let end = at + #len;
                        if input.len() >= end && input[at..end] == [#(#byte_lits),*] {
                            *p = end;
                            #payload_push
                            builder.select_alt_branch(
                                ::bbnf_ir::registry::AltBranchId::new(#idx_u16),
                            );
                            break 'try_branches;
                        }
                    }
                }
            }
            // Regex-led branches dispatch through the per-grammar
            // regex-scan adapter and push the typed leaf on match.
            // Same discriminator-recording shape as the Literal arm.
            //
            // AZ-III.W3c.1 — typed-payload activation: a `Map(Regex)`
            // wrapper's constant-fold (e.g. `/\s*>\s*/ -> 1u8`) flows
            // through `payload_push` so the branch deposits its
            // declared u8 / u32 / bool instead of unit.
            IrNode::Regex(sid) => {
                let pattern = ir.get_string(*sid).to_string();
                let regex_scan_ident =
                    super::super::super::regex_scan_adapter::regex_scan_adapter_ident(
                        &super::super::sanitise_grammar(grammar_suffix),
                    );
                quote! {
                    {
                        if let ::core::option::Option::Some(match_len) =
                            #regex_scan_ident(#pattern, input, *p)
                        {
                            *p += match_len as usize;
                            #payload_push
                            builder.select_alt_branch(
                                ::bbnf_ir::registry::AltBranchId::new(#idx_u16),
                            );
                            break 'try_branches;
                        }
                    }
                }
            }
            // Seq branches always use the shared structural walker. It owns
            // the canonical regex adapter, OptionalWhitespace bookends, Map
            // replacement, and discarded-side rollback; keeping a second
            // "literal chain" scanner here previously changed regex language
            // and erased whitespace.
            IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
                let seq_body = super::super::inline::emit_seq_branch_structural_struct_direct(
                    inner,
                    &format_ident!("__shape_support_{}", grammar_suffix),
                    grammar_suffix,
                    ir,
                    payload_planner,
                );
                quote! {
                    {
                        let attempt_p = *p;
                        let attempt_builder = builder.checkpoint();
                        let attempt: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            #mapped_replacement_start
                            #seq_body
                            #mapped_replacement_finish
                            #unmapped_fallback
                            ::core::result::Result::Ok(())
                        })();
                        match attempt {
                            ::core::result::Result::Ok(()) => {
                                builder.select_alt_branch(
                                    ::bbnf_ir::registry::AltBranchId::new(#idx_u16),
                                );
                                builder.commit(attempt_builder);
                                break 'try_branches;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = attempt_p;
                                builder.rollback(attempt_builder);
                            }
                        }
                    }
                }
            }
            _ => quote! {},
        };
        arms.push(body);
    }
    quote! {
        'try_branches: loop {
            #(#arms)*
            return Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            });
        }
    }
}

/// AZ-III.W3c.1 — emit the `builder.push_leaf_with_*` call materialising
/// an AltDispatch-Alt branch's typed `->` projection.
///
/// Symmetric to `wrap::struct_direct::wrap_branch_payload_push`: the
/// alt_dispatch shape is Wrap's strict superset (per
/// `shapes/alt_dispatch/mod.rs` doc), so the same payload-fold rule
/// applies. Walks the branch's outermost `Map { fn_id }` chain
/// (skipping `OptionalWhitespace` trivia), inspects the bound
/// `FnDescriptor`, and constant-folds the leaf projections that CSS
/// L4's `namedColor`'s 150 `"name" -> 0xRRGGBBAAu32` and `combinator`'s
/// `-> Nu8` annotations produce. Non-constant maps (`Input`,
/// `FnCall`, `BinOp`, `UnaryOp`) and unmapped branches fall back to
/// `push_leaf_with_unit()` without fabricating a typed payload. Structural
/// Alt selection remains a separate operation.
///
/// AZ-IV.W1-CLOSE.B — visibility widened from `pub(super)` to the
/// shapes module so the inline structural-branch helper can apply
/// the same fold to each inner Alt arm encountered while emitting a
/// prefix-tree-factored Seq. Without per-inner-arm fold the 148
/// non-singleton namedColor branches lose their declared u32 payload
/// (the outer Seq's tail is the inner Alt; `find_map_fn` cannot
/// recover a single fn_id from N alternative arms).
pub(in crate::backend::rust::emitter::shapes) fn branch_payload_push(
    branch_root: &IrNode,
    ir: &GrammarIR,
) -> TokenStream {
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            // Post-prefix-tree-factor branches look like
            // `Seq(Literal("a"), Map(Literal("liceblue"), fn))`. The
            // typed payload lives on the continuation (the LAST
            // structurally meaningful child); the leading literal
            // bytes are the factored prefix that the Seq's body
            // checks already enforce. Walk the LAST child so we
            // capture the typed projection without picking up an
            // earlier Map that may live on a side rule.
            IrNode::Seq(children) => children.last().and_then(find_map_fn),
            _ => None,
        }
    }
    let unit_push = quote! {
        <_ as crate::runtime::StructBuilder>::push_leaf_with_unit(builder);
    };
    let Some(fn_id) = find_map_fn(branch_root) else {
        return unit_push;
    };
    let Some(fn_desc) = ir.fns.get(fn_id as usize) else {
        return unit_push;
    };
    match payload_push_from_fn(fn_desc, ir) {
        Ok(Some(payload)) => payload,
        Ok(None) => unit_push,
        Err(message) => quote! { compile_error!(#message); },
    }
}

/// Strip Map / OptionalWhitespace trivia.
pub(super) fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}
