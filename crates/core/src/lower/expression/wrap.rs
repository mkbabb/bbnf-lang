//! Mapped-factor / grouped-term / map-arrow lowering.
//!
//! Three concerns share this module:
//!
//! 1. `mapped_factor = factor ( "->" value_expr type? )?` — the
//!    optional mapping wraps the factor in `IrNode::Map`.
//! 2. Grouped terms `( ... )` / `[ ... ]` / `{ ... }` / `@{ ... }` —
//!    the four flavours all share a `[open_delim, inner, close_delim]`
//!    layout and dispatch on the leading byte of the term span.
//! 3. `lower_map_arrow` + `try_specialize_map_fn` — the value-
//!    expression / type-annotation lowering on the right-hand side of
//!    the `->` arrow, with type-driven specialization to the bespoke
//!    `NumberConvert` / `HexConvert` descriptors.

use bbnf_ir::{FnDescriptor, FnId, IrNode, MapExpr, NumericDomain, TypeDesc};
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};

use super::super::LowerCtx;
use super::super::value_expr::{
    deep_unwrap_value, extract_value_func_name, is_type_name, lower_value_expr,
    unwrap_value_ident_str,
};
use super::{dispatch_expression, lower_rhs};

/// The four grouped-term flavors, discriminated by the opening delimiter
/// byte of the term compound's span.
#[derive(Clone, Copy)]
pub(crate) enum GroupKind {
    /// `"(" rhs ")"` — plain grouping.
    Paren,
    /// `"[" rhs "]"` — optional group, lowered to `Repeat { lo: 0, hi: 1 }`.
    Optional,
    /// `"{" rhs "}"` — many-group, lowered to `Repeat { lo: 0, hi: u32::MAX }`.
    Many,
    /// `"@{" rhs "}"` — span-capture, lowered to `Map + FnDescriptor::SpanCapture`.
    SpanCapture,
}

fn is_value_expr_kind(kind: Option<BbnfCompoundKind>) -> bool {
    matches!(
        kind,
        Some(
            BbnfCompoundKind::ValueExpr
                | BbnfCompoundKind::ValueClosure
                | BbnfCompoundKind::ValueOr
                | BbnfCompoundKind::ValueAnd
                | BbnfCompoundKind::ValueCmp
                | BbnfCompoundKind::ValueAdd
                | BbnfCompoundKind::ValueMul
                | BbnfCompoundKind::ValueUnary
                | BbnfCompoundKind::ValueAtom
                | BbnfCompoundKind::ValuePath
                | BbnfCompoundKind::ValueInput
                | BbnfCompoundKind::ValueFnCall,
        )
    )
}

fn exact_bounds(view: BbnfView<'_, '_>, role: &str) -> (usize, usize) {
    let input = RuntimeView::input(&view);
    let (lo, hi) = view
        .span_bounds()
        .unwrap_or_else(|| panic!("mapped_factor: {role} has no structural source bounds"));
    let (lo, hi) = (lo as usize, hi as usize);
    if lo > hi || hi > input.len() || !input.is_char_boundary(lo) || !input.is_char_boundary(hi) {
        panic!(
            "mapped_factor: {role} bounds {lo}..{hi} are invalid for {} source bytes",
            input.len(),
        );
    }
    (lo, hi)
}

fn bounded_text<'a>(input: &'a str, lo: usize, hi: usize, role: &str) -> &'a str {
    input
        .get(lo..hi)
        .unwrap_or_else(|| panic!("mapped_factor: {role} bounds {lo}..{hi} split the source text"))
}

fn is_identifier(text: &str) -> bool {
    let bytes = text.as_bytes();
    bytes
        .first()
        .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
        && bytes
            .iter()
            .all(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
}

/// Lower a `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ?` view.
///
/// The generated schema is the authority: `Factor`, the value-expression
/// head, and `TypeAnnotation` are direct children with exact source bounds.
/// Value and type payloads are sliced only from those structural bounds. No
/// source scanner, anonymous-wrapper fallback, or parent-span recovery can
/// manufacture a map that the tree did not materialize.
pub(crate) fn lower_mapped_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let input = RuntimeView::input(&node);
    let (mapped_lo, mapped_hi) = exact_bounds(node, "mapped factor");
    let mut factor: Option<BbnfView<'a, 'a>> = None;
    let mut value_expr_head: Option<BbnfView<'a, 'a>> = None;
    let mut type_annotation: Option<BbnfView<'a, 'a>> = None;
    for child in node.children() {
        match child.compound_kind() {
            Some(BbnfCompoundKind::Factor) => {
                if factor.replace(child).is_some() {
                    panic!("mapped_factor: duplicate structural Factor child");
                }
            }
            Some(BbnfCompoundKind::TypeAnnotation) => {
                if type_annotation.replace(child).is_some() {
                    panic!("mapped_factor: duplicate structural TypeAnnotation child");
                }
            }
            kind if is_value_expr_kind(kind) => {
                if value_expr_head.replace(child).is_some() {
                    panic!("mapped_factor: duplicate structural value-expression child");
                }
            }
            other => {
                panic!(
                    "mapped_factor: unexpected direct child {:?} ({other:?})",
                    child.kind(),
                );
            }
        }
    }
    let factor = factor.unwrap_or_else(|| panic!("mapped_factor: missing structural Factor child"));
    let (factor_lo, factor_hi) = exact_bounds(factor, "factor");
    if factor_lo < mapped_lo || factor_hi > mapped_hi {
        panic!("mapped_factor: Factor bounds escape their parent");
    }
    let base = dispatch_expression(factor, ctx);

    let Some(value_expr) = value_expr_head else {
        if type_annotation.is_some() {
            panic!("mapped_factor: TypeAnnotation exists without a value expression");
        }
        if !bounded_text(input, factor_hi, mapped_hi, "factor tail")
            .trim()
            .is_empty()
        {
            panic!("mapped_factor: non-map bytes follow the structural Factor child");
        }
        return base;
    };

    let (value_lo, value_hi) = exact_bounds(value_expr, "value expression");
    if factor_hi > value_lo || value_hi > mapped_hi {
        panic!("mapped_factor: value-expression bounds overlap or escape their parent");
    }
    let rhs_text = bounded_text(input, value_lo, value_hi, "value expression").trim();
    if rhs_text.is_empty() {
        panic!("mapped_factor: structurally present value expression has empty source bounds");
    }

    let type_name = type_annotation.map(|annotation| {
        let (annotation_lo, annotation_hi) = exact_bounds(annotation, "type annotation");
        if value_hi > annotation_lo || annotation_hi > mapped_hi {
            panic!("mapped_factor: type-annotation bounds overlap or escape their parent");
        }
        if !bounded_text(input, value_hi, annotation_lo, "annotation gap")
            .trim()
            .is_empty()
        {
            panic!("mapped_factor: unexpected bytes precede the type annotation");
        }
        if !bounded_text(input, annotation_hi, mapped_hi, "annotation tail")
            .trim()
            .is_empty()
        {
            panic!("mapped_factor: unexpected bytes follow the type annotation");
        }
        let annotation_text =
            bounded_text(input, annotation_lo, annotation_hi, "type annotation").trim();
        let name = annotation_text
            .strip_prefix(':')
            .map(str::trim)
            .filter(|name| is_identifier(name))
            .unwrap_or_else(|| {
                panic!("mapped_factor: malformed bounded type annotation {annotation_text:?}")
            });
        name
    });
    if type_annotation.is_none()
        && !bounded_text(input, value_hi, mapped_hi, "value-expression tail")
            .trim()
            .is_empty()
    {
        panic!("mapped_factor: unexpected bytes follow the value expression");
    }

    let fn_id = lower_map_arrow(value_expr, type_name, rhs_text, ctx);
    let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
    IrNode::Map {
        inner: Box::new(base),
        fn_id,
    }
}

// ─── Grouped-term lowering ────────────────────────────────────────────────────

/// Descend into the inner expression of a grouped term compound and
/// apply the grouping operator.
pub(super) fn lower_grouped_term<'a>(
    node: BbnfView<'a, 'a>,
    kind: GroupKind,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let inner_view = find_inner_expression(node).unwrap_or_else(|| {
        panic!(
            "lower_term (grouped): missing inner expression in span {:?}",
            node.span_text(),
        )
    });
    let expr = lower_rhs(inner_view, ctx);
    match kind {
        GroupKind::Paren => expr,
        GroupKind::Optional => IrNode::Repeat {
            inner: Box::new(expr),
            lo: 0,
            hi: 1,
        },
        GroupKind::Many => IrNode::Repeat {
            inner: Box::new(expr),
            lo: 0,
            hi: u32::MAX,
        },
        GroupKind::SpanCapture => {
            let fn_id = ctx.fns.push(FnDescriptor::SpanCapture);
            IrNode::Map {
                inner: Box::new(expr),
                fn_id,
            }
        }
    }
}

/// Locate the substantive inner child of a grouped term compound —
/// the `rhs` (or collapsed descendant) expression between the
/// `(...)` / `[...]` / `{...}` / `@{...}` delimiters.
///
/// Strategy: descend depth-first looking for the first compound
/// that's an expression-layer kind (`Rhs`, `Closure`, `Alternation`,
/// `Concatenation`, `BinaryFactor`, `MappedFactor`, `Factor`,
/// `Term`). First match wins — the outermost class that surfaces
/// is the root of the inner subtree.
fn find_inner_expression<'a>(node: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
    const EXPRESSION_KINDS: &[BbnfCompoundKind] = &[
        BbnfCompoundKind::Rhs,
        BbnfCompoundKind::Closure,
        BbnfCompoundKind::Alternation,
        BbnfCompoundKind::Concatenation,
        BbnfCompoundKind::BinaryFactor,
        BbnfCompoundKind::MappedFactor,
        BbnfCompoundKind::Factor,
        BbnfCompoundKind::Term,
    ];

    for &kind in EXPRESSION_KINDS {
        if let Some(v) = super::super::view_walk::find_descendant_by_kind(node, kind) {
            if let Some((lo, hi)) = v.byte_span() {
                if hi > lo {
                    return Some(v);
                }
            } else {
                // No span recoverable — accept anyway; the lowering
                // pass downstream will inspect content directly.
                return Some(v);
            }
        }
    }
    None
}

// ─── MapArrow / ValueExpr lowering ─────────────────────────────────────────────

/// Resolve a type name string to a `TypeDesc`, preferring concrete
/// scalar variants over the generic `TypeDesc::Named`.
fn resolve_type_name(name: &str, ctx: &mut LowerCtx<'_>) -> TypeDesc {
    TypeDesc::from_scalar_name(name).unwrap_or_else(|| {
        let sid = ctx.strings.intern(name);
        TypeDesc::Named(sid)
    })
}

/// Lower a `->` mapping to a `FnId`.
fn lower_map_arrow<'a>(
    value_expr: BbnfView<'a, 'a>,
    type_name: Option<&str>,
    rhs_source: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_name.map(|name| resolve_type_name(name, ctx));

    // Type-shorthand: bare type name like `-> f64`.
    if let Some(name) = unwrap_value_ident_str(value_expr) {
        if is_type_name(name) && return_type.is_none() {
            let td = resolve_type_name(name, ctx);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(td),
            });
        }
    }

    let expected_numeric_domain = return_type.as_ref().and_then(NumericDomain::from_type_desc);
    let lowered = lower_value_expr(value_expr, rhs_source, expected_numeric_domain, ctx);
    let return_type = return_type.or_else(|| lowered.inferred_return_type());
    let mut map_expr = lowered.expr;
    map_expr
        .checked_fold(return_type.as_ref().and_then(NumericDomain::from_type_desc))
        .unwrap_or_else(|error| {
            panic!(
                "invalid numeric map expression {rhs_source:?}: {}",
                error.message()
            )
        });

    // @host return type propagation.
    let return_type = return_type.or_else(|| {
        let func_name = extract_value_func_name(deep_unwrap_value(value_expr));
        func_name.and_then(|name| {
            ctx.host_fns
                .and_then(|hosts| hosts.get(name.as_str()))
                .and_then(|opt_type| opt_type.as_ref())
                .map(|type_name| resolve_type_name(type_name, ctx))
        })
    });

    ctx.fns.push(FnDescriptor::Expr {
        expr: map_expr,
        return_type,
    })
}

// ─── Specialization ────────────────────────────────────────────────────────────

fn try_specialize_map_fn(inner: &IrNode, fn_id: FnId, ctx: &mut LowerCtx<'_>) -> FnId {
    let desc = &ctx.fns.fns[fn_id as usize];

    let desc_clone = desc.clone();
    let (expr, type_name_owned) = match &desc_clone {
        FnDescriptor::Expr {
            expr,
            return_type: Some(td),
        } => {
            let name = match td {
                TypeDesc::Named(sid) => Some(ctx.strings.resolve(*sid).to_owned()),
                TypeDesc::F64 => Some("f64".to_owned()),
                TypeDesc::U32 => Some("u32".to_owned()),
                _ => None,
            };
            match name {
                Some(n) => (expr.clone(), n),
                None => return fn_id,
            }
        }
        FnDescriptor::Expr {
            expr,
            return_type: None,
        } => {
            let IrNode::Regex(sid) = inner else {
                return fn_id;
            };
            let pattern = ctx.strings.resolve(*sid).to_owned();
            if let MapExpr::FnCall { name, args } = expr {
                if args.len() == 1
                    && matches!(args[0], MapExpr::Input | MapExpr::InputProp { .. })
                    && matches!(classify_regex(&pattern), RegexClass::HexDigits)
                {
                    let fn_path_str = ctx.strings.resolve(*name).to_owned();
                    let path_sid = ctx.strings.intern(&fn_path_str);
                    return ctx.fns.push(FnDescriptor::HexConvert { fn_path: path_sid });
                }
            }
            return fn_id;
        }
        _ => return fn_id,
    };

    let regex_sid = match inner {
        IrNode::Regex(sid) => *sid,
        _ => return fn_id,
    };

    let pattern = ctx.strings.resolve(regex_sid).to_owned();

    match type_name_owned.as_str() {
        "f64" => {
            if matches!(expr, MapExpr::Input) {
                if let RegexClass::Numeric {
                    allow_leading_dot, ..
                } = classify_regex(&pattern)
                {
                    ctx.fns
                        .push(FnDescriptor::NumberConvert { allow_leading_dot })
                } else {
                    fn_id
                }
            } else {
                fn_id
            }
        }
        "u32" => {
            if let MapExpr::FnCall { name, args } = &expr {
                if args.len() == 1
                    && matches!(args[0], MapExpr::Input | MapExpr::InputProp { .. })
                    && matches!(classify_regex(&pattern), RegexClass::HexDigits)
                {
                    let fn_path_str = ctx.strings.resolve(*name).to_owned();
                    let path_sid = ctx.strings.intern(&fn_path_str);
                    ctx.fns.push(FnDescriptor::HexConvert { fn_path: path_sid })
                } else {
                    fn_id
                }
            } else {
                fn_id
            }
        }
        _ => fn_id,
    }
}

#[cfg(test)]
#[path = "../../../tests/lower/expression/wrap.rs"]
mod tests;
