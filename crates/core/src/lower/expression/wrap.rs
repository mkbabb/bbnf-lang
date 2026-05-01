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

use bbnf_ir::{FnDescriptor, FnId, IrNode, MapExpr, TypeDesc};
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};

use super::super::LowerCtx;
use super::super::value_expr::{
    deep_unwrap_value, extract_value_func_name, is_type_name, lower_value_expr,
    split_numeric_suffix, unwrap_value_ident_str,
};
use super::repeat::apply_modifier;
use super::{dispatch_expression, find_unquoted, lower_leaf_by_span_text_str, lower_rhs};

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

/// Lower a `mapped_factor = factor ( "->" value_expr type? )?` view.
///
/// The first child is the underlying factor. The optional `->`
/// mapping is detected via span content: when the trimmed span
/// starts with `->` / `=>`, the mapping group carries the value
/// expression and optional type annotation as children of the
/// mapping subtree.
pub(crate) fn lower_mapped_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Under struct-direct, `factor` is inlined into `mapped_factor`,
    // so this compound's children are
    //   `[big_comment?, term, modifier?, big_comment?, mapping?]`
    // with each optional slot represented by an empty-span leaf.
    // Classify children by span content rather than by positional
    // index.
    let body = peel_mapped_factor_body(node);
    let mut term_node: Option<BbnfView<'a, 'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut mapping_node: Option<BbnfView<'a, 'a>> = None;
    for c in body.children() {
        let span_text = c.span_text();
        let trimmed = span_text.trim();
        if trimmed.is_empty() {
            continue;
        }
        if matches!(trimmed, "?" | "?w" | "*" | "+") {
            modifier_text = Some(trimmed.to_string());
            continue;
        }
        if trimmed.starts_with("->") || trimmed.starts_with("=>") {
            mapping_node = Some(c);
            continue;
        }
        if term_node.is_none() {
            term_node = Some(c);
        }
    }
    // AW-II.W5b.2 — group wrapping (`{ ... }` / `[ ... ]` / `@{...}`)
    // is the responsibility of the term dispatch, not the
    // mapped_factor layer.
    let mut base = if let Some(term) = term_node {
        dispatch_expression(term, ctx)
    } else {
        // No term child surfaced in the view — recover the leaf from the
        // compound's own span_text after stripping any trailing
        // modifier and any trailing mapping group.
        let raw = node.span_text();
        let mut stripped: &str = raw.trim();
        if let Some(modifier) = &modifier_text {
            stripped = stripped
                .strip_suffix(modifier.as_str())
                .unwrap_or(stripped)
                .trim();
        }
        if let Some(idx) = find_unquoted(stripped, "->") {
            stripped = stripped[..idx].trim();
        } else if let Some(idx) = find_unquoted(stripped, "=>") {
            stripped = stripped[..idx].trim();
        }
        lower_leaf_by_span_text_str(stripped, ctx).unwrap_or_else(|| {
            panic!(
                "mapped_factor: no term child and span_text {:?} (after stripping \
                 modifier {:?} + mapping) resolved to {:?} which is not a recognisable leaf",
                raw, modifier_text, stripped
            )
        })
    };
    if let Some(modifier) = &modifier_text {
        base = apply_modifier(base, modifier);
    }
    let Some(mapping_node) = mapping_node else {
        return base;
    };
    // Extract the value_expr + optional type_annotation from the
    // mapping subtree.
    let value_expr = find_value_expr_child(mapping_node)
        .expect("mapped_factor mapping: missing value expression");
    let type_ann = find_type_annotation_child(mapping_node);
    let fn_id = lower_map_arrow(value_expr, type_ann, ctx);
    let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
    IrNode::Map {
        inner: Box::new(base),
        fn_id,
    }
}

/// Find the value-expression head inside a mapping subtree.
///
/// The mapping's children include the `->` Span token, optional
/// whitespace placeholders, the value-expression head, and an
/// optional type annotation. Under struct-direct, the value-
/// expression sub-grammar (imported from `expressions.bbnf`) is
/// not enumerated by [`BbnfCompoundKind::from_rule_name`] — its
/// compounds collapse into [`BbnfCompoundKind::Other`]. We
/// descend depth-first looking for the first non-empty,
/// non-arrow-token candidate that's NOT a type-annotation
/// compound.
fn find_value_expr_child<'a>(node: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
    fn is_value_expr_head_kind(kind: Option<BbnfCompoundKind>) -> bool {
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
                    | BbnfCompoundKind::ValueFnCall
                    | BbnfCompoundKind::Other,
            ),
        )
    }
    fn descend<'a>(view: BbnfView<'a, 'a>, out: &mut Option<BbnfView<'a, 'a>>) {
        if out.is_some() {
            return;
        }
        let kind = view.compound_kind();
        let trimmed = view.span_text().trim();
        // Leaves: discriminate by span_text content.
        if kind.is_none() {
            if trimmed.is_empty() {
                return;
            }
            if trimmed == "->" || trimmed == "=>" {
                return;
            }
            if trimmed.starts_with(':') {
                return;
            }
            *out = Some(view);
            return;
        }
        // Compounds: the value-expression sub-grammar can collapse to
        // typed-leaf bool / int / float values whose source-text is
        // dropped by the parse-time projection (`BbnfValue::Bool` /
        // `Int` / `Float`). For these the recursive `compute_byte_span`
        // walk returns None and `span_text()` is `""` — an empty span
        // does NOT mean "no value-expression head", it means "every
        // descendant projected to a typed leaf". Discriminate by
        // structural kind instead and only fall back to the span-text
        // guard for `Other`-kinded wrappers (which carry source-bytes
        // verbatim in the parse_that-shaped source view).
        if is_value_expr_head_kind(kind) {
            // Skip type-annotation subtrees — they begin with `:`.
            if trimmed.starts_with(':') {
                return;
            }
            *out = Some(view);
            return;
        }
        for child in view.children() {
            descend(child, out);
            if out.is_some() {
                return;
            }
        }
    }
    let mut out: Option<BbnfView<'a, 'a>> = None;
    for child in node.children() {
        descend(child, &mut out);
        if out.is_some() {
            return out;
        }
    }
    out
}

/// Find the type-annotation subtree inside a mapping subtree.
///
/// `type_annotation = ":" ?w , type_name` — its compound kind is
/// `Other` (sub-grammar), discriminated structurally by the
/// leading `:` byte of its span.
fn find_type_annotation_child<'a>(node: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
    fn descend<'a>(view: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
        let trimmed = view.span_text().trim();
        if trimmed.starts_with(':') {
            // Confirm it's not just a literal `:` standalone — a
            // type annotation always has at least one type-name
            // character after the colon.
            if trimmed.len() > 1 {
                return Some(view);
            }
        }
        for child in view.children() {
            if let Some(found) = descend(child) {
                return Some(found);
            }
        }
        None
    }
    descend(node)
}

/// Peel a `mapped_factor`'s anonymous-wrapper body.
///
/// Under structural emission, the mapped_factor rule body may be
/// wrapped in one or more anonymous (`Other`-kinded) compounds.
/// Collapse single-anonymous-child chains until the view's direct
/// children are the semantic slots.
fn peel_mapped_factor_body<'a>(mut view: BbnfView<'a, 'a>) -> BbnfView<'a, 'a> {
    loop {
        let children: Vec<BbnfView<'a, 'a>> = view.children().collect();
        if children.len() != 1 {
            return view;
        }
        let only_child = children[0];
        if !matches!(only_child.compound_kind(), Some(BbnfCompoundKind::Other)) {
            return view;
        }
        view = only_child;
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
    type_ann: Option<BbnfView<'a, 'a>>,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_ann.and_then(|ann| {
        // type_annotation = (":", type_name) — find the type-name
        // payload inside the annotation subtree.
        let trimmed = ann.span_text().trim();
        let stripped = trimmed.strip_prefix(':')?.trim();
        if stripped.is_empty() {
            return None;
        }
        Some(resolve_type_name(stripped, ctx))
    });

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

    // Numeric literal suffix → scalar type.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let t = leaf.span_text().trim_start();
        let first_byte = t.as_bytes().first().copied();
        let is_numeric = matches!(first_byte, Some(b'0'..=b'9') | Some(b'.'));
        if !is_numeric {
            return None;
        }
        let (_, suffix) = split_numeric_suffix(t);
        if suffix.is_empty() {
            None
        } else {
            Some(TypeDesc::from_scalar_name(suffix).unwrap_or_else(|| {
                let sid = ctx.strings.intern(suffix);
                TypeDesc::Named(sid)
            }))
        }
    });

    // Bool literal → bool type.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let t = leaf.span_text().trim_start();
        let is_word_boundary = |s: &str, len: usize| {
            !s.as_bytes()
                .get(len)
                .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
        };
        let is_bool = (t.starts_with("true") && is_word_boundary(t, 4))
            || (t.starts_with("false") && is_word_boundary(t, 5));
        if is_bool { Some(TypeDesc::Bool) } else { None }
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

    let map_expr = lower_value_expr(value_expr, ctx);
    let return_type = return_type.or_else(|| match &map_expr {
        MapExpr::BoolLit(_) => Some(TypeDesc::Bool),
        _ => None,
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
