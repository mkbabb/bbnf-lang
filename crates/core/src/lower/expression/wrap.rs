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
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

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

/// Lower a `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ?` view.
///
/// **Structural detection** (AZ-IV.W0.3): the canonical generated
/// `mapped_factor` parser does not wrap the optional mapping group
/// in an anonymous compound — the `->` punctuator is consumed via a
/// direct byte check (no Span pushed), and `value_expr` /
/// `type_annotation` surface as direct children of `mapped_factor`.
/// Predicate-driven detection (matching a child whose trimmed span
/// starts with `->`) silently misses this shape and drops the
/// `IrNode::Map { fn_id }` wrapper.
///
/// We classify children by structural role (compound kind +
/// positional order) rather than by source-prefix substring:
///   1. The first substantive non-Unit child is the `factor`
///      (compound_kind = Factor, or any compound carrying the
///      grouped/leaf term in inlined shape).
///   2. Subsequent substantive children are the mapping payload —
///      `value_expr` head followed by an optional `type_annotation`.
///   3. A `:`-prefixed span identifies the `type_annotation` even
///      when its compound_kind is `Other` (the `type_annotation`
///      sub-rule is not in `BbnfCompoundKind::from_rule_name`'s
///      alphabet).
///
/// Per `feedback_typed-materialization-invariant`, every `->` in the
/// grammar source must reach the tape emitter; if structural
/// detection finds a `value_expr` head past the factor but the
/// `Map { fn_id }` wrapper cannot be built, the function panics
/// rather than silently returning the bare factor.
pub(crate) fn lower_mapped_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let body = peel_mapped_factor_body(node);
    let mut term_node: Option<BbnfView<'a, 'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut value_expr_head: Option<BbnfView<'a, 'a>> = None;
    let mut type_annotation: Option<BbnfView<'a, 'a>> = None;
    let mut has_unit_marker = false;
    for c in body.children() {
        // AZ-IV.W1.6 (Fermat F8): the canonical generated modifier
        // emitter pushes the matched token as a typed Span via
        // `push_leaf_with_str` — Unit children would mean a
        // degenerate emitter push. Record its presence for the
        // typed-materialization invariant; the source-byte modifier
        // recovery is deleted (`recover_modifier` retired).
        if matches!(c.kind(), BbnfKind::Unit) {
            has_unit_marker = true;
            continue;
        }
        let span_text = c.span_text();
        let trimmed = span_text.trim();
        // AZ-IV.W1.2 — value-expression compounds whose body projects
        // to a typed leaf (`MapExpr::IntLit`, `FloatLit`, etc.) collapse
        // to a `BbnfValue::Int(_)` / `Float(_)` / etc. payload at parse
        // time, leaving the wrapping `value_expr` / `value_or` /
        // `value_and` / `value_atom` compound with `byte_span() == None`
        // and `span_text() == ""`. The pre-W1.2 loop's blanket
        // empty-span skip dropped those compounds — the `Map { fn_id:
        // IntLit(N) }` wrapper for every `"#N/A" -> 0u8` /
        // `"+" -> 0u8` style declaration silently vanished.
        //
        // Accept compounds whose `compound_kind` identifies a
        // value-expression head even when their span is empty;
        // span-driven classification still discriminates among Span
        // leaves (modifier punctuators, `->`/`=>` arrows,
        // type-annotation `:` prefixes, factor source slices).
        let is_value_expr_compound_head = matches!(
            c.compound_kind(),
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
            ),
        );
        if trimmed.is_empty() && !is_value_expr_compound_head {
            continue;
        }
        if matches!(trimmed, "?" | "?w" | "*" | "+") {
            modifier_text = Some(trimmed.to_string());
            continue;
        }
        if trimmed.starts_with("->") || trimmed.starts_with("=>") {
            // Legacy bootstrap_parser shape (pre-AZ-II.cutover.D)
            // wrapped the mapping group in an anonymous compound
            // whose first byte was the arrow. The modern canonical
            // parser elides that wrapper, but we honour the legacy
            // shape via fallthrough so any orphan path that still
            // produces it routes through `find_value_expr_child` /
            // `find_type_annotation_child` below.
            value_expr_head = find_value_expr_child(c).or(value_expr_head);
            if type_annotation.is_none() {
                type_annotation = find_type_annotation_child(c);
            }
            continue;
        }
        // AZ-IV.W1.9 — type_annotation now carries a dedicated
        // structural kind. Detect by compound_kind first so the
        // `:` literal's missing Span (the literal is consumed
        // without a Span push under struct-direct projection)
        // does NOT silently drop the annotation when the sub-grammar
        // collapses to a Unit + branch_tag pair.
        if matches!(c.compound_kind(), Some(BbnfCompoundKind::TypeAnnotation)) {
            type_annotation = Some(c);
            continue;
        }
        // Legacy span-based detection — surviving for orphan shapes
        // that produce a `:`-prefixed source span (e.g. host-directive
        // type annotations, which are not the same compound as
        // value_expr's `type_annotation`).
        if trimmed.starts_with(':') {
            type_annotation = Some(c);
            continue;
        }
        // Value-expression compound head — claim it as the value_expr
        // even when its span text is empty (typed-leaf projection
        // dropped the source bytes). The factor's compound_kind is
        // `Factor` / `Term`, never one of the value_expr kinds, so
        // ordering against the term_node placement is unambiguous.
        if is_value_expr_compound_head {
            if value_expr_head.is_none() {
                value_expr_head = Some(c);
            }
            continue;
        }
        if term_node.is_none() {
            // First substantive non-arrow child is the factor.
            term_node = Some(c);
            continue;
        }
        // Subsequent substantive child past the factor is the
        // value_expr head. Type-annotations were filtered by the
        // `:`-prefix branch above.
        if value_expr_head.is_none() {
            value_expr_head = Some(c);
        }
    }
    // AZ-IV.W1.6 typed-materialization invariant: a `Unit` modifier
    // marker without a structural span-text resolution means the
    // codegen modifier emitter ran without pushing a typed Span
    // — same defect class as the pre-W1.6 source-byte recovery
    // covered up. Surface it loudly so the canonical parser is
    // fixed at source rather than the lower path absorbing the
    // information loss.
    if has_unit_marker && modifier_text.is_none() {
        panic!(
            "mapped_factor: Unit modifier marker without typed-Span resolution \
             in span {:?} ({} children) — typed-materialization invariant \
             violated. Post-W1.6 the modifier emitter pushes typed Span via \
             push_leaf_with_str.",
            node.span_text(),
            node.num_children(),
        );
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
    // Typed-materialization invariant: if the source contains a
    // `->` arrow (detectable from the compound's own span_text) but
    // structural detection failed to find a value_expr head, panic
    // loudly rather than silently dropping the Map wrapper.
    if value_expr_head.is_none() {
        let raw = node.span_text();
        if find_unquoted(raw, "->").is_some() || find_unquoted(raw, "=>").is_some() {
            panic!(
                "mapped_factor: source span {:?} contains a `->`/`=>` arrow \
                 but structural detection found no value_expr head among the \
                 compound's {} children — typed-materialization invariant violated",
                raw,
                node.num_children(),
            );
        }
        return base;
    }
    let value_expr = value_expr_head.unwrap();
    // AZ-IV.W1.9 — when the type_annotation compound's `byte_span()`
    // is empty (the `:` literal is consumed without a Span push and
    // `type_name`'s alt branches push only `push_branch_tag` +
    // `push_leaf_with_unit`), the annotation text must be recovered
    // from the enclosing `mapped_factor`'s source. Pass the parent
    // span as the fallback recovery source so `lower_map_arrow` can
    // re-extract the `: <Type>` suffix structurally.
    let parent_span_for_type = node.span_text();
    let fn_id = lower_map_arrow(value_expr, type_annotation, parent_span_for_type, ctx);
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
        // Type-annotation compounds are never the value-expression
        // head — short-circuit before any span-text inspection so the
        // walk does not mis-claim a typed `: <Type>` suffix.
        if matches!(kind, Some(BbnfCompoundKind::TypeAnnotation)) {
            return;
        }
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

/// AZ-IV.W1.9 — recover the trailing `: <Type>` suffix from a
/// `mapped_factor`'s source text when the structural type_annotation
/// compound has collapsed to an empty span.
///
/// Walks the parent source from the end, finds the last `:` that's
/// NOT part of a `::` value-path separator, and returns the trimmed
/// type-name identifier that follows.
///
/// Returns `None` when no `:`-suffix is present (e.g. an arrow
/// without an annotation), when the suffix is empty, or when the
/// candidate is not a valid identifier (filtering out malformed
/// recoveries).
fn recover_type_name_from_parent(source: &str) -> Option<&str> {
    let bytes = source.as_bytes();
    let mut idx = bytes.len();
    while idx > 0 {
        idx -= 1;
        if bytes[idx] != b':' {
            continue;
        }
        // Skip the second `:` of a `::` value-path separator.
        if idx > 0 && bytes[idx - 1] == b':' {
            idx -= 1;
            continue;
        }
        if idx + 1 < bytes.len() && bytes[idx + 1] == b':' {
            // Leading `:` of a `::` separator — keep walking.
            continue;
        }
        let candidate = source[idx + 1..].trim();
        if candidate.is_empty() {
            return None;
        }
        // Validate identifier shape — type names match
        // `[_a-zA-Z][_a-zA-Z0-9]*`.
        let bytes = candidate.as_bytes();
        if !(bytes[0].is_ascii_alphabetic() || bytes[0] == b'_') {
            return None;
        }
        if !bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_')
        {
            return None;
        }
        return Some(candidate);
    }
    None
}

/// Lower a `->` mapping to a `FnId`.
fn lower_map_arrow<'a>(
    value_expr: BbnfView<'a, 'a>,
    type_ann: Option<BbnfView<'a, 'a>>,
    parent_source: &str,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_ann.and_then(|ann| {
        // type_annotation = (":", type_name) — find the type-name
        // payload inside the annotation subtree.
        //
        // AZ-IV.W1.9: under struct-direct projection the `:` literal
        // is consumed without a Span push and `type_name`'s alt
        // branches deposit only `push_branch_tag` + `push_leaf_with_unit`,
        // so the type_annotation compound's `byte_span()` collapses
        // to empty. The annotation text must then be recovered from
        // the enclosing `mapped_factor`'s span — find the trailing
        // ` : <Type>` (avoiding `::` value-path separators) and
        // resolve the identifier.
        let trimmed = ann.span_text().trim();
        if let Some(stripped) = trimmed.strip_prefix(':') {
            let inner = stripped.trim();
            if !inner.is_empty() {
                return Some(resolve_type_name(inner, ctx));
            }
        }
        // Fallback: parse the parent `mapped_factor`'s source for the
        // trailing `:`-suffixed type name. Walk from the end so we
        // find the LAST `:`, ignoring `::` separators in value-path
        // segments (`std::convert::From`).
        recover_type_name_from_parent(parent_source).map(|name| resolve_type_name(name, ctx))
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
