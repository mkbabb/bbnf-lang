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

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::LowerCtx;
use super::super::tape_walk::find_descendant_by_kind;
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
/// mapping is detected via span emptiness: when present, the
/// mapping carries `(arrow_keyword, value_expr_view, type_view?)`
/// as its children (regardless of whether the optimizer wrapped
/// them in a sub-compound).
pub(crate) fn lower_mapped_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Under the clean regen, `factor` is inlined into
    // `mapped_factor`, so this compound's children are
    //   `[big_comment?, term, modifier?, big_comment?, mapping?]`
    // with each optional slot represented by an empty-span
    // placeholder. Classify children by span content rather than
    // by positional index: the modifier is the child whose trimmed
    // span is one of `?` / `?w` / `*` / `+`; the mapping group is
    // the child whose trimmed span starts with `->` / `=>`; the
    // term is the first remaining substantive child.
    //
    // Under DTA the mapped_factor body is wrapped in one or more
    // anonymous Seq compounds (walker sentinel rule_kind ∈
    // {Unknown, int_lit}). A direct `node.children()` iteration
    // enumerates `[Seq]` rather than the semantic [term, modifier,
    // mapping] layout; every span starts from the factor's leading
    // byte, so the `trimmed.starts_with("->")` mapping-detection
    // check never fires and the `->` annotation silently drops.
    // Descend through anonymous wrappers to the true body whose
    // direct children carry the semantic slots.
    let body = peel_mapped_factor_body(node);
    let mut term_node: Option<BbnfBootstrapNodeView<'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut mapping_node: Option<BbnfBootstrapNodeView<'a>> = None;
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
    // mapped_factor layer. When `term_node` is present, the term's
    // span carries the bracket delimiter; `dispatch_expression` →
    // `lower_term` inspects the span's leading byte and routes to
    // `lower_grouped_term`, which applies the `Repeat` / `Map`
    // wrapping exactly once. Applying a second wrap here produced a
    // double-`Repeat` for every EBNF `{ letter | digit | "_" }` site
    // and every `{ character - "'" }` — the resulting DTA had nested
    // Repeat states and the `identifier` parser looped at offset 0.
    //
    // The `term_node = None` branch below recovers a leaf from the
    // compound's span text; that path never had the wrapping call —
    // it returns the bare leaf and the caller handles `{...}`
    // wrapping one level higher in the grammar.
    let mut base = if let Some(term) = term_node {
        dispatch_expression(term, ctx)
    } else {
        // No tape-level term child — the compound's body is a bare
        // leaf (identifier, literal, regex) that consumed bytes
        // without pushing a record. Recover the leaf from the
        // compound's own span_text after stripping any trailing
        // modifier (`?w`/`?`/`*`/`+`) and any trailing mapping
        // group (`-> ...` / `=> ...`).
        let raw = node.span_text();
        let mut stripped: &str = raw.trim();
        // Strip trailing modifier first (it's closest to the term).
        if let Some(modifier) = &modifier_text {
            stripped = stripped
                .strip_suffix(modifier.as_str())
                .unwrap_or(stripped)
                .trim();
        }
        // Strip mapping group — everything from `->` / `=>` onward.
        // Use find_unquoted to avoid matching `->` inside quoted
        // literals like `"->"`.
        if let Some(idx) = find_unquoted(stripped, "->") {
            stripped = stripped[..idx].trim();
        } else if let Some(idx) = find_unquoted(stripped, "=>") {
            stripped = stripped[..idx].trim();
        }
        lower_leaf_by_span_text_str(stripped, ctx).unwrap_or_else(|| {
            panic!(
                "mapped_factor: no tape term child and span_text {:?} (after stripping \
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
    // Extract the value_expr + optional type_annotation. The
    // mapping group's children are normally [arrow_kw, value_expr,
    // type_annotation?]; under flattened shapes, walk the children
    // and pluck the value_expr / type_annotation by rule_kind.
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

fn find_value_expr_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // Outermost-first descendant search. Each call to
    // `find_descendant_by_kind` returns the first occurrence in
    // document order, so the lookups run from the outermost class
    // (`value_expr`) down through each precedence wrapper to the
    // inlined atoms. First hit wins.
    //
    // Under DTA, structurally-anonymous groupings (the mapping's
    // `( "->" ?w , ( value_expr , ... ) )` parent) emit Seq compounds
    // whose `variant_idx` was never stamped by a `DtaState::Ref`
    // dispatch — they surface via the `int_lit`/`Unknown` sentinel on
    // `rule_kind()`. Iterating direct children and admitting the
    // whole value-layer set (including `int_lit` for the real literal
    // leaf case) would return the sentinel Seq before ever reaching
    // the real `value_expr` inside. Targeting specific kinds in
    // outermost-first priority order avoids the sentinel trap — no
    // descendant of a mapping Seq has `rule_kind == value_expr`
    // unless it's a genuine `value_expr` record.
    const OUTER_KINDS: &[BbnfBootstrapRuleKind] = &[
        BbnfBootstrapRuleKind::value_expr,
        BbnfBootstrapRuleKind::value_closure,
        BbnfBootstrapRuleKind::value_or,
        BbnfBootstrapRuleKind::value_and,
        BbnfBootstrapRuleKind::value_cmp,
        BbnfBootstrapRuleKind::value_add,
        BbnfBootstrapRuleKind::value_mul,
        BbnfBootstrapRuleKind::value_unary,
        BbnfBootstrapRuleKind::value_atom,
        BbnfBootstrapRuleKind::value_fn_call,
        BbnfBootstrapRuleKind::value_path,
        BbnfBootstrapRuleKind::value_input,
        BbnfBootstrapRuleKind::value_ident,
        BbnfBootstrapRuleKind::float_lit,
        BbnfBootstrapRuleKind::bool_lit,
        BbnfBootstrapRuleKind::string_lit,
        // `int_lit` goes last — real int_lit leaves carry their
        // numeric literal span; if a sentinel-tagged Seq wrapper also
        // carries `int_lit` its span will overlap (or exceed) a real
        // value_expr descendant, which the outermost lookups above
        // resolve first.
        BbnfBootstrapRuleKind::int_lit,
    ];

    for &kind in OUTER_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            return Some(v);
        }
    }
    None
}

fn find_type_annotation_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // The `type_annotation` compound lives at unpredictable depths
    // under DTA's Seq wrappers (the mapping grouping may nest one
    // or more structurally-anonymous Seq layers between the
    // `mapping_node` and the inner `type_annotation`). A descendant
    // search handles both fn-per-rule and DTA shapes uniformly.
    find_descendant_by_kind(node, BbnfBootstrapRuleKind::type_annotation)
}

/// Peel a `mapped_factor`'s anonymous-wrapper body.
///
/// Under DTA, the mapped_factor rule body is often wrapped in one
/// or more anonymous Seq compounds emitted by the walker (stamped
/// with the sentinel rule_kind `int_lit` or `Unknown` because no
/// `DtaState::Ref` captured a semantic rule identity). The caller
/// wants to iterate the semantic `[term, modifier?, mapping?]`
/// slots — direct `node.children()` would return the wrapper.
///
/// Collapse single-anonymous-child chains until the view's direct
/// children are the semantic slots. If there's no descent to do
/// (the direct children are already the semantic slots, or the
/// lone child isn't anonymous), return `node` unchanged.
fn peel_mapped_factor_body<'a>(
    mut view: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    use crate::runtime::tape::TapeKind;
    loop {
        let children: Vec<BbnfBootstrapNodeView<'a>> = view.children().collect();
        if children.len() != 1 {
            return view;
        }
        let only_child = children[0];
        let is_anon_wrapper = matches!(
            only_child.kind(),
            TapeKind::Rule | TapeKind::Seq | TapeKind::Alt | TapeKind::Repeat,
        ) && matches!(
            only_child.rule_kind(),
            BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
        );
        if !is_anon_wrapper {
            return view;
        }
        view = only_child;
    }
}

// ─── Grouped-term lowering ────────────────────────────────────────────────────

/// Descend into the inner expression of a grouped term compound and apply
/// the grouping operator.
pub(super) fn lower_grouped_term<'a>(
    node: BbnfBootstrapNodeView<'a>,
    kind: GroupKind,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let inner_view = find_inner_expression(node).unwrap_or_else(|| {
        panic!(
            "lower_term (grouped): missing inner expression in span {:?}",
            node.span_text(),
        )
    });
    // `lower_rhs` peels `rhs`/`grammar_item`/`directive`/`lhs` wrappers
    // before dispatching, matching the top-level rule body entry point.
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
/// Under DTA the walker emits the opening and closing delimiters
/// as `TapeKind::Literal` / `Span` leaves alongside a Seq compound
/// that carries the body's semantic children. Fn-per-rule emission
/// placed the body compound directly as a child of the grouped
/// term; DTA's structural lifter wraps it one Seq deeper, so a
/// direct-children scan misses the inner expression.
///
/// Strategy (in order):
///
/// 1. **Primary**: `find_descendant_by_kind` for each expression-
///    layer rule kind in outermost-first order (`rhs`, `alternation`,
///    `concatenation`, `binary_factor`, `mapped_factor`, `factor`,
///    `term`, `closure`). First match wins — the outermost class
///    that surfaces in the descendants is the root of the inner
///    subtree. Returning at the first hit avoids picking a nested
///    `term` when the body is a multi-branch alternation.
/// 2. **Fallback**: iterate descendants in document order; stop at
///    the first compound whose span is non-empty and whose
///    `rule_kind()` is a body-expression class (the same set as
///    above). Handles tape shapes where the outermost wrapper was
///    inlined away by the lifter.
///
/// Bracket Literal leaves are skipped implicitly — their `rule_kind`
/// is not in the expression-layer set.
fn find_inner_expression<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // Outermost-first: a nested `term` inside a multi-branch
    // alternation must not pre-empt the alternation itself.
    const EXPRESSION_KINDS: &[BbnfBootstrapRuleKind] = &[
        BbnfBootstrapRuleKind::rhs,
        BbnfBootstrapRuleKind::closure,
        BbnfBootstrapRuleKind::alternation,
        BbnfBootstrapRuleKind::concatenation,
        BbnfBootstrapRuleKind::binary_factor,
        BbnfBootstrapRuleKind::mapped_factor,
        BbnfBootstrapRuleKind::factor,
        BbnfBootstrapRuleKind::term,
    ];

    for &kind in EXPRESSION_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            let (lo, hi) = v.span();
            if hi > lo {
                return Some(v);
            }
        }
    }

    // Fallback: document-order descent looking for any compound
    // with an expression-layer rule_kind (covers tape shapes the
    // outermost-first scan missed — e.g. if the lifter inlined
    // every layer down to an alternation sub-branch without an
    // `alternation` kind surviving).
    find_body_expression_descendant(node, EXPRESSION_KINDS)
}

/// Document-order descent helper — returns the first compound under
/// `view` (inclusive) whose `rule_kind()` is in `kinds` and whose
/// span is non-empty. Used as the fallback arm of
/// [`find_inner_expression`].
fn find_body_expression_descendant<'a>(
    view: BbnfBootstrapNodeView<'a>,
    kinds: &[BbnfBootstrapRuleKind],
) -> Option<BbnfBootstrapNodeView<'a>> {
    let kind = view.rule_kind();
    let (lo, hi) = view.span();
    if hi > lo && kinds.contains(&kind) {
        return Some(view);
    }
    for child in view.children() {
        if let Some(found) = find_body_expression_descendant(child, kinds) {
            return Some(found);
        }
    }
    None
}

// ─── MapArrow / ValueExpr lowering ─────────────────────────────────────────────

/// Resolve a type name string to a `TypeDesc`, preferring concrete scalar
/// variants (`TypeDesc::F64`, `TypeDesc::U8`, etc.) over the generic
/// `TypeDesc::Named`. Falls back to `Named` for unknown type names so
/// backend-specific resolution can still occur.
fn resolve_type_name(name: &str, ctx: &mut LowerCtx<'_>) -> TypeDesc {
    TypeDesc::from_scalar_name(name).unwrap_or_else(|| {
        let sid = ctx.strings.intern(name);
        TypeDesc::Named(sid)
    })
}

/// Lower a `->` mapping to a `FnId`.
///
/// `value_expr` is the value expression node, `type_ann` is the
/// optional type annotation node.
fn lower_map_arrow<'a>(
    value_expr: BbnfBootstrapNodeView<'a>,
    type_ann: Option<BbnfBootstrapNodeView<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_ann.and_then(|ann| {
        if ann.rule_kind() == BbnfBootstrapRuleKind::type_annotation {
            // type_annotation = (":", type_node) — child(1) is the name.
            let type_node = ann.child(1)?;
            let name = match type_node.rule_kind() {
                BbnfBootstrapRuleKind::type_name | BbnfBootstrapRuleKind::identifier => {
                    type_node.span_text()
                }
                _ => return None,
            };
            Some(resolve_type_name(name, ctx))
        } else {
            None
        }
    });

    // Type-shorthand: bare type name like `-> f64`.
    // unwrap_value_ident_str recursively peels value expression wrappers.
    if let Some(name) = unwrap_value_ident_str(value_expr) {
        if is_type_name(name) && return_type.is_none() {
            let td = resolve_type_name(name, ctx);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(td),
            });
        }
    }

    // Extract type suffix from integer/float literals when no explicit type annotation.
    // The tape-rewrite may fold `int_lit`/`float_lit` into `value_atom`,
    // so when the leaf rule_kind is `value_atom` we recover the type
    // suffix from the span text — the span IS the authoritative source
    // of the literal's textual form.
    // Numeric literal suffix → scalar type.
    //
    // Every leading-byte-numeric literal carries its textual form in
    // its span; the suffix disambiguates the scalar type (`0u8`, `3.14f64`).
    // Under DTA the rule_kind is one of `int_lit` / `float_lit` (real
    // leaf rules), `value_atom` (optimizer inlined the leaf into the
    // atom), or `int_lit` / `Unknown` as the walker's sentinel for
    // compounds never stamped by a `DtaState::Ref`. All cases share
    // the same disambiguator — a digit or `.` leading the trimmed
    // span text. Gate on that; rule_kind variance across inlining
    // modes is structural noise.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let t = leaf.span_text().trim_start();
        let first_byte = t.as_bytes().first().copied();
        let kind_admits = matches!(
            leaf.rule_kind(),
            BbnfBootstrapRuleKind::int_lit
                | BbnfBootstrapRuleKind::float_lit
                | BbnfBootstrapRuleKind::value_atom
                | BbnfBootstrapRuleKind::Unknown
        );
        let is_numeric = matches!(first_byte, Some(b'0'..=b'9') | Some(b'.'));
        if !(kind_admits && is_numeric) {
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
    //
    // `bool_lit` is the canonical rule_kind when the grammar's own
    // `bool_lit` rule surfaces its own compound. Under DTA the
    // optimizer frequently inlines `value_atom`'s alt of leaves into
    // the atom compound (rule_kind stays `value_atom`) or further
    // inlines the atom wrapper itself into an anonymous Seq whose
    // walker sentinel rule_kind lands as `int_lit` / `Unknown`.
    // Every case collapses to the same disambiguation: the trimmed
    // span text is exactly `true` or `false` bounded by a non-
    // identifier byte. Gate on that disambiguator rather than on a
    // rule_kind whitelist — rule_kind drift under DTA is structural
    // noise, not a signal.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let t = leaf.span_text().trim_start();
        let is_word_boundary = |s: &str, len: usize| {
            !s.as_bytes()
                .get(len)
                .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
        };
        let is_bool = matches!(leaf.rule_kind(), BbnfBootstrapRuleKind::bool_lit)
            || matches!(
                leaf.rule_kind(),
                BbnfBootstrapRuleKind::value_atom
                    | BbnfBootstrapRuleKind::int_lit
                    | BbnfBootstrapRuleKind::Unknown
            ) && ((t.starts_with("true") && is_word_boundary(t, 4))
                || (t.starts_with("false") && is_word_boundary(t, 5)));
        if is_bool {
            Some(TypeDesc::Bool)
        } else {
            None
        }
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

    ctx.fns.push(FnDescriptor::Expr {
        expr: map_expr,
        return_type,
    })
}

// ─── Specialization ────────────────────────────────────────────────────────────

fn try_specialize_map_fn(inner: &IrNode, fn_id: FnId, ctx: &mut LowerCtx<'_>) -> FnId {
    let desc = &ctx.fns.fns[fn_id as usize];

    // Extract the expression and type name for specialization.
    // Handles both concrete scalar TypeDescs (TypeDesc::F64, etc.) and
    // legacy Named("f64") — the latter may still appear from explicit
    // type annotations using unknown names.
    //
    // AU.2.4: The type annotation is optional — when it's absent, we
    // attempt structural inference from the (regex, expr) pair: a
    // HexDigits regex paired with `FnCall(_, [Input])` is a hex
    // converter, even without an explicit `: u32` annotation. This
    // side-steps a bootstrap-grammar surface-syntax quirk where
    // `-> fn_call(input) : type_name` loses the type annotation
    // between the function-call arg list and the type name.
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
            // Type-free specialization: infer from (regex, expr) shape.
            // Currently only the hex-converter pattern is inferred;
            // number-conversion already requires the `-> f64` shorthand
            // to carry type information explicitly.
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
                    return ctx
                        .fns
                        .push(FnDescriptor::HexConvert { fn_path: path_sid });
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
                if let RegexClass::Numeric { allow_leading_dot, .. } = classify_regex(&pattern) {
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
