//! Atom-layer lowering: unary prefixes, parenthesised expressions,
//! identifier-shaped atoms, function calls, paths, and string literals.

use bbnf_ir::{MapExpr, MapUnaryOp};

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfValue, BbnfView};
use crate::runtime::RuntimeView;

use super::super::LowerCtx;
use super::dispatch_value_expr;
use super::literals::{parse_float_literal, parse_numeric_literal_text};
use super::simple_kinds::lookup_value_env;
use super::view_walk::find_descendant_by_compound_kind;

// ─── Unary ───────────────────────────────────────────────────────────────────

/// Lower a `value_unary` compound. Body is `( "!" | "-" )? value_atom`.
/// The prefix consumes bytes without pushing, so we identify it by
/// inspecting the unary compound's span text. If the leading byte
/// is `!` or `-`, wrap the atom in a unary op; otherwise the body
/// is the bare atom.
pub(super) fn lower_value_unary<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let text = node.span_text_opt().unwrap_or("");
    let first_byte = text.as_bytes().first().copied();
    match first_byte {
        Some(b'!') | Some(b'-') => {
            let atom = first_atom_child(node).unwrap_or(node);
            let inner = dispatch_value_expr(atom, ctx);
            let op = if first_byte == Some(b'!') {
                MapUnaryOp::Not
            } else {
                MapUnaryOp::Neg
            };
            MapExpr::UnaryOp {
                op,
                inner: Box::new(inner),
            }
        }
        _ => {
            // Bare atom — descend through the single child.
            let atom = first_atom_child(node).unwrap_or(node);
            // No child compound at all — fall back to atom dispatch
            // on `node` itself (which classifies the span text).
            if same_focus(atom, node) {
                lower_value_atom(node, ctx)
            } else {
                dispatch_value_expr(atom, ctx)
            }
        }
    }
}

/// Find the first child compound of a unary view that looks like an
/// atom (or a transparent wrapper around one). The grammar
/// guarantees `value_atom` is the sole child compound.
fn first_atom_child<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
) -> Option<BbnfView<'a, 'p>> {
    find_descendant_by_compound_kind(node, BbnfCompoundKind::ValueAtom)
        .filter(|v| !same_focus(*v, node))
        .or_else(|| RuntimeView::children(&node).next())
}

/// Compare two views' focused values for identity. Replaces the
/// tape-shaped predecessor's `cursor().offset()` equality test —
/// struct-direct focuses are compared by underlying `BbnfValue`
/// payload (compound IDs are unique per parse, leaf payloads identify
/// themselves).
pub(super) fn same_focus<'a, 'p: 'a>(
    a: BbnfView<'a, 'p>,
    b: BbnfView<'a, 'p>,
) -> bool {
    match (a.focus(), b.focus()) {
        (BbnfValue::Compound(x), BbnfValue::Compound(y)) => x == y,
        (BbnfValue::Span(x), BbnfValue::Span(y)) => {
            x.as_ptr() == y.as_ptr() && x.len() == y.len()
        }
        (BbnfValue::Int(x), BbnfValue::Int(y)) => x == y,
        (BbnfValue::Float(x), BbnfValue::Float(y)) => x.to_bits() == y.to_bits(),
        (BbnfValue::Bool(x), BbnfValue::Bool(y)) => x == y,
        (BbnfValue::Tag(x), BbnfValue::Tag(y)) => x == y,
        (BbnfValue::Unit, BbnfValue::Unit) => true,
        _ => false,
    }
}

// ─── Atom classification ─────────────────────────────────────────────────────

/// Lower a `value_atom` compound. Under struct-direct projection the
/// atom's leaf alts (int/float/bool/string) collapse to their typed
/// payload directly — `BbnfValue::Int` / `Float` / `Bool` / `Span`.
/// The compound surfaces only when the atom resolved to a parenthesised
/// sub-expression or a multi-segment path / fn-call structural body.
///
/// Disambiguation walks the atom's span text from the first
/// non-whitespace byte:
///
///   - `0`-`9` or `.` followed by digit → numeric literal
///   - `'` / `"` → string literal
///   - `t` / `f` (followed by `rue`/`alse`) → bool literal
///   - `(` → parenthesised sub-expression — descend into the
///     value_expr child
///   - leading `input` keyword (`input` followed by `.` or end) →
///     input chain
///   - identifier-leading + `(` → function call
///   - identifier-leading + `::` → path
///   - bare identifier → ident
pub(super) fn lower_value_atom<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // Leaf-projected payloads: dispatch directly on the typed leaf.
    match node.focus() {
        BbnfValue::Int(v) => return MapExpr::IntLit(v),
        BbnfValue::Float(v) => return MapExpr::FloatLit(v),
        BbnfValue::Bool(v) => return MapExpr::BoolLit(v),
        BbnfValue::Span(s) => {
            return classify_span_atom(s, ctx);
        }
        BbnfValue::Compound(_) => {}
        BbnfValue::Tag(_) | BbnfValue::Unit => {
            panic!(
                "lower/value_expr/atom.rs: lower_value_atom on Tag / Unit \
                 leaf — value-expression atoms never project to these arms"
            );
        }
    }

    // Compound atom — classify by span text.
    let text = node.span_text_opt().unwrap_or_else(|| {
        panic!(
            "lower/value_expr/atom.rs: lower_value_atom on compound focus \
             with no recoverable span (kind = {:?})",
            node.compound_kind(),
        )
    });
    let trimmed = text.trim_start();
    let first = trimmed.as_bytes().first().copied();

    match first {
        Some(b'0'..=b'9') => parse_numeric_literal_text(trimmed),
        Some(b'.') => {
            // `.5e3` — leading dot float.
            parse_float_literal(trimmed)
        }
        Some(b'"') => MapExpr::StringLit(intern_string_lit_inner(trimmed, ctx)),
        Some(b'\'') => MapExpr::StringLit(intern_string_lit_inner(trimmed, ctx)),
        Some(b'(') => lower_paren_atom(node, ctx),
        Some(b'!') | Some(b'-') => {
            // Unary prefix leaked into atom dispatch — the optimizer
            // collapsed the `value_unary` wrapper and what reached us
            // is a unary-shaped compound tagged `value_atom`. Route
            // through the unary path so the `!` / `-` prefix is honoured.
            lower_value_unary(node, ctx)
        }
        Some(b) if b == b'_' || (b as char).is_ascii_alphabetic() => {
            lower_atom_named(node, trimmed, ctx)
        }
        Some(_) | None => panic!(
            "lower/value_expr/atom.rs: lower_value_atom saw an unexpected \
             leading byte in atom span {:?} (compound_kind = {:?})",
            text,
            node.compound_kind(),
        ),
    }
}

/// Classify a `Span`-shaped atom by its leading byte. The atom may
/// be a quoted string literal, an `input` chain head, a bare ident,
/// or a path / fn-call surface (recovered via slice walk).
fn classify_span_atom<'p>(text: &'p str, ctx: &mut LowerCtx<'p>) -> MapExpr {
    let trimmed = text.trim_start();
    let first = trimmed.as_bytes().first().copied();
    match first {
        Some(b'"') | Some(b'\'') => {
            MapExpr::StringLit(intern_string_lit_inner(trimmed, ctx))
        }
        _ => lower_bare_ident(trimmed, ctx),
    }
}

/// Public entry for lowering a `Span`-payload value-expression leaf
/// (an identifier, value_ident, or string literal that the typed
/// projection delivered as `BbnfValue::Span`). Routes through
/// [`classify_span_atom`] — surfaced for use by the top-level
/// dispatcher's leaf fast-path in `mod.rs`.
pub(super) fn lower_bare_ident_or_string<'p>(
    text: &'p str,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    classify_span_atom(text, ctx)
}

/// Lower a parenthesised atom: `( value_expr )`. The structural
/// shape pushes only the inner `value_expr` compound (the parens
/// consume bytes without pushing). The grammar guarantees exactly
/// one semantic child compound here.
fn lower_paren_atom<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let inner = find_descendant_by_compound_kind(node, BbnfCompoundKind::ValueExpr)
        .or_else(|| RuntimeView::children(&node).next())
        .expect("lower_paren_atom: parenthesised atom is missing its value_expr child");
    dispatch_value_expr(inner, ctx)
}

/// Lower an atom whose leading text is identifier-shaped: bool
/// literal, `input` chain, function call, path, or bare ident.
/// Disambiguates from the source slice without depending on
/// per-leaf rule_kind pushes.
fn lower_atom_named<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    trimmed: &'p str,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // Bool literals.
    if trimmed.starts_with("true") && !next_ident_byte(trimmed, 4) {
        return MapExpr::BoolLit(true);
    }
    if trimmed.starts_with("false") && !next_ident_byte(trimmed, 5) {
        return MapExpr::BoolLit(false);
    }

    // Input chain: `input` keyword followed by zero or more `.ident`
    // accessors. The structural-mode tape pushes a single Repeat
    // compound for the prop chain (always pushed even when empty).
    if trimmed.starts_with("input") && !next_ident_byte(trimmed, 5) {
        return lower_input_chain(node, trimmed, ctx);
    }

    // Identifier head: scan the contiguous identifier run.
    let head_len = scan_ident_len(trimmed);
    debug_assert!(
        head_len > 0,
        "lower_atom_named: failed to extract identifier head from {:?}",
        trimmed,
    );
    let after_head = trimmed[head_len..].trim_start();
    if after_head.starts_with('(') {
        return lower_fn_call_atom(node, trimmed, ctx);
    }
    if after_head.starts_with("::") {
        return lower_path_atom(node, trimmed, ctx);
    }
    // Bare ident — possibly bound by a value-closure parameter.
    let name = &trimmed[..head_len];
    lower_bare_ident(name, ctx)
}

/// True iff `text[idx]` looks like a continuation byte of an
/// identifier. Used to ensure `true` / `input` keywords aren't
/// misidentified as the prefix of a longer identifier.
fn next_ident_byte(text: &str, idx: usize) -> bool {
    text.as_bytes().get(idx).is_some_and(|b| {
        b.is_ascii_alphanumeric() || *b == b'_'
    })
}

/// Scan the leading identifier run length: `[_a-zA-Z][_a-zA-Z0-9]*`.
pub(super) fn scan_ident_len(text: &str) -> usize {
    let bytes = text.as_bytes();
    let mut i = 0;
    if let Some(b) = bytes.first() {
        if b.is_ascii_alphabetic() || *b == b'_' {
            i += 1;
            while let Some(b) = bytes.get(i) {
                if b.is_ascii_alphanumeric() || *b == b'_' {
                    i += 1;
                } else {
                    break;
                }
            }
        }
    }
    i
}

/// Lower an `input` chain. The atom compound's children are exactly
/// one `Repeat` compound (the prop chain — always pushed, may be
/// empty). Each prop-chain iteration consumed `.` then an
/// identifier; the identifier text comes from the source slice
/// after the dot.
pub(super) fn lower_input_chain<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    trimmed: &'p str,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // The grammar produces `Repeat(.ident)*`. Recover the prop name
    // by scanning the source slice past the `input` keyword: each
    // `.ident` segment yields one prop name; we use the LAST prop
    // (matching the legacy semantics — only the leaf prop is
    // surfaced as a `MapExpr::InputProp`).
    let after = &trimmed[5..]; // skip "input"
    let mut last_prop: Option<&str> = None;
    let mut rest = after;
    loop {
        let stripped = rest.trim_start();
        if let Some(after_dot) = stripped.strip_prefix('.') {
            let after_dot = after_dot.trim_start();
            let name_len = scan_ident_len(after_dot);
            if name_len == 0 {
                break;
            }
            last_prop = Some(&after_dot[..name_len]);
            rest = &after_dot[name_len..];
        } else {
            break;
        }
    }
    // Suppress unused warning on `node` — the source-slice walk above
    // is the canonical recovery path; we keep the parameter to mirror
    // the other atom lowering helpers.
    let _ = node;
    match last_prop {
        Some(name) => {
            let sid = ctx.strings.intern(name);
            MapExpr::InputProp { prop: sid }
        }
        None => MapExpr::Input,
    }
}

/// Lower a function-call atom: `path(args?)`. The atom compound's
/// children are: `Repeat` (path segments — always pushed),
/// `Repeat` (the optional arg list — always pushed; contains
/// `[value_expr, Repeat([rest_value_exprs])]` when non-empty,
/// otherwise empty).
pub(super) fn lower_fn_call_atom<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    trimmed: &'p str,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // Recover the full path from the source slice. The path is
    // `ident (:: ident)*`; everything up to the opening `(` belongs
    // to the path.
    let path_text = recover_call_path(trimmed);
    let name_sid = ctx.strings.intern(&path_text);

    // Walk the atom's children to find the arg list.
    let args: Vec<MapExpr> = collect_fn_call_args(node, ctx);
    MapExpr::FnCall {
        name: name_sid,
        args,
    }
}

/// Reconstruct a `::`-separated path text from the atom's source
/// slice. Walks the leading bytes, accumulating identifier segments
/// joined by `::`, until the run terminates (whitespace, `(`, end
/// of slice, or any non-identifier non-`::` byte). Used for both
/// path-only atoms and the path prefix of function-call atoms.
pub(super) fn recover_call_path(trimmed: &str) -> String {
    let mut out = String::new();
    let mut i = 0;
    let bytes = trimmed.as_bytes();
    loop {
        // Identifier segment.
        let start = i;
        while let Some(b) = bytes.get(i) {
            if b.is_ascii_alphanumeric() || *b == b'_' {
                i += 1;
            } else {
                break;
            }
        }
        if i == start {
            break;
        }
        if !out.is_empty() {
            out.push_str("::");
        }
        out.push_str(&trimmed[start..i]);
        // `::` separator?
        if bytes.get(i).copied() == Some(b':') && bytes.get(i + 1).copied() == Some(b':') {
            i += 2;
            continue;
        }
        break;
    }
    out
}

/// Collect a function call's argument expressions. The atom
/// compound's children are a sequence of children compounds; the
/// arg list is the trailing structural sub-tree whose own children
/// include `value_expr` compounds.
///
/// Under struct-direct projection the atom body's compound shape is
/// determined by the codegen — the arg list lives as a structural
/// sub-tree of the fn-call compound. Walk all descendant
/// `value_expr` compounds rooted strictly below the atom (skipping
/// nested fn-call / closure boundaries).
fn collect_fn_call_args<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> Vec<MapExpr> {
    let mut args: Vec<MapExpr> = Vec::new();
    collect_value_expr_args_rec(node, &mut args, ctx, /* skip_self */ true);
    args
}

/// Recursive walker collecting top-level `value_expr` compound
/// arguments under `node`, stopping at nested fn-call / closure /
/// paren-atom boundaries (those would belong to inner expressions).
fn collect_value_expr_args_rec<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    args: &mut Vec<MapExpr>,
    ctx: &mut LowerCtx<'p>,
    skip_self: bool,
) {
    if !skip_self {
        if node.compound_kind() == Some(BbnfCompoundKind::ValueExpr) {
            args.push(dispatch_value_expr(node, ctx));
            return;
        }
    }
    if node.kind() != BbnfKind::Compound {
        return;
    }
    for child in RuntimeView::children(&node) {
        let kind = child.compound_kind();
        match kind {
            Some(BbnfCompoundKind::ValueExpr) => {
                args.push(dispatch_value_expr(child, ctx));
            }
            Some(BbnfCompoundKind::ValueClosure)
            | Some(BbnfCompoundKind::ValueFnCall)
            | Some(BbnfCompoundKind::ValueAtom) => {
                // Don't descend into nested expression boundaries.
            }
            _ => {
                collect_value_expr_args_rec(child, args, ctx, /* skip_self */ false);
            }
        }
    }
}

/// Lower a path atom: `ident::ident::...` with no trailing `(`.
/// Mirrors the legacy semantics — single-segment paths fall back to
/// bare-ident resolution; multi-segment paths become a function
/// call on `MapExpr::Input`.
pub(super) fn lower_path_atom<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    trimmed: &'p str,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    // The path is fully recoverable from the source slice; the
    // structural-mode tape doesn't push per-segment records.
    let _ = node;
    let path = recover_call_path(trimmed);
    if !path.contains("::") {
        return lower_bare_ident(&path, ctx);
    }
    if let Some(bound) = lookup_value_env(&path, &ctx.value_env) {
        return bound;
    }
    let sid = ctx.strings.intern(&path);
    MapExpr::FnCall {
        name: sid,
        args: vec![MapExpr::Input],
    }
}

// ─── Bare-ident resolution ───────────────────────────────────────────────────

/// Resolve a bare identifier in a value-expression context: closure
/// param bindings shadow ambient names; otherwise the identifier
/// becomes a one-arg function call on `MapExpr::Input` (the legacy
/// semantics — bare names are treated as transformer functions).
pub(super) fn lower_bare_ident<'p>(name: &str, ctx: &mut LowerCtx<'p>) -> MapExpr {
    if let Some(bound) = lookup_value_env(name, &ctx.value_env) {
        return bound;
    }
    let sid = ctx.strings.intern(name);
    MapExpr::FnCall {
        name: sid,
        args: vec![MapExpr::Input],
    }
}

// ─── String literal ──────────────────────────────────────────────────────────

pub(super) fn lower_string_lit<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let text = match node.focus() {
        BbnfValue::Span(s) => s,
        _ => node.span_text_opt().unwrap_or(""),
    };
    MapExpr::StringLit(intern_string_lit_inner(text, ctx))
}

/// Strip the surrounding quote characters from a string-literal
/// span and intern the inner text. The grammar permits `"..."` and
/// `'...'`; the same delimiter byte appears at both ends.
fn intern_string_lit_inner<'p>(text: &str, ctx: &mut LowerCtx<'p>) -> bbnf_ir::StringId {
    let inner = if text.len() >= 2 {
        &text[1..text.len() - 1]
    } else {
        text
    };
    ctx.strings.intern(inner)
}
