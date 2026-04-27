//! Atom-layer lowering: unary prefixes, parenthesised expressions,
//! identifier-shaped atoms, function calls, paths, and string literals.

use bbnf_ir::{MapExpr, MapUnaryOp};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::find_descendant_by_kind;

use super::super::LowerCtx;
use super::dispatch_value_expr;
use super::literals::{parse_float_literal, parse_numeric_literal_text};
use super::precedence::descend_anonymous_wrappers;
use super::simple_kinds::lookup_value_env;

// ─── Unary ───────────────────────────────────────────────────────────────────

/// Lower a `value_unary` compound. Body is `( "!" | "-" )? value_atom`.
/// The prefix consumes bytes without pushing, so we identify it by
/// inspecting the unary compound's span text. If the leading byte
/// is `!` or `-`, wrap the atom in a unary op; otherwise the body
/// is the bare atom.
pub(super) fn lower_value_unary<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
    let first_byte = text.as_bytes().first().copied();
    match first_byte {
        Some(b'!') | Some(b'-') => {
            let atom = first_atom_child(node).unwrap_or(node);
            // Avoid an infinite recursion if `first_atom_child`
            // returned the unary compound itself (defensive — the
            // grammar guarantees a value_atom child).
            if atom.cursor().offset() == node.cursor().offset() {
                panic!(
                    "lower/value_expr.rs: lower_value_unary saw a unary \
                     compound with no atom child (text = {:?})",
                    text,
                );
            }
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
            if atom.cursor().offset() == node.cursor().offset() {
                // No child compound at all — fall back to atom
                // dispatch on `node` itself (which classifies the
                // span text).
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
///
/// Under DTA the atom sits inside an anonymous Seq wrapper emitted
/// by the walker for the unary's body; `node.children().next()` may
/// return that wrapper rather than the `value_atom` compound itself.
/// Descend to the first `value_atom` descendant to resolve under
/// both DTA and fn-per-rule shapes uniformly.
fn first_atom_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_atom)
        .filter(|v| v.cursor().offset() != node.cursor().offset())
        .or_else(|| node.children().next())
}

// ─── Atom classification ─────────────────────────────────────────────────────

/// Lower a `value_atom` compound. Under structural mode the atom
/// rule has its leaf alts (int/float/bool/string/ident/path/input/
/// fn_call/parenthesised) inlined into a single function with no
/// per-alt sub-rule pushes — only the inner Repeat compounds (path
/// segments, input prop chain, function-call arg list) and any
/// recursive `value_expr` compound (for `(expr)` and fn-call args)
/// reach the tape.
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
pub(super) fn lower_value_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let text = node.span_text();
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
            "lower/value_expr.rs: lower_value_atom saw an unexpected \
             leading byte in atom span {:?} (rule_kind = {:?})",
            text,
            node.rule_kind(),
        ),
    }
}

/// Lower a parenthesised atom: `( value_expr )`. The structural
/// shape pushes only the inner `value_expr` compound (the parens
/// consume bytes without pushing). The grammar guarantees exactly
/// one semantic child compound here.
///
/// Under DTA the atom's body is wrapped in an anonymous Seq
/// compound; `children().next()` may return that wrapper rather
/// than the `value_expr` record directly. Descend to the first
/// `value_expr` descendant; the outer `node.rule_kind()` is
/// `value_atom`, not `value_expr`, so the descent correctly returns
/// a distinct inner view.
fn lower_paren_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let inner = find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_expr)
        .or_else(|| node.children().next())
        .expect("lower_paren_atom: parenthesised atom is missing its value_expr child");
    dispatch_value_expr(inner, ctx)
}

/// Lower an atom whose leading text is identifier-shaped: bool
/// literal, `input` chain, function call, path, or bare ident.
/// Disambiguates from the source slice without depending on
/// per-leaf rule_kind pushes.
fn lower_atom_named<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
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
pub(super) fn lower_input_chain<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // The grammar produces `Repeat(.ident)*`. Recover the prop name
    // by scanning the source slice past the `input` keyword: each
    // `.ident` segment yields one prop name; we use the LAST prop
    // (matching the legacy semantics — only the leaf prop is
    // surfaced as a `MapExpr::InputProp`).
    //
    // Source-slice walk is cheaper than enumerating the Repeat's
    // children (which carry no useful payload — the inner ident
    // scan doesn't push) and equivalent in semantics.
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
pub(super) fn lower_fn_call_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    // Recover the full path from the source slice. The path is
    // `ident (:: ident)*`; everything up to the opening `(` belongs
    // to the path.
    let path_text = recover_call_path(trimmed);
    let name_sid = ctx.strings.intern(&path_text);

    // Walk the atom's children to find the arg list. The path's
    // `(::ident)*` Repeat contains no Rule pushes, so the only Rule
    // compounds we encounter are the arg `value_expr` rules.
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
/// compound's children are a sequence of Repeat compounds; the arg
/// list is the trailing Repeat whose own children include
/// `value_expr` compounds. We scan all Repeat children and gather
/// any nested compound whose kind is `Rule` (i.e., a `value_expr`
/// rule push) — that gives us each arg in source order.
///
/// Under DTA the atom compound's body sits inside an anonymous Seq
/// wrapper; `node.children()` returns `[Seq]` rather than the
/// expected Repeat siblings. Descend through anonymous wrappers
/// first, then apply the Repeat-scan logic on the true body.
/// Additionally — under DTA a `value_expr` push surfaces as a
/// compound whose `rule_kind == value_expr`, not just any
/// `TapeKind::Rule`. Gate on rule_kind to avoid mistaking a nested
/// anonymous Seq-Rule for an argument.
fn collect_fn_call_args<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> Vec<MapExpr> {
    use crate::runtime::tape::TapeKind;
    let mut args: Vec<MapExpr> = Vec::new();

    // Descend through the DTA Seq wrapper to reach the atom body
    // whose direct children are the path + arg-list Repeats.
    let body = descend_anonymous_wrappers(node);

    // Arg is any descendant `value_expr` compound — but we must
    // avoid collecting args from any nested parenthesised
    // sub-expression or closure body. The arg list lives inside
    // the trailing Repeat of the fn-call atom; iterate that
    // Repeat's direct children (not descendants) to avoid
    // cross-boundary collection.
    //
    // The atom may have multiple Repeat children — one for the
    // path's `(::ident)*` segment list and one for the optional
    // arg list. Walk every Repeat (direct children of the atom
    // body) and pull out any nested compound. The path Repeat
    // contains only inlined ident scans which push nothing; the
    // arg-list Repeat contains `value_expr` rule compounds.
    let is_value_expr = |v: &BbnfBootstrapNodeView<'a>| {
        v.rule_kind() == BbnfBootstrapRuleKind::value_expr
    };
    for child in body.children() {
        if child.kind() == TapeKind::Repeat {
            for inner in child.children() {
                match inner.kind() {
                    TapeKind::Rule => {
                        // Under DTA a Rule compound may be an
                        // anonymous Seq-wrapper around the real
                        // value_expr rather than the value_expr
                        // itself. Prefer the value_expr descendant
                        // when the direct child's rule_kind isn't
                        // already value_expr.
                        if is_value_expr(&inner) {
                            args.push(dispatch_value_expr(inner, ctx));
                        } else if let Some(ve) = find_descendant_by_kind(
                            inner,
                            BbnfBootstrapRuleKind::value_expr,
                        ) {
                            args.push(dispatch_value_expr(ve, ctx));
                        } else {
                            // Optimizer fully inlined — dispatch on
                            // the compound directly; the handler
                            // classifies by rule_kind.
                            args.push(dispatch_value_expr(inner, ctx));
                        }
                    }
                    TapeKind::Repeat => {
                        // The optional arg list's tail-rest Repeat
                        // (`(, value_expr)*`) — recurse one level.
                        for grand in inner.children() {
                            if grand.kind() == TapeKind::Rule {
                                if is_value_expr(&grand) {
                                    args.push(dispatch_value_expr(grand, ctx));
                                } else if let Some(ve) = find_descendant_by_kind(
                                    grand,
                                    BbnfBootstrapRuleKind::value_expr,
                                ) {
                                    args.push(dispatch_value_expr(ve, ctx));
                                } else {
                                    args.push(dispatch_value_expr(grand, ctx));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    args
}

/// Lower a path atom: `ident::ident::...` with no trailing `(`.
/// Mirrors the legacy semantics — single-segment paths fall back to
/// bare-ident resolution; multi-segment paths become a function
/// call on `MapExpr::Input`.
pub(super) fn lower_path_atom<'a>(
    node: BbnfBootstrapNodeView<'a>,
    trimmed: &'a str,
    ctx: &mut LowerCtx<'a>,
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
pub(super) fn lower_bare_ident<'a>(name: &str, ctx: &mut LowerCtx<'a>) -> MapExpr {
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

pub(super) fn lower_string_lit<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    MapExpr::StringLit(intern_string_lit_inner(node.span_text(), ctx))
}

/// Strip the surrounding quote characters from a string-literal
/// span and intern the inner text. The grammar permits `"..."` and
/// `'...'`; the same delimiter byte appears at both ends.
fn intern_string_lit_inner<'a>(text: &str, ctx: &mut LowerCtx<'a>) -> bbnf_ir::StringId {
    let inner = if text.len() >= 2 {
        &text[1..text.len() - 1]
    } else {
        text
    };
    ctx.strings.intern(inner)
}
