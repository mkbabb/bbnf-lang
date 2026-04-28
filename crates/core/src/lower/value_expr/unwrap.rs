//! Value-expression unwrap helpers — peel chain wrappers down to
//! the innermost identifier or atom view.
//!
//! Exposed as `pub(crate)` for the expression lowering layer
//! (`lower/expression.rs`), which uses them for type-shorthand and
//! `@host` return-type recovery on `-> T` map arrows.

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfValue, BbnfView};

use super::atom::{recover_call_path, same_focus, scan_ident_len};
use super::precedence::collect_chain_operands;
use super::simple_kinds::value_expr_head;
use super::view_walk::find_descendant_by_compound_kind;

// ─── Value-expression helpers exported to expression.rs ──────────────────────

/// Extract a `&str` from a value expression view that's a bare
/// identifier or a single-segment path. Used by
/// `lower/expression.rs::lower_map_arrow` to detect the type-
/// shorthand `-> f64` form (where the value expression is just a
/// type name).
///
/// Recursively unwraps precedence-chain wrappers (`value_or` /
/// `value_and` / `value_cmp` / `value_add` / `value_mul` /
/// `value_unary` / `value_atom`) when each wrapper has only one
/// child compound (i.e. no operators in the chain). Returns the
/// raw identifier byte slice without copying.
pub(crate) fn unwrap_value_ident_str<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
) -> Option<&'p str> {
    let mut cur = node;
    loop {
        // Leaf payload — a `Span` carries the identifier slice
        // directly; other typed leaves can't yield an ident-string
        // without their span.
        match cur.focus() {
            BbnfValue::Span(s) => {
                return Some(s);
            }
            BbnfValue::Int(_) | BbnfValue::Float(_) | BbnfValue::Bool(_)
            | BbnfValue::Tag(_) | BbnfValue::Unit => {
                return None;
            }
            BbnfValue::Compound(_) => {}
        }

        match cur.compound_kind() {
            // value_path is identifier-shaped iff it's a single
            // segment (no `::`).
            Some(BbnfCompoundKind::ValuePath) => {
                let text = cur.span_text_opt()?.trim();
                return if text.contains("::") { None } else { Some(text) };
            }
            // Top-level value_expr wrapper — peel into the inner
            // head.
            Some(BbnfCompoundKind::ValueExpr) => {
                cur = value_expr_head(cur)?;
            }
            // Precedence-chain wrappers: descend through the
            // first-and-only operand if there are no operators.
            Some(BbnfCompoundKind::ValueOr)
            | Some(BbnfCompoundKind::ValueAnd)
            | Some(BbnfCompoundKind::ValueCmp)
            | Some(BbnfCompoundKind::ValueAdd)
            | Some(BbnfCompoundKind::ValueMul) => {
                // Single-operand chain → text == operand text.
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return None;
                }
                cur = operands.into_iter().next().unwrap();
            }
            Some(BbnfCompoundKind::ValueUnary) => {
                // Bare unary (no `!`/`-`) — descend into the atom.
                let text = cur.span_text_opt()?;
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return None;
                }
                cur = find_descendant_by_compound_kind(cur, BbnfCompoundKind::ValueAtom)
                    .filter(|v| !same_focus(*v, cur))
                    .or_else(|| RuntimeView::children(&cur).next())?;
            }
            Some(BbnfCompoundKind::ValueAtom) => {
                // Atom is identifier-shaped iff its leading non-ws
                // byte is `_`/alpha and the contiguous identifier
                // run equals the trimmed text length.
                let text = cur.span_text_opt()?;
                let trimmed = text.trim();
                let first = trimmed.as_bytes().first().copied()?;
                if first != b'_' && !(first as char).is_ascii_alphabetic() {
                    return None;
                }
                let head = scan_ident_len(trimmed);
                if head == trimmed.len() {
                    return Some(&trimmed[..head]);
                } else {
                    return None;
                }
            }
            // Anonymous / Other compounds may surface when the
            // optimizer fully inlined the value-layer wrappers and
            // the atom's span IS the identifier-shaped text. Try
            // the same atom-style classification.
            Some(BbnfCompoundKind::Other) => {
                let text = cur.span_text_opt()?;
                let trimmed = text.trim();
                let first = trimmed.as_bytes().first().copied()?;
                if first.is_ascii_digit() || first == b'.' {
                    return None;
                }
                if first != b'_' && !(first as char).is_ascii_alphabetic() {
                    return None;
                }
                let head = scan_ident_len(trimmed);
                if head == trimmed.len() {
                    return Some(&trimmed[..head]);
                } else {
                    return None;
                }
            }
            _ => return None,
        }
        // Termination check — stop on any non-compound focus that
        // didn't already short-circuit above.
        if cur.kind() != BbnfKind::Compound {
            // Fall back to the span path for leaf focuses we
            // didn't explicitly handle.
            match cur.focus() {
                BbnfValue::Span(s) => return Some(s),
                _ => return None,
            }
        }
    }
}

/// Recursively unwrap value-expression wrappers down to the
/// innermost atom view. Used by `lower_map_arrow` to extract the
/// leaf node for type-suffix and bool-literal detection.
pub(crate) fn deep_unwrap_value<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
) -> BbnfView<'a, 'p> {
    let mut cur = node;
    loop {
        match cur.compound_kind() {
            // Top-level value_expr wrapper — peel into the inner
            // head.
            Some(BbnfCompoundKind::ValueExpr) => {
                if let Some(head) = value_expr_head(cur) {
                    cur = head;
                } else {
                    return cur;
                }
            }
            Some(BbnfCompoundKind::ValueOr)
            | Some(BbnfCompoundKind::ValueAnd)
            | Some(BbnfCompoundKind::ValueCmp)
            | Some(BbnfCompoundKind::ValueAdd)
            | Some(BbnfCompoundKind::ValueMul) => {
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return cur;
                }
                cur = operands.into_iter().next().unwrap();
            }
            Some(BbnfCompoundKind::ValueUnary) => {
                let text = cur.span_text_opt().unwrap_or("");
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return cur;
                }
                if let Some(atom) = find_descendant_by_compound_kind(cur, BbnfCompoundKind::ValueAtom)
                    .filter(|v| !same_focus(*v, cur))
                    .or_else(|| RuntimeView::children(&cur).next())
                {
                    cur = atom;
                } else {
                    return cur;
                }
            }
            Some(BbnfCompoundKind::ValueAtom) => {
                // Atoms are leaf-shaped — return as-is. Callers
                // (`lower_map_arrow`'s suffix / bool detection)
                // inspect the span text directly.
                return cur;
            }
            _ => return cur,
        }
    }
}

/// Extract a function name from a value expression that's a bare
/// function call (e.g. `myfunc(input)`) or a callable identifier.
/// Used by `lower_map_arrow` for `@host` return-type propagation —
/// the recovered name is looked up in the host-fn table.
///
/// Identifier-shaped atoms reach this helper because the structural-
/// mode tape inlines `value_ident` / `value_path` / `value_fn_call`
/// into `value_atom`. We classify by inspecting the atom span text:
/// any leading identifier byte yields a candidate name (path
/// segments included). The downstream lookup tolerates misses, so
/// returning a name eagerly is correct — non-host names simply
/// fail the host-table check.
pub(crate) fn extract_value_func_name<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
) -> Option<String> {
    // Leaf-payload Span identifiers map to themselves.
    if let BbnfValue::Span(s) = node.focus() {
        return Some(s.to_string());
    }
    match node.compound_kind() {
        Some(BbnfCompoundKind::ValuePath) => {
            Some(recover_call_path(node.span_text_opt()?.trim_start()))
        }
        Some(BbnfCompoundKind::ValueFnCall) => {
            Some(recover_call_path(node.span_text_opt()?.trim_start()))
        }
        Some(BbnfCompoundKind::ValueAtom) => {
            let trimmed = node.span_text_opt()?.trim_start();
            let first = *trimmed.as_bytes().first()?;
            if !(first.is_ascii_alphabetic() || first == b'_') {
                return None;
            }
            Some(recover_call_path(trimmed))
        }
        _ => None,
    }
}

/// Whitelist of builtin Rust type names recognised by the type-
/// shorthand `-> T` form.
///
/// `"Span"` admits the grammar-level `-> Span` shorthand so token rules
/// (`identifier`, `string_lit`, `regex`, `big_comment`, `comment`) route
/// through the backend-agnostic span-payload path rather than degrading
/// to `TypeDesc::Named("Span")`.
pub(crate) fn is_type_name(name: &str) -> bool {
    matches!(
        name,
        "Span"
            | "f64"
            | "f32"
            | "u32"
            | "u64"
            | "i32"
            | "i64"
            | "usize"
            | "u8"
            | "u16"
            | "i8"
            | "i16"
    )
}
