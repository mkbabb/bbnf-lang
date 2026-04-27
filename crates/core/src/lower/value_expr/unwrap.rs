//! Value-expression unwrap helpers — peel chain wrappers down to
//! the innermost identifier or atom view.
//!
//! Exposed as `pub(crate)` for the expression lowering layer
//! (`lower/expression.rs`), which uses them for type-shorthand and
//! `@host` return-type recovery on `-> T` map arrows.

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::lower::tape_walk::find_descendant_by_kind;

use super::atom::{recover_call_path, scan_ident_len};
use super::precedence::collect_chain_operands;
use super::simple_kinds::value_expr_head;

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
pub(crate) fn unwrap_value_ident_str<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<&'a str> {
    use crate::runtime::tape::TapeKind;
    let mut cur = node;
    loop {
        match cur.rule_kind() {
            BbnfBootstrapRuleKind::value_ident
            | BbnfBootstrapRuleKind::identifier => {
                return Some(cur.span_text());
            }
            BbnfBootstrapRuleKind::value_path => {
                let text = cur.span_text().trim();
                return if text.contains("::") { None } else { Some(text) };
            }
            // Top-level value_expr wrapper — peel into the inner
            // head. Under DTA, `children().next()` picks the
            // anonymous Seq wrapper rather than the semantic head;
            // descend to the first value-layer rule compound.
            BbnfBootstrapRuleKind::value_expr => {
                cur = value_expr_head(cur)?;
            }
            // Precedence-chain wrappers: descend through the
            // first-and-only operand if there are no operators.
            BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul => {
                // Single-operand chain → text == operand text. Use
                // `collect_chain_operands` to detect. The operand
                // collection is already DTA-aware (W4.2 migration)
                // so this site inherits the descent through any
                // anonymous wrappers transparently.
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return None;
                }
                cur = operands.into_iter().next().unwrap();
            }
            BbnfBootstrapRuleKind::value_unary => {
                // Bare unary (no `!`/`-`) — descend into the atom.
                // Under DTA the atom sits one Seq deeper; descend to
                // the value_atom descendant rather than picking the
                // anonymous wrapper via `children().next()`.
                let text = cur.span_text();
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return None;
                }
                cur = find_descendant_by_kind(cur, BbnfBootstrapRuleKind::value_atom)
                    .filter(|v| v.cursor().offset() != cur.cursor().offset())
                    .or_else(|| cur.children().next())?;
            }
            BbnfBootstrapRuleKind::value_atom => {
                // Atom is identifier-shaped iff its leading non-ws
                // byte is `_`/alpha and the contiguous identifier
                // run equals the trimmed text length.
                let text = cur.span_text();
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
            // Under DTA the walker surfaces `int_lit` / `Unknown` as
            // the sentinel rule_kind for compounds emitted without a
            // `DtaState::Ref`. In the value-expression chain, these
            // appear when the optimizer inlined the `value_unary` +
            // `value_atom` layers entirely — the sentinel compound's
            // span text IS the atom's text (for a type-name / bare
            // ident: `"i64"`, `"Span"`). Classify it as an atom when
            // the span is identifier-shaped.
            BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
                let text = cur.span_text();
                let trimmed = text.trim();
                let first = trimmed.as_bytes().first().copied()?;
                // If the span starts with a digit or `.`, this is a
                // real int_lit / numeric — not an identifier.
                if first.is_ascii_digit() || first == b'.' {
                    return None;
                }
                // Treat as value_atom body: identifier-shaped span.
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
        // Termination is guaranteed because every iteration either
        // returns or descends through `children().next()` /
        // `collect_chain_operands`, both of which strictly shrink
        // the visited subtree. Defensive guard: stop on any
        // non-compound tape kind (every value-expression rule_kind
        // we care about emits a Rule compound).
        if !matches!(cur.kind(), TapeKind::Rule | TapeKind::Repeat | TapeKind::Seq | TapeKind::Alt) {
            return None;
        }
    }
}

/// Recursively unwrap value-expression wrappers down to the
/// innermost atom view. Used by `lower_map_arrow` to extract the
/// leaf node for type-suffix and bool-literal detection.
pub(crate) fn deep_unwrap_value<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    let mut cur = node;
    loop {
        match cur.rule_kind() {
            // Top-level value_expr wrapper — peel into the inner
            // head. Under DTA, `children().next()` picks the
            // anonymous Seq wrapper rather than the semantic head;
            // use `value_expr_head` to descend to the first
            // value-layer rule.
            BbnfBootstrapRuleKind::value_expr => {
                if let Some(head) = value_expr_head(cur) {
                    cur = head;
                } else {
                    return cur;
                }
            }
            BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul => {
                let operands = collect_chain_operands(cur);
                if operands.len() != 1 {
                    return cur;
                }
                cur = operands.into_iter().next().unwrap();
            }
            BbnfBootstrapRuleKind::value_unary => {
                let text = cur.span_text();
                let first = text.as_bytes().first().copied();
                if first == Some(b'!') || first == Some(b'-') {
                    return cur;
                }
                // Under DTA the atom sits one Seq deeper; descend to
                // the value_atom descendant rather than picking the
                // anonymous wrapper via `children().next()`.
                if let Some(atom) =
                    find_descendant_by_kind(cur, BbnfBootstrapRuleKind::value_atom)
                        .filter(|v| v.cursor().offset() != cur.cursor().offset())
                        .or_else(|| cur.children().next())
                {
                    cur = atom;
                } else {
                    return cur;
                }
            }
            BbnfBootstrapRuleKind::value_atom => {
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
pub(crate) fn extract_value_func_name<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<String> {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::value_ident | BbnfBootstrapRuleKind::identifier => {
            Some(node.span_text().to_string())
        }
        BbnfBootstrapRuleKind::value_path => {
            Some(recover_call_path(node.span_text().trim_start()))
        }
        BbnfBootstrapRuleKind::value_fn_call => {
            Some(recover_call_path(node.span_text().trim_start()))
        }
        BbnfBootstrapRuleKind::value_atom => {
            let trimmed = node.span_text().trim_start();
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

