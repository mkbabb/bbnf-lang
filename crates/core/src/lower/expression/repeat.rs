//! Factor + quantifier-modifier lowering.
//!
//! `factor = big_comment? term ?w modifier? big_comment?`
//!
//! Children occupy fixed positional slots; under struct-direct
//! emission, comment / modifier slots project to typed leaves
//! (Span for comments, Span for modifiers). The wrapper's SPAN
//! disambiguates: the modifier slot's trimmed span is exactly one
//! of `?w` / `?` / `*` / `+`; the term slot's span is the term
//! expression text.

use bbnf_ir::IrNode;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

use super::super::LowerCtx;
use super::super::view_walk::find_sibling_by_kind;
use super::lower_term;

/// Lower a `factor = big_comment? term ?w modifier? big_comment?` view.
///
/// Children occupy fixed positional slots `[big_comment?, term-wrapper,
/// modifier-wrapper, big_comment?]`. Dispatch by role from the
/// direct-child sequence:
///
/// 1. Walk direct children once, classifying each by its trimmed
///    SPAN text:
///    - empty trimmed span → placeholder, skip
///    - `compound_kind ∈ {BigComment, Comment-like}` → metadata, skip
///    - trimmed span ∈ `{?w, ?, *, +}` → modifier
///    - otherwise → first such child is the term wrapper
/// 2. Fall back to `find_sibling_by_kind(Term)` if the classifier
///    produced no term candidate.
/// 3. Apply the modifier's quantifier to the base term.
pub(crate) fn lower_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let mut term_node: Option<BbnfView<'a, 'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut has_unit_marker = false;
    for child in node.children() {
        // Detect the Unit leaf the codegen modifier emitter pushes
        // (`push_leaf_with_unit`). It carries no source span, so
        // `span_text().trim()` is empty and the classification path
        // below would skip it; we record its presence as a signal
        // that the modifier rule fired and a source-gap scan should
        // recover the punctuator.
        if matches!(child.kind(), BbnfKind::Unit) {
            has_unit_marker = true;
            continue;
        }
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        // Comment leaves carry their own span text (the comment
        // slug). Filter them out by leading marker — `//` for line
        // comments and `/*` for block comments.
        if is_comment_span(trimmed) {
            continue;
        }
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            modifier_text = Some(trimmed.to_string());
            continue;
        }
        if term_node.is_none() {
            term_node = Some(child);
        }
    }
    let term = term_node
        .or_else(|| find_sibling_by_kind(node, BbnfCompoundKind::Term))
        .or_else(|| find_term_child_by_elimination(node))
        .unwrap_or_else(|| panic!("factor: missing term child in span {:?}", node.span_text(),));
    let base = lower_term(term, ctx);

    // Source-gap modifier recovery (mirror of pratt::recover_binary_op):
    // the codegen alt_dispatch path emits the `modifier?` slot as a
    // `Unit` leaf via `push_leaf_with_unit` when the modifier rule
    // fires, so the children classification above sees the Unit
    // child but its span is empty (`Unit` carries no source position
    // — `compute_byte_span` returns `None` for it, and the parent
    // factor's span ends at the term's last source-bearing
    // descendant). The Unit's existence flags that the modifier
    // matched at parse time; scan the input forward from the term's
    // end (skipping whitespace) for the punctuator.
    //
    // Multi-character modifiers (`?w`) are tested before single-char
    // (`?`) so `?` doesn't shadow `?w`.
    if modifier_text.is_none() && has_unit_marker {
        if let Some(text) = recover_modifier(term) {
            modifier_text = Some(text.to_string());
        }
    }

    if let Some(text) = modifier_text {
        return apply_modifier(base, &text);
    }
    base
}

/// Recover a quantifier modifier (`?w` / `?` / `*` / `+`) by scanning
/// the source slice forward from the term's end.
///
/// The codegen alt_dispatch shape emits the optional `modifier` child
/// as a `Unit` leaf when the modifier rule fires. `Unit` carries no
/// source position (`compute_byte_span` returns `None`), so the
/// factor's own `byte_span()` ends at the rightmost source-bearing
/// descendant — typically the term's last leaf, NOT past the
/// modifier punctuator. Furthermore, when the term is a grouped
/// form (`( rhs )` / `[ rhs ]` / `{ rhs }`) the codegen also pushes
/// the closing delimiter as a non-span consumption, so the term's
/// own `byte_span()` ends at the inner expression's last leaf —
/// inside the parens, not at the closing `)`. Scan the input from
/// that anchor forward, skipping whitespace and any close-delimiter
/// bytes the codegen consumed without span-pushing, then read the
/// modifier punctuator directly.
///
/// Caller guards: only invoked when [`lower_factor`] sees a `Unit`
/// child marker among the factor's children (which signals the
/// codegen modifier emitter ran). Without that signal, scanning
/// past the term would falsely consume an unrelated trailing token.
///
/// Two-character `?w` is tested before single-char `?` so the
/// shorter form doesn't shadow the longer one.
pub(super) fn recover_modifier<'a>(term: BbnfView<'a, 'a>) -> Option<&'static str> {
    let (_, term_hi) = term.byte_span()?;
    let input = term.input();
    let mut tail = input.get(term_hi as usize..)?;
    // Skip whitespace and group-closing delimiters (`)` / `]` / `}`)
    // that the codegen Term-shape emitter consumes without pushing
    // a Span. The grammar admits only `?w`/`?`/`*`/`+` at the
    // modifier slot, so the first byte after these skipped bytes
    // is unambiguously the modifier punctuator.
    loop {
        let bytes = tail.as_bytes();
        let Some(&b) = bytes.first() else {
            return None;
        };
        match b {
            b' ' | b'\t' | b'\r' | b'\n' | b')' | b']' | b'}' => {
                tail = &tail[1..];
            }
            _ => break,
        }
    }
    // `?w` before `?` so the single-char form doesn't shadow the
    // two-char form. `*` and `+` are unambiguous.
    if tail.starts_with("?w") {
        return Some("?w");
    }
    if tail.starts_with('?') {
        return Some("?");
    }
    if tail.starts_with('*') {
        return Some("*");
    }
    if tail.starts_with('+') {
        return Some("+");
    }
    None
}

/// Whether a trimmed span is recognisably a comment leaf. Covers
/// the bbnf `comment` (`//...`) and `big_comment` (`/* ... */`)
/// rules that project to Span leaves under struct-direct.
fn is_comment_span(trimmed: &str) -> bool {
    trimmed.starts_with("//") || trimmed.starts_with("/*")
}

/// Locate the term child of a `factor` compound by eliminating the
/// known metadata / placeholder children.
///
/// The factor body is `big_comment? term ?w modifier? big_comment?`,
/// so any child whose compound kind is not a comment/modifier slot
/// and whose span is non-empty carries the term.
fn find_term_child_by_elimination<'a>(node: BbnfView<'a, 'a>) -> Option<BbnfView<'a, 'a>> {
    for child in node.children() {
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        if is_comment_span(trimmed) {
            continue;
        }
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            continue;
        }
        return Some(child);
    }
    None
}

/// Apply a quantifier modifier (`?` / `*` / `+` / `?w`) to a base
/// expression, producing the wrapping `Repeat` / `OptionalWhitespace`
/// IR node.
pub(super) fn apply_modifier(base: IrNode, text: &str) -> IrNode {
    match text {
        "?" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: 1,
        },
        "*" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: u32::MAX,
        },
        "+" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 1,
            hi: u32::MAX,
        },
        "?w" => IrNode::OptionalWhitespace(Box::new(base)),
        _ => base,
    }
}
