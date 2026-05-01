//! Alias detection for AST-level diagnostics.
//!
//! AZ-II.cutover.D — rewritten against the struct-direct
//! [`BbnfView`] surface. Grouped expressions and plain-reference
//! shapes are distinguished by the `term` compound's leading-byte
//! signature (the `(rhs)` / `[rhs]` / `{rhs}` / `@{rhs}` and bare
//! `identifier (call_args)?` branches share a single
//! `BbnfCompoundKind::Term` discriminator under struct-direct, so
//! their disambiguation runs off the focused compound's span text
//! and child structure rather than a sub-variant tag).
//!
//! Every dispatch here is on the compound focus's
//! [`BbnfCompoundKind`]; leaf focuses route through `view.kind()`.

use std::collections::{HashMap, HashSet};

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};
use crate::types::AST;

/// Find rules whose RHS is simply a reference to another nonterminal.
pub fn find_aliases<'a>(
    ast: &'a AST<'a>,
    cyclic_rules: &HashSet<&'a str>,
) -> HashMap<&'a str, &'a str> {
    let mut aliases = HashMap::new();

    for (&name, entry) in ast {
        if cyclic_rules.contains(name) {
            continue;
        }

        if let Some(target) = extract_alias_target(entry.rhs) {
            if ast.contains_key(target) {
                aliases.insert(name, target);
            }
        }
    }

    aliases
}

/// Extract the target nonterminal name if the expression is a simple
/// alias (possibly grouped).
fn extract_alias_target<'a>(view: BbnfView<'a, 'a>) -> Option<&'a str> {
    match view.compound_kind() {
        // Leaf focus — direct identifier reference if the leaf is a
        // Span whose content matches the identifier shape.
        None => match view.kind() {
            BbnfKind::Span => {
                let text = view.span_text().trim();
                if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                    Some(text_in_input(view.input(), text))
                } else {
                    None
                }
            }
            _ => None,
        },

        // Transparent wrappers — descend into the single inner child.
        Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs)
        | Some(BbnfCompoundKind::Rhs) => view.child(0).and_then(extract_alias_target),

        // `term = ε | identifier (call_args)? | literal | regex
        //       | "(" rhs ")" | "[" rhs "]" | "{" rhs "}" | "@{" rhs "}"`.
        //
        // Struct-direct collapses every Alt branch to a single
        // `BbnfCompoundKind::Term` discriminator — disambiguate via
        // the compound's leading source byte:
        //
        // - `(`  → `(rhs)` grouped form preserves alias semantics;
        //   descend into the inner Rhs compound child.
        // - `[`/`{`/`@` → optional / repetition / host-call; not an
        //   alias.
        // - otherwise → bare `identifier (call_args)?` form. Alias
        //   only when the compound carries no `CallArg` child.
        Some(BbnfCompoundKind::Term) => {
            let leading = view.span_text().as_bytes().first().copied();
            if leading == Some(b'(') {
                let inner = view.find_descendant_by_kind(BbnfCompoundKind::Rhs)?;
                return extract_alias_target(inner);
            }
            if matches!(leading, Some(b'[') | Some(b'{') | Some(b'@')) {
                return None;
            }
            // Bare term: the compound holds an identifier Span child
            // and (optionally) a CallArg compound child. Alias only
            // when the CallArg slot is absent.
            let has_call_args = view
                .find_descendant_by_kind(BbnfCompoundKind::CallArg)
                .is_some();
            if has_call_args {
                return None;
            }
            // Find the identifier Span — first Span descendant whose
            // text is a bare identifier.
            find_first_identifier_text(view)
        }

        // factor = (comment_before?, term, modifier?, comment_after?)
        // — alias only when the modifier slot is absent. Modifier
        // detection runs off the compound's span text: if the trimmed
        // span ends in `?` / `*` / `+` then the modifier slot is
        // populated.
        Some(BbnfCompoundKind::Factor) => {
            let span = view.span_text().trim_end();
            if matches!(span.as_bytes().last(), Some(b'?') | Some(b'*') | Some(b'+')) {
                return None;
            }
            // Locate the inner `term` compound and recurse. If absent,
            // fall back to a span-text identifier check.
            if let Some(term) = view.find_descendant_by_kind(BbnfCompoundKind::Term) {
                extract_alias_target(term)
            } else {
                let text = view.span_text().trim();
                if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                    Some(text_in_input(view.input(), text))
                } else {
                    None
                }
            }
        }

        // mapped_factor = (factor, ("->" value_expr type_annotation?)?)
        // — alias only when the mapping slot is absent. Detect the
        // mapping by scanning the compound's span text for `->`; the
        // mapping is the only way `->` reaches the source slice.
        Some(BbnfCompoundKind::MappedFactor) => {
            if view.span_text().contains("->") {
                return None;
            }
            if let Some(inner) = view
                .find_descendant_by_kind(BbnfCompoundKind::Factor)
                .or_else(|| view.find_descendant_by_kind(BbnfCompoundKind::Term))
            {
                return extract_alias_target(inner);
            }
            let text = view.span_text().trim();
            if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
                Some(text_in_input(view.input(), text))
            } else {
                None
            }
        }

        // Single-branch alternation / single-element concatenation /
        // single-operand binary factor / single-arg call_arg —
        // descend transparently when there's exactly one substantive
        // child, else alias chain breaks.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            let mut iter = view.children_iter();
            let first = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            extract_alias_target(first)
        }
        Some(BbnfCompoundKind::Concatenation) => {
            let mut iter = view.children_iter();
            let first = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            extract_alias_target(first)
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            let mut iter = view.children_iter();
            let first = iter.next()?;
            if iter.next().is_some() {
                return None;
            }
            extract_alias_target(first)
        }

        // Closures and remaining compound shapes — no alias semantics.
        Some(_) => None,
    }
}

/// Locate the first descendant Span leaf whose content reads as a
/// bare identifier and return its text re-borrowed against the
/// input slice.
fn find_first_identifier_text<'a>(view: BbnfView<'a, 'a>) -> Option<&'a str> {
    if view.kind() == BbnfKind::Span {
        let text = view.span_text().trim();
        if !text.is_empty() && super::deps::is_ident(text.as_bytes()) {
            return Some(text_in_input(view.input(), text));
        }
        return None;
    }
    for child in view.children_iter() {
        if let Some(t) = find_first_identifier_text(child) {
            return Some(t);
        }
    }
    None
}

/// Re-borrow `text` from `input` so the returned slice has the
/// `'a` lifetime of the input. Used for span-text projections that
/// the underlying view returns with its document-bound lifetime.
fn text_in_input<'a>(input: &'a str, text: &str) -> &'a str {
    let input_start = input.as_ptr() as usize;
    let input_end = input_start + input.len();
    let s_start = text.as_ptr() as usize;
    let s_end = s_start + text.len();
    if s_start < input_start || s_end > input_end {
        if let Some(pos) = input.find(text) {
            return &input[pos..pos + text.len()];
        }
        return &input[..0];
    }
    let lo = s_start - input_start;
    let hi = lo + text.len();
    &input[lo..hi]
}
