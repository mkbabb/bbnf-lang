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

/// Branch tags for the `term` rule's eight-branch alternation that
/// correspond to grouped forms. Their parsers consume opening and
/// closing delimiters without pushing Spans, so the term's recovered
/// `byte_span()` ends inside the delimiters and the modifier-recovery
/// scan must walk past inner-factor modifiers and any number of
/// nested close-delimiters before reading this factor's modifier.
///
/// - `5`: `@{ rhs }` — SpanCapture (close delim `}`)
/// - `6`: `( rhs )`  — Paren        (close delim `)`)
/// - `7`: `[ rhs ]`  — Optional     (close delim `]`)
/// - `8`: `{ rhs }`  — Many         (close delim `}`)
const GROUPED_TERM_BRANCH_TAGS: [u32; 4] = [5, 6, 7, 8];

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
///
/// **AZ-IV.W0.3 typed-materialization invariant**: if structural
/// detection observed a `Unit` modifier marker (the codegen
/// emitter's signal that the `modifier?` slot fired) but neither
/// span-text classification nor `recover_modifier` resolved a
/// punctuator, panic loudly rather than silently returning the
/// bare term. Per `feedback_typed-materialization-invariant`,
/// every modifier annotation in the grammar source must reach the
/// IR — silent drops corrupt every downstream rule body invisibly.
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
    // end for the punctuator, structurally distinguishing grouped
    // (`( … )` / `[ … ]` / `{ … }` / `@{…}`) from non-grouped terms.
    if modifier_text.is_none() && has_unit_marker {
        if let Some(text) = recover_modifier(term) {
            modifier_text = Some(text.to_string());
        }
    }

    // Typed-materialization invariant: a `Unit` marker without a
    // resolved modifier means the codegen emitter recorded a
    // matched modifier but lowering could not recover its
    // punctuator — that's a structural-detection failure, not a
    // graceful fallback. Surface it loudly so the offending shape
    // is fixed at the source rather than silently dropped from the
    // IR.
    if has_unit_marker && modifier_text.is_none() {
        panic!(
            "factor: Unit modifier marker present but no punctuator could be recovered \
             from term span {:?} (factor span = {:?}, term branch_tag = {:?}). \
             The codegen modifier emitter ran at parse time but the source-gap scan \
             produced no `?`/`?w`/`*`/`+` punctuator — typed-materialization invariant violated.",
            term.span_text(),
            node.span_text(),
            term.branch_tag(),
        );
    }

    if let Some(text) = modifier_text {
        return apply_modifier(base, &text);
    }
    base
}

/// Recover a quantifier modifier (`?w` / `?` / `*` / `+`) by scanning
/// the source slice forward from the term's end.
///
/// **AZ-IV.W0.3 structural recovery**. The codegen alt_dispatch
/// shape emits the optional `modifier` child as a `Unit` leaf via
/// `push_leaf_with_unit` when the modifier rule fires. `Unit`
/// carries no source position (`compute_byte_span` returns `None`),
/// so the factor's `byte_span()` ends at the term's last
/// source-bearing descendant — the modifier punctuator must be
/// recovered from the input slice past that anchor.
///
/// Two recovery shapes apply, dispatched on whether the term is a
/// grouped form (Paren / Optional / Many / SpanCapture, identified
/// by the `term` rule's branch_tag in [`GROUPED_TERM_BRANCH_TAGS`]):
///
/// **Non-grouped terms** (identifier / literal / regex / epsilon /
/// `term` branch_tags 0..=4 / non-Term sub-expression terms): the
/// term's `byte_span()` covers the entire token, so the FACTOR's
/// modifier — if any — is the first `?w` / `?` / `*` / `+` past
/// optional whitespace.
///
/// **Grouped terms** (`( … )`, `[ … ]`, `{ … }`, `@{ … }`): the
/// codegen consumes the closing delimiter without pushing a Span,
/// and any inner grouped term whose `byte_span()` hi coincides with
/// OUR term's hi has its closing delimiter still pending in the
/// source past `term_hi` (its parser hadn't reached the close at
/// the moment our rightmost span was pushed). Each such "still-open"
/// inner group contributes one close-delim that must be consumed
/// before OUR group's close-delim is reached. Walk the term's
/// descendant tree once to count `n_pending_inner_closes`, then
/// scan source forward consuming whitespace + modifier punctuators
/// + close-delims until exactly `(n_pending_inner_closes + 1)`
/// close-delims have been consumed. The next non-whitespace token
/// is OUR factor's modifier (if any).
///
/// Two-character `?w` is tested before single-char `?` so the
/// shorter form doesn't shadow the longer.
///
/// Caller guards: only invoked when [`lower_factor`] sees a `Unit`
/// child marker among the factor's children. Without that signal,
/// scanning past the term would falsely consume an unrelated
/// trailing token.
pub(super) fn recover_modifier<'a>(term: BbnfView<'a, 'a>) -> Option<&'static str> {
    let (_, term_hi) = term.byte_span()?;
    let input = term.input();
    let tail = input.get(term_hi as usize..)?;

    if is_grouped_term(term) {
        // OUR group contributes one close; each pending-still-open
        // inner grouped term contributes one more.
        let n_inner = count_pending_inner_groups(term, term_hi);
        recover_modifier_grouped(tail, n_inner + 1)
    } else {
        recover_modifier_non_grouped(tail)
    }
}

/// True iff `term` is a grouped term whose codegen parser consumed
/// closing-delimiter bytes without pushing Span leaves — i.e. the
/// term's `byte_span()` ends inside the closing delimiter rather
/// than after it.
fn is_grouped_term(term: BbnfView<'_, '_>) -> bool {
    if !term.is_compound_kind(BbnfCompoundKind::Term) {
        return false;
    }
    match term.branch_tag() {
        Some(tag) => GROUPED_TERM_BRANCH_TAGS.contains(&tag),
        None => false,
    }
}

/// Count strict descendants of `term` that are grouped Term
/// compounds whose `byte_span().hi == term_hi`. Each such
/// descendant has its close-delimiter pending in the source past
/// `term_hi` (because its parse hadn't reached the close at the
/// moment OUR term's rightmost descendant span was pushed).
fn count_pending_inner_groups(term: BbnfView<'_, '_>, term_hi: u32) -> usize {
    fn walk(view: BbnfView<'_, '_>, term_hi: u32, count: &mut usize, is_root: bool) {
        if !is_root && is_grouped_term(view) {
            if let Some((_, hi)) = view.byte_span() {
                if hi == term_hi {
                    *count += 1;
                }
            }
        }
        for child in view.children() {
            walk(child, term_hi, count, false);
        }
    }
    let mut count = 0usize;
    walk(term, term_hi, &mut count, true);
    count
}

/// Recover the modifier punctuator for a non-grouped term. The
/// term's `byte_span()` covers the entire token, so the modifier
/// — if any — is the first `?w` / `?` / `*` / `+` past optional
/// leading whitespace.
fn recover_modifier_non_grouped(mut tail: &str) -> Option<&'static str> {
    while let Some(&b) = tail.as_bytes().first() {
        if matches!(b, b' ' | b'\t' | b'\r' | b'\n') {
            tail = &tail[1..];
            continue;
        }
        break;
    }
    read_modifier_token(tail)
}

/// Recover the modifier punctuator for a grouped term. Walk the
/// source past `term_hi` consuming whitespace + inner-factor
/// modifier punctuators + close-delims until exactly
/// `closes_to_consume` close-delims have been consumed (which
/// includes the OUR group's close as the LAST one). Then skip
/// whitespace and read OUR factor's modifier (if any).
fn recover_modifier_grouped(tail: &str, closes_to_consume: usize) -> Option<&'static str> {
    let bytes = tail.as_bytes();
    let mut idx = 0usize;
    let mut closes_seen = 0usize;
    while idx < bytes.len() && closes_seen < closes_to_consume {
        let b = bytes[idx];
        match b {
            b' ' | b'\t' | b'\r' | b'\n' => idx += 1,
            b')' | b']' | b'}' => {
                idx += 1;
                closes_seen += 1;
            }
            b'?' if bytes.get(idx + 1) == Some(&b'w') => idx += 2,
            b'?' | b'*' | b'+' => idx += 1,
            _ => break,
        }
    }
    if closes_seen < closes_to_consume {
        return None;
    }
    // Past OUR close, skip whitespace and read OUR modifier.
    let mut after = &tail[idx..];
    while let Some(&b) = after.as_bytes().first() {
        if matches!(b, b' ' | b'\t' | b'\r' | b'\n') {
            after = &after[1..];
            continue;
        }
        break;
    }
    read_modifier_token(after)
}

/// Read a `?w` / `?` / `*` / `+` token from the head of `tail`,
/// returning the matched static slice or `None`. Two-character
/// `?w` is tested before single-char `?` so the shorter form
/// doesn't shadow the longer.
fn read_modifier_token(tail: &str) -> Option<&'static str> {
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
