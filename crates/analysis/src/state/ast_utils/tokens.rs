//! Collect semantic tokens from a struct-direct [`BbnfView`] rule
//! RHS.
//!
//! AZ-II.cutover.D4 — re-targets the pre-cutover.D tape-cursor walker
//! at [`BbnfView`] + [`BbnfCompoundKind`] dispatch. Rule references,
//! literals, regex, and the epsilon keyword all surface as Span
//! leaves under the new runtime; structural compounds (alternation,
//! concatenation, binary_factor, mapped_factor, factor, term,
//! closure) cascade into the recursive walk below.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::{BbnfCompoundKind, BbnfView};

use super::super::types::{SemanticTokenInfo, token_types};

/// Collect semantic tokens from a [`BbnfView`].
pub fn collect_semantic_tokens(
    node: BbnfView<'_, '_>,
    tokens: &mut Vec<SemanticTokenInfo>,
) {
    // Span leaves carry their own byte-span via the BbnfValue::Span
    // payload. Classify by leading byte under the analysis layer's
    // pre-existing scheme (identifier-like → RULE_REFERENCE, quoted
    // → STRING, slashed → REGEXP, "ε" → KEYWORD).
    if !node.is_compound() {
        emit_leaf_token(node, tokens);
        return;
    }

    match node.compound_kind() {
        // Term: dispatch on branch_tag.
        Some(BbnfCompoundKind::Term) => {
            match node.branch_tag() {
                Some(0) => {
                    // Epsilon keyword.
                    if let Some((lo, hi)) = node.byte_span() {
                        tokens.push(SemanticTokenInfo {
                            span: (lo, hi),
                            token_type: token_types::KEYWORD,
                        });
                    }
                }
                Some(1) => {
                    // Identifier with optional call-args.
                    if let Some(ident) = node.child(0) {
                        if let Some((lo, hi)) = ident.byte_span() {
                            tokens.push(SemanticTokenInfo {
                                span: (lo, hi),
                                token_type: token_types::RULE_REFERENCE,
                            });
                        }
                    }
                    // Recurse into call-arg children.
                    for c in node.children().filter(|c| {
                        c.compound_kind() == Some(BbnfCompoundKind::CallArg)
                    }) {
                        collect_semantic_tokens(c, tokens);
                    }
                }
                Some(b @ 4..=7) => {
                    // Grouped: descend into the inner Rhs.
                    let _ = b;
                    if let Some(inner) = node
                        .children()
                        .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
                    {
                        collect_semantic_tokens(inner, tokens);
                    }
                }
                _ => {
                    // Term branches 2 / 3 (bare literal / regex) — the
                    // sole substantive child is a Span leaf the walker
                    // emits directly.
                    if let Some(inner) = node.child(0) {
                        collect_semantic_tokens(inner, tokens);
                    }
                }
            }
        }

        // Structural compounds: walk every iteration child.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::Concatenation) => {
            for child in super::iter_iteration_views(node) {
                collect_semantic_tokens(child, tokens);
            }
        }

        // Binary factor: walk every operand.
        Some(BbnfCompoundKind::BinaryFactor) => {
            for operand in super::collect_binary_operand_views(node) {
                collect_semantic_tokens(operand, tokens);
            }
        }

        // Mapped factor: recurse into the inner factor.
        Some(BbnfCompoundKind::MappedFactor) => {
            if let Some(inner) = node.child(0) {
                collect_semantic_tokens(inner, tokens);
            }
        }

        // Factor: find the term child and recurse.
        Some(BbnfCompoundKind::Factor) => {
            for c in node.children() {
                if c.compound_kind() == Some(BbnfCompoundKind::Term) {
                    collect_semantic_tokens(c, tokens);
                    break;
                }
            }
        }

        // Closure: recurse into the body. The body is the trailing
        // Rhs child of the closure compound.
        Some(BbnfCompoundKind::Closure) => {
            if let Some(body) = node
                .children()
                .find(|c| c.compound_kind() == Some(BbnfCompoundKind::Rhs))
            {
                collect_semantic_tokens(body, tokens);
            }
        }

        // Transparent wrappers — descend into child(0).
        Some(BbnfCompoundKind::Rhs)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs) => {
            if let Some(inner) = node.child(0) {
                collect_semantic_tokens(inner, tokens);
            }
        }

        // CallArg: walk every operand (binary_factor or mapped_factor).
        Some(BbnfCompoundKind::CallArg) => {
            for c in node.children() {
                collect_semantic_tokens(c, tokens);
            }
        }

        // Other compounds (directives, value-expressions, etc.) —
        // recurse into structural children but do not emit tokens
        // for the compound's own span.
        _ => {
            for c in node.children() {
                collect_semantic_tokens(c, tokens);
            }
        }
    }
}

/// Emit a semantic token for a Span-leaf focus, classified by the
/// leading source byte.
fn emit_leaf_token(node: BbnfView<'_, '_>, tokens: &mut Vec<SemanticTokenInfo>) {
    let span = match node.byte_span() {
        Some(s) => s,
        None => return,
    };
    let text = match node.span_text() {
        Some(t) => t.trim(),
        None => return,
    };
    if text.is_empty() {
        return;
    }
    // Adjust the recorded span to drop leading / trailing whitespace
    // — the analysis layer's existing convention is to point exactly
    // at the printable run.
    let raw = node.span_text().unwrap_or("");
    let lead_ws = raw.len() - raw.trim_start().len();
    let trail_ws = raw.len() - raw.trim_end().len();
    let tok_lo = span.0 + lead_ws;
    let tok_hi = span.1 - trail_ws;
    let token_type = classify_leaf_token(text);
    if let Some(tt) = token_type {
        tokens.push(SemanticTokenInfo {
            span: (tok_lo, tok_hi),
            token_type: tt,
        });
    }
}

/// Classify a leaf token by its first byte / shape.
fn classify_leaf_token(text: &str) -> Option<u32> {
    if text == "\u{03b5}" || text == "epsilon" {
        return Some(token_types::KEYWORD);
    }
    let first = text.as_bytes().first().copied()?;
    if first == b'"' || first == b'\'' || first == b'`' {
        Some(token_types::STRING)
    } else if first == b'/' {
        Some(token_types::REGEXP)
    } else if first == b'_' || first.is_ascii_alphabetic() {
        Some(token_types::RULE_REFERENCE)
    } else {
        None
    }
}
