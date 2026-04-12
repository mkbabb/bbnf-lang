//! Recursively collect nonterminal references from a tape-first
//! `BbnfBootstrapNodeView` rule RHS.
//!
//! Walks the tape view shape-agnostically: `term_1` identifier-call
//! compounds, nested `term`/`factor`/`mapped_factor` wrappers,
//! grouped `term_2` expressions, alternation / concatenation, and
//! binary-factor chains all cascade into a recursive descent that
//! bottoms out at the `identifier` leaf.

use bbnf::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::types::ReferenceInfo;

/// Check if a string is a valid BBNF identifier: `[_a-zA-Z][_a-zA-Z0-9-]*`.
fn is_ident(s: &str) -> bool {
    let mut chars = s.bytes();
    match chars.next() {
        Some(b) if b == b'_' || b.is_ascii_alphabetic() => {}
        _ => return false,
    }
    chars.all(|b| b == b'_' || b == b'-' || b.is_ascii_alphanumeric())
}

/// Collect references from a compound node (mapped_factor / factor)
/// where the inner term body may be fully inlined. First tries to
/// find Rule children to recurse into; when none produce references,
/// scans the source gaps between child spans for bare identifier
/// tokens.
fn collect_refs_from_compound(
    node: BbnfBootstrapNodeView<'_>,
    refs: &mut Vec<ReferenceInfo>,
) {
    use ::bbnf::runtime::tape::TapeKind;

    // First pass: recurse into any Rule children.
    let initial_count = refs.len();
    for c in node.children() {
        if c.kind() == TapeKind::Rule {
            collect_references(c, refs);
        }
    }

    // If Rule children produced references, we're done.
    if refs.len() > initial_count {
        return;
    }

    // Second pass: find identifiers in the span text gaps between
    // child records. The tape-first generator consumes leaf tokens
    // (identifier, literal, regex) via span-only operations that
    // don't push child records — their text sits in the gap between
    // adjacent child spans.
    let (node_lo, node_hi) = node.span();
    let input = node.input();

    // Collect child spans to identify gaps.
    let child_spans: Vec<(u32, u32)> = node
        .children()
        .map(|c| c.span())
        .filter(|(lo, hi)| lo < hi) // skip empty-span placeholders
        .collect();

    // Scan gaps for identifiers.
    let mut scan_start = node_lo;
    for &(clo, chi) in &child_spans {
        if scan_start < clo {
            extract_ident_from_range(input, scan_start as usize, clo as usize, refs);
        }
        scan_start = chi;
    }
    // Trailing gap.
    if scan_start < node_hi {
        extract_ident_from_range(input, scan_start as usize, node_hi as usize, refs);
    }
}

/// Scan a byte range for a leading identifier token and push it as a reference.
fn extract_ident_from_range(
    input: &str,
    lo: usize,
    hi: usize,
    refs: &mut Vec<ReferenceInfo>,
) {
    let text = input[lo..hi].trim();
    if text.is_empty() {
        return;
    }
    // Find the leading identifier run.
    let ident_len = text
        .bytes()
        .enumerate()
        .take_while(|&(i, b)| {
            if i == 0 {
                b == b'_' || b.is_ascii_alphabetic()
            } else {
                b == b'_' || b == b'-' || b.is_ascii_alphanumeric()
            }
        })
        .count();
    if ident_len == 0 {
        return;
    }
    let ident = &text[..ident_len];
    // Skip BBNF keywords and epsilon.
    if matches!(ident, "epsilon" | "ε") {
        return;
    }
    // Compute byte offset in the original input.
    let lead_ws = input[lo..hi].len() - input[lo..hi].trim_start().len();
    let ident_start = lo + lead_ws;
    refs.push(ReferenceInfo {
        name: ident.to_string(),
        span: (ident_start, ident_start + ident_len),
    });
}

/// Recursively collect nonterminal references from a bootstrap tape view.
pub fn collect_references(node: BbnfBootstrapNodeView<'_>, refs: &mut Vec<ReferenceInfo>) {
    match node.rule_kind() {
        // Leaf: identifier reference. The bare `identifier` leaf
        // variant pushes a rule compound whose span covers the
        // identifier text; the name is its span_text.
        BbnfBootstrapRuleKind::identifier => {
            let (lo, hi) = node.span();
            refs.push(ReferenceInfo {
                name: node.span_text().to_string(),
                span: (lo as usize, hi as usize),
            });
        }

        // term_1: identifier + optional call args.
        //   term_1 = identifier ( "(" first_arg ( "," arg )* ")" )?
        // Child layout under the tape view:
        //   child(0) = identifier
        //   child(1) = optional call-args group
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                let (lo, hi) = ident.span();
                refs.push(ReferenceInfo {
                    name: ident.span_text().to_string(),
                    span: (lo as usize, hi as usize),
                });
            }
            // Any remaining children (the optional call-args group)
            // are descended into — the args themselves may carry
            // nonterminal references.
            for c in node.children().skip(1) {
                collect_references(c, refs);
            }
        }

        // Structural: alternation, concatenation. Walk every
        // iteration child and recurse.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::concatenation => {
            for child in super::iter_iteration_views(node) {
                collect_references(child, refs);
            }
        }

        // Binary factor: first operand + rest operands.
        BbnfBootstrapRuleKind::binary_factor => {
            for operand in super::collect_binary_operand_views(node) {
                collect_references(operand, refs);
            }
        }

        // Mapped factor / factor: the factor body may be fully inlined
        // into the mapped_factor compound — individual optionals from
        // `factor = big_comment? , term ?w , modifier? , big_comment?`
        // appear as direct children, and the `term` content (identifier,
        // literal, regex, grouped expression) is consumed as a span
        // operation without a child Rule record.
        //
        // Strategy: first try to find Rule children to recurse into.
        // If none are found, scan the node's span text for identifier
        // tokens that represent nonterminal references.
        BbnfBootstrapRuleKind::mapped_factor | BbnfBootstrapRuleKind::factor => {
            collect_refs_from_compound(node, refs);
        }

        // Transparent wrappers — descend into the single inner child.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::rhs
        | BbnfBootstrapRuleKind::grammar_item
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::lhs => {
            if let Some(inner) = node.child(0) {
                collect_references(inner, refs);
            }
        }

        // Grouped: "(" rhs ")" / "[" rhs "]" / "{" rhs "}" / "@{" rhs "}"
        //   term_2 / value_atom_0 layout: child(0) = open, child(1) = inner, child(2) = close
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            if let Some(inner) = node.child(1) {
                collect_references(inner, refs);
            }
        }

        // Closure: |params| body — recurse into the body child
        // (trailing child of the closure compound).
        BbnfBootstrapRuleKind::closure => {
            if let Some(body) = node.child(4) {
                collect_references(body, refs);
            }
        }

        // Anonymous wrapper compounds (variant_idx=0 collides with
        // `int_lit`): peel to substantive Rule children and recurse.
        // When no Rule children exist, check if the span text is a
        // bare identifier — the tape-first generator may have fused
        // the identifier into a span-only scan without pushing a
        // separate Rule record.
        // Mirrors the lowering's dispatch_expression fallback +
        // is_single_token_span / lower_leaf_by_span_text path.
        BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
            use ::bbnf::runtime::tape::TapeKind;
            let mut found_rule = false;
            for c in node.children() {
                if c.kind() == TapeKind::Rule {
                    collect_references(c, refs);
                    found_rule = true;
                }
            }
            if !found_rule {
                // No Rule children — check if this wrapper's span
                // text is a bare identifier token.
                let text = node.span_text().trim();
                if !text.is_empty() && is_ident(text) {
                    let (lo, hi) = node.span();
                    // Compute trimmed span offsets.
                    let lead_ws = node.span_text().len() - node.span_text().trim_start().len();
                    let trail_ws = node.span_text().len() - node.span_text().trim_end().len();
                    let ident_lo = lo as usize + lead_ws;
                    let ident_hi = hi as usize - trail_ws;
                    refs.push(ReferenceInfo {
                        name: text.to_string(),
                        span: (ident_lo, ident_hi),
                    });
                }
            }
        }

        // Terminals and value-expression compounds don't carry
        // grammar nonterminal references. Everything else is a
        // leaf, directive, or grammar-level construct and
        // contributes nothing.
        _ => {}
    }
}
