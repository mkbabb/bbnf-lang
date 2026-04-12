//! Dependency graph construction from grammar AST.
//!
//! Tranche AC.2: rewritten against the tape-first view surface.
//! Identifier references are collected by walking `children()` on a
//! [`BbnfBootstrapNodeView`] and matching on [`BbnfBootstrapRuleKind`].
//!
//! Tranche AF.0: shape-agnostic structural walk — every semantic
//! handler dispatches on canonical `rule_kind` names only. Sub-
//! variant wrapper kinds (`term_N`) that structural-mode dedup may
//! or may not produce are never referenced; the generic descent
//! into `children()` collects identifier nodes regardless of which
//! wrapper compounds the optimizer has elided around them.

use indexmap::{IndexMap, IndexSet};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};
use crate::types::AST;

/// Rule name → set of referenced rule names.
///
/// `IndexMap` and `IndexSet` are used (not `HashMap`/`HashSet`)
/// because both Tarjan SCC and Kahn topological sort iterate this
/// graph and the iteration order influences the order in which
/// rules end up in the lowered IR — and therefore the order of
/// generated enum variants. Insertion-order semantics keep
/// codegen output byte-stable across runs.
pub type Dependencies<'a> = IndexMap<&'a str, IndexSet<&'a str>>;

/// Build a dependency graph from the grammar AST.
pub fn calculate_ast_deps<'a>(ast: &AST<'a>) -> Dependencies<'a> {
    let mut deps = Dependencies::new();
    for (&name, entry) in ast.iter() {
        let mut refs = IndexSet::new();
        collect_nonterminal_refs(entry.rhs, &mut refs);
        deps.insert(name, refs);
    }
    deps
}

/// Recursively collect nonterminal (identifier) references from a
/// bootstrap view node.
///
/// The walk dispatches on canonical `rule_kind` first, then falls
/// through to a shape-agnostic structural walk. Under the tape-
/// first bootstrap parser, leaf terminals (identifier, literal,
/// regex) are consumed as span-only operations that do NOT push
/// child `Rule` records. Their text sits in the span gap between
/// adjacent child records of the parent compound. The
/// `mapped_factor` / `factor` arms mirror the analysis layer's
/// `collect_refs_from_compound` — scan span-text gaps for bare
/// identifier tokens.
pub fn collect_nonterminal_refs<'a>(
    node: BbnfBootstrapNodeView<'a>,
    refs: &mut IndexSet<&'a str>,
) {
    use ::bbnf::runtime::tape::TapeKind;

    match node.rule_kind() {
        // Leaf: an explicit identifier rule compound.
        BbnfBootstrapRuleKind::identifier => {
            let text = node.span_text().trim();
            if !text.is_empty() && !is_value_keyword(text) {
                refs.insert(text);
            }
        }

        // term_1: identifier + optional call args.
        BbnfBootstrapRuleKind::term_1 => {
            if let Some(ident) = node.child(0) {
                let text = ident.span_text().trim();
                if !text.is_empty() && is_ident(text.as_bytes()) && !is_value_keyword(text) {
                    refs.insert(text);
                }
            }
            for c in node.children().skip(1) {
                collect_nonterminal_refs(c, refs);
            }
        }

        // Structural: alternation, concatenation — iterate branches.
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::concatenation => {
            for child in iter_tape_iteration_views(node) {
                collect_nonterminal_refs(child, refs);
            }
        }

        // Binary factor: first operand + rest operands.
        BbnfBootstrapRuleKind::binary_factor => {
            for operand in collect_tape_binary_operand_views(node) {
                collect_nonterminal_refs(operand, refs);
            }
        }

        // mapped_factor / factor: the term body is often fully inlined
        // into the compound — individual optionals appear as empty
        // Repeat children and the actual content (identifier, literal,
        // regex) is consumed span-only. Scan the span-text gaps between
        // child records for bare identifier tokens.
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
                collect_nonterminal_refs(inner, refs);
            }
        }

        // Grouped: "(" rhs ")" / "[" rhs "]" / "{" rhs "}" / "@{" rhs "}"
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            if let Some(inner) = node.child(1) {
                collect_nonterminal_refs(inner, refs);
            }
        }

        // Closure: |params| body — recurse into body.
        BbnfBootstrapRuleKind::closure => {
            // Body is the trailing Rule child.
            for c in node.children() {
                if c.kind() == TapeKind::Rule {
                    collect_nonterminal_refs(c, refs);
                }
            }
        }

        // Anonymous wrapper compounds (variant_idx=0 collides with
        // `int_lit` in the enum): peel to substantive Rule children.
        BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::Unknown => {
            let mut found_rule = false;
            for c in node.children() {
                if c.kind() == TapeKind::Rule {
                    collect_nonterminal_refs(c, refs);
                    found_rule = true;
                }
            }
            if !found_rule {
                let text = node.span_text().trim();
                if !text.is_empty() && is_ident(text.as_bytes()) && !is_value_keyword(text) {
                    refs.insert(text);
                }
            }
        }

        // Terminals, value-expression compounds, comments — no grammar refs.
        _ => {}
    }
}

/// Scan span-text gaps of a compound node for bare identifier tokens.
/// Mirrors the analysis layer's `collect_refs_from_compound`.
fn collect_refs_from_compound<'a>(
    node: BbnfBootstrapNodeView<'a>,
    refs: &mut IndexSet<&'a str>,
) {
    use ::bbnf::runtime::tape::TapeKind;

    // First pass: recurse into Rule children.
    let initial_count = refs.len();
    for c in node.children() {
        if c.kind() == TapeKind::Rule {
            collect_nonterminal_refs(c, refs);
        }
    }
    if refs.len() > initial_count {
        return;
    }

    // Second pass: scan span-text gaps for identifiers.
    let (node_lo, node_hi) = node.span();
    let input = node.input();

    let child_spans: Vec<(u32, u32)> = node
        .children()
        .map(|c| c.span())
        .filter(|(lo, hi)| lo < hi)
        .collect();

    let mut scan_start = node_lo;
    for &(clo, chi) in &child_spans {
        if scan_start < clo {
            extract_ident_from_range(input, scan_start as usize, clo as usize, refs);
        }
        scan_start = chi;
    }
    if scan_start < node_hi {
        extract_ident_from_range(input, scan_start as usize, node_hi as usize, refs);
    }
}

/// Scan a byte range for a leading identifier token.
fn extract_ident_from_range<'a>(
    input: &'a str,
    lo: usize,
    hi: usize,
    refs: &mut IndexSet<&'a str>,
) {
    let text = input[lo..hi].trim();
    if text.is_empty() {
        return;
    }
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
    if !is_value_keyword(ident) {
        // Compute the slice within the input string so we get a
        // reference with the `'a` lifetime of `input`.
        let lead_ws = input[lo..hi].len() - input[lo..hi].trim_start().len();
        let abs_lo = lo + lead_ws;
        let abs_hi = abs_lo + ident_len;
        refs.insert(&input[abs_lo..abs_hi]);
    }
}

/// Iterate iteration-pair children, unwrapping a single top-level
/// `TapeKind::Repeat` wrapper. Local mirror of the analysis layer's
/// `iter_iteration_views` / `iter_rep_children`.
pub(crate) fn iter_tape_iteration_views<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use ::bbnf::runtime::tape::TapeKind;

    let raw_children: Vec<_> = {
        let mut children = node.children();
        let first = match children.next() {
            Some(c) => c,
            None => return Vec::new(),
        };
        if children.next().is_none() && first.kind() == TapeKind::Repeat {
            first.children().collect()
        } else {
            node.children().collect()
        }
    };

    raw_children
        .into_iter()
        .filter_map(|pair| {
            let candidate = match pair.kind() {
                TapeKind::Seq => match pair.child(0) {
                    Some(c) => c,
                    None => return None,
                },
                _ => pair,
            };
            let span = candidate.span_text().trim();
            if span.is_empty() || span == "|" || span == "," {
                return None;
            }
            Some(peel_wrapper(candidate))
        })
        .collect()
}

/// Collect binary-factor operand views.
pub(crate) fn collect_tape_binary_operand_views<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use ::bbnf::runtime::tape::TapeKind;
    let mut children = node.children();
    let first = match children.next() {
        Some(f) => f,
        None => return Vec::new(),
    };
    let rest: Vec<_> = children.collect();
    let mut v = vec![first];
    if rest.len() == 1 && rest[0].kind() == TapeKind::Repeat {
        v.extend(rest[0].children());
    } else {
        v.extend(rest);
    }
    v
}

/// Peel anonymous wrapper compounds to reach the substantive Rule child.
fn peel_wrapper<'a>(node: BbnfBootstrapNodeView<'a>) -> BbnfBootstrapNodeView<'a> {
    use ::bbnf::runtime::tape::TapeKind;
    let kind = node.rule_kind();
    if !matches!(kind, BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit) {
        return node;
    }
    let substantive: Vec<_> = node
        .children()
        .filter(|c| c.kind() == TapeKind::Rule)
        .collect();
    match substantive.len() {
        1 => peel_wrapper(substantive[0]),
        _ => node,
    }
}

/// Returns true if the text is a value keyword that should never
/// be treated as a nonterminal reference (e.g., `true`, `false` in
/// `-> true` mapper expressions).
fn is_value_keyword(s: &str) -> bool {
    matches!(s, "true" | "false" | "null" | "epsilon" | "ε")
}

/// Quick identifier check on a byte slice.
pub(crate) fn is_ident(s: &[u8]) -> bool {
    match s.first() {
        Some(&b) if b == b'_' || b.is_ascii_alphabetic() => {}
        _ => return false,
    }
    s[1..].iter().all(|&b| b == b'_' || b == b'-' || b.is_ascii_alphanumeric())
}
