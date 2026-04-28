//! Dependency graph construction from grammar AST.
//!
//! AZ-II.cutover.D — rewritten against the struct-direct
//! [`BbnfView`] surface. Identifier references are collected by
//! walking [`BbnfView::children_iter`] and dispatching on
//! [`BbnfCompoundKind`] for compound focuses or on
//! [`BbnfKind::Span`] for borrowed-source leaves.
//!
//! The struct-direct emitter never produces anonymous wrapper
//! compounds — every compound entry has a named
//! [`BbnfCompoundKind`] populated from the registry's
//! `StructLayout::rule_name`. The pre-cutover.D `int_lit` /
//! `Unknown` peel-through dispatch (which existed only because the
//! tape walker's `variant_idx = 0` collided with the leaf-shape
//! enum's zeroth variant) is therefore gone.

use indexmap::{IndexMap, IndexSet};

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};
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
/// struct-direct view node.
///
/// The walk dispatches on [`BbnfView::compound_kind`] for compounds
/// and on the focused [`BbnfKind`] for leaves. Span leaves whose
/// text matches the identifier shape contribute a reference; literal
/// / regex / numeric leaves emit nothing. The `closure` arm skips
/// the parameter identifiers (the leading Span children) so they
/// aren't mis-classified as nonterminal references.
pub fn collect_nonterminal_refs<'a>(view: BbnfView<'a, 'a>, refs: &mut IndexSet<&'a str>) {
    match view.compound_kind() {
        // ─── Leaf focus dispatch ──────────────────────────────────
        None => {
            if view.kind() == BbnfKind::Span {
                let raw = view.span_text();
                let text = raw.trim();
                if !text.is_empty()
                    && is_ident(text.as_bytes())
                    && !is_value_keyword(text)
                {
                    refs.insert(slice_lifetime_extend(view.input(), text));
                }
            }
        }
        // ─── Compound focus dispatch ──────────────────────────────
        Some(BbnfCompoundKind::Alternation)
        | Some(BbnfCompoundKind::Concatenation)
        | Some(BbnfCompoundKind::CallArg) => {
            for child in view.children_iter() {
                collect_nonterminal_refs(child, refs);
            }
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            for child in view.children_iter() {
                collect_nonterminal_refs(child, refs);
            }
        }
        Some(BbnfCompoundKind::MappedFactor) | Some(BbnfCompoundKind::Factor) => {
            collect_refs_from_compound(view, refs);
        }
        // `value_expr` and its sub-grammar form a self-contained value-
        // expression scope on the right side of a `->` map arrow. The
        // identifiers therein (host-fn names, parameter binders, type
        // annotations) belong to the value-expression alphabet, NOT to
        // grammar-rule references. Bootstrap-parser shape exposes
        // host-fn idents as Span leaves under `value_fn_call` and the
        // pre-cutover.H validator mis-classified them as nonterminal
        // refs, breaking JSON regen-check shape parity.
        Some(BbnfCompoundKind::ValueExpr)
        | Some(BbnfCompoundKind::ValueClosure)
        | Some(BbnfCompoundKind::ValueOr)
        | Some(BbnfCompoundKind::ValueAnd)
        | Some(BbnfCompoundKind::ValueCmp)
        | Some(BbnfCompoundKind::ValueAdd)
        | Some(BbnfCompoundKind::ValueMul)
        | Some(BbnfCompoundKind::ValueUnary)
        | Some(BbnfCompoundKind::ValueAtom)
        | Some(BbnfCompoundKind::ValuePath)
        | Some(BbnfCompoundKind::ValueInput)
        | Some(BbnfCompoundKind::ValueFnCall) => {
            // value-expression scope — never contributes nonterminal
            // refs. Skip the entire subtree.
        }
        // `term = "ε" | identifier , ( "(" , call_arg ... ")" ) ?
        //       | literal | regex | "@{" rhs "}" | "(" rhs ")"
        //       | "[" rhs "]" | "{" rhs "}"`.
        //
        // Under struct-direct the `term` compound's children carry
        // either:
        //   - a Span leaf (identifier / literal / regex)
        //   - a CallArg compound (when call_arg present alongside
        //     identifier)
        //   - an Rhs compound (the four grouped forms)
        //   - the `epsilon` literal — no record is emitted, so the
        //     compound has zero children
        //
        // Walk every child: nested rhs / call_arg compounds recurse;
        // bare Span children check identifier shape; numeric / unit
        // leaves are skipped. literal / regex leaves are also Span
        // values but their content begins with `"` / `'` / `/`, so
        // `is_ident` rules them out.
        Some(BbnfCompoundKind::Term)
        | Some(BbnfCompoundKind::GrammarItem)
        | Some(BbnfCompoundKind::Directive)
        | Some(BbnfCompoundKind::Lhs)
        | Some(BbnfCompoundKind::Rhs) => {
            // First pass: descend into structural children. If any
            // child contributed a ref, we are done. Otherwise fall
            // back to span-text inspection so a single-token term
            // (literal / regex / identifier already inlined into the
            // compound's text via its child Span) still registers.
            let initial = refs.len();
            for child in view.children_iter() {
                collect_nonterminal_refs(child, refs);
            }
            if refs.len() == initial {
                let text = view.span_text().trim();
                if !text.is_empty()
                    && is_ident(text.as_bytes())
                    && !is_value_keyword(text)
                {
                    refs.insert(slice_lifetime_extend(view.input(), text));
                }
            }
        }
        // `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`.
        // The leading Span children are parameter names — skip them.
        // Trailing children that are compounds (the rhs body) recurse
        // normally. The struct-direct emitter records identifier
        // params as Span leaves AND the rhs as a compound child.
        Some(BbnfCompoundKind::Closure) => {
            for child in view.children_iter() {
                if child.is_compound() {
                    collect_nonterminal_refs(child, refs);
                }
            }
        }
        // Top-level grammar / rule / directive compounds aren't
        // expected on the RHS-walking path (they're peeled before
        // descent). Defensive recurse-anyway: better to over-collect
        // than to drop a valid reference.
        Some(_) => {
            for child in view.children_iter() {
                collect_nonterminal_refs(child, refs);
            }
        }
    }
}

/// Compound-aware identifier scanner. Mirrors the analysis layer's
/// `collect_refs_from_compound` — first descend into children, then
/// fall back to scanning span-text gaps for a leading identifier
/// when no child contributed a reference.
fn collect_refs_from_compound<'a>(view: BbnfView<'a, 'a>, refs: &mut IndexSet<&'a str>) {
    let initial = refs.len();
    for child in view.children_iter() {
        collect_nonterminal_refs(child, refs);
    }
    if refs.len() > initial {
        return;
    }
    let Some((node_lo, node_hi)) = view.span_range() else {
        return;
    };
    let input = view.input();
    let mut scan_start = node_lo;
    for child in view.children_iter() {
        if let Some((clo, chi)) = child.span_range() {
            if scan_start < clo {
                extract_ident_from_range(input, scan_start, clo, refs);
            }
            scan_start = chi;
        }
    }
    if scan_start < node_hi {
        extract_ident_from_range(input, scan_start, node_hi, refs);
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
        let lead_ws = input[lo..hi].len() - input[lo..hi].trim_start().len();
        let abs_lo = lo + lead_ws;
        let abs_hi = abs_lo + ident_len;
        refs.insert(&input[abs_lo..abs_hi]);
    }
}

/// Re-borrow `text` from `input` so the returned slice has the
/// `'a` lifetime of the input. Used when `view.span_text()` returns a
/// slice whose lifetime is tied to the document's input — we know the
/// slice is a sub-slice of the input, so the re-borrow is safe.
fn slice_lifetime_extend<'a>(input: &'a str, text: &str) -> &'a str {
    let input_start = input.as_ptr() as usize;
    let input_end = input_start + input.len();
    let s_start = text.as_ptr() as usize;
    let s_end = s_start + text.len();
    if s_start < input_start || s_end > input_end {
        // Fallback: text is not a sub-slice of input. This should not
        // happen on the happy path; locate-or-insert a stable slice
        // by linear search (very rare; bounded by input length).
        if let Some(pos) = input.find(text) {
            return &input[pos..pos + text.len()];
        }
        // Last resort — return an empty slice rather than panic. The
        // collected refs set will never contain a stale pointer.
        return &input[..0];
    }
    let lo = s_start - input_start;
    let hi = lo + text.len();
    &input[lo..hi]
}

/// Returns true if the text is a value keyword that should never
/// be treated as a nonterminal reference (e.g., `true`, `false` in
/// `-> true` mapper expressions).
///
/// Type-name keywords are admitted here so the bootstrap parser's
/// type-annotation surfacing (e.g. `int_lit = /…/ -> i64` —
/// the `i64` Span surfaces inside a `value_expr` subtree) does not
/// register the type name as a nonterminal reference. The list
/// mirrors `crate::lower::value_expr::is_type_name` which the
/// `lower_map_arrow` consumer already special-cases.
fn is_value_keyword(s: &str) -> bool {
    matches!(
        s,
        "true"
            | "false"
            | "null"
            | "epsilon"
            | "ε"
            | "Span"
            | "f32"
            | "f64"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "usize"
            | "bool"
            | "input"
    )
}

/// Quick identifier check on a byte slice.
pub(crate) fn is_ident(s: &[u8]) -> bool {
    match s.first() {
        Some(&b) if b == b'_' || b.is_ascii_alphabetic() => {}
        _ => return false,
    }
    s[1..].iter().all(|&b| b == b'_' || b == b'-' || b.is_ascii_alphanumeric())
}
