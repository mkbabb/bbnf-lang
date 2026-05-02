//! Closure partitioning helpers.
//!
//! Closures are first-class grammar functions expanded inline during
//! lowering; the AST partition extracts them before SCC/topo analysis
//! so the analyzer never sees a closure RHS as a structural rule.
//!
//! # Surface
//!
//! - [`partition_closures`] — separate closure rules from the AST and
//!   return `(closures, non-closure rules)`.
//! - [`is_closure_rhs`] — predicate on a struct-direct view; recognises
//!   the `Closure` compound shape after unwrapping single-branch
//!   structural wrappers.
//! - [`collect_closure_param_names`] — walk a closure body and collect
//!   every parameter Span as a `&'a str` against the input lifetime.

use crate::runtime::bbnf::{BbnfCompoundKind, BbnfView};
use crate::types::AST;

/// Separate closure rules from the AST. Returns (closures, non-closure rules).
pub(super) fn partition_closures<'a>(ast: AST<'a>) -> (Vec<(&'a str, BbnfView<'a, 'a>)>, AST<'a>) {
    let mut closures: Vec<(&'a str, BbnfView<'a, 'a>)> = Vec::new();
    let mut rules: AST<'a> = indexmap::IndexMap::new();

    for (&name, entry) in &ast {
        if is_closure_rhs(entry.rhs) {
            closures.push((name, entry.rhs));
        } else {
            rules.insert(name, *entry);
        }
    }

    (closures, rules)
}

/// Check if a struct-direct RHS view is a closure, unwrapping
/// single-branch structural wrappers.
pub(super) fn is_closure_rhs(view: BbnfView<'_, '_>) -> bool {
    match view.compound_kind() {
        Some(BbnfCompoundKind::Closure) => true,
        // Unwrap single-branch alternation / call_arg wrappers.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            let mut iter = view.children_iter();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            // The struct-direct alternation child is the chosen branch
            // value directly (no extra Seq wrapper); recurse.
            is_closure_rhs(first)
        }
        Some(BbnfCompoundKind::Concatenation) => {
            let mut iter = view.children_iter();
            let Some(first) = iter.next() else {
                return false;
            };
            if iter.next().is_some() {
                return false;
            }
            is_closure_rhs(first)
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            // `binary_factor = mapped_factor , ( binary_operators ?w
            //   mapped_factor ?w ) *`. A bare binary_factor with no
            // operators is a single-operand wrapper — recurse into
            // child(0). When operator children are present (compound
            // count > 1) the chain is operator-typed, not a closure.
            if view.num_children() != 1 {
                return false;
            }
            view.child(0).map(is_closure_rhs).unwrap_or(false)
        }
        Some(BbnfCompoundKind::MappedFactor) => {
            // `mapped_factor = factor , ( "->" ?w , value_expr ,
            //   type_annotation ? ) ?`. The mapping slot manifests in
            // the source span as a `->` substring; absent when the
            // compound's text doesn't contain `->`.
            if view.span_text().contains("->") {
                return false;
            }
            view.child(0).map(is_closure_rhs).unwrap_or(false)
        }
        Some(BbnfCompoundKind::Factor) => {
            // `factor = big_comment ? , term ?w , modifier ? ,
            //   big_comment ?`. The modifier slot manifests as a
            // trailing `?` / `*` / `+` byte on the compound's text.
            // Comments embed `/*` markers; recurse only when neither
            // is present.
            let text = view.span_text().trim();
            if matches!(text.as_bytes().last(), Some(b'?') | Some(b'*') | Some(b'+')) {
                return false;
            }
            if text.contains("/*") {
                return false;
            }
            // Locate the inner `term` and recurse on it; if absent,
            // dispatch on whatever the factor's first child is.
            let inner = view
                .find_descendant_by_kind(BbnfCompoundKind::Term)
                .or_else(|| view.child(0));
            inner.map(is_closure_rhs).unwrap_or(false)
        }
        _ => false,
    }
}

/// Collect closure parameter names from a struct-direct view node.
///
/// `closure = "|" , identifier , ( "," ?w , identifier ) * , "|" ?w , rhs`
/// — the leading Span children of a `Closure` compound are the
/// parameter names; the trailing compound child is the body. Walking
/// the closure's children and grabbing every Span-typed leaf yields
/// every param without depending on positional indices.
pub(super) fn collect_closure_param_names<'a>(
    view: BbnfView<'a, 'a>,
    params: &mut std::collections::HashSet<&'a str>,
) {
    match view.compound_kind() {
        Some(BbnfCompoundKind::Closure) => {
            for child in view.children_iter() {
                if !child.is_compound() {
                    if child.is_span() {
                        // The Span text comes from the document's input
                        // slice; re-borrow against `view.input()` so
                        // the lifetime matches the param-set's `'a`.
                        let text = child.span_text();
                        if !text.is_empty() {
                            params.insert(slice_in_input(view.input(), text));
                        }
                    }
                }
                // Compound children (the rhs body) don't contribute
                // closure params for the OUTER closure's signature —
                // nested closures inside the body are handled via
                // their own collect_closure_param_names call when the
                // outer rule list is iterated.
            }
        }
        // Unwrap single-branch structural wrappers.
        Some(BbnfCompoundKind::Alternation) | Some(BbnfCompoundKind::CallArg) => {
            let mut iter = view.children_iter();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    collect_closure_param_names(first, params);
                }
            }
        }
        Some(BbnfCompoundKind::Concatenation) => {
            let mut iter = view.children_iter();
            if let Some(first) = iter.next() {
                if iter.next().is_none() {
                    collect_closure_param_names(first, params);
                }
            }
        }
        Some(BbnfCompoundKind::BinaryFactor) => {
            if view.num_children() != 1 {
                return;
            }
            if let Some(inner) = view.child(0) {
                collect_closure_param_names(inner, params);
            }
        }
        Some(BbnfCompoundKind::MappedFactor) => {
            if view.span_text().contains("->") {
                return;
            }
            if let Some(inner) = view.child(0) {
                collect_closure_param_names(inner, params);
            }
        }
        Some(BbnfCompoundKind::Factor) => {
            let text = view.span_text().trim();
            if matches!(text.as_bytes().last(), Some(b'?') | Some(b'*') | Some(b'+')) {
                return;
            }
            if text.contains("/*") {
                return;
            }
            let inner = view
                .find_descendant_by_kind(BbnfCompoundKind::Term)
                .or_else(|| view.child(0));
            if let Some(inner) = inner {
                collect_closure_param_names(inner, params);
            }
        }
        _ => {}
    }
}

/// Re-borrow `text` against `input` so the returned slice has the
/// `'a` lifetime of the input. The struct-direct `BbnfValue::Span`
/// payload is already a sub-slice of the document's input; the
/// pointer-arithmetic recover keeps the lifetime tight without
/// allocation.
fn slice_in_input<'a>(input: &'a str, text: &str) -> &'a str {
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
