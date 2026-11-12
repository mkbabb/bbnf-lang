//! `BBNF-RX-NOU`: a regex leaf whose reading differs without `u`.
//!
//! A BBNF regex leaf means what ECMAScript reads with the `u` flag: a class
//! is a set of scalar values and matches one whole scalar (sk-v27 OR-1,
//! LW3.RX-1d). A leaf is written without flags, and the TypeScript target
//! and other readers of the source may read it without `u`, over UTF-16 code
//! units. The two readings differ only where a class can match a surrogate
//! code unit (a negated class, `.`, `\S`, a class with an astral member or a
//! range across the surrogate block); the engine's lint
//! ([`parse_that::regex::lint`], id `RX-NOU`) names those leaves, and this
//! pass reports them under the BBNF namespace.

use bbnf::runtime::RuntimeView;
use bbnf::runtime::bbnf::BbnfView;
use bbnf::types::AST;
use ls_types::*;
use parse_that::regex::lint::{RX_NOU, reads_differently_without_u};

use crate::analysis::LineIndex;

/// The lint's code: the engine's general id under the `BBNF-` namespace.
pub fn code() -> String {
    format!("BBNF-{RX_NOU}")
}

pub(super) fn lint_non_u_leaves(
    ast: &AST<'_>,
    line_index: &LineIndex,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut leaves = Vec::new();
    for entry in ast.values() {
        regex_leaves(entry.rhs, &mut leaves);
    }
    for (span, leaf) in leaves {
        let Some(pattern) = leaf.strip_prefix('/').and_then(|p| p.strip_suffix('/')) else {
            continue;
        };
        // A leaf the engine does not parse is reported by the pipeline, not here.
        let Ok(hir) = parse_that::regex::parse(pattern) else {
            continue;
        };
        if reads_differently_without_u(&hir) {
            diagnostics.push(Diagnostic {
                range: line_index.span_to_range(span.0, span.1),
                severity: Some(DiagnosticSeverity::INFORMATION),
                code: Some(NumberOrString::String(code())),
                source: Some(crate::DIAGNOSTIC_SOURCE.into()),
                message: "This regex is read with `u` semantics (one scalar per class match); \
                          read without `u`, a class here can also match half of a surrogate pair"
                    .into(),
                ..Default::default()
            });
        }
    }
}

/// Every regex leaf (`/…/`) under `node`, with its source span.
fn regex_leaves<'a>(node: BbnfView<'a, 'a>, out: &mut Vec<((usize, usize), &'a str)>) {
    if !node.is_compound() {
        if let (Some((lo, _)), Some(raw)) = (node.span_range(), node.span_text_opt()) {
            let text = raw.trim();
            if text.len() >= 2 && text.starts_with('/') && text.ends_with('/') {
                let lead = raw.len() - raw.trim_start().len();
                out.push(((lo + lead, lo + lead + text.len()), text));
            }
        }
        return;
    }
    for child in node.children() {
        regex_leaves(child, out);
    }
}
