//! Helper utilities for IR pretty-printer codegen.
//!
//! Hint conversion, IR node unwrapping helpers.

use bbnf_ir::{IrNode, PrettyHints};

/// Convert a `PrettyHints` struct to a `Vec<String>` for use with the existing
/// doc generation functions.
pub(crate) fn pretty_hints_to_strings(ph: &PrettyHints) -> Vec<String> {
    let mut hints = Vec::new();
    if ph.group {
        hints.push("group".to_string());
    }
    if ph.indent {
        hints.push("indent".to_string());
    }
    if ph.dedent {
        hints.push("dedent".to_string());
    }
    if ph.block {
        hints.push("block".to_string());
    }
    if ph.blankline {
        hints.push("blankline".to_string());
    }
    if ph.nobreak {
        hints.push("nobreak".to_string());
    }
    if ph.softbreak {
        hints.push("softbreak".to_string());
    }
    if ph.hardbreak {
        hints.push("hardbreak".to_string());
    }
    if ph.compact {
        hints.push("compact".to_string());
    }
    if ph.fast {
        hints.push("fast".to_string());
    }
    if ph.off {
        hints.push("off".to_string());
    }
    if let Some(ref s) = ph.sep {
        hints.push(format!("sep(\"{}\")", s));
    }
    if let Some(ref s) = ph.split {
        hints.push(format!("split(\"{}\")", s));
    }
    hints
}

/// Unwrap Map wrappers to get to the inner expression.
pub(crate) fn unwrap_ir_map(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_ir_map(inner),
        other => other,
    }
}

/// Unwrap OptionalWhitespace wrappers.
pub(crate) fn unwrap_ir_whitespace(node: &IrNode) -> &IrNode {
    match node {
        IrNode::OptionalWhitespace(inner) => unwrap_ir_whitespace(inner),
        other => other,
    }
}
