//! AST helper functions over `bbnf::grammar::generated::BbnfBootstrapEnum` nodes.
//!
//! Pure functions consumed by `state/diagnostics.rs` and `features/*.rs`.

use bbnf::grammar::generated::BbnfBootstrapEnum;

pub mod cycles;
pub mod format;
pub mod references;
pub mod spans;
pub mod tokens;

pub use cycles::{build_cycle_path, compute_reachable_rules};
pub use format::{format_expression_short, format_value_expr_short};
pub use references::collect_references;
pub use spans::{compute_expression_end, compute_expression_end_pub};
pub use tokens::collect_semantic_tokens;

/// Format a byte as a display-friendly character for inlay hints.
pub fn format_char(b: u8) -> String {
    match b {
        b'\t' => "\\t".into(),
        b'\n' => "\\n".into(),
        b'\r' => "\\r".into(),
        b' ' => "SP".into(),
        0x0b => "\\v".into(),
        0x0c => "\\f".into(),
        c if c.is_ascii_graphic() => format!("'{}'", c as char),
        c => format!("0x{:02x}", c),
    }
}

/// Check if a rule RHS is effectively empty (epsilon only).
pub fn is_empty_rhs(node: &BbnfBootstrapEnum<'_>) -> bool {
    match node {
        BbnfBootstrapEnum::term_0(_) => true,
        // Unwrap structural wrappers
        BbnfBootstrapEnum::term(inner) => is_empty_rhs(inner),
        BbnfBootstrapEnum::factor((_, inner, None, _)) => is_empty_rhs(inner),
        BbnfBootstrapEnum::mapped_factor((inner, None)) => is_empty_rhs(inner),
        BbnfBootstrapEnum::binary_factor((first, rest)) if rest.is_empty() => is_empty_rhs(first),
        BbnfBootstrapEnum::concatenation(parts) if parts.len() == 1 => is_empty_rhs(parts[0].0),
        BbnfBootstrapEnum::alternation(branches) if branches.len() == 1 => {
            is_empty_rhs(branches[0].0)
        }
        _ => false,
    }
}
