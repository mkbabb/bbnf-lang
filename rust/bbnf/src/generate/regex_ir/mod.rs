//! Regex classification, compilation, and inline code emission.
//!
//! Three subsystems:
//! - **classify** — HIR-based structural regex classification (Numeric, HexDigits, Identifier, etc.)
//! - **hir/dfa** — Two-tier inline code emission: HIR walker for simple patterns, DFA compiler for complex ones.
//! - **fast_paths** — Pattern detection and specialized inline byte-scanner emission.

pub mod classify;
pub mod fast_paths;
pub mod shorthand;

pub mod audit;
mod dfa;
mod hir;

pub use audit::audit_regex_pattern;
pub use dfa::try_emit_dfa_inline;
pub use hir::try_emit_regex_inline;

/// Emit a compile-time error for regex patterns unsupported by both the HIR walker
/// and DFA compiler. No runtime fallback — unsupported patterns are a build failure.
pub fn emit_regex_unsupported(pattern: &str) -> proc_macro2::TokenStream {
    let msg = format!(
        "regex pattern not compilable by HIR walker or DFA compiler: {}",
        pattern
    );
    quote::quote! { compile_error!(#msg) }
}
