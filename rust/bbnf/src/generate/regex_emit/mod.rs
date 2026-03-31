//! Regex-to-inline-code emission.
//!
//! Two tiers of regex code generation:
//! 1. **HIR walker** (`try_emit_regex_inline`): Walks regex-syntax HIR to emit
//!    direct byte operations. Best for simple patterns (classes, loops, concat).
//! 2. **DFA compiler** (`try_emit_dfa_inline`): Compiles pattern to a minimized
//!    DFA and emits inline state machine or transition table. Handles everything
//!    the HIR walker can't (complex alternation, Unicode properties, lazy quantifiers).
//!
//! Patterns that neither tier can handle produce a compile-time error via
//! `emit_regex_unsupported` — there is no runtime fallback.

pub mod audit;
mod dfa_emit;
mod hir_walk;

pub use dfa_emit::try_emit_dfa_inline;
pub use hir_walk::try_emit_regex_inline;

/// Emit a compile-time error for regex patterns unsupported by both the HIR walker
/// and DFA compiler. This replaces the old LazyLock runtime fallback — unsupported
/// patterns are now a build failure, not a silent runtime degradation.
pub fn emit_regex_unsupported(pattern: &str) -> proc_macro2::TokenStream {
    let msg = format!(
        "regex pattern not compilable by HIR walker or DFA compiler: {}",
        pattern
    );
    quote::quote! { compile_error!(#msg) }
}
