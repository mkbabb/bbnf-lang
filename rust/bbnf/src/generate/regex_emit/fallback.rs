//! Fallback regex emission via `LazyLock<Dfa>`.
//!
//! Used when `try_emit_regex_inline` and `try_emit_dfa_inline` both return
//! `None` — the pattern has features that cannot be compiled to inline byte
//! operations or a static DFA table. Compiles the DFA at runtime instead.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a `LazyLock`-cached DFA that matches the pattern at the current offset.
///
/// Returns `Option<Span>` — advances `state.offset` on match, returns `None`
/// on mismatch. The DFA is compiled once and reused across calls.
pub fn emit_regex_lazy_static(pattern: &str) -> TokenStream {
    let pattern_lit = proc_macro2::Literal::string(pattern);
    quote! {
        {
            let __start = state.offset;
            let __dfa = ::parse_that::cached_dfa(#pattern_lit);
            let __bytes = &state.src_bytes[__start..];
            match __dfa.find_at(__bytes, 0) {
                Some(__end) if __end > 0 => {
                    state.offset = __start + __end;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
                _ => None,
            }
        }
    }
}
