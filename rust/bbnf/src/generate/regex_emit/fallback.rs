//! Fallback regex emission via `LazyLock<Regex>`.
//!
//! Used when `try_emit_regex_inline` returns `None` — the pattern has features
//! that cannot be compiled to inline byte operations (e.g., Unicode properties,
//! backreferences, complex lookahead).

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a `LazyLock<Regex>` that matches the pattern at the current offset.
///
/// Returns `Option<Span>` — advances `state.offset` on match, returns `None`
/// on mismatch. The regex is compiled once and reused across calls.
pub fn emit_regex_lazy_static(pattern: &str) -> TokenStream {
    let pattern_lit = proc_macro2::Literal::string(pattern);
    quote! {
        {
            static __RE: ::std::sync::LazyLock<::regex::Regex> =
                ::std::sync::LazyLock::new(|| ::regex::Regex::new(#pattern_lit).unwrap());
            let __start = state.offset;
            __RE.find_at(state.src, __start)
                .filter(|m| m.start() == __start)
                .map(|m| {
                    state.offset = m.end();
                    ::parse_that::Span::new(__start, m.end(), state.src)
                })
        }
    }
}
