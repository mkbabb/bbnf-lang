//! Identifier kernel emission.
//!
//! Targets `parse_that::scan_ident` (`parsers/scan/ident.rs`).
//! Used by `RegexClass::Identifier`, `CssIdent`. The dialect flags
//! (`allows_leading_dash`, `allows_double_dash_prefix`) parameterize
//! the emitted `IdentConfig` so that the kernel scans exactly the
//! identifier syntax the originating regex declared — bare ident,
//! vendor-prefixed, custom-property, or the full CSS fold.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit a call to the default-config identifier scanner. Accepts
/// `[a-zA-Z_][\w-]*` (no vendor prefix, no custom property).
pub fn emit_call() -> TokenStream {
    quote! { ::parse_that::scan_ident(state, &::parse_that::DEFAULT_IDENT_CONFIG) }
}

/// Emit a call to the CSS-flavored identifier scanner. Accepts
/// vendor prefixes (`-foo`) and custom properties (`--foo`).
pub fn emit_call_css() -> TokenStream {
    quote! { ::parse_that::scan_ident(state, &::parse_that::CSS_IDENT_CONFIG) }
}

/// Emit a call to the identifier scanner parameterized by the
/// dialect flags lifted from `RegexClass::Identifier`. Picks the
/// right shared `IdentConfig` constant when the flag combination
/// matches one of the two pre-declared configs; otherwise emits an
/// inline `IdentConfig` literal so each grammar's idiosyncratic
/// dialect routes through the same kernel.
pub fn emit_call_with_flags(
    allow_leading_dash: bool,
    allow_double_dash_prefix: bool,
) -> TokenStream {
    match (allow_leading_dash, allow_double_dash_prefix) {
        (false, false) => emit_call(),
        (true, true) => emit_call_css(),
        (al, ad) => {
            let leading = if al { quote! { true } } else { quote! { false } };
            let double = if ad { quote! { true } } else { quote! { false } };
            quote! {
                ::parse_that::scan_ident(
                    state,
                    &::parse_that::IdentConfig {
                        allow_leading_dash: #leading,
                        allow_double_dash_prefix: #double,
                    },
                )
            }
        }
    }
}
