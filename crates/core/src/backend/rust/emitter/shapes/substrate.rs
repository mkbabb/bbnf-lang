//! AZ-I.W2-act.B3 — substrate-binding token-stream helpers.
//!
//! The Rust emitter splices the [`SubstrateBinding`]'s `builder_path`
//! / `document_path` into per-shape struct-direct parse-fn bodies.
//! This module factors the path-string-to-TokenStream conversion so
//! every per-shape emitter under
//! [`crate::backend::rust::emitter::shapes`] reaches the binding
//! through one resolution surface.
//!
//! The grammar-general struct-direct body emits:
//!
//! ```ignore
//! // `<builder_ty>` is `<builder_path>` token-substituted in.
//! pub fn parse_<shape>_<grammar>_<rule><'p>(
//!     input: &'p [u8],
//!     p: &mut usize,
//!     state: &mut <support>::ScanState,
//!     builder: &mut <builder_ty>,
//! ) -> Result<(), DtaError> {
//!     let __layout = StructLayout { … };
//!     <<builder_ty> as StructBuilder>::begin_compound(builder, &__layout);
//!     // …
//! }
//! ```
//!
//! Per `feedback_no-orthogonal-codepaths` there is one resolution
//! function per binding field; the per-shape emitters consume those
//! TokenStreams directly with no further branching.

use bbnf_ir::registry::EmitStrategy;
use proc_macro2::TokenStream;
use quote::quote;

#[cfg(test)]
use bbnf_ir::registry::SubstrateBinding;

/// Resolve the rust-backend builder path token-stream from an
/// [`EmitStrategy`].
///
/// The returned TokenStream is the bare type path (no lifetime
/// parameters). The per-shape emitters splice `<builder_path><'p>` to
/// pin the lifetime where needed.
#[inline]
pub fn builder_path(strategy: &EmitStrategy) -> TokenStream {
    match strategy {
        EmitStrategy::StructDirect { rust, .. } => substrate_path(rust.builder_path),
    }
}

/// Resolve the rust-backend document path token-stream.
///
/// Used by `parse_body` (the per-grammar parse fn entry) to project
/// the builder's `finalise()` return type into the matching grammar-
/// specific document.
#[inline]
pub fn document_path(strategy: &EmitStrategy) -> TokenStream {
    match strategy {
        EmitStrategy::StructDirect { rust, .. } => substrate_path(rust.document_path),
    }
}

/// Convert a fully-qualified type-path string (e.g.
/// `"::bbnf::runtime::css_l4::CssStructBuilder"`) into a
/// [`TokenStream`] usable as a path expression in `quote!`.
///
/// The string is parsed via [`syn::Path`], reusing the host crate's
/// existing parsing infrastructure (the proc-macro retired at B2 but
/// the emitter still pulls in syn for token-stream construction
/// across the codegen surface).
///
/// **AZ-IV.W1.8 (Fermat F4) — hard fail**: a malformed binding string
/// is a manifest-side defect. Per AZ-IV §Hard Gates 19 + Invariants
/// 13 (No silent fallback), the resolver does NOT route a parse
/// failure into a default builder; it `panic!`s with the offending
/// binding so the manifest gate (W0) catches it loudly. The previous
/// `JsonStructBuilder` fallback emitted code that drove JSON's
/// compound family for non-JSON grammars — exactly the substrate the
/// AZ-IV thesis names as a deletion target.
fn substrate_path(path: &'static str) -> TokenStream {
    match syn::parse_str::<syn::Path>(path) {
        Ok(parsed) => quote! { #parsed },
        Err(err) => panic!(
            "substrate_path: invalid binding {path:?}: {err}; the substrate-binding \
             must be a fully-qualified Rust type path (e.g. \
             `crate::runtime::json::JsonStructBuilder`). Per AZ-IV §Hard Gates 19 \
             the manifest gate enforces well-formed paths; a parse failure here \
             means the registry/manifest landed a malformed entry."
        ),
    }
}

/// Resolve the rust-backend builder type with a `'p` lifetime
/// parameter, suitable for splicing as a function-argument type in
/// `quote!`. The result is `<builder_path><'p>`.
///
/// `lifetime` carries the bare ident (e.g. `p`); the helper prepends
/// the leading apostrophe via [`syn::Lifetime`] so the spliced tokens
/// emit a Rust lifetime parameter rather than a generic-type
/// argument. Pre-fix the helper spliced a bare Ident, producing
/// `JsonStructBuilder<p>` and tripping E0107 on every emitted
/// struct-direct parse fn signature.
#[inline]
pub fn builder_ty_with_lifetime(
    strategy: &EmitStrategy,
    lifetime: &proc_macro2::Ident,
) -> TokenStream {
    let path = builder_path(strategy);
    let lt = syn::Lifetime::new(&format!("'{}", lifetime), proc_macro2::Span::call_site());
    quote! { #path<#lt> }
}

/// Resolve the rust-backend builder type with the elided `'_` lifetime
/// parameter, suitable for splicing as a trait-impl source.
///
/// `<builder_ty> as crate::runtime::StructBuilder` — the explicit
/// trait projection lets per-shape emitters call the trait methods
/// without name-resolution drift from the builder's inherent impl.
#[inline]
pub fn builder_ty_elided(strategy: &EmitStrategy) -> TokenStream {
    let path = builder_path(strategy);
    quote! { #path<'_> }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_strategy(builder: &'static str, document: &'static str) -> EmitStrategy {
        EmitStrategy::StructDirect {
            rust: SubstrateBinding {
                builder_path: builder,
                document_path: document,
            },
            ts: None,
            wasm: None,
        }
    }

    #[test]
    fn json_path_resolves() {
        let strategy = make_strategy(
            "::bbnf::runtime::json::JsonStructBuilder",
            "::bbnf::runtime::json::JsonDocument",
        );
        let ts = builder_path(&strategy).to_string();
        assert!(ts.contains("JsonStructBuilder"), "got {}", ts);
    }

    #[test]
    fn css_l4_path_resolves() {
        let strategy = make_strategy(
            "::bbnf::runtime::css_l4::CssStructBuilder",
            "::bbnf::runtime::css_l4::CssDocument",
        );
        let ts = builder_path(&strategy).to_string();
        assert!(ts.contains("CssStructBuilder"), "got {}", ts);
    }

    #[test]
    fn builder_ty_with_lifetime_emits_apostrophe() {
        // AZ-I.W2-act.recovery — gap #1 regression. `quote!` splicing
        // a bare proc_macro2::Ident produces `<p>` (a generic type
        // argument), not `<'p>` (a lifetime parameter). The helper
        // converts the ident through `syn::Lifetime` so the emitted
        // tokens carry the leading apostrophe.
        let strategy = make_strategy(
            "::bbnf::runtime::json::JsonStructBuilder",
            "::bbnf::runtime::json::JsonDocument",
        );
        let lt_ident = proc_macro2::Ident::new("p", proc_macro2::Span::call_site());
        let ts = builder_ty_with_lifetime(&strategy, &lt_ident).to_string();
        assert!(ts.contains("'p"), "expected lifetime apostrophe in {}", ts);
        assert!(
            !ts.contains("< p >") && !ts.contains("<p>"),
            "expected lifetime, not generic type, in {}",
            ts
        );
    }
}
