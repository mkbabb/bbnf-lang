//! AZ-IV.W1.8 — `crate::backend::rust::emitter::shapes::substrate`
//! token-stream resolution tests.
//!
//! Relocated from the inline `#[cfg(test)] mod tests { ... }` block in
//! `crates/core/src/backend/rust/emitter/shapes/substrate.rs` per
//! `feedback_no-inline-tests` (every `#[cfg(test)]` in `src/` moves
//! to `tests/`). The helpers exercise the public surface
//! ([`builder_path`], [`builder_ty_with_lifetime`]) — the
//! `substrate_path` helper is reachable through them.
//!
//! These cover:
//! - JSON binding resolves a `JsonStructBuilder` token-path (regression
//!   for the `JsonStructBuilder` fallback that was deleted at AZ-IV
//!   W1.8 / Fermat F4 — every grammar must reach its own substrate).
//! - CSS L4 binding resolves to `CssStructBuilder`.
//! - `builder_ty_with_lifetime` emits a `'p` lifetime parameter
//!   (regression for the AZ-I.W2-act gap #1: `quote!`-splicing a bare
//!   `Ident` produced `<p>`, a generic-type argument, not `<'p>`).

use bbnf::backend::rust::emitter::shapes::substrate::{builder_path, builder_ty_with_lifetime};
use bbnf_ir::registry::{EmitStrategy, SubstrateBinding};

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
