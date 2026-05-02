//! AZ-IV.AUDIT-A.W1 — substrate-path resolution surface tests.
//!
//! Mirrors the contract previously held by an inline `#[cfg(test)] mod
//! tests` block in
//! [`crate::backend::rust::emitter::shapes::substrate`]; per
//! `feedback_no-inline-tests` the verification lands at
//! `crates/core/tests/` so production source files carry production
//! code only.
//!
//! The tests exercise three properties of the substrate-path resolver:
//! 1. Well-formed JSON binding paths parse and survive round-trip.
//! 2. Well-formed CSS L4 binding paths parse and survive round-trip.
//! 3. The lifetime-parameterised builder type emits a Rust lifetime
//!    (`'p`), not a generic-type argument (`<p>`).

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
