//! TypeScript schema emitter — placeholder.
//!
//! Contract (fixed; implementation deferred):
//! - discriminated-union narrowing helpers
//! - `walkChildren(node, visitor)` direct dispatch
//! - `spanText(node)` / `identifierText(node)` accessors
//! - `as*Directive(node)` typed extractors
//! - fold / lowering scaffold in TypeScript form
//!
//! When implemented, this module will mirror `rust.rs` but emit TypeScript
//! source instead of `proc_macro2::TokenStream`.

use super::super::model::CstSchema;

/// Generate TypeScript CST helpers from a `CstSchema`.
///
/// **Not yet implemented.** Returns an empty string. The contract is fixed
/// so callers can wire this in once the TS backend gains parity with Rust.
pub fn generate(_schema: &CstSchema) -> String {
    String::new()
}
