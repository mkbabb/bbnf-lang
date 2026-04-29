//! Rust schema emitter.
//!
//! O3 removes schema-emitted view helpers from the generated parser
//! surface. The schema helper bundle used to append identifier and
//! directive accessors that depended on generated view records, but
//! StructDirect parsers now expose document-owned projection APIs
//! instead. Keep the entry point so the surrounding generator does not
//! need a signature change during this carve.

mod directives;
mod identifiers;

use std::collections::HashSet;

use bbnf_ir::RuleId;
use proc_macro2::TokenStream;

use super::super::model::CstSchema;

/// Generate schema-specific Rust helpers.
///
/// The O3 schema carve intentionally emits nothing: generated
/// StructDirect parsers must not receive schema helper APIs that
/// preserve the retired generated view surface. The submodule calls
/// remain so any later strategy-aware reintroduction happens at the
/// same module boundary instead of at the top-level generator.
pub fn generate(schema: &CstSchema, _fused_number_rules: &HashSet<RuleId>) -> TokenStream {
    drop(identifiers::generate(schema));
    drop(directives::generate_module(schema));
    TokenStream::new()
}
