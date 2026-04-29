//! Directive schema helper emission.
//!
//! The O3 schema carve disables directive helper codegen because the
//! previous helpers preserved generated view accessors. StructDirect
//! callers must use document-owned projection APIs instead.

use proc_macro2::TokenStream;

use super::super::super::model::CstSchema;

/// Emit directive schema helpers.
pub(super) fn generate_module(_schema: &CstSchema) -> TokenStream {
    TokenStream::new()
}
