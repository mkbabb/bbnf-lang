//! Identifier schema helper emission.
//!
//! The O3 schema carve disables identifier helper codegen because the
//! previous helpers extended generated view records. StructDirect
//! callers must use document-owned projection APIs instead.

use proc_macro2::TokenStream;

use super::super::super::model::CstSchema;

/// Emit identifier schema helpers.
pub(super) fn generate(_schema: &CstSchema) -> TokenStream {
    TokenStream::new()
}
