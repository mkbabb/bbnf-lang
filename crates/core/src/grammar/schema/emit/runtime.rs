//! Runtime metadata schema emitter — placeholder.
//!
//! Contract (fixed; implementation deferred):
//! - compact metadata tables (variant tags, field roles, traversal order,
//!   transparent-child edges, directive field maps)
//! - consumed by the WASM backend or runtime introspection callers
//!
//! When implemented, this module will serialize the schema into a compact,
//! versioned binary blob (likely MessagePack), letting WASM consumers walk
//! the CST without re-deriving structure.

use super::super::model::CstSchema;

/// Serialize a `CstSchema` to a runtime metadata blob.
///
/// **Not yet implemented.** Returns an empty `Vec<u8>`. The contract is
/// fixed so the WASM backend can wire this in once it gains a CST consumer.
pub fn generate(_schema: &CstSchema) -> Vec<u8> {
    Vec::new()
}
