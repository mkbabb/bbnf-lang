//! Rust backend projection helpers with live source consumers.
//!
//! AZ-II O5 removed the legacy tape-backed generated view surface
//! from this module. Direct-to-struct grammars own their runtime
//! views and path-query traits in `crate::runtime`; this module now
//! only hosts support code still used by backend analysis and the
//! temporary runtime color compatibility export.

pub mod color;
pub mod named_types;
mod peel;

pub use color::{COLOR_PAYLOAD_BYTES, Color, ColorSpace};
pub use named_types::RustNamedTypes;
