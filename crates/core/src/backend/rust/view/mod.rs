//! Rust backend projection helpers with live source consumers.
//!
//! Direct-to-struct grammars own their runtime views and path-query
//! traits in `crate::runtime`. This module hosts only the
//! IR-derived named-type resolver still used by the backend analysis
//! driver.

pub mod named_types;

pub use named_types::RustNamedTypes;
