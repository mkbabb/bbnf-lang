//! `runtime` — types emitted directly into generated parser code.
//!
//! Tranche AB.2 introduces the `Parsed<View>` owning parse result
//! type. Future AB phases expand this module with view cursor
//! accessor helpers and the `ParseErr` discriminator once the
//! emitter migration is complete.

pub mod parsed;

pub use parsed::Parsed;
