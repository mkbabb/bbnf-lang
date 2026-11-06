#![forbid(unsafe_code)]
//! Quarantined v3 proof carrier for the BBNF semantic type kernel.
//!
//! This crate deliberately has no production integration path.  It imports the
//! real `bbnf-ir` boundaries so inference coverage cannot drift behind a toy IR.

pub mod type_kernel;
