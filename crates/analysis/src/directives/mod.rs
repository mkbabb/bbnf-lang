//! Directive definitions, extraction, validation, and metadata.
//!
//! Each directive type has its own module containing its info struct,
//! extraction logic (from parsed grammar), and validation + semantic token generation.

pub mod debug;
pub mod hints;
pub mod import;
pub mod recover;
pub mod token;
pub mod ws;
