//! Regex tier audit — diagnoses which emission tier handles each pattern.
//!
//! Thin wrapper around `emit::audit_regex_pattern` for backward compatibility.
//! The canonical audit implementation now lives in `emit/mod.rs`.

pub use super::emit::{RegexTier, audit_regex_pattern};
