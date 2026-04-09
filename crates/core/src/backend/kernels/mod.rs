//! Tranche V.7 — backend kernel registry.
//!
//! One sub-module per recognizer family. Each module exports a thin
//! `emit_*` function that takes a `RecognizerSignature` (or its
//! constituent fields) and returns a `TokenStream` invoking the
//! relevant `parse_that` primitive.
//!
//! V.7 scope: provides the file home for family-specific emission and
//! routes the three new `RegexClass` variants
//! (`CharClassQuantified`, `PrefixThenClass`, `AccelDriven`) into
//! kernel-style helper calls. Hoisting via signature-hash dedup is
//! plumbed but not yet active — every call site emits inline. The
//! follow-up tranche enables hoisting once the V.6 CSP cost model is
//! tuned.

pub mod balanced_wrap;
pub mod charclass;
pub mod comment_ws;
pub mod identifier;
pub mod number;
pub mod prefix_class;
pub mod quoted_string;
pub mod sep_list;
