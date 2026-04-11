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
//!
//! Each kernel is a **pure** emitter: its inputs are pre-computed by
//! the driver (bytes, `CharSet128`s, TokenStreams, configs from the
//! IR sidecars `ir.delim_scan_configs`, `ir.key_dispatch_configs`,
//! `ir.node_facts`, etc.), and the kernel body only constructs the
//! `TokenStream` to splice in. No kernel reaches into `&GrammarIR`
//! or walks the IR tree — per the Tranche AF.1 invariant, all facts
//! flow from the cache through the caller, and the kernels never
//! recompute anything per-emission.

mod charset_shapes;

pub mod balanced_wrap;
pub mod charclass;
pub mod comment_ws;
pub mod identifier;
pub mod number;
pub mod prefix_class;
pub mod punct_ws_region;
pub mod quoted_string;
