//! Aggregated integration test binary — IR pass behaviour.
//!
//! Each sub-module is a formerly free-standing `tests/*.rs` file; folding
//! them into one binary root cuts rustc/linker invocations without changing
//! test semantics. Add new pass-level tests as additional `mod ...;`
//! declarations here, with the actual test bodies under `tests/passes/`.

mod charset;
mod context_facts;
mod dag_invariant;
mod dag_roundtrip;
mod passes_alias;
mod passes_dispatch;
mod passes_follow;
mod passes_inline;
mod passes_lr;
mod passes_optimize;
mod passes_payload;
mod passes_prefix;
mod passes_prune;
mod passes_span;
mod push_fingerprint;
mod recognizer;
mod regex_first;
