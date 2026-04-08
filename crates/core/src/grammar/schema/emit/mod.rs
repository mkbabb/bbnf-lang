//! Per-target schema emitters.
//!
//! Mirrors the repo's `backend/{rust,ts,wasm}/` per-target emitter idiom,
//! but for the *frontend* CST helpers (accessors, walkers, directive
//! extractors). Each emitter takes a `CstSchema` and produces target-specific
//! source code or runtime metadata.
//!
//! - `rust` — full parity with the v1 visitor codegen (current target)
//! - `ts` — placeholder, contract fixed
//! - `runtime` — placeholder, runtime metadata tables for WASM/introspection

pub mod rust;
pub mod runtime;
pub mod ts;
