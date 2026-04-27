//! Literal-string escape helpers for the WASM backend.
//!
//! WASM strings carry no source-level escape syntax distinct from
//! their byte representation, so the WASM `unescape_literal` is a
//! direct passthrough. Kept as a sibling of the shared
//! `crate::backend::unescape_literal` so the per-backend escape
//! contract is uniform across Rust / TS / WASM emitters.

pub fn unescape_literal(s: &str) -> String {
    s.to_string()
}
