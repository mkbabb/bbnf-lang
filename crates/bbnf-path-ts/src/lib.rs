//! TS / WASM binding for the BBNF `path!` macro.
//!
//! AZ-IV.W5.1 — this cdylib is the TS-frontend twin of the Rust
//! `bbnf-path` proc-macro. Where the proc-macro lifts a path literal
//! into a `TypedPath<G, T>` at Rust compile time, this cdylib lifts
//! the same path literal at TS load time (or at build time under
//! constant-folding bundlers — Vite, esbuild, SWC) into the same
//! `TypedPath` shape transported through `serde-wasm-bindgen`.
//!
//! ## Surface
//!
//! Two `#[wasm_bindgen]` exports drive the TS template-tag at
//! `ts/path.ts`:
//!
//! - [`compile_path`] — lex the path string through the same
//!   `bbnf_regex::lex_path` the proc-macro uses, validate the segment
//!   sequence against the per-grammar [`fixture::GrammarFixture`], and
//!   emit a serialized [`schema::TypedPathPayload`]. The payload's
//!   shape is byte-identical to the Rust frontend's
//!   `bbnf::path::TypedPath::owned_segments()` projection so that the
//!   isomorphism test at `tests/isomorphic_path_error.rs` round-trips
//!   the error taxonomy without re-projection.
//! - [`execute_path`] — apply a previously compiled
//!   [`schema::TypedPathPayload`] to a JS-side document view and
//!   return the leaf value. The W5.2 Node-execute proof drives this
//!   entry against `data/json/twitter.json`.
//!
//! ## Isomorphism contract (per `feedback_isomorphic-api`)
//!
//! The TS template-tag produces `TypedPath` payloads bit-equivalent to
//! `bbnf::path::TypedPath::<G, ()>::from_owned(...)`; the
//! [`schema::PathErrorPayload`] surface is byte-identical to
//! `bbnf::path::PathError` so the TS frontend renders the same
//! `{ segment, struct_name, alternatives, reason }` diagnostic the
//! Rust frontend renders. The isomorphism is asserted in
//! `tests/isomorphic_path_error.rs`.
//!
//! ## Architecture caveat
//!
//! `bbnf-path` is a proc-macro crate (`proc-macro = true`); proc-macro
//! crates cannot be path-deps of a non-proc-macro library. The macro's
//! lex / lower / validate logic is therefore mirrored at function
//! scope inside [`compile`]; the per-grammar fixture registry is the
//! same shape carried by `crates/bbnf-path/src/registry.rs`. When T4
//! lands per-grammar `pub const REGISTRY: StructRegistry` in the
//! generated code, both frontends swap their fixture lookup for the
//! production const — the cdylib's public surface stays unchanged.

#![allow(clippy::needless_pass_by_value)]

use wasm_bindgen::prelude::*;

mod compile;
mod fixture;
mod schema;
mod template_tag;

pub use schema::{OwnedSegmentPayload, PathErrorPayload, PathErrorReasonPayload, TypedPathPayload};

/// Native-host entry mirroring [`compile_path`] without the wasm-bindgen
/// transport layer. Used by the Rust-side isomorphism tests (the
/// wasm-bindgen exports cannot be invoked on non-wasm targets — they
/// panic at the imported-function shim).
///
/// The success/failure shape is identical to the wasm-bindgen export:
/// the cdylib serializes the same [`TypedPathPayload`] / [`PathErrorPayload`]
/// values that traverse the JS/WASM boundary, just without the
/// `JsValue` round-trip.
pub fn compile_path_native(
    path_str: &str,
    grammar: &str,
) -> Result<TypedPathPayload, PathErrorPayload> {
    compile::compile(path_str, grammar)
}

/// Native-host entry mirroring [`execute_path`].
pub fn execute_path_native(
    path: &TypedPathPayload,
    document: &serde_json::Value,
) -> serde_json::Value {
    compile::execute(path, document)
}

/// Initialise the WASM panic hook for legible browser-console errors.
/// Optional (`panic-hook` feature); a no-op when the feature is off.
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "panic-hook")]
    console_error_panic_hook::set_once();
}

/// Lift a path string into a [`TypedPathPayload`] validated against
/// the named grammar's compile-time fixture.
///
/// Mirrors `path!(GrammarMarker, ...)` in the Rust frontend. Accepts
/// the path-string forms the proc-macro accepts: `$.field[0].name`,
/// `field.0.name`, `*`, `[*]`, `@variant`. Throws a `JsError` carrying
/// a serialized [`PathErrorPayload`] when the path fails to resolve.
///
/// ```js
/// import { compile_path } from "bbnf-path-ts";
/// const p = compile_path("$.statuses[0].text", "Json");
/// // p has the same shape as the Rust `path!(Json, "statuses", 0, "text")`.
/// ```
#[wasm_bindgen]
pub fn compile_path(path_str: &str, grammar: &str) -> Result<JsValue, JsError> {
    let payload = compile::compile(path_str, grammar)
        .map_err(|err| JsError::new(&serde_json::to_string(&err).unwrap_or_default()))?;
    serde_wasm_bindgen::to_value(&payload).map_err(|e| JsError::new(&e.to_string()))
}

/// Apply a [`TypedPathPayload`] to a JS-side document view and return
/// the leaf value (or `JsValue::null` if the path traverses a missing
/// field). The document view is the parsed-JSON shape the W1 TS
/// backend emits for the same grammar.
///
/// AZ-IV.W5.2 (TS Node Execution Proof) drives this entry through a
/// `cargo nextest` integration test that spawns Node, invokes the
/// W1-emitted TS parser, and asserts identical leaf extraction
/// against `JsonParser::parse(input)?.get(p)` in the Rust frontend.
#[wasm_bindgen]
pub fn execute_path(typed_path: JsValue, document_view: JsValue) -> Result<JsValue, JsError> {
    let path: TypedPathPayload = serde_wasm_bindgen::from_value(typed_path)
        .map_err(|e| JsError::new(&format!("invalid TypedPath payload: {e}")))?;
    let document: serde_json::Value = serde_wasm_bindgen::from_value(document_view)
        .map_err(|e| JsError::new(&format!("invalid document view: {e}")))?;
    let leaf = compile::execute(&path, &document);
    serde_wasm_bindgen::to_value(&leaf).map_err(|e| JsError::new(&e.to_string()))
}

/// Re-export the TS template-tag bridge module so consumers can
/// import the JS shim string at build time.
pub use template_tag::TEMPLATE_TAG_JS;
