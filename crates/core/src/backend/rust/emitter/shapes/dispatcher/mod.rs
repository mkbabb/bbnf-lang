//! Dispatcher emitter — top-level `parse_<grammar>_<root>` entry
//! point + the per-grammar shape support module (SIMD whitespace
//! cache, first-byte dispatch helpers).
//!
//! # Role — AW-V.W3.2
//!
//! The dispatcher mirrors the prototype's
//! `json_prototype::parse_json` shape: skip initial whitespace,
//! dispatch on the first byte to the appropriate shape function,
//! verify trailing whitespace. Per-rule recursion threads through the
//! dispatcher (e.g. object's value-position reads dispatch back
//! through the shape dispatcher to land on number / string / bool /
//! null / nested object / array arms).
//!
//! The support module emits per-grammar SIMD scaffolding —
//! `ScanState` (64-byte whitespace bitmap cache, mirroring
//! `json-prototype`'s `src/simd.rs::ScanState`), `skip_space`,
//! `first_quote_or_backslash`, and friends. Emitting these per-grammar
//! (rather than referencing a cross-crate symbol) keeps the hot path
//! free of function-call boundaries — every SIMD helper resolves at
//! link time into `parse_<grammar>_<root>`.
//!
//! Module layout (B5.W3):
//! - [`symbol_composition`] — `parse_<...>` ident generators
//!   (dispatcher and per-shape entry points).
//! - [`support`]             — per-grammar SIMD/scan support module
//!   (`ScanState`, `skip_space*`, `first_quote_or_backslash`,
//!   bitmap kernels, CTNS probe).
//! - [`cross_shape`]         — top-level dispatcher fns
//!   (`emit_dispatcher`) + Alt-byte-dispatch body emission.
//! - [`ref_call`]            — per-Ref value-position routing
//!   (`emit_ref_call_shape`, leading-ws admission analysis,
//!   `collect_value_refs`).
//! - [`scan_policy`]         — grammar-activated structural-scan
//!   admission facts (`lookup_scan_policy`).

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use proc_macro2::Ident;
use quote::format_ident;

mod cross_shape;
mod ref_call;
mod scan_policy;
mod support;
mod symbol_composition;

pub use cross_shape::emit_dispatcher;
pub use ref_call::{collect_value_refs, emit_ref_call_shape};
pub(crate) use scan_policy::{lookup_scan_policy, ScanActivationFlags};
pub use support::emit_support_module;
pub use symbol_composition::{dispatcher_fn_ident, shape_fn_ident};

/// Helper: derive the `__shape_support_<grammar>` module identifier.
/// Kept as a fn so `quote!` interpolation works cleanly.
pub(super) fn support_mod_ident(grammar_suffix: &str) -> Ident {
    format_ident!("__shape_support_{}", grammar_suffix)
}

/// Convert a [`ShapeTag`] into the shape-fn prefix used by the
/// emitter (`object` / `array` / `string` / `number` / `keyword` /
/// `scalar` for W3; `pratt` / `unordered` / `arglist` / `flat` /
/// `wrap` / `hregex` for W4).
pub(super) fn shape_tag_name(tag: ShapeTag) -> &'static str {
    match tag {
        ShapeTag::Object => "object",
        ShapeTag::Array => "array",
        ShapeTag::String => "string",
        ShapeTag::Number => "number",
        ShapeTag::Keyword => "keyword",
        ShapeTag::Scalar => "scalar",
        ShapeTag::Pratt => "pratt",
        ShapeTag::Unordered => "unordered",
        ShapeTag::ArgList => "arglist",
        ShapeTag::Flat => "flat",
        ShapeTag::Wrap => "wrap",
        ShapeTag::HRegex => "hregex",
        ShapeTag::AltDispatch => "altdispatch",
        ShapeTag::None => "unknown",
    }
}
