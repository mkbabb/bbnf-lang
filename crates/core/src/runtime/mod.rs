//! `runtime` — types emitted directly into generated parser code.
//!
//! Tranche AB.2a introduced `Parsed<View>`; Tranche AC.1 adds
//! `ParseErr`. Together they form the public surface area of
//! every generated `Grammar::parse(input)` entry point:
//!
//! ```ignore
//! impl Grammar {
//!     pub fn parse(
//!         input: &str,
//!     ) -> Result<crate::runtime::Parsed<Self>, ParseErr> { ... }
//! }
//! ```
//!
//! `Parsed<Grammar>` is marker-typed over the grammar struct itself;
//! the root view lifetime is lent by `&self` via the `Root` trait's
//! GAT `type View<'tape>` (landing in AC.2 alongside the emitter
//! rewrite).

pub mod error;
pub mod handle;
pub mod parsed;
pub mod path;

pub use error::ParseErr;
pub use handle::{CompoundHandle, StringHandle};
pub use parsed::{Parsed, PathQuery, Root, ValueRoot};
pub use path::{IntoPathSegment, Path, PathSegment};

// B5.W1 — the unified [`tape::Tape<R>`] substrate is the sole write
// + read + projection surface the grammar-emitted parse entry uses.
// Pre-B5.W1 `FusedBuilder` / `FusedOutput<R>` / `ValueFramesOutput<R>`
// triumvirate retired alongside the welded boundary; transitional
// `pub use` aliases keep pre-regen generated code resolving until
// `cargo xtask regen` rewrites every callsite.
#[allow(deprecated)]
pub use tape::{
    FusedBuilder, FusedOutput, PayloadTag, PayloadValue, ValueChildren, ValueFrame,
    ValueFramesOutput,
};

/// Re-export the full `tape` public surface from `bbnf::runtime`.
///
/// Generated parsers reference `crate::runtime::tape::*` for tape
/// types (`Tape`, `FusedBuilder`, `TapeOffset`, `TapeCursor`,
/// `TapeKind`, `TapeBuildError`) so downstream consumers do not need
/// a direct `tape` dependency — `bbnf` already carries it as
/// the substrate for the generated code. This keeps
/// `the proc-macro derive (retired B2)` usage single-dep from the consumer's point
/// of view.
pub use tape;

/// Re-export the `simd-scan` public surface from `bbnf::runtime`.
///
/// The emitted `parse()` body constructs a
/// [`scan::StructuralAlphabet`] from `GRAMMAR_PROFILE` (via
/// `StructuralAlphabet::from_profile`) and calls
/// [`scan::scan_structural`] to build the per-parse
/// [`scan::StructuralIndex`] before invoking the specialised walker.
/// The walker consumes the index via its dual-cursor slot column,
/// turning per-byte dispatch into O(1) cursor jumps.
///
/// AW-III.W5.d — integrates the SIMD pre-pass kernel into the hot
/// path. Pre-W5.d the emitted walker received an empty index; the
/// bitmap kernel never ran.
pub use simd_scan as scan;

/// AW.0.5: typed view-layer projections the generated `.as_color()`
/// shims reference. The Rust-side `Color` struct + `ColorSpace`
/// enum live in the backend's `view/color.rs`; this re-export
/// surfaces them at the stable `crate::runtime::view::*` path so
/// generated `the proc-macro derive (retired B2)` output reaches the types without
/// depending on crate-internal `backend::rust::view::*` paths.
pub mod view {
    pub use crate::backend::rust::view::color::{Color, ColorSpace, COLOR_PAYLOAD_BYTES};
}
