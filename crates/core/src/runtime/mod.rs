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
//!     ) -> Result<::bbnf::runtime::Parsed<Self>, ParseErr> { ... }
//! }
//! ```
//!
//! `Parsed<Grammar>` is marker-typed over the grammar struct itself;
//! the root view lifetime is lent by `&self` via the `Root` trait's
//! GAT `type View<'tape>` (landing in AC.2 alongside the emitter
//! rewrite).

pub mod error;
pub mod parsed;

pub use error::ParseErr;
pub use parsed::{Parsed, Root};

/// Re-export the full `tape` public surface from `bbnf::runtime`.
///
/// Generated parsers reference `::bbnf::runtime::tape::*` for tape
/// types (`Tape`, `TapeBuilder`, `TapeOffset`, `TapeCursor`,
/// `TapeKind`, `TapeBuildError`) so downstream consumers do not need
/// a direct `tape` dependency — `bbnf` already carries it as
/// the substrate for the generated code. This keeps
/// `#[derive(Parser)]` usage single-dep from the consumer's point
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
/// surfaces them at the stable `::bbnf::runtime::view::*` path so
/// generated `#[derive(Parser)]` output reaches the types without
/// depending on crate-internal `backend::rust::view::*` paths.
pub mod view {
    pub use crate::backend::rust::view::color::{Color, ColorSpace, COLOR_PAYLOAD_BYTES};

    /// AX.W1.B: first-class CSS Value API isomorphic to
    /// `lightningcss::stylesheet::StyleSheet`. See
    /// `docs/tranches/AX/parity/css_divergence.md` for the variant
    /// coverage ledger.
    pub use crate::backend::rust::view::css;
}
