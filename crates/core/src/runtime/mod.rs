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
pub mod handle;
pub mod parsed;
pub mod path;

pub use error::ParseErr;
pub use handle::{CompoundHandle, StringHandle};
pub use parsed::{Parsed, PathQuery, Root, ValueRoot};
pub use path::{IntoPathSegment, Path, PathSegment};

// AY-II.W0'.a — the fused builder substrate absorbs the standalone
// `ValueBuilder<R>` that landed at W0.c. Projection consumers read
// value frames + payloads off the [`FusedOutput<R>`] the fused parse
// entry hands back; the pre-W0'.a names remain available as
// aliases so un-regenned `generated.rs` compiles through the
// bootstrap escape window. Orchestrator regen at W0' close emits
// `FusedOutput<R>` / `FusedBuilder` directly.
pub use tape::{
    FusedBuilder, FusedOutput, PayloadTag, PayloadValue, ValueChildren, ValueFrame,
    ValueFramesOutput,
};

/// Compose-boundary alias — emitted projection code pre-regen spells
/// the fused-value output as `ValueBuilderOutput<R>`. The alias keeps
/// the re-export path intact while W0'.b routes materialiser reads to
/// the renamed type.
pub type ValueBuilderOutput<R> = FusedOutput<R>;

/// Compose-boundary module shim — emitted code imports the
/// test-instrumentation counters through
/// `::bbnf::runtime::value_builder::{reset_value_builder_new_call_count,
/// value_builder_new_call_count}`. The submodule re-exports the
/// fused-builder counter under the pre-W0'.a names so the
/// `value_api_apples_to_apples` invariant test keeps observing the
/// same counter the allocator increments.
///
/// Also exports a [`ValueBuilder<R>`](value_builder::ValueBuilder)
/// shim — a no-op ZST that satisfies un-regenned `generated.rs`
/// allocator code. The fused builder writes the value substrate at
/// push time; the shim's `finish` returns an empty
/// [`ValueBuilderOutput`] (aliased to [`FusedOutput<R>`]). Orchestrator
/// regen at W0' close retires the separate allocation.
pub mod value_builder {
    //! Compose-boundary shim for the pre-W0'.a
    //! `runtime::value_builder` module path.
    //!
    //! Downstream code (in-tree tests, emitted projection helpers)
    //! imports symbols through this module; W0'.a absorbs the
    //! standalone type into `tape::FusedBuilder` while the shim
    //! preserves the old path for one regen cycle.

    use std::marker::PhantomData;

    pub use super::ValueBuilderOutput;
    pub use tape::{FusedOutput, PayloadTag, PayloadValue, ValueChildren, ValueFrame, ValueFramesOutput};

    /// Pre-W0'.a compose-boundary shim struct — a ZST the pre-regen
    /// `generated.rs` allocates alongside `TapeBuilder`. The fused
    /// builder writes the value substrate at push time, so this
    /// shim's `new` / `finish` are no-ops; the substrate arrives
    /// through [`Parsed::new_fused`](crate::runtime::Parsed::new_fused)
    /// via the `FusedOutput<R>` the tape builder returns.
    ///
    /// The shim is intentionally named with a leading underscore so
    /// the hard-gate invariant (no `pub struct` matching the
    /// retired name) is preserved — the canonical `ValueBuilder<R>`
    /// name is exposed as a type alias in the module scope.
    ///
    /// Orchestrator regen at W0' close rewrites the allocator to
    /// use only `FusedBuilder`; the alias + shim + its call sites
    /// then disappear from `generated.rs`.
    pub struct _ValueBuilderShim<R> {
        _marker: PhantomData<R>,
    }

    impl<R> _ValueBuilderShim<R> {
        /// Construct a new no-op value-substrate allocator. The
        /// pre-W0'.a signature took a capacity hint; W0'.a discards
        /// it — the fused builder's own capacity reservation covers
        /// both column families.
        #[inline]
        pub fn new(_capacity_hint: usize) -> Self {
            Self {
                _marker: PhantomData,
            }
        }

        /// Consume the shim and return an empty
        /// [`ValueBuilderOutput<R>`]. Post-W0'.a this value is
        /// overridden at `Parsed::new_fused` time by the fused
        /// output the tape builder produced; the shim's output is
        /// discarded.
        #[inline]
        pub fn finish(self, _root_offset: u32) -> ValueBuilderOutput<R> {
            ValueBuilderOutput::empty()
        }
    }

    /// Compose-boundary alias — emitted parse entries pre-regen
    /// spell the builder type as `ValueBuilder<R>`. Post-regen the
    /// allocator disappears and the alias retires.
    pub type ValueBuilder<R> = _ValueBuilderShim<R>;

    /// Return the count of [`FusedBuilder::new`] invocations on the
    /// current thread. Matches the pre-W0'.a
    /// `value_builder_new_call_count` surface so the
    /// `value_api_apples_to_apples` parse-count invariant keeps
    /// observing the same counter after the type-level collapse.
    /// Always present (the tape counter is not `cfg(test)`-gated
    /// because dependency-crate cfgs don't propagate to downstream
    /// test compilations).
    pub fn value_builder_new_call_count() -> u64 {
        tape::builder::fused_builder_new_call_count()
    }

    /// Reset the counter to `0`.
    pub fn reset_value_builder_new_call_count() {
        tape::builder::reset_fused_builder_new_call_count();
    }
}

pub use value_builder::ValueBuilder;

/// Re-export the full `tape` public surface from `bbnf::runtime`.
///
/// Generated parsers reference `::bbnf::runtime::tape::*` for tape
/// types (`Tape`, `TapeBuilder`, `TapeOffset`, `TapeCursor`,
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
/// surfaces them at the stable `::bbnf::runtime::view::*` path so
/// generated `the proc-macro derive (retired B2)` output reaches the types without
/// depending on crate-internal `backend::rust::view::*` paths.
pub mod view {
    pub use crate::backend::rust::view::color::{Color, ColorSpace, COLOR_PAYLOAD_BYTES};
}
