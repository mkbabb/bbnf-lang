//! Rust-backend per-type-name resolution for the payload layout
//! pass.
//!
//! AW.0.5: implements [`bbnf_ir::passes::NamedTypeResolver`] for the
//! Rust backend. The IR crate owns the trait (pass-side); this
//! module owns the Rust-specific name table. Per AU.4.2's stated
//! path — "codegen handles struct projections via per-backend type
//! tables, not a central registry" — each backend carries its own
//! sibling resolver module.
//!
//! Known-name table:
//!
//! | `TypeDesc::Named(sid)` name | Tuple shape                         | Total bytes |
//! |-----------------------------|-------------------------------------|-------------|
//! | `Color`                     | `(U8, F64, F64, F64, F64)`          | 40          |
//! | `ColorMix`                  | `(U8, F64, F64, F64, F64)`          | 40          |
//!
//! The byte layout `[u8 @ 0][7 B pad][f64 c1 @ 8][f64 c2 @ 16]
//! [f64 c3 @ 24][f64 alpha @ 32]` falls out of the planner's natural-
//! alignment arithmetic — no hand-packing arm. Consumers on the
//! view side decode via [`super::color::Color::decode`].
//!
//! TS / WASM backends carry their own sibling resolvers
//! (`crates/core/src/backend/ts/view/named_types.rs`,
//! `crates/core/src/backend/wasm/view/named_types.rs`) when those
//! backends land AW.0.5 parity.

use bbnf_ir::passes::NamedTypeResolver;
use bbnf_ir::{GrammarIR, StringId, TypeDesc};

/// Rust-backend resolver for backend-specific named types.
///
/// Borrows the grammar's string table so [`Self::resolve_named`]
/// can map a `StringId` back to its interned `&str` form and
/// pattern-match on the well-known Rust-side type names.
pub struct RustNamedTypes<'ir> {
    strings: &'ir [String],
}

impl<'ir> RustNamedTypes<'ir> {
    /// Construct the resolver from a borrow of the IR's string table.
    ///
    /// Cheap; the resolver is a thin wrapper over `&[String]` with
    /// no heap allocation. Build once per backend-analysis run and
    /// pass into [`bbnf_ir::passes::compute_payload_layouts_with_resolver`].
    #[inline]
    pub fn from_ir(ir: &'ir GrammarIR) -> Self {
        Self { strings: &ir.strings }
    }
}

impl<'ir> NamedTypeResolver for RustNamedTypes<'ir> {
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc> {
        let name = self.strings.get(sid as usize).map(String::as_str)?;
        match name {
            // CSS L4 colour-function typed projection.
            // `colorFunction`, `colorFn`, and `colorMix` rules
            // declare `-> input : Color` / `: ColorMix` in
            // `grammar/css/l4/color.bbnf`; the planner admits the
            // tuple shape at `LARGE_PAYLOAD_MAX` and the emitter's
            // `aggregate_payload_ctor` routes through
            // `PayloadData::LargeAggregate`. The view-layer decoder
            // in `super::color::Color::decode` reconstructs the
            // typed struct from the 40 B blob.
            //
            // Discriminant encoding (`ColorSpace`):
            //   0 = Rgb       4 = Hwb       8 = Oklch
            //   1 = Rgba      5 = Lab
            //   2 = Hsl       6 = Lch
            //   3 = Hsla      7 = Oklab
            //
            // `alpha` may be `f64::NAN` when the input carried no
            // alpha channel (the emitter's alpha-less arm writes
            // `NAN.to_le_bytes()` into the slot). Consumers
            // distinguish NaN via `Color::alpha.is_nan()` and pick
            // a default or propagate the absence.
            "Color" | "ColorMix" => Some(TypeDesc::Tuple(vec![
                TypeDesc::U8,
                TypeDesc::F64,
                TypeDesc::F64,
                TypeDesc::F64,
                TypeDesc::F64,
            ])),
            _ => None,
        }
    }
}
