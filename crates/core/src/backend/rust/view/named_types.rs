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
//! ## AW-III.W6.4 — universal resolver table
//!
//! Pre-W6.4 the resolver's match arm hardcoded `"Color" | "ColorMix"`
//! to one tuple shape. W6.4 extracts the binding table into a
//! `static &[NamedTypeBinding]` slice so every named type a
//! production grammar declares reaches the same admission path.
//! Adding a new aggregate payload — JSON `Value` tree fields, BBNF
//! `RuleEntry` / `RhsNode` variants, Sheets `Expr` / `Literal` /
//! `Cell` / `FnCall` variants — is a two-line append at the
//! `BINDINGS` table; no ad-hoc pattern matches, no per-grammar
//! conditional branches.
//!
//! Known-name table (data, not code):
//!
//! | name       | rust field tuple                   | bytes |
//! |------------|-------------------------------------|-------|
//! | `Color`    | `(U8, F64, F64, F64, F64)`          | 40    |
//! | `ColorMix` | `(U8, F64, F64, F64, F64)`          | 40    |
//!
//! The byte layout `[u8 @ 0][7 B pad][f64 c1 @ 8][f64 c2 @ 16]
//! [f64 c3 @ 24][f64 alpha @ 32]` falls out of the planner's natural-
//! alignment arithmetic. Consumers on the view side decode via
//! [`super::color::Color::decode`].
//!
//! TS / WASM backends carry their own sibling resolvers
//! (`crates/core/src/backend/ts/view/named_types.rs`,
//! `crates/core/src/backend/wasm/view/named_types.rs`) when those
//! backends land AW.0.5 parity. The shape of the binding table
//! mirrors the Rust one.

use bbnf_ir::passes::NamedTypeResolver;
use bbnf_ir::{GrammarIR, StringId, TypeDesc};

/// AW-III.W6.4 — one row in the universal named-type binding table.
///
/// Each entry pins a grammar-level name to the concrete scalar-tuple
/// shape the Rust backend admits for it. The `name` match is
/// case-sensitive and matches the StringId-interned text emitted by
/// the BBNF lowering pass (`-> input : <Name>`).
///
/// The struct is `#[derive(Clone, Copy)]` so table rows can be
/// pointer-embedded into `static` context and passed through
/// `resolve_named_type` without allocation.
#[derive(Clone, Copy)]
pub struct NamedTypeBinding {
    /// Interned grammar-level name (matched case-sensitively against
    /// the `GrammarIR` string table).
    pub name: &'static str,
    /// Concrete scalar-tuple shape the planner admits for this name.
    /// Returned to the layout pass as a `TypeDesc::Tuple(fields)`.
    pub fields: &'static [TypeDesc],
}

/// AW-III.W6.4 — canonical Color / ColorMix field tuple
/// `(U8, F64, F64, F64, F64)`.
///
/// Factored into a named constant so future colour-like bindings
/// (e.g. lightningcss's per-channel aggregate projections) reuse the
/// same shape without re-declaring the primitive list inline.
const COLOR_FIELDS: &[TypeDesc] = &[
    TypeDesc::U8,
    TypeDesc::F64,
    TypeDesc::F64,
    TypeDesc::F64,
    TypeDesc::F64,
];

/// AW-III.W6.4 — universal Rust-backend binding table.
///
/// Append a row here to admit a new named type at the planner's
/// aggregate-payload seam. Every entry resolves to a
/// `TypeDesc::Tuple(fields)` during layout admission; no ad-hoc
/// variant dispatch is emitted downstream.
///
/// Current rows mirror the AW.0.5 colour-function tuple (CSS L4
/// `colorFunction`, `colorFn`, `colorMix`). Rows for BBNF, JSON, and
/// Sheets named aggregates land when those grammars adopt the
/// `-> input : <Name>` projection form (AU.6.7 — arena-carried
/// decoded structs).
pub static BINDINGS: &[NamedTypeBinding] = &[
    NamedTypeBinding {
        name: "Color",
        fields: COLOR_FIELDS,
    },
    NamedTypeBinding {
        name: "ColorMix",
        fields: COLOR_FIELDS,
    },
];

/// AW-III.W6.4 — universal name-to-binding lookup.
///
/// Takes an already-resolved `TypeDesc` (mined by upstream passes
/// from `-> input : <Name>` annotations) and returns the concrete
/// binding row that admits it. Returns `None` for every other
/// `TypeDesc` variant — the layout pass then falls through to the
/// structural compound form.
///
/// The API accepts `&TypeDesc` plus the interned string table so
/// future callers on the TS / WASM side (carrying their own
/// [`NamedTypeResolver`] impl) mirror this signature without
/// reaching into the Rust-backend's private state.
#[inline]
pub fn resolve_named_type<'a>(
    type_desc: &TypeDesc,
    strings: &[String],
) -> Option<&'a NamedTypeBinding> {
    let sid = match type_desc {
        TypeDesc::Named(sid) => *sid,
        _ => return None,
    };
    let name = strings.get(sid as usize)?;
    BINDINGS.iter().find(|b| b.name == name.as_str())
}

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

    /// Borrow the underlying string table. Exposed so callers that
    /// hold a [`RustNamedTypes`] can drive [`resolve_named_type`]
    /// directly with their own `TypeDesc`.
    #[inline]
    pub fn strings(&self) -> &'ir [String] {
        self.strings
    }
}

impl<'ir> NamedTypeResolver for RustNamedTypes<'ir> {
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc> {
        // AW-III.W6.4: delegate to the universal binding table.
        // The legacy `match` over `"Color" | "ColorMix"` folded into
        // the data-driven lookup below; the only per-backend knob is
        // which rows populate `BINDINGS`.
        //
        // Discriminant encoding for every resolved name is pinned by
        // the grammar's own `-> Nu8` annotations — e.g. `Color` /
        // `ColorMix` use `ColorSpace` (see
        // `crates/core/src/backend/rust/view/color.rs`). Consumers
        // apply their own discriminant policy above the raw tuple.
        //
        // `alpha` may be `f64::NAN` when the input carried no alpha
        // channel (the emitter's alpha-less arm writes
        // `NAN.to_le_bytes()` into the slot). Consumers distinguish
        // via `<Color>::alpha.is_nan()`.
        let name = self.strings.get(sid as usize).map(String::as_str)?;
        BINDINGS
            .iter()
            .find(|b| b.name == name)
            .map(|b| TypeDesc::Tuple(b.fields.to_vec()))
    }
}
