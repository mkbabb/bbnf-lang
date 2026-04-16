//! Per-backend named-type resolution for the aggregate payload
//! layout pass.
//!
//! AW.0.5: when a rule projects `TypeDesc::Named(StringId)` (e.g.
//! CSS L4 `colorFunction -> input : Color`), the layout pass needs
//! to know whether that name has a concrete scalar-tuple shape the
//! [`plan_layout`](super::plan_layout) planner can admit.
//!
//! Per AU.4.2's stated path — "codegen handles struct projections
//! via per-backend type tables, not a central registry" — the
//! resolution is **backend-specific**. The Rust backend maps
//! `"Color"` to `(u8, f64, f64, f64, f64)`; the TS/WASM backends
//! carry their own sibling resolvers. IR does not own the name
//! table; IR owns only this trait.
//!
//! The scaffold `StructRegistry` approach from AS.2.3 / AT.6.1 was
//! deleted in AU.4.2 (commit `ab8588a`) after two tranches of zero
//! population. AW.0.5 does NOT reintroduce it; per-backend resolver
//! is the explicit forward path.
//!
//! Callers: [`super::compute_payload_layouts_with_resolver`].
//!
//! Example Rust-backend implementation (lives in
//! `crates/core/src/backend/rust/view/named_types.rs`):
//!
//! ```ignore
//! use bbnf_ir::passes::payload::NamedTypeResolver;
//! use bbnf_ir::{StringId, TypeDesc};
//!
//! pub struct RustNamedTypes<'ir> { strings: &'ir [String] }
//!
//! impl<'ir> NamedTypeResolver for RustNamedTypes<'ir> {
//!     fn resolve_named(&self, sid: StringId) -> Option<TypeDesc> {
//!         match self.strings.get(sid as usize).map(|s| s.as_str())? {
//!             "Color" | "ColorMix" => Some(TypeDesc::Tuple(vec![
//!                 TypeDesc::U8, TypeDesc::F64, TypeDesc::F64,
//!                 TypeDesc::F64, TypeDesc::F64,
//!             ])),
//!             _ => None,
//!         }
//!     }
//! }
//! ```

use crate::types::{StringId, TypeDesc};

/// Resolves a `TypeDesc::Named(sid)` to its concrete structural
/// shape for a specific backend.
///
/// Implementations return `Some(TypeDesc::Tuple(...))` when the
/// backend admits the named type as a scalar tuple layout; `None`
/// for names the backend does not recognise (the layout pass then
/// skips admission — the rule keeps its compound structural form).
///
/// The resolver is invoked during [`super::compute_payload_layouts_with_resolver`]
/// for every rule whose type is `TypeDesc::Named`. It must be cheap
/// to call — the caller walks every rule once. Pure lookups on an
/// `&'static [(name, shape)]` table are the expected shape.
pub trait NamedTypeResolver {
    /// Resolve a named type to its concrete structural shape, or
    /// `None` if the backend does not recognise the name.
    ///
    /// `sid` is an interned `StringId` from the `GrammarIR`'s string
    /// table. Implementations typically resolve `sid` back to `&str`
    /// against the same string table and pattern-match on the
    /// well-known backend names.
    fn resolve_named(&self, sid: StringId) -> Option<TypeDesc>;
}

/// No-op resolver: resolves nothing. Used by the legacy entry point
/// [`super::compute_payload_layouts`] to preserve the pre-AW behaviour
/// (skip `TypeDesc::Named` at admission) for call sites that do not
/// supply a backend resolver yet (VM / TS / WASM compile paths).
pub struct NullResolver;

impl NamedTypeResolver for NullResolver {
    #[inline]
    fn resolve_named(&self, _sid: StringId) -> Option<TypeDesc> {
        None
    }
}
