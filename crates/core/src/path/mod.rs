//! Compile-time-typed path IR.
//!
//! AZ-IV.W2 lands the typed-path surface that drives the `path!`
//! proc-macro, the IR `path_check` pass, the lazy bail-out parse
//! recognizer, and the TS template-literal binding.
//!
//! Sub-units:
//!
//! - [`ir`] — `Path<'a>`, `PathSegment<'a>`, `TypedPath<G, T>`,
//!   `OwnedPathSegment`, `IntoPathSegment` (W2.1 — landed).
//! - [`error`] — `PathError`, `PathErrorReason` (W2.1 — landed).
//! - [`type_check`] — offline `check_path_against_registry` entry
//!   point (W2.1 — landed).
//! - [`markers`] — grammar marker ZSTs `Json` / `CssL4` / `Sheets` /
//!   `Bbnf` (W2.1 — landed).
//! - `ascent` — `AscentStrategy` trait + reversal seam (W2.5 — not
//!   yet landed).
//! - `wildcard` — wildcard lazy-iter execution (W2.5 — not yet
//!   landed).
//! - `variant_select` — typed-enum variant resolver (W2.5 — not yet
//!   landed).
//!
//! The existing borrowed `runtime::path::Path<'a>` stays in place per
//! the W2 modify-carve rule; this module is the typed-path surface
//! sibling. They share the `IntoPathSegment` shape but live in
//! disjoint modules so the typed surface can evolve without touching
//! the path-alphabet borrowing surface.

pub mod error;
pub mod ir;
pub mod markers;
pub mod type_check;

pub use error::{PathError, PathErrorReason};
pub use ir::{IntoPathSegment, OwnedPathSegment, Path, PathSegment, TypedPath};
pub use markers::{Bbnf, CssL4, Json, Sheets};
pub use type_check::{check_path, check_path_against_registry};
