//! Compile-time-typed path IR.
//!
//! AZ-IV.W2 lands the typed-path surface that drives the `path!`
//! proc-macro, the IR `path_check` pass, the lazy bail-out parse
//! recognizer, and the TS template-literal binding. AZ-IV.W3.1 layers
//! the executor surface on top: [`schema::PathSchema`],
//! [`cursor::PathCursor`], and [`executor::PathExecutor`] are the
//! lazy-bail-out runtime the per-grammar `parse_with` entry points
//! (W3.2) thread through.
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
//! - [`ascent`] — `AscentStrategy` trait + three impls + reversal
//!   seam (W2.5 — landed).
//! - [`wildcard`] — wildcard lazy-iter execution + depth cap (W2.5 —
//!   landed).
//! - [`variant_select`] — typed-enum variant resolver (W2.5 — landed).
//! - [`schema`] — `PathSchema` trait abstracting over `TypedPath`
//!   and future dynamic paths (W3.1 — landed).
//! - [`cursor`] — `PathCursor` state machine + `Decision` /
//!   `SegmentKind` alphabet (W3.1 — landed).
//! - [`executor`] — `PathExecutor` top-level orchestrator (W3.1 —
//!   landed).
//!
//! The existing borrowed `runtime::path::Path<'a>` stays in place per
//! the W2 modify-carve rule; this module is the typed-path surface
//! sibling. They share the `IntoPathSegment` shape but live in
//! disjoint modules so the typed surface can evolve without touching
//! the path-alphabet borrowing surface.

pub mod ascent;
pub mod cursor;
pub mod error;
pub mod executor;
pub mod ir;
pub mod markers;
pub mod schema;
pub mod type_check;
pub mod variant_select;
pub mod wildcard;

pub use ascent::{AscentStrategy, DefaultAscent, HybridSidecar, InStructPointer, RootTraversal};
pub use cursor::{Decision, PathCursor, SegmentKind};
pub use error::{PathError, PathErrorReason};
pub use executor::PathExecutor;
pub use ir::{IntoPathSegment, OwnedPathSegment, Path, PathSegment, TypedPath};
pub use markers::{Bbnf, CssL4, Json, Sheets};
pub use schema::{GrammarMarker, PathSchema};
pub use type_check::{check_path, check_path_against_registry};
pub use variant_select::select_variant;
pub use wildcard::{
    DEFAULT_WILDCARD_DEPTH_CAP, WildcardConfig, WildcardIter, WithAnchors, ends_with_wildcard,
};
