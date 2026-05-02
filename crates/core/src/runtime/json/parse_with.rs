//! AZ-IV.W3.7 — JSON `parse_with` entry point (cursor-threaded).
//!
//! Lazy bail-out parse surface for the JSON grammar. The function
//! constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Json, T>`](crate::path::TypedPath)), wires the cursor's
//! decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and threads the cursor through
//! the cursor-aware generated dispatcher
//! [`parse_JsonParser_value`](crate::grammar::generated::json::parse_JsonParser_value).
//! The dispatcher consults the cursor at every shape-decision site
//! (Array / Object / Wrap / Flat / cross-shape Ref); subtrees the path
//! does not visit are byte-skipped and never push records into the
//! [`JsonStructBuilder`]. After the dispatcher returns, the builder is
//! finalised against `input` and the leaf is projected through
//! [`JsonDocument::get`] using the document's
//! [`JsonPathQuery`] trait family.
//!
//! ## Lazy-error-elision contract
//!
//! Bytes the cursor causes the dispatcher to skip never reach a parse
//! error path — that is the contract by construction. Specifically, on
//! a fixture where bytes after the path's reach are malformed, the
//! lazy lane returns `Some(leaf)` (the path resolved before the
//! malformation was observed) while the eager lane returns a parse
//! error. The negative-fixture row in
//! `crates/core/tests/parse_with_json.rs` exercises this directly.
//!
//! ## Path mapping
//!
//! The new typed path alphabet
//! ([`crate::path::ir::PathSegment`]) is a superset of the legacy
//! borrowed path the document's `get` consumes
//! ([`crate::runtime::path::PathSegment`]). The lowering converts:
//!
//! - `Field(s)`        → `runtime::path::PathSegment::Field(s)`
//! - `Index(i)`        → `runtime::path::PathSegment::Index(i)`
//! - `VariantName(s)`  → `runtime::path::PathSegment::Field(s)` (the
//!   document's existing walker treats variant-selection as a field
//!   step against the typed-value sum; full enum-aware variant
//!   resolution lands with the W4 typed-step executor.)
//! - `Wildcard`        → bails (`None`); the document's `get` cannot
//!   materialise a wildcard iter. Lazy wildcard execution belongs to a
//!   future stream-shaped surface.

use super::document::{JsonDocument, JsonPathQuery};
use crate::grammar::generated::json::{
    __path_plan, __shape_support_JsonParser, parse_JsonParser_value,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Json;
use crate::runtime::json::JsonStructBuilder;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` is unrepresentable in the
/// document's projection step and the caller (`parse_with`) bails
/// before reaching the document walker.
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

/// Run a path-driven JSON parse and project the leaf at `path`.
///
/// `T` is the leaf type the path resolves to; today the bound is the
/// document-owned [`JsonPathQuery`] trait, matching the existing
/// `JsonDocument::get` surface. Returns `None` when the parse fails
/// inside the path's reach, the path falls outside the document, or
/// any segment fails to resolve. Parse errors past the path's reach
/// are silently elided — the lazy contract.
pub fn parse_with<T>(input: &str, path: &TypedPath<Json, T>) -> Option<T>
where
    T: JsonPathQuery,
{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        },
        |src, cursor| {
            let mut state = __shape_support_JsonParser::ScanState::new();
            let mut builder = JsonStructBuilder::new();
            let mut pos: usize = 0;
            parse_JsonParser_value(src.as_bytes(), &mut pos, &mut state, &mut builder, cursor)
                .ok()?;
            let doc: JsonDocument<'_> = builder.finalise(src);
            let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                legacy.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(LegacyPath::new(&legacy))
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::OwnedPathSegment;

    #[test]
    fn parse_with_resolves_string_leaf() {
        let path: TypedPath<Json, &str> =
            TypedPath::from_owned(vec![OwnedPathSegment::Field("title".to_owned())]);
        let out = parse_with::<&str>(r#"{"title":"hi"}"#, &path);
        assert_eq!(out, Some("hi"));
    }

    #[test]
    fn parse_with_resolves_number_leaf() {
        let path: TypedPath<Json, f64> =
            TypedPath::from_owned(vec![OwnedPathSegment::Field("count".to_owned())]);
        let out = parse_with::<f64>(r#"{"count":42}"#, &path);
        assert_eq!(out, Some(42.0));
    }

    #[test]
    fn parse_with_returns_none_on_missing_field() {
        let path: TypedPath<Json, &str> =
            TypedPath::from_owned(vec![OwnedPathSegment::Field("absent".to_owned())]);
        let out = parse_with::<&str>(r#"{"title":"hi"}"#, &path);
        assert!(out.is_none());
    }
}
