//! AZ-IV.W3.2 — JSON `parse_with` entry point.
//!
//! Lazy bail-out parse surface for the JSON grammar. The function
//! constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Json, T>`](crate::path::TypedPath)), wires the cursor's
//! decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and hands the cursor to
//! [`PathExecutor::execute`]. The executor's parse-fn closure runs the
//! existing eager [`JsonParser::parse`] and projects the path's leaf
//! through [`JsonDocument::get`] using the `JsonPathQuery` trait
//! family the document already owns.
//!
//! ## Lazy vs. eager today
//!
//! W3.2 lands the entry-point dispatch surface and the cursor wiring;
//! truly-lazy descent through the generated parse functions is the
//! W3.3 follow-on (the codegen plan exists, the cursor consults it,
//! but the parse loop today runs the eager path and re-projects via
//! the document's typed path traits). Same `Option<T>` semantics as
//! `parse(input)?.get(path)`; the cursor carries the decision plan so
//! when W3.3 threads the cursor through `parse_<rule>` the entry point
//! does not change shape.
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
//!   resolution lands with the W3.3 plan + W4 typed-step executor.)
//! - `Wildcard`        → bails (`None`); the eager fallback cannot
//!   materialise a wildcard iter without an additional lane. Lazy
//!   wildcard execution belongs to the cursor-threaded path that
//!   W3.3 wires.

use super::document::{JsonDocument, JsonPathQuery};
use crate::grammar::generated::json::{__path_plan, JsonParser};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Json;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` is unrepresentable in the
/// eager fallback and the caller (`parse_with`) bails before reaching
/// the document walker.
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
/// `JsonDocument::get` surface. Returns `None` when the parse fails,
/// the path falls outside the document, or any segment fails to
/// resolve (per the trait's `Option<T>` contract).
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
        |src, _cursor| {
            let doc: JsonDocument<'_> = JsonParser::parse(src).ok()?;
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
