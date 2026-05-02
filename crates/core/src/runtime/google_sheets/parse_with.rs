//! AZ-IV.W3.2 — Google Sheets `parse_with` entry point.
//!
//! Lazy bail-out parse surface for the Google Sheets formula grammar.
//! Constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Sheets, T>`](crate::path::TypedPath)), wires the
//! cursor's decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and hands the cursor to
//! [`PathExecutor::execute`]. The executor's parse-fn closure runs the
//! existing eager [`GoogleSheetsParser::parse`] and projects the
//! path's leaf through [`SheetsDocument::get`] using the
//! `SheetsPathQuery` trait family the document already owns.
//!
//! Sheets compounds are positional, so the cursor sees `Index` steps
//! exclusively in well-formed paths; `Field` and `VariantName` steps
//! against a Sheets compound resolve to `None` per
//! `SheetsPathQuery::query`.

use super::document::{SheetsDocument, SheetsPathQuery};
use crate::grammar::generated::google_sheets::{__path_plan, GoogleSheetsParser};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Sheets;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` returns `None`.
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

/// Run a path-driven Sheets parse and project the leaf at `path`.
///
/// `T` is the leaf type; the bound is the document-owned
/// [`SheetsPathQuery`] trait.
pub fn parse_with<T>(input: &str, path: &TypedPath<Sheets, T>) -> Option<T>
where
    T: SheetsPathQuery,
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
            let doc: SheetsDocument<'_> = GoogleSheetsParser::parse(src).ok()?;
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

    /// Lazy + eager parity: `parse_with(input, &path)` matches
    /// `parse(input)?.get(legacy_path)` for the same path.
    #[test]
    fn parse_with_parity_against_eager() {
        // `=42` parses as a formula whose first compound child is the
        // number primitive. Index-step paths walk the compound tree.
        let src = "=42";
        let path: TypedPath<Sheets, f64> =
            TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);
        let lazy = parse_with::<f64>(src, &path);

        let doc = GoogleSheetsParser::parse(src).expect("Sheets parse");
        let legacy = [LegacySegment::Index(0), LegacySegment::Index(0)];
        let eager = doc.get::<f64>(LegacyPath::new(&legacy));

        assert_eq!(lazy, eager, "lazy + eager same Option<f64> semantics");
    }

    #[test]
    fn parse_with_returns_none_on_invalid_input() {
        let path: TypedPath<Sheets, f64> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<f64>("not a formula @@@", &path);
        assert!(out.is_none());
    }
}
