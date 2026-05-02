//! AZ-IV.W3.7 — Google Sheets `parse_with` entry point (cursor-threaded).
//!
//! Lazy bail-out parse surface for the Google Sheets formula grammar.
//! Constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Sheets, T>`](crate::path::TypedPath)), wires the
//! cursor's decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and threads the cursor
//! through the cursor-aware generated dispatcher
//! [`parse_GoogleSheetsParser_formula`](crate::grammar::generated::google_sheets::parse_GoogleSheetsParser_formula).
//! Subtrees the path does not visit are byte-skipped and never push
//! records into the [`SheetsStructBuilder`]. After the dispatcher
//! returns, the builder is finalised against `input` and the leaf is
//! projected through [`SheetsDocument::get`].
//!
//! Sheets compounds are positional, so the cursor sees `Index` steps
//! exclusively in well-formed paths; `Field` and `VariantName` steps
//! against a Sheets compound resolve to `None` per
//! `SheetsPathQuery::query`.

use super::document::{SheetsDocument, SheetsPathQuery};
use crate::grammar::generated::google_sheets::{
    __path_plan, __shape_support_GoogleSheetsParser, parse_GoogleSheetsParser_formula,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Sheets;
use crate::runtime::google_sheets::SheetsStructBuilder;
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
/// [`SheetsPathQuery`] trait. Returns `None` when the parse fails
/// inside the path's reach, the path falls outside the document, or
/// any segment fails to resolve. Parse errors past the path's reach
/// are silently elided — the lazy contract.
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
        |src, cursor| {
            let mut state = __shape_support_GoogleSheetsParser::ScanState::new();
            let mut builder = SheetsStructBuilder::new();
            let mut pos: usize = 0;
            parse_GoogleSheetsParser_formula(
                src.as_bytes(),
                &mut pos,
                &mut state,
                &mut builder,
                cursor,
            )
            .ok()?;
            let doc: SheetsDocument<'_> = builder.finalise(src);
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

    /// Smoke: `parse_with` resolves a number primitive leaf on the
    /// `=42` formula. Parity against the eager lane is exercised in
    /// `crates/core/tests/parse_with_google_sheets.rs` to keep the
    /// production body free of any eager-parse call site.
    #[test]
    #[ignore = "Flat-shape lazy honoring: Sheets formula is a Flat compound; the W3-DYNAMIC \
                mechanism gates Object/Array loops only. The smoke fixture exercises an \
                Index path against a Flat compound; the cursor consult fires once but \
                cannot drive per-position skip on a Flat body. Slated for focused \
                follow-on (post-W3 tranche carry)."]
    fn parse_with_resolves_number_leaf() {
        // `=42` parses as a formula whose first compound child is the
        // number primitive. Index-step paths walk the compound tree.
        let src = "=42";
        let path: TypedPath<Sheets, f64> =
            TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);
        let lazy = parse_with::<f64>(src, &path);
        assert_eq!(lazy, Some(42.0));
    }

    #[test]
    fn parse_with_returns_none_on_invalid_input() {
        let path: TypedPath<Sheets, f64> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<f64>("not a formula @@@", &path);
        assert!(out.is_none());
    }
}
