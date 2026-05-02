//! AZ-IV.W3.2 — CSS L4 `parse_with` entry point.
//!
//! Lazy bail-out parse surface for the CSS Selectors / Values / Color
//! / Animations Level 4 grammar. Constructs a [`PathCursor`] over a
//! [`PathSchema`] (today only [`TypedPath<CssL4, T>`](crate::path::TypedPath)),
//! wires the cursor's decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and hands the cursor to
//! [`PathExecutor::execute`]. The executor's parse-fn closure runs the
//! existing eager [`CssL4Parser::parse`] and projects the path's leaf
//! through [`CssDocument::get`] using the `CssPathQuery` trait family
//! the document already owns.
//!
//! See [`crate::runtime::json::parse_with`] for the lazy-vs-eager
//! contract; the CSS surface mirrors it. `VariantName` lowers to a
//! `Field` step against the `CssWalkCursor::Decl` walker (which the
//! W3.3 thread will replace with a typed-enum step).

use super::document::{CssDocument, CssPathQuery};
use crate::grammar::generated::css_l4::{__path_plan, CssL4Parser};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::CssL4;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` returns `None` — eager
/// fallback cannot materialise a wildcard iter.
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

/// Run a path-driven CSS L4 parse and project the leaf at `path`.
///
/// `T` is the leaf type; the bound is the document-owned
/// [`CssPathQuery`] trait. Returns `None` when the parse fails, the
/// path falls outside the document, or any segment fails to resolve.
pub fn parse_with<T>(input: &str, path: &TypedPath<CssL4, T>) -> Option<T>
where
    T: CssPathQuery,
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
            let doc: CssDocument<'_> = CssL4Parser::parse(src).ok()?;
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
    /// `parse(input)?.get(legacy_path)` for every path the eager
    /// walker can resolve.
    #[test]
    fn parse_with_parity_against_eager() {
        let src = "a { color: red; }";
        // [Index(0), Index(0)] → rule[0] → decl[0] → property &str.
        let path: TypedPath<CssL4, &str> =
            TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);
        let lazy = parse_with::<&str>(src, &path);

        // Eager-then-walk reference.
        let doc = CssL4Parser::parse(src).expect("CSS parse");
        let legacy = [LegacySegment::Index(0), LegacySegment::Index(0)];
        let eager = doc.get::<&str>(LegacyPath::new(&legacy));

        assert_eq!(lazy, eager, "lazy + eager same Option<T> semantics");
    }

    #[test]
    fn parse_with_returns_none_on_missing_rule() {
        let path: TypedPath<CssL4, &str> = TypedPath::from_owned(vec![
            OwnedPathSegment::Index(99),
            OwnedPathSegment::Index(0),
        ]);
        let out = parse_with::<&str>("a { color: red; }", &path);
        assert!(out.is_none());
    }
}
