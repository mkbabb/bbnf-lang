//! AZ-IV.W3.7 — CSS L4 `parse_with` entry point (cursor-threaded).
//!
//! Lazy bail-out parse surface for the CSS Selectors / Values / Color
//! / Animations Level 4 grammar. Constructs a [`PathCursor`] over a
//! [`PathSchema`] (today only [`TypedPath<CssL4, T>`](crate::path::TypedPath)),
//! wires the cursor's decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and threads the cursor through
//! the cursor-aware generated dispatcher
//! [`parse_CssL4Parser_stylesheet`](crate::grammar::generated::css_l4::parse_CssL4Parser_stylesheet).
//! Subtrees the path does not visit are byte-skipped and never push
//! records into the [`CssStructBuilder`]. After the dispatcher returns,
//! the builder is finalised against `input` and the leaf is projected
//! through [`CssDocument::get`].
//!
//! See [`crate::runtime::json::parse_with`] for the lazy-error-elision
//! contract; the CSS surface mirrors it. `VariantName` lowers to a
//! `Field` step against the document walker (the W4 typed-step
//! executor will replace this with a typed-enum step).

use super::document::{CssDocument, CssPathQuery};
use crate::grammar::generated::css_l4::{
    __path_plan, __shape_support_CssL4Parser, parse_CssL4Parser_stylesheet,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::CssL4;
use crate::runtime::css_l4::CssStructBuilder;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` returns `None` — the
/// document projection cannot materialise a wildcard iter.
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
/// [`CssPathQuery`] trait. Returns `None` when the parse fails
/// inside the path's reach, the path falls outside the document, or
/// any segment fails to resolve. Parse errors past the path's reach
/// are silently elided — the lazy contract.
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
        |src, cursor| {
            let mut state = __shape_support_CssL4Parser::ScanState::new();
            let mut builder = CssStructBuilder::new();
            let mut pos: usize = 0;
            parse_CssL4Parser_stylesheet(
                src.as_bytes(),
                &mut pos,
                &mut state,
                &mut builder,
                cursor,
            )
            .ok()?;
            let doc: CssDocument<'_> = builder.finalise(src);
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

    /// Smoke: `parse_with` resolves a property &str leaf on a
    /// well-formed CSS rule. Parity against the eager lane is
    /// exercised cross-crate in `crates/core/tests/parse_with_css_l4.rs`
    /// to keep this production module free of any eager-parse call
    /// site (Hard Gate 15).
    #[test]
    fn parse_with_resolves_property_leaf() {
        let src = "a { color: red; }";
        // [Index(0), Index(0)] → rule[0] → decl[0] → property &str.
        let path: TypedPath<CssL4, &str> =
            TypedPath::from_owned(vec![OwnedPathSegment::Index(0), OwnedPathSegment::Index(0)]);
        let lazy = parse_with::<&str>(src, &path);
        assert!(lazy.is_some(), "first declaration's property resolves");
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
