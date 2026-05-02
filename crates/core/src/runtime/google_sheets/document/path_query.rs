//! Typed path-query trait for [`super::SheetsDocument`].
//!
//! AZ-I.W2-act.B2 — `SheetsPathQuery` mirrors `JsonPathQuery` for the
//! Sheets surface. Sheets compounds are positional, so the walker
//! uses [`PathSegment::Index`] only; a [`PathSegment::Field`] step
//! against a Sheets compound returns `None`. (Future grammar
//! refinements that expose named fields — e.g. `cell.sheet_prefix`,
//! `cell.cell_ref` — could add field-keyed dispatch by widening this
//! trait without breaking the index path.)

use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::path::{Path, PathSegment};

use super::SheetsDocument;

/// AZ-I.W2-act.B2 — typed path-query trait, mirroring
/// `JsonPathQuery` for the Sheets surface.
///
/// Sheets compounds are positional, so the walker uses
/// [`PathSegment::Index`] only; a [`PathSegment::Field`] step against
/// a Sheets compound returns `None`. (Future grammar refinements that
/// expose named fields — e.g. `cell.sheet_prefix`,
/// `cell.cell_ref` — could add field-keyed dispatch by widening this
/// trait without breaking the index path.)
pub trait SheetsPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any path segment fails to match.
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk the document's compound tree following `path` from `root`,
/// returning the resolved [`SheetsValue`] reference (or `None` on
/// out-of-range index / type mismatch).
#[inline]
fn walk_path<'a, 'p>(doc: &'a SheetsDocument<'p>, path: Path<'_>) -> Option<&'a SheetsValue<'p>> {
    let mut current: &'a SheetsValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (SheetsValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            // Sheets compounds are positional, not keyed. Field steps
            // are unsupported; any other shape (scalar leaves) cannot
            // accept a step.
            _ => return None,
        };
    }
    Some(current)
}

impl SheetsPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Number(n) => Some(*n),
            _ => None,
        }
    }
}

impl SheetsPathQuery for bool {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl SheetsPathQuery for u8 {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            SheetsValue::Tag(t) | SheetsValue::Error(t) => Some(*t),
            SheetsValue::SheetPrefix { tag, .. } => Some(*tag),
            _ => None,
        }
    }
}

impl SheetsPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        match value {
            SheetsValue::String(s)
            | SheetsValue::CellRef(s)
            | SheetsValue::Identifier(s)
            | SheetsValue::SheetPrefix { text: s, .. } => {
                let extended: &'p str = *s;
                // SAFETY: the borrowed `&str` slice lives for `'p`
                // (the document's input lifetime); the trait surface
                // elides the explicit `'p` because `&str` is invariant
                // in lifetime here.
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl SheetsPathQuery for SheetsValue<'_> {
    #[inline]
    fn query<'p>(doc: &SheetsDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        // SAFETY: SheetsValue is Copy and carries a `'p` lifetime
        // that outlives the caller's borrow on `doc`. The transmute
        // re-projects the lifetime to the trait's elided one.
        let copied: SheetsValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<SheetsValue<'p>, SheetsValue<'_>>(copied) })
    }
}
