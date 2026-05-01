//! AZ-II.cutover.E (Phase 2) — `CsvDocument` + view / value / path
//! accessor surface.
//!
//! Mirrors [`crate::runtime::bbnf::BbnfDocument`] /
//! [`crate::runtime::json::JsonDocument`] /
//! [`crate::runtime::google_sheets::SheetsDocument`].
//!
//! The struct-direct CSV parse path returns a [`CsvDocument`] whose
//! root [`CsvValue`] borrows from the input lifetime `'p` and whose
//! [`CsvArena`] owns every compound child slice.

use crate::runtime::csv::arena::{CsvArena, CsvCompound, CsvCompoundId, CsvCompoundKind};
use crate::runtime::csv::value::CsvValue;
use crate::runtime::path::{Path, PathSegment};

/// Discriminator over the typed shapes a [`CsvValue`] takes.
///
/// Mirrors `JsonKind` / `SheetsKind` / `BbnfKind` for consumers
/// branching on `view.kind()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CsvKind {
    /// Source-borrowed span leaf (`escaped`, `textdata`, `field`
    /// projection).
    Span,
    /// Unit-typed leaf.
    Unit,
    /// Compound rule (`record` / `csv`).
    Compound,
}

/// The root document returned by `bbnf::grammar::generated::csv::CsvParser::parse`
/// (post-Phase 2 regen).
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
#[derive(Debug)]
pub struct CsvDocument<'p> {
    /// The compound child arena — owns every compound entry the
    /// document references via handles.
    pub arena: CsvArena<'p>,
    /// The root value of the document.
    pub root: CsvValue<'p>,
    /// The input slice the parse consumed.
    pub input: &'p str,
}

impl<'p> CsvDocument<'p> {
    /// Construct a document from a populated arena, root value, and
    /// the input slice.
    #[inline]
    pub fn new(arena: CsvArena<'p>, root: CsvValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`CsvValue`].
    #[inline]
    pub fn root(&self) -> &CsvValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`CsvArena`].
    #[inline]
    pub fn arena(&self) -> &CsvArena<'p> {
        &self.arena
    }

    /// Borrow the input slice the parse consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a [`CsvCompoundId`] handle to its compound entry.
    #[inline]
    pub fn compound(&self, id: CsvCompoundId) -> &CsvCompound<'p> {
        self.arena.compound(id)
    }

    /// Yield a [`CsvView`] focused on the document root.
    #[inline]
    pub fn view<'a>(&'a self) -> CsvView<'a, 'p> {
        CsvView {
            doc: self,
            focus: self.root,
        }
    }

    /// Borrow the root value.
    #[inline]
    pub fn to_value(&self) -> &CsvValue<'p> {
        &self.root
    }

    /// Typed path query.
    #[inline]
    pub fn get<T: CsvPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

/// A thin newtype over `&CsvDocument` exposing the focused-view
/// surface. Mirrors [`crate::runtime::bbnf::BbnfView`] /
/// [`crate::runtime::json::JsonView`].
#[derive(Debug, Clone, Copy)]
pub struct CsvView<'a, 'p: 'a> {
    pub(crate) doc: &'a CsvDocument<'p>,
    pub(crate) focus: CsvValue<'p>,
}

impl<'a, 'p: 'a> CsvView<'a, 'p> {
    /// Construct a view focused on a specific [`CsvValue`].
    #[inline]
    pub fn focused(doc: &'a CsvDocument<'p>, focus: CsvValue<'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a CsvDocument<'p> {
        self.doc
    }

    /// The focused [`CsvValue`].
    #[inline]
    pub fn focus(&self) -> CsvValue<'p> {
        self.focus
    }

    /// Borrow the root [`CsvValue`].
    #[inline]
    pub fn root(&self) -> &'a CsvValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a CsvArena<'p> {
        &self.doc.arena
    }

    /// Resolve a compound handle through the document's arena.
    #[inline]
    pub fn compound(&self, id: CsvCompoundId) -> &'a CsvCompound<'p> {
        self.doc.compound(id)
    }

    /// Discriminator over the focused value's typed shape.
    #[inline]
    pub fn kind(&self) -> CsvKind {
        match &self.focus {
            CsvValue::Span(_) => CsvKind::Span,
            CsvValue::Unit => CsvKind::Unit,
            CsvValue::Compound(_) => CsvKind::Compound,
        }
    }

    /// `true` iff the focused value is a compound.
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, CsvValue::Compound(_))
    }

    /// `true` iff the focused value is a span (borrowed source slice).
    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, CsvValue::Span(_))
    }

    /// Borrow the document's input slice.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }

    /// Compound-kind discriminator accessor — returns the compound's
    /// [`CsvCompoundKind`] for compound focuses, `None` for leaves.
    #[inline]
    pub fn compound_kind(&self) -> Option<CsvCompoundKind> {
        match self.focus {
            CsvValue::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }
    }
}

/// Typed path-query trait, mirroring AY's `runtime::PathQuery<T>` for
/// the CSV struct-direct surface.
pub trait CsvPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any path segment fails to match.
    fn query<'p>(doc: &CsvDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk the document's compound tree following `path` from `root`,
/// returning the resolved [`CsvValue`] reference (or `None` on
/// missing field / out-of-range index).
#[inline]
fn walk_path<'a, 'p>(doc: &'a CsvDocument<'p>, path: Path<'_>) -> Option<&'a CsvValue<'p>> {
    let mut current: &'a CsvValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (CsvValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            // CSV does not carry named fields; field-keyed access is
            // unsupported. Index queries are the only navigation.
            (CsvValue::Compound(_), PathSegment::Field(_)) => return None,
            // Any step against a leaf scalar: type mismatch.
            _ => return None,
        };
    }
    Some(current)
}

impl CsvPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &CsvDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            CsvValue::Span(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl CsvPathQuery for CsvValue<'_> {
    #[inline]
    fn query<'p>(doc: &CsvDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: CsvValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<CsvValue<'p>, CsvValue<'_>>(copied) })
    }
}
