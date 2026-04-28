//! AZ-II.cutover.A — `BbnfDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct BBNF parse path returns a [`BbnfDocument`] whose
//! root [`BbnfValue`] borrows from the input lifetime `'p` and whose
//! [`BbnfArena`] owns every compound child slice. Mirrors
//! [`crate::runtime::json::JsonDocument`] /
//! [`crate::runtime::google_sheets::SheetsDocument`].

use crate::runtime::bbnf::arena::{BbnfArena, BbnfCompound, BbnfCompoundId};
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::path::{Path, PathSegment};

/// Discriminator over the typed shapes a [`BbnfValue`] takes.
///
/// Mirrors `JsonKind` / `SheetsKind` for consumers branching on
/// `view.kind()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BbnfKind {
    /// Integer leaf (`int_lit`).
    Int,
    /// Float leaf (`float_lit`).
    Float,
    /// Boolean leaf (`bool_lit`).
    Bool,
    /// Source-borrowed span leaf (`identifier`, `literal`, etc.).
    Span,
    /// Tagged-enum discriminator.
    Tag,
    /// Unit-typed leaf.
    Unit,
    /// Compound rule.
    Compound,
}

/// The root document returned by `bbnf::grammar::generated::bbnf::BbnfBootstrap::parse`
/// (post-cutover.B regen).
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
#[derive(Debug)]
pub struct BbnfDocument<'p> {
    /// The compound child arena — owns every compound entry the
    /// document references via handles.
    pub arena: BbnfArena<'p>,
    /// The root value of the document.
    pub root: BbnfValue<'p>,
    /// The input slice the parse consumed.
    pub input: &'p str,
}

impl<'p> BbnfDocument<'p> {
    /// Construct a document from a populated arena, root value, and
    /// the input slice.
    #[inline]
    pub fn new(arena: BbnfArena<'p>, root: BbnfValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`BbnfValue`].
    #[inline]
    pub fn root(&self) -> &BbnfValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`BbnfArena`].
    #[inline]
    pub fn arena(&self) -> &BbnfArena<'p> {
        &self.arena
    }

    /// Borrow the input slice the parse consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a [`BbnfCompoundId`] handle to its compound entry.
    #[inline]
    pub fn compound(&self, id: BbnfCompoundId) -> &BbnfCompound<'p> {
        self.arena.compound(id)
    }

    /// Yield a [`BbnfView`] focused on the document root.
    #[inline]
    pub fn view<'a>(&'a self) -> BbnfView<'a, 'p> {
        BbnfView { doc: self, focus: self.root }
    }

    /// Borrow the root value, mirroring `Parsed::to_value()` semantics.
    #[inline]
    pub fn to_value(&self) -> &BbnfValue<'p> {
        &self.root
    }

    /// Typed path query, mirroring `Parsed::get::<T>(path)` semantics.
    #[inline]
    pub fn get<T: BbnfPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

/// A thin newtype over `&BbnfDocument` exposing the focused-view
/// surface. Mirrors [`crate::runtime::json::JsonView`] /
/// [`crate::runtime::google_sheets::SheetsView`].
#[derive(Debug, Clone, Copy)]
pub struct BbnfView<'a, 'p: 'a> {
    pub(crate) doc: &'a BbnfDocument<'p>,
    /// The focused [`BbnfValue`] this view observes. Defaults to
    /// `doc.root` for [`BbnfDocument::view`]; sub-views (produced by
    /// `RuntimeView::children`) yield views with the same `doc` but
    /// a different focus.
    pub(crate) focus: BbnfValue<'p>,
}

impl<'a, 'p: 'a> BbnfView<'a, 'p> {
    /// Construct a view focused on a specific [`BbnfValue`].
    #[inline]
    pub fn focused(doc: &'a BbnfDocument<'p>, focus: BbnfValue<'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a BbnfDocument<'p> {
        self.doc
    }

    /// The focused [`BbnfValue`].
    #[inline]
    pub fn focus(&self) -> BbnfValue<'p> {
        self.focus
    }

    /// Borrow the root [`BbnfValue`].
    #[inline]
    pub fn root(&self) -> &'a BbnfValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a BbnfArena<'p> {
        &self.doc.arena
    }

    /// Resolve a compound handle through the document's arena.
    #[inline]
    pub fn compound(&self, id: BbnfCompoundId) -> &'a BbnfCompound<'p> {
        self.doc.compound(id)
    }

    /// Discriminator over the focused value's typed shape.
    #[inline]
    pub fn kind(&self) -> BbnfKind {
        match &self.focus {
            BbnfValue::Int(_) => BbnfKind::Int,
            BbnfValue::Float(_) => BbnfKind::Float,
            BbnfValue::Bool(_) => BbnfKind::Bool,
            BbnfValue::Span(_) => BbnfKind::Span,
            BbnfValue::Tag(_) => BbnfKind::Tag,
            BbnfValue::Unit => BbnfKind::Unit,
            BbnfValue::Compound(_) => BbnfKind::Compound,
        }
    }

    /// `true` iff the focused value is a compound.
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, BbnfValue::Compound(_))
    }

    /// `true` iff the focused value is a span (borrowed source slice).
    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, BbnfValue::Span(_))
    }

    /// `true` iff the focused value is a numeric leaf.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, BbnfValue::Int(_) | BbnfValue::Float(_))
    }

    /// `true` iff the focused value is a boolean leaf.
    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.focus, BbnfValue::Bool(_))
    }

    /// `true` iff the focused value is a tag-discriminator leaf.
    #[inline]
    pub fn is_tag(&self) -> bool {
        matches!(self.focus, BbnfValue::Tag(_))
    }

    /// `true` iff the focused value is a unit leaf.
    #[inline]
    pub fn is_unit(&self) -> bool {
        matches!(self.focus, BbnfValue::Unit)
    }
}

/// Typed path-query trait, mirroring AY's `runtime::PathQuery<T>` for
/// the BBNF struct-direct surface.
///
/// Implementations cover: `&str` (Span leaves), `i64` (Int leaves),
/// `f64` (Float / Int via `as_f64` widening), `bool` (Bool leaves),
/// `u8` (Tag leaves), and `BbnfValue` (identity).
pub trait BbnfPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any path segment fails to match.
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk the document's compound tree following `path` from `root`,
/// returning the resolved [`BbnfValue`] reference (or `None` on
/// missing field / out-of-range index).
#[inline]
fn walk_path<'a, 'p>(doc: &'a BbnfDocument<'p>, path: Path<'_>) -> Option<&'a BbnfValue<'p>> {
    let mut current: &'a BbnfValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (BbnfValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            // Field-keyed access against a compound: BBNF's compound
            // children are positional (no key/value pair shape), so a
            // `Field` step against a `Compound` resolves to the first
            // child whose adjacent kind discriminator matches the
            // field name. This mirrors the JSON/Sheets pattern of
            // routing field steps through the structural layout.
            (BbnfValue::Compound(_id), PathSegment::Field(_name)) => {
                // BBNF does not carry named-field metadata on compound
                // entries — every rule is positional. Field steps are
                // therefore unsupported at the document level; consumers
                // that need named navigation should use Index steps with
                // the field's positional index.
                return None;
            }
            // Any step against a leaf scalar: type mismatch.
            _ => return None,
        };
    }
    Some(current)
}

impl BbnfPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            BbnfValue::Span(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl BbnfPathQuery for i64 {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            BbnfValue::Int(v) => Some(*v),
            _ => None,
        }
    }
}

impl BbnfPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            BbnfValue::Float(v) => Some(*v),
            BbnfValue::Int(v) => Some(*v as f64),
            _ => None,
        }
    }
}

impl BbnfPathQuery for bool {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            BbnfValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

impl BbnfPathQuery for u8 {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            BbnfValue::Tag(t) => Some(*t),
            _ => None,
        }
    }
}

impl BbnfPathQuery for BbnfValue<'_> {
    #[inline]
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: BbnfValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<BbnfValue<'p>, BbnfValue<'_>>(copied) })
    }
}
