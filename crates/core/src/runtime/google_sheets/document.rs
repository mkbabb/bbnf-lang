//! AZ-I.W2-act.B2 — `SheetsDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct Sheets parse path returns a [`SheetsDocument`]
//! whose root [`SheetsValue`] borrows from the input lifetime `'p`
//! and whose [`SheetsArena`] owns every compound child slice. This
//! module wraps the document with the same API the JSON runtime
//! exposes (per W2-act.A's accessor contract): `view`, `to_value`,
//! `get::<T>(path)`.
//!
//! The accessor surface mirrors `JsonDocument`; consumers writing
//! against either path observe a uniform shape across the three
//! data grammars.

use crate::runtime::google_sheets::arena::{SheetsArena, SheetsCompoundId, SheetsCompoundView};
use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::path::{Path, PathSegment};

/// The root document returned by
/// `bbnf::grammar::generated::google_sheets::GoogleSheetsParser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
#[derive(Debug)]
pub struct SheetsDocument<'p> {
    /// The compound child arena — owns every `[SheetsValue]` slice
    /// the document references via handles.
    pub arena: SheetsArena<'p>,
    /// The root value of the document.
    pub root: SheetsValue<'p>,
}

impl<'p> SheetsDocument<'p> {
    /// Construct a document from a populated arena and a root value.
    /// The typical caller is the generated parse function; consumers
    /// outside the emitter rarely build a `SheetsDocument` directly.
    #[inline]
    pub fn new(arena: SheetsArena<'p>, root: SheetsValue<'p>) -> Self {
        Self { arena, root }
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`SheetsArena`].
    #[inline]
    pub fn arena(&self) -> &SheetsArena<'p> {
        &self.arena
    }

    /// Resolve a [`SheetsCompoundId`] handle to the compound entry
    /// (kind + child slice).
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        self.arena.compound(id)
    }

    /// Borrowed root view, mirroring the
    /// `JsonDocument::view()` surface.
    #[inline]
    pub fn view<'a>(&'a self) -> SheetsView<'a, 'p> {
        SheetsView { doc: self }
    }

    /// Borrowed root value, mirroring `JsonDocument::to_value()`
    /// semantics. Where `Parsed::to_value()` projected the tape into
    /// a `<Grammar>Value` enum, the struct-direct path's
    /// [`SheetsDocument`] already carries the typed value tree —
    /// `to_value()` simply lends its root by reference.
    #[inline]
    pub fn to_value(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Typed path query, mirroring `JsonDocument::get::<T>(path)`
    /// semantics.
    ///
    /// The walker descends from `doc.root()` following
    /// [`PathSegment::Index`] steps against
    /// [`SheetsValue::Compound`] child slices. There is no
    /// field-keyed step in Sheets's grammar (compounds are
    /// positional, not keyed); a `PathSegment::Field` step against a
    /// Sheets compound returns `None`.
    #[inline]
    pub fn get<T: SheetsPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

/// AZ-I.W2-act.B2 — a thin newtype over `&SheetsDocument`.
///
/// Mirrors `JsonView`; the two-lifetime parameter shape preserves
/// compositional invariance through the arena's `Vec<SheetsValue<'p>>`
/// owner.
#[derive(Debug, Clone, Copy)]
pub struct SheetsView<'a, 'p: 'a> {
    doc: &'a SheetsDocument<'p>,
}

impl<'a, 'p: 'a> SheetsView<'a, 'p> {
    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a SheetsDocument<'p> {
        self.doc
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &'a SheetsValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a SheetsArena<'p> {
        &self.doc.arena
    }

    /// Resolve a compound handle through the document's arena.
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'a, 'p> {
        self.doc.compound(id)
    }

    /// Discriminator over the root value's typed shape.
    #[inline]
    pub fn kind(&self) -> SheetsKind {
        match &self.doc.root {
            SheetsValue::Number(_) => SheetsKind::Number,
            SheetsValue::String(_) => SheetsKind::String,
            SheetsValue::Bool(_) => SheetsKind::Bool,
            SheetsValue::Error(_) => SheetsKind::Error,
            SheetsValue::CellRef(_) => SheetsKind::CellRef,
            SheetsValue::Identifier(_) => SheetsKind::Identifier,
            SheetsValue::SheetPrefix { .. } => SheetsKind::SheetPrefix,
            SheetsValue::Tag(_) => SheetsKind::Tag,
            SheetsValue::Compound(_) => SheetsKind::Compound,
        }
    }

    /// `true` iff the root is a compound (any non-leaf rule).
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.doc.root, SheetsValue::Compound(_))
    }

    /// `true` iff the root is a number.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.doc.root, SheetsValue::Number(_))
    }

    /// `true` iff the root is a string-shaped leaf (string / cell_ref /
    /// identifier / sheet_prefix text).
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(
            self.doc.root,
            SheetsValue::String(_)
                | SheetsValue::CellRef(_)
                | SheetsValue::Identifier(_)
                | SheetsValue::SheetPrefix { .. }
        )
    }
}

/// Discriminator over the typed shapes a [`SheetsValue`] takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SheetsKind {
    /// `number = /…/ -> f64`.
    Number,
    /// `string = /"…"/`.
    String,
    /// `boolean = /TRUE/i | /FALSE/i`.
    Bool,
    /// `error_literal = "#N/A" -> 0u8 | …`.
    Error,
    /// `cell_ref = /…/`.
    CellRef,
    /// `identifier = /…/`.
    Identifier,
    /// `sheet_prefix` projection.
    SheetPrefix,
    /// Operator-tag projection (`compare_op`, `add_op`, etc.).
    Tag,
    /// Compound shape — any non-leaf rule.
    Compound,
}

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
fn walk_path<'a, 'p>(
    doc: &'a SheetsDocument<'p>,
    path: Path<'_>,
) -> Option<&'a SheetsValue<'p>> {
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
