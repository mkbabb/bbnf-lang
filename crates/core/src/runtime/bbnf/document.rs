//! AZ-II.cutover.A — `BbnfDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct BBNF parse path returns a [`BbnfDocument`] whose
//! root [`BbnfValue`] borrows from the input lifetime `'p` and whose
//! [`BbnfArena`] owns every compound child slice. Mirrors
//! [`crate::runtime::json::JsonDocument`] /
//! [`crate::runtime::google_sheets::SheetsDocument`].

use crate::runtime::bbnf::arena::{BbnfArena, BbnfCompound, BbnfCompoundId, BbnfCompoundKind};
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

    /// Borrow the root value.
    #[inline]
    pub fn to_value(&self) -> &BbnfValue<'p> {
        &self.root
    }

    /// Typed path query.
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

    /// Borrow the document's input slice. Inherent re-export of
    /// [`crate::runtime::RuntimeView::input`] so consumers can read
    /// the input without importing the trait at every call site.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }

    // ─── AZ-II.cutover.D3 — extension surface ─────────────────────
    // Walking accessors used by `crates/core/src/graph/{deps,metadata}.rs`
    // and `crates/core/src/pipeline/{compile,directives}.rs` after the
    // BBNF consumer migration. Mirrors the helpers retired from
    // `crates/core/src/lower/view_walk.rs` (find_descendant_by_kind,
    // peel_transparent) and operate over the struct-direct
    // `BbnfCompoundKind` alphabet.

    /// Number of structural children in this view's focus. Compound
    /// focuses report their child count; leaf focuses report `0`.
    #[inline]
    pub fn num_children(&self) -> usize {
        match self.focus {
            BbnfValue::Compound(id) => self.doc.compound(id).children.len(),
            _ => 0,
        }
    }

    /// Aggregate byte range covering this view's focus. Walks
    /// every Span descendant (computed via pointer-arithmetic
    /// against the document's input slice) and returns the union
    /// `(min_lo, max_hi)`. Returns `None` when the focus contains
    /// no source-bearing leaves (pure scalar compounds, Unit
    /// focuses, or Span values whose pointer falls outside the
    /// input slice).
    pub fn span_range(&self) -> Option<(usize, usize)> {
        let input = self.doc.input;
        let input_start = input.as_ptr() as usize;
        let input_end = input_start + input.len();
        let mut acc: Option<(usize, usize)> = None;
        self.fold_span_range(input_start, input_end, &mut acc);
        acc
    }

    fn fold_span_range(
        &self,
        input_start: usize,
        input_end: usize,
        acc: &mut Option<(usize, usize)>,
    ) {
        match self.focus {
            BbnfValue::Span(s) => {
                let s_start = s.as_ptr() as usize;
                let s_end = s_start + s.len();
                if s_start < input_start || s_end > input_end {
                    return;
                }
                let lo = s_start - input_start;
                let hi = s_end - input_start;
                *acc = Some(match *acc {
                    None => (lo, hi),
                    Some((a, b)) => (a.min(lo), b.max(hi)),
                });
            }
            BbnfValue::Compound(_) => {
                for child in self.children_iter() {
                    child.fold_span_range(input_start, input_end, acc);
                }
            }
            _ => {}
        }
    }

    /// Children iterator over compound focuses. Yields owned views
    /// (each with the same document lifetime) in source order; leaves
    /// yield nothing. Mirrors [`crate::runtime::RuntimeView::children`]
    /// without the trait-method receiver-borrow dance — usable inside
    /// recursive helpers without importing the trait.
    #[inline]
    pub fn children_iter(&self) -> BbnfChildrenSlice<'a, 'p> {
        match self.focus {
            BbnfValue::Compound(id) => BbnfChildrenSlice {
                doc: self.doc,
                children: &self.doc.compound(id).children,
                index: 0,
            },
            _ => BbnfChildrenSlice {
                doc: self.doc,
                children: &[],
                index: 0,
            },
        }
    }

    /// Depth-first descent: return the first descendant compound
    /// whose [`BbnfCompoundKind`] equals `target` (the receiver itself
    /// counts as a candidate). Mirrors the lowering
    /// `view_walk::find_descendant_by_kind`.
    pub fn find_descendant_by_kind(&self, target: BbnfCompoundKind) -> Option<BbnfView<'a, 'p>> {
        if self.compound_kind() == Some(target) {
            return Some(*self);
        }
        for child in self.children_iter() {
            if let Some(found) = child.find_descendant_by_kind(target) {
                return Some(found);
            }
        }
        None
    }

    /// Iterate the iteration children of a compound focus, unwrapping
    /// the implicit `Repeat` shape that tape walkers produced. Under
    /// struct-direct the children are already flat (the builder
    /// deposits each iteration's body directly), so this is the same
    /// view sequence as [`Self::children_iter`].
    #[inline]
    pub fn iter_children(&self) -> BbnfChildrenSlice<'a, 'p> {
        self.children_iter()
    }
}

/// Slice-backed children iterator. Co-resident with [`BbnfView`] so
/// callers can use the iterator inside non-trait-bound recursion (the
/// trait `RuntimeView::children` returns `impl Iterator + '_` whose
/// hidden type doesn't compose with explicit recursion).
#[derive(Clone)]
pub struct BbnfChildrenSlice<'a, 'p: 'a> {
    doc: &'a BbnfDocument<'p>,
    children: &'a [BbnfValue<'p>],
    index: usize,
}

impl<'a, 'p: 'a> Iterator for BbnfChildrenSlice<'a, 'p> {
    type Item = BbnfView<'a, 'p>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let value = self.children.get(self.index)?;
        self.index += 1;
        Some(BbnfView::focused(self.doc, *value))
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
