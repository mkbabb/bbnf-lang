//! AZ-II.cutover.E (Phase 2) — `MathDocument` + view / value / path
//! accessor surface. Mirror of `CsvDocument` / `BbnfDocument`.

use crate::runtime::math::arena::{MathArena, MathCompoundId};
use crate::runtime::math::kind::{MathCompound, MathCompoundKind};
use crate::runtime::math::value::MathValue;
use crate::runtime::path::{Path, PathSegment};

/// Discriminator over the typed shapes a [`MathValue`] takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathKind {
    Span,
    Unit,
    Compound,
}

/// The root document.
#[derive(Debug)]
pub struct MathDocument<'p> {
    pub arena: MathArena<'p>,
    pub root: MathValue<'p>,
    pub input: &'p str,
}

impl<'p> MathDocument<'p> {
    #[inline]
    pub fn new(arena: MathArena<'p>, root: MathValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    #[inline]
    pub fn root(&self) -> &MathValue<'p> {
        &self.root
    }

    #[inline]
    pub fn arena(&self) -> &MathArena<'p> {
        &self.arena
    }

    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    #[inline]
    pub fn compound(&self, id: MathCompoundId) -> &MathCompound<'p> {
        self.arena.compound(id)
    }

    #[inline]
    pub fn view<'a>(&'a self) -> MathView<'a, 'p> {
        MathView {
            doc: self,
            focus: self.root,
        }
    }

    #[inline]
    pub fn to_value(&self) -> &MathValue<'p> {
        &self.root
    }

    #[inline]
    pub fn get<T: MathPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

/// A thin newtype over `&MathDocument` exposing the focused-view surface.
#[derive(Debug, Clone, Copy)]
pub struct MathView<'a, 'p: 'a> {
    pub(crate) doc: &'a MathDocument<'p>,
    pub(crate) focus: MathValue<'p>,
}

impl<'a, 'p: 'a> MathView<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a MathDocument<'p>, focus: MathValue<'p>) -> Self {
        Self { doc, focus }
    }

    #[inline]
    pub fn document(&self) -> &'a MathDocument<'p> {
        self.doc
    }

    #[inline]
    pub fn focus(&self) -> MathValue<'p> {
        self.focus
    }

    #[inline]
    pub fn root(&self) -> &'a MathValue<'p> {
        &self.doc.root
    }

    #[inline]
    pub fn arena(&self) -> &'a MathArena<'p> {
        &self.doc.arena
    }

    #[inline]
    pub fn compound(&self, id: MathCompoundId) -> &'a MathCompound<'p> {
        self.doc.compound(id)
    }

    #[inline]
    pub fn kind(&self) -> MathKind {
        match &self.focus {
            MathValue::Span(_) => MathKind::Span,
            MathValue::Unit => MathKind::Unit,
            MathValue::Compound(_) => MathKind::Compound,
        }
    }

    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, MathValue::Compound(_))
    }

    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, MathValue::Span(_))
    }

    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }

    /// Compound-kind discriminator accessor.
    #[inline]
    pub fn compound_kind(&self) -> Option<MathCompoundKind> {
        match self.focus {
            MathValue::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }
    }
}

/// Typed path-query trait for math.
pub trait MathPathQuery: Sized {
    fn query<'p>(doc: &MathDocument<'p>, path: Path<'_>) -> Option<Self>;
}

#[inline]
fn walk_path<'a, 'p>(doc: &'a MathDocument<'p>, path: Path<'_>) -> Option<&'a MathValue<'p>> {
    let mut current: &'a MathValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (MathValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            (MathValue::Compound(_), PathSegment::Field(_)) => return None,
            _ => return None,
        };
    }
    Some(current)
}

impl MathPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &MathDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            MathValue::Span(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl MathPathQuery for MathValue<'_> {
    #[inline]
    fn query<'p>(doc: &MathDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: MathValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<MathValue<'p>, MathValue<'_>>(copied) })
    }
}
