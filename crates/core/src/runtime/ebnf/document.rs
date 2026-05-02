//! AZ-II.cutover.E (Phase 2) — `EbnfDocument` + view / value /
//! path accessor surface. Mirror of `CsvDocument`.

use crate::runtime::ebnf::arena::{EbnfArena, EbnfCompoundId};
use crate::runtime::ebnf::kind::{EbnfCompound, EbnfCompoundKind};
use crate::runtime::ebnf::value::EbnfValue;
use crate::runtime::path::{Path, PathSegment};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EbnfKind {
    Span,
    Unit,
    Compound,
}

#[derive(Debug)]
pub struct EbnfDocument<'p> {
    pub arena: EbnfArena<'p>,
    pub root: EbnfValue<'p>,
    pub input: &'p str,
}

impl<'p> EbnfDocument<'p> {
    #[inline]
    pub fn new(arena: EbnfArena<'p>, root: EbnfValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    #[inline]
    pub fn root(&self) -> &EbnfValue<'p> {
        &self.root
    }
    #[inline]
    pub fn arena(&self) -> &EbnfArena<'p> {
        &self.arena
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    #[inline]
    pub fn compound(&self, id: EbnfCompoundId) -> &EbnfCompound<'p> {
        self.arena.compound(id)
    }

    #[inline]
    pub fn view<'a>(&'a self) -> EbnfView<'a, 'p> {
        EbnfView {
            doc: self,
            focus: self.root,
        }
    }

    #[inline]
    pub fn to_value(&self) -> &EbnfValue<'p> {
        &self.root
    }

    #[inline]
    pub fn get<T: EbnfPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EbnfView<'a, 'p: 'a> {
    pub(crate) doc: &'a EbnfDocument<'p>,
    pub(crate) focus: EbnfValue<'p>,
}

impl<'a, 'p: 'a> EbnfView<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a EbnfDocument<'p>, focus: EbnfValue<'p>) -> Self {
        Self { doc, focus }
    }

    #[inline]
    pub fn document(&self) -> &'a EbnfDocument<'p> {
        self.doc
    }
    #[inline]
    pub fn focus(&self) -> EbnfValue<'p> {
        self.focus
    }
    #[inline]
    pub fn root(&self) -> &'a EbnfValue<'p> {
        &self.doc.root
    }
    #[inline]
    pub fn arena(&self) -> &'a EbnfArena<'p> {
        &self.doc.arena
    }

    #[inline]
    pub fn compound(&self, id: EbnfCompoundId) -> &'a EbnfCompound<'p> {
        self.doc.compound(id)
    }

    #[inline]
    pub fn kind(&self) -> EbnfKind {
        match &self.focus {
            EbnfValue::Span(_) => EbnfKind::Span,
            EbnfValue::Unit => EbnfKind::Unit,
            EbnfValue::Compound(_) => EbnfKind::Compound,
        }
    }

    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, EbnfValue::Compound(_))
    }
    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, EbnfValue::Span(_))
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }

    #[inline]
    pub fn compound_kind(&self) -> Option<EbnfCompoundKind> {
        match self.focus {
            EbnfValue::Compound(id) => Some(self.doc.compound(id).kind),
            _ => None,
        }
    }
}

pub trait EbnfPathQuery: Sized {
    fn query<'p>(doc: &EbnfDocument<'p>, path: Path<'_>) -> Option<Self>;
}

#[inline]
fn walk_path<'a, 'p>(doc: &'a EbnfDocument<'p>, path: Path<'_>) -> Option<&'a EbnfValue<'p>> {
    let mut current: &'a EbnfValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (EbnfValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            (EbnfValue::Compound(_), PathSegment::Field(_)) => return None,
            _ => return None,
        };
    }
    Some(current)
}

impl EbnfPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &EbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            EbnfValue::Span(s) => {
                let extended: &'p str = *s;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            _ => None,
        }
    }
}

impl EbnfPathQuery for EbnfValue<'_> {
    #[inline]
    fn query<'p>(doc: &EbnfDocument<'p>, path: Path<'_>) -> Option<Self> {
        let value = walk_path(doc, path)?;
        let copied: EbnfValue<'p> = *value;
        Some(unsafe { core::mem::transmute::<EbnfValue<'p>, EbnfValue<'_>>(copied) })
    }
}
