use crate::runtime::bbnf::arena::{
    BbnfArena, BbnfCompound, BbnfCompoundId, BbnfCompoundKind,
};
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::path::{Path, PathSegment};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BbnfKind {
    Int,
    Float,
    Bool,
    Span,
    Tag,
    Unit,
    Compound,
}
#[derive(Debug)]
pub struct BbnfDocument<'p> {
    pub arena: BbnfArena<'p>,
    pub root: BbnfValue<'p>,
    pub input: &'p str,
}
impl<'p> BbnfDocument<'p> {
    #[inline]
    pub fn new(arena: BbnfArena<'p>, root: BbnfValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }
    #[inline]
    pub fn root(&self) -> &BbnfValue<'p> {
        &self.root
    }
    #[inline]
    pub fn arena(&self) -> &BbnfArena<'p> {
        &self.arena
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }
    #[inline]
    pub fn compound(&self, id: BbnfCompoundId) -> &BbnfCompound<'p> {
        self.arena.compound(id)
    }
    #[inline]
    pub fn view<'a>(&'a self) -> BbnfView<'a, 'p> {
        BbnfView {
            doc: self,
            focus: self.root,
        }
    }
    #[inline]
    pub fn to_value(&self) -> &BbnfValue<'p> {
        &self.root
    }
    #[inline]
    pub fn get<T: BbnfPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
}
#[derive(Debug, Clone, Copy)]
pub struct BbnfView<'a, 'p: 'a> {
    pub(crate) doc: &'a BbnfDocument<'p>,
    pub(crate) focus: BbnfValue<'p>,
}
impl<'a, 'p: 'a> BbnfView<'a, 'p> {
    #[inline]
    pub fn focused(doc: &'a BbnfDocument<'p>, focus: BbnfValue<'p>) -> Self {
        Self { doc, focus }
    }
    #[inline]
    pub fn document(&self) -> &'a BbnfDocument<'p> {
        self.doc
    }
    #[inline]
    pub fn focus(&self) -> BbnfValue<'p> {
        self.focus
    }
    #[inline]
    pub fn root(&self) -> &'a BbnfValue<'p> {
        &self.doc.root
    }
    #[inline]
    pub fn arena(&self) -> &'a BbnfArena<'p> {
        &self.doc.arena
    }
    #[inline]
    pub fn compound(&self, id: BbnfCompoundId) -> &'a BbnfCompound<'p> {
        self.doc.compound(id)
    }
    #[inline]
    pub fn kind(&self) -> BbnfKind {
        match self.focus {
            BbnfValue::Int(_) => BbnfKind::Int,
            BbnfValue::Float(_) => BbnfKind::Float,
            BbnfValue::Bool(_) => BbnfKind::Bool,
            BbnfValue::Span(_) => BbnfKind::Span,
            BbnfValue::Tag(_) => BbnfKind::Tag,
            BbnfValue::Unit => BbnfKind::Unit,
            BbnfValue::Compound(_) => BbnfKind::Compound,
        }
    }
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, BbnfValue::Compound(_))
    }
    #[inline]
    pub fn is_span(&self) -> bool {
        matches!(self.focus, BbnfValue::Span(_))
    }
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, BbnfValue::Int(_) | BbnfValue::Float(_))
    }
    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.focus, BbnfValue::Bool(_))
    }
    #[inline]
    pub fn is_tag(&self) -> bool {
        matches!(self.focus, BbnfValue::Tag(_))
    }
    #[inline]
    pub fn is_unit(&self) -> bool {
        matches!(self.focus, BbnfValue::Unit)
    }
    #[inline]
    pub fn input(&self) -> &'p str {
        self.doc.input
    }
    #[inline]
    pub fn num_children(&self) -> usize {
        match self.focus {
            BbnfValue::Compound(id) => self.doc.compound(id).children.len(),
            _ => 0,
        }
    }
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
                *acc = Some(
                    match *acc {
                        None => (lo, hi),
                        Some((a, b)) => (a.min(lo), b.max(hi)),
                    },
                );
            }
            BbnfValue::Compound(_) => {
                for child in self.children_iter() {
                    child.fold_span_range(input_start, input_end, acc);
                }
            }
            _ => {}
        }
    }
    #[inline]
    pub fn children_iter(&self) -> BbnfChildrenSlice<'a, 'p> {
        match self.focus {
            BbnfValue::Compound(id) => {
                BbnfChildrenSlice {
                    doc: self.doc,
                    children: &self.doc.compound(id).children,
                    index: 0,
                }
            }
            _ => {
                BbnfChildrenSlice {
                    doc: self.doc,
                    children: &[],
                    index: 0,
                }
            }
        }
    }
    pub fn find_descendant_by_kind(
        &self,
        target: BbnfCompoundKind,
    ) -> Option<BbnfView<'a, 'p>> {
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
    #[inline]
    pub fn iter_children(&self) -> BbnfChildrenSlice<'a, 'p> {
        self.children_iter()
    }
}
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
pub trait BbnfPathQuery: Sized {
    fn query<'p>(doc: &BbnfDocument<'p>, path: Path<'_>) -> Option<Self>;
}
#[inline]
fn walk_path<'a, 'p>(
    doc: &'a BbnfDocument<'p>,
    path: Path<'_>,
) -> Option<&'a BbnfValue<'p>> {
    let mut current: &'a BbnfValue<'p> = &doc.root;
    for segment in path.iter() {
        current = match (current, segment) {
            (BbnfValue::Compound(id), PathSegment::Index(idx)) => {
                let entry = doc.compound(*id);
                entry.children.get(*idx)?
            }
            (BbnfValue::Compound(_), PathSegment::Field(_)) => return None,
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
