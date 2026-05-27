use crate::runtime::arena_template::CompoundSlabArena;
use crate::runtime::css_pretty::kind::CssPrettyCompound;
#[derive(Debug, Default)]
pub struct CssPrettyArena<'p>(CompoundSlabArena<CssPrettyCompound<'p>>);
impl<'p> CssPrettyArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self(CompoundSlabArena::new())
    }
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        Self(CompoundSlabArena::with_capacity(n))
    }
    #[inline]
    pub(crate) fn from_template(t: CompoundSlabArena<CssPrettyCompound<'p>>) -> Self {
        Self(t)
    }
    #[inline]
    pub fn push_compound(&mut self, c: CssPrettyCompound<'p>) -> CssPrettyCompoundId {
        CssPrettyCompoundId(self.0.push_compound(c))
    }
    #[inline]
    pub fn compound(&self, id: CssPrettyCompoundId) -> &CssPrettyCompound<'p> {
        self.0.compound(id.0)
    }
    #[inline]
    pub fn compound_count(&self) -> usize {
        self.0.compound_count()
    }
    #[inline]
    pub fn truncate(&mut self, n: usize) {
        self.0.truncate(n);
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssPrettyCompoundId(u32);
impl CssPrettyCompoundId {
    pub const EMPTY: Self = Self(0);
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    #[inline]
    pub(crate) const fn from_raw(id: u32) -> Self {
        Self(id)
    }
}
