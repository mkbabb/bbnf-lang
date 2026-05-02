//! AZ-IV.W5.3 — Math parse arena. Thin newtype around
//! [`CompoundSlabArena<MathCompound<'p>>`].

use crate::runtime::arena_template::CompoundSlabArena;
use crate::runtime::math::kind::MathCompound;

#[derive(Debug, Default)]
pub struct MathArena<'p>(CompoundSlabArena<MathCompound<'p>>);

impl<'p> MathArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self(CompoundSlabArena::new())
    }
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        Self(CompoundSlabArena::with_capacity(n))
    }
    #[inline]
    pub(crate) fn from_template(t: CompoundSlabArena<MathCompound<'p>>) -> Self {
        Self(t)
    }
    #[inline]
    pub fn push_compound(&mut self, c: MathCompound<'p>) -> MathCompoundId {
        MathCompoundId(self.0.push_compound(c))
    }
    #[inline]
    pub fn compound(&self, id: MathCompoundId) -> &MathCompound<'p> {
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
pub struct MathCompoundId(u32);

impl MathCompoundId {
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
