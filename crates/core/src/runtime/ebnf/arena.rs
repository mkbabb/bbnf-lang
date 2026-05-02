//! AZ-IV.W5.3 — EBNF parse arena. Thin newtype around
//! [`CompoundSlabArena<EbnfCompound<'p>>`].

use crate::runtime::arena_template::CompoundSlabArena;
use crate::runtime::ebnf::kind::EbnfCompound;

#[derive(Debug, Default)]
pub struct EbnfArena<'p>(CompoundSlabArena<EbnfCompound<'p>>);

impl<'p> EbnfArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self(CompoundSlabArena::new())
    }
    #[inline]
    pub fn with_capacity(n: usize) -> Self {
        Self(CompoundSlabArena::with_capacity(n))
    }
    #[inline]
    pub(crate) fn from_template(t: CompoundSlabArena<EbnfCompound<'p>>) -> Self {
        Self(t)
    }
    #[inline]
    pub fn push_compound(&mut self, c: EbnfCompound<'p>) -> EbnfCompoundId {
        EbnfCompoundId(self.0.push_compound(c))
    }
    #[inline]
    pub fn compound(&self, id: EbnfCompoundId) -> &EbnfCompound<'p> {
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
pub struct EbnfCompoundId(u32);

impl EbnfCompoundId {
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
