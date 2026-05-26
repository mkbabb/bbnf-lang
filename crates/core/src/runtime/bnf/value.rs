use crate::runtime::bnf::arena::BnfCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BnfValue<'p> {
    Span(&'p str),
    Unit,
    Compound(BnfCompoundId),
}
impl<'p> Default for BnfValue<'p> {
    fn default() -> Self {
        Self::Unit
    }
}
