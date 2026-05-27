use crate::runtime::ebnf::arena::EbnfCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EbnfValue<'p> {
    Span(&'p str),
    Unit,
    Compound(EbnfCompoundId),
}
impl<'p> Default for EbnfValue<'p> {
    fn default() -> Self {
        Self::Unit
    }
}
