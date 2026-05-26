use crate::runtime::math::arena::MathCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathValue<'p> {
    Span(&'p str),
    Unit,
    Compound(MathCompoundId),
}
impl<'p> Default for MathValue<'p> {
    fn default() -> Self {
        Self::Unit
    }
}
