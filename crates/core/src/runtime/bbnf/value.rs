use crate::runtime::bbnf::arena::BbnfCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BbnfValue<'p> {
    Int(i64),
    Float(f64),
    Bool(bool),
    Span(&'p str),
    Tag(u8),
    Unit,
    Compound(BbnfCompoundId),
}
impl<'p> Default for BbnfValue<'p> {
    fn default() -> Self {
        BbnfValue::Unit
    }
}
