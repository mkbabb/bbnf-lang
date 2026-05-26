use crate::runtime::csv::arena::CsvCompoundId;
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CsvValue<'p> {
    Span(&'p str),
    Unit,
    Compound(CsvCompoundId),
}
impl<'p> Default for CsvValue<'p> {
    fn default() -> Self {
        Self::Unit
    }
}
