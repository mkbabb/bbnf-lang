use bbnf_ir::registry::StructLayout;
use crate::runtime::csv::value::CsvValue;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CsvCompoundKind {
    Record,
    Csv,
    Field,
    Other,
}
impl CsvCompoundKind {
    pub fn from_layout(layout: &StructLayout) -> Self {
        match layout.rule_id {
            2 => Self::Record,
            3 => Self::Csv,
            _ => Self::Other,
        }
    }
}
#[derive(Debug, Clone)]
pub struct CsvCompound<'p> {
    pub kind: CsvCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<CsvValue<'p>>,
}
impl<'p> Default for CsvCompound<'p> {
    fn default() -> Self {
        Self {
            kind: CsvCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}
