use bbnf_ir::registry::StructLayout;
use crate::runtime::bnf::value::BnfValue;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BnfCompoundKind {
    Identifier,
    Terminal,
    Nonterminal,
    Term,
    Expression,
    Alternation,
    Rhs,
    Lhs,
    Rule,
    Grammar,
    Other,
}
impl BnfCompoundKind {
    pub fn from_layout(layout: &StructLayout) -> Self {
        match layout.rule_id {
            0 => Self::Terminal,
            1 => Self::Nonterminal,
            2 => Self::Alternation,
            3 => Self::Rule,
            4 => Self::Grammar,
            _ => Self::Other,
        }
    }
}
#[derive(Debug, Clone)]
pub struct BnfCompound<'p> {
    pub kind: BnfCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<BnfValue<'p>>,
}
impl<'p> Default for BnfCompound<'p> {
    fn default() -> Self {
        Self {
            kind: BnfCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}
