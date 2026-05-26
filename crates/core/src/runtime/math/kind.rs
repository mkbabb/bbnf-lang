use bbnf_ir::registry::StructLayout;
use crate::runtime::math::value::MathValue;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathCompoundKind {
    Expr,
    Term,
    Factor,
    Wrapped,
    P,
    Pp,
    Ppp,
    Pppp,
    Ppppp,
    Pppppp,
    Other,
}
impl MathCompoundKind {
    pub fn from_layout(_layout: &StructLayout) -> Self {
        Self::Other
    }
}
#[derive(Debug, Clone)]
pub struct MathCompound<'p> {
    pub kind: MathCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<MathValue<'p>>,
}
impl<'p> Default for MathCompound<'p> {
    fn default() -> Self {
        Self {
            kind: MathCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}
