use bbnf_ir::registry::StructLayout;
use crate::runtime::css_pretty::value::CssPrettyValue;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CssPrettyCompoundKind {
    Ws,
    SelectorSpan,
    ValueSpan,
    PropertyName,
    Important,
    OptSemicolon,
    Declaration,
    BlockContent,
    RuleBlock,
    QualifiedRule,
    MediaRule,
    SupportsRule,
    FontFaceRule,
    ImportRule,
    AtRuleBody,
    GenericAtRule,
    AtRule,
    RuleItem,
    RuleList,
    Stylesheet,
    Other,
}
impl CssPrettyCompoundKind {
    pub fn from_layout(layout: &StructLayout) -> Self {
        match layout.rule_id {
            0 => Self::Important,
            1 => Self::ImportRule,
            2 => Self::Declaration,
            3 => Self::GenericAtRule,
            4 => Self::QualifiedRule,
            5 => Self::MediaRule,
            6 => Self::SupportsRule,
            7 => Self::FontFaceRule,
            9 => Self::RuleBlock,
            10 => Self::BlockContent,
            13 => Self::RuleList,
            14 => Self::Stylesheet,
            _ => Self::Other,
        }
    }
}
#[derive(Debug, Clone)]
pub struct CssPrettyCompound<'p> {
    pub kind: CssPrettyCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<CssPrettyValue<'p>>,
}
impl<'p> Default for CssPrettyCompound<'p> {
    fn default() -> Self {
        Self {
            kind: CssPrettyCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}
