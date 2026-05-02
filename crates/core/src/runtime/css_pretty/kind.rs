//! AZ-IV.W5.3 — CssPretty compound kind discriminator + arena entry shape.

use bbnf_ir::registry::{StructLayout, StructRegistry};

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
        match StructRegistry::compound_kind_for_layout(layout) {
            "important" => Self::Important,
            "importRule" => Self::ImportRule,
            "declaration" => Self::Declaration,
            "genericAtRule" => Self::GenericAtRule,
            "qualifiedRule" => Self::QualifiedRule,
            "mediaRule" => Self::MediaRule,
            "supportsRule" => Self::SupportsRule,
            "fontFaceRule" => Self::FontFaceRule,
            "ruleBlock" => Self::RuleBlock,
            "blockContent" => Self::BlockContent,
            "ruleList" => Self::RuleList,
            "stylesheet" => Self::Stylesheet,
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
