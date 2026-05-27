use bbnf_ir::registry::StructLayout;
use crate::runtime::builder_template::{SimpleCompound, SimpleStructBuilder, SimpleValue};
use crate::runtime::css_pretty::arena::{CssPrettyArena, CssPrettyCompoundId};
use crate::runtime::css_pretty::document::CssPrettyDocument;
use crate::runtime::css_pretty::kind::{CssPrettyCompound, CssPrettyCompoundKind};
use crate::runtime::css_pretty::value::CssPrettyValue;
impl<'p> SimpleValue<'p> for CssPrettyValue<'p> {
    #[inline]
    fn from_span(s: &'p str) -> Self {
        Self::Span(s)
    }
    #[inline]
    fn unit() -> Self {
        Self::Unit
    }
    #[inline]
    fn from_compound_index(id_plus_one: u32) -> Self {
        Self::Compound(CssPrettyCompoundId::from_raw(id_plus_one))
    }
}
impl<'p> SimpleCompound<'p, CssPrettyValue<'p>> for CssPrettyCompound<'p> {
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<CssPrettyValue<'p>>,
    ) -> Self {
        Self {
            kind: CssPrettyCompoundKind::from_layout(layout),
            branch_tag,
            children,
        }
    }
}
pub type CssPrettyStructBuilder<'p> = SimpleStructBuilder<
    'p,
    CssPrettyValue<'p>,
    CssPrettyCompound<'p>,
>;
pub type CssPrettyStructCheckpoint<'p> = crate::runtime::builder_template::SimpleCheckpoint<
    'p,
    CssPrettyValue<'p>,
>;
impl<'p> CssPrettyStructBuilder<'p> {
    #[inline]
    pub fn finalise(self, input: &'p str) -> CssPrettyDocument<'p> {
        let (template_arena, root) = self.into_finalised();
        CssPrettyDocument::new(
            CssPrettyArena::from_template(template_arena),
            root,
            input,
        )
    }
}
