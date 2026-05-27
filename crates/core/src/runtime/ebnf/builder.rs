use bbnf_ir::registry::StructLayout;
use crate::runtime::builder_template::{SimpleCompound, SimpleStructBuilder, SimpleValue};
use crate::runtime::ebnf::arena::{EbnfArena, EbnfCompoundId};
use crate::runtime::ebnf::document::EbnfDocument;
use crate::runtime::ebnf::kind::{EbnfCompound, EbnfCompoundKind};
use crate::runtime::ebnf::value::EbnfValue;
impl<'p> SimpleValue<'p> for EbnfValue<'p> {
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
        Self::Compound(EbnfCompoundId::from_raw(id_plus_one))
    }
}
impl<'p> SimpleCompound<'p, EbnfValue<'p>> for EbnfCompound<'p> {
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<EbnfValue<'p>>,
    ) -> Self {
        Self {
            kind: EbnfCompoundKind::from_layout(layout),
            branch_tag,
            children,
        }
    }
}
pub type EbnfStructBuilder<'p> = SimpleStructBuilder<
    'p,
    EbnfValue<'p>,
    EbnfCompound<'p>,
>;
pub type EbnfStructCheckpoint<'p> = crate::runtime::builder_template::SimpleCheckpoint<
    'p,
    EbnfValue<'p>,
>;
impl<'p> EbnfStructBuilder<'p> {
    #[inline]
    pub fn finalise(self, input: &'p str) -> EbnfDocument<'p> {
        let (template_arena, root) = self.into_finalised();
        EbnfDocument::new(EbnfArena::from_template(template_arena), root, input)
    }
}
