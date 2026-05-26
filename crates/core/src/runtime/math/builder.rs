use bbnf_ir::registry::StructLayout;
use crate::runtime::builder_template::{SimpleCompound, SimpleStructBuilder, SimpleValue};
use crate::runtime::math::arena::{MathArena, MathCompoundId};
use crate::runtime::math::document::MathDocument;
use crate::runtime::math::kind::{MathCompound, MathCompoundKind};
use crate::runtime::math::value::MathValue;
impl<'p> SimpleValue<'p> for MathValue<'p> {
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
        Self::Compound(MathCompoundId::from_raw(id_plus_one))
    }
}
impl<'p> SimpleCompound<'p, MathValue<'p>> for MathCompound<'p> {
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<MathValue<'p>>,
    ) -> Self {
        Self {
            kind: MathCompoundKind::from_layout(layout),
            branch_tag,
            children,
        }
    }
}
pub type MathStructBuilder<'p> = SimpleStructBuilder<
    'p,
    MathValue<'p>,
    MathCompound<'p>,
>;
pub type MathStructCheckpoint<'p> = crate::runtime::builder_template::SimpleCheckpoint<
    'p,
    MathValue<'p>,
>;
impl<'p> MathStructBuilder<'p> {
    #[inline]
    pub fn finalise(self, input: &'p str) -> MathDocument<'p> {
        let (template_arena, root) = self.into_finalised();
        MathDocument::new(MathArena::from_template(template_arena), root, input)
    }
}
