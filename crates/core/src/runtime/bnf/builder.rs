//! AZ-IV.W5.3 — `BnfStructBuilder` — thin instantiation of
//! [`SimpleStructBuilder`].

use bbnf_ir::registry::StructLayout;

use crate::runtime::bnf::arena::BnfArena;
use crate::runtime::bnf::document::BnfDocument;
use crate::runtime::bnf::kind::{BnfCompound, BnfCompoundKind};
use crate::runtime::bnf::value::BnfValue;
use crate::runtime::builder_template::{SimpleCompound, SimpleStructBuilder, SimpleValue};

impl<'p> SimpleValue<'p> for BnfValue<'p> {
    #[inline]
    fn from_span(s: &'p str) -> Self {
        BnfValue::Span(s)
    }
    #[inline]
    fn unit() -> Self {
        BnfValue::Unit
    }
    #[inline]
    fn from_compound_index(id_plus_one: u32) -> Self {
        BnfValue::Compound(crate::runtime::bnf::arena::BnfCompoundId::from_raw(
            id_plus_one,
        ))
    }
}

impl<'p> SimpleCompound<'p, BnfValue<'p>> for BnfCompound<'p> {
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<BnfValue<'p>>,
    ) -> Self {
        Self {
            kind: BnfCompoundKind::from_layout(layout),
            branch_tag,
            children,
        }
    }
}

pub type BnfStructBuilder<'p> = SimpleStructBuilder<'p, BnfValue<'p>, BnfCompound<'p>>;
pub type BnfStructCheckpoint<'p> =
    crate::runtime::builder_template::SimpleCheckpoint<'p, BnfValue<'p>>;

impl<'p> BnfStructBuilder<'p> {
    #[inline]
    pub fn finalise(self, input: &'p str) -> BnfDocument<'p> {
        let (template_arena, root) = self.into_finalised();
        BnfDocument::new(BnfArena::from_template(template_arena), root, input)
    }
}
