//! AZ-IV.W5.3 — `CsvStructBuilder` — thin instantiation of
//! [`SimpleStructBuilder`].

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder_template::{SimpleCompound, SimpleStructBuilder, SimpleValue};
use crate::runtime::csv::arena::CsvArena;
use crate::runtime::csv::document::CsvDocument;
use crate::runtime::csv::kind::{CsvCompound, CsvCompoundKind};
use crate::runtime::csv::value::CsvValue;

impl<'p> SimpleValue<'p> for CsvValue<'p> {
    #[inline]
    fn from_span(s: &'p str) -> Self {
        CsvValue::Span(s)
    }
    #[inline]
    fn unit() -> Self {
        CsvValue::Unit
    }
    #[inline]
    fn from_compound_index(id_plus_one: u32) -> Self {
        CsvValue::Compound(crate::runtime::csv::arena::CsvCompoundId::from_raw(
            id_plus_one,
        ))
    }
}

impl<'p> SimpleCompound<'p, CsvValue<'p>> for CsvCompound<'p> {
    #[inline]
    fn new_entry(
        layout: &StructLayout,
        branch_tag: Option<u32>,
        children: Vec<CsvValue<'p>>,
    ) -> Self {
        Self {
            kind: CsvCompoundKind::from_layout(layout),
            branch_tag,
            children,
        }
    }
}

pub type CsvStructBuilder<'p> = SimpleStructBuilder<'p, CsvValue<'p>, CsvCompound<'p>>;
pub type CsvStructCheckpoint<'p> =
    crate::runtime::builder_template::SimpleCheckpoint<'p, CsvValue<'p>>;

impl<'p> CsvStructBuilder<'p> {
    #[inline]
    pub fn finalise(self, input: &'p str) -> CsvDocument<'p> {
        let (template_arena, root) = self.into_finalised();
        CsvDocument::new(CsvArena::from_template(template_arena), root, input)
    }
}
