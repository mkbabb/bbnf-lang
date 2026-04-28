//! AZ-II.cutover.E (Phase 2) — `BnfStructBuilder`. Mirror of
//! `CsvStructBuilder`.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use crate::runtime::bnf::arena::{BnfArena, BnfCompound, BnfCompoundKind};
use crate::runtime::bnf::document::BnfDocument;
use crate::runtime::bnf::value::BnfValue;

#[derive(Debug)]
struct OpenFrame<'p> {
    kind: BnfCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<BnfValue<'p>>,
}

#[derive(Debug)]
pub struct BnfStructBuilder<'p> {
    arena: BnfArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<BnfValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for BnfStructBuilder<'p> {
    fn default() -> Self { Self::new() }
}

impl<'p> BnfStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: BnfArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: BnfArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> BnfDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "BnfStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("BnfStructBuilder::finalise called before any value emission");
        BnfDocument::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: BnfValue<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}

impl<'p> StructBuilder for BnfStructBuilder<'p> {
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let kind = BnfCompoundKind::from_rule_name(layout.rule_name.as_str());
        self.stack.push(OpenFrame { kind, branch_tag: None, children: Vec::new() });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self.stack.pop().expect("BnfStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(BnfCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(BnfValue::Compound(id));
    }

    #[inline] fn push_leaf_with_f64(&mut self, _v: f64)  { self.deposit(BnfValue::Unit); }
    #[inline] fn push_leaf_with_i64(&mut self, _v: i64)  { self.deposit(BnfValue::Unit); }
    #[inline] fn push_leaf_with_u64(&mut self, _v: u64)  { self.deposit(BnfValue::Unit); }
    #[inline] fn push_leaf_with_bool(&mut self, _v: bool) { self.deposit(BnfValue::Unit); }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(BnfValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(BnfValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
