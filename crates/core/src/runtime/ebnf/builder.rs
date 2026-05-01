//! AZ-II.cutover.E (Phase 2) — `EbnfStructBuilder`. Mirror of
//! `CsvStructBuilder`.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::ebnf::arena::{EbnfArena, EbnfCompound, EbnfCompoundKind};
use crate::runtime::ebnf::document::EbnfDocument;
use crate::runtime::ebnf::value::EbnfValue;
use crate::runtime::handle::CompoundHandle;

#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: EbnfCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<EbnfValue<'p>>,
}

#[derive(Debug)]
pub struct EbnfStructBuilder<'p> {
    arena: EbnfArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<EbnfValue<'p>>,
    next_handle: u64,
}

/// Rollback snapshot for [`EbnfStructBuilder`].
#[derive(Debug, Clone)]
pub struct EbnfStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<EbnfValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for EbnfStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> EbnfStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: EbnfArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: EbnfArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> EbnfDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "EbnfStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("EbnfStructBuilder::finalise called before any value emission");
        EbnfDocument::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: EbnfValue<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}

impl<'p> StructBuilder for EbnfStructBuilder<'p> {
    type Checkpoint = EbnfStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        EbnfStructCheckpoint {
            compounds: self.arena.compound_count(),
            stack: self.stack.clone(),
            root: self.root,
            next_handle: self.next_handle,
        }
    }

    #[inline]
    fn rollback(&mut self, checkpoint: Self::Checkpoint) {
        self.arena.truncate(checkpoint.compounds);
        self.stack = checkpoint.stack;
        self.root = checkpoint.root;
        self.next_handle = checkpoint.next_handle;
    }

    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let kind = EbnfCompoundKind::from_rule_name(layout.rule_name.as_str());
        self.stack.push(OpenFrame {
            kind,
            branch_tag: None,
            children: Vec::new(),
        });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("EbnfStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(EbnfCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(EbnfValue::Compound(id));
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, _v: f64) {
        self.deposit(EbnfValue::Unit);
    }
    #[inline]
    fn push_leaf_with_i64(&mut self, _v: i64) {
        self.deposit(EbnfValue::Unit);
    }
    #[inline]
    fn push_leaf_with_u64(&mut self, _v: u64) {
        self.deposit(EbnfValue::Unit);
    }
    #[inline]
    fn push_leaf_with_bool(&mut self, _v: bool) {
        self.deposit(EbnfValue::Unit);
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(EbnfValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(EbnfValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
