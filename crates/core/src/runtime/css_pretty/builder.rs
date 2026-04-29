//! AZ-II.cutover.E (Phase 2) — `CssPrettyStructBuilder`. Mirror of
//! `CsvStructBuilder`.

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
use crate::runtime::css_pretty::arena::{CssPrettyArena, CssPrettyCompound, CssPrettyCompoundKind};
use crate::runtime::css_pretty::document::CssPrettyDocument;
use crate::runtime::css_pretty::value::CssPrettyValue;

#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: CssPrettyCompoundKind,
    branch_tag: Option<u32>,
    children: Vec<CssPrettyValue<'p>>,
}

#[derive(Debug)]
pub struct CssPrettyStructBuilder<'p> {
    arena: CssPrettyArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<CssPrettyValue<'p>>,
    next_handle: u64,
}

/// Rollback snapshot for [`CssPrettyStructBuilder`].
#[derive(Debug, Clone)]
pub struct CssPrettyStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<CssPrettyValue<'p>>,
    next_handle: u64,
}

impl<'p> Default for CssPrettyStructBuilder<'p> {
    fn default() -> Self { Self::new() }
}

impl<'p> CssPrettyStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: CssPrettyArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: CssPrettyArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }

    #[inline]
    pub fn finalise(mut self, input: &'p str) -> CssPrettyDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "CssPrettyStructBuilder::finalise called with {} open frame(s)",
            self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("CssPrettyStructBuilder::finalise called before any value emission");
        CssPrettyDocument::new(self.arena, root, input)
    }

    #[inline]
    fn deposit(&mut self, value: CssPrettyValue<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}

impl<'p> StructBuilder for CssPrettyStructBuilder<'p> {
    type Checkpoint = CssPrettyStructCheckpoint<'p>;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        CssPrettyStructCheckpoint {
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
        let kind = CssPrettyCompoundKind::from_rule_name(layout.rule_name.as_str());
        self.stack.push(OpenFrame { kind, branch_tag: None, children: Vec::new() });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }

    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self.stack.pop().expect("CssPrettyStructBuilder::end_compound on empty stack");
        let id = self.arena.push_compound(CssPrettyCompound {
            kind: frame.kind,
            branch_tag: frame.branch_tag,
            children: frame.children,
        });
        self.deposit(CssPrettyValue::Compound(id));
    }

    #[inline] fn push_leaf_with_f64(&mut self, _v: f64)  { self.deposit(CssPrettyValue::Unit); }
    #[inline] fn push_leaf_with_i64(&mut self, _v: i64)  { self.deposit(CssPrettyValue::Unit); }
    #[inline] fn push_leaf_with_u64(&mut self, _v: u64)  { self.deposit(CssPrettyValue::Unit); }
    #[inline] fn push_leaf_with_bool(&mut self, _v: bool) { self.deposit(CssPrettyValue::Unit); }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let lifetime_extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(CssPrettyValue::Span(lifetime_extended));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(CssPrettyValue::Unit);
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
}
