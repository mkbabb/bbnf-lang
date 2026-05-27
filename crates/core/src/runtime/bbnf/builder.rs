use bbnf_ir::registry::StructLayout;
use crate::runtime::bbnf::arena::{BbnfArena, BbnfCompound, BbnfCompoundKind};
use crate::runtime::bbnf::document::BbnfDocument;
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;
#[derive(Debug, Clone)]
struct OpenFrame<'p> {
    kind: BbnfCompoundKind,
    branch_tag: Option<u32>,
    start_offset: Option<u32>,
    end_offset: Option<u32>,
    children: Vec<BbnfValue<'p>>,
}
#[derive(Debug)]
pub struct BbnfStructBuilder<'p> {
    arena: BbnfArena<'p>,
    stack: Vec<OpenFrame<'p>>,
    root: Option<BbnfValue<'p>>,
    next_handle: u64,
}
#[derive(Debug, Clone)]
pub struct BbnfStructCheckpoint<'p> {
    compounds: usize,
    stack: Vec<OpenFrame<'p>>,
    root: Option<BbnfValue<'p>>,
    next_handle: u64,
}
impl<'p> Default for BbnfStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'p> BbnfStructBuilder<'p> {
    #[inline]
    pub fn new() -> Self {
        Self {
            arena: BbnfArena::new(),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }
    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            arena: BbnfArena::with_capacity(compounds),
            stack: Vec::with_capacity(16),
            root: None,
            next_handle: 0,
        }
    }
    #[inline]
    pub fn finalise(mut self, input: &'p str) -> BbnfDocument<'p> {
        debug_assert!(
            self.stack.is_empty(),
            "BbnfStructBuilder::finalise called with {} open frame(s)", self.stack.len()
        );
        let root = self
            .root
            .take()
            .expect("BbnfStructBuilder::finalise called before any value emission");
        BbnfDocument::new(self.arena, root, input)
    }
    #[inline]
    fn deposit(&mut self, value: BbnfValue<'p>) {
        match self.stack.last_mut() {
            None => self.root = Some(value),
            Some(frame) => frame.children.push(value),
        }
    }
}
impl<'p> StructBuilder for BbnfStructBuilder<'p> {
    type Checkpoint = BbnfStructCheckpoint<'p>;
    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        BbnfStructCheckpoint {
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
        self.stack
            .push(OpenFrame {
                kind: BbnfCompoundKind::from_layout(layout),
                branch_tag: None,
                start_offset: None,
                end_offset: None,
                children: Vec::new(),
            });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, 0)
    }
    fn end_compound(&mut self, _handle: CompoundHandle) {
        let frame = self
            .stack
            .pop()
            .expect("BbnfStructBuilder::end_compound on empty stack");
        let bounds = match (frame.start_offset, frame.end_offset) {
            (Some(start), Some(end)) => Some((start, end)),
            _ => None,
        };
        let id = self
            .arena
            .push_compound(BbnfCompound {
                kind: frame.kind,
                branch_tag: frame.branch_tag,
                bounds,
                children: frame.children,
            });
        self.deposit(BbnfValue::Compound(id));
    }
    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        self.deposit(BbnfValue::Float(value));
    }
    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        self.deposit(BbnfValue::Int(value));
    }
    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        self.deposit(BbnfValue::Int(value as i64));
    }
    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.deposit(BbnfValue::Bool(value));
    }
    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let extended: &'p str = unsafe { std::mem::transmute(value) };
        self.deposit(BbnfValue::Span(extended));
    }
    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.deposit(BbnfValue::Unit);
    }
    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.branch_tag = Some(branch_index);
        }
    }
    #[inline]
    fn record_compound_bounds_start(&mut self, offset: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.start_offset = Some(offset);
        }
    }
    #[inline]
    fn record_compound_bounds_end(&mut self, offset: u32) {
        if let Some(frame) = self.stack.last_mut() {
            frame.end_offset = Some(offset);
        }
    }
}
