//! Shared flat-tape runtime substrate.
//!
//! A grammar-agnostic, crate-internal append-only post-order tape that
//! replaces the per-grammar `OpenFrame` builder and `Vec<Vec<_>>` arena
//! slabs. Recovered from the A-series 16-byte AoS `TapeRec` (which hit
//! CSS bootstrap 454 / normalize 735 Mbps) and refined toward the skinny
//! SoA tape's payload encoding, but kept AoS first: AoS removes the
//! `OpenFrame` enum and the seven `pending_*` `Vec<Vec>` in one move, and
//! the same [`cursor::TapeCursor`] API rides a later SoA split.
//!
//! Three responsibilities, three files:
//! - [`record`] — the 16-byte `#[repr(C)]` `TapeRec` (const-asserted).
//! - [`arena`] — the 8-aligned `PayloadArena` bump allocator.
//! - [`cursor`] — `TapeCursor` post-order navigation + lazy decode.
//!
//! [`TapeStructBuilder`] is the single generic [`StructBuilder`] impl;
//! it is layout-driven and carries no per-grammar route strings, so the
//! same builder serves CSS, JSON, sheets, and bbnf. Per
//! `feedback_no-orthogonal-codepaths` it is introduced as the shared
//! model, not a CSS-only fork.

pub mod arena;
pub mod cursor;
pub mod record;

use bbnf_ir::registry::StructLayout;

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;

use arena::PayloadArena;
use cursor::{root_offset, TapeCursor, TapeRef};
use record::{LeafType, TapeOffset, TapeRec, TAPE_NONE};

/// An in-flight open compound: where its first child sits, the source
/// span start, the rule's layout id (`meta_idx`), and the branch tag
/// stamped while it was open.
#[derive(Debug, Clone, Copy)]
struct OpenScope {
    /// Record offset of this compound's first child (== `records.len()`
    /// at `begin_compound`, before any child is appended).
    child_start: u32,
    /// Source byte offset captured at `begin_compound` / via
    /// `record_compound_bounds_start`.
    span_lo: u32,
    span_hi: u32,
    /// 5-bit discriminant index — the layout rule id, clamped.
    meta_idx: u8,
    /// 6-bit branch tag stamped by `push_branch_tag`.
    branch: u8,
    has_branch: bool,
}

/// The single generic tape builder. Grammar-agnostic: it dispatches on
/// the [`StructLayout`] handed to `begin_compound`, never on per-grammar
/// route strings.
#[derive(Debug)]
pub struct TapeStructBuilder<'p> {
    records: Vec<TapeRec>,
    arena: PayloadArena,
    /// Open compounds, innermost last.
    scopes: Vec<OpenScope>,
    /// Pending compound-bounds starts (mirrors the trait's
    /// `record_compound_bounds_start/_end` LIFO).
    span_starts: Vec<u32>,
    input: &'p [u8],
    root: Option<TapeOffset>,
    next_handle: u64,
}

/// O(1) checkpoint: two lengths plus the open-scope depth. Copy, no
/// `Vec` clone — strictly cheaper than the eager builder's stack clone.
#[derive(Debug, Clone, Copy)]
pub struct TapeCheckpoint {
    records_len: u32,
    arena_len: u32,
    scopes_len: u32,
    span_starts_len: u32,
    next_handle: u64,
    root: Option<TapeOffset>,
}

impl<'p> Default for TapeStructBuilder<'p> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'p> TapeStructBuilder<'p> {
    pub fn new() -> Self {
        Self {
            records: Vec::with_capacity(256),
            arena: PayloadArena::with_capacity(256),
            scopes: Vec::with_capacity(16),
            span_starts: Vec::with_capacity(16),
            input: &[],
            root: None,
            next_handle: 0,
        }
    }

    /// Borrow the completed tape for lazy-view decode. Valid only after
    /// the final `end_compound`.
    pub fn tape_ref<'a>(&'a self, source: &'a str) -> TapeRef<'a> {
        TapeRef {
            records: &self.records,
            arena: &self.arena,
            source,
        }
    }

    /// A cursor at the root compound, or `None` for an empty parse.
    pub fn root_cursor<'a>(&'a self, source: &'a str) -> Option<TapeCursor<'a>> {
        let off = self.root.or_else(|| root_offset(&self.records))?;
        Some(TapeCursor::new(self.tape_ref(source), off))
    }

    pub fn records(&self) -> &[TapeRec] {
        &self.records
    }

    pub fn arena(&self) -> &PayloadArena {
        &self.arena
    }

    /// Append a leaf record onto the current open compound (or top level).
    #[inline]
    fn push_leaf_rec(&mut self, rec: TapeRec) {
        self.records.push(rec);
    }
}

impl<'p> StructBuilder for TapeStructBuilder<'p> {
    type Checkpoint = TapeCheckpoint;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        TapeCheckpoint {
            records_len: self.records.len() as u32,
            arena_len: self.arena.len(),
            scopes_len: self.scopes.len() as u32,
            span_starts_len: self.span_starts.len() as u32,
            next_handle: self.next_handle,
            root: self.root,
        }
    }

    #[inline]
    fn rollback(&mut self, c: Self::Checkpoint) {
        self.records.truncate(c.records_len as usize);
        self.arena.truncate(c.arena_len);
        self.scopes.truncate(c.scopes_len as usize);
        self.span_starts.truncate(c.span_starts_len as usize);
        self.next_handle = c.next_handle;
        self.root = c.root;
    }

    #[inline]
    fn bind_input(&mut self, input: &[u8]) {
        self.input = unsafe { core::mem::transmute::<&[u8], &'p [u8]>(input) };
    }

    #[inline]
    fn record_compound_bounds_start(&mut self, offset: u32) {
        self.span_starts.push(offset);
        if let Some(scope) = self.scopes.last_mut() {
            if scope.span_lo == TAPE_NONE {
                scope.span_lo = offset;
            }
        }
    }

    #[inline]
    fn record_compound_bounds_end(&mut self, offset: u32) {
        let _ = self.span_starts.pop();
        if let Some(scope) = self.scopes.last_mut() {
            scope.span_hi = offset;
            if scope.span_lo == TAPE_NONE {
                scope.span_lo = offset;
            }
        }
    }

    #[inline]
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let meta = (layout.rule_id & 0x1F) as u8;
        self.scopes.push(OpenScope {
            child_start: self.records.len() as u32,
            span_lo: TAPE_NONE,
            span_hi: TAPE_NONE,
            meta_idx: meta,
            branch: 0,
            has_branch: false,
        });
        self.next_handle = self.next_handle.wrapping_add(1);
        CompoundHandle::new(self.next_handle, layout.rule_id)
    }

    #[inline]
    fn end_compound(&mut self, _handle: CompoundHandle) {
        let scope = self
            .scopes
            .pop()
            .expect("TapeStructBuilder::end_compound on empty scope stack");
        let child_start = scope.child_start;
        let has_children = (self.records.len() as u32) > child_start;
        let (lo, hi) = (
            if scope.span_lo == TAPE_NONE {
                0
            } else {
                scope.span_lo
            },
            if scope.span_hi == TAPE_NONE {
                0
            } else {
                scope.span_hi
            },
        );
        let mut rec = TapeRec::open(lo, hi, scope.meta_idx);
        rec.set_has_children(has_children);
        rec.child_off = if has_children { child_start } else { TAPE_NONE };
        if scope.has_branch {
            rec.set_branch(scope.branch);
        }
        let off = self.records.len() as u32;
        self.records.push(rec);
        if self.scopes.is_empty() {
            self.root = Some(off);
        }
    }

    #[inline]
    fn push_leaf_with_f64(&mut self, value: f64) {
        let off = self.arena.write_f64(value);
        self.push_leaf_rec(TapeRec::leaf(LeafType::F64, 0, 0, off));
    }

    #[inline]
    fn push_leaf_with_i64(&mut self, value: i64) {
        let off = self.arena.write_i64(value);
        self.push_leaf_rec(TapeRec::leaf(LeafType::I64, 0, 0, off));
    }

    #[inline]
    fn push_leaf_with_u64(&mut self, value: u64) {
        if value <= u32::MAX as u64 {
            // inline-pack into child_off.
            self.push_leaf_rec(TapeRec::leaf(LeafType::U64, 0, 0, value as u32));
        } else {
            let off = self.arena.write_u64(value);
            // arena offset stored in child_off; span_hi flags arena path
            // is not needed because U64 with off >= records is ambiguous,
            // so wide u64 always rides the arena read via as_u64.
            let mut rec = TapeRec::leaf(LeafType::U64, 0, 0, off);
            rec.span_lo = 1; // marker: arena-resident
            self.push_leaf_rec(rec);
        }
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.push_leaf_rec(TapeRec::leaf(LeafType::Bool, 0, 0, value as u32));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        // Zero-copy when the slice points into the bound input.
        let base = self.input.as_ptr() as usize;
        let p = value.as_ptr() as usize;
        if !self.input.is_empty() && p >= base && p + value.len() <= base + self.input.len() {
            let lo = (p - base) as u32;
            let hi = lo + value.len() as u32;
            let mut rec = TapeRec::leaf(LeafType::Str, lo, hi, TAPE_NONE);
            rec.extra |= record::STRING_BORROW_BIT;
            self.push_leaf_rec(rec);
        } else {
            let (off, len) = self.arena.write_str(value);
            // store len in span_hi for read_str reconstruction.
            self.push_leaf_rec(TapeRec::leaf(LeafType::Str, 0, len, off));
        }
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.push_leaf_rec(TapeRec::leaf(LeafType::Unit, 0, 0, TAPE_NONE));
    }

    #[inline]
    fn push_branch_tag(&mut self, branch_index: u32) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.branch = (branch_index & 0x3F) as u8;
            scope.has_branch = true;
        }
    }
}
