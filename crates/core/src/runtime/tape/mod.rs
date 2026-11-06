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

use bbnf_ir::registry::{AltBranchId, StructLayout};
use core::marker::PhantomData;
use core::sync::atomic::{AtomicU64, Ordering};

use crate::runtime::builder::StructBuilder;
use crate::runtime::handle::CompoundHandle;

use arena::PayloadArena;
use cursor::{TapeCursor, TapeRef};
use record::{LeafType, TAPE_NONE, TapeOffset, TapeRec};

/// Process-wide source for private Tape builder nonces. One atomic operation is
/// paid at builder construction, not on the `begin_compound` hot path.
static NEXT_TAPE_BUILDER_NONCE: AtomicU64 = AtomicU64::new(1);

#[inline]
fn advance_tape_builder_nonce(current: u64) -> Option<u64> {
    u32::try_from(current).ok()?;
    current.checked_add(1)
}

#[inline]
fn fresh_tape_builder_nonce() -> u32 {
    let nonce = NEXT_TAPE_BUILDER_NONCE
        .fetch_update(
            Ordering::Relaxed,
            Ordering::Relaxed,
            advance_tape_builder_nonce,
        )
        .unwrap_or_else(|_| panic!("TapeStructBuilder exhausted builder nonces"));
    u32::try_from(nonce).expect("validated Tape builder nonce must fit u32")
}

/// Pack one builder nonce and its local scope sequence into a non-semantic
/// capability identity. Both lanes are exact `u32` values, so the mapping is
/// collision-free within the process lifetime.
#[inline]
const fn tape_scope_token(builder_nonce: u32, scope_sequence: u32) -> u64 {
    (builder_nonce as u64) << 32 | scope_sequence as u64
}

/// An in-flight open compound: where its first child sits, the source
/// span start, the rule's exact layout id, and the branch tag
/// stamped while it was open.
#[derive(Debug, Clone, Copy)]
struct OpenScope {
    /// Never-reused identity paired with the returned CompoundHandle.
    scope_token: u64,
    /// Record offset of this compound's first child (== `records.len()`
    /// at `begin_compound`, before any child is appended).
    child_start: u32,
    /// Source byte offset captured at `begin_compound` / via
    /// `record_compound_bounds_start`.
    span_lo: u32,
    span_hi: u32,
    /// Unmatched bounds start for this exact scope.
    bounds_start: Option<u32>,
    /// Exact, untruncated grammar rule identity.
    rule_id: u32,
    /// Structural branch selected while the compound is open.
    branch: Option<AltBranchId>,
}

/// The single generic tape builder. Grammar-agnostic: it dispatches on
/// the [`StructLayout`] handed to `begin_compound`, never on per-grammar
/// route strings.
#[derive(Debug)]
pub struct TapeStructBuilder<'p> {
    records: Vec<TapeRec>,
    /// Exact rule id at each record offset. Leaf entries are ignored.
    rule_ids: Vec<u32>,
    arena: PayloadArena,
    /// Open compounds, innermost last.
    scopes: Vec<OpenScope>,
    /// Private process-unique identity used only to reject cross-builder
    /// capabilities. It is not a grammar or record identifier.
    builder_nonce: u32,
    /// Never-restored local capability sequence. The wider counter lets
    /// `u32::MAX` be issued once; the following begin fails before mutation.
    next_scope_sequence: u64,
    /// Owned parse input. Cursors never accept an independently supplied
    /// source, so recorded byte offsets cannot be rebound to other bytes.
    source: Vec<u8>,
    /// Retains the established builder type shape without borrowing input.
    marker: PhantomData<&'p ()>,
    root: Option<TapeOffset>,
}

/// Transactional checkpoint. Record and arena payloads remain length-marked;
/// the shallow scope stack is cloned so each scope's token, bounds pairing,
/// branch, and span mutations are restored exactly. Scope-token issuance is
/// intentionally not checkpointed: rolled-back handles must stay stale forever.
///
/// The checkpoint itself is linear to prevent one snapshot from resurrecting
/// the same open-scope capabilities more than once.
#[derive(Debug)]
pub struct TapeCheckpoint {
    records_len: usize,
    arena_len: u32,
    scopes: Vec<OpenScope>,
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
            rule_ids: Vec::with_capacity(256),
            arena: PayloadArena::with_capacity(256),
            scopes: Vec::with_capacity(16),
            builder_nonce: fresh_tape_builder_nonce(),
            next_scope_sequence: 1,
            source: Vec::new(),
            marker: PhantomData,
            root: None,
        }
    }

    /// Borrow the tape for lazy-view decode. Record construction and source
    /// provenance remain sealed inside the builder.
    pub fn tape_ref(&self) -> TapeRef<'_> {
        TapeRef::new(&self.records, &self.rule_ids, &self.arena, &self.source)
    }

    /// A cursor at the root compound, or `None` for an empty parse.
    pub fn root_cursor(&self) -> Option<TapeCursor<'_>> {
        let off = self.root?;
        self.cursor_at(off)
    }

    /// A checked cursor at an arbitrary record offset.
    pub fn cursor_at(&self, off: u32) -> Option<TapeCursor<'_>> {
        TapeCursor::new(self.tape_ref(), off)
    }

    pub fn records(&self) -> &[TapeRec] {
        &self.records
    }

    pub fn arena(&self) -> &PayloadArena {
        &self.arena
    }

    #[inline]
    fn next_record_offset(&self) -> u32 {
        u32::try_from(self.records.len()).expect("tape exceeds u32 record-offset domain")
    }

    /// Reserve one exact, non-semantic begin/end capability. The local counter
    /// is deliberately absent from checkpoints, so rollback cannot make a
    /// discarded handle current again.
    #[inline]
    fn reserve_scope_token(&mut self) -> u64 {
        let scope_sequence = u32::try_from(self.next_scope_sequence)
            .unwrap_or_else(|_| panic!("TapeStructBuilder exhausted local scope token domain"));
        let next_scope_sequence = self
            .next_scope_sequence
            .checked_add(1)
            .unwrap_or_else(|| panic!("TapeStructBuilder exhausted local scope token domain"));
        let scope_token = tape_scope_token(self.builder_nonce, scope_sequence);
        self.next_scope_sequence = next_scope_sequence;
        scope_token
    }

    /// Append a leaf record onto the current open compound (or top level).
    #[inline]
    fn push_leaf_rec(&mut self, rec: TapeRec) {
        self.records.push(rec);
        self.rule_ids.push(0);
    }
}

impl<'p> StructBuilder for TapeStructBuilder<'p> {
    type Checkpoint = TapeCheckpoint;

    #[inline]
    fn checkpoint(&self) -> Self::Checkpoint {
        TapeCheckpoint {
            records_len: self.records.len(),
            arena_len: self.arena.len(),
            scopes: self.scopes.clone(),
            root: self.root,
        }
    }

    #[inline]
    fn rollback(&mut self, c: Self::Checkpoint) {
        self.records.truncate(c.records_len);
        self.rule_ids.truncate(c.records_len);
        self.arena.truncate(c.arena_len);
        self.scopes = c.scopes;
        self.root = c.root;
    }

    #[inline]
    fn bind_input(&mut self, input: &[u8]) {
        assert!(
            self.records.is_empty() && self.scopes.is_empty() && self.root.is_none(),
            "input must be bound before tape construction"
        );
        self.source.clear();
        self.source.extend_from_slice(input);
    }

    #[inline]
    fn record_compound_bounds_start(&mut self, offset: u32) {
        let scope = self
            .scopes
            .last_mut()
            .expect("TapeStructBuilder::record_compound_bounds_start without an open scope");
        assert!(
            scope.bounds_start.is_none(),
            "TapeStructBuilder::record_compound_bounds_start duplicated for scope token {} rule {}",
            scope.scope_token,
            scope.rule_id,
        );
        scope.bounds_start = Some(offset);
        if scope.span_lo == TAPE_NONE {
            scope.span_lo = offset;
        }
    }

    #[inline]
    fn record_compound_bounds_end(&mut self, offset: u32) {
        let scope = self
            .scopes
            .last_mut()
            .expect("TapeStructBuilder::record_compound_bounds_end without an open scope");
        let start = scope.bounds_start.unwrap_or_else(|| {
            panic!(
                "TapeStructBuilder::record_compound_bounds_end without a matching start for scope token {} rule {}",
                scope.scope_token, scope.rule_id,
            )
        });
        assert!(
            offset >= start,
            "TapeStructBuilder compound bounds end {offset} precedes start {start} for scope token {} rule {}",
            scope.scope_token,
            scope.rule_id,
        );
        scope.bounds_start = None;
        scope.span_hi = offset;
        if scope.span_lo == TAPE_NONE {
            scope.span_lo = offset;
        }
    }

    #[inline]
    fn begin_compound(&mut self, layout: &StructLayout) -> CompoundHandle {
        let scope_token = self.reserve_scope_token();
        if self.scopes.is_empty() {
            self.root = None;
        }
        self.scopes.push(OpenScope {
            scope_token,
            child_start: self.next_record_offset(),
            span_lo: TAPE_NONE,
            span_hi: TAPE_NONE,
            bounds_start: None,
            rule_id: layout.rule_id,
            branch: None,
        });
        CompoundHandle::new(scope_token, layout.rule_id)
    }

    #[inline]
    fn end_compound(&mut self, handle: CompoundHandle) {
        let open = self
            .scopes
            .last()
            .expect("TapeStructBuilder::end_compound on empty scope stack");
        assert_eq!(
            handle.scope_token(),
            open.scope_token,
            "TapeStructBuilder::end_compound received a stale, cross-builder, or out-of-order handle"
        );
        assert_eq!(
            handle.rule_id(),
            open.rule_id,
            "TapeStructBuilder::end_compound handle/layout rule identity mismatch"
        );
        assert!(
            open.bounds_start.is_none(),
            "TapeStructBuilder::end_compound with unmatched bounds start for scope token {} rule {}",
            open.scope_token,
            open.rule_id,
        );

        let scope = self
            .scopes
            .pop()
            .expect("TapeStructBuilder::end_compound on empty scope stack");
        let child_start = scope.child_start;
        let has_children = self.records.len() > child_start as usize;
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
        let mut rec = TapeRec::open(lo, hi, scope.branch);
        rec.set_has_children(has_children);
        rec.child_off = if has_children { child_start } else { TAPE_NONE };
        let off = self.next_record_offset();
        self.records.push(rec);
        self.rule_ids.push(scope.rule_id);
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
        if value < u64::from(TAPE_NONE) {
            // inline-pack into child_off.
            self.push_leaf_rec(TapeRec::leaf(LeafType::U64, 0, 0, value as u32));
        } else {
            let off = self.arena.write_u64(value);
            // `TAPE_NONE` is reserved, so u32::MAX and wider values ride
            // the arena under an explicit record flag.
            let mut rec = TapeRec::leaf(LeafType::U64, 0, 0, off);
            rec.set_u64_arena_payload();
            self.push_leaf_rec(rec);
        }
    }

    #[inline]
    fn push_leaf_with_u8(&mut self, value: u8) {
        self.push_leaf_rec(TapeRec::leaf(LeafType::U8, 0, 0, u32::from(value)));
    }

    #[inline]
    fn push_leaf_with_bool(&mut self, value: bool) {
        self.push_leaf_rec(TapeRec::leaf(LeafType::Bool, 0, 0, value as u32));
    }

    #[inline]
    fn push_leaf_with_str(&mut self, value: &str) {
        let (off, len) = self.arena.write_str(value);
        // Store len in span_hi for read_str reconstruction.
        self.push_leaf_rec(TapeRec::leaf(LeafType::Str, 0, len, off));
    }

    #[inline]
    fn push_leaf_with_unit(&mut self) {
        self.push_leaf_rec(TapeRec::leaf(LeafType::Unit, 0, 0, TAPE_NONE));
    }

    #[inline]
    fn select_alt_branch(&mut self, branch: AltBranchId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.branch = Some(branch);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::panic::{AssertUnwindSafe, catch_unwind};

    use super::*;

    #[test]
    fn capability_identity_packing_is_exact() {
        assert_eq!(tape_scope_token(1, 1), 0x0000_0001_0000_0001);
        assert_ne!(tape_scope_token(1, 2), tape_scope_token(2, 1));
        assert_eq!(
            tape_scope_token(u32::MAX, u32::MAX),
            u64::MAX,
            "both identity lanes retain their full u32 domains"
        );
    }

    #[test]
    fn builder_nonce_exhaustion_is_detected_without_wrapping() {
        assert_eq!(
            advance_tape_builder_nonce(u64::from(u32::MAX)),
            Some(u64::from(u32::MAX) + 1)
        );
        assert_eq!(advance_tape_builder_nonce(u64::from(u32::MAX) + 1), None);
        assert_eq!(advance_tape_builder_nonce(u64::MAX), None);
    }

    #[test]
    fn local_scope_exhaustion_rejects_before_tape_mutation() {
        let mut builder = TapeStructBuilder::new();
        builder.next_scope_sequence = u64::from(u32::MAX);

        let final_token = builder.reserve_scope_token();
        assert_eq!(final_token as u32, u32::MAX);
        let exhausted_sequence = builder.next_scope_sequence;

        let rejected = catch_unwind(AssertUnwindSafe(|| {
            builder.reserve_scope_token();
        }));
        assert!(rejected.is_err());
        assert_eq!(builder.next_scope_sequence, exhausted_sequence);
        assert!(builder.records.is_empty());
        assert!(builder.rule_ids.is_empty());
        assert!(builder.scopes.is_empty());
        assert!(builder.root.is_none());
    }
}
