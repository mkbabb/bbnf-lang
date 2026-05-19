mod assembler;
pub mod event_grammar;
#[cfg(test)]
mod event_grammar_tests;
mod offsets;

use std::marker::PhantomData;
use std::sync::atomic::{AtomicU64, Ordering};

pub use assembler::{CapacityPlan, TapeBuilder};
pub use event_grammar::{AnyGrammar, EventGrammar};
pub use offsets::OffsetTapeStats;

static NEXT_TAPE_ID: AtomicU64 = AtomicU64::new(1);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
#[repr(transparent)]
pub struct OffsetFlags(pub u8);

impl OffsetFlags {
    pub const NONE: Self = Self(0);
    pub const HAS_ESC: u8 = 0x01;
    pub const HAS_CONTROL: u8 = 0x02;

    pub const fn with(self, bit: u8) -> Self {
        Self(self.0 | bit)
    }

    pub const fn contains(self, bit: u8) -> bool {
        self.0 & bit != 0
    }

    pub const fn bits(self) -> u8 {
        self.0
    }
}

pub struct PayloadArena {
    bytes: Vec<u8>,
    #[cfg(any(test, feature = "bench-counters"))]
    writes: u32,
    #[cfg(any(test, feature = "bench-counters"))]
    allocations: u32,
}

impl PayloadArena {
    pub fn empty() -> Self {
        Self {
            bytes: Vec::new(),
            #[cfg(any(test, feature = "bench-counters"))]
            writes: 0,
            #[cfg(any(test, feature = "bench-counters"))]
            allocations: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) -> u32 {
        let offset = checked_u32(self.bytes.len());
        #[cfg(any(test, feature = "bench-counters"))]
        let old_capacity = self.bytes.capacity();
        self.bytes.extend_from_slice(bytes);
        #[cfg(any(test, feature = "bench-counters"))]
        {
            self.writes += 1;
            if self.bytes.capacity() != old_capacity {
                self.allocations += 1;
            }
        }
        offset
    }

    #[cfg(any(test, feature = "bench-counters"))]
    pub fn write_count(&self) -> u32 {
        self.writes
    }

    #[cfg(any(test, feature = "bench-counters"))]
    pub fn allocation_count(&self) -> u32 {
        self.allocations
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct TapeId(pub u64);

pub struct Tape<'input> {
    source: &'input [u8],
    offsets: Vec<u32>,
    flag_cursors: Vec<u32>,
    flag_values: Vec<u8>,
    payloads: PayloadArena,
    id: TapeId,
}

impl<'input> Tape<'input> {
    pub(crate) fn from_offsets(
        source: &'input [u8],
        offsets: Vec<u32>,
        flag_cursors: Vec<u32>,
        flag_values: Vec<u8>,
        payloads: PayloadArena,
    ) -> Self {
        debug_assert_eq!(flag_cursors.len(), flag_values.len());
        Self {
            source,
            offsets,
            flag_cursors,
            flag_values,
            payloads,
            id: TapeId(NEXT_TAPE_ID.fetch_add(1, Ordering::Relaxed)),
        }
    }

    pub fn source(&self) -> &'input [u8] {
        self.source
    }

    pub fn offsets(&self) -> &[u32] {
        &self.offsets
    }

    pub fn flag_cursors(&self) -> &[u32] {
        &self.flag_cursors
    }

    pub fn flag_values(&self) -> &[u8] {
        &self.flag_values
    }

    pub fn offset_at(&self, cursor: u32) -> Option<usize> {
        self.offsets
            .get(cursor as usize)
            .map(|offset| *offset as usize)
    }

    pub fn flags_at(&self, cursor: u32) -> Option<OffsetFlags> {
        self.flag_cursors
            .binary_search(&cursor)
            .ok()
            .and_then(|index| self.flag_values.get(index).copied())
            .map(OffsetFlags)
    }

    pub fn offset_bytes(&self) -> usize {
        self.offsets.len() * std::mem::size_of::<u32>()
    }

    pub fn flag_bytes(&self) -> usize {
        self.flag_cursors.len() * std::mem::size_of::<u32>() + self.flag_values.len()
    }

    pub fn offset_capacity_bytes(&self) -> usize {
        self.offsets.capacity() * std::mem::size_of::<u32>()
            + self.flag_cursors.capacity() * std::mem::size_of::<u32>()
            + self.flag_values.capacity()
    }

    pub fn payloads(&self) -> &PayloadArena {
        &self.payloads
    }

    pub fn id(&self) -> TapeId {
        self.id
    }
}

pub struct ValueRef<'doc, 'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar> {
    tape: &'doc Tape<'input>,
    cursor: u32,
    _kind: PhantomData<fn() -> K>,
    _grammar: PhantomData<fn() -> G>,
    _input: PhantomData<&'input [u8]>,
}

impl<'doc, 'input: 'doc, K, G: EventGrammar> Copy for ValueRef<'doc, 'input, K, G> {}

impl<'doc, 'input: 'doc, K, G: EventGrammar> Clone for ValueRef<'doc, 'input, K, G> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'doc, 'input: 'doc, K, G: EventGrammar> ValueRef<'doc, 'input, K, G> {
    pub fn new(tape: &'doc Tape<'input>, cursor: u32) -> Self {
        Self {
            tape,
            cursor,
            _kind: PhantomData,
            _grammar: PhantomData,
            _input: PhantomData,
        }
    }

    pub fn erase(self) -> ValueRef<'doc, 'input, AnyKind, G> {
        ValueRef::new(self.tape, self.cursor)
    }

    pub fn tape(&self) -> &'doc Tape<'input> {
        self.tape
    }

    pub fn cursor(&self) -> u32 {
        self.cursor
    }

    pub fn index(&self) -> u32 {
        self.cursor
    }

    pub fn offset(&self) -> usize {
        self.tape
            .offset_at(self.cursor)
            .expect("ValueRef cursor must point into sealed offset tape")
    }
}

pub enum AnyKind {}

pub trait DocumentView<'a> {
    type Root: 'a;
    fn root_value(&'a self) -> Self::Root;
    fn tape_id(&self) -> TapeId;
    fn source(&'a self) -> &'a [u8];
}

pub(crate) fn checked_u32(value: usize) -> u32 {
    debug_assert!(u32::try_from(value).is_ok());
    value as u32
}
