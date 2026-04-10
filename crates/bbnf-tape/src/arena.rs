//! `ChunkedArena<T>` — append-only chunked storage.
//!
//! Each chunk is a `Vec<T>` preallocated to `CHUNK_CAPACITY` slots and
//! never reallocated — appends that exceed a chunk's capacity spill
//! into a new chunk. Indexing decodes `(chunk_idx, within_chunk)` via
//! shift + mask. `ChunkedArena::push` is O(1) amortized with no
//! realloc-copy cost.
//!
//! This is the runtime substrate the tape writes into. It subsumes the
//! former `BumpSlab + Vec<T> scratch` two-copy path by construction:
//! the chunk IS the final storage, so there's no intermediate buffer
//! and no copy on commit.

use std::fmt;

/// Number of `TapeRec` slots per chunk. 4096 × 16 bytes = 64 KB per
/// chunk, one L1-data-cache page on typical x86_64 and half an L1
/// page on M-series ARM. Chosen so the arena's per-chunk allocation
/// amortizes over thousands of records — a 100k-record parse produces
/// ~25 chunks, one malloc each.
pub const CHUNK_CAPACITY: usize = 4096;

const CHUNK_SHIFT: usize = 12; // log2(4096) — must match CHUNK_CAPACITY
const CHUNK_MASK: usize = CHUNK_CAPACITY - 1;

/// Append-only chunked storage for fixed-size records.
pub struct ChunkedArena<T> {
    /// Vector of chunks. `chunks[i]` has at most `CHUNK_CAPACITY`
    /// slots. All but the last chunk are full (length ==
    /// `CHUNK_CAPACITY`); the last chunk is the current write target.
    chunks: Vec<Vec<T>>,
    /// Total items pushed across all chunks. Equal to
    /// `chunks[..-1].iter().map(|c| c.len()).sum() + chunks.last().len()`.
    total: usize,
}

impl<T> ChunkedArena<T> {
    /// Construct an empty arena with one preallocated chunk.
    pub fn new() -> Self {
        Self {
            chunks: vec![Vec::with_capacity(CHUNK_CAPACITY)],
            total: 0,
        }
    }

    /// Construct an empty arena sized for `expected` records. Rounds
    /// up to the next chunk boundary.
    pub fn with_capacity(expected: usize) -> Self {
        let chunk_count = expected.div_ceil(CHUNK_CAPACITY).max(1);
        let mut chunks = Vec::with_capacity(chunk_count);
        chunks.push(Vec::with_capacity(CHUNK_CAPACITY));
        Self { chunks, total: 0 }
    }

    /// Append a record; returns the flat index.
    ///
    /// When the current chunk is full, allocate a fresh chunk before
    /// appending. The caller-visible index is monotone — it never
    /// decreases and never reuses a slot.
    #[inline]
    pub fn push(&mut self, value: T) -> usize {
        // SAFETY: chunks is always non-empty (constructors seed one).
        let current = self.chunks.last().expect("ChunkedArena invariant: chunks non-empty");
        if current.len() == CHUNK_CAPACITY {
            self.chunks.push(Vec::with_capacity(CHUNK_CAPACITY));
        }
        let idx = self.total;
        self.chunks
            .last_mut()
            .expect("ChunkedArena invariant: chunks non-empty")
            .push(value);
        self.total += 1;
        idx
    }

    /// Get a record by flat index. Panics on out-of-range indices.
    #[inline]
    pub fn get(&self, idx: usize) -> &T {
        debug_assert!(idx < self.total, "ChunkedArena::get out of range: {}", idx);
        let chunk_idx = idx >> CHUNK_SHIFT;
        let within = idx & CHUNK_MASK;
        &self.chunks[chunk_idx][within]
    }

    /// Get a record by flat index, returning `None` on out-of-range.
    #[inline]
    pub fn try_get(&self, idx: usize) -> Option<&T> {
        if idx >= self.total {
            return None;
        }
        Some(self.get(idx))
    }

    /// Number of records stored.
    #[inline]
    pub fn len(&self) -> usize {
        self.total
    }

    /// Is the arena empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.total == 0
    }

    /// Number of chunks currently allocated.
    pub fn chunk_count(&self) -> usize {
        self.chunks.len()
    }

    /// Iterate every record in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.chunks.iter().flat_map(|c| c.iter())
    }
}

impl<T> Default for ChunkedArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: fmt::Debug> fmt::Debug for ChunkedArena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ChunkedArena")
            .field("total", &self.total)
            .field("chunks", &self.chunks.len())
            .finish()
    }
}
