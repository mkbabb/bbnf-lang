//! AY.W3a — runtime handles for the Value API substrate.
//!
//! Two `Copy` handle types underpin the `to_value` + `get_by_path`
//! lanes emitted in AY.W3b. They are intentionally schema-free — the
//! per-grammar `<Grammar>Value` enum interprets them in context.
//!
//! # `StringHandle`
//!
//! Every string-shaped leaf in a parsed tree resolves through a
//! `StringHandle`. The handle encodes two facts in a single `u64`:
//! the originating byte offset in the caller's input buffer (the
//! upper 48 bits of `loc`) and a single bit indicating whether the
//! payload was re-hosted in the parse arena (the top bit,
//! [`StringHandle::ARENA_BIT`]). The discriminator distinguishes
//! zero-copy borrows — where [`raw_offset`] indexes directly into
//! the input `&str` — from arena-decoded payloads emitted when a
//! string requires unescaping or case-folding.
//!
//! # `CompoundHandle`
//!
//! Compound (object / array / struct) values point into the
//! grammar-specific struct arena. `record_offset` is the absolute
//! arena record offset; `record_count` is the compound's record span.
//!
//! Both handles are `Copy`, 16-or-fewer bytes, and ABI-stable — the
//! emitter plants them directly into `<Grammar>Value` variants.
//!
//! [`raw_offset`]: StringHandle::raw_offset

/// Handle pointing at a string payload in either the caller's input
/// buffer or the parser's arena.
///
/// `loc` packs the byte offset (bits 0-62) with the arena-hosted flag
/// (bit 63, [`ARENA_BIT`](Self::ARENA_BIT)). `len` is the byte length
/// of the payload — valid for both the input-borrow case and the
/// arena case (the arena is a contiguous byte region).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringHandle {
    /// Packed location — upper bit is [`Self::ARENA_BIT`], the
    /// remaining 63 bits encode the byte offset.
    pub loc: u64,
    /// Length of the string payload in bytes.
    pub len: u32,
}

/// Handle pointing at a compound record span inside a parsed
/// grammar-specific arena.
///
/// `record_offset` is the absolute record offset of the compound's
/// header record; `record_count` spans the entire subtree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompoundHandle {
    /// Absolute record offset of the compound's header record.
    pub record_offset: u64,
    /// Number of arena records the compound spans, inclusive of the
    /// header.
    pub record_count: u32,
}

impl StringHandle {
    /// Bit flag indicating the payload lives in the parser's arena
    /// rather than the caller's input buffer. Set via
    /// [`StringHandle::arena`]; probed via
    /// [`StringHandle::is_arena`].
    pub const ARENA_BIT: u64 = 1 << 63;

    /// Construct a handle pointing at a slice of the caller's input
    /// buffer. `offset` must fit in 63 bits (i.e. the top bit must
    /// be zero — byte offsets in a `&str` are naturally bounded by
    /// `isize::MAX`, so this is a debug-only invariant).
    #[inline]
    pub const fn borrow(offset: u64, len: u32) -> Self {
        debug_assert!(offset & Self::ARENA_BIT == 0);
        Self { loc: offset, len }
    }

    /// Construct a handle pointing at an arena-hosted payload.
    /// `offset` is the byte offset inside the arena buffer.
    #[inline]
    pub const fn arena(offset: u64, len: u32) -> Self {
        debug_assert!(offset & Self::ARENA_BIT == 0);
        Self {
            loc: offset | Self::ARENA_BIT,
            len,
        }
    }

    /// `true` iff the payload is arena-hosted (produced by an
    /// unescape / fold / normalise pass), `false` for zero-copy
    /// borrows from the caller's input.
    #[inline]
    pub const fn is_arena(&self) -> bool {
        self.loc & Self::ARENA_BIT != 0
    }

    /// Extract the raw byte offset, masking the arena flag bit.
    /// Valid as an index into whichever buffer [`is_arena`] selects.
    ///
    /// [`is_arena`]: Self::is_arena
    #[inline]
    pub const fn raw_offset(&self) -> u64 {
        self.loc & !Self::ARENA_BIT
    }

    /// Length of the payload in bytes.
    #[inline]
    pub const fn len(&self) -> u32 {
        self.len
    }

    /// `true` iff the payload is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl CompoundHandle {
    /// Construct a compound handle.
    #[inline]
    pub const fn new(record_offset: u64, record_count: u32) -> Self {
        Self {
            record_offset,
            record_count,
        }
    }

    /// Absolute record offset of the compound's header.
    #[inline]
    pub const fn record_offset(&self) -> u64 {
        self.record_offset
    }

    /// Record count of the compound subtree (header inclusive).
    #[inline]
    pub const fn record_count(&self) -> u32 {
        self.record_count
    }
}
