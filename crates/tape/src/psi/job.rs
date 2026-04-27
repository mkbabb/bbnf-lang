//! Decode-job descriptors — [`PayloadKind`] taxonomy and [`PayloadJob`] record.
//!
//! Stage A produces these jobs; Stage B consumes them. The struct is
//! `#[repr(C)]` so the in-memory shape matches the codegen-time literal
//! the emitter materialises.

/// Terminal scanner kind selecting how the Stage-B worker decodes a
/// `PayloadJob`'s `input_lo..input_hi` slice.
///
/// One byte wide. Variants are added as the emitter grows new payload
/// shapes; every variant must have a registered scanner in
/// [`super::PayloadStream::fill_columns`].
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PayloadKind {
    /// `f64` from a textual numeric — JSON `number`, CSS `<number>`,
    /// Sheets `=NUMBER(...)`. Routed through
    /// `parse_that::scan_number_f64`.
    F64 = 0,
    /// `u8` discriminant — CSS unit (`px` = 1, `em` = 2, …), CSS
    /// keyword enum branch index, Sheets operator discriminant.
    U8 = 1,
    /// `bool` — `true` / `false` literal. The decoded value is
    /// `0` / `1` written into the `pay_narrow` column.
    Bool = 2,
    /// 32-bit hex colour — `#rrggbbaa` with `a = 0xFF` default.
    /// Lands in `pay_narrow`.
    HexU32 = 3,
    /// `i64` integer literal — BBNF `int_lit`, Sheets `INT64(...)`.
    /// Routed through `parse_that::parse_i64_from_bytes`. Lands in
    /// `pay_wide` (8 bytes).
    I64 = 4,
    /// Decoded JSON string — UTF-8 byte slice with escapes resolved.
    /// Stage B writes through `decode_json_string_to_arena` (AV.4.3
    /// `simdjson`-scale path), framed as `(len: u32 LE, bytes)` in
    /// `pay_agg`.
    String = 5,
    /// Oversized aggregate (> 16 bytes — CSS colour functions).
    /// Stage B copies the source slice verbatim into `pay_agg` at the
    /// pre-allocated arena slot; the width is recovered from the
    /// grammar's payload-layout table at read time.
    AggregateLarge = 6,
}

impl PayloadKind {
    /// Total count of variants — used to size scanner dispatch tables.
    pub const COUNT: usize = 7;

    /// Convert a raw byte to a `PayloadKind`, returning `None` for
    /// unknown discriminants. Used by the emitter when it materialises
    /// a `PayloadJob` literal at codegen time.
    #[inline]
    pub const fn from_u8(b: u8) -> Option<Self> {
        match b {
            0 => Some(Self::F64),
            1 => Some(Self::U8),
            2 => Some(Self::Bool),
            3 => Some(Self::HexU32),
            4 => Some(Self::I64),
            5 => Some(Self::String),
            6 => Some(Self::AggregateLarge),
            _ => None,
        }
    }

    /// Total byte width of this kind's encoded payload in `pay_agg`.
    /// `String` / `AggregateLarge` are variable-width (dictated by
    /// the matched input slice); the helper returns `0` for those so
    /// the capacity-reservation pass keys off the matched length.
    ///
    /// AW-III.W1 unified arena emission: every non-variable scalar
    /// payload lands in [`crate::columns::Columns::pay_agg`] as
    /// fixed-width little-endian bytes. The single-arena path means
    /// downstream readers (`payload_bytes`, `payload_scalar`) see one
    /// source of truth; the legacy `pay_narrow` / `pay_wide` columns
    /// survive only for pre-AW unit-test plumbing and the AW-IV
    /// bulk-typed visitors.
    #[inline]
    pub const fn arena_byte_width(self) -> usize {
        match self {
            Self::U8 | Self::Bool => 1,
            Self::HexU32 => 4,
            Self::F64 | Self::I64 => 8,
            Self::String | Self::AggregateLarge => 0,
        }
    }
}

/// One decode unit produced by Stage A and consumed by Stage B.
///
/// Layout is `#[repr(C)]` so the in-memory shape matches the codegen-
/// time literal the emitter produces. AW-III.W1 widened the
/// `column_idx: u8` slot to a `u32` `arena_offset` because the
/// unified arena emission path needs byte offsets that range over
/// the entire `pay_agg` length (megabytes); the prior 8-bit slot
/// only sufficed under the narrow / wide column-rank scheme. Total
/// size is 20 bytes; chunks of 3 jobs occupy each cache line.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PayloadJob {
    /// Structural record index in [`crate::columns::Columns`]. The
    /// Stage-B worker updates the matching record's `child_off` slot
    /// to point at the arena offset where the decoded payload lands.
    pub rec_idx: u32,
    /// Source byte range start — inclusive. The Stage-B scanner reads
    /// `input[input_lo..input_hi]`.
    pub input_lo: u32,
    /// Source byte range end — exclusive.
    pub input_hi: u32,
    /// Pre-allocated arena byte offset in
    /// [`crate::columns::Columns::pay_agg`]. Stage A monotonically
    /// advances the arena cursor per job so every offset is unique
    /// and bounds-disjoint from its peers; the Stage-B writer stamps
    /// the decoded value at this offset.
    pub arena_offset: u32,
    /// Terminal scanner selector.
    pub kind: PayloadKind,
    /// Padding to align the struct on a natural 4-byte boundary; the
    /// bytes are zero-initialised at construction.
    pub _pad: [u8; 3],
}

impl PayloadJob {
    /// Construct a `PayloadJob` with the padding bytes zero-initialised.
    /// The const-eval-friendly form the emitter uses when materialising
    /// a `static [PayloadJob; N]` array at codegen time.
    #[inline]
    pub const fn new(
        rec_idx: u32,
        input_lo: u32,
        input_hi: u32,
        kind: PayloadKind,
        arena_offset: u32,
    ) -> Self {
        Self {
            rec_idx,
            input_lo,
            input_hi,
            arena_offset,
            kind,
            _pad: [0; 3],
        }
    }

    /// Length of the source byte slice this job covers — `input_hi -
    /// input_lo`. Cheap helper for capacity estimation in Stage B's
    /// arena pre-reservation.
    #[inline]
    pub const fn input_len(&self) -> u32 {
        self.input_hi - self.input_lo
    }
}

// Compile-time guarantees the layout the emitter relies on.
const _PAYLOAD_JOB_SIZE: () = {
    assert!(std::mem::size_of::<PayloadJob>() == 20);
};
const _PAYLOAD_JOB_ALIGN: () = {
    assert!(std::mem::align_of::<PayloadJob>() == 4);
};
