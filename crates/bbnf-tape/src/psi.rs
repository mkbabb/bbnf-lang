//! Payload-Side Information (PSI) — Tranche AV Phase 4 (AV.4.1 / AV.4.2).
//!
//! # Architectural role
//!
//! The Dispatch Tape Automaton (Stage A — `crates/bbnf-tape/src/dta.rs`)
//! emits the tape's structural skeleton in a single linear byte pass:
//! every record lands with its kind, variant, and `span_lo` populated.
//! Scalar leaves whose payloads need decoding (`f64`, `u8`, `bool`,
//! `hex u32`, decoded JSON strings, oversized colour-function aggregates)
//! are not decoded inline — that work is deferred to **Stage B**, which
//! consumes a `Vec<PayloadJob>` describing exactly what to decode and
//! where to write the result.
//!
//! ```text
//! input bytes ─► Stage A (DTA) ─► Tape skeleton + Vec<PayloadJob>
//!                                                ▼
//!                                       Stage B (rayon)
//!                                                ▼
//!                                       Tape with payloads filled
//! ```
//!
//! # PSI stream construction (AV.4.1)
//!
//! Stage A emits one [`PayloadJob`] per scalar leaf that requires
//! payload decoding. The job carries:
//!
//! - `rec_idx` — the structural record's index in
//!   [`Columns`](crate::columns::Columns), set by the DTA at push time.
//! - `input_lo` / `input_hi` — the source-byte slice the scanner reads.
//! - `kind` — the [`PayloadKind`] selecting the terminal scanner.
//! - `column_idx` — the slot in the active payload column where the
//!   decoded value lands. The DTA pre-allocates column ranks for every
//!   job so workers see disjoint write targets.
//!
//! The struct is `#[repr(C)]` and 16 bytes wide so a `Vec<PayloadJob>`
//! has the same cache-line behaviour as a `Vec<TapeRec>` — four jobs
//! per 64 B line. Stage B's rayon stride is chosen to honour this
//! alignment and avoid false sharing across workers.
//!
//! Capacity is derived from
//! [`GrammarProfile::leaves_per_input_byte`](crate::profile::GrammarProfile::leaves_per_input_byte)
//! `× input.len()`. For scalar-sparse grammars (BBNF, Sheets), this is
//! 1–2% of input bytes; for scalar-dense grammars (canada.json), up to
//! 20%. A single up-front allocation, no growth thrash.
//!
//! # Stage B rayon payload fill (AV.4.2)
//!
//! [`PayloadStream::fill_columns`] dispatches the decoded values into
//! the matching typed payload column. The driver uses the grammar's
//! [`parallel_break_even_bytes`](crate::profile::GrammarProfile::parallel_break_even_bytes)
//! threshold from the [`GrammarProfile`](crate::profile::GrammarProfile)
//! to choose between the sequential walk and the rayon
//! `par_chunks_mut`-driven parallel walk. Both paths write into the
//! same column slots — the API is uniform; only the iterator differs.
//!
//! Cache-line alignment: [`Self::CHUNK_RECS`] is `4` so each rayon
//! chunk owns exactly one 64 B cache line of `PayloadJob`s. False
//! sharing on the *job* stream is structurally impossible (workers
//! own disjoint chunks), and false sharing on the *column* stream is
//! impossible by construction — every job's `column_idx` is unique
//! because the DTA assigns ranks in push order. The only contention
//! risk is across-line tearing on the destination columns when two
//! workers write adjacent slots in `pay_narrow` / `pay_wide`. That is
//! benign on every architecture in scope (x86/ARM both guarantee
//! aligned-store atomicity at the word boundary).

use crate::columns::Columns;
use crate::profile::GrammarProfile;

#[cfg(feature = "rayon")]
use rayon::iter::ParallelIterator;
#[cfg(feature = "rayon")]
use rayon::slice::ParallelSlice;

/// Terminal scanner kind selecting how the Stage-B worker decodes a
/// `PayloadJob`'s `input_lo..input_hi` slice.
///
/// One byte wide. Variants are added as the emitter grows new payload
/// shapes; every variant must have a registered scanner in
/// [`PayloadStream::fill_columns`].
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
    /// payload lands in [`Columns::pay_agg`] as fixed-width little-
    /// endian bytes. The single-arena path means downstream readers
    /// (`payload_bytes`, `payload_scalar`) see one source of truth;
    /// the legacy `pay_narrow` / `pay_wide` columns survive only for
    /// pre-AW unit-test plumbing and the AW-IV bulk-typed visitors.
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
    /// Structural record index in [`Columns`]. The Stage-B worker
    /// updates the matching record's `child_off` slot to point at
    /// the arena offset where the decoded payload lands.
    pub rec_idx: u32,
    /// Source byte range start — inclusive. The Stage-B scanner reads
    /// `input[input_lo..input_hi]`.
    pub input_lo: u32,
    /// Source byte range end — exclusive.
    pub input_hi: u32,
    /// Pre-allocated arena byte offset in [`Columns::pay_agg`].
    /// Stage A monotonically advances the arena cursor per job so
    /// every offset is unique and bounds-disjoint from its peers; the
    /// Stage-B writer stamps the decoded value at this offset.
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

/// The PSI stream — a `Vec<PayloadJob>` plus capacity-estimation
/// helpers that bridge Stage A and Stage B.
///
/// Owned by the parser between Stage A and Stage B. After Stage B
/// drains the stream into the column substrate, the stream is
/// discarded — no per-parse allocation survives the Stage-B closure.
#[derive(Debug, Default)]
pub struct PayloadStream {
    /// Decode jobs in push order — the order Stage A emitted them.
    /// Stage B consumes either sequentially or via rayon
    /// `par_chunks_mut` per the fingerprint gate.
    jobs: Vec<PayloadJob>,
}

impl PayloadStream {
    /// Cache-line stride: number of `PayloadJob`s that fit in a
    /// 64-byte line. The rayon Stage-B walk uses this as the chunk
    /// size so every worker owns a whole cache line.
    pub const CHUNK_RECS: usize = 64 / std::mem::size_of::<PayloadJob>();

    /// Construct an empty stream. Used by tests; production callers
    /// reach for [`Self::with_capacity_for`] to pre-allocate from the
    /// grammar profile.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Pre-allocate from the grammar's
    /// [`leaves_per_input_byte`](GrammarProfile::leaves_per_input_byte)
    /// estimate × input length. The estimate is conservative (overshoots
    /// for scalar-dense grammars such as canada.json), so growth thrash
    /// during Stage A is structurally avoided.
    #[inline]
    pub fn with_capacity_for(profile: &GrammarProfile, input_len: usize) -> Self {
        let estimate = (profile.leaves_per_input_byte * input_len as f32) as usize;
        Self {
            jobs: Vec::with_capacity(estimate),
        }
    }

    /// Append a job to the stream. The DTA emitter calls this once
    /// per scalar leaf during Stage A; the cost is one store per leaf
    /// plus the amortised Vec growth (zero growths if the capacity
    /// was pre-allocated correctly).
    #[inline]
    pub fn push(&mut self, job: PayloadJob) {
        self.jobs.push(job);
    }

    /// Read-only view of the job stream. Stage B and tests use this.
    #[inline]
    pub fn jobs(&self) -> &[PayloadJob] {
        &self.jobs
    }

    /// Number of jobs in the stream.
    #[inline]
    pub fn len(&self) -> usize {
        self.jobs.len()
    }

    /// Whether the stream is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.jobs.is_empty()
    }

    /// Truncate the job stream to `new_len` jobs. Used by the DTA
    /// walker's `AltLinear` backtracking to discard payload writes
    /// from a failed branch before probing the next one.
    #[inline]
    pub fn truncate(&mut self, new_len: usize) {
        self.jobs.truncate(new_len);
    }

    /// Whether the parallel Stage-B path should fire for an input of
    /// `input_len` bytes given the grammar's profile.
    ///
    /// Returns `true` iff `input_len ≥ profile.parallel_break_even_bytes`
    /// AND the rayon feature is enabled at compile time AND the stream
    /// has enough jobs to warrant chunking (≥ 2 chunks).
    ///
    /// When `false`, [`Self::fill_columns`] runs the sequential walk;
    /// when `true`, it dispatches `rayon::par_chunks_mut` over the
    /// PSI stream.
    #[inline]
    pub fn should_parallelise(&self, profile: &GrammarProfile, input_len: usize) -> bool {
        if profile.parallel_break_even_bytes == 0 {
            return false;
        }
        if input_len < profile.parallel_break_even_bytes as usize {
            return false;
        }
        self.jobs.len() >= 2 * Self::CHUNK_RECS
    }

    /// Drain the stream into the column substrate.
    ///
    /// For each job, runs the matching scanner over `input[input_lo..
    /// input_hi]` and writes the decoded value into the slot
    /// pre-allocated by Stage A. The single API serves both the
    /// sequential and parallel paths; the dispatch fork on
    /// [`Self::should_parallelise`] decides which iterator the closure
    /// runs over.
    ///
    /// On parallel dispatch each worker owns a [`Self::CHUNK_RECS`]-
    /// sized run of jobs — exactly one cache line — so worker stores
    /// into the destination columns never share a 64 B line on the
    /// PSI side. Cross-worker contention on the destination columns is
    /// avoided by the DTA's pre-allocated `column_idx` ranks: every
    /// job has a unique slot.
    ///
    /// Returns the number of jobs processed (always equal to
    /// `self.len()`); the caller may discard the stream after the
    /// call.
    pub fn fill_columns(
        &self,
        input: &[u8],
        columns: &mut Columns,
        profile: &GrammarProfile,
    ) -> usize {
        let parallel = self.should_parallelise(profile, input.len());
        if parallel {
            self.fill_parallel(input, columns)
        } else {
            self.fill_sequential(input, columns)
        }
    }

    /// Sequential Stage-B walk. The dispatch fork in
    /// [`Self::fill_columns`] selects this path when the parallel
    /// break-even gate is not cleared.
    ///
    /// Same write strategy as the parallel path — disjoint slot
    /// writes via [`ColumnCells`] — so the two paths share an
    /// observable behaviour. The match on [`PayloadKind`] compiles
    /// to a jump table; the body of [`write_decoded`] is the same
    /// scanner ladder both paths execute.
    fn fill_sequential(&self, input: &[u8], columns: &mut Columns) -> usize {
        let caps = self.required_column_capacities();
        caps.reserve(columns);
        let cells = ColumnCells::from(&mut *columns);
        // SAFETY: same disjointness invariant as the parallel path,
        // exercised serially. Every job's `column_idx` is unique per
        // `PayloadKind`; the columns are pre-sized to admit every
        // slot. The closure has exclusive `&mut Columns` access for
        // the duration of the walk so the cells outlive every write.
        for job in &self.jobs {
            unsafe {
                write_decoded(job, input, &cells);
            }
        }
        self.jobs.len()
    }

    /// Parallel Stage-B walk via rayon `par_chunks` over the PSI
    /// stream. Each worker decodes its chunk's jobs and writes the
    /// decoded values directly into the matching column slots.
    ///
    /// **Safety contract.** Stage A guarantees every job's
    /// `column_idx` is unique per `PayloadKind` — a slot in
    /// `pay_narrow`, `pay_wide`, or `pay_agg` is touched by exactly
    /// one job. The columns are pre-resized to the required
    /// capacities by [`Self::required_column_capacities`], so every
    /// slot is in-bounds and disjoint across workers. Writes are
    /// raw-pointer stores guarded by this disjointness invariant —
    /// no `&mut T` aliases ever co-exist across threads.
    ///
    /// Cache lines: [`Self::CHUNK_RECS`] is 4, so each rayon chunk
    /// owns one 64 B line of `PayloadJob`s on the read side.
    /// Destination column writes can land on shared cache lines
    /// (adjacent `column_idx` values), but the writes themselves are
    /// disjoint and the underlying architectures (x86, ARM) tolerate
    /// concurrent stores to different bytes within a line at the
    /// hardware-coherence level.
    #[cfg(feature = "rayon")]
    fn fill_parallel(&self, input: &[u8], columns: &mut Columns) -> usize {
        let caps = self.required_column_capacities();
        caps.reserve(columns);
        // Capture raw pointers + lengths so the parallel closure can
        // write into disjoint slots without holding a `&mut Vec<_>`
        // across threads. The disjointness invariant comes from
        // Stage A's per-kind monotonic `column_idx` allocation.
        let cells = ColumnCells::from(&mut *columns);
        // SAFETY: every job's `column_idx` is unique per `PayloadKind`
        // (Stage A invariant), the columns are pre-sized to admit
        // every job's slot (caps.reserve above), and the cell pointers
        // outlive the par_chunks closure (cells captured by ref into
        // each thread).
        self.jobs
            .par_chunks(Self::CHUNK_RECS)
            .for_each(|chunk| {
                for job in chunk {
                    unsafe {
                        write_decoded(job, input, &cells);
                    }
                }
            });
        self.jobs.len()
    }

    /// Sequential fallback when the rayon feature is disabled at
    /// compile time. Behaviour is identical to [`Self::fill_sequential`]
    /// — the parallel path collapses to the same code.
    #[cfg(not(feature = "rayon"))]
    fn fill_parallel(&self, input: &[u8], columns: &mut Columns) -> usize {
        self.fill_sequential(input, columns)
    }

    /// Compute the arena capacity required to host every job's
    /// pre-allocated slot.
    ///
    /// AW-III.W1 unified arena emission: every job's decoded payload
    /// lands in [`Columns::pay_agg`] at the byte offset Stage A
    /// allocated via the arena cursor. The offset+width per job is
    /// the upper-bound on `pay_agg.len()`. Variable-width payloads
    /// (`String`, `AggregateLarge`) reserve `input_len` bytes; fixed-
    /// width scalars reserve [`PayloadKind::arena_byte_width`].
    fn required_column_capacities(&self) -> ColumnCapacities {
        let mut caps = ColumnCapacities::default();
        for job in &self.jobs {
            let width = match job.kind.arena_byte_width() {
                0 => job.input_len() as usize,
                w => w,
            };
            caps.arena = caps.arena.max(job.arena_offset as usize + width);
        }
        caps
    }
}

/// Arena capacity hint derived from the PSI stream.
///
/// Stage B reserves enough space in [`Columns::pay_agg`] to land
/// every job's decoded payload at its allocated byte offset before
/// any decode runs. The reservation is `resize`-with-zero so
/// subsequent slot writes land in pre-existing memory — no growth
/// during the Stage-B walk.
///
/// AW-III.W1: every payload kind serialises to `pay_agg`; the prior
/// `narrow` / `wide` capacity slots are gone alongside the column
/// emission paths.
#[derive(Debug, Default)]
struct ColumnCapacities {
    arena: usize,
}

impl ColumnCapacities {
    fn reserve(&self, columns: &mut Columns) {
        if self.arena > columns.pay_agg.len() {
            columns.pay_agg.resize(self.arena, 0);
        }
    }
}

/// Raw cell pointers + lengths into a [`Columns::pay_agg`] arena.
///
/// Captured by [`PayloadStream::fill_sequential`] /
/// [`PayloadStream::fill_parallel`] before the walk so workers can
/// write into disjoint byte ranges without holding a `&mut Vec<_>`
/// across thread boundaries. The disjointness invariant — every
/// job's `(arena_offset, width)` pair is unique — comes from Stage
/// A's monotonic per-job allocation, so two writes never target the
/// same byte.
///
/// The struct is `Send + Sync` because raw pointers carry no `Send`
/// inheritance constraint; the safety contract lives at the
/// `unsafe` write site.
///
/// AW-III.W1: the unified arena emission path collapsed the prior
/// per-kind cell trio (narrow / wide / agg) to a single agg pointer —
/// every payload kind serialises into `pay_agg`.
#[derive(Clone, Copy)]
struct ColumnCells {
    pay_agg: *mut u8,
    pay_agg_len: usize,
}

// SAFETY: `ColumnCells` carries raw pointers into a `&mut Columns`
// that outlives the `par_chunks` walk — the `fill_parallel` /
// `fill_sequential` callers hold exclusive `&mut Columns` access for
// the entire duration of the closure, and the disjointness invariant
// above guarantees no two concurrent writes touch the same address.
unsafe impl Send for ColumnCells {}
unsafe impl Sync for ColumnCells {}

impl From<&mut Columns> for ColumnCells {
    fn from(columns: &mut Columns) -> Self {
        Self {
            pay_agg: columns.pay_agg.as_mut_ptr(),
            pay_agg_len: columns.pay_agg.len(),
        }
    }
}

/// Decode `job`'s source slice and write the result into the arena
/// (`pay_agg`) slot at `job.column_idx`.
///
/// AW-III.W1 unified arena emission: every payload kind serialises
/// to little-endian bytes in [`Columns::pay_agg`]. Downstream
/// consumers read via `payload_bytes(rec, width)` /
/// `payload_scalar::<T>` keyed by the record's `child_off` (set to
/// `job.column_idx` at Stage A by the walker).
///
/// # Safety
///
/// - `cells` must point at a valid `Columns` substrate that outlives
///   this call — established by [`PayloadStream::fill_sequential`] /
///   [`PayloadStream::fill_parallel`] holding a `&mut Columns`
///   throughout.
/// - Every job's `(kind, column_idx)` must be unique across all
///   concurrent invocations — established by Stage A's per-kind
///   monotonic `column_idx` allocation.
/// - `column_idx` must address a pre-allocated slot — established by
///   [`PayloadStream::required_column_capacities`] +
///   [`ColumnCapacities::reserve`] sizing the arena to admit every
///   job before the walk begins.
#[inline]
unsafe fn write_decoded(job: &PayloadJob, input: &[u8], cells: &ColumnCells) {
    let lo = job.input_lo as usize;
    let hi = job.input_hi as usize;
    let slice = &input[lo..hi];
    let dst_off = job.arena_offset as usize;
    match job.kind {
        PayloadKind::F64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<f64>().unwrap_or(0.0).to_bits();
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::I64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            let bits = s.parse::<i64>().unwrap_or(0) as u64;
            let bytes = bits.to_le_bytes();
            debug_assert!(dst_off + 8 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 8);
            }
        }
        PayloadKind::U8 => {
            let value = slice.first().copied().unwrap_or(0);
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::Bool => {
            let value: u8 = if slice.eq_ignore_ascii_case(b"true") { 1 } else { 0 };
            debug_assert!(dst_off + 1 <= cells.pay_agg_len);
            unsafe {
                *cells.pay_agg.add(dst_off) = value;
            }
        }
        PayloadKind::HexU32 => {
            let value = parse_hex_u32(slice);
            let bytes = value.to_le_bytes();
            debug_assert!(dst_off + 4 <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(bytes.as_ptr(), cells.pay_agg.add(dst_off), 4);
            }
        }
        PayloadKind::String | PayloadKind::AggregateLarge => {
            debug_assert!(dst_off + slice.len() <= cells.pay_agg_len);
            unsafe {
                std::ptr::copy_nonoverlapping(
                    slice.as_ptr(),
                    cells.pay_agg.add(dst_off),
                    slice.len(),
                );
            }
        }
    }
}

/// Parse a hex colour byte slice (`#rgb` / `#rgba` / `#rrggbb` /
/// `#rrggbbaa`) into a `u32` per CSS Color Level 4. The 3-digit and
/// 4-digit forms expand each nibble: `#abc` → `0xAABBCCFF`,
/// `#abcd` → `0xAABBCCDD`. Missing alpha defaults to `0xFF`.
/// Used by the `PayloadKind::HexU32` decode path.
#[inline]
fn parse_hex_u32(slice: &[u8]) -> u32 {
    let bytes = if slice.first() == Some(&b'#') {
        &slice[1..]
    } else {
        slice
    };
    fn nibble(b: u8) -> Option<u32> {
        Some(match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => return None,
        })
    }
    let mut nibbles = [0u32; 8];
    let n = bytes.len().min(8);
    for (i, &b) in bytes.iter().take(n).enumerate() {
        match nibble(b) {
            Some(v) => nibbles[i] = v,
            None => return 0,
        }
    }
    match n {
        // #rgb → #rrggbbff
        3 => {
            ((nibbles[0] * 0x11) << 24)
                | ((nibbles[1] * 0x11) << 16)
                | ((nibbles[2] * 0x11) << 8)
                | 0xFF
        }
        // #rgba → #rrggbbaa
        4 => {
            ((nibbles[0] * 0x11) << 24)
                | ((nibbles[1] * 0x11) << 16)
                | ((nibbles[2] * 0x11) << 8)
                | (nibbles[3] * 0x11)
        }
        // #rrggbb → #rrggbbff
        6 => {
            (nibbles[0] << 28)
                | (nibbles[1] << 24)
                | (nibbles[2] << 20)
                | (nibbles[3] << 16)
                | (nibbles[4] << 12)
                | (nibbles[5] << 8)
                | 0xFF
        }
        // #rrggbbaa → verbatim
        8 => {
            (nibbles[0] << 28)
                | (nibbles[1] << 24)
                | (nibbles[2] << 20)
                | (nibbles[3] << 16)
                | (nibbles[4] << 12)
                | (nibbles[5] << 8)
                | (nibbles[6] << 4)
                | nibbles[7]
        }
        _ => 0,
    }
}
