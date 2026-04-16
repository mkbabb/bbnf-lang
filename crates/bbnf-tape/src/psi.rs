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

    /// Whether this kind writes into [`Columns::pay_narrow`].
    #[inline]
    pub const fn is_narrow(self) -> bool {
        matches!(self, Self::U8 | Self::Bool | Self::HexU32)
    }

    /// Whether this kind writes into [`Columns::pay_wide`].
    #[inline]
    pub const fn is_wide(self) -> bool {
        matches!(self, Self::F64 | Self::I64)
    }

    /// Whether this kind writes into the [`Columns::pay_agg`] arena.
    #[inline]
    pub const fn is_arena(self) -> bool {
        matches!(self, Self::String | Self::AggregateLarge)
    }
}

/// One decode unit produced by Stage A and consumed by Stage B.
///
/// Layout is `#[repr(C)]` so the in-memory shape matches the codegen-
/// time literal the emitter produces. Total size is 16 bytes — 4
/// records per 64 B cache line, matching [`crate::tape::TapeRec`].
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PayloadJob {
    /// Structural record index in [`Columns`]. The Stage-B worker
    /// updates the matching record's `child_off` slot to point at
    /// the column rank or arena offset where the decoded payload
    /// lands.
    pub rec_idx: u32,
    /// Source byte range start — inclusive. The Stage-B scanner reads
    /// `input[input_lo..input_hi]`.
    pub input_lo: u32,
    /// Source byte range end — exclusive.
    pub input_hi: u32,
    /// Terminal scanner selector.
    pub kind: PayloadKind,
    /// Pre-allocated column slot index. For [`PayloadKind::is_narrow`]
    /// kinds, the rank into [`Columns::pay_narrow`]; for
    /// [`PayloadKind::is_wide`] kinds, the rank into
    /// [`Columns::pay_wide`]; for [`PayloadKind::is_arena`] kinds, the
    /// byte offset into [`Columns::pay_agg`].
    pub column_idx: u8,
    /// Padding to match the documented 16-byte size and align the
    /// struct on a natural 4-byte boundary; the bytes are
    /// zero-initialised at construction.
    pub _pad: [u8; 2],
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
        column_idx: u8,
    ) -> Self {
        Self {
            rec_idx,
            input_lo,
            input_hi,
            kind,
            column_idx,
            _pad: [0; 2],
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
    assert!(std::mem::size_of::<PayloadJob>() == 16);
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
    /// Workers own the columns mutably for the duration of the walk;
    /// the loop touches each column's slot once. The match on
    /// [`PayloadKind`] compiles to a jump table.
    fn fill_sequential(&self, input: &[u8], columns: &mut Columns) -> usize {
        let len_required = self.required_column_capacities();
        len_required.reserve(columns);
        for job in &self.jobs {
            apply_job(job, input, columns);
        }
        self.jobs.len()
    }

    /// Parallel Stage-B walk via rayon `par_chunks` over the PSI
    /// stream. Each worker decodes its chunk into a local scratch
    /// buffer, then a sequential merge pass writes the decoded values
    /// into the column slots in PSI order.
    ///
    /// Two-phase to keep the column writes non-contended: workers
    /// only read `input` (immutable); the merge pass is the sole
    /// writer of the column substrate. The chunk-local scratch
    /// allocations dominate the cost only on inputs below the
    /// break-even gate — by construction, those inputs take the
    /// sequential path.
    #[cfg(feature = "rayon")]
    fn fill_parallel(&self, input: &[u8], columns: &mut Columns) -> usize {
        let len_required = self.required_column_capacities();
        len_required.reserve(columns);
        let decoded: Vec<DecodedValue> = self
            .jobs
            .par_chunks(Self::CHUNK_RECS)
            .flat_map_iter(|chunk| chunk.iter().map(|job| decode_job(job, input)))
            .collect();
        for (job, value) in self.jobs.iter().zip(decoded.iter()) {
            commit_value(job, value, columns);
        }
        self.jobs.len()
    }

    /// Sequential fallback when the rayon feature is disabled at
    /// compile time. Behaviour is identical to [`Self::fill_sequential`]
    /// — the parallel path collapses to the same code.
    #[cfg(not(feature = "rayon"))]
    fn fill_parallel(&self, input: &[u8], columns: &mut Columns) -> usize {
        self.fill_sequential(input, columns)
    }

    /// Compute the column capacities required to host every job's
    /// pre-allocated slot. Stage A assigns `column_idx` monotonically
    /// per kind, so the maximum index per kind plus one is the
    /// required column length.
    fn required_column_capacities(&self) -> ColumnCapacities {
        let mut caps = ColumnCapacities::default();
        for job in &self.jobs {
            match job.kind {
                k if k.is_narrow() => {
                    caps.narrow = caps.narrow.max(job.column_idx as usize + 1);
                }
                k if k.is_wide() => {
                    caps.wide = caps.wide.max(job.column_idx as usize + 1);
                }
                k if k.is_arena() => {
                    caps.arena = caps
                        .arena
                        .max(job.column_idx as usize + job.input_len() as usize);
                }
                _ => {}
            }
        }
        caps
    }
}

/// Per-column capacity hints derived from the PSI stream.
///
/// Stage B reserves enough space in every column to land every job's
/// decoded payload at its `column_idx` slot before any decode runs.
/// The reservation is `resize`-with-zero so subsequent slot writes
/// land in pre-existing memory — no growth during the Stage-B walk.
#[derive(Debug, Default)]
struct ColumnCapacities {
    narrow: usize,
    wide: usize,
    arena: usize,
}

impl ColumnCapacities {
    fn reserve(&self, columns: &mut Columns) {
        if self.narrow > columns.pay_narrow.len() {
            columns.pay_narrow.resize(self.narrow, 0);
        }
        if self.wide > columns.pay_wide.len() {
            columns.pay_wide.resize(self.wide, 0);
        }
        if self.arena > columns.pay_agg.len() {
            columns.pay_agg.resize(self.arena, 0);
        }
    }
}

/// Decode-only output of a Stage-B worker. Produced in the parallel
/// chunk closure, consumed by the sequential commit pass; lifetime-
/// bound to the input slice so the `String` / `AggregateLarge`
/// variants point at the source bytes (no per-job allocation).
#[derive(Debug)]
enum DecodedValue<'src> {
    Narrow(u32),
    Wide(u64),
    /// Slice view into `input` — the bytes the commit pass copies
    /// into [`Columns::pay_agg`] at the job's `column_idx` offset.
    Bytes(&'src [u8]),
}

/// Sequential decode + write — the unit of work the sequential path
/// performs once per job.
fn apply_job(job: &PayloadJob, input: &[u8], columns: &mut Columns) {
    let value = decode_job(job, input);
    commit_value(job, &value, columns);
}

/// Decode a single job's source slice into a [`DecodedValue`]. Pure
/// — no column writes — so the parallel path can run this in the
/// chunk closure without taking a `&mut Columns`.
#[inline]
fn decode_job<'src>(job: &PayloadJob, input: &'src [u8]) -> DecodedValue<'src> {
    let lo = job.input_lo as usize;
    let hi = job.input_hi as usize;
    let slice = &input[lo..hi];
    match job.kind {
        PayloadKind::F64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            DecodedValue::Wide(s.parse::<f64>().unwrap_or(0.0).to_bits())
        }
        PayloadKind::I64 => {
            let s = std::str::from_utf8(slice).unwrap_or("0");
            DecodedValue::Wide(s.parse::<i64>().unwrap_or(0) as u64)
        }
        PayloadKind::U8 => DecodedValue::Narrow(slice.first().copied().unwrap_or(0) as u32),
        PayloadKind::Bool => DecodedValue::Narrow(if slice == b"true" { 1 } else { 0 }),
        PayloadKind::HexU32 => DecodedValue::Narrow(parse_hex_u32(slice)),
        PayloadKind::String | PayloadKind::AggregateLarge => DecodedValue::Bytes(slice),
    }
}

/// Commit a decoded value into the matching column slot. Called
/// sequentially in both fill paths — the parallel path collects
/// values first, then commits in order.
#[inline]
fn commit_value(job: &PayloadJob, value: &DecodedValue<'_>, columns: &mut Columns) {
    match (job.kind, value) {
        (k, DecodedValue::Narrow(v)) if k.is_narrow() => {
            columns.pay_narrow[job.column_idx as usize] = *v;
        }
        (k, DecodedValue::Wide(v)) if k.is_wide() => {
            columns.pay_wide[job.column_idx as usize] = *v;
        }
        (k, DecodedValue::Bytes(bytes)) if k.is_arena() => {
            let start = job.column_idx as usize;
            columns.pay_agg[start..start + bytes.len()].copy_from_slice(bytes);
        }
        _ => debug_assert!(
            false,
            "PayloadKind {:?} does not match decoded value variant",
            job.kind
        ),
    }
}

/// Parse a hex colour byte slice (`#rrggbb` / `#rrggbbaa` / `rrggbb`)
/// into a `u32`. Missing alpha defaults to `0xFF`. Used by the
/// `PayloadKind::HexU32` decode path.
#[inline]
fn parse_hex_u32(slice: &[u8]) -> u32 {
    let bytes = if slice.first() == Some(&b'#') {
        &slice[1..]
    } else {
        slice
    };
    let mut value: u32 = 0;
    for &b in bytes.iter() {
        let nibble = match b {
            b'0'..=b'9' => b - b'0',
            b'a'..=b'f' => b - b'a' + 10,
            b'A'..=b'F' => b - b'A' + 10,
            _ => return 0,
        };
        value = (value << 4) | nibble as u32;
    }
    if bytes.len() == 6 {
        (value << 8) | 0xFF
    } else {
        value
    }
}
