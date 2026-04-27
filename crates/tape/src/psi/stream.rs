//! [`PayloadStream`] — the Stage-A → Stage-B job queue.
//!
//! Owned by the parser between Stage A and Stage B. After Stage B
//! drains the stream into the column substrate, the stream is
//! discarded — no per-parse allocation survives the Stage-B closure.

use crate::columns::Columns;
use crate::profile::GrammarProfile;

#[cfg(feature = "rayon")]
use rayon::iter::ParallelIterator;
#[cfg(feature = "rayon")]
use rayon::slice::ParallelSlice;

use super::column_cells::{write_decoded, ColumnCapacities, ColumnCells};
use super::job::{PayloadJob, PayloadKind};

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

    /// Append a job to the stream.
    ///
    /// # Hot-path elision (AW-IV.W2.3)
    ///
    /// Post-AW-IV.W2.3 this method is **not reached** from the
    /// per-grammar walker's hot path for inline-decodable scalar
    /// payload kinds — `F64`, `I64`, `U8`, `Bool`, `HexU32`. Those
    /// kinds decode directly into
    /// [`Columns::pay_agg`](crate::columns::Columns::pay_agg) at parse
    /// time via the decoder-splice emitter
    /// (`crates/core/src/backend/rust/emitter/dta_walker/decoders.rs`,
    /// W2.3.a) with the arena column pre-sized from an input-
    /// proportional capacity hint (W2.3.b); no [`PayloadJob`] is
    /// constructed and no `Vec::push` runs. `nm target/release/deps/
    /// <bench>` on a bench binary whose grammar contains only inline-
    /// decodable scalar payloads shows the `PayloadStream::push`
    /// symbol absent.
    ///
    /// The method remains the canonical append entry-point for the
    /// two residual populations that genuinely require Stage-B
    /// deferral:
    ///
    /// - [`PayloadKind::String`] — escape-resolving JSON-string decode
    ///   into a framed `(len: u32, bytes)` arena slot. The decoder
    ///   (`decoders::json_string::decode_into`) runs a stateful
    ///   escape-handling loop whose per-record cost amortises rayon's
    ///   scheduling overhead above the break-even threshold in
    ///   [`GrammarProfile::parallel_break_even_bytes`](crate::profile::GrammarProfile::parallel_break_even_bytes).
    /// - [`PayloadKind::AggregateLarge`] — verbatim byte copy of
    ///   oversize aggregates (CSS `color()`-style > 16 B payloads).
    ///   Per-record cost is modest but the copy itself widens with
    ///   input and benefits from Stage-B parallelism on large corpora.
    ///
    /// For both residual kinds the walker emit at
    /// `dta_walker::lower_state::emit_regex_arm` splices the arena-
    /// reserve + `push` pair inline (`emit_psi_push_inline` in
    /// `dta_walker::helpers`). The splice keeps the surface `push`
    /// symbol load-bearing in every per-grammar walker whose IR lifts
    /// at least one `String` or `AggregateLarge` payload; grammars
    /// whose payloads are all inline-decodable scalars emit zero
    /// references.
    ///
    /// Additionally the cold-path `dispatch_one` replay surface
    /// (`driver::dispatch_one`, the AX correctness ground truth)
    /// funnels every payload kind — scalar or residual — through
    /// `push` uniformly; the cold path's single `psi.push` call at
    /// `driver.rs::dispatch_one` is never reached from the per-grammar
    /// hot walker and is attributed cold at whole-function granularity.
    ///
    /// # Annotation
    ///
    /// `#[inline(always)]` is preserved from AW-IV.W2.1 so any splice
    /// site the codegen still emits (the residual-kind arms; the
    /// cold-path driver) folds the one-instruction `Vec::push` body
    /// into the call site without a function-call boundary surviving
    /// in the bench binary's `nm` output. The annotation is not a
    /// hot-path optimisation hint; per W2.3 the hot path of the
    /// per-grammar walker does not reach this method for scalar
    /// payloads regardless.
    #[inline(always)]
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
    /// Cache lines: [`Self::CHUNK_RECS`] is `64 /
    /// size_of::<PayloadJob>()` (three jobs per line at the current
    /// 20 B struct width) so each rayon chunk fits inside one 64 B
    /// line of `PayloadJob`s on the read side. Destination arena
    /// writes can land on shared cache lines (adjacent `arena_offset`
    /// ranges), but the writes themselves are disjoint and the
    /// underlying architectures (x86, ARM) tolerate concurrent stores
    /// to different bytes within a line at the hardware-coherence
    /// level.
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
            let width = match (job.kind, job.kind.arena_byte_width()) {
                // String payloads land as `(len: u32, bytes)` framed
                // arena slots; reserve the 4-byte header alongside
                // the matched length minus the surrounding quotes.
                // The trim is conservative — the worst case is the
                // matched length (no quotes to trim).
                (PayloadKind::String, _) => {
                    let len = job.input_len() as usize;
                    4 + len
                }
                (_, 0) => job.input_len() as usize,
                (_, w) => w,
            };
            caps.arena = caps.arena.max(job.arena_offset as usize + width);
        }
        caps
    }
}
