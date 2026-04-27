//! Payload-Side Information (PSI) — Tranche AV Phase 4 (AV.4.1 / AV.4.2).
//!
//! # Architectural role
//!
//! The Dispatch Tape Automaton (Stage A — `crates/tape/src/dta.rs`)
//! emits the tape's structural skeleton in a single linear byte pass:
//! every record lands with its kind, variant, and `span_lo` populated.
//! Two populations of payload decoding fan out from this skeleton:
//!
//! - **Inline-decodable scalars** — `f64`, `i64`, `u8`, `bool`,
//!   `HexU32` — decode trivially from the matched byte slice via
//!   Eisel-Lemire / integer parse / direct byte cast / discriminant /
//!   nibble expansion. Their decoded value fits in the arena slot the
//!   emitter already reserved, so **Stage A writes the decoded bytes
//!   straight into [`Columns::pay_agg`](crate::columns::Columns::pay_agg)
//!   at parse time** — no PSI job, no post-parse scheduling. The
//!   per-grammar walker's Regex arms splice the decoder body inline
//!   via the decoder-emit fragments in
//!   `crates/core/src/backend/rust/emitter/dta_walker/decoders.rs`
//!   (AW-IV.W2.3.a); the arena column is pre-sized from an
//!   input-proportional capacity hint (AW-IV.W2.3.b) so the direct-
//!   write path sees neither `psi.push` nor a per-record capacity
//!   check. Payload-less Regex leaves (`RegexPayloadKind::None`) were
//!   already PSI-free pre-W2.3 — their Span emission needs no decode.
//!
//! - **Residual non-scalar payloads** — `String` (JSON-escape decode
//!   into a framed `(len: u32, bytes)` arena slot via
//!   [`decoders::json_string::decode_into`](crate::decoders::json_string::decode_into))
//!   and `AggregateLarge` (verbatim byte copy for CSS `color()`-style
//!   oversize aggregates) — are **not** inline-decodable. Their decode
//!   bodies are non-trivial (tens-to-hundreds of instructions per
//!   record, branchy escape-handling for strings) and pay enough
//!   per-record work that amortising the decode across a rayon
//!   `par_chunks_mut` pass pays for the scheduling overhead. For these
//!   kinds Stage A schedules a [`PayloadJob`] via
//!   [`PayloadStream::push`] and **Stage B** drains the queue with the
//!   sequential or parallel walk chosen by
//!   [`PayloadStream::should_parallelise`].
//!
//! ```text
//! input bytes ─► Stage A (DTA) ──────────────────────────────────► tape
//!                   │                                                 ▲
//!                   ├─ inline-decodable scalar? ──► write pay_agg ────┘
//!                   │  (F64/I64/U8/Bool/HexU32)      no PSI, no Stage B
//!                   │
//!                   └─ residual (String/AggregateLarge)
//!                         ─► Vec<PayloadJob> ─► Stage B (rayon) ─► pay_agg
//! ```
//!
//! The post-AW-IV.W2.3 per-grammar walker's `nm` scan therefore shows
//! `PayloadStream::push` **absent** from hot-path arms whose payload
//! kind is scalar — the call is structurally gone, not merely inlined
//! away. The symbol persists only where a `String`- or `AggregateLarge`-
//! kinded leaf is lowered and where the cold-path replay driver
//! (`driver::dispatch_one`, the AX correctness ground truth) still
//! funnels every payload kind through the PSI queue uniformly. Every
//! `psi.push` in a per-grammar walker Regex arm post-W2.3 corresponds
//! to one of those two residual populations.
//!
//! # PSI stream construction (AV.4.1)
//!
//! For the residual non-scalar populations, Stage A emits one
//! [`PayloadJob`] per leaf that requires Stage-B decoding. The job
//! carries:
//!
//! - `rec_idx` — the structural record's index in
//!   [`Columns`](crate::columns::Columns), set by the DTA at push time.
//! - `input_lo` / `input_hi` — the source-byte slice the scanner reads.
//! - `kind` — the [`PayloadKind`] selecting the terminal scanner.
//! - `arena_offset` — the byte offset in [`Columns::pay_agg`](crate::columns::Columns::pay_agg)
//!   where the decoded payload lands. Stage A monotonically advances
//!   the arena cursor per job so offsets are unique and bounds-disjoint
//!   across workers.
//!
//! The struct is `#[repr(C)]` and 20 bytes wide so a `Vec<PayloadJob>`
//! places roughly three jobs per 64 B cache line. Stage B's rayon
//! stride ([`PayloadStream::CHUNK_RECS`]) is chosen to keep every
//! worker's chunk on one cache line of the job stream, avoiding false
//! sharing across workers.
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
//! the matching typed payload column for the residual non-scalar
//! populations (`String`, `AggregateLarge`) the walker scheduled. The
//! driver uses the grammar's
//! [`parallel_break_even_bytes`](crate::profile::GrammarProfile::parallel_break_even_bytes)
//! threshold from the [`GrammarProfile`] to choose between the
//! sequential walk and the rayon
//! `par_chunks_mut`-driven parallel walk. Both paths write into the
//! same column slots — the API is uniform; only the iterator differs.
//! For grammars whose payloads are exclusively inline-decodable
//! scalars the PSI stream is empty after parse, [`PayloadStream::len`]
//! returns zero, and [`PayloadStream::fill_columns`] is a no-op — the
//! hot-path decoder-splice already populated every arena slot.
//!
//! Cache-line alignment: [`PayloadStream::CHUNK_RECS`] is `64 /
//! size_of::<PayloadJob>()` (three jobs per line at the current 20 B
//! struct width) so each rayon chunk fits inside one 64 B cache line
//! of `PayloadJob`s with no straddle. False
//! sharing on the *job* stream is structurally impossible (workers
//! own disjoint chunks), and false sharing on the arena stream is
//! impossible by construction — every job's `arena_offset` is unique
//! because Stage A monotonically advances the arena cursor per job.
//! The only contention risk is across-line tearing on the destination
//! arena when two workers write adjacent byte ranges in `pay_agg`.
//! That is benign on every architecture in scope (x86/ARM both
//! guarantee aligned-store atomicity at the word boundary).

mod column_cells;
mod job;
mod stream;

pub use job::{PayloadJob, PayloadKind};
pub use stream::PayloadStream;
