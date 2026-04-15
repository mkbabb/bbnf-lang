# Research 02 — Fingerprint-Directed Memory Planner (FDMP)

*Verbatim deliverable from architecture research agent, April 2026.
Backs AV Phase 1 (`GrammarProfile`) and the per-grammar capacity
closures for every AV-era buffer.*

---

# AU.CACHE — Cache-Locality, Prefetching, and Codegen-Directed Memory Shaping

## 1. Angle headline

The wave-2 profile tells us the tape itself isn't the bottleneck: `push_compound` is ≤ 5% and `TapeCursor::record()` is a single unchecked load. What bleeds cycles is **allocator thrash on the growing tape** (10–22% of `parse_simple`) and the walker's **stride** — the bytes of `TapeRec` the view actually consults per step. Both are reachable only by codegen, never by a generic cache-tuning pass. The proposal below turns the codegen fingerprint into a first-class memory planner.

## 2. Motivation tied to wave-2 evidence

Three facts converge:

1. `TapeBuilder::with_capacity(input.len()/2+2)` is a single uniform divisor. Sheets needs ~1 rec/byte; canada JSON needs `len/8`; css_l4_grammar sits near `len/6`. The result — `RawVec::grow_one` + `_mi_heap_realloc_zero` 10–22% of `parse_simple` Sheets samples (profiling-2.md:198, 207), up to 9% of json.bbnf inclusive — is allocator noise that reveals *codegen-time* knowledge was thrown away.
2. The `.view()` accessor surface (confirmed in `crates/core/src/backend/rust/view/{alt.rs, seq.rs, leaves.rs}`, `crates/bbnf-tape/src/cursor.rs`) exhibits a sharply **bimodal** field footprint. Every `Alt.as_<variant>()` reads `meta_idx()` (byte 0 + bit 7 of byte 1) and optionally descends via `child(0)` (bytes 12–15). Every `Seq.child_i()` calls `backward_step` which consults `has_children()` (byte 1 bit 6) + `child_off` (bytes 12–15). `span_lo`/`span_hi` (bytes 4–11) are untouched during structural descent and read only at leaf `.text()` or `.span()` sites.
3. AU.6.7 collapses payload side-cars into one arena, freeing the `payload_idx` half-word (bytes 2–3) as a design variable — a fact the textbook "put hot fields in the same cache line" play can't exploit because it has no grammar-specific notion of which fields are hot.

Those three observations, combined, let a codegen-directed memory planner close allocator thrash (10–22%), tighten structural walks (≥ 50% of CSS self-time), and bank the freed bits toward denser records — all with zero changes to the grammar authoring surface.

## 3. Novel proposal — **Fingerprint-Directed Memory Planner (FDMP)**

A codegen pass that consumes the per-grammar push fingerprint (already computed, already written out under `.profiles/samply/prebuild/expand/<bench>/expand.rs`) and emits three correlated decisions in one pass:

**(a) Per-grammar capacity law.** Replace the universal `input.len()/2+2` with a closure emitted at codegen time from the fingerprint: `fn predict_tape_capacity(n_bytes: usize) -> (records, payload_bytes)`. For Sheets the emitter sees `push_compound=37, push_leaf=0, push_leaf_with=0` and a grammar-shape coefficient `compounds_per_byte ≈ 1.1` measured against a training corpus, so it emits `records ≈ input.len() * 11 / 10`, `payload_bytes = 0`. For CSS L4 (`234/22/7`) it emits `records ≈ input.len() / 6`, `payload_bytes ≈ input.len() / 64`. The emitter writes the closure — no runtime branch, no heuristic table.

**(b) Mimalloc segment-aware rounding.** Apple Silicon mimalloc's small-object segment is 2 MiB. The codegen rounds the predicted `records * 16` byte count **up** to the mimalloc segment boundary if predicted capacity crosses a segment, or **up** to the next power of two within a single segment if it doesn't. One allocation, one segment touch, zero `grow_one` for any input the fingerprint correctly predicts. Capacity becomes: `round_to_alloc_class(predict_tape_capacity(n))`.

**(c) Split-skeleton tape, emitted conditionally.** When the fingerprint shows `push_leaf_with = 0` (Sheets, bbnf_monolithic), the codegen emits a **skeleton-only tape**: the `TapeRec` is compressed to 8 bytes — `{ kind_meta: u8, flags: u8, span_delta_lo_u16: u16, child_delta_u32: u32 }` — where `span_delta_lo_u16` is the span length (16-bit suffices: CSS tokens are rarely >64 KiB, a compile-time assertion picks a 24-bit packing if the fingerprint disagrees) and `child_delta_u32` is the **delta** from the compound's own offset to its first child, not an absolute offset. 8 bytes = 8 records per 64-byte cache line; a pre-order walk doubles its stride density exactly on the grammar (Sheets) whose walk is pure structure. For grammars with a non-zero payload column (JSON, CSS L4), the codegen keeps the 16-byte layout but reorders fields to `{ kind_meta, flags, child_off_lo_u16, child_off_hi_u16, span_lo, span_hi, child_payload_u32 }` — placing the hot quartet (kind_meta, flags, child_off low-halves) in the first 8 bytes of the record, so `TapeCursor::child(0)` reads exactly one half-line.

The load-bearing point — a generic parser library cannot do this. FDMP leans on **grammar-specific static knowledge** that only `bbnf-ir` and the emitter can see: `push_compound/push_leaf/push_leaf_with_*` counts, whether any `-> T` projection ever reaches a payload, and a per-grammar span-width distribution. Nothing in cssparser, lightningcss, or sonic can emit a 8-byte skeleton tape because their AST is fixed at the library level. BBNF can.

Sketch, emitted into `generated.rs` by `crates/core/src/backend/rust/emitter/grammar.rs`:

```rust
// Emitted per grammar: Sheets example
const TAPE_CAPACITY_NUM: usize = 11;
const TAPE_CAPACITY_DEN: usize = 10;
const PAYLOAD_CAPACITY_NUM: usize = 0;
type TapeRecSkel = bbnf::runtime::tape::TapeRecSkel8;  // 8-byte variant

pub fn parse(input: &str) -> Result<Parsed<'_, Self>, ParseErr> {
    let predicted_recs =
        (input.len() * TAPE_CAPACITY_NUM) / TAPE_CAPACITY_DEN + 4;
    let cap = round_to_mimalloc_class::<TapeRecSkel>(predicted_recs);
    let mut builder = TapeBuilder::<TapeRecSkel>::with_capacity(cap);
    // …
}
```

## 4. Measurement plan

- Primary counters: `_mi_heap_realloc_zero` + `RawVec::grow_one` + `finish_grow` — must drop below 2% of `parse_simple`, `canada`, and `json.bbnf` samples post-FDMP. Wave-2 baseline: 10–22% / up to 9%.
- Secondary counters: `TapeCursor::child`, `TapeCursor::backward_step` — both expected to lose one cache miss per compound descent on Sheets after the skeleton-tape lands (L1 dcache miss rate via `samply record --load-perf-event L1-dcache-load-misses` on a Linux profile agent; on Apple Silicon, `powermetrics --samplers cpu_power` captures retired-load-miss).
- Tertiary: MB/s on the AU wave matrix. Expected motion — Sheets `parse_simple` from 93 to ≥ 140 MB/s (removing allocator thrash), CSS bootstrap from 578 to ≥ 610 MB/s (hitting AU.2 gate by reducing tape re-zero cost), json_monolithic canada from 1293 to ≥ 1400 MB/s. All three are on the allocator side; none require grammar changes.
- Falsification: if predicted capacity is off by more than 1.25× on any corpus fixture, we measured the wrong coefficient. `scripts/prepare-profile-wave.sh` already produces the `wave.tsv`; the fingerprint coefficient fitter can piggyback.

## 5. Interaction with AU.6.2 and AU.6.7

AU.6.2 proposes a per-grammar divisor; FDMP (a) is that, promoted from a scalar to a closure fit against the training corpus and extended to payload pre-reservation. AU.6.7 unifies the arena; FDMP (c) exploits the freed `payload_idx` u16 to widen `child_off` to a full 32 bits inline, eliminating the AU.6.7-era `child_off` pun between "tape offset" and "arena offset" — the skeleton layout uses a `child_delta_u32` relative to the compound itself and a separate `payload_delta_u32` for leaves, so the two roles never collide. The split between skeleton tape (structural-only) and full tape (with payload column) is the *mechanical consequence* of AU.6.7's type classification reaching the emitter.

## 6. Honest risks

- **Skeleton-tape 16-bit span cap.** CSS and BBNF tokens comfortably fit; raw-HTML-embedded CSS with 64 KiB+ comment payloads does not. Mitigation: codegen asserts `max_observed_span_len < 65536` over the training corpus, falls back to 24-bit packing (5 records/line) if the assertion fails. Never silent — the choice appears in the emitted constants.
- **Coefficient drift across inputs.** Fitting capacity against the training corpus risks under-provisioning pathological inputs. Mitigation: the closure emits `predicted + input.len() / 32` as safety margin, and the builder still falls back to `grow_one` if the prediction is wrong — the claim is that the steady state is zero growth, not that growth is impossible.
- **Codegen complexity.** Two tape layouts (8 B skeleton, 16 B full) means two `TapeBuilder`/`TapeCursor` implementations. The `no-orthogonal-codepaths` invariant demands they be one generic over the record width, parameterised by a single associated constant — `TapeRec: Pod + KindMetaBits`. The grammar picks the parameter at codegen time; there is one monolithic path per grammar, exactly one across the workspace.
- **Mimalloc segment rounding is allocator-coupled.** If the system allocator changes (`jemalloc`, `system`), the rounding law changes. Mitigation: the emitter wraps the rounding in `runtime::alloc::round_up_to_class()`, owned by `bbnf-tape` and feature-gated per allocator. This is a library-local constant, not a hardware assumption.
- **Prefetch temptation.** Explicit `prefetch_read_data` was angle 3 of the seed. FDMP intentionally does **not** emit prefetch intrinsics — software prefetch on Apple Silicon is frequently a net loss (the hardware stride prefetcher is exceptional). The sole concession is field reordering so the hardware prefetcher's natural stride (one line = four 16 B records or eight 8 B records) lands on records whose hot fields are in the first half-line.

The load-bearing claim — FDMP is **what a grammar-driven codegen can see that a library cannot**: the join of the push fingerprint, the payload classification, the corpus-fitted density coefficient, and the allocator class. Every one of those inputs is on disk today (`.profiles/samply/prebuild/expand/<bench>/expand.rs`, `bbnf_ir::passes::PayloadLayout`, wave.tsv, mimalloc's published constants); FDMP closes the loop from grammar to allocation.

Key files referenced:
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/tape.rs` (lines 56-89: `TapeRec` layout + size/align asserts)
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/builder.rs` (lines 68-76: `with_capacity` hint; lines 203-464: typed push variants)
- `/Users/mkbabb/Programming/bbnf-lang/crates/bbnf-tape/src/cursor.rs` (lines 115-187: hot walk reads kind_meta + flags + child_off only)
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/backend/rust/emitter/grammar.rs` (lines 438-442: the one-size-fits-all divisor to replace)
- `/Users/mkbabb/Programming/bbnf-lang/crates/core/src/backend/rust/view/{alt.rs,seq.rs,leaves.rs}` (confirm hot-field footprint of cursors)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AU/profiling-2.md` (lines 195-208: Sheets allocator thrash; lines 96-103: push fingerprints; line 355: per-grammar divisor already acknowledged)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AU/AU.md` (lines 508-623: AU.6.2 and AU.6.7 that FDMP extends)
