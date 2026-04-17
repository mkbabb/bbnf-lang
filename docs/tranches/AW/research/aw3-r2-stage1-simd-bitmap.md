# R2 — Stage-1 SIMD Structural-Bitmap Pre-Pass: Archaeology & Canonical Design

## 1. Archaeology — definitive timeline

All six hashes from the AV research appendix exist and resolve on `master`:

| Commit | Tranche | Net effect |
|---|---|---|
| `4114695b` | AO.0.1 | `compute_structural_bytes` IR pass; derivation only, no consumer. |
| `7198c974` | AO.0.4–0.6 | First consumer. `emit_grammar_impl` pre-scans whole input (`scan_structural + filter_quote_parity` → `Vec<u32>` in `ParserState`); `alt.rs` jumps positions via `advance_to_structural`; `ws.rs` elides `?w`. |
| `2fa31721` | AP.1b (on) | Replaces mutating `advance_to_structural` with peek-only `sync_structural_cursor_to_offset` + `current_structural_byte`. Hybrid dispatch. |
| `4417f8a7` | AP.1b (off) | Commit msg: *"pre-scan overhead... costs ~15-25% without WS elision"*. Sets `structural_mode = false`; infrastructure kept. |
| `2a8af086` | AP.1 (off earlier) | Bug: `advance_to_structural` jumps past non-structural bytes (digits), mis-routes Alt; force-restores `?w`. |
| `2f7c1bd4` | AQ.5 | Deletes ~1,500 LOC. AQ.md:51-59 arithmetic: WS 110 µs < pre-scan cost 300 µs → -190 µs on citm. |
| `e225ade9` | AU.2.7 IR | `compute_structural_alphabet` at `crates/ir/src/passes/sets/structural_alphabet.rs`. Revives derivation shape; *no* cursor, *no* consumer. |
| `143d19ee` | AU.2.7 emitter | `emit_structural_bitmap_kernel` at `crates/core/src/generate/regex/emit/simd.rs:92-113`; `parse-that` kernel at `parsers/scan/structural_bitmap.rs`. Deletes `emit_memchr1/2/3` + `emit_nibble_lut_scan`. |

**Timeline:** AO→birth. AP→enable, misroute, peek-only, gate off. AQ→delete. AU.2.7 v2→resurrect derivation + *per-site* SIMD helper; no driver-level pre-pass restored.

## 2. AU.2.7 v2 dissection — what shipped

The v2 kernel is **not a pre-pass**. `emit_structural_bitmap_kernel` (`simd.rs:92-113`) inlines, at each negated-char-class call site, a `static __LO_LUT/__HI_LUT` pair and a call to `::parse_that::find_next_structural_from(&state.src_bytes, __start, &__LO_LUT, &__HI_LUT)`. That helper (`structural_bitmap.rs:93-133`) walks 64-byte stripes, CTZ-terminates on first hit, returns. Call sites are `emit/mod.rs:185,186,328,330`. There is no `ParserState` cursor, no `Vec<u32>` index, no driver consumption.

**Why AU.2.7 missed the 650 MB/s gate** (FINAL.md:101-112, bootstrap 411→454):

1. **Per-invocation, not once-per-input.** Each call rebuilds LUT registers (`u8x16::from_array` ×2 + `splat`) and enters the stripe loop. CSS property values < 64 B pay one stripe-cycle overhead nibble_lut_scan avoided. `__declaration` + `__compoundSelector` call it many times per line.
2. **No quote parity.** No XOR/CLMUL mask. Safe only because the emitter routes *negated char-class* regex through it — strings are tokenized via `quoted_string` kernels elsewhere. Cannot subsume "find next structural" universally.
3. **Digraphs mined but unused.** `structural_alphabet.rs:88-106` collects `/*`, `*/`, `->`, `(*`, `*)`. `emit_structural_bitmap_kernel` ignores them — single bytes only. `emit_ws_interleaved_negated_scan_*` (`simd.rs:174-227`) still byte-loops, calling `scan_ws_block_comments` every iteration.
4. **Grammar-wide LUT never emitted.** `bbnf-tape/src/profile.rs:179` exposes `structural_alphabet: &'static [u8]` but `profile.rs:240` defaults `&[]`; no codegen site reads it. Every call builds its own `__LO_LUT` from local `targets`. The "one kernel per grammar" v2 premise did not ship.

## 3. Was the pre-pass ever driver-consumed?

**Only in AO→AP.** `emit_grammar_impl` at `7198c974` wrote a `parse()` prelude that populated `ParserState.structural_index: *const u32`, `structural_len: u32`, `structural_cursor: u32` (fields deleted at `2f7c1bd4`, see AQ.md:237-239). `alt.rs` emission used `advance_to_structural` (v1) or `current_structural_byte` (AP.1b peek-only).

**Never since.** The DTA driver at `crates/bbnf-tape/src/driver.rs:852-1136` (`dispatch_one`) walks byte-at-a-time: `DtaState::ByteDispatch` reads `input.get(*pos as usize)` (`driver.rs:998`); `DtaState::Regex` delegates scanning to the `RegexScanner` trait (`driver.rs:893-914`), which may use SIMD internally but the driver holds no index. `grep -rn 'find_next_structural_from\|NibbleBitmapIter' crates/bbnf-tape/src` returns zero. The only `structural_alphabet` references inside `bbnf-tape` are the unused `profile.rs:179,240` field.

## 4. Verdict

**Chronically deferred: YES.** Six tranches (AO, AP, AQ, AU, AV, AW) have touched it; none has landed a driver-consumed structural index. v1 was right in shape, wrong in economics (pre-AP.3.1 WS was the savings target; AP.3.1 collapsed it). AQ deleted instead of pivoting to the DTA walker W1 of AW later introduced — taken before DTA's own per-byte tax was known.

**Attempted optimally: NO.** v2 conflated "stage-1 pre-pass" with "per-scanner SIMD helper". The four missed items (call-site cost, quote parity, digraphs, grammar-wide LUT) each have a named file:line above; none is inherent to stage-1. The architectural gate was met (old emitters deleted); the architectural *goal* (amortise delimiter scanning across the whole parse) was abandoned. The 01-simd-structural-bitmap.md proposal's §3 sketch describes a true pre-pass; the code that landed does not implement it.

## 5. Canonical design — AW-III.W5.5 (new wave slot)

**Wave placement.** arch-comparison.md Category A names this AW-III.W5.5 NEW; concurring. Must land *before* W5.6 codegen-specialised walker so W5.6 attribution is clean.

### 5.1 IR surface

Extend `crates/ir/src/passes/sets/structural_alphabet.rs`:
- Add `digraph_mask: [u64; 4]` (first-byte bitset).
- Remove the `≤ 8` gate; emit two variants — `small` (≤ 8, shared nibble-LUT) and `wide` (9..=16, expanded membership bit — generalize `build_nibble_luts` from `1 << i` presence to any-non-zero membership as v2 proposal §3 flagged at `simd.rs:65-72`).
- Run `compute_structural_alphabet_v3` in `crates/core/src/pipeline/compile.rs` between `generate_dispatch_tables` and `compute_regex_info`.

### 5.2 New emitter module — `crates/core/src/generate/dta/stage1.rs`

Emits `fn scan_structural_<grammar>(src: &PaddedView) -> StructuralIndex` as part of the parse prelude (alongside `DTA_TABLE`):
- NEON: four `vqtbl1q_u8` + `vshrn_n_u16 #4` bitmask → one `u64` per 64 B stripe.
- x86_64: `vpshufb` + `vpmovmskb` + `_pext_u64` to compact indices directly (simdjson `flatten_bits`).
- **Quote parity folded in**: parallel `quote_mask` (`vceqq_u8` against `"`, `vextq_u8` for escape lookbehind, `vmull_p64` CLMUL on aarch64, `_mm_clmulepi64` on x86). Bytes inside strings masked off before compaction.
- **Digraphs folded in**: per `alphabet.digraphs` pair, `vceqq_u8 + vextq_u8` shifted compare (proposal §3 lines 55-58); OR into structural mask.

Output type at new `crates/bbnf-tape/src/stage1.rs`:

```rust
pub struct StructuralIndex { pub positions: Vec<u32>, pub kinds: Vec<u8> }
```

### 5.3 Driver consumption — redesigned `dta_run`

Replace the `pos: u32` cursor with a dual cursor:

```rust
struct Cursor<'a> { src: &'a [u8], idx: &'a StructuralIndex, pos: u32, slot: u32 }
```

Three changes in `crates/bbnf-tape/src/driver.rs`:

1. `DtaState::ByteDispatch` (`driver.rs:997-1016`): replace `input.get(*pos as usize)` with `idx.kinds[cursor.slot]` after `cursor.advance_to_next_structural()`. The walker stops reading `src[pos]` — it reads the precomputed byte at the next structural slot. Bytes between slots are implicit literal/regex content.
2. `DtaState::Regex` (`driver.rs:893-914`): scans become *bounded* to `[cursor.pos, idx.positions[cursor.slot])`. The scanner receives a pre-bounded view; match length is capped by the next structural offset. Eliminates open-ended scan tails currently dominating JSON `memchr::closure#0` (7-19%, profiling-2.md:133).
3. New arm `DtaState::ConsumeToNextStructural`: for `Seq` children expressing "eat everything up to delimiter X", directly jump `cursor` to `idx.positions[cursor.slot]` in O(1) — no byte-stepping. Replaces every byte-at-a-time loop inside `__value`/`__pair`.

Savepoints (`FrameStackSavepoint`, `driver.rs:256-267`) gain a `slot: u32` field so `savepoint()`/`restore()` (`driver.rs:359-381`) snapshot the structural cursor atomically with columns/psi/stack. This is the AQ-5 "unsaved structural cursor on checkpoint" failure, fixed by extending the existing savepoint record.

### 5.4 PSI interaction

`crates/bbnf-tape/src/psi.rs:380-460` reads `(span_lo, span_hi)`. With stage-1 indexing, most span boundaries *are* `idx.positions[slot]` values — emit `PayloadJob::slot_range: (u32, u32)` instead of byte offsets. `par_chunks` split partitions on slot index rather than byte offset; smaller chunks, same rayon parallelism.

### 5.5 WS-elision correctness

AQ's WS regression was input-size-sensitive. Under DTA, WS trim is a state (`DtaState::WsTrim`, `driver.rs:1218-1244`), not a per-byte hook. With stage-1 indexing, the span `[cursor.pos, idx.positions[slot])` is non-structural by construction. `DtaState::WsTrim` collapses to `cursor.pos = idx.positions[cursor.slot]` + SIMD-checkable whitespace-only validation on the skipped span (a second whitespace-bitmap pass fused into stage 1). No disabled WS elision — WS is subsumed.

### 5.6 AQ-5 failure modes addressed

| AQ-5 failure | Canonical fix |
|---|---|
| Scalar `filter_quote_parity` | CLMUL/PMULL parity folded into stage-1; inside-string bytes masked. |
| Duplicated Alt match arms | `DtaState::AltLinear` consumes `idx.kinds[slot]`; one match, not per-branch peek. |
| Unsaved structural cursor on checkpoint | `FrameStackSavepoint.slot: u32` snapshotted with other lengths. |
| Disabled WS elision | §5.5 — WS collapses to cursor jump, always on. |

## 6. Cost model & expected impact

Stage-1 cost (NEON, 64 B stripe): 4 × `vqtbl1q` + 4 × shift + 4 × bitmask + 1 × OR ≈ 14 µops/stripe ≈ 3.5 cycles on M4 P-core. citm 1.7 MB: 26 600 stripes × 3.5 cy / 4.5 GHz ≈ 21 µs — well under AQ-era 300 µs because CLMUL replaces the 100 µs scalar parity, `_pext_u64` replaces branchy Vec fill, stride-64 replaces stride-16.

Walker savings: SYNTHESIS.md gives `dispatch_one` + `try_branch` self-time ≥ 24%; stage-1 cuts byte visits by ~14× at bootstrap's 7% structural density. arch-comparison.md's 3× JSON twitter / 4.9× CSS normalize projections assume exactly this flow. Wave: AW-III.W5.5, before W5.6. One focused wave; IR pass ≤ 100 LOC from present `structural_alphabet.rs`; parse-that kernel extends `structural_bitmap.rs` with CLMUL parity + `_pext` compaction; driver delta is the `Cursor` + savepoint struct outlined in §5.3.
