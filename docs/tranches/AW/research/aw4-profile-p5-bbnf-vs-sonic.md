# AW-IV P5 — bbnf vs. sonic-rs twin-pair attribution (`json_monolithic_value`)

Binary: `.profiles/shared-target/release/deps/json_monolithic_value-ff9373572eef197d` (post-W1.4-aggressive HEAD `2ca0f7af`).
Artefacts: `.profiles/samply/json_monolithic_value/{bbnf,sonic}_{data,twitter,citm,canada,data_xl}/`.
Expand: `.profiles/samply/prebuild/expand/json_monolithic_value/expand.rs` (9488 lines).
CPU: Apple M4 Max; cycles/byte at 4.0 GHz (frequency cancels in ratios).

## Executive summary (≤150 words)

sonic-rs runs **9.7×–13.6× faster** than bbnf across all five JSON twin pairs and spends its budget in just **two monomorphised symbols** — `parse_object::<DocumentVisitor>` and `parse_array::<DocumentVisitor>` — covering **81–88% of self-time**. Those functions are tight inner loops with a cached SIMD whitespace bitmap and compile-time-monomorphised visitor calls: no function-call boundary inside the per-token loop. bbnf's `__dta_walker_inline::run` *is* monolithic (51 state arms, ~5500 lines) but every arm tail-calls the 150-line cross-crate helper `bbnf_tape::driver::advance_or_pop_with`, and the bench then walks the tape a second time via `walk_cursor` to materialise the value tree — four symbols cover 80% of self-time. Roughly 30% of bbnf cycles are work sonic does not do at all. Closing the gap requires per-shape specialised loops (`parse_object_<grammar>` etc.) fused with an inline visitor — both primitives already exist in `bbnf-simd-scan` + `bbnf-tape`.

## 1 Per-entry bench pair table

Bytes from `data/json/*.json`; ns from per-entry `bench.txt`; cycles/byte at 4.0 GHz.

| dataset  | size (B)   | bbnf ns/iter | sonic ns/iter | bbnf MB/s | sonic MB/s | ratio  | bbnf cyc/B | sonic cyc/B |
|----------|-----------:|-------------:|--------------:|----------:|-----------:|-------:|-----------:|------------:|
| data     | 35,491     | 175,696      | 14,947        | 202       | 2,374      | 11.75× | 19.80      | 1.68        |
| twitter  | 631,514    | 3,056,262    | 238,065       | 206       | 2,652      | 12.87× | 19.36      | 1.51        |
| citm     | 1,727,204  | 7,533,774    | 584,541       | 229       | 2,954      | 12.90× | 17.45      | 1.35        |
| canada   | 2,251,051  | 19,124,604   | 1,413,654     | 117       | 1,592      | 13.61× | 33.98      | 2.51        |
| data_xl  | 21,281,177 | 140,689,191  | 14,590,591    | 151       | 1,458      | 9.66×  | 26.44      | 2.74        |

Current twitter gap is **12.87×** (task brief cited 11×). All five pairs in the 9.7×–13.6× band; the headline stands.

## 2 Per-entry top-10 self-time

Extracted from `profile.json.gz` via `/tmp/p5_top_self2.py` (resolves through `profile.json.syms.json`; leaf-inlined frame used when present).

### bbnf side — top-ranked symbol per entry

| dataset | total-samples | 1st (%) | 2nd (%) | 3rd (%) | 4th (%) |
|---|---:|---|---|---|---|
| data    | 57 871 | walker::run (47.1) | walk_cursor (13.0) | finalise fold (11.4) | advance_or_pop (10.3) |
| twitter |  3 474 | walker::run (52.8) | walk_cursor (12.1) | finalise fold (10.4) | advance_or_pop (9.5) |
| citm    |  3 117 | walker::run (49.6) | walk_cursor (14.1) | finalise fold (12.8) | advance_or_pop (11.5) |
| canada  |  7 892 | walker::run (44.7) | advance_or_pop (13.2) | walk_cursor (13.2) | finalise fold (12.0) |
| data_xl | 58 446 | walker::run (47.0) | walk_cursor (13.0) | finalise fold (11.4) | advance_or_pop (10.5) |

Ranks 5–10 draw from `psi::write_decoded` (2–5%), `NibbleLut::expand`/`neon::scan_nibble` (3–5%), `MiMalloc::alloc` (4–5% citm/data_xl), `FrameStack::nvf` (1.6–2.3%), `f64::from_str` (0.4–4%), utf8-validate (1–4%), `load` (1.3–2.6%). ("walker::run" = `__jsonparser_emit_impl::__dta_walker_inline::run`; "finalise fold" = `<Iter<u8>>::fold` under `bbnf_tape::finaliser::finalise`; "advance_or_pop" = `bbnf_tape::driver::advance_or_pop_with`; "nvf" = `FrameStack::nearest_variant_frame`.)

### sonic side — top-ranked per entry

| dataset | 1st (%) | 2nd (%) | 3rd (%) | 4th (%) |
|---|---|---|---|---|
| data    | parse_object (46.9) | parse_array (40.2) | load (5.2) | visit_container_end (2.8) |
| twitter | parse_object (74.6) | load (6.7) | deserialize_value (4.6) | parse_array (4.6) |
| citm    | parse_object (62.0) | parse_array (19.4) | load (8.2) | visit_container_end (7.0) |
| canada  | parse_array (79.1) | load (8.1) | visit_container_end (7.9) | (num kernel 2.5) |
| data_xl | parse_object (44.5) | parse_array (43.3) | load (5.5) | visit_container_end (2.6) |

Ranks 5–10 are `mimalloc` (<2%), harness (<2%), utf8-validate (<2%), `Deserializer::deserialize_value` (<1%), unresolved dyld/libsystem (<3%). (`parse_object` = `Parser<PaddedSliceRead>::parse_object::<DocumentVisitor>`; `parse_array` likewise.)

**Distinct hot symbols to reach 80% self-time:** bbnf 4, sonic 2 — all five entries.

## 3 sonic-rs architectural signature

From profiles + `~/.cargo/registry/src/index.crates.io-*/sonic-rs-0.3.17/src/parser.rs`:

- **Single hottest symbol:** `parse_object::<DocumentVisitor>` at 47%–75% on object-skewed inputs. `parse_array` at 19%–79%. On canada (array-of-arrays numerics), `parse_array` alone owns 79%.
- **Monolithic vs. dispatcher:** Monolithic. `parse_object` (parser.rs:418–446) is one inner loop `parse_string_inplace → parse_object_clo → parse_value → skip_space` — four monomorphised calls, no trait-object dispatch. LLVM inlines `parse_value`'s 6-arm byte-match (parser.rs:542–555). `<V: JsonVisitor>` monomorphised to `<DocumentVisitor>` at compile time; no vtable.
- **SIMD inlining:** Yes. `skip_space` (parser.rs:1205–1240) uses a cached 64-bit whitespace bitmap (`nospace_bits`); hot path is two non-whitespace fast-paths + `bitmap.trailing_zeros()`. String-unescape + number kernels inline into parent `parse_*` frames (symbols never exceed 1% self-time). The SIMD pre-scan is amortised and does not appear.
- **80% width:** **2 symbols**, covering 81–88% across all five entries. Remaining ~15% is load / mimalloc / UTF-8 / harness — none of it inside the parse loop.

## 4 bbnf emitted-walker signature

From `expand.rs:1356–6844` (walker) + `crates/bbnf-tape/src/driver.rs:2518` (`advance_or_pop_with`):

- **Single hottest symbol:** `__dta_walker_inline::run` at 44–53% self-time. One function, `match cur: u16` with **51 state arms** over ~5500 lines (40 of them end with an `advance_or_pop_with` call). Every arm inlines its state-specific body (literal byte-cmp, inline DFA match-loop for Regex/WsTrim, Seq reserve_compound etc.).
- **Monolithic vs. dispatcher:** Hybrid. The `match cur` is jump-table-ready (LLVM br.table on aarch64) and arm bodies are inline — but every arm terminates with a call to `advance_or_pop_with` (driver.rs:2518–2660+), a 150-line helper that walks `FrameStack` to select the next state (Seq-next, Alt-close, Repeat-re-enter-or-close). It appears standalone as 9–13% self-time — it does not inline across the crate boundary. Per-state *bodies* are inline; per-state *transitions* are not. The fast-path inline Seq-next shortcut (expand.rs:1483–1503) catches some cases; the general path dominates.
- **SIMD inlining:** Partial. `NibbleLut::expand` + `neon::scan_nibble` show 3–5% under outer frame `JsonParser::parse` → SIMD structural pre-scan runs once before the walker, does not re-enter. `DtaDfaScanner::scan` is gone after W1.4-aggressive (was 15% pre-W1.4); inline DFA bodies spliced into walker arms as labelled blocks. No per-arm quote-parity or whitespace-bitmap SIMD.
- **80% width:** **4 symbols** — walker (45–53%), bench `walk_cursor` (12–14%), `finaliser::finalise` fold (0–13%), `advance_or_pop_with` (9–13%). Only ~55–65% of self-time is in the emitted parser itself; the rest is post-parse finalisation + a second tape walk to materialise the typed value.

## 5 Key comparison — cycles/byte and interpretive tax

Breakdown for bbnf_twitter (19.36 cyc/B, median representative):

| component | %self | cyc/B | character |
|---|---:|---:|---|
| walker arm bodies | 52.8 | 10.22 | byte-cmp, DFA step, scope push/pop, tape emit |
| `walk_cursor` | 12.1 | 2.34 | **second pass** materialising typed Value |
| `finaliser::finalise` fold | 10.4 | 2.01 | post-parse max-depth |
| `advance_or_pop_with` | 9.5 | 1.83 | **pure interpretive dispatch** |
| `psi::write_decoded` | 5.1 | 0.98 | string unescape into arena |
| SIMD pre-scan | 3.5 | 0.69 | one-shot, amortised |
| FrameStack + utf8 + f64::from_str + mimalloc | ~6 | 1.16 | bookkeeping + kernels + alloc |

**Pure interpretive overhead** (sonic has none): `advance_or_pop_with + FrameStack::nvf` ≈ 11% ≈ **2.1 cyc/B**. **Second-pass materialisation** (sonic folds this inline): `walk_cursor + finalise fold` ≈ 22% ≈ **4.3 cyc/B**. **Together ~33% of bbnf cycles ≈ 6.4 cyc/B are work sonic does not do.** Of the remaining 13 cyc/B in the walker, sonic delivers comparable scope (byte compare + DFA step + number/string + emit) at 1.5 cyc/B — an ~8× per-byte density gap inside the parse loop, attributable to per-arm multi-writes into Columns + PayloadStream + frame_depth vs sonic's single visitor-pointer fused store.

## 6 Prototype question — what bbnf already has

| sonic-rs lever | available now? | missing piece |
|---|:---:|---|
| Per-shape monomorphic hot loops (`parse_object` / `parse_array` / `parse_string` / `parse_number`) | No | Walker is one loop over 51 state arms; no emitter pass projects grammar compounds as shape-specialised mutual-recursion loops. |
| Visitor monomorphised at compile time (no vtable) | Partial | bbnf writes to concrete `Columns + PayloadStream + FrameStack` (no dispatch) but per-arm emission is multi-write; a `V: JsonVisitor` trait monomorphised over `DocumentBuilder` would let LLVM fuse parse + build. |
| Direct-into-struct writes | Partial | `payload_string_with_source` zero-copy (W6) covers the direct *read* side. Direct *write* into a target `enum Value` field (f64 register → `Value::Number`) bypassing the tape is not wired. |
| SIMD-inlined string / quote-parity / whitespace-bitmap scanning | Partial | `bbnf-simd-scan` exposes `scan_nibble` + `NibbleLut` + `StructuralIndex` as a pre-scan. Per-shape-loop memchr(b'"'), quote-parity, cached `nospace_bits` are not called from per-arm bodies. |

**Substrate exists; consumer missing.** The ingredient set (`bbnf-simd-scan` NEON kernels, `bbnf-tape` Columns/PayloadStream/FrameStack, `payload_string_with_source` zero-copy) is already library code. Missing is an emitter that projects the grammar's recursive-compound shape as per-compound tight loops with an inline visitor. The current emitter is a state-machine interpreter with inlined arm bodies; sonic's shape-recursive direct-call chain is a legitimate alternative codegen projection — bbnf chose the general path. A JSON-only prototype is a second emitter pass inside the existing pipeline, not a parallel runtime.

## 7 Per-entry one-line conclusions

- **data (35 KB): sonic 11.75× faster** — sonic 2 symbols = 87% (parse_object 47% + parse_array 40%); bbnf 4 symbols = 82% (walker 47%, walk_cursor 13%, finalise 11%, advance_or_pop 10%).
- **twitter (632 KB): sonic 12.87× faster** — `parse_object` alone owns 75% (string-keyed objects in one loop); bbnf splits walker 53% + walk_cursor 12% + finalise 10% + advance_or_pop 9%.
- **citm (1.7 MB): sonic 12.90× faster** — sonic 2 symbols = 81%; bbnf 4 symbols = 88% with `advance_or_pop_with` alone at 11% (largest non-body tax in the family).
- **canada (2.3 MB): sonic 13.61× faster — worst ratio** — `parse_array` absorbs 79% (inline float kernel); bbnf's numeric path pays walker 45% + advance_or_pop 13% + finalise fold 12% + f64::from_str 4% + utf8-validate 4%: every float costs an arm transition.
- **data_xl (21 MB): sonic 9.66× faster — best bbnf showing** — mixed objects/arrays amortise fixed setup; bbnf's walker at 47% still dominates but allocation friction (5% mimalloc bbnf vs 0% sonic non-parse) narrows the relative gap at scale.

## 8 Executive diagnosis (input to B1 prototype brainstorm)

A JSON-only bbnf prototype that closes the 10–14× gap must replicate four concrete sonic properties that the current general walker cannot express. **First, per-shape monomorphic hot loops** — rather than one `__dta_walker_inline::run` with 51 state arms, emit `parse_object_<grammar>`, `parse_array_<grammar>`, `parse_string_<grammar>`, `parse_number_<grammar>` as separate functions, each a tight inner `loop { match byte { ... } }` that mutually recurses into sibling shape-functions. Recursive descent, not state-machine interpretation. The emitter projects the grammar's recursive-compound family this way for object/array/string/number while the general fn-per-rule shape remains the fallback for non-JSON grammars. **Second, eliminate `advance_or_pop_with` from the per-byte path** — its work (Seq-next, Alt-close, Repeat-re-enter) becomes an inline per-shape tail in the specialised loops (`continue`, `break`, re-loop); no FrameStack needed, each loop owns its control state on the CPU stack. **Third, fuse parse + walk into one visitor** — take the bench's `walk_cursor` (`payload_f64` / `payload_string_with_source` / recurse children) and inline it into parse arms via a monomorphic `V: JsonVisitor` the emitter knows about; `visit_number(f64)` is called from inside `parse_number_<grammar>` with the value still in a register, no tape round-trip. **Fourth, inline the SIMD kernels per shape** — `parse_string_<grammar>` gets NEON `memchr(b'"', b'\\')`; `parse_number_<grammar>` gets the NEON digit-run SIMD; `skip_space` gets a cached whitespace bitmap. All three SIMD primitives live in `bbnf-simd-scan` already — they need to be *called from inside the specialised loop bodies*, not from a pre-scan the walker re-parses. Substrate present; consumer is the specialised emitter. This is a JSON-family emitter extension folded into the existing pipeline, not a parallel code path.
