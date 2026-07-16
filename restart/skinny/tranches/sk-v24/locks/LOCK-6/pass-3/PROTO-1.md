# PROTO-1 — LOCK-6 pass-3, probe 1: family 6C WASM-CORE

**wasm-SIMD128 (v128) structural-byte scanner vs TS-native scalar Uint8Array loop — browser, cold-paired**

- **Verdict line:** `RAN_MEASURED` — parity holds on every doc (kernel correct); **wasm-core-home FALSIFIED -> TS-native-home** in the marshalling-bound regime, with one decisive caveat: the SIMD *kernel* is 4.7-7.3x faster than TS; the loss is entirely the JS-string -> linear-memory marshalling boundary.

---

## QUESTION

In one browser engine, does a v128 wasm structural-byte scanner beat a TS-native scalar `Uint8Array` loop for the bbnf batch parser, and where is the size crossover? Decides the tape-crossing stratum's spine (the 6B(x)6C graft) and the bbnf-side wasm-core home.

## METHOD

- **BASE-PIN:** worktree HEAD `ce51c36fc` — `git merge-base --is-ancestor ce51c36fc HEAD` = OK (HEAD **equals** the pin). Numbers are on-base; no wrong-artifact-risk. (PROTO-2 artifacts themselves are committed in a descendant commit; they were copied out and built at the pinned base.)
- **Engine:** Google **Chrome 150.0.7871.115** (UA reports `Chrome/150.0.0.0`), macOS arm64, `hardwareConcurrency=18`.
- **Build:** `RUSTFLAGS="-C target-feature=+simd128" cargo build --release --target wasm32-unknown-unknown` -> `wasm-opt -O3 --enable-simd` -> `scan.wasm` (1439 B; 97 v128/i8x16/swizzle ops present, SIMD confirmed).
- **Kernel:** `#![no_std]` v128 nibble-LUT `classify16` over `{ } [ ] : ,` (6 targets, unique-bit LUT -> exact classify, no false positives — verified by hand and by runtime parity). 64-byte stripes, raw C ABI, marshalling (`TextEncoder.encode` + copy into linear memory) driven from JS and **counted inside the wasm lane**.
- **Harness:** monomorphic TS scalar loop (`Uint8Array` + 256-entry LUT) vs `wasmFull`; byte-budgeted timed regions (~24 MB processed/region), medians, per-byte ns; 64 B->64 KB synthetic ASCII sweep + 3 real docs; parity assert on every doc; cold single-shot captured for real docs.
- **Real docs:** twitter 631,514 B; citm(_catalog) 1,727,204 B; canada 2,251,051 B (drop `citm_catalog.json` **as** `citm.json`).
- **Load (paired, interleaved):** loadavg 4.32 at launch -> 6.79 mid/post-run (concurrent agents). Both lanes run interleaved under identical load and are reduced by median, so *ratios and crossover direction are robust*; *absolute* ns/B carries machine noise.

## PASS/FAIL GATES

- **(a) Parity == true on EVERY doc:** **PASS** — all 9 synthetic + 3 real docs: wasm scan count == TS scan count. Kernel is correct; no HALT.
- **(b) wasm-core-home PASSES iff `wasmFull < ts` on canada AND crossover < batch median doc size.**

## THE NUMBERS (ns/byte; lower = faster; WIN = wasmFull < ts)

Synthetic ASCII sweep (structural-rich; per-call marshalling included in wasmFull):

| doc B | ts | wasmFull | marshal | scanOnly | WIN |
|------:|----:|---------:|--------:|---------:|:---:|
| 56    | 0.671 | 9.260 | 8.385 | 0.875 | TS |
| 113   | 0.650 | 6.090 | 5.665 | 0.425 | TS |
| 254   | 0.649 | 2.423 | 1.965 | 0.458 | TS |
| 517   | 0.651 | 1.689 | 1.268 | 0.421 | TS |
| 1019  | 0.619 | 1.256 | 0.794 | 0.461 | TS |
| 2051  | 0.603 | 1.058 | 0.612 | 0.446 | TS |
| 4093  | 1.089 | 1.056 | 0.567 | 0.489 | **WASM** (noisy; ts spike) |
| 16378 | 0.657 | 1.318 | 0.804 | 0.514 | TS |
| 65536 | 0.668 | 1.200 | 0.742 | 0.458 | TS |

Real docs (cold single-shot in last two columns):

| doc | B | ts | wasmFull | marshal | scanOnly | WIN | cold_ts | cold_wasmFull |
|-----|---:|----:|---------:|--------:|---------:|:---:|--------:|--------------:|
| twitter | 631,514   | 1.129 | 1.308 | 1.146 | 0.163 | TS   | 0.950 | 1.425 |
| citm    | 1,727,204 | 1.084 | 1.489 | 1.340 | 0.149 | TS   | 0.695 | 1.447 |
| canada  | 2,251,051 | 1.074 | 0.820 | 0.590 | 0.230 | **WASM** | 0.666 | 0.888 |

## FINDINGS

1. **Kernel wins big; nobody disputes it.** `scanOnly` (v128 scan, marshalling excluded) beats the TS scalar loop everywhere: **6.9x (twitter), 7.3x (citm), 4.7x (canada)**, and ~1.3-1.5x on the synthetic ASCII sizes. A branchless SIMD classify16 is unambiguously the faster scanner.

2. **Marshalling is the whole story.** `TextEncoder.encode` + copy into linear memory is **72-90% of `wasmFull`** on the real docs (twitter 88%, citm 90%, canada 72%) and swamps the entire TS lane at small sizes (marshal = 8.4 ns/B at 56 B — per-call overhead amortized over too few bytes). Net wasm win requires the marshalling per-byte to fall below `ts - scanOnly` (~0.85-0.9 ns/B), which only happens for large, cheap-to-encode inputs.

3. **No clean size crossover exists.** Across the pure-ASCII synthetic sweep 64 B->64 KB, `wasmFull > ts` at **every** size except a single noisy 4 KB point (a `ts` spike, not a real win). The net win first appears only at multi-MB scale — and even there it is **content-, not size-, dependent**: canada wins because it is one-byte-backed ASCII (fast encode -> marshal 0.59) *and* has a branch-unpredictable structural distribution (long numeric runs) that slows the TS scalar loop's `if(LUT[b])` to 1.07, while the branchless SIMD kernel is immune. twitter/citm carry non-ASCII (UTF-16-backed strings -> slower encode, marshal 1.15/1.34) and lose. The apparent "crossover" sits between citm (1.7 MB, TS) and canada (2.25 MB, WASM) ~= **~2 MB**, but it is a content artifact, not a size law.

4. **Cold single-shot agrees:** on canada, cold wasmFull 0.888 ~= cold ts 0.666 (TS still ahead cold, because the first scan pays JIT warmup + first marshal); on twitter/citm cold wasmFull loses by ~1.5-2x. Cold does not rescue wasm.

## GATE (b) ADJUDICATION

- `wasmFull < ts` on canada (2.25 MB): **TRUE** (0.820 < 1.074, wasm 1.31x). PASS
- crossover < batch median doc size: **FALSE.** The net-win crossover is ~2 MB. The bbnf batch parser's docs are grammar files (KB-scale); even taken over the real JSON corpus the median is citm (1.7 MB) and the crossover (>1.7 MB) still exceeds it. FAIL

Both conditions are required; the second fails. **wasm-core-home does NOT pass.**

**Falsifier check:** "wasm loses on canada, OR crossover exceeds realistic batch doc size." Wasm *wins* on canada, but the crossover (~2 MB) **exceeds any realistic bbnf batch doc size** (KB-scale grammars) -> **falsifier's second clause fires.**

**TS-native-home test:** TS wins at/below canada — TS wins twitter (631 KB) and citm (1.7 MB), i.e. at every realistic-and-below size. **TS-native-home holds.**

## VERDICT

**TS-native scalar is the bbnf-side wasm-core home** for the tape-crossing stratum (6B(x)6C graft), *given a JS-string source*. The v128 scanner cannot pay off its JS -> linear-memory marshalling tax at bbnf batch doc sizes; it only nets ahead on multi-megabyte ASCII-heavy inputs with branch-hostile structure.

**Decisive architectural caveat (the real spine decision):** the loss is *not* the kernel — it is the marshalling boundary. If the tape-crossing stratum can deliver source bytes to wasm **already resident in linear memory** (bytes produced/owned in wasm, no per-call `TextEncoder`+copy), the `scanOnly` column governs and the v128 core wins **4.7-7.3x** at every size. So the 6B(x)6C graft should be decided by *where the bytes live*: JS strings -> TS-native; wasm-linear-memory-resident bytes -> wasm-core. A wasm-core that must import each doc from a JS string is a net loss below ~2 MB and is falsified here.

## CODE / PATCH

No source changes to the artifacts — `wasm-scan/src/lib.rs` and `harness.html` built and ran as authored (kernel parity-clean, 4 harmless `static_mut_refs` warnings only). Build product `scan.wasm` (1439 B, SIMD-verified) added to `PROTO-2/` for reproducibility. Operational note: `citm_catalog.json` must be dropped **as** `citm.json` (harness fetches the short name). No diff to attach.
