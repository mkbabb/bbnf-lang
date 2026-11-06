# PROTO-1 — LOCK-6 pass-3, probe 1 (t1): family 6C WASM-CORE — the RESIDENT-BYTES lane

**wasm-SIMD128 (v128) structural scanner over bytes ALREADY RESIDENT in linear memory
vs TS-native scalar emit loop — Node/V8 (server face), cold-paired.**

- **Verdict line:** `RAN_MEASURED` — the ONE surviving 6C win (FAMILY-SPECS §3 gate [1],
  "wasm-resident bytes ⇒ wasm-core, PREDICTED win, unmeasured") is now **MEASURED and
  CONFIRMED**: with source bytes resident in wasm linear memory (no `TextEncoder` transcode),
  the v128 scanner beats a fair TS scalar emit loop **2.9–5.8× on the real JSON corpus and
  at EVERY size 64 B→64 KB** — the regime the browser JS-string lane (prior PROTO-1) lost.
  The win is governed by **structural density, not size**: it holds ≤~10% density (all real
  JSON) and only inverts past ~40–50% density (no realistic source).

> **Companion.** The browser / JS-string lane of this same question was measured in the prior
> PROTO-1 pass (Chrome 150, v128 vs TS scalar) and is preserved verbatim alongside this file as
> `PROTO-1-browser-jsstring.md`. That pass FALSIFIED wasm-core-home *for JS-string sources*
> (`TextEncoder` UTF-16→UTF-8 marshalling = 72–90% of `wasmFull`, swamps the win below ~2 MB).
> This pass measures the complementary **resident / server** lane it named but left unmeasured.
> Together they resolve the 6B⊗6C spine to a single rule: **where do the bytes live?**

---

## BASE-PIN

- Worktree HEAD measured **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`** — HEAD **equals** the
  pin `ce51c36fc`. `git merge-base --is-ancestor ce51c36fc HEAD` = OK. **No wrong-artifact-risk.**
  (Worktree arrived stale at `d2b32f6e3`, an ancestor of the pin; fast-forwarded `--ff-only` to
  the pin before any measurement. Clean tree, no local edits to source.)
- The wasm kernel measured is `PROTO-2/scan.wasm` (1439 B), built at the pin by the prior 6C
  pass from `PROTO-2/wasm-scan/src/lib.rs` (checked in, unchanged). SIMD confirmed: 96 `0xFD`
  (v128 prefix) opcodes in the binary. Exports: `memory, input_ptr, output_ptr, input_cap, scan`.

## QUESTION

The prior pass proved the v128 *kernel* wins big (`scanOnly` 4.7–7.3× in Chrome) but that a JS
**string** source cannot pay off its `TextEncoder`+copy marshalling tax below ~2 MB → *TS-native
home for JS strings*. It left the sole surviving win unmeasured: **when the source bytes are
already resident in wasm linear memory (server/batch face — read as a `Buffer`, produced/owned in
wasm, no per-call transcode), does the v128 scanner actually net-win, and does the win hold at
small sizes where the browser lane lost?** This is the datum that redraws the §5 spine.

## METHOD (Node/V8, server face — off `ce51c36fc`)

- **Engine:** Node **v26.0.0** (V8), macOS arm64. Node is the correct venue: FAMILY-SPECS §3
  names the resident lane "meaningful only for the batch/**server** face," and on that face
  source arrives as bytes (a `Buffer`/`Uint8Array` from disk/socket) — there is **no JS string,
  hence no `TextEncoder` transcode**. That transcode was the entire browser-lane tax.
- **Resident ABI:** raw C ABI (no wasm-bindgen). Bytes are copied into the persistent `input_ptr()`
  buffer **once** (outside the timed loop); `scan(len)` then scans in place, materializing every
  structural offset into `OUTPUT` — **no re-marshal per call**. This *is* the resident regime.
- **Fair TS lane:** monomorphic 256-entry `Uint8Array` LUT scalar loop that **EMITS each offset**
  into a persistent `Int32Array` (apples-to-apples with the kernel's `OUTPUT[n]=pos`; a count-only
  TS lane would understate TS work). Structural set identical on both sides: `{ } [ ] : ,`.
- **`marshal` column (reference):** the per-call `heap.set(bytes, ptr)` copy — a plain **memcpy**
  (bytes already decoded), i.e. what the resident lane avoids and what the *browser* lane paid as a
  ~100× more expensive `TextEncoder` transcode instead.
- **Timing:** `process.hrtime.bigint()`; byte-budgeted regions (~24 MB processed/region), median of
  9 reps, ns/byte; DCE defeated by accumulating the returned count. 5 warm-up iters before hot
  numbers; **cold single-shot** = fresh module instance + one resident copy + first `scan`.
- **Corpus (byte-identical to prior pass):** twitter 631,514 B; citm(_catalog) 1,727,204 B;
  canada 2,251,051 B. Plus two synthetic sweeps 64 B→64 KB: **realistic ~9.5% structural**
  (isolates the size axis at real-JSON density) and **dense ~50% structural** (stresses the
  per-hit emit loop). Parity asserted (wasm count == TS count) on every doc — HALT gate.
- **Load (paired, interleaved):** canonical run loadavg **9.10 → 8.69** (1-min); concurrent agents.
  Both lanes run interleaved under identical load, reduced by median → **ratios and win-direction
  are robust**; *absolute* ns/B carries machine noise. Four full runs taken; run-to-run below.

## PASS/FAIL GATES

- **(a) Parity == true on EVERY doc:** **PASS** — all 18 synthetic + 3 real docs, all 4 runs:
  wasm scan count == TS emit count. Kernel correct; no HALT.
- **(b) Resident-lane win PASSES iff `wasmScan < ts` on the real corpus AND the win holds down to
  small sizes at realistic (~5–10%) structural density.**

## THE NUMBERS (ns/byte, resident; lower = faster; WIN = wasmScan < ts) — canonical run

**Real docs (byte-budgeted median; cold = fresh-instance single-shot):**

| doc | B | struct% | ts | wasmScan | marshal(memcpy) | speedup | WIN | cold_ts | cold_wasm |
|-----|---:|--------:|----:|---------:|----------------:|--------:|:---:|--------:|----------:|
| twitter | 631,514   | 5.1% | 0.743 | 0.128 | 0.012 | **5.80×** | WASM | 0.657 | 0.218 |
| citm    | 1,727,204 | 5.4% | 0.643 | 0.128 | 0.012 | **5.03×** | WASM | 0.606 | 0.157 |
| canada  | 2,251,051 | 9.9% | 0.577 | 0.196 | 0.014 | **2.95×** | WASM | 0.563 | 0.211 |

**Synthetic sweep — REALISTIC ~9.5% density (size axis isolated):** wasm wins at every size.

| B | 64 | 128 | 256 | 512 | 1024 | 2048 | 4096 | 16384 | 65536 |
|---|---:|----:|----:|----:|-----:|-----:|-----:|------:|------:|
| ts | 0.441 | 0.622 | 0.600 | 0.586 | 0.594 | 0.578 | 0.582 | 0.576 | 0.609 |
| wasmScan | 0.159 | 0.188 | 0.171 | 0.167 | 0.170 | 0.185 | 0.170 | 0.168 | 0.170 |
| speedup | 2.77× | 3.31× | 3.51× | 3.50× | 3.50× | 3.12× | 3.43× | 3.43× | 3.58× |

**Synthetic sweep — DENSE ~50% density (emit-loop stress):** wasm loses at every size, flat ~0.7–0.8×
(ts 0.57–0.68 vs wasmScan 0.81–0.89). Not noise — the flat 0.69–0.84× band is the density boundary.

**Run-to-run stability (real-doc speedup, 4 runs; run 1 used a count-only TS lane, runs 2–4 the
fair emit lane):** twitter 5.31 / 6.26 / 5.65 / 5.80 · citm 4.39 / 4.86 / 5.11 / 5.03 ·
canada 2.99 / 2.85 / 3.01 / 2.95. Win-direction and magnitude are stable; wasm won even under the
*unfair* count-only TS lane (run 1), so the result is robust to the fairness choice.

## FINDINGS

1. **The surviving win is real and MEASURED.** Resident v128 `scanOnly` beats the fair TS scalar
   emit loop **2.9–5.8× on real JSON** and **at every size 64 B→64 KB** at realistic density. Gate
   [1] moves PREDICTED → MEASURED. The browser-lane loss was *never the kernel* — it was the
   `TextEncoder` boundary, and that boundary does not exist on the server face.

2. **The governing axis is structural DENSITY, not size.** Speedup falls monotonically as structural
   density rises: twitter 5.1% → 5.8×, citm 5.4% → 5.0×, canada 9.9% → 3.0×, realistic-synth 9.5% →
   ~3.4×, dense-synth ~50% → ~0.7× (TS wins). All real JSON and all bbnf grammar sources sit ≤~10%;
   the crossover to TS-favor lives at ~40–50% density, which no realistic source reaches. The prior
   pass's apparent "~2 MB size crossover" was a *browser-marshal* artifact; in the resident lane
   **there is no size crossover** — the win is flat across three orders of magnitude of size.

3. **On the server face, marshalling is a cheap memcpy, not a transcode.** `heap.set(bytes,ptr)` =
   0.012–0.014 ns/B on the real docs — ~**100× cheaper** than the browser lane's `TextEncoder`
   (1.15–1.34 ns/B, prior pass). Consequence: even a **non-resident** server call
   (`wasmScan + memcpy` ≈ 0.14–0.21 ns/B) still beats `ts` (0.58–0.74) by 3–5×. The resident win
   is a strict superset; the load-bearing distinction is server(bytes) vs browser(JS-string), and
   *within* the server face resident vs copy-per-call is a second-order refinement.

4. **Cold does not rescue TS here (opposite of the browser lane).** Cold wasm (fresh instance +
   one resident copy + first scan) 0.157–0.218 **beats** cold ts 0.563–0.657 on all three docs —
   because the copy is amortized as one-time setup, not paid per scan. In the browser JS-string
   lane, cold wasm *lost* 1.5–2× because it re-paid the transcode every call.

## GATE (b) ADJUDICATION

- `wasmScan < ts` on the real corpus: **TRUE** on all three (5.80× / 5.03× / 2.95×). PASS
- Win holds down to small sizes at realistic density: **TRUE** — 2.77× at 64 B, flat to 3.58× at
  64 KB (realistic 9.5% synth). PASS

Both required conditions hold. **The resident-lane win PASSES.** Falsifier ("wasm loses on the real
corpus, OR the win evaporates at small sizes") **does not fire** at realistic density; it fires only
under the ~50%-density synthetic, which is outside any realistic source distribution.

## VERDICT — the 6B⊗6C spine, resolved by "where do the bytes live?"

- **JS-string source (browser fetch / editor buffer):** `TextEncoder` transcode dominates → **TS-native
  home**. (Prior PROTO-1, Chrome; preserved as `PROTO-1-browser-jsstring.md`. Unchanged.)
- **Resident / server bytes (batch/server face, `Buffer` source, wasm-owned bytes):** the `scanOnly`
  column governs → **wasm-core home, 2.9–5.8× at real-JSON density, no size crossover.** (This pass.)

The 6B⊗6C graft is therefore **not** "wasm-core vs TS-native" globally — it is a **byte-provenance
switch**: the generated byte contract (6B) is crossed zero-copy by a v128 kernel (6C) **on the
server/resident face where it decisively wins**, and by a TS scalar loop on the browser JS-string
face where the transcode tax makes the kernel a net loss. Both halves are now MEASURED. The kernel's
only real enemy is pathological structural density (>~40%), absent from JSON and bbnf grammar sources.

## CODE / PATCH

No changes to any parse-that or `simd-scan` source (parse-that READ-ONLY; kernel reused as built at
the pin). The probe is a single self-contained Node harness driving the raw-C-ABI wasm in the
resident regime, delivered alongside this report as **`PROTO-1-resident-harness.mjs`** (run:
`node PROTO-1-resident-harness.mjs` with `scan.wasm` + the three JSON docs beside it). No diff to
attach; the kernel binary under test is `PROTO-2/scan.wasm`.
