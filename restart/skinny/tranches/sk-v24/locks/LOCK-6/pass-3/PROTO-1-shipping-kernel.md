# PROTO-1 (shipping-kernel re-measurement) — LOCK-6 pass-3, probe 1: family 6C WASM-CORE, the RESIDENT-BYTES lane

**Shipping `crates/simd-scan` v128 kernel, exposed as a raw-ABI wasm cdylib, over bytes
ALREADY RESIDENT in linear memory — vs a marshalled arm and a TS-native scalar arm.
Node/V8, cold, paired, three independent runs.**

- **Verdict line:** `RAN_MEASURED` — **MIXED, re-draws the §5 spine as BIMODAL along
  (doc-size × batch), not flat.** The v128 kernel win *does* survive residency — but with the
  **actual shipping entry point** (`scan_structural`, which allocates a fresh `StructuralIndex`
  and rebuilds its LUTs per call), the win is **size/batch-gated**, not the flat 2.9–5.8× the
  prior bespoke-ABI probe reported. It **clears ≥2× only from ~8 KB batched upward** (8 KB×8 =
  2.9×, 64 KB = 2.5–3.7×, real twitter.json 631 KB = 2.6×) and **fails ≥2× at the smallest bbnf
  grammar-doc size** (1 KB single = **0.9×**, 1 KB batched = 1.5–1.8×). So 6C's zero-copy crossing
  **pays for the batch/server face and is marginal-to-losing for small single browser docs** —
  exactly the INC-1 regime ("the ONLY regime the crossing pays").

> **Relationship to the prior probe.** An earlier pass-3 probe-1 run (`PROTO-1.md`, 20:17,
> preserved verbatim as `PROTO-1-prior-bespoke.md`) measured a **bespoke** kernel
> (`PROTO-2/wasm-scan`) through a raw C ABI that wrote offsets into a **persistent OUTPUT buffer**
> — eliding per-call allocation, LUT rebuild, and boundary cost — and reported a **flat 2.9–5.8×
> with "no size crossover."** This run measures the **actual shipping `crates/simd-scan` crate as
> shipped** (path-dep, `scan_structural` returning a heap `StructuralIndex`). The two reconcile:
> the **SIMD scan itself** wins flat (prior), but the **shipped API's per-call fixed cost** erodes
> the win below ~8 KB (this run). The "no size crossover" claim was an artifact of the persistent-
> OUTPUT ABI; the real shipping crate has an **~8–64 KB crossover.**

---

## BASE-PIN

- Worktree HEAD measured **`ce51c36fcbdf5108eb028005586672d9a6d2ca1b`** — HEAD **equals** the pin
  `ce51c36fc`. `git merge-base --is-ancestor ce51c36fc HEAD` = OK, before and after all builds.
  **No wrong-artifact-risk.** Clean tree; the only additions are the untracked probe crate
  `wasm/bbnf-wasm-6c/` (patch attached).
- Kernel under test: `crates/simd-scan/src/wasm.rs` (v128 `i8x16.swizzle` nibble-LUT + `i8x16.bitmask`
  + shared shift-XOR quote parity), reached via `simd_scan::scan_structural` → `wasm::scan`.
  Built cdylib **`bbnf_wasm_6c.wasm` (21,183 B)**, **156 `0xFD` (v128 prefix) bytes** present
  ⇒ SIMD kernel is live in the binary. Exports: `memory, input_ptr, input_cap, scan_resident`.

## QUESTION

Does the v128 `scanOnly` win (4.7–7.3× vs TS-scalar in Chrome, already proven) **survive when the
source is already resident in wasm linear memory** (no per-call `TextEncoder` marshal), at bbnf
batch/server doc sizes? PROTO-6C measured only the marshalled JS-string lane and falsified
wasm-core-home there. Residency is the plane's weakest-evidenced row and the only regime the
6B⊗6C zero-copy crossing can pay (INC-1).

## METHOD (Node v26.0.0 / V8, macOS arm64, off `ce51c36fc`)

- **Build:** shipping `crates/simd-scan` path-dep, `cargo build --release --target
  wasm32-unknown-unknown` with `RUSTFLAGS="-C target-feature=+simd128"` (simd128 is how the
  `core::arch::wasm32` intrinsics in `wasm.rs` are enabled; a bare cargo `feature` would not turn
  the target-feature on). Detached single-package workspace (`wasm/bbnf-wasm-6c/`) so nothing joins
  the root graph. Isolated `CARGO_TARGET_DIR` in scratchpad.
- **Resident ABI (raw C, no wasm-bindgen):** a 4 MiB `static mut` linear-memory arena. The harness
  writes source bytes into `input_ptr()` **once, outside the timed region**; `scan_resident(off,len)`
  then scans in place via the shipping kernel. Return value = structural-offset count (defeats DCE).
- **Three arms, identical structural set** (singletons `{ } [ ] : ,` outside JSON string bodies,
  backslash-parity escaping — parity is a HALT gate, asserted wasm-count == ts-count on every config):
  - **(a) resident** — copy once, then time `scan_resident` alone.
  - **(b) marshalled** — time `TextEncoder.encode(str)` (UTF-16→UTF-8) + `heap.set` copy + scan,
    per doc: the PROTO-6C browser lane, sanity anchor.
  - **(c) scan.ts** — TS-native scalar loop, monomorphic 256-entry `Int8Array` LUT, replicates the
    kernel's tail epilogue exactly, emits each offset into a **persistent** `Int32Array`.
- **Corpus:** real `twitter.json` (631,515 B) as the anchor; sweep **doc-size {1, 8, 64} KB × batch
  {1, 8, 64}** where each "doc" is a real-density slice of twitter (realistic byte distribution;
  identical bytes on both lanes so parity holds regardless of slice validity).
- **Timing:** `process.hrtime.bigint()`; metric **ms/MB** (lower = faster); **speedup = ts/res**
  (>1 ⇒ wasm wins). **Cold + paired:** a **fresh wasm instance per trial** (cold), the three arms
  **interleaved on identical bytes**, **median of 15 cold trials** per config, `--expose-gc` between
  trials. Three full independent runs. loadavg recorded per run.
- **Load (paired, interleaved):** run1 loadavg 6.90; runs 2–3 loadavg **20.09** (heavy concurrent
  agents). Absolute ms/MB rises under load; **ratios and win-direction are robust** because both
  arms run interleaved under identical load — confirmed by the stable cross-run pattern below.

## PASS/FAIL GATE (brief)

- **GREEN** iff `scan_resident` beats `scan.ts` by **≥2× at KB-scale batch sizes**.
- **RED** iff `scan_resident` fails 2× at realistic bbnf sizes (win is large-doc/batch-only).

## THE NUMBERS — speedup (ts/res), 3 runs + median (WIN = ≥2×)

| doc×batch | total B | run1 | run2 | run3 | **median** | ≥2×? |
|-----------|--------:|-----:|-----:|-----:|-----------:|:----:|
| **1KB × 1**  | 1,024   | 0.91 | 1.05 | 0.91 | **0.91** | **NO** |
| **1KB × 8**  | 8,192   | 1.41 | 1.58 | 1.53 | **1.53** | **NO** |
| **1KB × 64** | 65,536  | 1.49 | 1.79 | 1.79 | **1.79** | **NO** |
| **8KB × 1**  | 8,192   | 1.75 | 2.23 | 2.07 | **2.07** | ~yes |
| **8KB × 8**  | 65,536  | 2.25 | 3.02 | 2.91 | **2.91** | YES |
| **8KB × 64** | 524,288 | 2.42 | 2.99 | 2.78 | **2.78** | YES |
| **64KB × 1** | 65,536  | 2.71 | 2.73 | 2.75 | **2.73** | YES |
| **64KB × 8** | 524,288 | 3.71 | 3.24 | 2.50 | **3.24** | YES |
| **64KB × 64**| 4.19 MB | 2.85 | 3.46 | 3.16 | **3.16** | YES |
| **twitter.json** | 631,515 | 2.58 | — | — | **~2.58** | YES |

**Absolute ms/MB (run1) — the fixed-cost story:** resident falls **1.83 (1KB×1) → 0.35 (64KB×64)**
— a ~5× swing that is *entirely resident-side per-call fixed cost being amortized*; `scan.ts` is
**flat ~1.05–1.95 ms/MB across all sizes** (pure JS, no boundary, reuses its `Int32Array`). The
crossover is caused by the wasm lane's per-call tax, not by the SIMD scan.

**Marshalled arm (mar/res, browser-lane tax) — corroborates PROTO-6C:** 1KB×1 = **8.6–10.6×**
(fixed `TextEncoder` transcode dominates tiny docs), decaying to **1.0–1.7×** at 64 KB / twitter.
The JS-string lane's tax is real and largest exactly where the resident lane is already weakest.

## FINDINGS

1. **The kernel win survives residency — but is SIZE/BATCH-GATED with the shipping API.** Resident
   `scan_structural` beats fair TS scalar **2.5–3.7× from ~8 KB batched upward and on the real
   631 KB corpus (2.6×)**, but **only 0.9× at 1 KB single** and **≤1.8× for all 1 KB configs**.
   The ≥2× crossover sits at **total-bytes ≈ 8–64 KB**.

2. **The small-size deficit is per-call FIXED cost, not the SIMD scan.** `scan.ts` ms/MB is flat
   across three orders of magnitude; resident ms/MB is not. The shipping entry point pays, *per
   call*: (i) the JS↔wasm boundary crossing, (ii) `NibbleLut::from_singletons` + `expand()` +
   `build_quote_byte_lut` rebuilt every call in `scan_nibble`, (iii) a fresh `StructuralIndex`
   (two `Vec` allocations) it returns and drops. At 1 KB these dominate; by 64 KB they amortize.

3. **This corrects — not contradicts — the prior probe.** The bespoke persistent-OUTPUT ABI
   measured the *raw SIMD scan* (flat 2.9–5.8×, no crossover). Measuring the *shipped crate as
   shipped* reveals the real ~8–64 KB crossover the bespoke ABI hid. Both are true at their own
   altitude: SIMD wins flat; the shipped API's per-call overhead gates the small end.

4. **The marshalled (browser JS-string) lane loses hardest where the resident lane is weakest.**
   `TextEncoder` tax is 8.6–10.6× the resident scan at 1 KB. So on the *browser small-doc* face,
   6C is doubly disadvantaged (marshal tax + unamortized per-call cost); on the *batch/server*
   face (resident bytes, ≥8 KB batched) it clears ≥2× cleanly.

## GATE ADJUDICATION

- **≥2× at KB-scale batch sizes — NOT uniformly met.** 8 KB batched (×8, ×64) and 64 KB clear ≥2×;
  **1 KB (single and batched) does NOT** (0.9–1.8×), and 8 KB *single* is borderline (2.07×).
- Falsifier "win is large-doc/batch-only" **partially fires** for the small single-doc face and
  **does not fire** for the batch/server face.
- **Adjudication: MIXED.** Neither a clean GREEN (not ≥2× at *all* KB-scale sizes) nor a clean RED
  (it is *not* large-doc-only — it wins from ~8 KB batched, well below "large"). The honest verdict
  is **bimodal**, which the brief anticipates ("Either verdict re-draws the §5 spine").

## VERDICT — the §5 spine, re-drawn as BIMODAL along (provenance × size × batch)

- **JS-string source, small/browser (fetch, editor buffer, ≤~8 KB):** marshal tax + unamortized
  per-call cost ⇒ **TS-native home.** (PROTO-6C + this run's 1 KB rows + marshalled arm.)
- **Resident/server bytes, batched or ≥~8 KB, up to the real 631 KB corpus:** shipping kernel wins
  **2.5–3.7×** ⇒ **wasm-core home.** (This run.) This is the INC-1 regime; the 6B⊗6C zero-copy
  crossing **pays here and only here.**
- **Actionable spine implication (as-shipped vs achievable):** the small-doc deficit is *fixable in
  the kernel API*, not intrinsic. A `scan_into(&mut StructuralIndex)` entry point (clear+reuse
  buffer) plus hoisting the LUT build out of `scan_nibble` would remove costs (ii)+(iii) and
  push the crossover down toward the raw-SIMD flat win the prior probe saw. The crossing pays for
  batch/server today; extending it to small browser docs is a bounded kernel-API amendment, not a
  new subsystem.

## CODE / PATCH

New detached probe crate `wasm/bbnf-wasm-6c/` (Cargo.toml + src/lib.rs + harness.mjs), reusing the
shipping `crates/simd-scan` kernel verbatim (path-dep, no fork). No edits to any workspace source;
parse-that untouched (not referenced). Full unified diff attached as `PROTO-1-shipping.patch`;
runnable harness attached as `PROTO-1-shipping-harness.mjs`
(`node --expose-gc harness.mjs <bbnf_wasm_6c.wasm>`, corpus resolved from `skinny/test_data/`).
