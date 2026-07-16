# LOCK-4 / pass-2 / PROTO-1 — The TS execution face under the measured V8 refutation

**Probe:** the first honest TS number for a fast isomorphic terminal (families 4C/4B/4D, INCOMPAT-2).
**Verdict:** **RAN — MEASURED — PASS.** A build-time-specialized emitter (B) beats today's
`.parser`-IC baseline by **×1.23** (median of 3 cold processes) with **zero** `.parser(`
indirection in the emitted body; the runtime switch-VM decoder (A) loses **~22%** to B and
merely **ties** the naive baseline — it is refuted as the fast face. LOCK-4 has a fast
cross-language TS terminal, **but only via a build-time emitter**, not a runtime decoder.

---

## 1. The question

INCOMPAT-2 asks whether any family can name a terminal that is *fast on V8* and *isomorphic*
to the Rust core. The refutation-prior is hostile and already measured: parse-that's own
`SpanParser` runtime-switch VM lost **10–14%** to closures on V8, was adversarially re-run at
**−14%**, then **KILLED** (`parse-that/docs/future-research.md` §7, lines 71–109), leaving the
law: *only a build-time-specialized emitter survives on V8; a runtime `switch` dispatch does not.*

The probe confronts that law with one small projection-dense region built three ways, measured
cold and paired in one process, then adjudicated:

- **PASS** iff **(B) ≥ ×1.05 baseline with zero `.parser` indirection** **AND** (A)'s V8 penalty
  is pinned to ±2%.
- **FAIL-and-name-negative** iff **(B) cannot beat baseline** (a fast isomorphic TS terminal is
  refuted — LOCK-4 has no cross-language fast face) **OR** **(A) ties (B) within noise** (the
  SpanParser law is overturned — re-open 4C-decoder-as-runtime).

## 2. The region (one, parsimonious)

The canonical value.js hot shape: the **`rgb( n , n , n )` color triple** — an
`all()`-of-mapped sequence of **arity 7**, deliberately **past the arity-2/3 unrolled arms** of
`fuseAll` (`leaf.ts:179`) so the baseline exercises the **general `.parser`-IC loop**
(`leaf.ts:254`, `parsers[i].parser(state)` per element), which is the honest thing to beat.

Leaves (parse-that's real `string()`/`regex()` primitives):
`[ string("rgb(") , regex(/[0-9]+(?:\.[0-9]+)?/) , regex(/\s*,\s*/) , regex(num) , regex(sep) , regex(num) , string(")") ]`
Projection (identical across all three lanes): `out → { r:+out[1], g:+out[3], b:+out[5] }`.

## 3. The three runtimes

All three expose the **same shape** — a raw `ParserFunction (state)->state` — and are driven by
the **same** `drive(fn, src)` (fresh `ParserState`, call `fn`, read value). This isolates the
**terminal execution face**: no packrat / error-display wrapper pollutes any lane, and the three
are byte-identical in output (verified below).

- **C — baseline (today's `.parser`-IC closure path).**
  `all(lp,num,comma,num,comma,num,rp).map(project)`. Two indirection layers: the `map`-wrapper
  `Parser.parser` (`parser.ts:146`) calls the `all` `Parser.parser`, which is `fuseAll`'s general
  arm running `parsers[i].parser(state)` — a property (`.parser`) load per element, per parse.

- **B — build-time-emitted specialized closure.** Generalizes `fuseAll` past arity-3 to a
  **`new Function`-emitted** closure. The 7 leaves' **raw functions** are captured **once** as
  lexical consts `f0..f6` (bound at emit time), and the emitted body calls them **by lexical name**
  — `f0(state); … f6(state);` — threading the *exact* drop-`undefined` + offset-restore semantics
  of `fuseAll`, then applying the bound `mapfn` inline. **No `.parser` field is read at runtime.**
  The emitted source string is exported and grepped (§5).

- **A — switch-loop VM over a Rust-`bbnf_ir`-shaped msgpack bytecode program.** The region is
  encoded as a msgpack `fixarray` of `[tag, payload]` op-tuples (`tag 0 = STRING`, `tag 1 =
  REGEX`) — the `bbnf_ir` leaf-op shape, `to_vec_named`-isomorphic — **98 bytes**. At load the blob
  is **decoded once**; STRING payloads kept as literals, REGEX payloads **re-derived as sticky
  `RegExp`** (`/…/y`). The per-parse hot loop is a `for` + **`switch(op.tag)`** — the exact
  runtime-dispatch shape the SpanParser prior refuted.

  > **Provenance note (honest).** The 98-byte msgpack blob is hand-encoded **in-harness** to the
  > `bbnf_ir::to_vec_named` leaf-op shape; a live Rust serialize round-trip was outside the 50-min
  > cap. The perf question for A is **invariant to byte provenance** — A's cost is the runtime
  > `switch` over a decoded ops array with sticky regexes, not the bytes' origin. Provenance affects
  > isomorphism-of-record, not the V8 dispatch cost measured here. The decode + RegExp construction
  > happen **once at load**, amortized out of the hot loop, so A's measured penalty is a **lower
  > bound** on a runtime-decoder face (a per-parse re-decode would be strictly worse).

## 4. Method — interleaved paired-cold sampler

`vitest` process (the `vitest bench` family harness). tinybench's per-function isolation is *not*
used — the brief mandates **sample-interleaved** A,B,C measured adjacent in time so the lanes share
thermal/GC/load conditions and the **ratio** is robust.

- Per sample: one **K=3000**-iteration batch of each lane, timed with `performance.now()`.
- Order **alternated per sample** (`ABC` on even, `CBA` on odd) — "both orders", cancels order bias.
- **N = 240** samples (≥ 200) per window, **2 windows** per process, **WARMUP = 60 000** iters/lane
  to tier every lane to V8's optimizing compiler before sampling.
- Report per-lane **median** batch-ms + **MAD**; pooled across windows = median-of-window-medians.
- Repeated across **3 independent cold-V8 processes** at **different system loads**.
- `os.loadavg()` recorded per window; correctness re-asserted at the end of every run.

**Environment:** Apple **M5 Max**, 18 CPUs, **Node v26.0.0**, **vitest 3.2.4**, macOS 25.4.0.
Machine under heavy concurrent load throughout (loadavg 9.98–13.4 of 18).

## 5. Instrumentation gate (pre-bench)

`vitest run test/lock4-proto1-verify.test.ts` — 3/3 pass:

- **All three lanes byte-identical** on `"rgb(255, 128, 0)"` → `{ r:255, g:128, b:0 }`; identical
  rejection of `"rgb(255,)"` (all → `undefined`).
- **B emitted-source `.parser(` count = `0`** (regex `/\.parser\(/g`). The emitted body is 7
  `f{i}(state)` calls + inline `mapfn(out)` — reproduced in full in the test log.
- A program byte length = **98** (msgpack).

## 6. The numbers (batch-median ms; ratios are the deliverable)

Three independent cold-V8 processes; each row is that process's pooled (median-of-2-windows):

| run | loadavg₁ | A med (ms) | B med (ms) | C med (ms) | **B ÷ C** (B speedup) | **A ÷ B** (A penalty) | **A ÷ C** |
|-----|---------:|-----------:|-----------:|-----------:|----------------------:|----------------------:|----------:|
| 1   | 13.45    | 0.5542     | **0.4555** | 0.5606     | **×1.231**            | ×1.217                | ×0.989    |
| 2   |  9.98    | 0.5349     | **0.4361** | 0.5145     | **×1.180**            | ×1.227                | ×1.040    |
| 3   | 10.06    | 0.5358     | **0.4397** | 0.5412     | **×1.231**            | ×1.219                | ×0.990    |
| **median** | | | | | **×1.231** | **×1.219** | **×0.990** |

Per-window stability inside run 1 (paired, both orders): `B÷C` = 1.227 / 1.234; `A÷B` = 1.224 /
1.209 — windows agree within ~1.5%. Per-sample dispersion MAD ≈ 0.03–0.04 ms (~7% of median);
the **ratio** is far tighter because the lanes are paired-cold.

**Reading the ratios against the axes:**
- **B beats baseline** by **×1.18–×1.23** (median ×1.23) — comfortably past the ×1.05 gate,
  every process, both load windows. Emitted body carries **zero** `.parser` loads.
- **A's penalty vs B** is **×1.217–×1.227** — **pinned within 0.8%**, i.e. inside ±2%. A is
  ~**22% slower** than the specialized closure.
- **A vs baseline C** = **×0.99–×1.04** — A **ties the naive baseline** (within ±4%, straddling 1).

## 7. Adjudication

**PASS conditions — both met:**
1. **(B) ≥ ×1.05 baseline with zero `.parser` indirection** → ×1.23 median, ×1.18 worst, `0`
   `.parser(` in the emitted body. ✔
2. **(A)'s V8 penalty pinned to ±2%** → A÷B measured at 1.217 / 1.227 / 1.219 across three
   independent processes — spread 0.8%, inside ±2%. ✔

**FAIL triggers — neither fired:**
- *(B) cannot beat baseline* → did not fire; B wins decisively and reproducibly.
- *(A) ties (B) within noise* → did not fire; A is ~22% slower than B (≫ the ~1% ratio noise),
  consistent across 3 processes. The SpanParser law **survives**.

**On the refutation-prior (§7 of future-research.md):** the prior measured the tagged
runtime-`switch` VM at **−10…−14%** vs *the closure lane*. This probe **confirms and sharpens** it:
- vs the **build-time-specialized** closure (B), the switch-VM (A) is **−22%** — *worse* than the
  prior, because the prior's closure lane was un-specialized. Build-time specialization widens the
  gap the runtime `switch` must overcome.
- vs the **naive** `.parser`-IC closure (C — the true isomorph of the prior's "closure lane"), A
  **ties** (±4%). So the honest refinement is: *the runtime-decoder is not slower than a naive
  combinator baseline — it is decisively beaten only once you build-time-specialize.* Either way,
  A is **not a fast face**: B dominates it by ~22%.

## 8. What this decides for LOCK-4

- **A fast isomorphic TS terminal EXISTS** — it is **family B**: a build-time emitter that
  specializes a static parser list into one monomorphic closure calling its members **by lexical
  name**, with the map fused inline and **no `.parser` field** touched at runtime. INCOMPAT-2's
  "no terminal can be named without a fast TS face" is **satisfiable** on this route.
- **The runtime-decoder face (4C-decoder-as-runtime) stays CLOSED as the fast face.** A `switch`-VM
  over decoded bytecode is a legitimate *portability/size* face (98 bytes, ties naive baseline) but
  is **~22% off** the emitter and therefore cannot be the *fast* terminal. The brief's re-open
  condition ("A ties B") did not trigger.
- **Consequence for the family cut:** any terminal LOCK-4 names for the TS face must lower through
  a **build-time specialization pass** (the generalized-`fuseAll` emitter), not a runtime VM. The
  Rust core and the TS emitter share the *IR* (the same op list drives both `to_vec_named` and the
  emitter), but the TS *execution* face is specialized at build time — isomorphic in structure,
  divergent in lowering strategy, exactly as the SpanParser law requires.

## 9. Artifacts

- `proto1-code/lock4-proto1-runtimes.ts` — the three runtimes (C baseline, B emitter, A msgpack
  switch-VM) + shared driver/corpus. Imports parse-that's real `all/string/regex/ParserState`.
- `proto1-code/lock4-proto1-verify.test.ts` — equivalence + `.parser(`-grep + byte-length gate.
- `proto1-code/lock4-proto1-sampler.test.ts` — the interleaved paired-cold sampler.
- Ran on a **read-only rsync scratch** of `parse-that/typescript` (excl. node_modules/.git/dist;
  node_modules symlinked) — parse-that untouched. Files placed at `test/` top-level to match the
  scratch's `vitest.config.ts` include (`test/*.test.ts`).

**Reproduce:** in the scratch, `npx vitest run test/lock4-proto1-verify.test.ts` then
`npx vitest run test/lock4-proto1-sampler.test.ts` (grep `[PROTO1-RESULT]`).

---

### VERDICT LINE

**RAN / MEASURED / PASS** — (B) build-time-emitted specialized closure = **×1.23** over the
`.parser`-IC baseline with **0** `.parser(` in the emitted body; (A) runtime switch-VM = **−22%**
vs B (penalty pinned to ±0.8% across 3 cold processes), **ties** naive baseline. Fast isomorphic TS
terminal **CONFIRMED via a build-time emitter**; runtime-decoder-as-fast-face **REFUTED**; the
SpanParser law **survives**. LOCK-4 may name a TS terminal **only** if it lowers through the
specializing emitter, never a runtime VM.
