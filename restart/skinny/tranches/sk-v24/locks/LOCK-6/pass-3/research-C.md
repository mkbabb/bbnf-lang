# LOCK-6 · Family 6C — WASM-CORE · Pass-3 Research (research-C)

The Rust core compiled to wasm is the one implementation; TypeScript is bindings plus an
idiom layer; TS-native code exists only where V8 measurably wins. This report answers the
three 6C directives with concrete mechanism, in-tree substrate, integration seams, prior art,
named risks, and the exact prototype that moves the family. Every claim names its byte or its
citation.

---

## The substrate already exists (the physical core)

WASM-CORE is not a greenfield gamble; the two organs it needs are already in-tree and proven.

- **A real wasm-SIMD128 structural scanner.** `crates/simd-scan/src/wasm.rs` (433 LOC) is a
  working SIMD128 kernel: `pub fn scan(input, alphabet) -> StructuralIndex`
  (`wasm.rs:49`), nibble-LUT swizzle classification `classify_chunk_nibble` (`wasm.rs:238`,
  `u8x16_swizzle`/`v128_and`/`u8x16_ne`/`u8x16_bitmask`), 64-byte stripes (4 × `v128` per
  stripe, matching NEON), and a dedicated quote/escape structural scan (`wasm.rs:302`). It
  is the byte-for-byte functional mirror of `neon.rs` in the same directory-module
  (`alphabet.rs`, `neon.rs`, `wasm.rs`, `scalar.rs`, `avx2.rs`, `avx512.rs`, `parity.rs`,
  `index.rs`, `compaction.rs`). The x86 arms (`avx2.rs`/`avx512.rs`) are dead by refutation
  law iv; the live browser-deployable arm is **`wasm.rs`**, its native sibling `neon.rs`, and
  the scalar oracle `scalar.rs`. This one directory-module *is* LOCK-9's one-kernel-library
  thesis realized: one classifier, three emit backends, one scalar parity floor (`parity.rs`).
- **A cdylib home.** `wasm/Cargo.toml`: crate `bbnf-wasm`, `crate-type = ["cdylib","rlib"]`,
  `wasm-bindgen = "0.2"`, `serde-wasm-bindgen = "0.6"`; entry `wasm/src/lib.rs`. The cdylib
  boundary already compiles.
- **A byte-exact tape crossing.** PROVE-14 (CONFIRMED) proved the four SoA planes
  `Tape::{source, offsets, flag_cursors, flag_values}` (`crates/runtime/src/tape/mod.rs`
  ~121-137) decode node-for-node identically in TS over typed arrays, and stop-early get
  clears 2× at **4.4×** (full-walk honestly at ~0.9× parity, abrogated). GN-4.W4 is the
  productionization plan: `crates/bbnf-wasm/src/tape_export.rs` exports the four planes as
  `Uint32Array`/`Uint8Array`; `ts/tape/src/tape.ts` is the monomorphic `TapeDoc` decoder (4
  ctor-only fields, single hidden class, byte-hash fast path).

Caveat of record: top-level `crates/` and `wasm/` are archival per the constitution; GN-1
ports these organs into the greenfield workspace. The point stands — the wasm-SIMD128 kernel
and the cdylib are *proven*, not hypothesized, and 6C re-homes rather than invents them. This
is the concrete shape of "the Rust core is the one implementation": the browser runs `bbnf-wasm`
= {`simd-scan::wasm::scan` structural index} → {tape build} → {four-plane export}; TS holds
only `TapeDoc` + the pointer/stop-early idiom.

---

## Q1 — wasm-SIMD128 kernel vs TS-native scalar structural scan

**The two options, both browser-deployable.**
- **Option A (wasm-SIMD128).** `simd-scan::wasm::scan` compiled into `bbnf-wasm`, invoked
  once per document (or once per batch buffer). Classifies structural/quote/escape bytes via
  the nibble-LUT swizzle at 16 B/instruction, 64 B/stripe. Costs: one-time module
  instantiate (amortized across the batch), a source copy-in across the ABI boundary
  (`Uint8Array` → wasm linear memory), then near-memory-bandwidth classification.
- **Option B (TS-native scalar).** A `scan.ts` byte loop (the V8-side mirror of `scalar.rs`)
  building the same `StructuralIndex`. Costs: zero instantiate, zero boundary copy, but
  cold-JIT ~1 byte/iteration with branch-per-byte.

**Prior art sets the throughput ceiling.** simdjson's WASM-SIMD128 path (Langdale & Lemire,
the "fallback→v128" port) runs the same movemask-narrow classification this kernel uses and
reaches ~1 GB/s in-browser, against V8 `JSON.parse` at 200–400 MB/s for ASCII-key objects and
native NEON simdjson at 1–2 GB/s ([jsonic.io JSON-performance guide]; simdjson project). WASM
SIMD128 is **128-bit only** — no AVX2/512 width in the browser — so the ceiling is set by
issuing 4 × `v128` per 64-byte stripe (exactly what `wasm.rs` does), yielding a real ~4–8×
over the scalar loop but never the 32–64 B/cycle of a native AVX host. The medium/dev.to
"JS vs wasm-bindgen vs raw-wasm+SIMD" measurements corroborate: raw wasm-SIMD beats scalar JS
on data-parallel scans once the payload clears the boundary-crossing fixed cost.

**But PROVE-14's law binds the cold-small case.** Cold V8 is a hand-optimized C++ near-optimum
on small inputs; on a single 2 KB CSS value parsed once, wasm's instantiate + copy-in dwarfs
the scan and **the scalar TS lane wins**. Asserting the wasm win for that regime is the
refutation-law-iv "stable-port perf claim" trap. The discriminator is exactly the family's
deployment context — **"the bbnf *batch* parser"**: a batch amortizes one instantiate across N
documents and keeps the corpus resident in linear memory, so the wasm-SIMD128 lane wins in the
regime it is built for. The answer is therefore a **crossover, not a verdict**: scalar wins
cold-single-small; wasm-SIMD128 wins batched/large; the prototype must locate the crossover
(doc-size × batch-count) and report both numbers, per the directive. Refutation guard: the
kernel classifies **structural bytes** (quote/escape/bracket via the alphabet LUT), never
numeric fields — no digit-scan SIMD (row 02); `wasm.rs` already honors this.

**What the prototype must test (base-pinned, cold, paired).** On the `bbnf-wasm` cdylib in
node-wasm and a real browser: (a) instantiate once, feed a shared corpus = {twitter.json 631 KB}
∪ {a batch of N small CSS/JSON docs}; time `structural_index` build for `wasm::scan` vs
`scan.ts`, *including* instantiate+copy-in in the wasm lane's timed region (as PROVE-14 counted
the plane copy against the tape); (b) sweep doc-size and batch-count to find the crossover; (c)
deliver both lanes' ms/MB and the crossover point. This is the first thing that moves 6C from
"kernel exists" to "kernel is the right browser choice, measured."

---

## Q2 — `memory.grow()` and view invalidation on the two-parses-through-one-instance path

**The hazard is spec-level and certain.** `WebAssembly.Memory.prototype.grow()` detaches the
backing `ArrayBuffer`; *every* typed-array view over it becomes detached (`byteLength → 0`),
**even `grow(0)`** ([MDN Memory.grow]; [WebAssembly/spec#646]; [awesome.red-badger detached
ArrayBuffer]). Reads on a detached view throw `TypeError: ... detached ArrayBuffer`. This lands
squarely on the two-parses-through-one-instance path — the exact batch regime Q1 wants. Parse
#1 hands JS a `TapeDoc` holding `Uint32Array`/`Uint8Array` views over the four planes in wasm
memory; parse #2 allocates its own source+tape, linear memory grows to fit, and parse #1's
views (and the `TapeDoc` built on them) **silently detach**. The 4.4× stop-early lane is
worthless if the tape it navigates is a detached buffer.

**Three mechanisms, priced against the stop-early margin.**
1. **Copy-out (JS owns the planes).** After each parse, `.slice()` the four planes out of wasm
   memory into JS-owned `ArrayBuffer`s; `TapeDoc` holds detach-immune copies. Cost = memcpy of
   the tape planes only — for twitter.json the tape is **119,852 B (18.98% of source)**,
   dominated by `offsets.u32` (118,292 B); the source plane need not be copied if leaves are
   materialized eagerly, or is copied lazily on demand. A ~119 KB copy is cheap against a full
   631 KB `JSON.parse`, so the **stop-early ≥2× margin survives** (the copy is a fraction of the
   parse it replaces); but for the *full-walk* lane already at ~0.9× parity, the copy pushes it
   further under 1.0 — consistent with GN-4's abrogation of full-walk as a goal.
2. **Pre-reserve / never-grow.** Instantiate `WebAssembly.Memory` with a `maximum` sized to the
   largest batch document, so `grow()` never fires mid-batch and views stay valid. Trades peak
   memory for zero copy-out. Feasible precisely because the tape is **offset-only** — PROVE-14
   proved the payload arena empty — so arena size is predictable (source + ≤~19% offsets).
3. **Re-materialize views per call** (`instance.exports.memory.buffer` re-read, the
   wasm-bindgen idiom, cf. wasm-bindgen#4395 / emscripten `HEAPU8` re-slice). Necessary for the
   *caller* after any grow, but **insufficient alone** for a long-lived `TapeDoc` captured
   across a second parse — a captured view cannot be retroactively re-based. So mechanism 3 is a
   correctness-hygiene rule, not a two-parses solution; the two-parses path needs 1 or 2.

**The corruption counterexample (why this is mandatory, not optional).** If a `TapeDoc` naïvely
re-fetches `.buffer` after grow, it reads **parse #2's bytes at parse #1's offsets** — a false
parity PASS on ASCII-only leaves and silent corruption on the rest. This is the same class as
GN-4 risk R4 (wasm-plane drift passing JSON-parity by ASCII coincidence). Copy-out or
pre-reserve is a correctness precondition of the batch face.

**What the prototype must test.** Drive two parses through one `bbnf-wasm` instance where parse
#2 forces `grow()`; assert parse #1's `TapeDoc` reads throw/corrupt **without** mitigation; then
measure (i) copy-out cost (plane memcpy ns) and confirm stop-early holds ≥2×, and (ii) the
pre-reserved-max variant's peak-memory cost with zero copy. Report copy-out ns against the
stop-early ns budget — the directive's exact ask.

---

## Q3 — the confinement datum, re-confirmed in-tree, and cdylib vs two instances

**In-tree re-confirmation: the confinement datum is SUPERSEDED — the divorce has landed.**
value.js is now **4.0.0** in the working tree (`value.js/package.json:3`) and has **divorced
parse-that entirely**: zero `@mkbabb/parse-that` imports under `value.js/src` (the only hits are
comments boasting "parse-that-FREE"), and its parsing is its own hand-written surface — 9 public
synchronous entries `parseStylesheet`/`parseCssColor`/`parseCssScalar`/`parseCssValue`/
`parseCssValues`/`parseKeyframeSelector`/`parseTimingFunction`/`parseAnimationTimeline`/
`parseAnimationRange` in `value.js/src/v4/css/{grammar,stylesheet}.ts`. The XL-G "open
value.js-4.0.0 divorce" is **realized at HEAD**: value.js no longer confines parse-that at all.
keyframes.js@6.0.0 now depends only on `@mkbabb/value.js@4.0.0` (`keyframes.js/package.json:69`)
— no parse-that, direct or transitive — so it too is fully detached.

The R4-era census (value.js consuming `all()`/`dispatch()` across dozens of sites) is
**obsolete**. The **only** remaining live parse-that consumer in-tree is **latex-paper**
(`@mkbabb/parse-that ^0.7.1`, 8 grammar files import combinators: `src/grammar/{primitives,
commands,math,text,compile,document,environments}.ts` + `src/bibtex/parser.ts`), and it exposes
**zero `.parse()` sync sites** in `src` — its parse entry is a grammar-compile step behind
`compile.ts`, not scattered synchronous call sites. Note the version skew: latex-paper is pinned
pre-1.0.0 (`^0.7.1`), so it does not even track the current parse-that.

**Consequence for wasm-core.** The historical objection to a wasm-core — "wasm instantiate is
async, but parse-that's `.parse()` is synchronous, so every sync call site needs a ready
instance" — has **largely evaporated**. value.js's dozens of sync sites are gone (it divorced);
keyframes is detached; the residual sync-confinement surface is latex-paper's single
grammar-compile entry plus any direct parse-that CLI/tests. A wasm-core parse-that faces a
top-level-await/bootstrap boundary at essentially **one** consumer entry, not a census. **FLAG
(F6/owner-bound):** this collapse rests on value.js@4.0.0 HEAD; an owner could re-marry a future
value.js to parse-that, restoring confinement — revalidate at the gestalt. As of HEAD, confined
sync surface ≈ latex-paper alone.

**Price: one consolidated cdylib vs two cold instances.**
- **Two cold instances** (`bbnf-wasm` + a separate `parse-that-wasm`): each carries its own
  linear memory, its own module compile+instantiate — and module compile is the **dominant cold
  cost** (a nontrivial module is hundreds of KB → several ms to compile+instantiate) — and
  **double the source copy-in** when both parse the same buffer. Note parse-that's Rust port has
  **no cdylib today** (`parse-that/rust/Cargo.toml` declares no `crate-type`/wasm-bindgen), so
  the second instance is itself greenfield.
- **One consolidated cdylib** (bbnf tape core + parse-that combinator core in one `bbnf-wasm`):
  single instantiate, single linear memory the corpus lives in *once*, and a **shared structural
  scanner** — `crates/simd-scan` is already the one kernel both faces consume, so consolidation
  is the physical form of LOCK-9's one-kernel-library and the constitution's "Rust core is the
  one implementation." Strictly cheaper on cold instantiate and on memory. Cost: it couples the
  two release cadences into one artifact and bloats the smallest consumer's download
  (latex-paper would pull the whole bbnf core for a LaTeX grammar — its bundle could triple).
  Mitigation: the consolidated cdylib serves the batch/server face; thin per-consumer builds via
  feature-gated cdylib + wasm tree-shaking serve leaf consumers.

**What the prototype must test.** Build both variants — (a) one `bbnf-wasm` exporting both the
bbnf-tape parse and a parse-that combinator parse; (b) two separate cdylibs each cold-instantiated
— and measure cold `instantiate + first-parse` ms and peak memory for each on a shared corpus.
Report both, per the directive.

---

## Integration seams (path:line — where extant code serves)

- **wasm-SIMD128 kernel**: `crates/simd-scan/src/wasm.rs:49` (`scan`), `:238`
  (`classify_chunk_nibble`), `:302` (quote/escape scan); native mirror `neon.rs`; scalar oracle
  `scalar.rs`; parity floor `parity.rs`; alphabet `alphabet.rs`. (GN-1 ports `simd-scan` to the
  greenfield.)
- **cdylib home**: `wasm/Cargo.toml` (crate `bbnf-wasm`, `crate-type=["cdylib","rlib"]`,
  wasm-bindgen 0.2, serde-wasm-bindgen 0.6); entry `wasm/src/lib.rs`.
- **Four-plane export**: `crates/runtime/src/tape/mod.rs` ~121-137 (`Tape::{source,offsets,
  flag_cursors,flag_values}`); GN-4.W4 target `crates/bbnf-wasm/src/tape_export.rs`.
- **TS idiom layer (bindings only)**: PROVE-14 `crates/bbnf-bench/ts-tape/tape.mjs` (364) →
  GN-4.W4 `ts/tape/src/tape.ts` (`TapeDoc`, 4 ctor-only fields, single hidden class);
  `pointer.ts` (the 4.4× stop-early lane); `load.ts` (`decodeTape` typed-array construction).
- **parse-that wasm-core (greenfield)**: no cdylib today (`parse-that/rust/Cargo.toml`); the
  consolidated-cdylib route homes the combinator core inside `bbnf-wasm` beside `simd-scan`.
- **Live parse-that consumer of record**: `latex-paper` `src/grammar/*` (8 files, `^0.7.1`); the
  divorced non-consumers are `value.js@4.0.0` (`src/v4/css/*`, its own 9 sync entries) and
  `keyframes.js@6.0.0` (value.js-only).

---

## Named risks / counterexamples

- **R1 (Q2, aliasing corruption).** Re-fetching `.buffer` after `grow()` reads the next parse's
  bytes at the same offsets → false parity on ASCII-only leaves, corruption elsewhere (GN-4 R4
  class). Copy-out or pre-reserve is a correctness precondition, not an optimization.
- **R2 (Q1, cold-single-doc loss).** PROVE-14 law: cold V8 is near-optimal; wasm-SIMD128 loses
  on a single small doc because instantiate+copy-in dwarfs the scan. Counterexample: a 2 KB CSS
  value parsed once → scalar TS wins. The wasm claim is batch/throughput-scoped; universalizing
  it trips refutation law iv.
- **R3 (Q1, SIMD128 narrowness).** WASM SIMD128 is 128-bit only; the 64-byte-stripe kernel
  issues 4 × `v128` to compensate. The win over scalar is real (~4–8×) but never native AVX
  width — do not quote 32–64 B/cycle for the browser lane.
- **R4 (Q3, divorce reversal).** The confinement collapse rests on value.js@4.0.0 HEAD; an owner
  could re-adopt parse-that. F6/owner-bound; revalidate at gestalt.
- **R5 (Q3, consolidated-cdylib coupling).** One artifact couples release cadences and bloats the
  smallest consumer's download (latex-paper bundle triples). Mitigation: feature-gated cdylib +
  wasm tree-shaking for leaf consumers; the consolidated build serves the batch/server face.

---

## The one prototype that moves 6C

A single node+browser harness on the `bbnf-wasm` cdylib that, cold and base-pinned:
1. times `simd-scan::wasm::scan` vs a scalar `scan.ts` on a shared corpus across doc-size ×
   batch-count, both numbers, locating the crossover (Q1);
2. drives two parses through one instance, forces `grow()`, proves `TapeDoc` detachment, then
   measures copy-out (plane memcpy) vs pre-reserved-max, re-checking the ≥2× stop-early margin
   survives (Q2);
3. builds consolidated-cdylib vs two-cold-instances and reports `instantiate + first-parse` ms
   and peak memory for each (Q3).

The kernel, the cdylib, and the byte-exact tape crossing already exist; this harness converts
"the substrate is present" into "the wasm core is the measured right boundary, and its two
correctness hazards are priced."

---

### Sources
- MDN, *WebAssembly.Memory.prototype.grow()* — grow detaches the backing ArrayBuffer and all views.
- WebAssembly/spec issue #646 — JS API for grow-memory and detaching array buffers.
- awesome.red-badger (Chris Whealy), *WebAssembly Memory Growth and the Detached ArrayBuffer Problem*.
- wasm-bindgen issue #4395 — `TypedArray::view` over a detached ArrayBuffer; re-slice mitigation.
- jsonic.io, *JSON Parse Performance JavaScript: V8 Fast-Path, simdjson & Streaming* — V8 200–400 MB/s, simdjson NEON 1–2 GB/s.
- medium/@oemaxwell & dev.to (Bence R.), *Rust + WebAssembly Performance: JavaScript vs wasm-bindgen vs raw WASM (with SIMD)*.
- PROVE-14 REPORT+VERIFY (in-tree) — byte-exact tape crossing, 4.4× stop-early, full-walk ~0.9× non-goal.
