# LOCK-6 · Family 6C — WASM-CORE · pass-3 research (research-C)

## The family, made concrete

6C's thesis: **the Rust core compiled to wasm is the one implementation; TS is bindings
plus an idiom layer; TS-native code survives only where V8 measurably wins.** The isomorphism
is then *physical* — there is one parser, one tape builder, one regex engine, authored once in
Rust and shipped to the browser as a `cdylib`. The TS surface is a thin decoder over the four
tape planes plus the idiomatic value API. This family's job is to establish, by measurement,
**where the wasm boundary is drawn** — which work crosses into wasm and which stays TS-native.

The measured boundary already exists and is the family's founding datum. PROVE-14 crossed the
offset-tape SoA layout to TS byte-exactly (all 6 structural counts + codeUnits + 32-bit
fingerprint identical across 320 cold children) and found two lanes with opposite verdicts:

- **Full-walk materialization: TS-native loses** — cold tape-decode+walk is at *parity* with
  `JSON.parse`+walk (0.90–0.98×; VERIFY 0.90–0.92×). Cold native `JSON.parse` is a
  hand-optimized C++ parser; a cold-JIT JS walk cannot beat it on a full materialization.
  This is the proof that a naive "decode everything in TS" boundary is wrong.
- **Stop-early get: the crossed tape wins 4.4×** — extracting three leaves from `statuses[0]`
  and stopping skips `statuses[1..99]`; `JSON.parse` must parse the whole 631 KB document to
  hand back any field (VERIFY 4.25–4.56×, N=40, two windows).

The 6C reading of that split: the wasm core's value is **not** re-materializing JS objects
(native `JSON.parse` owns that) — it is **building the tape once in wasm and navigating it
lazily in TS**. The tape crossing *is* the isomorphism plane. The three pass-3 questions
sharpen exactly where that crossing pays and where it is a correctness hazard.

## Substrate census (what exists at HEAD)

- **The four tape planes are already public slices**, the exact wasm export surface:
  `skinny/crates/runtime/src/tape/mod.rs:120` (`source() -> &[u8]`), `:124` (`offsets() ->
  &[u32]`), `:128` (`flag_cursors() -> &[u32]`), `:132` (`flag_values() -> &[u8]`), `:164`
  (`payloads()`). A `#[wasm_bindgen]` export hands these four to JS unchanged; PROVE-14's
  `tape_dump.rs` already proved the export *shape*.
- **No wasm target exists in either repo.** `skinny/crates/bbnf-simd/src/lib.rs:285,305,322,344`
  are all `#[cfg(target_arch = "aarch64")]` — there is **no `wasm32`/`simd128`/`v128` arm
  anywhere**, and no scalar fallback wired for a non-aarch64 build. parse-that's `rust/`
  workspace has no `wasm-bindgen`, no `cdylib`, no `crate-type`. **A wasm-SIMD128 structural
  kernel is net-new; there is nothing to shim.** This is the single largest gap in the family.
- **PROVE-14's decoder is pure scalar JS + typed arrays — no wasm and no SIMD.** The prototype
  lives in worktree `wf_386900ed-462-14/skinny/crates/bbnf-bench/ts-tape/` (`tape.mjs` 364 LOC
  monomorphic `TapeDoc`, `load.mjs`, `pointer.mjs`). Its 4.4× stop-early number is a
  *scalar-TS-over-a-Rust-built-tape* number — it says nothing yet about wasm-SIMD vs TS-scalar
  for the *scan itself*.

## Q1 — wasm-SIMD128 kernel vs TS-native scalar scan (the browser structural scan)

**Status: UNMEASURED — the prototype is owed.** No wasm build exists, so no honest paired
number can be quoted; anything else would violate Constitution vii (concrete artifacts or the
SPEC-ONLY label). What the design and the SOTA say:

**The kernel under test.** The "structural scan" is the classify pass that emits the offset
tape — structural bytes (`{ } [ ] , :`) plus quote/escape boundaries. Per SPEC §0 row 02
(NO_GO, law) it scans **structural bytes only, never digit fields**; per row 04 it uses the
movemask-narrow + compaction class, **not block scheduling**. The aarch64 kernel is the
GATE=16 shufti/truffle prefilter (row 01) and the vshrn-narrow string scan (row 03). A wasm
port targets `v128` SIMD128 intrinsics.

**The honest hypothesis, not a claim.** Two SOTA facts frame the expected result:
1. wasm-SIMD128 delivers **1.7–4.5× over vanilla scalar wasm** and **~6–10× over pure JS** on
   data-parallel kernels (InfoQ; general wasm-SIMD literature) — the *ceiling* if the scan is
   SIMD-friendly and the doc is skip-heavy.
2. But row 01 already found NEON's structural advantage is **skip-heavy-only**: dense twitter
   was **1.03–1.05×** over a scalar membership scan; the win is 4.7–7.5× only on sparse/skip
   structure. And row 04 found classify is **ALU-bound on the movemask bit-stitch** even on
   NEON. **wasm SIMD128 has no native movemask** (`i8x16.bitmask` exists but the
   narrow/compaction idioms of rows 03/04 do not map 1:1; `vshrn` has no direct `v128`
   equivalent) — the movemask-emulation tax is *worse* on wasm than on NEON.

So the defensible prediction: **wasm-SIMD128 wins on skip-heavy/sparse documents, ties or
loses to TS-scalar on dense documents like twitter.** The refutation-law discipline (row 09,
"two-arm window asymmetry is the classic self-deception; paired interleaving mandatory even
for obvious wins") binds directly — a NEON→SIMD128 port carries **zero perf claim** until
measured paired, exactly as PROVE-09's shim did.

**The measurement owed (base-pinned, XL-H).** Three arms, one binary each, paired-interleaved
cold `node` children over twitter.json (and a sparse/skip corpus): (a) wasm-SIMD128 kernel,
(b) wasm-scalar fallback, (c) TS-native scalar scan (the PROVE-14 classify loop). Report
ratio (a/c) and (b/c) with 95% CI, N≥200, loadavg per block. **Browser caveat (R4 below):**
node v26 shares V8 but the browser deployment path (streaming instantiation, `fetch`-delivered
planes, no `readFileSync`) differs on the cold path — a Chrome-side re-run is owed before any
"browser-deployable" verdict.

## Q2 — `memory.grow()` mid-batch and outstanding typed-array views

**Status: CONFIRMED by spec; copy-out cost already measured.** This is a correctness fact, not
a perf question, and the answer is unambiguous.

**`memory.grow()` detaches the entire ArrayBuffer.** Growing a `WebAssembly.Memory` — via the
JS `Memory.prototype.grow()` or the `memory.grow` instruction — detaches the old
`ArrayBuffer` and allocates a fresh one; **every outstanding `TypedArray`/`DataView` over
wasm linear memory is invalidated** (byteLength → 0, reads throw `TypeError: … detached
ArrayBuffer`). This fires **even for `grow(0)`** (MDN; WebAssembly/spec #646;
wasm-bindgen #4395; WebKit bug 182327). In the two-parses-through-one-instance path this is a
live hazard: parse #1 hands JS zero-copy `Uint32Array` views into the offset plane; parse #2
of a larger document grows the arena; **parse #1's views silently detach** — a crash, or worse,
a coincidental wrong read.

**The mitigation is already the measured default.** PROVE-14's `decodeTape`
(`ts-tape/load.mjs`) does **not** hand back zero-copy views — its `toU32()` does
`buf.buffer.slice(byteOffset, byteOffset+byteLength)` into a **fresh 0-aligned ArrayBuffer**,
then views that. This copy-out **severs the detachment hazard entirely** (the planes now live
in JS-owned memory that no wasm grow can touch) *and the copy is inside the timed region* —
the 118 KB offset-plane copy (18.98% of the 631 KB source) is charged against the tape lane,
and the stop-early lane **still wins 4.4×**. So: the copy-out that makes the two-parse path
grow-safe is **already priced into the stop-early margin**, with ~2.2× of headroom to spare.

**Quantified against the margin.** Copy-out = one `memcpy` of the offset plane (≈118 KB for
twitter, the flag planes are ~1.5 KB). Against a stop-early lane that beats `JSON.parse` by
4.4× on a 631 KB doc, the copy is comfortably subsumed — the gate threshold is ≥2×, the
measured value is 4.4×, and the copy is already inside that number. The three grow-safe
patterns, in preference order: **(1) copy-out** (measured, default, and the only one that
survives an arena that must grow); **(2) pre-grow to worst-case** and never grow mid-batch
(zero-copy stays valid, but caps document size); **(3) re-acquire views after every wasm call**
(wasm-bindgen's `getUint32Memory0` cached-view invalidation pattern — fragile, easy to miss a
site, and the source of #4395). 6C should adopt copy-out as the contract and document zero-copy
as forbidden across a grow boundary.

## Q3 — the confinement datum (value.js sync-site count) + cdylib pricing

**In-tree finding: the sync-site count is now ZERO — value.js has fully divorced parse-that.**
`/Users/mkbabb/Programming/value.js` at `package.json` **version 4.0.0** (branch `tranche-u`,
HEAD `86a9a17`) has **no `@mkbabb/parse-that` dependency** (zero hits in `package.json`, zero
in `package-lock.json`). The only two occurrences in `src/` are comments *declaring the
absence*: `src/subpaths/math.ts:2` ("`— pure numeric math (O.W2). parse-that-FREE.`") and
`src/subpaths/transform.ts:4` ("`zero parsing, zero parse-that`"). R4 §7 recorded value.js
3.1.0 importing 9 parse-that symbols across ~9 sites; **4.0.0 imports none.** The XL-G P1 open
question ("value.js 4.0.0 divorce: intended, or a WIP tranche-u branch artifact?") is
**empirically real in-tree** — I flag it exactly as the directive requires: an **owner-bound /
F6 assumption**, because `tranche-u` is a WIP branch and the owner has not confirmed 4.0.0 as
the canonical cut. Under *both* hypotheses the design must hold.

**Consequence for the cdylib pricing.** The "confinement" argument for **two cold instances**
(bbnf-wasm and a separate parse-that-regex-wasm, isolated so a consumer can take one without
the other) was motivated by consumers that call *into* parse-that synchronously. With value.js
divorced, **no in-tree consumer forces the two-instance split** — building two isolated
instances for value.js's benefit would be consumer-less substrate (Constitution viii). Pricing
the two options:

- **Two cold instances** = two `WebAssembly.instantiate` calls (each ~ms-scale cold for a
  few-hundred-KB module), **two linear memories**, and — the killer — **any data crossing
  between them must copy through JS** (no shared linear memory; the regex engine cannot read
  the generator's tape in place). For a pipeline where the generated parser calls the regex
  engine on inner scans, that is a per-call JS round-trip.
- **One consolidated cdylib** = one instantiate, one linear memory, **zero inter-module copy**;
  the regex engine and tape builder share bytes in place. Cost: it **couples versioning** — the
  generator and the regex engine ship as one artifact. But XL-C already resolves the tension:
  parse-that stays the **one-regex-home** as a *Rust source dependency* (GN-1 vendors
  `parse-that-regex`), so a single cdylib that *statically links* the vendored regex crate is
  **layout-compatible with one-regex-home** — one wasm module, regex compiled in, no second
  instance, no divorce from parse-that's authorship.

**Verdict for 6C: one consolidated cdylib.** Absent a live consumer demanding isolation (and
value.js demands none), the consolidated module is strictly cheaper (one instantiate, one
memory, zero cross-copy) and honors one-regex-home via vendored static linkage. The
two-instance option should be revived only if the owner confirms a consumer that must take the
regex engine *without* the generator — a question that belongs to LOCK-8's consumer contract,
not assumed here.

## Integration seams (path:line)

- **wasm export surface**: `skinny/crates/runtime/src/tape/mod.rs:120–164` (the four plane
  accessors + payloads). A `bbnf-wasm` `cdylib` sub-crate `#[wasm_bindgen]`-exports these as
  `Uint32Array`/`Uint8Array` views; the copy-out (Q2) happens JS-side in the decoder.
- **the net-new kernel home**: `skinny/crates/bbnf-simd/src/lib.rs:285–344` is aarch64-only;
  a `wasm32`+`simd128` arm and a scalar fallback are owed here (colocation edict:
  `key_scan`/classify get `#[cfg(target_arch="wasm32")]` siblings, not a new god module).
- **the decoder of record**: worktree `wf_386900ed-462-14/skinny/crates/bbnf-bench/ts-tape/`
  (`tape.mjs`, `load.mjs` `toU32`/`decodeTape` = the copy-out site, `pointer.mjs` = the 4.4×
  stop-early lane). Graduates to `ts/tape/src/*.ts` under GN-4.W4.
- **V8 discipline for the TS face** (R4 §6): `TapeDoc`'s 4 fields ctor-only (single hidden
  class, `%HaveSameMap`-stable), classify switches on the source byte (no name gate),
  `Int8Array(128)` dispatch table for first-byte fan-out, zero post-ctor mutation.
- **consumer datum**: `/Users/mkbabb/Programming/value.js@4.0.0` (branch `tranche-u`),
  `src/subpaths/{math,transform}.ts` — the "parse-that-FREE" declarations; sync-site count 0.

## Prior art (citations)

- **`memory.grow` detachment**: MDN `WebAssembly.Memory.prototype.grow`
  (developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/Memory/grow);
  WebAssembly/spec #646 (JS API grow + detach); wasm-bindgen #4395 (TypedArray::view over
  detached buffer); WebKit bug 182327; Red Badger "memory-grow and detached ArrayBuffers".
- **wasm-SIMD vs scalar**: InfoQ "WebAssembly SIMD + multithreading performance"
  (SIMD 1.7–4.5× over vanilla wasm; wasm 1.5–2.5× over JS baseline).
- **structural-scan SOTA**: simdjson (Langdale & Lemire, "Parsing Gigabytes of JSON per
  Second") is the movemask-classify baseline; the wasm-SIMD128 port is the browser analog.
  sonic-rs is the paired native baseline across SPEC §0.
- **V8 shape monomorphism**: `%HaveSameMap` / `--allow-natives-syntax` hidden-class probe
  (R4 §6, PROVE-14 attack 8).

## Named risks / counterexamples

- **R1 — the NEON→SIMD128 "shim" is a fresh loss risk, not a port.** wasm SIMD128 lacks
  `vshrn`-narrow (row 03's win) and emulates movemask (row 04's ALU bottleneck is *worse*).
  *Counterexample:* on dense twitter, where NEON was only 1.03–1.05×, the wasm-SIMD kernel
  **loses** to TS-scalar (emulation tax > parallelism gain), and the family over-claims a
  browser SIMD win that only exists on skip-heavy corpora. Mitigation: paired, per-corpus,
  no claim without the number (row 09).
- **R2 — zero-copy across a grow boundary is a silent-corruption bug.** *Counterexample:* a
  two-parse batch where doc #2 > doc #1 grows the arena; doc #1's retained `Uint32Array` reads
  a detached buffer (or garbage after a coincidental realloc). Mitigation: copy-out is the
  contract; zero-copy across a grow is forbidden and CI-gated.
- **R3 — designing two instances for a divorced consumer is consumer-less substrate.**
  *Counterexample:* shipping `bbnf-wasm` + `regex-wasm` isolated "for value.js" when value.js
  4.0.0 imports neither. Mitigation: one consolidated cdylib until the owner names a real
  isolation consumer (XL-G, deferred to LOCK-8).
- **R4 — node numbers are not browser numbers.** PROVE-14 measured node v26 (shared V8) reading
  planes via `readFileSync`; the browser gets them via `fetch`/streaming instantiation with no
  filesystem. *Counterexample:* a 4.4× node stop-early that erodes in Chrome because
  instantiation-streaming + first-fetch cost dominates the cold path. Mitigation: the Q1/Q2
  prototype runs a Chrome arm, not node alone.

## What a prototype must test to move 6C

1. **Build the first wasm target and settle Q1.** A `wasm32`+`simd128` structural-scan kernel
   (structural bytes only — no digit scan, row 02) + a wasm-scalar fallback + the TS-scalar
   scan. Paired cold `node`-child ratios over twitter (dense) *and* a skip-heavy corpus,
   base-pinned (name HEAD, XL-H), N≥200, 95% CI, loadavg per block — plus a **Chrome arm**.
   Falsifier: wasm-SIMD ≥ TS-scalar on the skip corpus; the honest dense result reported even
   if it is a loss.
2. **Exercise the grow hazard and confirm the copy-out margin (Q2).** Two parses through one
   instance where #2 forces `memory.grow`; assert #1's zero-copy view detaches (byteLength→0
   / `TypeError`); then measure the copy-out decode and confirm stop-early holds ≥2× (target
   4.4×). Falsifier: copy-out drops stop-early below 2×, or a zero-copy path survives grow by
   luck (masking the bug).
3. **Price instantiation and pin the confinement datum (Q3).** Cold `instantiate` ms +
   linear-memory footprint for one consolidated cdylib (regex vendored) vs two separate module
   instances, plus the per-cross-call JS-copy cost the two-instance path incurs. Re-confirm the
   value.js sync-site count against whatever the owner ratifies as canonical 4.0.0 (flag until
   answered). Falsifier: two instances price cheaper than one (would reopen the verdict), or a
   consumer emerges that needs the regex engine without the generator.
