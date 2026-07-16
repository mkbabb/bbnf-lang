# LOCK-6 · Family 6C — WASM-CORE · pass-3 research (research-C)

## The family, made concrete

6C's thesis: **the Rust core compiled to wasm is the one implementation; TS is bindings plus
an idiom layer; TS-native code survives only where V8 measurably wins.** The isomorphism is
then *physical* — one parser, one tape builder, one regex engine, authored once in Rust and
shipped to the browser as a `cdylib`; the TS surface is a thin decoder over the four tape
planes plus the idiomatic value API. This family's charge is to establish, **by measurement**,
where the wasm boundary is drawn — which work crosses into wasm, which stays TS-native.

The founding datum is PROVE-14: the offset-tape SoA layout crosses to TS byte-exactly (6
structural counts + codeUnits + 32-bit fingerprint identical across 320 cold children), with
two lanes of opposite verdict — **full-walk materialization loses** (cold tape-decode+walk at
parity with native `JSON.parse`, 0.90–0.98×; VERIFY 0.90–0.92×), **stop-early get wins 4.4×**
(three leaves out of `statuses[0]`, skipping `statuses[1..99]`; VERIFY 4.25–4.56×). The 6C
reading: the wasm core's value is not re-materializing JS objects (native `JSON.parse` owns
that) — it is **building the tape once and navigating it lazily**. Pass-3's three questions
were owed a running wasm artifact; **that artifact now exists** and rewrites Q1 from prediction
to measurement, and — decisively — **relocates the whole boundary decision from "which scanner
is faster" to "where do the source bytes live."**

## Substrate census (what exists at the base pin `ce51c36fc`)

- **The four tape planes are already public slices** — the exact wasm export surface. PROVE-14
  established them at `crates/runtime/src/tape/mod.rs:126–137` (`source()/offsets()/
  flag_cursors()/flag_values()`, `payloads()` proven empty on the offset-only tape). These
  live in the greenfield/worktree tree of record, not the doc-only frozen snapshot; the
  archival moved code out of `restart/skinny/crates/`. A `#[wasm_bindgen]` export hands the
  four to JS unchanged; `tape_dump.rs:78` already proved the export *shape*.
- **A wasm-SIMD128 kernel is net-new — and now built.** `bbnf-simd` at base is aarch64-only;
  there is no committed `wasm32`/`v128` arm on the mainline. PROTO-1's `wasm-scan/src/lib.rs`
  is the first wasm target in the family: a `#![no_std]` `v128` nibble-LUT `classify16` over
  `{ } [ ] : ,` (unique-bit LUT → exact classify, no false positives), 64-byte stripes, raw C
  ABI (no wasm-bindgen, so the marshalling tax is explicit and counted). Built
  `RUSTFLAGS=-C target-feature=+simd128` → `wasm-opt -O3 --enable-simd` → **1439 B `scan.wasm`,
  97 v128 ops present**. This is the substrate Q1 was owed.
- **PROVE-14's decoder is pure scalar JS + typed arrays.** Its `decodeTape` (`ts-tape/load.mjs`
  `toU32`) does **not** hand back zero-copy views — it `slice()`s each plane into a fresh
  0-aligned `ArrayBuffer` (the copy-out that answers Q2), inside the timed region.

## Q1 — wasm-SIMD128 kernel vs TS-native scalar scan (browser, cold-paired) — **RAN_MEASURED**

PROTO-1 settled this in **Chrome 150.0.7871.115** (macOS arm64, `hardwareConcurrency=18`) —
a real browser engine, not node alone, so the "browser-deployable" qualifier is discharged.
Both lanes run interleaved-paired under identical load (loadavg 4.3→6.8), reduced by median;
ratios and crossover direction are robust, absolutes carry machine noise. **Parity held on
every doc** (9 synthetic + 3 real: wasm scan count == TS scan count — kernel correct, no HALT).

**The two-part result, which points opposite ways.**

1. **The kernel wins big — nobody disputes it.** `scanOnly` (v128 scan, marshalling excluded)
   beats the TS scalar `Uint8Array`+LUT loop everywhere: **6.9× twitter, 7.3× citm, 4.7×
   canada** on the real docs; ~1.3–1.5× on synthetic ASCII. A branchless `classify16` is
   unambiguously the faster scanner, and it is *immune* to the branch-mispredict tax that the
   scalar `if(LUT[b])` pays on branch-hostile input.

2. **Marshalling is the whole story, and it inverts the verdict.** `TextEncoder.encode` +
   copy-into-linear-memory is **72–90% of `wasmFull`** on the real docs (twitter 88%, citm
   90%, canada 72%) and *swamps the entire TS lane* at small sizes (marshal = 8.4 ns/B at 56 B
   — per-call overhead over too few bytes). Net wasm requires the marshal per-byte to fall
   below `ts − scanOnly` (~0.85–0.9 ns/B), which happens only for large, cheap-to-encode
   inputs.

**The measured table (ns/byte; WIN = `wasmFull < ts`):**

| doc | B | ts | wasmFull | marshal | scanOnly | WIN |
|---|---:|---:|---:|---:|---:|:--:|
| twitter | 631,514 | 1.129 | 1.308 | 1.146 | 0.163 | TS |
| citm | 1,727,204 | 1.084 | 1.489 | 1.340 | 0.149 | TS |
| canada | 2,251,051 | 1.074 | 0.820 | 0.590 | 0.230 | **WASM** (1.31×) |

Synthetic ASCII 64 B→64 KB: `wasmFull > ts` at **every** size but one noisy 4 KB `ts`-spike.
**No clean size crossover exists.** The net win first appears at multi-MB scale and is
**content-, not size-, dependent**: canada wins because it is single-byte-backed ASCII (fast
encode → marshal 0.59) *and* branch-hostile (long numeric runs slow the scalar loop to 1.07
while the SIMD kernel is immune); twitter/citm carry UTF-16-backed non-ASCII strings (slow
encode → marshal 1.15/1.34) and lose. The apparent crossover between citm (1.7 MB, TS) and
canada (2.25 MB, WASM) sits **~2 MB**, but it is a content artifact, not a law. Cold
single-shot agrees: cold `wasmFull` still loses on twitter/citm and only ties canada.

**Verdict (PROTO-1, gate (b)):** `wasmFull < ts` on canada is TRUE, but the crossover (~2 MB)
**exceeds any realistic bbnf batch doc** (grammar files are KB-scale; even the JSON corpus
median citm at 1.7 MB is below it). The falsifier's second clause fires. **wasm-core-home does
NOT pass for a JS-string source; TS-native scalar is the bbnf-side home** for the tape-crossing
stratum given JS-string input.

**But the falsification is conditional, and the condition is the real spine decision.** The
loss is *not the kernel* — it is the JS-string→linear-memory boundary. **If the bytes are
already resident in wasm linear memory** (produced/owned in wasm, no per-call `TextEncoder`+
copy), the `scanOnly` column governs and the v128 core wins **4.7–7.3× at every size**. So the
6C boundary is decided by **where the bytes live, not which scanner is faster**:

- **source arrives as a JS string** → TS-native scalar scan (marshalling tax un-payable below
  ~2 MB) — the batch-parse-from-`fetch`-text case.
- **bytes born/resident in wasm** → wasm-core scan (kernel is 4.7–7.3× and this is exactly the
  PROVE-14 shape: **Rust builds the tape in linear memory, TS navigates it**) — no marshal,
  because the tape never left wasm.

This is not a defeat of 6C — it is its **correct formulation**. A wasm-core that must import
each document from a JS string is falsified; a wasm-core that **owns the byte fabric** (parse
producing a resident tape, TS as a zero-marshal navigator) is exactly where the kernel's 7×
lives. The isomorphism plane is *physical* precisely because the bytes never cross back.

## Q2 — `memory.grow()` mid-batch and outstanding typed-array views

**Status: correctness-CONFIRMED by spec; copy-out already priced — and it is the mirror of
Q1's marshal tax.** `memory.grow()` (the JS `Memory.prototype.grow` or the `memory.grow`
instruction) **detaches the old `ArrayBuffer` and allocates a fresh one**; every outstanding
`TypedArray`/`DataView` over wasm linear memory is invalidated (byteLength→0, reads throw
`TypeError: … detached ArrayBuffer`) — this fires **even for `grow(0)`** (MDN;
WebAssembly/spec #646; wasm-bindgen #4395; WebKit 182327). In the two-parses-through-one-
instance path this is live: parse #1 hands JS zero-copy `Uint32Array` views into the offset
plane; parse #2 of a larger doc grows the arena; **parse #1's views silently detach** — a
crash, or a coincidental wrong read.

**The mitigation is already the measured default.** PROVE-14's `decodeTape` copies each plane
via `buf.buffer.slice(...)` into a fresh JS-owned `ArrayBuffer` before viewing — this severs
the detachment hazard (the planes now live where no wasm grow can touch) *and the copy is
inside the timed region*: the 118 KB offset-plane copy (18.98% of 631 KB) is charged against
the tape lane, and stop-early **still wins 4.4×** with ~2.2× headroom over the ≥2× gate. So the
grow-safe copy-out is **already inside the stop-early margin**.

**The Q1↔Q2 synthesis (new to this pass).** PROTO-1's marshal cost (copy *into* linear memory)
and PROVE-14's copy-out (copy *out of* linear memory) are **the same tax measured in opposite
directions** — the JS↔linear-memory boundary. This unifies the family's cost model: whichever
direction the bytes cross the wasm wall, they pay one memcpy. It also **reinforces the Q1
verdict**: keeping the tape resident in wasm avoids *both* the inbound marshal (Q1) *and* the
grow-detachment need to copy out on the hot path — the stop-early navigator can read a single,
grow-stable snapshot copied out **once**, not per get. Grow-safe patterns, in order:
**(1) copy-out** (measured default, the only one that survives a growing arena);
**(2) pre-grow to worst case** (zero-copy stays valid but caps doc size);
**(3) re-acquire views after every wasm call** (wasm-bindgen's `getUint32Memory0` pattern —
fragile, the source of #4395). 6C adopts copy-out as the contract; zero-copy across a grow is
forbidden and CI-gated.

## Q3 — the confinement datum (value.js sync-site count) + cdylib pricing

**In-tree finding: the sync-site count is ZERO — value.js has fully divorced parse-that.**
`/Users/mkbabb/Programming/value.js` is at **`package.json` version 4.0.0**, branch
**`tranche-u`**, HEAD `86a9a17`, with **no `@mkbabb/parse-that` dependency** (zero hits in
`package.json`; zero import sites in `src/`). The only two `src/` occurrences are declarations
of the *absence*: `src/subpaths/math.ts:2` ("pure numeric math (O.W2). **parse-that-FREE**") and
`src/subpaths/transform.ts:4` ("zero parsing, **zero parse-that**"). R4 §7 recorded value.js
3.1.0 importing 9 parse-that symbols across ~9 sites; **4.0.0 imports none.** The XL-G open
question ("4.0.0 divorce: intended, or a WIP tranche-u artifact?") is **empirically real
in-tree** — flagged exactly as the directive requires: an **owner-bound / F6 assumption**,
because `tranche-u` is a WIP branch and the owner has not certified 4.0.0 as canonical. The 6C
design must hold under *both* hypotheses.

**Consequence for cdylib pricing.** The "two cold instances" argument (isolate `bbnf-wasm`
from a separate `parse-that-regex-wasm` so a consumer can take one without the other) was
motivated by consumers calling *into* parse-that synchronously. With value.js divorced,
**no in-tree consumer forces the split** — building two isolated instances for value.js would
be consumer-less substrate (Constitution viii).

- **Two cold instances** = two `WebAssembly.instantiate` (each ~ms-scale cold for a few-hundred-
  KB module), **two linear memories**, and the killer: **any data crossing between them copies
  through JS** — the regex engine cannot read the generator's tape in place; every inner-scan
  call is a JS round-trip paying the exact Q1/Q2 boundary tax, per call.
- **One consolidated cdylib** = one instantiate, one linear memory, **zero inter-module copy**;
  the regex engine and tape builder share bytes in place (the resident-bytes regime where the
  Q1 kernel wins 7×). Cost: it couples versioning. But XL-C resolves the tension — parse-that
  stays one-regex-home as a **Rust source dependency** (GN-1 vendors `parse-that-regex`), so a
  single cdylib that *statically links* the vendored regex crate is **layout-compatible with
  one-regex-home**: one wasm module, regex compiled in, no second instance, no authorship
  divorce.

**Verdict for 6C: one consolidated cdylib.** Absent a live consumer demanding isolation (and
value.js 4.0.0 demands none), the consolidated module is strictly cheaper *and* keeps bytes
resident — the very condition under which Q1's wasm-core pays. It also honors one-regex-home
via vendored static linkage. Two instances revive only if the owner names a consumer that must
take the regex engine **without** the generator — a LOCK-8 consumer-contract question, not
assumed here.

## Integration seams (path:line)

- **wasm export surface**: `crates/runtime/src/tape/mod.rs:126–137` (four plane accessors) +
  `payloads()` (greenfield tree; PROVE-14's `tape_dump.rs:78` proved the export shape). A
  `bbnf-wasm` `cdylib` sub-crate `#[wasm_bindgen]`-exports these; copy-out (Q2) is JS-side.
- **net-new kernel home**: the base `bbnf-simd` is aarch64-only; PROTO-1's
  `PROTO-2/wasm-scan/src/lib.rs` (`classify16`, `scan`, 1439 B `scan.wasm`) is the first
  `wasm32`+`simd128` arm and the kernel of record for the resident-bytes lane. Colocation edict:
  a `#[cfg(target_arch="wasm32")]` sibling to the aarch64 `key_scan`/classify, not a god module.
- **decoder of record**: PROVE-14 `ts-tape/` (`tape.mjs` 364 LOC monomorphic `TapeDoc`,
  `load.mjs` `toU32`/`decodeTape` = the copy-out site, `pointer.mjs` = the 4.4× stop-early
  lane). Graduates to `ts/tape/src/*.ts` under GN-4.W4.
- **V8 discipline for the TS face** (R4 §6): `TapeDoc`'s 4 fields ctor-only (single hidden
  class, `%HaveSameMap`-stable), classify switches on the source byte (no name gate),
  `Int8Array(128)` first-byte dispatch, zero post-ctor mutation.
- **confinement datum**: `/Users/mkbabb/Programming/value.js` @ 4.0.0 (branch `tranche-u`, HEAD
  `86a9a17`), `src/subpaths/{math,transform}.ts` — the "parse-that-FREE" declarations;
  sync-site count 0.

## Prior art (citations)

- **`memory.grow` detachment**: MDN `WebAssembly.Memory.prototype.grow`; WebAssembly/spec #646
  (JS API grow + detach); wasm-bindgen #4395 (TypedArray::view over detached buffer); WebKit
  182327.
- **wasm-SIMD128 vs scalar**: the InfoQ / general wasm-SIMD literature (SIMD 1.7–4.5× over
  vanilla wasm) is the *ceiling*; PROTO-1 measures the *floor* — `scanOnly` 4.7–7.3× over
  TS-scalar, erased below ~2 MB by the marshalling boundary. Both are true; the boundary
  reconciles them.
- **structural-scan SOTA**: simdjson (Langdale & Lemire, "Parsing Gigabytes of JSON per
  Second") is the movemask-classify baseline; the `v128 classify16` nibble-LUT is its browser
  analog. sonic-rs is the paired native baseline across SPEC §0. Note `v128` has
  `i8x16.bitmask` but lacks `vshrn`-narrow — the wasm movemask/narrow idiom is *not* a 1:1 NEON
  port, which is exactly why the kernel's per-byte win (4.7–7.3×) is smaller than NEON's
  structural advantage on skip-heavy corpora and why row-09 discipline (no shim perf claim)
  governs any NEON→SIMD128 framing.
- **V8 shape monomorphism**: `%HaveSameMap` / `--allow-natives-syntax` hidden-class probe
  (R4 §6, PROVE-14 attack 8).

## Named risks / counterexamples

- **R1 — "wasm-core scans JS strings" is falsified, not merely risky.** PROTO-1 shows the
  marshalling tax un-payable below ~2 MB; a 6C that ships a wasm scanner fed JS-string
  documents is a **measured net loss** on every realistic bbnf doc. *Mitigation:* 6C's wasm
  boundary is drawn at **byte residency** — wasm owns the tape fabric (source in, tape built,
  navigate in TS); no per-doc JS-string marshal on the hot path.
- **R2 — zero-copy across a grow boundary is silent corruption.** *Counterexample:* a two-parse
  batch where doc #2 > doc #1 grows the arena; doc #1's retained `Uint32Array` reads a detached
  buffer (or garbage after realloc). *Mitigation:* copy-out is the contract, CI-gated;
  grow-stable single snapshot per batch.
- **R3 — two instances for a divorced consumer is consumer-less substrate.** *Counterexample:*
  shipping `bbnf-wasm` + `regex-wasm` isolated "for value.js" when 4.0.0 imports neither — and
  paying the inter-module JS-copy tax (R2's boundary again) on every inner regex scan.
  *Mitigation:* one consolidated cdylib (vendored regex static-link) until the owner names a
  real isolation consumer (XL-G → LOCK-8).
- **R4 — the ~2 MB crossover is content-dependent, not a size law.** *Counterexample:* a
  KB-scale but pathologically branch-hostile ASCII grammar where the SIMD kernel would win —
  masked by treating "2 MB" as the switch. *Mitigation:* the boundary is byte-residency, which
  is content-agnostic; the size crossover is a JS-string-source artifact 6C does not rely on.

## What a prototype must test to move 6C

Q1 is now settled for the JS-string regime (PROTO-1, Chrome-measured); the open probe is the
**resident-bytes regime** that the Q1 finding makes the family's spine.

1. **The wasm-resident tape-build lane (the real 6C).** Extend PROTO-1's `scan` into a full
   `bbnf-wasm` `parse(src)` that produces the four planes **in linear memory**, and drive
   stop-early gets from TS over the resident planes with **no per-get re-marshal** — one
   copy-out snapshot per batch (Q2). Paired cold Chrome + node children over
   twitter/citm/canada, base-pinned (XL-H), N≥200, 95% CI. *Falsifier:* the resident-tape
   stop-early fails to beat `JSON.parse` by ≥2×, or the one-time copy-out per batch erodes the
   margin below the PROVE-14 4.4×.
2. **Exercise the grow hazard against the resident snapshot (Q2).** Two parses through one
   instance where #2 forces `memory.grow`; assert #1's zero-copy view detaches (byteLength→0 /
   `TypeError`); confirm the single copy-out snapshot is grow-stable and stop-early holds ≥2×.
   *Falsifier:* a zero-copy path survives grow by luck (masking the bug), or copy-out drops
   stop-early below 2×.
3. **Price the consolidated cdylib and pin the confinement datum (Q3).** Cold `instantiate` ms
   + linear-memory footprint for one consolidated cdylib (regex vendored, static-linked) vs two
   separate instances, plus the per-cross-call JS-copy the two-instance path incurs on inner
   regex scans. Re-confirm the value.js sync-site count against whatever the owner ratifies as
   canonical 4.0.0 (flag until answered). *Falsifier:* two instances price cheaper than one, or
   a consumer emerges that needs the regex engine without the generator.
