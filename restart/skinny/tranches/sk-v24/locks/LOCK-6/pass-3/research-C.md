# LOCK-6 · Family 6C — WASM-CORE · pass-3 research

**Thesis (charter).** The Rust core compiled to wasm is the one implementation; TypeScript is
bindings plus an idiom layer; TS-native code exists only where V8 measurably wins. The proof
that such a boundary exists is already banked: the offset-tape crosses to TypeScript
byte-exactly (node-for-node across 320 cold children), the stop-early get clears 2× at 4.4×
against `JSON.parse`, and the full-walk lane sits at ~0.91× parity — cold native `JSON.parse`
is a hand-tuned C++ parser V8 ships, and cold-JIT JS cannot beat it on full materialization.
That last fact is the family's compass: the browser win is *not touching what you don't read*,
not out-parsing the native lane.

The one-implementation seam is concrete, not aspirational. `crates/runtime/src/tape/mod.rs`
(~121–137) exposes the four planes `Tape::{source, offsets, flag_cursors, flag_values}` as
public slices; the export shape was proven by `tape_dump.rs:78`; the TS decoder that consumes
them lives at `crates/bbnf-bench/ts-tape/{tape,load,pointer,verify,bench}.mjs` (703 LOC). The
production seam is GN-4.W4: a new `crates/bbnf-wasm` cdylib (`wasm-bindgen`, `crate-type =
["cdylib"]`, target `wasm32-unknown-unknown`) exporting those four planes, and the `ts/tape/`
package holding the `TapeDoc` monomorphic decoder (four ctor-only fields → single V8 hidden
class; classify switches on the raw source byte, no name gate — R4 §6). This report develops
the family against the three pass-3 questions.

---

## Q1 — wasm-SIMD128 kernel vs TS-native scalar scan for the browser structural scan

**The scan in question.** The browser-deployable structural scan is the classify pass that
finds `{ } [ ] " : ,` and builds the `offsets` plane (29,573 u32 for twitter, 118,292 B).
Natively this is the fused NEON scan (SPEC §0 feature 03, `vshrn` narrow, CONFIRMED
0.898–0.931× cyc/B). In the browser there are exactly two candidates, and both must be
measured cold, paired, base-pinned to the PROVE-14 HEAD `d2b32f6e3`, with `JSON.parse` as the
neutral referee.

**Option A — wasm-SIMD128 kernel.** The Rust classify core compiled to wasm32 with the
`simd128` feature: `i8x16.eq` of each 16-byte block against the structural bytes, `v128.or`
the class lanes, `i8x16.bitmask` → 16-bit mask, then scalar compaction of set bits into
offsets. This is the simdjson structural-scan shape. It is portable across Chrome/Firefox/
Safari on one code path (SIMD128 shipped everywhere by 2023).

**The named penalty — the M-series bitmask tax.** `i8x16.bitmask` has *no* single-instruction
NEON lowering. On AArch64 it lowers to a **6-instruction scalar sequence** (mask each lane's
high bit, multiply by the bit-gather magic constant `0x2040810204081`, `lsr`/`bfxil` to
stitch) or an **8–10-instruction** vector variant (ARM community blog, *Implementing the
WebAssembly bitmask operations on AArch64*). x86 SSE does this in one `pmovmskb`; the M-series
— the certified target, "M-series is NEON-only" (SPEC §0) — does not have it. This is the
*exact* movemask bit-stitch that SPEC §0 refutation **04** names as the ALU-bound classify
floor, and refutation **09** names as the sink of the `portable_simd→NEON` shim perf claim
("the shim emits 2× the movemask reduces… parity-to-slower"). A browser wasm-SIMD128 scan on
Apple Silicon (Safari/JSC, or any wasm runtime lowering through the same NEON path) re-incurs a
penalty this campaign already measured and *refuted as a lever*. A wasm-SIMD win observed only
on Chrome/node (x86 dev boxes, or V8's own lowering) is the classic two-arm window asymmetry
SPEC §0/09 warns against — and x86 is abrogated by edict, so that arm is inadmissible as the
deciding evidence.

**Option B — TS-native scalar scan.** V8 JITs a monomorphic byte loop over `Uint8Array` with
a 256-entry `Int8Array` classify LUT (the `dispatch` idiom, R4 §6d, leaf.ts:100–151). V8
compiles scalar loops well but cannot auto-vectorize; PROVE-14 measured a cold-JIT JS full
walk at 0.91–0.98× native `JSON.parse` — i.e. V8 scalar already sits *at* the native
full-parse lane without any SIMD. The scalar scan reads the JS string directly (no copy-in).

**The decisive frame — the scan choice is load-bearing only for a non-goal.** PROVE-14 settled
that full tape-build in the browser loses to native `JSON.parse` on *both* candidate kernels
(neither cold-JIT JS nor cold wasm out-parses a hand-tuned C++ parser on full materialization).
The lane that *wins* — stop-early get, 4.4× — does almost **no** structural scanning of skipped
subtrees: it skips by bracket-depth counting to the target, materializing three leaves out of
631 KB (0.032% of the document). Therefore the wasm-SIMD-vs-TS-scalar contest governs only the
full-build lane, which is already a documented browser non-goal (GN-4 §Refutation guards). On
the winning lane the kernel choice is largely moot. wasm-SIMD additionally pays two taxes the
scalar lane escapes: the M-series bitmask lowering, and **copy-in** — the source must be
encoded into wasm linear memory (`TextEncoder`/copy loop) before the kernel can touch it,
~631 KB per twitter parse, a cost the scalar lane (indexing the JS string) never pays.

**Provisional verdict for the family:** TS-native scalar wins the browser structural scan on
the merits that matter — no bitmask tax, no copy-in, already at native parity — while
wasm-SIMD128's only structural advantage (vectorized classify) buys speed on a lane that loses
to native regardless. The wasm core's value is the *tape it hands across* (built natively on
the server/build side, or by the wasm module when the caller wants the crossed tape), not a
browser-side SIMD classify. This does **not** retreat from WASM-CORE: it sharpens where the
wasm boundary earns its keep (the crossed tape + stop-early navigation) versus where TS-native
is the measured winner (the scalar scan), which is exactly the constitution's clause iii —
"TS-native only where V8 measurably wins."

**What a prototype MUST test.** Build the same twitter `offsets` plane two ways in-browser:
(A) a `bbnf-wasm` `simd128` classify kernel; (B) a TS-native `Uint8Array` + LUT scalar
classify. Cold, paired, N≥200 fresh child/page contexts, on **both** a V8 host (node/Chrome)
**and** a JSC host (Safari/`jsc`) — the M-series bitmask penalty is runtime-specific and
invisible on V8 alone. `JSON.parse` (parse the tree, no walk) as referee. Report wasm-SIMD's
copy-in cost on a separate line, ns/B directional (in-browser has no PMU). The gate is not
"which is faster" in the abstract but "does wasm-SIMD beat TS-scalar *on JSC/M-series after
copy-in* by a margin the crossed-tape lane cannot already deliver for free."

---

## Q2 — memory.grow() mid-batch and outstanding typed-array views

**Definitive answer: YES, and even `grow(0)` detaches.** `WebAssembly.Memory.prototype.grow()`
throws away the backing `ArrayBuffer` and allocates a new one; *every* view over the old buffer
(`Uint8Array`, `Uint32Array`, …) is detached — `byteLength → 0`, subsequent access reads
zero/throws. This holds for `grow(0)` too (MDN `Memory/grow`; the WebKit #182327 / Gecko
detach bugs; the red-badger *Memory Growth and the Detached ArrayBuffer Problem* write-up).
`wasm-bindgen` lives with this via the `cachedUint8Memory`/re-fetch pattern and has an open
sharp edge where `TypedArray::view` can even observe a detached buffer *during* view creation
if a grow races (wasm-bindgen issue #4395).

**The two-parses-through-one-instance hazard.** Parse doc A in a `bbnf-wasm` instance → hand JS
views over A's tape planes → parse doc B in the *same* instance. If B is larger, or the parse
arena grows, B's parse triggers `memory.grow`, and A's outstanding `Uint32Array` offsets view
is **silently detached**. This is a *correctness* landmine, not merely a thrown error: a
stop-early get against A's tape after B's parse can index a zero-length or re-based buffer and
fold plausible-but-wrong values — the ASCII-coincidence failure class GN-4 R4 already flags for
wasm-plane drift.

**The mitigation is already the proven shape — copy-out is free against the margin.** PROVE-14's
`load.mjs` does *not* hand live wasm views to `TapeDoc`; it reads the four planes into *fresh,
JS-owned* typed arrays, and the timed tape lane **includes** that 118 KB offset-plane copy —
and still won stop-early at 4.4× (VERIFY: 4.44×, 320 cold children). So the detachment-safe
architecture (TapeDoc consumes copied planes, never live views) is the one already measured and
certified. The per-doc copy-out is `offsets` (118,292 B) + `flag_cursors` (1,248 B) +
`flag_values` (312 B) ≈ **120 KB** memcpy; `source` needs no copy (JS already owns the string
it passed in). That ~120 KB sits inside the 4.4× stop-early margin by construction — it is
already the numerator of the certified tape/JSON.parse ratio. Full-walk is a non-goal, so the
copy is irrelevant there.

Two alternatives are inferior for this family: (b) re-fetch views after every wasm call
(wasm-bindgen's pattern) forbids `TapeDoc` from holding views as ctor fields across a second
parse — it fights the monomorphic single-hidden-class shape R4 §6 requires; and (c) a fresh
instance per doc is priced under Q3 and pays instantiate cost per parse. Copy-out (a) both
isolates and is already banked.

**What a prototype MUST test.** Two parses through one `bbnf-wasm` instance where doc B is
sized to force `memory.grow`. Assert doc A's stop-early get is byte-exact **after** B's parse
under (a) copied-out planes (must pass) and (b) live views held across the grow (must fail —
proving the detachment is real, not theoretical). Price the ~120 KB copy-out in ns against the
stop-early margin, and confirm the 4.4× survives the copy (it already did in PROVE-14; the
grow-forcing corpus is the new stressor).

---

## Q3 — the confinement datum + consolidated cdylib vs two cold instances

**The confinement datum re-confirmed in-tree — and it has moved.** value.js is now **4.0.0**
with **zero dependencies** (`package.json`: `dependencies {}`, `peerDependencies {}`) and **no
`@mkbabb/parse-that` import anywhere in `src`** (grep: 0 importing files, 0 synchronous
`.parse(` combinator sites). It carries its own parser under `src/v4/{color,css,easing,value}
.ts`. This is the **value.js-4.0.0 divorce** (XL-G) made concrete: the heavy synchronous
consumer R4 recorded — 3.1.0 on parse-that `^1.0.0`, "59 `all()` sites + `dispatch`" across
`src/parsing/**` — is **stale**. The historical sync-site count belongs to value.js ≤3.1.0; in
the divorced 4.0.0 tree the count against parse-that is **0**. keyframes.js, which consumed
parse-that only transitively *through* value.js (R4 §7), is severed by the same divorce. The
live external consumer set is now effectively **latex-paper alone** (pinned `^0.7.1`, pre-1.0,
unmigrated — R4 §7 SKEW).

**Why this strengthens WASM-CORE materially.** The standing objection to a wasm core was
"value.js drives parse-that through dozens of *synchronous* combinator call-sites; a wasm
boundary needs `WebAssembly.instantiate` (async) and would break them." With the divorce that
objection is **void**: no entangled sync consumer remains to migrate. A wasm-core parse-that
becomes a *new-adopter* surface, not a re-plumbing of an interlocked dependent. FLAG
(F6/owner-bound): whether value.js ever *re-adopts* a wasm-core parse-that is an owner decision,
not a technical constraint this family can resolve; the divorce means the family need not design
around value.js's former sync surface at all. This is a named supersession of R4's consumer
table (§7), not a silent contradiction.

**Consolidated cdylib vs two cold instances — the pricing.**

- **One consolidated cdylib** (bbnf tape/scan core + parse-that regex/combinator core in a
  single wasm32 module): one `WebAssembly.instantiate`, one linear memory, one module compile
  amortized across every lane. Cost side: the shared linear memory *couples* the two subsystems
  under Q2 — a regex parse that grows memory detaches an outstanding tape view; the module is
  larger to compile once (cold instantiate ≈ compile(bytes) + memory reservation).
- **Two cold instances** (`bbnf-wasm` + a separate `parse-that-wasm`): isolation (separate
  linear memories — a regex grow cannot detach a tape view), but **2× fixed instantiate
  overhead**, 2× baseline memory reservation, and any cross-subsystem data must be copied
  *across* two memories (no shared buffer). On a cold browser page load the doubled instantiate
  is the real tax.

For the SK-V24 shape the browser deliverable is the **bbnf tape core** (GN-4.W4); parse-that's
regex is consumed *by the generator on the build side*, not independently in-browser — and its
one former standalone in-browser consumer (value.js) has divorced. So the **consolidated cdylib
is favored**: single cold instantiate, and the Q2 coupling is managed by the copy-out that is
already the proven shape (planes leave wasm memory immediately; nothing outstanding to detach).
Two instances win *only* if some consumer needs parse-that combinators standalone in-browser
without the bbnf core — a surface with **no live consumer** post-divorce.

**What a prototype MUST test.** Cold `WebAssembly.instantiate` + first-parse latency for (i) one
consolidated cdylib exporting both `parse_tape` and `regex_find`, vs (ii) two separate cdylibs,
across N≥30 *fresh page contexts* (cold-load reality, not a warmed instance), with gzipped
`.wasm` byte-size as a co-metric. The consolidated module must also carry the Q2 stressor (a
regex grow must not corrupt an outstanding tape read) to prove copy-out isolation holds under
one shared memory.

---

## Prior art (citations)

- **wasm-SIMD128 / `i8x16.bitmask`**: WebAssembly SIMD spec (v128; the four `*.bitmask` ops
  return a 32-bit lane-high-bit mask); ARM community blog *Implementing the WebAssembly bitmask
  operations on AArch64* — the 6-instruction scalar / 8–10-instruction vector lowering, no
  `pmovmskb` equivalent on NEON. Ties directly to SPEC §0 refutations 04 (movemask-stitch is
  the ALU floor) and 09 (the NEON-shim double-movemask perf claim refuted).
- **memory.grow detachment**: MDN `WebAssembly.Memory.prototype.grow` (detaches even `grow(0)`);
  wasm-bindgen issue #4395 (`TypedArray::view` over a detaching buffer); red-badger *Memory
  Growth and the Detached ArrayBuffer Problem* (the redefine-over-new-buffer pattern); WebKit
  bug #182327; the Resizable/Growable ArrayBuffer proposal (the forward fix, not yet a baseline
  to rely on).
- **The On-Demand thesis** ("not touching what you don't read"): simdjson On-Demand / sonic-rs
  get; PROVE-14 REPORT+VERIFY (the 4.4× stop-early, the ~0.91× full-walk non-goal, byte-exact
  plane parity).
- **V8 monomorphism discipline**: R4 §6 (single-hidden-class shapes, `Int8Array` dispatch table,
  the megamorphic `.parser` call site) — the governing constraint on any TS-native lane this
  family keeps.

## Named risks & counterexamples

- **R-C1 (SIMD asymmetry).** A wasm-SIMD128 classify that "wins" measured only on Chrome/node
  (V8/x86) while tying or losing on Safari/M-series (the 6–10-instruction `i8x16.bitmask` tax)
  — the two-arm asymmetry SPEC §0/09 names as the classic self-deception. Counterexample:
  wasm-SIMD beats TS-scalar 1.3× on V8, 0.95× on JSC after copy-in.
- **R-C2 (non-goal lane).** The scan-kernel contest governs only full tape-build, which loses to
  native `JSON.parse` on both kernels (PROVE-14). A wasm-SIMD "win" on a lane that is still 0.9×
  native is pyrrhic. Counterexample: wasm-SIMD full-build 1.2× TS-scalar but 0.88× JSON.parse.
- **R-C3 (silent detachment).** A view held across a second parse that grows memory reads
  wrong-but-plausible offsets and passes ASCII parity by coincidence (GN-4 R4's drift class).
  Correctness bug, not a crash. Copy-out is the guard.
- **R-C4 (copy-in).** wasm-SIMD requires the source in linear memory; ~631 KB `TextEncoder`/copy
  per twitter parse that the TS-scalar lane never pays. Counterexample: wasm-SIMD scan faster but
  net loses after copy-in on small/medium docs.
- **R-C5 (shared-memory coupling).** One consolidated cdylib couples regex-grow to tape-view
  detachment; only safe because copy-out evacuates planes immediately. A future lane that holds a
  live view across a regex call reopens the hazard.

## What moves the family (the prototype battery, base-pinned `d2b32f6e3`)

1. Two-way structural-scan build (wasm-SIMD128 vs TS-scalar-LUT) of the twitter `offsets` plane,
   cold paired N≥200, **on both V8 and JSC hosts**, `JSON.parse` referee, copy-in priced
   separately. Decides Q1 without the x86-only self-deception.
2. Two-parses-through-one-instance with doc B forcing `memory.grow`; assert doc A byte-exact
   under copy-out (pass) and live-view (fail); price ~120 KB copy-out against the 4.4× margin.
   Decides Q2.
3. Cold `WebAssembly.instantiate` + first-parse for one consolidated cdylib vs two instances,
   N≥30 fresh page contexts, gzipped `.wasm` size co-metric, with the regex-grow-vs-tape-view
   isolation check on the consolidated module. Decides Q3.

The seam these ride is already built (`crates/bbnf-wasm` + `ts/tape`, GN-4.W4); the four public
plane accessors (`crates/runtime/src/tape/mod.rs:121-137`) are the ABI; `TapeDoc` consuming
copied planes is the memory.grow-safe shape PROVE-14 already certified. The family's sharpened
claim: the wasm core is the one *tape-producing* implementation and the crossed-tape stop-early
navigator; the TS-native scalar scan is the measured winner for in-browser classify; and the
value.js divorce removes the last obstacle to letting the wasm boundary be async at instantiate.
