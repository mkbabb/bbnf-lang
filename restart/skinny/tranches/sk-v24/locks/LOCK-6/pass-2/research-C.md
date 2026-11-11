# LOCK-6 pass-2 · family 6C — WASM-CORE

The Rust core compiled to wasm is the one implementation; TS is bindings plus an idiom
layer; TS-native code exists only where V8 measurably wins. Developed alone to its
strongest form. Steering questions Q5–Q7 (the Wasm-core block) plus the Q7-owned
cross-lock adjudication.

---

## 0. The mechanism made concrete (the strongest form of 6C)

The naive reading of "compile the Rust core to wasm, hand the result to JS" is refuted
before it starts by two measured facts: (a) PROVE-14's full-walk lane is at parity with
native `JSON.parse` (~0.9×) — materialising a full JS object graph across the boundary
cannot beat V8's C++ parser; (b) prior art puts the wasm↔JS FFI + memory-copy crossover
at **~50–100 KB** — below it, `JSON.parse` wins because call-setup + boundary copy
dominate parse. A wasm core that returns materialised values therefore LOSES on the
common case. The full-walk 0.9× is not a footnote — it is the boundary theorem in
miniature.

The strongest 6C is the boundary-avoiding formulation, and it is exactly the tape:

1. **wasm owns the structural scan.** The Rust tape builder (`parse_tape`) compiled to
   `wasm32`, using a **SIMD128 (`v128`) structural classifier**, writes the four SoA
   planes into wasm linear memory — `offsets:u32`, `flag_cursors:u32`, `flag_values:u8`,
   `source:u8` (the PROVE-14 layout, 18.98% of source, payload arena empty).
2. **The result never crosses as values.** wasm-bindgen returns the four planes as
   **zero-copy typed-array views** over `WebAssembly.Memory.buffer` (`Uint32Array` /
   `Uint8Array`) — no per-node marshalling. The only inbound copy is `source` → linear
   memory (one `set()` of the input `ArrayBuffer`), the only outbound cost is view
   construction.
3. **The walk stays TS.** PROVE-14 proved a monomorphic TS decoder walks those planes
   node-for-node byte-exactly, at **parity on full-walk** and **4.4× on stop-early** vs
   `JSON.parse`. The walk is precisely the surface where "V8 measurably wins" — a second
   wasm crossing per `get()` would re-pay boundary cost, so the lazy `nextSibling` /
   stop-early navigation is TS-native by design. This is the 6C idiom-layer clause made
   concrete, and it is derived from measurement, not asserted.

So 6C's real claim is narrow and defensible: **wasm is the one implementation of the
structural scan (SIMD, above the FFI crossover, result kept in linear memory); TS is the
bindings (typed-array views) and the idiom walk (where V8 wins and a re-crossing would
lose).** The dividing line is not "Rust vs TS" but "scan vs walk", and PROVE-14 already
measured which side each belongs on.

---

## 1. Q5 — is wasm-SIMD faster than a TS-native scalar structural scan?

**The comparator must change.** PROVE-14 compared a TS scalar tape *decoder* against
`JSON.parse` — but the tape it decoded was built by **native Rust**, not wasm, and never
crossed a wasm boundary. So PROVE-14 measures the *walk*, not the *scan*. The scan is
**unmeasured in-tree** and is the whole of family 6C's open question. A
wasm-vs-native-NEON figure is decision-irrelevant (native NEON is not browser-deployable);
the only decision-relevant contest is **wasm-SIMD128 structural scan vs a TS-native
scalar structural scan**, both browser-deployable, both producing the same offset-tape.

**Comparator, corpus, throughput figures needed (the prototype's exact job):**

- **Comparator A (the honest baseline):** a pure-TS scalar structural scanner that
  produces the identical SoA offset-tape (walk the `source` bytes, emit `u32` offsets at
  structural bytes, sparse-flag escaped strings). This is what a no-wasm TS backend would
  ship. NOT `JSON.parse` — `JSON.parse` materialises and is a different lane.
- **Comparator B (the reference ceiling):** `JSON.parse` (materialise), for context only.
- **Corpus:** `twitter.json` (631,515 B — above the ~100 KB crossover), a **sub-50 KB**
  document (below the crossover, where wasm is predicted to lose), and a **batch of N
  small docs in one wasm call** (the amortising regime).
- **Figures:** MB/s throughput per scanner; the one-time wasm `init()`/instantiate cost
  (amortised across the batch); the `source`→linear-memory copy cost per call; and — the
  crux — **whether the SIMD128 win survives cold**.

**The two named risks that make this a genuine open question, not a foregone win:**

- **R-COLD (the decisive one).** Browsers run wasm under **Liftoff** (baseline, no SIMD
  optimisation) on first calls, tiering to TurboFan only after warmup. The standing law
  forbids warm benches — so a **cold single-parse runs Liftoff wasm**, where the SIMD128
  advantage may not materialise and scalar-TS (JIT-warmed by the surrounding harness) can
  win. The honest wasm-SIMD win lives in **batch mode** (many docs, one call, tier-up
  mid-call) or is quoted with the Liftoff caveat. This tension — SIMD needs TurboFan,
  cold law forbids warmup — is the sharpest thing the prototype must resolve, and it is
  why 6C's deploy target is a **batch parser**, not a per-keystroke one.
- **R-BITMASK.** wasm SIMD128 and aarch64 NEON are both 128-bit, so the structural-scan
  kernel translates near-directly — *except* the movemask step. The `v128` movemask is
  `i8x16.bitmask`, which V8 lowers on arm64 to a NEON sequence that is plausibly the
  **double-`addv` reduction PROVE-09 already measured as a regression** on native (SPEC §0
  row 09: the shim "emits 2× the movemask reduces"). If V8's `i8x16.bitmask` lowering is
  that expensive sequence, the wasm structural scan reaches only ~scalar parity and 6C's
  scan-side advantage evaporates. **The prototype must disasm V8's wasm codegen**
  (`d8 --print-wasm-code` / tier-up dump) for the bitmask lowering — this is the single
  measurement that decides whether wasm-SIMD beats scalar-TS at all.

**Prior art bearing on the prediction:** simdjson native reaches 2–4 GB/s vs V8's
200–400 MB/s by scanning 32–64 B/cycle; the simdjson maintainers' own WASM discussion
frames a wasm build as a stage-1 `tokenize()` producing indices in linear memory —
structurally identical to 6C's plane export. The boundary literature is unanimous that the
win is real **only above ~50–100 KB and only when the result stays in linear memory** —
which is precisely the tape formulation, and precisely why the sub-50 KB corpus point is
mandatory (it is the predicted loss that keeps the claim honest).

Sources: [simdjson](https://github.com/simdjson/simdjson) ·
[simdjson WASM discussion #1912](https://github.com/simdjson/simdjson/discussions/1912) ·
[V8 wasm SIMD](https://v8.dev/features/simd) ·
[16 patterns for crossing the wasm boundary](https://dev.to/rafacalderon/16-patterns-for-crossing-the-webassembly-boundary-and-the-one-that-wants-to-kill-them-all-5kb) ·
[surma.dev — is wasm magic pixie dust](https://surma.dev/things/js-to-asc/).

---

## 2. Q6 — the synchronous-instantiation constraint (re-confirmed, and largely refuted)

**In-tree readability:** there is nothing to re-confirm in the bbnf tree — **zero
`wasm-bindgen`, `cdylib`, or `wasm32` exists anywhere** (verified: no `cdylib`/`wasm-bindgen`
in any `skinny/**/Cargo.toml`; no `wasm32`/`simd128`/`v128` under `skinny/crates/*/src`;
parse-that `rust/` likewise carries no wasm crate). So the "synchronous-instantiation
constraint that confines a wasm core below the combinator plane" is **an F6-bound
assumption carried on faith**, not an in-tree fact. It must be adjudicated from the
platform, and the platform has moved:

- The 4 KB synchronous-`new WebAssembly.Module()` size limit on the main thread was
  **deprecated and removed in Chrome 114** (shipped 2023), on the rationale that V8 lazy
  compilation makes `new WebAssembly.Module()` **< 1 s even for the biggest modules on the
  weakest devices**. Safari and Firefox **never** imposed the limit. So on current
  browsers a real parser core CAN be compiled synchronously from in-memory bytes.
- Even where one wants the async path (`instantiateStreaming`), the operative pattern is a
  **one-time async `init()`** (the wasm-bindgen default) after which **every `parse()` call
  is synchronous**. A wasm core therefore does **not** confine the combinator plane below
  an async boundary *per call* — it imposes a single async boot, once, at module load
  (`await init()` / top-level await). After boot, the combinator plane presents a fully
  synchronous API. The "confined below the combinator plane" framing is refuted as a
  per-call constraint and survives only as a **one-time boot obligation** — which the batch
  deploy target (R-COLD) already amortises.

**The residual honest constraint:** the combinator plane cannot be *synchronously
constructed at import time* if it needs the wasm instance during module evaluation; it must
gate first use behind the resolved `init()` promise. For parse-that's synchronous
combinator API this is a real seam (a `Parser` built at grammar-construction time cannot
touch wasm until boot completes) — but it is a construction-ordering seam, not a
per-parse async tax.

**Pricing one consolidated cdylib vs two.** The union is two repos (bbnf tape parser;
parse-that regex + combinator backbone). Two cdylibs = **two fetch round-trips + two
compiles + two `init()` boots + a wasm↔wasm↔JS triple boundary** whenever the combinator
plane calls the tape/regex core, and **two disjoint linear memories** (a value crossing one
must be copied to reach the other). One consolidated cdylib = **one boot, one linear memory,
zero cross-module boundary**, at the cost of a single larger blob (lazy compilation means
unused functions aren't compiled until called, so the larger blob is not a proportional cold
tax). For an in-browser deploy where combinators sit atop the tape/regex core, **one cdylib
is strictly cheaper on cold-instantiation and boundary count**; the only argument for two is
independent versioning of the two repos' browser artefacts — a packaging preference, not a
performance one. Recommendation to the gestalt: **one consolidated cdylib** for the union's
browser face, ties directly to LOCK-8C (shared runtime layout) and LOCK-9 (one kernel home).

Sources: [Chromium intent-to-ship: deprecate WebAssembly.Module size limit](https://groups.google.com/a/chromium.org/g/blink-dev/c/nJw2zwaiJ2s/m/EYPgC5D3LwAJ) ·
[web.dev — loading wasm efficiently](https://web.dev/articles/loading-wasm) ·
[WebAssembly/design #1462 async compile without event-loop turn](https://github.com/WebAssembly/design/issues/1462).

---

## 3. Q7 — the second in-tree kernel: dual-maintenance, and where it attributes

**The fact (verified).** `bbnf-simd` is structured `mod aarch64` (NEON intrinsics, every
kernel gated `#[cfg(target_arch = "aarch64")]`) + `mod scalar` (the parity reference);
there is **no `mod wasm32`**. The NEON intrinsics do **not** compile to wasm — on `wasm32`
every gated call falls through to the scalar path. Therefore a wasm core that wants the
structural-scan SIMD win must mint a **third kernel arm**: `wasm32`/`v128` via
`core::arch::wasm32::simd128` or the `portable_simd` path (which lowers to `v128` on
`wasm32` when `-C target-feature=+simd128`). This is the dual-maintenance cost Q7 asks to
attribute.

**Attribution — the adjudication proposal (owed to LOCK-9).** The wasm128 kernel arm should
**re-attribute to LOCK-9, not to this plane**, precisely when LOCK-9 unifies the kernel
home. LOCK-9A ("one shared aarch64 kernel library, parse-that scanners as thin consumers")
is the natural host: if the kernel library exposes each scanner behind a backend trait /
`portable_simd` seam with arms `{aarch64-NEON, scalar, wasm128}`, then the wasm core
**inherits** the SIMD scan for free and 6C owns only the *plumbing* (the `cdylib`, the
wasm-bindgen plane export, the TS idiom layer) — never a duplicated kernel. Conversely, if
LOCK-9 keeps repo-local hand-NEON kernels (9B, "native rebirth, no cross-repo coupling"),
the wasm128 arm is orphaned and its dual-maintenance **does** land on this plane as a third
hand-written kernel set — the worst outcome. **So 6C's kernel cost is a function of LOCK-9's
verdict, and 6C should be recorded as depending on LOCK-9A to keep its kernel surface at
zero.** The interlock name: *LOCK-6C.wasm-scan ⇐ LOCK-9A.kernel-home* (backend arm), with
LOCK-9B as the cost-bearing fallback.

**The refutation-law check (critical nuance).** SPEC §0 row 09 refuted `portable_simd→NEON`
as a **native perf claim** (parity-to-slower; emits 2× movemask reduces). That refutation
**does not bind the wasm arm**: on `wasm32` there is *no native NEON to beat* — `v128` is the
only SIMD the platform offers, so `portable_simd→v128` (or hand `core::arch::wasm32`) is
compared against **scalar wasm/TS**, not against tuned NEON. The row-09 law is a
native-target law; carrying it onto the wasm target would be a category error. What DOES
carry (see R-BITMASK, §1) is the *mechanism* concern: V8's `i8x16.bitmask` lowering may be
the same expensive movemask row 09 flagged — so the row-09 *lesson* (movemask narrowing is
the lever) applies, while the row-09 *verdict* (portable_simd loses) does not.

---

## 4. The Q7-owned cross-lock adjudications (propose, do not decree)

- **Regex exemption vs LOCK-5 ONE-HIR.** A permanent Rust-DFA ↔ TS-`RegExp` mirroring
  exemption is a **layout** exemption, orthogonal to LOCK-5's HIR-form verdict. Under 6C the
  question dissolves favourably: if regex is inside the **one consolidated cdylib**, the
  browser runs the **Rust DFA compiled to wasm** — there is no TS-`RegExp` mirror to exempt,
  because TS holds only bindings. The exemption is only needed if regex stays TS-native
  (`leaf.ts:317` uses native `RegExp`); 6C's strongest form removes that need by putting the
  DFA in wasm. Proposal: **do not freeze a Rust-DFA↔RegExp allow-list under 6C** — 6C
  eliminates the mirror rather than mirroring it; if a TS-native `RegExp` idiom layer is kept
  for small/cold patterns (below the wasm crossover, mirroring the walk-stays-TS logic), that
  is a *measured V8-wins carve-out*, not a permanent structural exemption. Owed jointly to
  LOCK-5.

- **Emitter-derived name map / grammar-derived half.** Out of 6C's subject: 6C mirrors a
  *runtime* (tape layout, plane ABI), not grammar-derived names — that half belongs to
  LOCK-3's transposition spine. 6C only asserts the **tape ABI** (the four planes' order,
  stride, endianness) is the mirrored contract; the name map is not 6C's to own.

- **Freeze-ordering DAG.** 6C is a **downstream** freeze. Its digest alphabet is the
  grammar-derived Value ADT (LOCK-1) and its reference-of-last-resort is the meta-circular
  oracle (LOCK-7); additionally 6C's kernel surface depends on LOCK-9A (§3) and its regex
  face on LOCK-5. Constraint: **this plane freezes AFTER LOCK-1, LOCK-5, LOCK-7, and LOCK-9.**
  The tape ABI (offsets `u32` LE, sparse `flag_cursors` sorted, `flag_values=STRING_NEEDS_
  DECODE=0x01`) can be frozen independently and early — it is already byte-exact-proven
  (PROVE-14) — but the *value semantics* riding it wait on LOCK-1.

---

## 5. Substrate, seams (path:line), and what the prototype must test

**Substrate that already exists (the export shape is proven):**

- `crates/runtime/src/tape/mod.rs:120-137` — the four public plane accessors
  `source()` / `offsets()` / `flag_cursors()` / `flag_values()`, verified present. This is
  the exact ABI wasm-bindgen exports; PROVE-14's `tape_dump.rs:78` already proved the export
  shape with a native dumper.
- PROVE-14 decoder `crates/bbnf-bench/ts-tape/{tape,load,pointer,verify,bench}.mjs` (703 LOC)
  — the TS idiom layer, verbatim-portable to `ts/tape/src/*.ts`; oracle is `JSON.parse`.
- GN-4.W4 (`docs/tranches/sk-v24/GN-4.md:310-385`) already scopes the productionization:
  `crates/bbnf-wasm/` new `cdylib` + `ts/tape/` package, +960 LOC, born-verified on
  stop-early + parity, born-RED on wasm-plane byte-identity vs the `tape_dump` fixture (the
  R4 endianness/stride drift guard). **This is 6C's landing seam** — but note GN-4.W4 as
  written treats wasm as a mere *plane exporter* over a tape the wasm module still builds; it
  does not yet resolve whether the **scan** runs SIMD128 or scalar inside wasm. That gap is
  exactly family 6C's contribution.

**What is NOT in the tree and must be built:** `mod wasm32` in the kernel library (or its
LOCK-9A backend arm), the `core::arch::wasm32` / `portable_simd` structural classifier, the
`cdylib` with `crate-type=["cdylib"]` + `wasm-bindgen`, and the batch-mode harness.

**The prototype that moves family 6C (the single decisive experiment):**

1. Compile `parse_tape`'s structural scan to `wasm32-unknown-unknown` with
   `-C target-feature=+simd128`, using a `v128` classifier for the structural/quote/escape
   bytes; write the four planes into linear memory; wasm-bindgen zero-copy views out.
2. Cold-per-parse **and** batch-mode, measure MB/s of: **(A) wasm-SIMD128 scan**,
   **(B) TS-native scalar scan** (same tape output), **(C) `JSON.parse`** (context). Corpus
   `twitter.json` (631 KB) + a sub-50 KB doc + an N-doc batch. Report boot cost, source-copy
   cost, and cold-vs-batch separately.
3. **Disasm V8's `i8x16.bitmask` lowering** on arm64 (`d8 --print-wasm-code`); decide
   R-BITMASK: is the movemask the row-09 double-`addv`, or a cheap sequence?
4. Confirm the crossed tape still walks at PROVE-14's parity/4.4× under the TS idiom layer
   (regression floor).

**The gate that decides the family:** wasm-SIMD128 scan beats TS-native scalar scan on
throughput **at batch mode on the ≥100 KB corpus, with the boot amortised**, AND the
sub-50 KB point honestly documented as a loss (the crossover), AND R-BITMASK disasm showing
the movemask is not the refuted sequence. If cold single-parse cannot beat scalar-TS (R-COLD)
the family's verdict is narrowed to "batch parser only, boot amortised" — still a real win,
honestly bounded. If R-BITMASK shows the expensive movemask and wasm-SIMD only reaches scalar
parity, the family collapses to "wasm buys cross-language reach + the crossed tape, zero scan
speedup" — which is what GN-4.W4 already claims, and 6C adds nothing beyond it.

---

## 6. Named risks / counterexamples (consolidated)

- **R-COLD** — Liftoff-tier cold wasm loses the SIMD win; wasm-SIMD honest only in batch /
  tiered-up mode. Counterexample: cold single-shot twitter parse runs Liftoff, slower than
  JIT-warm scalar TS. Mitigation: batch deploy target; quote SIMD figures with tier state.
- **R-BITMASK** — V8's `i8x16.bitmask` arm64 lowering may be the row-09 double-`addv`
  movemask; wasm scan reaches only scalar parity. Counterexample: disasm shows the expensive
  reduce; 6C's scan advantage is nil. Mitigation: measure the lowering before claiming a win.
- **R-CROSSOVER** — sub-50 KB documents: wasm loses to `JSON.parse` and to scalar-TS because
  boundary-copy + boot dominate. Counterexample: a UI parsing many tiny config blobs one at a
  time. Mitigation: batch into one wasm call; route small/cold inputs to the TS scalar/`RegExp`
  idiom layer (the V8-wins carve-out).
- **R-ABI-DRIFT** (GN-4.W4 R4) — zero-copy `Uint32Array` view mis-aligns on endianness/stride
  and still "parses" on ASCII-only leaves. Mitigation: born-RED byte-identity of wasm planes vs
  the `tape_dump` fixture, not merely `JSON.parse` parity.
- **R-KERNEL-ORPHAN** (§3) — if LOCK-9 chooses 9B (repo-local hand kernels), the wasm128 arm is
  a third hand-written kernel set maintained on this plane. Mitigation: bind 6C to LOCK-9A;
  record 9B as the cost-bearing fallback.
- **R-BOOT-ORDERING** (§2) — a synchronous combinator built at import time cannot touch wasm
  before `init()` resolves. Mitigation: gate first use behind the resolved boot promise; it is a
  construction-ordering seam, not a per-parse async tax.

---

*Family 6C, strongest form: wasm owns the SIMD structural scan and keeps the tape in linear
memory; TS owns the bindings and the lazy/stop-early walk (where V8 wins and re-crossing
loses); the boundary theorem and PROVE-14 draw the line at scan-vs-walk. The one decisive
prototype is wasm-SIMD128-scan vs TS-native-scalar-scan, batch mode, ≥100 KB, with the
`i8x16.bitmask` lowering disassembled. The kernel cost re-attributes to LOCK-9A or orphans
under 9B; the sync-instantiation constraint is refuted as per-call and survives only as a
one-time boot; one consolidated cdylib beats two on cold cost and boundary count.*
