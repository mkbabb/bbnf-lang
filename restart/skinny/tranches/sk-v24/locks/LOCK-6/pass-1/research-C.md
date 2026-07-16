# LOCK-6 · Family 6C — WASM-CORE: the Rust core is the one implementation

*The isomorphism plane resolved by collapse, not by mirroring. There is one implementation —
the Rust generator's emitted parsers and kernels, compiled to WebAssembly — and TypeScript is
its binding surface plus a thin idiom layer. Cross-language "isomorphism to a reasonable point"
becomes an ABI boundary, not a duplicated module tree: the two languages share a byte-exact
data contract (the offset-tape planes) and nothing else. TS-native code exists only on the
measured islands where V8 defeats a wasm crossing — and the campaign already holds the proof
that such islands exist.*

---

## 1. The thesis, and where "reasonable" ends

6A/6B/6D answer the isomorphism question by keeping two implementations in agreement (mirrored
trees, generated skeletons, or a shared conformance corpus). 6C answers it by refusing the
second implementation. The Rust core is authored once; wasm is the single cross-language
artifact; the isomorphism obligation shrinks to **one data-layout contract** verified by
differential digest, with **no module-tree correspondence to lint and no skeleton generator to
maintain**. Drift is impossible over the collapsed surface because there is only one source of
truth for behavior — the Rust core — and the wasm build is a mechanical projection of it.

"Reasonable point" is therefore defined by 6C as a *measured boundary*, not an asserted layer
cut: the Rust core is the implementation everywhere its wasm projection is at-parity-or-better
with a hand-written TS path; TS-native code is admitted only where V8 measurably wins, and each
such island is a documented negative result carrying its number. PROVE-14 already fixes the
first island's coordinates (§3): full-materialization JSON is a place cold-JIT JS beats a wasm
crossing, so the union's `Value` full-walk lane on the web is TS-native by measurement, while
the stop-early / get / typed lanes are the wasm core. The boundary is empirical and per-lane,
which is exactly the constitution's clause (iii) demand — "measured, not asserted."

This family is the natural completion of the extant codex rather than a divergence from it.
GN-4.W4 (`docs/tranches/sk-v24/GN-4.md:310-384`) already specifies a `bbnf-wasm` cdylib
exporting the four tape planes and a productionized `ts/tape` decoder; SPEC §1
(`.../sk-v24/SPEC.md:64-66`) names "the cross-language tape (14) seeding the TS backend" as the
fourth value lane. 6C's claim is that this seam is not one lane among four but **the load-bearing
spine of the whole rust/ts split**: everything that can be the wasm core should be, and GN-4.W4
is its first vertebra.

---

## 2. The mechanism, made concrete

### 2.1 What compiles to wasm

The Rust generator emits, per grammar, a parser that builds the offset-tape. That emitted
parser plus the runtime tape, the value-fabric accessors (stop-early `path::get`, typed
`parse_into`), and the scan kernels are compiled to a `wasm32-unknown-unknown` cdylib via
`wasm-bindgen`. This is not a shim over a hand-JS engine; it is the identical Rust source that
the native `aarch64` build consumes, retargeted. The generator itself (grammar → IR → emit)
stays host-side Rust and is *not* shipped to wasm — only its *output* (the parsers) and the
runtime crosses. That distinction is what keeps the wasm artifact bounded.

### 2.2 The ABI: four planes, zero intermediate objects

The crossing is the Structure-of-Arrays tape, byte-for-byte. The Rust side already exposes it
as four public slices — `Tape::source()`, `offsets()`, `flag_cursors()`, `flag_values()`
(`crates/runtime/src/tape/mod.rs:120-135`, the `from_offsets` constructor at `:101-119` shows
the plane set: `offsets: Vec<u32>`, `flag_cursors: Vec<u32>`, `flag_values: Vec<u8>`,
`payloads: PayloadArena`, over a borrowed `source: &[u8]`). PROVE-14 proved the payload arena
is **empty** on the offset-only JSON tape (`PROVE-14/REPORT.md:56-60`), so the live crossing is
three integer planes + the source bytes: `offsets.u32` (118 KB / 29,573 tokens on twitter.json),
`flag_cursors.u32` (1,248 B / 312 escaped-string cursors, sorted), `flag_values.u8` (312 B,
`STRING_NEEDS_DECODE = 0x01`), `source.bin` (631 KB). Total tape = **18.98% of source**.

A wasm-bindgen export hands JS `Uint32Array`/`Uint8Array` **views over wasm linear memory** for
the three planes plus a `Uint8Array` over source. No node records, no discriminator plane: kind
is re-derived JS-side from `source[offsets[cursor]]` (`ts-tape/tape.mjs:19-28`), and flags are a
sparse sorted binary-search side channel. The TS decoder is a single monomorphic hidden class
`TapeDoc` with four constructor-only typed fields, an allocation-free integer-cursor walk
returning `u32` hashes (`ts-tape/tape.mjs:11-16, 90+`; `load.mjs:decodeTape`). This is the
idiom layer — pure TS, V8-shaped — but it carries **no parsing logic**; it navigates a tape the
wasm core built.

### 2.3 The two access modes across the boundary

- **Cross-once-navigate-many (the wasm-core win).** The wasm parser runs entirely inside linear
  memory; JS receives plane views and walks integer cursors, materializing only the leaves it
  visits. The byte-hash fast path folds ASCII string bytes with **zero** JS-string allocation,
  decoding only the escaped/non-ASCII minority through the flag channel
  (`PROVE-14/REPORT.md:120-125`). This is the stop-early / get / partial-projection regime.
- **Full materialization (the V8-native island).** Producing a complete JS object tree forces a
  JS-string allocation per leaf and a boundary read per plane element; the crossing cost plus
  the string-copy tax is not amortized. Here the wasm core loses to native `JSON.parse` — and
  6C's answer is not to patch the wasm walk toward an unreachable target but to **route
  full-walk to a TS-native path** (or native `JSON.parse` itself), documented with its number.

### 2.4 The idiom layer's true extent

TS holds three things and only three: (a) the plane-view binding + `TapeDoc` navigator
(mechanical, wasm-fed); (b) the measured V8-native islands (full-walk today; more only as
measurement finds them); (c) the parse-that combinator backbone
(`parse-that/typescript/src/parse/`, 2,650 LOC — `R4-parse-that-ts.md:27-45`), which is *not*
in the wasm-core's scope at all. parse-that is a standing hand-written TS/Rust combinator
library with its own consumers (value.js, latex-paper, keyframes.js); it is the deliberate
TS-native surface the constitution's clause (iii) protects, and PT-3 productionizes it
independently (`docs/tranches/sk-v24/PT-3.md:20-24`). 6C draws its boundary *around* parse-that,
not through it: the wasm core is the *generator's* runtime; parse-that is the *combinator*
runtime; they meet only where a future TS backend composes the two, which GN-4.W4/PT-3 declare
"complementary, non-gating" (`GN-4.md:108`).

---

## 3. The measured boundary — the island exists (PROVE-14 + corroboration)

PROVE-14 is the family's keystone because it measured *both sides* of the wasm/TS-native cut on
the same corpus, cold, paired, N≥200 across 320 cold children, adversarially re-verified
(`PROVE-14/VERIFY.md:1-7`, CONFIRMED):

| lane | tape-core (wasm-fed) vs JSON.parse | verdict | where it routes under 6C |
|---|---|---|---|
| stop-early get (3 leaves of `statuses[0]`, stop) | **4.4–4.8×** faster (`VERIFY.md:21-22`) | 2× MET | **wasm core** |
| full walk (materialize every leaf) | **~0.91×** (parity/slight loss, `VERIFY.md:19-20`) | 2× UNMET | **TS-native island** |
| layout parity | byte-exact node-for-node, all 6 counts + fp | PROVEN | contract verified by digest |

The full-walk loss is *structural*, not a tuning gap: cold V8's `JSON.parse` is a hand-tuned C++
parser; a cold-JIT JS walk that re-materializes every leaf cannot beat it, and the tape's
parse-avoidance is cancelled by the JS walk cost (`PROVE-14/REPORT.md:118-125`). Independent
web-scale corroboration: for string-heavy JSON tasks, **native TS can be ~25% faster than a
Rust/wasm crossing**, and the canonical remedy is exactly 6C's cross-once-navigate-many —
"pass a typed array, process it entirely inside wasm, return results" rather than chatter across
the boundary per element (1xapi.com Node.js wasm perf guide, 2026).

So 6C does not *hope* TS-native islands exist; the campaign *measured* the first one and its
cause. This is the family's strongest evidence: the boundary is drawn by a number, satisfying
clause (iii) and precept vii (concrete artifacts, not asserted layer cuts). The null hypothesis
6C must beat is not "does an island exist" (proven) but "is the wasm core cheap enough to
maintain that collapsing the two implementations pays" (§7).

---

## 4. The substrate

**Already present — the WASM-CORE precedent is in the tree.** `playground/src/wasm/` holds a
committed wasm-bindgen artifact set: `bbnf_wasm.js` (24 KB glue), `bbnf_wasm.d.ts` (8 KB typed
surface), and `bbnf_wasm_bg.wasm` (**3.63 MB**), dated pre-greenfield. This proves the pipeline
(a `bbnf-wasm` crate → `wasm-bindgen` → a typed JS/`.d.ts` surface consumed by the Vue
playground) is real and buildable — *and* it hands 6C its first cost witness: 3.6 MB of wasm is
a non-trivial instantiation surface (§7 R2). GN-0 archives this playground outright
(`SPEC.md:96`) and GN-6 rebuilds it "on the GN-4 seam" (`SPEC.md:154`) — 6C's substrate is that
rebuild, but disciplined to export the *tape planes*, not a fat monolithic parse API.

**To build (GN-4.W4 file plan, `GN-4.md:318-336`):**

```
crates/bbnf-wasm/                 NEW cdylib, crate-type=["cdylib"], wasm-bindgen dep
  src/lib.rs                      #[wasm_bindgen] parse(src:&str) -> TapeExport
  src/tape_export.rs              TapeExport { offsets:Uint32Array, flag_cursors:Uint32Array,
                                  flag_values:Uint8Array, source:Uint8Array } — views over the
                                  four planes (crates/runtime/src/tape/mod.rs:120-135)
ts/tape/                          NEW TS package — the idiom layer
  src/tape.ts   <- ts-tape/tape.mjs (364 LOC) typed; monomorphic TapeDoc
  src/load.ts   <- load.mjs; planes -> aligned typed arrays (decodeTape)
  src/pointer.ts<- pointer.mjs; stop-early tapeGet (the 4.4× lane)
```

Toolchain: `wasm-bindgen` (typed-array views, `.d.ts` emission) or `wasm-pack`; target
`wasm32-unknown-unknown` with `-Ctarget-feature=+simd128` *if* kernels are retargeted (§5).
The dumper `crates/bbnf-bench/src/bin/tape_dump.rs:78` already proves the export *shape* (it
reads exactly the four public accessors and writes the planes PROVE-14 crossed) — the live wasm
export supersedes it, keeping the dump as a cross-check fixture (`GN-4.md:346-350`).

---

## 5. The central technical crux — SIMD does not cross

The §0 performance swath is **NEON**: prefilter shufti/truffle (01, 7.5×/4.7×), fused string
scan via `vshrn` narrow (03, 0.90× cyc/B), the movemask-narrow classify lever (04), the
direct-to-struct SWAR sink (07), the owed SIMD key scanner (06/GN-4.W2). None of this is
portable across the wasm boundary for free:

- **`portable_simd` scalarizes on `wasm32`.** Known, open: portable_simd targeting
  `wasm32-unknown-unknown` with `+simd128` is fully scalarized to byte ops in real cases
  (rust-lang/portable-simd#320), and `+simd128` feature-gating is itself fragile
  (rust-lang/rust#133290). The `core::arch::aarch64` NEON intrinsics the kernels actually use do
  **not** compile to wasm at all — they are architecture-specific.
- **Therefore the wasm core has three, not two, kernel realities:** native NEON (the §0
  numbers), a scalar fallback, and — if wasm is to keep *any* SIMD — a **third hand-authored
  `core::arch::wasm32` v128 path** (`i8x16_swizzle` for shufti, `i8x16_bitmask` for the
  movemask, `v128_load`), which is a distinct SIMD dialect from NEON. This collides head-on with
  the standing law "M-series is NEON-only, no SVE for byte work" (that law is about the *native*
  target; it does not forbid a wasm dialect, but it signals the campaign's parsimony bias
  against a second SIMD idiom).

6C's honest position: **the wasm core ships the scalar fallback first.** PROVE-14 measured the
tape crossing with a *scalar* JS walk and still won stop-early 4.4× — because the win is
"don't touch what you don't read," not SIMD throughput. The Rust scalar tape builder is
competitive (the tape is 19% of source; building it is cheap relative to full JS
materialization). The SIMD kernels are a native-only accelerant that the wasm core *forgoes*
without losing its differentiating lane. Whether a `core::arch::wasm32` retarget of shufti pays
on wasm is a **named open measurement** (§8), not an assumption — and if it does not pay, the
negative result *is* a first-class deliverable (clause vii), and the wasm core stays scalar
with no perf claim (the §0/09 shim-refutation law: no unmeasured portable-SIMD speed claims).

This crux is the family's true content: 6C is not "recompile and ship." It is a principled
*partial* collapse — the **structural/value logic** (the tape build, the walk, the get, the
typed sink) is the one wasm implementation; the **SIMD acceleration** is a native-only layer
that the wasm projection legitimately drops, and TS-native islands catch what wasm-scalar cannot
serve at parity.

---

## 6. Integration seams (path:line)

| seam | path:line | role in 6C |
|---|---|---|
| tape plane accessors | `crates/runtime/src/tape/mod.rs:120-135` | the ABI export boundary (4 public slices) |
| tape plane set | `crates/runtime/src/tape/mod.rs:101-119` (`from_offsets`) | offsets/flag_cursors/flag_values/payloads over borrowed source |
| export-shape proof | `crates/bbnf-bench/src/bin/tape_dump.rs:78` | writes exactly the 4 planes; superseded by live wasm export |
| TS idiom decoder (seed) | `crates/bbnf-bench/ts-tape/tape.mjs:1-90+` (364 LOC) | monomorphic `TapeDoc`, kind-by-source-byte, sparse-flag search |
| typed-array crossing | `crates/bbnf-bench/ts-tape/load.mjs:decodeTape` | Buffer → aligned `Uint32Array` (the timed layout decode) |
| stop-early lane | `crates/bbnf-bench/ts-tape/pointer.mjs:tapeGet` | the 4.4× wasm-core win; `jsonGet` is the TS-native contrast |
| existing wasm artifact | `playground/src/wasm/bbnf_wasm{.js,.d.ts,_bg.wasm}` | WASM-CORE precedent (3.63 MB) + instantiation-cost witness |
| GN-4.W4 substrate spec | `docs/tranches/sk-v24/GN-4.md:318-384` | `bbnf-wasm` cdylib + `ts/tape` package + gates |
| value-fabric fourth lane | `docs/tranches/sk-v24/GN-4.md:398-414` (`value/`) | wasm/ts decode is "documented, not a Rust method" |
| the protected TS-native surface | `parse-that/typescript/src/parse/` (2,650 LOC) | combinator backbone — outside wasm-core scope by design |
| playground consumer seam | `playground/src/wasm/` + `src/lib/` | GN-6 rebuild reads the wasm export |

---

## 7. Prior art (with citations)

- **tree-sitter / web-tree-sitter** — the cleanest WASM-CORE precedent in the wild: one C core,
  Emscripten-compiled to wasm, a thin hand-JS binding (`Parser`, `Tree`, `TreeCursor`); the JS
  side navigates an in-wasm tree via integer cursors, materializing nodes lazily — structurally
  identical to `TapeDoc` over the offset tape. Validates "one core, thin idiom layer, cursor
  navigation, lazy leaf materialization."
- **The regex crate / regex-automata on wasm** — compiles as-is to wasm but **loses its SIMD
  prefilters** (Teddy/memchr paths are x86/aarch64 intrinsics); the wasm build runs the scalar
  automaton. This is the exact §5 reality: the automaton logic crosses, the SIMD accelerant does
  not. Precedent that a wasm core *legitimately* ships without the native SIMD layer.
- **SWC / Biome / esbuild** — the counterexample worth heeding: high-performance JS-tooling
  cores ship **native binaries** for Node and treat wasm as the *browser-only* fallback, because
  native beats wasm on the server. 6C's scope is the *browser/TS-embed* surface (the playground,
  a future web backend); it must not claim the wasm core displaces a native path where a native
  path is available. The union already ships native `aarch64` — wasm is additive, for the web.
- **wasm-bindgen memory model** — zero-copy typed-array views over `wasm.memory.buffer` are the
  standard technique, and the standard hazard is the **detached-ArrayBuffer-on-growth** problem:
  any wasm allocation that grows linear memory invalidates every previously-handed JS view
  (Awesome Badger, "Memory Grow and ArrayBuffers"; wasm-bindgen #3298). Directly governs §8 R1.
- **JS String Builtins / WasmGC / Wasm 3.0 string-copy-tax reductions** — the emerging remedy
  for the full-walk island: letting wasm reference JS strings without byte-copy could *move* the
  6C boundary (shrink the TS-native full-walk island). A named future re-measurement, not a
  present dependency (State of WebAssembly 2026).

Sources: [portable-simd#320](https://github.com/rust-lang/portable-simd/issues/320),
[rust-lang/rust#133290](https://github.com/rust-lang/rust/issues/133290),
[core::arch::wasm32](https://doc.rust-lang.org/stable/core/arch/wasm32/index.html),
[wasm-bindgen#3298](https://github.com/wasm-bindgen/wasm-bindgen/issues/3298),
[detached ArrayBuffer on memory grow](https://awesome.red-badger.com/chriswhealy/memory-grow-and-arraybuffers),
[Node.js wasm perf 2026](https://1xapi.com/blog/nodejs-api-webassembly-wasm-performance-2026),
[State of WebAssembly 2026](https://xuro.net/blog/state-of-webassembly-2026/).

---

## 8. Named risks with counterexamples

- **R1 — detached-buffer on memory growth (correctness, not perf).** The `TapeExport` hands JS
  zero-copy `Uint32Array` views over wasm linear memory. If any later wasm call (a second parse,
  a `format!`, an arena grow) grows memory, all four views silently detach and reads return
  garbage or throw. *Counterexample:* parse doc A, hold its plane views, parse doc B → B's
  allocation grows memory → A's `offsets` view is now detached; a walk over A reads zeros and
  "parses" to a wrong-but-plausible digest. *Mitigation to prototype:* either (a) copy the four
  planes out of wasm memory at export (the PROVE-14 `decodeTape` *already copies* the offset
  plane — `load.mjs:toU32` slices into a fresh ArrayBuffer — so the measured 4.4× **already pays
  the copy** and still wins), or (b) a single-shot parse-and-return-owned-arrays ABI with no
  retained views. Option (a) is proven; the copy is not free but is counted and survives.

- **R2 — instantiation / module-size cost erases the win on small inputs.** The extant artifact
  is 3.63 MB; `WebAssembly.instantiate` has non-trivial cost, and un-cached wasm recompiles per
  invocation (~5.5 ms, dropping to ~256–400 µs with a module cache — arxiv 2512.04089). *Counter-
  example:* a caller parsing many small documents pays instantiation + crossing per call and
  loses to `JSON.parse`, which has zero startup. *Mitigation:* the wasm module is instantiated
  once at app startup and cached (standard); the 6C win is claimed only for the amortized
  many-gets-over-a-crossed-tape regime PROVE-14 measured, never for one-shot small-doc parse.
  The size must be budgeted — strip the *generator* from the wasm build (§2.1), ship only
  runtime + emitted parsers.

- **R3 — the SIMD gap makes the wasm core lose its headline numbers.** §0's differentiator is
  grammar-specialized NEON; the wasm-scalar core has none of it. *Counterexample:* someone quotes
  "JSON tape 2.2× sonic" (`SPEC.md:73`) for the wasm path — a false claim, that is a native NEON
  number. *Mitigation:* the wasm core makes **no SIMD perf claim** (§0/09 law); its value is the
  cross-language stop-early lane (measured 4.4×) and layout unification, exactly as GN-4.W4's
  refutation guard already binds (`GN-4.md:375-377`). The `core::arch::wasm32` retarget is an
  optional, separately-measured accelerant whose failure is a documented negative result.

- **R4 — the "one implementation" claim is a fiction if TS-native islands proliferate.** Every
  V8-native island is a *second* implementation of that lane, maintained by hand, that can drift
  from the wasm core. *Counterexample:* full-walk lives in TS-native; a tape-layout change in
  Rust silently desyncs the TS full-walk decoder; no linter catches it because 6C deleted the
  mirror-tree lint. *Mitigation:* the differential digest oracle (`value_parity.rs` + the TS
  four-way check, `GN-4.md:420-435`) is the *only* isomorphism guarantee 6C keeps, and it must
  cover every TS-native island — an island without a digit-for-digit differential test is
  forbidden (this is 6C's non-negotiable, the price of deleting the mirror lint). The island
  count is a *maintenance metric*: if TS-native grows past a few measured lanes, 6C is failing
  and 6D (behavioral-only) or 6A (mirror-tree) is the better family — a falsifiable boundary.

- **R5 — the boundary is corpus-specific, not universal.** PROVE-14's island is JSON-on-twitter.
  CSS, CSV, TOML, sheets may draw the wasm/TS-native line elsewhere (CSS full-walk may *win* in
  wasm if there is no native `CSS.parse` competitor — there isn't). *Counterexample:* the CSS
  grammar has no native browser parser to lose to, so its wasm full-walk lane has no TS-native
  island and 6C is *more* favorable there than JSON suggests. *Consequence:* the boundary must
  be re-measured per grammar; JSON is the *hardest* case for 6C (it competes with a native C++
  parser), so a JSON-favorable result generalizes, and JSON-parity is the conservative floor.

---

## 9. What a prototype must test to move this family

1. **The live wasm export replaces the file dump (P0).** Build `crates/bbnf-wasm` as a
   `wasm-bindgen` cdylib exporting `parse(src) -> TapeExport{offsets,flag_cursors,flag_values,
   source}` as typed-array views, feed the *unchanged* `TapeDoc` decoder, and reproduce
   PROVE-14's byte-exact digest parity (all 6 counts + fp 986912547 on twitter.json) and the
   stop-early ratio **from the wasm boundary, not from `.u32` files**. Success = the crossing is
   real and the 4.4× survives the actual wasm→JS handoff (with the plane copy of R1 counted).

2. **The detached-buffer failure is provoked and cured (P0, correctness).** Parse two documents
   in sequence holding the first's views; assert the copy-on-export path yields correct digests
   where the retained-view path corrupts. This decides the ABI: copy-out vs owned-return.

3. **Module-size and cold instantiation budget (P1).** Measure instantiate + first-parse latency
   for the stripped `bbnf-wasm` (generator excluded) and find the document-size crossover below
   which `JSON.parse` wins — the honest lower bound of the wasm-core regime (R2).

4. **The `core::arch::wasm32` SIMD retarget — pays or does not (P1, negative-result-eligible).**
   Hand-author the shufti prefilter in `i8x16_swizzle` + `i8x16_bitmask`, compile with
   `+simd128`, and measure the wasm-scalar vs wasm-v128 tape build cold. Either it beats scalar
   (a wasm SIMD accelerant, no NEON claim) or it does not (documented negative; wasm stays
   scalar). Both outcomes advance the family.

5. **A second grammar re-draws the boundary (P2).** Run the CSS tape across the same wasm export
   and confirm the wasm/TS-native line moves (CSS has no native competitor → no full-walk island),
   proving the boundary is per-grammar-measured (R5) and generalizing the JSON floor.

6. **Island differential-digest coverage (P2, the isomorphism guarantee).** For the full-walk
   TS-native island, stand up the four-way `value_parity` oracle that catches a Rust-side tape
   change desyncing the TS decoder — the one lint 6C keeps in place of the deleted mirror-tree
   linter. Without it, 6C has no drift defense and reduces to two un-checked implementations.
