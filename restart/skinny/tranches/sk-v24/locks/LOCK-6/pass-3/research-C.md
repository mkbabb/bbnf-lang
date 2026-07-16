# LOCK-6 · Family 6C — WASM-CORE · pass-3 research

The family thesis under test: the Rust core compiled to wasm is the *one*
implementation; TypeScript is bindings plus an idiom layer; TS-native code exists
only where V8 measurably wins. The tape-crossing seed already proves boundaries of
that shape exist — the full-walk lesson is that native `JSON.parse` (hand-written C++
in V8) beats a cold-JIT JS walk on full materialization at ~0.91×, while the tape's
stop-early lane wins 4.4–4.6× because it does not touch what it does not read. This
report makes the wasm-core mechanism concrete, prices its two live seams, and states
exactly what a prototype must measure to move the family.

## The mechanism made concrete

One Rust cdylib (`bbnf-wasm`, the greenfield names it as a NEW `crate-type =
["cdylib"]` sub-crate, colocated: `crates/bbnf-wasm/{Cargo.toml, src/lib.rs,
src/tape_export.rs}`). It compiles the generator's *runtime* organs — the offset-tape
builder and the four plane accessors — to `wasm32-unknown-unknown`. The wasm module's
one export is `parse(src: &str) -> TapeExport`, handing JS four planes as typed-array
views over wasm linear memory: `offsets: Uint32Array`, `flag_cursors: Uint32Array`,
`flag_values: Uint8Array`, `source: Uint8Array`. TS is then a **monomorphic decoder**
(`TapeDoc`, four constructor-only fields, single hidden class, allocation-free
integer-cursor walk) plus the idiom surface (`getKey`/`indexArray`/`stringLeaf`/
`nextSibling`). The kind of every node is re-derived from `source[offsets[cursor]]` —
no stored discriminator crosses the boundary; the flag plane is a sparse sorted
side-channel binary-searched for the escaped-string minority.

The wasm-core question this family owns is narrower than "port everything": it is
**which structural-scan kernel runs in the browser** — a wasm-SIMD128 classify kernel
(the shufti/truffle prefilter, GATE=16, compiled to `v128`), or a TS-native scalar
byte loop — and **where the linear-memory boundary forces copy-out**.

## Q1 — wasm-SIMD128 kernel vs TS-native scalar scan for the batch parser

**Framing.** The batch parser's inner loop is the classify step: scan `source` bytes
into structural offsets. Two browser-deployable options:

- **(A) wasm-SIMD128 classify.** Crucially, this is **not** the NEON kernel recompiled
  — the certified NEON shufti/truffle intrinsics (`skinny/crates/bbnf-simd`, aarch64
  `tbl`/`vshrn`) do *not* cross to wasm. The wasm kernel is the **portable
  `std::simd::u8x16` nibble-LUT arm that the generator ALREADY emits for wasm32**:
  `crates/core/src/generate/regex/emit/simd.rs:85-135` (`emit_structural_bitmap_kernel`)
  gates on `target_feature` and states verbatim (simd.rs:93-95) that "**otherwise
  (aarch64, wasm32, non-AVX2 x86_64) the emitted scan routes through
  `parse_that::find_next_structural_from` — one `std::simd::u8x16` nibble-LUT
  classification per 16 bytes**." `std::simd::u8x16` lowers to WASM `v128`
  (`v128.load` / `i8x16.swizzle` / `i8x16.eq` / `i8x16.bitmask`) under
  `-C target-feature=+simd128`. WASM SIMD is *exactly* 128-bit; the width matches
  M-series NEON (128-bit, no SVE for byte work per SPEC §0). So the Q1 kernel is an
  **existing, cross-arch-parity-tested portable scanner** (`simd-scan::tests::correctness`,
  cited simd.rs:118-121) built to a `+simd128` target — the AVX2/NEON arms just drop
  out at the `#[cfg]`. No new emitter; only a wasm build.
- **(B) TS-native scalar scan.** A hand-tuned monomorphic byte loop in TS over a
  `Uint8Array`, V8-JITted. No boundary crossing, no wasm instantiation, no
  string-marshalling; but scalar, ~1 byte/iteration, and cold-JIT for the first
  document of a batch.

**The decisive cost is the boundary, not the kernel width.** The published JS-vs-raw-
wasm-vs-wasm-bindgen measurement is unambiguous: on a per-call array-modify, raw wasm
won ~4× and raw-wasm-SIMD ~6× over pure JS — **but wasm-bindgen LOST to pure JS**
(1.62 ms vs 1.40 ms) because wasm-bindgen "allocates a new buffer in wasm memory and
copies the contents" every call. For the bbnf face this is the UTF-16 JS string →
UTF-8 wasm-linear-memory encode+copy that `parse(src: &str)` pays on every call. That
copy is O(source length) and runs on the main thread. So the winner is
**size-dependent with a crossover**, and the crossover moves against wasm as
per-document input shrinks — a batch of many small documents is precisely the regime
where the boundary tax dominates and TS-native scalar can win outright.

**What the seed already tells us.** The tape's edge was never the full scan — full-
walk is honest parity/loss (~0.91×) because cold native `JSON.parse` is near-optimal
for full materialization. The wasm-core value is therefore NOT "scan faster than
JSON.parse in the browser"; it is (i) *reuse of the one certified kernel* (no second
scalar TS scanner to maintain), and (ii) the stop-early / on-demand lane where the
tape skips unread structure. Any Q1 claim that the SIMD128 lane *beats* JS on full
scan must clear the boundary tax first — and the refutation law (row 09) binds:
a portable-SIMD shim that emits 2× the movemask reduces did NOT reproduce its speed
claim; paired interleaving is mandatory even for "obvious" wins.

**Prior art / SOTA anchors.** simdjson minifies at 6 GB/s and validates UTF-8 at 13
GB/s *natively* (AVX-512, 256/512-bit); the wasm SIMD128 port is width-halved and
carries the linear-memory boundary, so browser throughput is a fraction — the
published body has no single certified simdjson-wasm GB/s number, which is itself the
gap a prototype fills. V8 `JSON.parse` is 200–400 MB/s for ASCII-key objects and
degrades 2–3× on Unicode keys / heterogeneous values — that degradation is the seam
where a tape/SIMD lane can win on *real* (non-ASCII, escape-laden) corpora like the
twitter Japanese-text fields. tree-sitter ships a wasm build and is the reference for
"one Rust/C core, browser via wasm, thin JS bindings" as a shipped architecture.

**A wired Q1 harness already exists (needs cold-paired re-cast).**
`parse-that/typescript/test/benchmarks/wasm-json.bench.ts` runs the exact shared-corpus
comparison — five lanes (JSON.parse, parse-that hand TS, BBNF→TS combinator, BBNF→WASM-VM
full-tree, BBNF→WASM-VM check-only) over data.json/apache-builds/**twitter (617 KB)**/citm
(1.7 MB)/canada (2.1 MB), instantiating the existing `bbnf-lang/wasm` cdylib
(`wasm/Cargo.toml`: `crate-type=["cdylib"]`, `wasm-opt=["-O3","--enable-simd"]`,
Cargo.toml:28). Two caveats before it can serve Q1: (1) its WASM lane is a *tree-walking
bytecode interpreter* (`wasm/src/vm.rs`, `parse_with_grammar`), NOT the tape+SIMD128 scan
— the Q1 kernel lane must replace it; (2) it uses `vitest bench` warmup=50/time=2000 —
**warm**, which the standing law forbids; re-cast as cold paired per PROVE-14 `bench.mjs`.

**Q1 verdict owed by prototype (not assertable now):** both lanes, cold, paired,
base-pinned, *in a real browser engine* (not node alone), with the string-marshalling
encode+copy counted INSIDE the wasm lane, swept across per-document sizes to locate
the crossover. Expect: wasm-SIMD128 wins on large single documents where the boundary
amortizes; TS-native scalar wins on batches of small documents where it does not.

## Q2 — `memory.grow()` mid-batch and outstanding typed-array views

**Answer: YES, categorically.** Growing a `WebAssembly.Memory` — via the JS API or the
`memory.grow` instruction — **detaches the backing `ArrayBuffer`**: its `byteLength`
becomes 0 and *every* `Uint32Array`/`Uint8Array` view over it is invalidated
transitively. In the two-parses-through-one-instance path, if parse 2 grows the arena
past a page boundary, all planes handed out from parse 1 (offsets, flags, source
views) silently detach; a subsequent `TapeDoc` walk over them reads a zero-length
buffer. wasm-bindgen has a live-tracked instance of exactly this hazard
(`TypedArray::view` may observe a detached buffer). The mitigations are: (a) re-acquire
all views after any call that can grow memory; (b) the Resizable/Growable ArrayBuffer
proposal + `WebAssembly.Memory.prototype.toResizableBuffer()`, whose views grow with
memory instead of detaching — but that is a newer surface with uneven engine support
and cannot be a base-pinned assumption.

**The seed already chose the robust design, and priced it.** The tape decoder does NOT
hold zero-copy views into wasm memory across parses — its `decodeTape` **copies each
plane out** into a fresh JS-owned `ArrayBuffer` before constructing `TapeDoc`
(`toU32(buf)` = `buf.buffer.slice(...)` → new `Uint32Array`). That copy-out is exactly
the copy that a `memory.grow` would otherwise force, and it is **already counted
against the tape lane's timed region** — the offsets plane for twitter is 118,292 B
(29,573 u32 tokens); the whole tape is 119,852 B = 18.98% of the 631,515 B source.
The tape lane pays that copy and *still* wins stop-early 4.4–4.6×. So the copy-out cost
is dominated by the stop-early margin by a factor well over 4×: **copy-out is the
correct, safe default and it does not endanger the family's one MET gate.**

The counterexample to guard: a zero-copy "view over wasm memory" optimization taken to
shave the 118 KB copy would reintroduce the detach hazard and, on a growing batch,
produce silently-wrong reads — a correctness bug, not a perf regression. The copy-out
is not overhead to remove; it is the isomorphism boundary made safe.

**Q2 prototype obligation:** run two parses through one instance where parse 2's arena
grows across a `WebAssembly.PageSize` (64 KiB) boundary; assert parse-1 views detach
(byteLength→0) under the naive zero-copy path; measure copy-out ms at twitter scale and
confirm it stays < ¼ of the stop-early saving (the seed's numbers predict it does).

## Q3 — the confinement datum and the value.js-4.0.0 divorce

**Re-confirmed in-tree, and it has MOVED.** The confinement datum (how many synchronous
call sites bind a consumer to parse-that's `.parse()`) matters because wasm
instantiation is asynchronous by default (`WebAssembly.instantiate` returns a Promise;
synchronous `new WebAssembly.Module` is spec-capped at 4 KB on the main thread). Every
synchronous consumer entry into a wasm-core parse-that must therefore either move async
(top-level `await` instantiation) or pre-instantiate once and hold the instance.

At the observed HEAD, **value.js is 4.0.0 and has fully divorced parse-that**: its
package `"version": "4.0.0"`; `src/` contains **zero** `@mkbabb/parse-that` imports
(the only two mentions are prose comments explicitly asserting `parse-that-FREE` at
`src/subpaths/math.ts:2` and `src/subpaths/transform.ts:4`); the CSS grammar is now a
**hand-rolled recursive-descent parser** — `src/v4/css/grammar.ts` has 53 `function`/
arrow definitions and **0** `Parser`/combinator references, threading a bespoke
`Result`/`ok`/`err` type from `../result`. So the value.js confinement datum is now
**≈ 0 synchronous parse-that sites in the runtime path** — the XL-G "value.js-4.0.0
divorce" is not a projection; it is landed. (node_modules still resolves
`@mkbabb/parse-that`, but only transitively/dev via `keyframes.js`; the src does not
import it.) This is on branch `tranche-u`; the prior recon captured the 3.1.0 era where
value.js imported `Parser, ParserState, all, any, dispatch, regex, string, whitespace,
mergeErrorState` across nine sites with 59 `all()` uses — that surface is gone at 4.0.

**Consequence for the family.** value.js can no longer be the confinement oracle for a
wasm-core parse-that — it exercises zero sync sites. The live confinement measure must
move to the consumers that still import combinators: **latex-paper** (0.2.1, pinned
`^0.7.1`, imports `Parser/ParserState/regex/string/any` across `src/bibtex/parser.ts`
+ six `src/grammar/*.ts` files) and **keyframes.js** (6.0.0, transitive). Flag this as
an **owner/F6-bound assumption**: the confinement datum is now a moving target because
the single largest consumer left the surface; the family should treat value.js as
*evidence that the sync-boundary problem can be dissolved by divorce*, not as a site to
solve it at.

**Pricing: one consolidated cdylib vs two cold instances.** A consolidated cdylib —
one `bbnf-wasm` module instantiated once, holding one `WebAssembly.Memory`, serving all
parse calls — pays instantiation once (compile + link + first-page alloc) and amortizes
across the batch; the cost is that batch documents share one growing arena (the Q2
grow-detach hazard, handled by copy-out). Two cold instances (e.g. bbnf-tape core +
parse-that-regex as separate modules, or per-worker instances) double the compile/
instantiate cost and the linear-memory footprint, and cannot share the source buffer
without a second copy across the module boundary. The consolidated single-cdylib,
single-`Memory`, copy-out-per-plane design is the family's substrate: one async
instantiation at module load, synchronous `parse()` thereafter, views copied out so
`memory.grow` is invisible to held tapes.

## Integration seams (path:line, where extant code serves)

- **The wasm-SIMD128 scan kernel, already gated:** `crates/core/src/generate/regex/
  emit/simd.rs:85-135` — the `#[cfg]` wasm32 arm routes to
  `parse_that::find_next_structural_from` (portable u8x16). This IS the Q1 kernel; a
  `+simd128` build is the only new work.
- **Existing cdylib precedent + wired Q1 corpus:** `bbnf-lang/wasm/` (Cargo.toml
  crate-type/wasm-opt simd; `src/vm.rs` handle-store + `parse_with_grammar`) and
  `parse-that/typescript/test/benchmarks/wasm-json.bench.ts` (five lanes, five corpora)
  — the working wasm-bindgen shape and the corpus loader to reuse.
- Plane accessors, the export shape: `Tape::{source, offsets, flag_cursors,
  flag_values}` at `crates/runtime/src/tape/mod.rs:126-137` — the four public slices a
  `#[wasm_bindgen]` export hands to JS unchanged.
- The proven decoder to productionize as `ts/tape/src/tape.ts`: the monomorphic
  `TapeDoc` (classify on `source[offsets[cursor]]`, sparse-flag binary search, lazy
  walk, pointer nav) — origin `crates/bbnf-bench/ts-tape/tape.mjs` (364 LOC).
- The copy-out that makes Q2 safe: `crates/bbnf-bench/ts-tape/load.mjs` `toU32()` =
  `buf.buffer.slice(...)` → fresh `Uint32Array`; `decodeTape` is the timed region.
- The stop-early workload (the one MET gate): `crates/bbnf-bench/ts-tape/pointer.mjs`
  `tapeGet` (statuses[0] × 3 leaves, `getKey`/`indexArray`/`stringLeaf`) vs `jsonGet`.
- The wasm sub-crate target already named by the baseline: `crates/bbnf-wasm/`
  (`crate-type=["cdylib"]`, wasm-bindgen) + `ts/tape/` package — the W4 seam feeding
  the greenfield playground's wasm boundary.
- Divorce evidence: `value.js/package.json:3` (`"version": "4.0.0"`);
  `value.js/src/v4/css/grammar.ts` (hand-rolled, 0 combinators);
  `value.js/src/subpaths/{math,transform}.ts:2-4` (`parse-that-FREE`).
- Consumers still on the surface: `latex-paper/src/{bibtex/parser.ts,grammar/*.ts}`.

## Prior art with citations

- WASM SIMD is fixed 128-bit, browsers lower `v128` to SSE/NEON near-native
  (MDN `WebAssembly.Memory.grow`; simdjson portability floor; InfoQ WASM SIMD+threads).
- Growing memory detaches the `ArrayBuffer` and invalidates all views; wasm-bindgen
  tracks the `TypedArray::view`-over-detached hazard (wasm-bindgen issue #4395;
  red-badger "Memory Growth and the Detached ArrayBuffer Problem"; Resizable/Growable
  ArrayBuffer proposal + `toResizableBuffer()`).
- wasm-bindgen loses to pure JS on per-call work due to buffer alloc+copy; raw wasm
  ~4×, raw-wasm-SIMD ~6× on amortizable array work (the JS-vs-wasm-bindgen-vs-raw-SIMD
  measurement).
- V8 `JSON.parse` 200–400 MB/s ASCII, 2–3× slower on Unicode keys; simdjson native
  6 GB/s minify / 13 GB/s UTF-8 validate (jsonic.io; simdjson repo; simdjson paper).
- tree-sitter: shipped one-Rust/C-core-to-wasm + thin JS bindings reference
  architecture for browser deployment.

## Named risks & counterexamples

1. **Boundary tax inverts the SIMD win on small-doc batches** — the wasm-bindgen-loses-
   to-JS datum is the counterexample; a batch of tiny documents can make TS-native
   scalar the winner. Q1 must sweep sizes, not assert one number.
2. **Zero-copy view optimization = silent corruption** — shaving the 118 KB copy-out
   reintroduces the `memory.grow` detach hazard; correctness bug across a growing batch.
3. **Async-instantiation friction** — a synchronous consumer of a wasm-core parse-that
   must go async or pre-instantiate; value.js dissolved this by divorcing, but latex-
   paper's synchronous grammar sites have not.
4. **The confinement oracle evaporated** — value.js 4.0.0 no longer exercises the sync
   boundary at all; the datum must be re-homed to latex-paper/keyframes.js or declared
   owner-bound.
5. **Full-walk stays a non-goal** — do not let a wasm SIMD scan lane resurrect the
   full-materialization 2× claim; native `JSON.parse` is near-optimal there (~0.91×).

## What a prototype must test to move this family

1. **Q1 crossover** — build the shufti/truffle classify as a `v128` wasm kernel AND a
   monomorphic TS scalar scan; run both cold, paired, base-pinned, **in a browser
   engine**, string-marshalling encode+copy counted in the wasm lane, swept across
   per-document sizes and batch counts; report the crossover size where SIMD128 stops
   winning. (Explicitly out of scope: wasm-vs-native-NEON — that comparison is settled.)
2. **Q2 detach exercise** — two parses through one instance, parse 2 growing the arena
   across a 64 KiB page; assert naive zero-copy parse-1 views detach; measure copy-out
   ms at twitter scale and confirm it stays < ¼ of the stop-early saving.
3. **Q3 pricing** — instantiate one consolidated `bbnf-wasm` cdylib once vs two cold
   instances; measure instantiate + first-parse + linear-memory footprint for each;
   re-count latex-paper's synchronous parse-that sites as the live confinement datum;
   record value.js 4.0.0 as the divorce baseline (≈0 sync sites).
