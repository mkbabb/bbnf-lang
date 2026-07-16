# LOCK-6 pass-3 — PRIOR-ART: THE ISOMORPHISM PLANE (rust/ts split)

**Role.** Prior-art researcher, no family allegiance. Registry withheld by protocol.
**Problem class.** "Proper isomorphism to a reasonable point, in layout and in code" between a
Rust generator/runtime and a TypeScript backbone — what is *mirrored* (module tree, names,
oracle tables, tape layout, bench protocol), what stays *idiomatic per language*, and where
V8 laws diverge from LLVM laws. The four candidate mechanisms (PORTFOLIO LOCK-6): 6A
mirror-tree + isomorphism linter, 6B spec-generated skeleton, 6C wasm-core, 6D
oracle-isomorphism (the stated null hypothesis).

The industry has run this experiment many times. Below: what exists, who won and lost, the
constraints our own evidence base already certifies, and the named traps.

---

## 1. The delivery-vehicle question (bears on 6C) — the SWC/Biome split is the settled verdict

The single most-cited data point for "one Rust core, consumed from TS" is **not** a binary
wasm-vs-hand-port choice — it is a *two-vehicle* consensus:

- **SWC** ships the Rust core to Node via **N-API / napi-rs** (native addon), explicitly for
  "maximum performance" in build tools; wasm is the fallback for browsers only.
  [deepwiki: swc node bindings](https://deepwiki.com/swc-project/swc/7.2-node.js-bindings)
- **Biome** ships the Rust core as **WASM** (three env-specific builds + a TS `js-api`
  wrapper) because its target includes the browser/portable surface.
  [deepwiki: biome JS API and WASM](https://deepwiki.com/biomejs/biome/7.2-javascript-api-and-wasm)
- Measured spread: native N-API ≈ **45–60% faster** than the same logic as WASM; WASM ≈ 45%
  faster than JS but "not quite native," with pathological cases (Parcel) **10–20× slower** in
  WASM than native.
  [yieldcode: native rust vs wasm](https://yieldcode.blog/post/native-rust-wasm/),
  [jetbrains: rust vs js/ts + wasm](https://blog.jetbrains.com/rust/2026/01/27/rust-vs-javascript-typescript/)

**Prior-art verdict for 6C.** The winning shape is **one core, two bindings (napi for Node,
wasm for browser)** — *not* "wasm is the one implementation." A literal single-wasm-core loses
~45% on the Node/build-tool surface that is bbnf's actual consumer (the value.js / latex-paper /
keyframes.js suites all run under Node/vitest — R4 §7). WASM-CORE-as-stated is a browser story;
elevating it to "the one implementation" contradicts the measured native-binding win.

## 2. The boundary tax (against a naive 6C, *for* PROVE-14's framing)

The wasm↔JS boundary copies data. `wasm-bindgen` string handling copies UTF-16 across
linear-memory via `TextDecoder`/`TextEncoder` — an unavoidable copy; the standing guidance is
"minimize copying," use integer/fixed-size types, or hold a **`js_sys::JsString` handle** and
never copy the bytes at all.
[wasm-bindgen string guide](https://rustwasm.github.io/docs/wasm-bindgen/reference/types/string.html),
[wasm-bindgen #2741 (minimize copying heap↔linear)](https://github.com/rustwasm/wasm-bindgen/issues/2741),
[HN: breaking the WASM/JS comm barrier](https://news.ycombinator.com/item?id=44656516)

This is **exactly** PROVE-14's measured result, independently arrived at: full-walk
materialization across the boundary sits at *parity* (0.9–0.98×) because every leaf must cross,
while stop-early "don't touch what you don't read" wins **4.4×** by crossing only offsets/handles.
The wasm-bindgen `JsString`-handle discipline and PROVE-14's offset-tape are the same insight.
**Constraint imposed:** the cross-language plane must be scoped to *offset/handle* traffic
(the tape planes `offsets`/`flag_cursors`/`flag_values`/`source` as typed-array views), never
full-value marshaling. GN-4.W4 already encodes this (full-walk = documented non-goal).

## 3. The NEON-vs-wasm-SIMD wall (kills 6C for the hot path — reinforced by our own §0 law)

bbnf's §0 differentiator is grammar-specialized **NEON** kernels: shufti/truffle prefilter
(01), fused string scan **vshrn narrow** (03), CSS shufti (05), the movemask-narrow lever (04's
finding). Compiling the Rust core to wasm **strips exactly these**:

- WASM SIMD cleanly supports only 128-bit "q" NEON ops; non-`q` variants **scalarize**; many
  NEON instructions are unavailable and emulated via slow multi-instruction or scalar paths —
  "the WebAssembly SIMD specification does not expose access to all native x86/ARM SIMD
  instructions."
  [Emscripten: SIMD porting](https://emscripten.org/docs/porting/simd.html),
  [V8: wasm SIMD](https://v8.dev/features/simd)

Our own §0 already law-binds this from the other side: **"M-series is NEON-only … warm benches
are disinformation," "x86 dead."** So the perf story cannot cross to wasm: napi keeps NEON (Node
wins), wasm accepts the scalarized tax (browser loses the kernels). **6C cannot be "the one
implementation" carrying the differentiator** — the differentiator is ISA-specific and dies at
the wasm boundary. 6C survives only as the *structural/browser* delivery of the non-SIMD layer.

## 4. 6B has the strongest, most-trodden prior art — FlatBuffers / Cap'n Proto / Kaitai

The "one machine-readable spec generates typed accessors in both languages over **identical
wire bytes**" pattern is a solved, industrial problem:

- **FlatBuffers**: `flatc` compiles one schema to C++/Rust/**TypeScript**/… ; **zero-copy**
  access (no deserialize step); all elements little-endian on every platform so bytes are
  identical cross-language; "readable across languages and schema versions."
  [FlatBuffers docs](https://flatbuffers.dev/), [flatc compiler](https://flatbuffers.dev/flatc/),
  [FlatBuffers Rust](https://flatbuffers.dev/languages/rust/)
- **Cap'n Proto**: same zero-copy promise, more complex encoding.
  [FlatBuffers (Wikipedia, incl. Cap'n Proto compare)](https://en.wikipedia.org/wiki/FlatBuffers)
- **Kaitai Struct**: a declarative `.ksy` (YAML) spec compiles to binary parsers in **Rust AND
  JavaScript** (and 10+ more) from *one* description; "describe the structure, not how you read
  it"; 200+ format library.
  [kaitai.io](https://kaitai.io/), [kaitai_struct repo](https://github.com/kaitai-io/kaitai_struct)

**Verdict for 6B.** This IS the proven route for the *layout/decoder* plane, and it is precisely
what PROVE-14 already demonstrated by hand (byte-exact SoA tape decoded node-for-node in TS). A
bbnf-grammar-driven tape-decoder skeleton generated into both Rust and TS is FlatBuffers/Kaitai
applied to the offset-tape — well-founded, drift-proof-by-construction where generated.
**Trap (named):** generated code is notoriously non-idiomatic (protobuf's reputation); the
80%-generated / 20%-hand boundary must be a clean partial/extension seam, and the schema
language's expressiveness ceiling bounds what can be generated vs hand-filled.

## 5. 6C done *right* is tree-sitter — grammar→core→many bindings, not parallel hand-parsers

The closest structural analog to bbnf's ambition (a grammar that yields a core consumed from
many languages): **tree-sitter** keeps ONE C core + a parser-generator (grammar→C), then
*binds* out — `binding_rust`, `binding_web` (emscripten→wasm, per-language `.wasm`), napi. It
does **not** maintain hand-written parallel parsers.
[tree-sitter using parsers](https://tree-sitter.github.io/tree-sitter/using-parsers/),
[web-tree-sitter](https://www.npmjs.com/package/web-tree-sitter),
[tree-sitter deepwiki](https://deepwiki.com/tree-sitter/tree-sitter)

**Lesson.** The winning multi-language parser systems mirror by GENERATION or BINDING from a
single source, never by hand-maintained twin implementations. **Trap:** the web path drags an
emscripten/docker toolchain and ships heavier per-grammar `.wasm` — the browser story is not free.

## 6. 6D is the industry default for *behavioral* isomorphism — and is insufficient alone

Cross-implementation conformance via a shared corpus + differential tests is the standard:

- **test262**: 50,000+ files, the ECMAScript conformance suite every JS engine differential-tests
  against. [tc39/test262](https://github.com/tc39/test262)
- **CommonMark**: 500+ embedded spec examples as `{markdown → expected HTML}` JSON — a portable
  conformance corpus with reference impls in C *and* JS.
  [commonmark-spec](https://github.com/commonmark/commonmark-spec)
- Differential testing (COMFORT, etc.): "implementations should produce identical results";
  divergence = bug.
  [arxiv 2104.07460 (JS-engine conformance fuzzing)](https://arxiv.org/abs/2104.07460)

**Verdict on the null hypothesis.** 6D *wins* on behavior — it is nearly free (bbnf already has
per-corpus parity oracles; GN-4.W5 is a four-lane parity oracle) and maximally robust to
idiomatic divergence. BUT a behavioral corpus proves *values*, not *layout* or *performance*:
it would **not** have caught PROVE-14's full-walk regression (both lanes yield byte-identical
values; they differ only in speed). So the maintenance/drift cost that "defeats" *pure* 6D is
exactly the layout/perf divergence a value-only corpus is blind to. **The evidence forces
6D-for-behavior PLUS 6B-for-layout, not 6D alone.**

## 7. Why 6A (code-level mirror) is refuted at the hot path by our own recon

R4 and PROVE-14 already draw the line the constitution demands ("measured, not asserted"):

- **TS has NO bespoke regex engine** — it delegates to native `RegExp` (sticky-flag `test()`),
  while the Rust side owns a bespoke HIR→NFA→DFA engine (R4 §3). Code-mirror of the regex plane
  is *impossible*; the isomorphism there can only be behavioral (6D) or a functor (LOCK-5's
  concern).
- **V8 laws ≠ LLVM laws** (R4 §6): arity-unrolled combinators, the `Int8Array(128)` dispatch
  table, the megamorphic `.parser` IC, hidden-class monomorphism, `Object.create` deopt risk —
  none has an LLVM analog. A mirrored hot-path *shape* would fight both compilers.
- **PROVE-14 full-walk parity** is the measured proof: cold-JIT JS cannot mirror NEON throughput
  on full materialization; the two languages need *different* hot-path strategies.

So 6A/mirror can bind only the **structural skeleton** (module tree, public API, names, oracle
tables) — never the kernels. And the **isomorphism-linter** (CI verifying tree-correspondence +
name-map) is largely *novel*: the industry analog is hand-maintained ports (Rust `simd-json` is a
hand port of C++ `simdjson`, kept in parity manually), which are exactly the drift-prone
maintenance burden 6B/6D exist to eliminate. A linter mitigates but does not remove that burden.

---

## 8. Synthesis — where "reasonable" ends (what the evidence base certifies)

The measured line, drawn by our own §0 + R4 + PROVE-14 and corroborated by every comparable
system above:

| Plane | Isomorphic? | Mechanism the prior art supports |
|---|---|---|
| Tape **layout** (offsets/flags/source) | YES — byte-exact | 6B spec-generated decoder (FlatBuffers/Kaitai/PROVE-14 proven) |
| **Behavior** (values, conformance) | YES | 6D shared corpus + differential oracle (test262/CommonMark; GN-4.W5) |
| Public **API surface / types / oracle tables** | YES (skeleton) | 6B generation or 6A mirror of the *structural* skeleton only |
| **Hot-path kernels** (NEON vs V8-monomorphic JS) | NO — measured to diverge | per-language, idiomatic; full-walk parity + wasm-SIMD wall forbid a shared strategy |
| **Regex engine** | NO (functor only) | Rust bespoke HIR vs TS native `RegExp` (R4 §3) — LOCK-5's functor, not code-mirror |
| **Delivery** | split | napi for Node (keeps NEON), wasm for browser (accepts scalar tax) — the SWC/Biome consensus |

**The forced conclusion.** No single family is the whole answer. The evidence composes them:
**6B(layout) + 6D(behavior)** as the isomorphism *spine*; a **native-core / two-binding** delivery
(not a literal single-wasm-core, which loses ~45% on the Node consumer surface and strips the
NEON differentiator); and an **explicit non-mirror** of the hot path and regex engine (R4's
V8-vs-LLVM laws + PROVE-14's full-walk are the named counterexamples that make code-mirror a
measured loss, not a stylistic choice). 6C survives narrowed to *browser delivery of the
non-SIMD structural layer*; 6A survives narrowed to *skeleton correspondence with a linter*,
its kernel-mirror ambition refuted.

**Traps carried forward (named).** (1) single-wasm-core costs ~45% on Node — SWC's measured
reason for napi; (2) wasm-bindgen full-value marshaling eats the win — cross only handles/offsets
(PROVE-14); (3) wasm SIMD scalarizes non-`q` NEON — the differentiator dies at that boundary;
(4) generated code is non-idiomatic (protobuf) — the generated/hand seam must be a clean
partial/extension; (5) a behavioral corpus is blind to layout/perf drift — 6D alone
under-constrains a performance product (PROVE-14 is the counterexample); (6) hand-maintained
twin ports (simd-json) are the drift burden 6B/6D exist to kill — an isomorphism linter mitigates
but never removes it; (7) two-arm window asymmetry (§0/09) — any cross-language bench is paired
or it is disinformation.
