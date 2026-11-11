# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split) — PRIOR-ART SWEEP (pass 1)

Role: prior-art researcher, no family allegiance. Question: for the problem class "maintain
a Rust core and a TypeScript face in true isomorphism *to a reasonable point*, in layout and
in code," what exists in the wild, what won and lost, which constraints our own evidence base
already fixes, and which traps comparable systems named. The four candidate families the
Constitution frames (PORTFOLIO LOCK-6): **6A MIRROR-TREE** (hand-mirrored trees + isomorphism
linter), **6B SPEC-GENERATED-SKELETON** (one machine-readable spec generates both), **6C
WASM-CORE** (Rust→wasm is the one impl; TS = bindings + idiom layer; TS-native only where V8
measurably wins), **6D ORACLE-ISOMORPHISM** (behavioral only; shared conformance corpus +
differential tests; layout free per language — the stated null hypothesis).

Citation style: `path:line` for in-repo; URL for web. Web accessed 2026-07-16.

---

## §0 What our OWN evidence base already fixes (binding, before any web claim)

These are not open questions for LOCK-6 to re-decide — they are measured facts the design
must consume:

1. **The wasm/JS boundary is the cost center, not the Rust core — measured twice, ours and
   the field's.** PROVE-14 (`prototypes/PROVE-14/REPORT.md:16-27`, `VERIFY.md` CONFIRMED)
   measured the TS tape full-walk at **parity (~0.9-0.98×) with native `JSON.parse`**, *not*
   the hoped 2×, precisely because a full materialization crosses every leaf. The verify fleet
   reproduced this and named the abrogation LAW (SPEC §0 row 14; GN-4.W4 makes it a documented
   non-goal, `GN-4.md:44-48,370-373`). The field agrees exactly: a real JSON-parser benchmark
   found **TypeScript 25% faster than the equivalent Rust-WASM impl** because "every string
   crossing requires encoding, copying, and allocation on both sides" and thousands of
   crossings compound into a wall
   ([kunalganglani.com](https://www.kunalganglani.com/blog/rust-wasm-vs-typescript-performance)).
   **Consequence:** any 6C "wasm is the one impl" claim that routes per-leaf materialization
   across the boundary is refuted before it is written. Our own PROVE-14 is the counterexample.

2. **The win that DOES cross is stop-early / don't-touch-what-you-don't-read.** PROVE-14 got
   **4.4× vs `JSON.parse`** on stop-early get, because the tape skips `statuses[1..99]` and
   materializes 3 leaves (`REPORT.md:20-27`, `VERIFY.md:17-28`). This is the On-Demand thesis;
   it is a *layout-crossing* win, not a *compute-crossing* win. LOCK-6's reasonable-point line
   must be drawn here: cross the **layout** (offset planes as typed-array views), amortize the
   one-time decode, never cross per-value.

3. **The tape is already an Arrow/Cap'n-Proto-class zero-copy layout.** The four planes
   (`offsets:u32`, `flag_cursors:u32`, `flag_values:u8`, `source`) decode **byte-exactly
   node-for-node** in TS via typed arrays (`REPORT.md:53-99`, 320 cold children,
   fingerprint-identical). SoA, no stored discriminator (kind re-derived from
   `source[offsets[cursor]]`), sparse-flag side channel. This is the physical isomorphism
   substrate; the seam is `crates/runtime/src/tape/mod.rs:121-137` (four public accessors) +
   one wasm-bindgen export (`REPORT.md:128-134`).

4. **The TS side runs V8 laws, not LLVM laws.** R4 (`research/R4-parse-that-ts.md:154-208`)
   enumerates the V8-sensitive surfaces: hidden-class stability (`TapeDoc` 4 ctor-only fields =
   single map, `PROVE-14 VERIFY §8`), megamorphic `.parser` IC, `Object.create` prototype-view
   deopt risk, monomorphic arity-unrolling as a do-not-regress. **Consequence:** true
   *code* isomorphism (same control flow line-for-line) is in tension with V8 idiom; the
   Constitution's clause (iii) "reasonable point, measured not asserted" is the whole game.

5. **The type plane costs zero at runtime, both sides (Constitution v).** TS inference (PT-E2,
   `PT-3.md:305-357`) mirrors the Rust zero-runtime-cost law: emitted JS must be byte-identical
   before/after tightening. Any 6B/6C scheme that adds a runtime cast or marshaling shim to
   satisfy types is a defect.

6. **parse-that is READ-ONLY to bbnf; the letter is the only artifact across the seam**
   (`PT-3.md:101-106`, SPEC §2b). So 6A/6B "shared repo, shared generator" schemes that assume
   one writable tree are constrained: the two repos are *coordinated by letter*, not merged.
   Any isomorphism mechanism must survive a repo boundary that no bbnf agent may cross.

---

## §1 6C WASM-CORE — "the Rust core is the one implementation"

**The winning shape in the field is exactly the hybrid our PROVE-14 lands, not the maximal
"one impl" claim.** The canonical modern examples ship a Rust/C core to wasm AND keep a native
path AND accept TS-native code at the boundary:

- **tree-sitter** is the closest structural analog: a grammar (authored in JS) generates **C**,
  compiled to a native library; language bindings (Rust, Python, …) wrap the C API; and a
  **wasm module** is *also* generated — "even the native Tree-sitter bindings can consume the
  .wasm" because ".wasm files are self-contained and easy to use"
  ([tree-sitter.github.io/using-parsers](https://tree-sitter.github.io/tree-sitter/using-parsers/)).
  The lesson: wasm-as-portable-core is a *distribution* convenience, layered *under* idiomatic
  per-language bindings — not a replacement for them. This is 6C tempered by 6A, and it is the
  dominant production pattern (swc, oxc, biome all ship native-first with wasm as a secondary
  target for the same core).

- **The boundary-pattern taxonomy is now codified.** The "16 Patterns for Crossing the
  WebAssembly Boundary"
  ([dev.to/rafacalderon](https://dev.to/rafacalderon/16-patterns-for-crossing-the-webassembly-boundary-and-the-one-that-wants-to-kill-them-all-5kb))
  maps directly onto PROVE-14's design and names the traps:
  - Pattern **#6 Typed Array Views** (zero-copy windows into linear memory) + **#8 Zero-Copy
    Format-Aligned Layout (Arrow C Data Interface / FlatBuffers / Cap'n Proto)** — *this is
    literally what PROVE-14 does* with the four planes. "Sharing becomes free at runtime; major
    complexity in lifecycle management."
  - Pattern **#9 String Passing**: "UTF-8↔UTF-16 transcoding is unavoidable for JS strings
    today. **Defer decoding: keep as UTF-8 byte arrays until absolutely necessary.**" — exactly
    PROVE-14's byte-hash fast path (fold ASCII bytes with zero JS-string alloc, decode only the
    escaped minority via the flag channel, `REPORT.md:118-125`), which lifted full-walk
    0.66×→0.98×.
  - Pattern **#10 Batch/Coalesce**: "Per-call overhead (~100ns) multiplied by thousands
    becomes the bottleneck. A naively written loop can pay 50,000× that cost per frame." — the
    quantified statement of PROVE-14's full-walk refutation.
  - **Named trap for us:** Pattern #6's fragility — "**Typed Array Views are invalidated if
    WASM calls `memory.grow()`**." PROVE-14 dumps planes once and never grows; a live
    `bbnf-wasm parse()` (GN-4.W4, `GN-4.md:319-336`) that grows linear memory mid-session will
    silently detach the JS views. This is a concrete born-RED hazard the W4 wasm parity gate
    must catch (`GN-4.md:540-544` R4 flags stride/endianness plane drift, but NOT grow-detach —
    a gap LOCK-6 should name).

- **The Component Model / WIT is "the pattern that wants to kill them all"** — a
  language-agnostic IDL (records, lists, strings) + Canonical ABI auto-generating the
  serialization. Status: **production-ready server-side (Wasmtime), NOT yet in browser
  engines** (same DEV source). **Consequence:** WIT is the theoretically-correct 6B/6C
  unifier but is unavailable on the browser target the shopwindow needs (GN-6). Betting the
  isomorphism plane on WIT today is premature; note it as the future convergence point, build
  the manual typed-array-view seam now.

**Verdict on 6C:** the maximal reading ("Rust wasm is THE impl, TS is thin bindings") is
**refuted by our own PROVE-14 full-walk** and the field's JSON-parser benchmark: for the
value-materialization lane, cold V8's native `JSON.parse`/JS beats cold-JIT-over-wasm. 6C
survives only in the tempered tree-sitter shape: **wasm core for the layout-crossing +
stop-early lanes, TS-native idiom where V8 wins (full materialization, the `TapeDoc` walk).**
The PROVE-14 full-walk non-goal is not a failure of 6C — it is the measured boundary that
*proves such boundaries exist* (PORTFOLIO 6C's own framing, `PORTFOLIO.md:156-158`).

---

## §2 6B SPEC-GENERATED-SKELETON — "one spec generates both skeletons"

The strongest prior art for LOCK-6 lives here, and it is directly transplantable to the tape
decoder:

- **Kaitai Struct is the existence proof for 6B applied to tape/binary layouts.** A declarative
  YAML DSL (`.ksy`) describing "the structure of the data, not how you read it," compiled by
  `ksc` into ready-to-use parser modules in **11 languages** (C++, C#, Go, Java, **JavaScript**,
  Lua, Nim, Perl, PHP, Python, Ruby; **Rust** in the repo's language list)
  ([kaitai.io](https://kaitai.io/),
  [github.com/kaitai-io/kaitai_struct](https://github.com/kaitai-io/kaitai_struct)). 180+
  format specs in the gallery. **This is exactly 6B's "shared machine-readable spec generates
  types + tape decoders + oracle tables; hand code fills bodies."** The tape's four-plane SoA
  layout (`REPORT.md:44-60`) is *more* regular than most Kaitai targets — a Kaitai-style spec
  generating both the Rust `Tape` accessors and the TS `TapeDoc` decoder from one description is
  well within demonstrated art. And it aligns with the Constitution's grammar-totality clause
  (i): the decoder spec is *itself* a bbnf-defined grammar, not a second bespoke language.

- **Rust→TS type generation is a solved, shipping category — with named limits.** `ts-rs`
  (derive-macro `TS` trait, per-type export), `typeshare` (**static analysis** of Rust source,
  Kotlin/Swift/TS), and `specta` (runtime trait + dependency-closure export, born from rspc's
  need to "export a type and all the types it depends on" — a limit ts-rs hit)
  ([docs.rs/ts-rs](https://docs.rs/ts-rs/latest/ts_rs/),
  [docs.rs/specta](https://docs.rs/specta/latest/specta/),
  [github.com/Aleph-Alpha/ts-rs](https://github.com/Aleph-Alpha/ts-rs)). **The named limitation
  is load-bearing for us:** "TypeScript generics, conditional types, or advanced inference
  **cannot be expressed via Rust struct derives; generated types are simpler than hand-written
  equivalents.**" PT-E2 (`PT-3.md:305-357`) wants *precise* TS inference (`all(a,b)` →
  `Parser<[A,B]>` via `infer V`, zero-cost) — a generated skeleton would flatten exactly the
  conditional-type machinery PT-E2 is built to sharpen. **Consequence:** 6B fits the *data/tape*
  plane (regular, layout-driven) but is a poor fit for the *combinator type algebra* (needs
  hand-written conditional types). The reasonable-point line for 6B: generate the tape decoder
  + oracle tables + value-enum shapes; hand-write the combinator inference.

- **Second named 6B cost:** the generated bindings become a CI orchestration burden — "if
  frontend CI must verify bindings before deployment, the test-based export model requires
  careful orchestration" (ts-rs docs). Across our *read-only repo boundary* (§0.6) this is
  sharper: bbnf cannot run parse-that's generator, so a cross-repo 6B pipeline must ship the
  generated artifact *inside the letter*, regenerated parse-that-side. 6B does not dissolve the
  boundary; it moves the sync problem into the regen gate.

**Verdict on 6B:** strong, demonstrated, and directly transplantable **for the tape/decoder/
oracle-table plane** (Kaitai is the proof; the layout is regular). Weak for the combinator type
algebra (type-gen tooling's own documented limit). It does not erase the cross-repo boundary —
it relocates drift-prevention into a regen-diff gate (see §3).

---

## §3 6A MIRROR-TREE & 6D ORACLE-ISOMORPHISM — hand-mirror + differential corpus

These two are the "human keeps them in sync" family and the "prove behavior matches, ignore
layout" family; the field's evidence pairs them tightly.

- **The universal field lament is drift, and the universal cure is a generated fixture + a
  CI drift gate.** "Every full-stack team hits the same invisible tax: keeping types in sync
  across languages… fields get added to the Rust struct but forgotten elsewhere, so the
  TypeScript interface is silently missing fields"
  ([aarambhdevhub / Medium](https://aarambhdevhub.medium.com/i-got-tired-of-type-drift-so-i-built-a-rust-crate-to-end-it-7754643f7ad1)).
  The named cure: "a **shared testvectors fixture set generated by a deterministic script**,
  consumed in integration tests, with a **drift-detection CI job that regenerates fixtures on
  every side change and fails the build if re-generated bytes differ.**" This is the operational
  core of BOTH 6A (a fixture-diff *is* the isomorphism linter) and 6D (the shared corpus).
  PROVE-14 already built the primitive: `verify.mjs` cross-checks the wasm/dump planes byte-for-
  byte, and GN-4.W4's born-RED gate asserts "wasm planes byte-identical to the `tape_dump`
  fixture" (`GN-4.md:362-367`) — that IS the drift gate, already specified.

- **6D's "shared conformance corpus + differential tests" is the industry standard for parser
  parity.** oxc/Rome run **Test262 + Babel + TypeScript conformance suites** as the parity
  oracle for a Rust parser against the JS-defined spec
  ([oxc-project.github.io/…/conformance](https://oxc-project.github.io/javascript-parser-in-rust/blog/conformance/)).
  Cross-language differential testing of JSON parsers is a published, peer-reviewed method
  (AsiaCCS'24: "Cross-Language Differential Testing of JSON Parsers," consensus-based
  normalization across C/C++/Rust/Java/Python,
  [dl.acm.org/10.1145/3634737.3657003](https://dl.acm.org/doi/10.1145/3634737.3657003)).
  PROVE-14's node-for-node digest parity vs `JSON.parse` across 320 cold children
  (`VERIFY.md:30-37`) is precisely this method, already discharged for the tape. The pre-built
  corpus to inherit: `JSONTestSuite` (nst) for JSON, the CommonMark/WPT model generally.

- **6A's hand-mirror + linter has a hard, documented failure mode: V8-vs-LLVM idiom divergence
  makes line-for-line mirroring counterproductive.** R4 (`R4-parse-that-ts.md:178-208`) shows
  the TS core is *deliberately* non-isomorphic to naive Rust: arity-unrolled `any`/`fuseAll`
  (leaf.ts:39-57,183-251), `Int8Array(128)` dispatch table, sticky-`test()` zero-alloc regex,
  `Object.create` view. These exist because V8 rewards them and would be absent from an
  LLVM-idiomatic Rust mirror. A 6A linter enforcing structural correspondence would fight the
  very monomorphizations PT-3.W4 exists to *protect* (`PT-3.md:360-401`). **Consequence:** a 6A
  *tree/name* mirror (module layout, public API names, oracle-table names) is cheap and useful;
  a 6A *code-shape* mirror is refuted by the measured V8 idiom. This is the Constitution's
  clause (iii) "reasonable point" line for 6A.

**Verdict on 6A/6D:** 6D is not really a competing family — it is the **substrate every family
needs**: the differential corpus + byte-fixture drift gate is the isomorphism proof regardless
of how the two sides are built. PROVE-14 already implements it. The Constitution frames 6D's
null hypothesis as "must be DEFEATED by measured maintenance/drift cost, or it wins"
(`PORTFOLIO.md:159-161`); the field evidence says a *pure* 6D (behavior only, layout totally
free) under-delivers exactly where our tape needs byte-layout identity to be zero-copy — so 6D
wins *as the verification layer* but does not win *as the whole answer* for the tape plane,
which needs 6B/6C layout identity. 6A survives as tree/name/oracle-table mirroring under a
linter; 6A-as-code-shape-mirror is defeated by R4's V8 idiom.

---

## §4 The synthesis the evidence points at (for downstream passes, not a verdict)

The prior art does not select one family — it stratifies them by plane, and the strata are
mutually consistent:

| Plane | Winning prior art | Family it realizes |
|---|---|---|
| **Tape layout** (4 SoA planes) | Cap'n Proto / FlatBuffers / **Arrow C Data Interface** zero-copy; Kaitai spec-gen | 6B generates it, 6C crosses it zero-copy |
| **Decoder / oracle tables** | Kaitai Struct multi-lang codegen | 6B (grammar-defined per Constitution i) |
| **Value-materialization lane** | tree-sitter native-first; V8 `JSON.parse` beats wasm (ours + field) | 6C **tempered** — TS-native where V8 wins |
| **Combinator type algebra** | ts-rs/typeshare/specta — *documented to flatten* conditional types | hand-written (PT-E2), NOT 6B-generated |
| **Parity verification** | oxc/Test262, AsiaCCS'24 differential, testvector drift-CI | 6D + 6A-linter, the shared substrate |
| **Module tree / names** | the cheap, universal mirror | 6A tree-mirror (NOT code-shape mirror) |

The boundary line the whole lock turns on — the Constitution's "reasonable point" — is drawn
by measurement in three places our evidence already fixes: **(a)** cross layout, never per-value
(PROVE-14 4.4× vs 0.9×); **(b)** generate the regular data planes, hand-write the V8/conditional-
type idiom (typeshare's limit + R4's monomorphizations); **(c)** the drift gate is a byte-
fixture diff in CI (the field's universal cure, already specified in GN-4.W4).

---

## §5 Named traps carried into downstream passes

1. **`memory.grow()` detaches typed-array views** (16-patterns #6) — a live `bbnf-wasm parse()`
   that grows linear memory silently invalidates the JS plane views. PROVE-14 dumped once and
   dodged this; the productionized W4 export must re-create views after any grow, or pre-size.
   *This hazard is not yet in GN-4.md's R4 (which covers stride/endianness drift, not
   grow-detach) — LOCK-6 should surface it.*
2. **The full-walk 2× temptation** — refuted twice (ours PROVE-14, field's 25%-slower JSON
   benchmark). Any pass proposing "wasm makes the TS full parse faster" is relitigating a
   measured NO. Cross for *layout + stop-early*, never for full materialization.
3. **Generated code flattens the type algebra** (typeshare/ts-rs documented limit) — do not
   6B-generate the combinator inference; PT-E2's `infer V` precision cannot be derive-macro'd.
4. **Code-shape mirroring fights V8** (R4 §6) — a 6A linter enforcing line-for-line Rust≡TS
   would delete the arity-unrolls/dispatch-tables PT-3.W4 protects. Mirror tree + names +
   oracle tables; do NOT mirror control-flow shape.
5. **The cross-repo boundary is not dissolvable by tooling** (§0.6, `PT-3.md:101-106`) — no 6B
   pipeline lets bbnf run parse-that's generator; the sync artifact ships *inside the letter*,
   regenerated parse-that-side. Any "one shared generator repo" design violates the non-clobber
   law.
6. **WIT/Component Model is not browser-ready** (16-patterns) — the theoretically-correct IDL
   unifier can't ship to the GN-6 playground target yet; note as future convergence, build the
   manual typed-array seam now.
7. **Warm/cross-run benches are disinformation** (SPEC §0; PROVE-11 §3.1 load-confound;
   `PT-3.md:71-76`) — every cross-language ratio must be cold, paired, single-process
   interleave; the naive `vitest bench` cross-run is refused as evidence.

---

## §6 Sources

In-repo (READ): `restart/skinny/tranches/sk-v24/prototypes/PROVE-14/{REPORT.md,VERIFY.md}`;
`restart/skinny/tranches/sk-v24/SPEC.md` §0/§1/§2b; `restart/skinny/tranches/sk-v24/research/R4-parse-that-ts.md`;
`docs/tranches/sk-v24/GN-4.md` (W4); `docs/tranches/sk-v24/PT-3.md`; `crates/runtime/src/tape/mod.rs:121-137` (per REPORT seam).

Web (accessed 2026-07-16):
- Rust-WASM vs TS JSON-parser boundary benchmark — https://www.kunalganglani.com/blog/rust-wasm-vs-typescript-performance
- 16 Patterns for Crossing the WebAssembly Boundary (+ Component Model/WIT) — https://dev.to/rafacalderon/16-patterns-for-crossing-the-webassembly-boundary-and-the-one-that-wants-to-kill-them-all-5kb
- Kaitai Struct (declarative binary spec → 11-language parsers) — https://kaitai.io/ , https://github.com/kaitai-io/kaitai_struct
- tree-sitter (grammar → C + native bindings + wasm consumed by native) — https://tree-sitter.github.io/tree-sitter/using-parsers/
- ts-rs / specta / typeshare (Rust→TS type gen + documented flattening limit) — https://docs.rs/ts-rs/latest/ts_rs/ , https://docs.rs/specta/latest/specta/ , https://github.com/Aleph-Alpha/ts-rs
- Cap'n Proto / FlatBuffers / Apache Arrow (zero-copy, same-bytes cross-language; Arrow uses FlatBuffers internally) — https://en.wikipedia.org/wiki/Cap%27n_Proto , https://en.wikipedia.org/wiki/FlatBuffers
- oxc conformance (Test262 + Babel + TypeScript suites as parity oracle) — https://oxc-project.github.io/javascript-parser-in-rust/blog/conformance/
- Cross-Language Differential Testing of JSON Parsers (AsiaCCS'24) — https://dl.acm.org/doi/10.1145/3634737.3657003
- Type-drift cure: generated testvectors + CI drift gate — https://aarambhdevhub.medium.com/i-got-tired-of-type-drift-so-i-built-a-rust-crate-to-end-it-7754643f7ad1
