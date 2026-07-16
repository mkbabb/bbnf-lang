# LOCK-6 · THE ISOMORPHISM PLANE (rust/ts split) · Pass-3 · PRIOR-ART RESEARCH

*Sweep of what exists / won / lost elsewhere for THIS problem class — cross-language
layout mirroring, spec-generated skeletons, wasm-core-vs-native, and behavioral
differential isomorphism. No family allegiance: each datum is filed to the family it
constrains, favorable or hostile. Every external claim carries a citation; every
internal constraint names its SPEC §0 / PROVE / seed source.*

*All five load-bearing external claims (tsgo structural-port-enforced-by-corpus; Arrow
Archery SoA+differential; `memory.grow` ArrayBuffer detach; the WASM string-boundary tax;
Kaitai per-language codegen + read-only/declarative limits) re-verified fresh against live
sources 2026-07-16 — corroborated, and the 6C trap strengthened by a direct 2026 case
(OpenUI's Rust-WASM→TS rewrite, below).*

---

## 0. The problem class, framed

LOCK-6 asks how a Rust core and a TS face stay "isomorphic to a reasonable point" —
in **layout** (module tree, tape bytes, oracle tables) and in **code**. This is not a
novel problem; it is one the interchange-format and cross-language-compiler worlds have
litigated for a decade. Four families map onto four settled industry patterns:

| Family | Industry pattern | Verdict in the wild |
|---|---|---|
| 6A MIRROR-TREE (hand-mirror + linter) | hand-ported cross-language codebases | RARE as a *checked* invariant; parity is enforced by tests, not tree-linters |
| 6B SPEC-GEN-SKELETON (one spec → both) | FlatBuffers / Cap'n Proto / Arrow / Kaitai | DOMINANT for layout; the default winner |
| 6C WASM-CORE (Rust→wasm, TS idiom edge) | SWC / oxc / lightningcss / biome | WINS on maintenance, LOSES at the string boundary |
| 6D ORACLE-ISOMORPHISM (behavioral only) | Arrow Archery / test262 / WPT / tsgo | The empirically strongest — hard to defeat |

The through-line: **the biggest real cross-language systems enforce parity behaviorally
(6D) and generate layout mechanically (6B); hand-mirroring (6A) is a means, not the
checked contract, and per-node wasm crossing (6C naive) is a known wall.**

---

## 6A — MIRROR-TREE: hand-maintained mirror + isomorphism linter

**The flagship real-world hand-mirror is Microsoft's TypeScript→Go port (`tsgo`, Project
Corsa, announced Mar 2025; TS 7.0 RC 2026).** It is a *port, not a rewrite* — the team
transplanted the codebase **file-by-file, structurally identical**, to preserve exact
type-checking semantics [VS Magazine 2025-03-11; DeepWiki microsoft/typescript-go;
TS devblog "Progress on TypeScript 7", Dec 2025]. This is 6A's thesis executed at the
largest possible scale — and the load-bearing lesson is **what enforced the parity**:
not a tree/name-map linter, but the **decade-old conformance suite** — ~20,000 tests,
~6,000 error-producing, matched in all but **74 cases**, plus internal fuzz rigs
[TS devblog Dec 2025; corroborated fresh 2026-07-16: DeepWiki microsoft/typescript-go
confirms the **file-by-file structural transplant** from Strada to preserve type-checking
semantics, and the native compiler passing **>95% of the conformance suite** — the
directional magnitude behind the 74-case figure]. The structural mirror was the *method*;
the **behavioral oracle was the checked invariant**. No public account describes a
cross-language *tree correspondence linter* catching drift; the tests did.

**Implication for 6A (hostile):** the constitution's own 6A proposes an "isomorphism
LINTER (CI verifies tree correspondence + a name map)." No prior art shows such a linter
winning in the wild — intra-language API-surface guards exist (`cargo-public-api`,
`api-extractor`) but stop at one language; a *cross-language name-map linter is
unprecedented infrastructure*. The tsgo datum says the 6D null hypothesis (behavioral
oracle) is what real hand-mirrors actually rely on — 6A must defeat 6D on measured
maintenance cost, and the strongest comparable defeated *itself* toward 6D.

- Pass-3 Q3 (name-map linter owner): prior art gives no external owner model; the honest
  routing is to the LOCK-3 IR tower (grammar-derived names have a single source there),
  with the linter as a thin consumer — but the tsgo evidence argues the *test corpus*
  (6D) is the cheaper and proven enforcement seat.

## 6B — SPEC-GEN-SKELETON: one machine-readable spec generates both

**This is the industry's default and it wins decisively for layout.** Protocol Buffers,
**FlatBuffers**, **Cap'n Proto**, Thrift, Bebop, and **Kaitai Struct** all consume one
schema and emit decoders/types in N languages. Two are directly load-bearing here:

**Apache Arrow is the strongest precedent — and it is the PROVE-14 shape.** Arrow is one
**columnar (SoA) layout spec** with *independent* implementations in C++, C#, Go, Java,
**JavaScript, Rust**, Swift, integration-tested cross-language via the **Archery** tool
against a **JSON reference representation** + IPC / C-Data / Flight protocols
[arrow.apache.org/docs/format/Integration.html; github.com/apache/arrow]. Arrow proves
BOTH halves of LOCK-6 at once: a **SoA layout crosses byte-exactly across Rust↔JS**
(exactly PROVE-14's claim) AND parity is held by a **differential integration test**
(exactly 6D). If one project already unifies 6B+6D at production scale for a SoA
cross-language buffer, that is the reference architecture to cite, not reinvent.

**Kaitai Struct is the direct 6B analog and supplies the trap.** A `.ksy` YAML layout DSL
compiles to parsers in **11 languages** [kaitai.io; github.com/kaitai-io/kaitai_struct].
Its documented limitations are precisely LOCK-6's fault lines:
- **Code-gen quality varies significantly per target language** [BigGo News 2025-10] —
  this IS the "idiomatic per language" idiom-tax the constitution names (6B Q1/Q3): one
  spec does not yield equally-good code everywhere.
- **Serialization is experimental; read-only is the sweet spot** — matches PROVE-14
  (decode crosses, the tape is a read view).
- **Checksums / derived / conditional logic push against the declarative model** — this
  is the **6B Q2 dialect blocker in the field**.

**6B Q2 (one spec for AoS-stored-discriminator AND SoA-derived-from-source) — the
precise reconciliation blocker, with citations.** FlatBuffers stores an explicit
**vtable of offsets** (an AoS-with-stored-layout dialect: a Table is a pointer to a
vtable of little-endian offsets to each element) [flatbuffers.dev; studyraid FlatBuffers
comparison]. Cap'n Proto and rkyv likewise **store** pointers/offsets [rkyv.org
feature-comparison; david.kolo.ski "rkyv is faster than…"]. **None of these express a
derived-from-source dialect** where the discriminator is *not stored* but re-derived
(PROVE-14: kind = `classify(source[offsets[cursor]])`, separators never emit an offset —
REPORT §"the layout that crossed"). Kaitai's closest primitive is **`instances`**
(computed/lazy derived fields) but those generate per-language *getters*, not a layout the
peer language reads zero-copy. **Verdict for 6B Q2:** no existing schema DSL spans both
dialects in one layout; the lawful move is to model the derived-from-source dialect as a
**decode function over the SoA planes** (a projection), not a second stored-layout
variant — file the union-of-dialects as the named blocker, exactly as Kaitai's own
limitation predicts.

**6B Q4 (shared bytecode VM as a generation target the type-refutation does not bind) —
precedented, route to LOCK-4C.** Instruction streams are data and cross-language-portable
by construction: LPeg compiles PEGs to a parsing-VM bytecode; Russ Cox's regex VM / RE2
compile to one instruction set; **WebAssembly itself is the universal shared instruction
stream**. A shared combinator bytecode (LOCK-4C VM-CORE) is therefore a live, precedented
generation target distinct from *type* generation — a type-generation refutation does not
bind an instruction-stream target. Routing owed jointly to LOCK-4/LOCK-8, as the directive
states.

## 6C — WASM-CORE: Rust→wasm is the one impl; TS idiom only where V8 wins

**Every major JS-facing Rust tool ships this pattern** — SWC, **@oxc-parser/wasm**,
**lightningcss-wasm**, biome, dprint [npmjs @oxc-parser/wasm; lightningcss.dev]. It wins
on single-source maintenance. **But the decisive, well-documented trap is the boundary
cost for string-heavy parsing** — and it matches PROVE-14 exactly:

- A cited case study: a **TS JSON parser beat the Rust/WASM one by ~25%** — the Rust was
  fast; the loss was **every key/value crossing the WASM↔JS boundary** [DEV.to "16
  Patterns for Crossing the WebAssembly Boundary", 2025; medium/ThinkingLoop]. Strings
  are UTF-16 GC objects in JS, byte arrays in linear memory: **every string crossing =
  encode + copy + alloc on both sides** [same]. This is *precisely* PROVE-14's full-walk
  parity finding (materialize every leaf → the crossing/walk cost cancels the
  parse-avoidance) and *validates* 6C's "TS-native only where V8 measurably wins."
- **The strongest, most recent corroboration (2026): OpenUI rewrote its `openui-lang`
  Rust-WASM parser in pure TypeScript for a ~3× speedup** [openui.com/blog/rust-wasm-parser;
  DEV.to "We Rewrote Our Rust WASM Parser in TypeScript…", 2026; kunalganglani.com
  "Rust WASM vs TypeScript" 2026]. Named cause: the **"WASM Boundary Tax"** —
  `serde-wasm-bindgen` must **recursively materialize** Rust data into real JS
  arrays/objects (many fine-grained boundary crossings per `parse()`), whereas the
  string-return path is one `memcpy` + V8's native C++ `JSON.parse`. This is the exact
  mechanism of PROVE-14's full-walk loss, at production scale, in the current year — and
  it names the specific anti-pattern LOCK-6 must never adopt: **do not vend a
  materialized object graph across the boundary; vend the tape's four planes once and
  decode/skip in JS** (the stop-early lane). The lesson cuts BOTH ways: it is why 6C's
  wasm-core wins only when it crosses in batch, and why a naive "wasm returns the parsed
  object" backend would *lose to plain TS*.
- **The winning wasm pattern is BATCH, not per-node**: parse the whole doc in wasm, cross
  once. This is why PROVE-14 stop-early wins **4.4×** (one crossing, the tape stays as a
  view; skip 99% of the doc) and full-walk loses (~0.9×, native `JSON.parse` is a
  hand-optimized C++ parser cold JS cannot beat) [PROVE-14 REPORT/VERIFY, SPEC §0 row 14].
  The abrogation of full-walk (GN-4.W4 non-goal) is the *correct* industry-consistent call.

**6C Q2 (memory.grow invalidating outstanding typed-array views) — CONFIRMED as a real
wasm-bindgen trap, with citations and a priced mitigation.** Every wasm `memory.grow()`
(any `malloc`) **vends a new `ArrayBuffer` and DETACHES the old one**; all outstanding
JS typed-array views over wasm memory are invalidated (read empty / throw)
[Awesome Badger "WebAssembly Memory Growth and the Detached ArrayBuffer Problem";
wasm-bindgen issue #4395 "TypedArray::view may use detached arraybuffer", #1206, #1079].
This **directly threatens the two-parses-through-one-instance path**: parse A hands back
views; parse B allocates → grows → A's views silently detach. Mitigations in the field:
(a) **recreate views after each call** (compare `.buffer` identity to detect a grow)
[Awesome Badger]; (b) **copy-out eagerly** to stable JS buffers; (c) the TC39
**Resizable/Growable ArrayBuffer** proposal (in-place grow, no detach) — but requires the
memory be a resizable buffer and is not universal [proposals.es Resizable ArrayBuffers].
**Priced against the stop-early margin:** PROVE-14 already **counts a copy of the 118 KB
offset plane against the tape lane and still wins 4.4× on stop-early** — so copy-out is
affordable for the stop-early lane; it erodes full-walk further (already abrogated).
The safe design is copy-out-the-four-planes-once (stable views), which PROVE-14's own
accounting proves survivable.

- Q3 (confinement / one consolidated cdylib vs two cold instances): prior art favors
  **one instance, batch-cross, copy-out planes** — the per-instance cold-start + the
  detach hazard both argue against many live instances holding views concurrently.

## 6D — ORACLE-ISOMORPHISM: behavioral only; shared corpus + differential tests

**This is the empirically strongest family — the pattern the largest cross-language
systems actually use, and it is hard to defeat.**

- **Apache Arrow Archery** — C++/C#/Go/Java/JS/Rust/Swift differentially tested against a
  JSON reference + IPC/C-Data/Flight [arrow.apache.org Integration.html]. The gold
  standard: N independent impls, one corpus, differential — *layout free per language
  above the shared buffer spec*.
- **tsgo** held Go↔JS parity via the **20k-test conformance suite** (74 discrepancies),
  not a layout mirror [TS devblog Dec 2025] — the largest recent hand-port, kept
  isomorphic behaviorally.
- **test262 (ECMAScript), Web Platform Tests, SQLite SQL Logic Test, ICU** — the "shared
  conformance corpus + many implementations" governance model at scale.

**6D Q1 (cross-language digest fold, running not spec) — ready-made corpora exist:**
- **Nigel Tao's `parse-number-fxx-test-data`: ~5.3M float-string→exact-bits cases**, used
  by fast_float / simdjson / Rust core [github.com/nigeltao/parse-number-fxx-test-data;
  crates.io/fast-float]. This is the **last-ULP number oracle** for 6D Q2's injected
  "last-ULP number delta." The exactness matters: RapidJSON's fast mode is off-by-1-ULP
  and thus non-conformant; Eisel-Lemire is the exact algorithm [lemire.me 2020-03-10;
  nigeltao.github.io eisel-lemire]. **The number fold must be exact-bits, not tolerant.**
- **JSONTestSuite (nst, "Parsing JSON is a Minefield")**: 300+ **named accept/reject**
  files (filename encodes expected accept/reject; handcrafted + AFL-fuzzed)
  [seriot.ch/security/parsing_json.html; github.com/nst/JSONTestSuite]. This is the
  ready **accept/reject partition** for 6D Q4 ("values + accept/reject IN; wording + span
  OUT"). Seriot's central finding — **real parsers DIVERGE on edge cases (numbers,
  Unicode, structure)** — is direct evidence the corpus is load-bearing, not decorative.

**6D Q2 (static corpus vs coverage-guided fuzzer) — prior art says use BOTH:**
JSONTestSuite itself mixes **handcrafted + American-Fuzzy-Lop** cases [github.com/nst];
tsgo used its suite **plus fuzz rigs** [TS devblog]. The handcrafted corpus catches known
edge classes cheaply; the coverage-guided fuzzer finds the unknown (off-by-one escape,
astral strings) at a cycles-to-detection cost. Both numbers owed, as the directive asks.

**6D DAG (freeze strictly after LOCK-1 alphabet + LOCK-7 tie-breaker) — corroborated:**
JSONTestSuite's **`i_` (implementation-defined) files** demonstrate the accept/reject
boundary is *spec-underdetermined* [nst/JSONTestSuite results]. That underdetermination is
resolved only by the grammar's alphabet (LOCK-1) and the ambiguity tie-break (LOCK-7) —
so 6D's corpus boundary genuinely depends on those locks freezing first. The DAG
constraint is not bookkeeping; it is where the `i_` cases get adjudicated.

---

## Constraints our own evidence base already imposes (binding, do not relitigate)

From **SPEC §0** and the seeds — these bound every family:

1. **Full-walk 2× is UNMET and abrogated** (native `JSON.parse` is near-optimal for full
   materialization; cold JS cannot beat it) — SPEC §0 row 14, GN-4.W4 non-goal,
   PROVE-14 VERIFY (~0.91×). The tape's value is **stop-early (4.4×)**, the
   *not-touching-what-you-don't-read* edge. Any wasm design that reintroduces per-node
   crossing re-loses full-walk — the oxc/25%-case trap confirms it externally.
2. **The tape that crosses is SoA, offset-only, derived-from-source**: kind re-derived
   from `source[offset]`, separators never emit, payload arena provably empty
   (PROVE-14 REPORT). **AoS-vs-SoA is MOOT at HEAD — do not relitigate** (SPEC §0
   refutations). This *is* the dialect 6B must express; FlatBuffers/Cap'n Proto's
   stored-offset dialect is the *other* pole (6B Q2).
3. **Warm benches are disinformation; paired cold interleaving is mandatory even for
   "obvious" wins** (the row-09 self-deception lesson) — SPEC §0. Any 6C wasm-vs-native or
   6D fold measurement is cold + paired + base-pinned, per the standing law.
4. **Type plane costs zero at runtime** (constitution v; PT-3 W3 mirrors it for TS) — a
   6B skeleton that generates *types* must erase; an instruction-stream target (6B Q4) is
   the runtime-bearing exception, routed to LOCK-4C.
5. **PROVE-14 counts the plane-copy against the tape and still wins stop-early** — the
   pricing precedent that makes the 6C Q2 copy-out mitigation affordable.
6. **Fast TS face = the BUILD-TIME EMITTER; runtime decoders survive only as
   isomorphism/correctness oracles** (pass-3 binding frame, XL-D) — this is 6D's seat:
   the decoder's job is to be the oracle, not the hot path.

---

## Traps named in comparable systems (fold into critique)

1. **Per-node WASM↔JS crossing is a wall for string-heavy parsing** (TS beat Rust/WASM by
   ~25%) — 6C. Batch-cross or lose. [DEV.to boundary-patterns; medium/ThinkingLoop]
2. **`memory.grow()` detaches all outstanding ArrayBuffer views** — 6C Q2, a silent
   correctness bug in the two-parse path. [wasm-bindgen #4395; Awesome Badger]
3. **Generated-code quality varies per target language; derived/conditional logic fights
   the declarative model** — 6B idiom-tax + Q2 dialect blocker. [Kaitai limitations,
   BigGo 2025-10]
4. **No schema DSL spans stored-discriminator AND derived-from-source in one layout** —
   FlatBuffers/Cap'n Proto/rkyv all *store* offsets; the union spec is genuinely novel,
   model the derived dialect as a projection. [flatbuffers.dev; rkyv.org]
5. **JSON parsers diverge on edge cases; the accept/reject boundary is
   spec-underdetermined (`i_` files)** — 6D needs the LOCK-1/LOCK-7 DAG dependency, and
   the corpus is load-bearing. [Seriot; nst/JSONTestSuite]
6. **Last-ULP float parsing is a real cross-impl divergence** (RapidJSON fast-mode
   non-conformant) — 6D number fold must be exact-bits, use the 5.3M-case corpus.
   [lemire.me; nigeltao parse-number-fxx]
7. **The flagship hand-mirror (tsgo) enforced parity by the test corpus, not a
   tree/name-map linter** — 6A's linter is unprecedented; the 6D null hypothesis is
   empirically the default. [TS devblog Dec 2025]

---

## The synthesis handoff (one paragraph)

The interchange-format world already settled this problem: **generate the layout from one
spec (6B, Arrow/Kaitai), enforce parity by a shared differential corpus (6D,
Archery/test262/JSONTestSuite), ship the core once and cross the wasm boundary in batch
(6C, the oxc lesson), and treat the hand-mirror tree (6A) as a means whose checked
invariant is the corpus, not a name-map linter (the tsgo lesson).** Arrow is the single
reference that unifies 6B+6D for a SoA cross-language buffer — the exact PROVE-14 shape.
The genuinely novel work our evidence base forces is narrow: a **layout spec that
expresses the derived-from-source (no-stored-discriminator) tape dialect** (6B Q2, which
no existing DSL does) and the **copy-out-vs-detach pricing** on the two-parse path (6C Q2,
which PROVE-14's own accounting shows is survivable on stop-early).

---

### Sources

- TypeScript→Go port (tsgo): VS Magazine 2025-03-11; TS devblog "Announcing TypeScript
  7.0 RC" + "Progress on TypeScript 7 (Dec 2025)"; DeepWiki microsoft/typescript-go.
- Apache Arrow: arrow.apache.org/docs/format/Integration.html; github.com/apache/arrow;
  arrow.apache.org/rust.
- Kaitai Struct: kaitai.io; github.com/kaitai-io/kaitai_struct; BigGo News 2025-10-05.
- FlatBuffers/Cap'n Proto/rkyv: flatbuffers.dev/benchmarks; rkyv.org/feature-comparison;
  david.kolo.ski "rkyv is faster than…"; studyraid FlatBuffers comparison.
- WASM boundary cost: DEV.to "16 Patterns for Crossing the WebAssembly Boundary" (2025);
  medium/ThinkingLoop "WebAssembly for JS Full-Stack, Minus the Hype"; npmjs
  @oxc-parser/wasm; lightningcss.dev. **OpenUI Rust-WASM→TS rewrite (2026):**
  openui.com/blog/rust-wasm-parser; DEV.to "We Rewrote Our Rust WASM Parser in TypeScript
  and It Got Faster"; kunalganglani.com "Rust WASM vs TypeScript: Why the Faster Language
  Lost [2026]"; neugierig.org "Moving data from Rust to JS" (2024).
- memory.grow detach: Awesome Badger "WebAssembly Memory Growth and the Detached
  ArrayBuffer Problem"; wasm-bindgen issues #4395, #1206, #1079; proposals.es Resizable
  and Growable ArrayBuffers.
- Float parsing: github.com/nigeltao/parse-number-fxx-test-data; fastfloat/fast_float;
  lemire.me 2020-03-10; nigeltao.github.io/blog/2020/eisel-lemire.
- JSON conformance: seriot.ch/security/parsing_json.html ("Parsing JSON is a Minefield");
  github.com/nst/JSONTestSuite.
- Internal: SPEC §0 (rows 14, refutations); PROVE-14 REPORT+VERIFY; GN-4.md (W4);
  R4-parse-that-ts.md; pass-3 DIRECTIVES-RESEARCH.md binding frame (XL-A..H).
