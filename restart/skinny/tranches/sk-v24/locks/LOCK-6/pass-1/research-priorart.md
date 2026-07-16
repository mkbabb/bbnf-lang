# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): PRIOR-ART RESEARCH (pass 1)

*Researcher: prior-art, pass 1. No family allegiance — this sweep serves 6A/6B/6C/6D
evenly. Registry withheld by protocol. Citations: web sources inline as markdown links;
codebase as `path:line`; evidence base as SPEC §0 / PROVE-14 / constitution clauses.*

---

## 0. What LOCK-6 decides (the problem class, restated from the charter)

Proper isomorphism **to a reasonable point**, in layout and in code — *measured, not
asserted* (Constitution iii). The verdict must partition the rust/ts surface into: what is
**mirrored** (module tree, names, oracle tables, tape layout, bench protocol) and what stays
**idiomatic per-language** (V8 laws vs LLVM laws). Four candidate architectural centers:

- **6A MIRROR-TREE** — hand-maintained mirrored trees + a CI isomorphism *linter*.
- **6B SPEC-GENERATED-SKELETON** — one machine-readable (grammar-driven) spec generates both
  skeletons; hand code fills idiomatic bodies; drift impossible where generated.
- **6C WASM-CORE** — Rust→wasm is the one implementation; TS = bindings + idiom layer;
  TS-native only where V8 measurably wins.
- **6D ORACLE-ISOMORPHISM** — behavioral isomorphism only; shared conformance corpus +
  differential tests; layout free per language. *Stated as the null hypothesis: it wins
  unless DEFEATED by measured maintenance/drift cost.*

This problem is **live and partially-solved in-tree already** (§1) and **heavily precedented
in the wider ecosystem** (§3), and — critically — our own evidence base already fixes several
of the answers as LAW (§2). The traps are named and measured elsewhere (§4).

---

## 1. THE EXTANT STATE — the rust/ts split already exists, half-isomorphic (codebase truth)

parse-that already ships **both** a Rust core (`rust/parse_that/`) and a TS core
(`typescript/src/parse/`), and its own docs *claim* isomorphism:
`parse-that/typescript/src/parse/` is annotated "**Core parser modules (isomorphic layout)**"
(root `CLAUDE.md:9`); the playground overview calls it "a parser combinator library with
**isomorphic** TypeScript and Rust implementations" (`docs/playground/overview.md:9`). So
LOCK-6 is *not* greenfield — it adjudicates an **existing, asserted-but-unverified**
isomorphism. The measured correspondence today:

| concern | Rust `parse_that/src/` | TS `src/parse/` | isomorphic? |
|---|---|---|---|
| core combinator type | `parse.rs` (`Parser<'a,O>`) | `parser.ts` (`Parser<T>`, 711 LOC) | names diverge (parse vs parser) |
| parse state | `state.rs` | `state.ts` | yes (name-level) |
| leaves | `leaf.rs` | `leaf.ts` | yes |
| lazy | `lazy.rs` | `lazy.ts` | yes |
| balanced split | `split.rs` | `split.ts` | yes |
| debug | `debug.rs` | `debug.ts` | yes |
| utils | `utils.rs` | `utils.ts` | yes (both are **god modules** — R4 §1.2) |
| domain parsers | `parsers/` | `parsers/` | dir-module both sides |
| **combinators** | `combinators/methods/{sequence,alternation,repetition,sep_by,minus,map,recover}.rs` | folded **inline into `parser.ts` methods** | **DIVERGENT** (Rust dir-module vs TS monolith) |
| **packrat** | (no dedicated file) | `packrat.ts` (488 LOC, opt-in WDM) | **TS-only home** |
| **arena** | `bump_slab.rs` | — | **Rust-only** (V8 has no arena analog) |
| **span/enum-dispatch** | `span_parser/` (9 files, vtable-free) | — | **Rust-only** (LLVM monomorph idiom) |
| **regex engine** | `rust/regex/` (bespoke HIR→NFA→DFA→egraph, 6742 LOC — R3) | **native JS `RegExp`** (`leaf.ts:317-369`) | **NO TS ENGINE** (R4 §3) |

**The measured lesson from our own tree:** isomorphism already holds cleanly at the
*leaf/state/lazy/split/debug/parsers* layer and **breaks precisely where each language's
performance idiom diverges** — Rust's arena (`bump_slab.rs`), enum-dispatch (`span_parser/`),
and bespoke DFA regex have **no V8 analog**; TS's packrat has a dedicated home Rust folds
elsewhere; TS uses native `RegExp` because a hand DFA cannot beat V8's JITed Irregexp on the
combinator hot path. This is the empirical shape of "reasonable point": **the boundary is not
arbitrary — it falls exactly at the V8-law/LLVM-law seam** (Constitution iii names this
explicitly). Any LOCK-6 verdict that mandates 1:1 layout *through* these idiom-divergent zones
fights the grain the codebase already discovered.

---

## 2. CONSTRAINTS OUR EVIDENCE BASE ALREADY IMPOSES (binding before any web citation)

These are not open questions — they are certified/frozen and pre-decide parts of the lock:

1. **The FFI/crossing cost is real and scales with payload (PROVE-14, CONFIRMED).** The
   offset-tape **layout crosses byte-exactly** rust→ts (all 6 structural counts + codeUnits +
   32-bit fingerprint identical across 320 cold children — REPORT §"Byte-exact parity",
   VERIFY confirms). BUT: **full-walk crossing is at PARITY with native `JSON.parse`
   (~0.91×), not 2×** — "cold native `JSON.parse` is a hand-optimized C++ parser; the TS lane
   is cold-JIT JS" (REPORT §"Why full-walk is parity"). **Stop-early wins 4.4×** *because it
   does not cross what it does not read.* This is the load-bearing constraint on **6C
   WASM-CORE**: crossing a *whole* materialized structure across the boundary is bottlenecked;
   the win exists **only in the amortized/partial-access regime.** SPEC §0 row 14 codifies
   this: "full-walk ~0.91× (2× honestly unmet — native parse is near-optimal for full
   materialization)."

2. **Behavioral isomorphism is already the plan-of-record's close gate.** GN-4.W4 ships the
   TS tape decoder whose **oracle is `JSON.parse`** (digest parity, `tape-parity.test.ts`);
   GN-4.W5's four-lane `value_parity.rs` + `cross-lane.mjs` asserts **rust and ts lanes agree
   node-for-node** (GN-4 §4 W5 gate). So the SPEC has *already committed to 6D-style
   differential parity oracles* as the binding gate — 6D is not hypothetical, it is installed.

3. **The type plane costs zero at runtime (Constitution v, SPEC row 08).** Any generated-
   skeleton scheme (**6B**) that emits *runtime* glue on the hot path violates this; PT-3.W3
   is explicit that TS inference tightening must be "compile-time only… emitted JS
   byte-identical" (PT-3 §W3 zero-runtime-cost invariant). 6B must generate *types + decoder
   tables + oracle fixtures*, never runtime dispatch.

4. **Colocation grand edict + no god modules (Constitution vi).** Any mirror scheme inherits
   the directory-module law on *both* sides. Note both `utils.ts` (R4 §1.2) and (by symmetry)
   Rust's `utils.rs` are **already-flagged god modules** — a mirror linter (6A) would have to
   encode "god-module-free" as an isomorphism invariant or it mirrors the rot.

5. **The bench protocol is already a shared law (SPEC §4, PT-3 §1.2).** Cold, paired
   interleaving, N≥200 (Rust) / N≥12 rounds × ≥300k ops (TS), warm benches forbidden,
   packrat armed-vs-unarmed disclosed. **The bench protocol is the one artifact that is
   *already* isomorphic across the split by decree** — the strongest existing evidence that
   "mirror the protocol/oracle tables" (6A/6D) is the low-cost, high-value axis.

6. **x86 dead / aarch64-only + NEON-only (Constitution iv, SPEC §0).** The Rust core's SIMD
   idiom (NEON shufti/truffle) has **no portable analog** — the refuted row 09
   (portable_simd→NEON shim, "parity-to-slower") proves you cannot shim your way to a shared
   SIMD layer. **6C cannot claim the Rust NEON kernels cross to wasm as-is** (wasm SIMD ≠
   NEON); the crossing is the *tape*, not the *kernel*.

7. **One regex home (Constitution i/ii, LOCK-5).** The regex engine is Rust-bespoke; TS uses
   native `RegExp`. LOCK-6 must not contradict LOCK-5's verdict — the regex *grammar* may be
   shared/bootstrapped, but the *engine* is measured Rust-only vs V8-native, a settled
   non-isomorphism (R4 §3: "any TS regex-engine work is greenfield… the bespoke engine lives
   on the Rust side").

---

## 3. PRIOR ART — what won and lost elsewhere for THIS problem class

### 3A. Structural file-by-file port + conformance oracle (the 6A×6D combination) — WON at scale

The strongest external precedent is **Microsoft's TypeScript→Go port ("Project Corsa" /
tsc-go)**. They **"moved the existing structure file-by-file… deliberately preserving the
same algorithms, the same data structures, and therefore the same type-checking semantics"** —
a *port*, not a rewrite — and validated equivalence against **"its decade-old test suite and
its internal fuzz-testing rigs,"** reporting **only 74 test-case discrepancies remaining**
(all known/intentional) at the Dec-2025 checkpoint
([Why a port not a rewrite](https://github.com/microsoft/typescript-go/discussions/410),
[VS Magazine](https://visualstudiomagazine.com/articles/2025/03/11/microsoft-ports-typescript-to-go-for-10x-native-performance-gains.aspx)).
Two lessons directly on-point: (i) **structural mirroring is a deliberate, sustainable
strategy** when the two languages' idioms are close enough (they chose Go partly *because*
"Go's syntax and style were more closely aligned to the existing… codebase"); (ii) **the
conformance corpus, not layout inspection, is what proves parity** — 6A's mirror is the
*maintenance* mechanism, 6D's oracle is the *correctness* mechanism. They are complementary,
not competing. This is the single most important external data point for LOCK-6: the
winning real-world play is **6A (mirror) + 6D (oracle) together**, not either alone.

### 3B. Schema-generated cross-language skeletons (6B) — WON for *data types*, LOSES for *logic*

protobuf / FlatBuffers / Cap'n Proto: "**all schema-driven** (you must define your message
types in a special language upfront)"; the schema is the single source of truth and "code
generation is always based on a single source of truth… rather than allowing manual
implementations to diverge"
([HN comparison](https://news.ycombinator.com/item?id=23589402), [FlatBuffers
Wikipedia](https://en.wikipedia.org/wiki/FlatBuffers) — targets C++/Rust/TS/… from one .fbs).
FlatBuffers is *especially* relevant: it is a **zero-copy** format (readers index into the
buffer without parsing) — structurally the **same idea as our offset-tape** crossing to TS.
For **Rust→TS type sharing** specifically, the ecosystem has converged on derive-macro
generators: **ts-rs** ("generate TypeScript bindings… keep your types in one place"),
**typeshare**, **specta** ([ts-rs](https://github.com/Aleph-Alpha/ts-rs),
[specta](https://docs.rs/specta/latest/specta/)). **Named limits (the losses):** ts-rs
"deals with types individually… not possible to export a type and all of the other types it
depends on"; typeshare "statically analyzes… results in a **loss of information** and lack of
compatibility with types from other crates" (specta docs). **Verdict for 6B:** schema-gen is
proven for *tape layout, type shapes, decoder tables, oracle fixtures* — the **data plane** —
and this maps cleanly onto our constitution (grammar IS the schema, so the generator already
exists). It is **unproven for combinator *logic*/control flow**; nobody generates the parser
body cross-language from a schema, they hand-write idiomatic bodies. 6B's honest scope =
"generate the mirror-able data/oracle surface, hand-fill the idiomatic bodies" — which is
*exactly* the charter's own framing of 6B.

### 3C. WASM-core + thin bindings (6C) — WON for correctness/reach, LOSES on the crossing tax

The Rust-JS-tooling cohort (**oxc, swc, Biome**) is the definitive prior art. Raw core speed
is enormous (oxc parser ≥3× swc, ≥5× Biome —
[oxc benchmarks](https://oxc.rs/docs/guide/benchmarks)). **But the crossing is the tax that
sinks the naive WASM-core play:** "**the serialization and deserialization costs increase
linearly with the input size**"; "passing Rust data structures to JavaScript is complex and
can significantly affect performance"; swc/oxc even resort to "**calling `JSON.parse` on
strings returned from Rust**" as the *faster* path than returning structured data over FFI
([Herrington Darkholme benchmark](https://dev.to/herrington_darkholme/benchmark-typescript-parsers-demystify-rust-tooling-performance-2go8),
[NAPI-RS v3](https://napi.rs/blog/announce-v3)). **This is PROVE-14's exact finding
independently reproduced at ecosystem scale:** crossing a full AST/structure is
boundary-bound, and the win survives only when you *don't* cross the whole thing. wasm-bindgen
further "**cannot use `std::thread`**" and third-party thread-dependent crates, needing
"complex conditional compilation" (oxc guide). **Verdict for 6C:** the WASM-core wins for a
browser playground / no-native-binary fallback (oxc's stated wasm use-cases) and for
*stop-early / partial-access* lanes (our 4.4×), but **loses as the universal "one
implementation" mechanism** for full-materialization TS work — the crossing tax makes a
TS-native decoder competitive-to-better. The PROVE-14 full-walk lesson is a specific instance
of the general oxc/swc law. 6C's honest scope = "wasm for the tape export + partial-access
lanes + playground; TS-native where full walk / hot combinators live."

### 3D. Differential testing / behavioral parity (6D) — the settled methodology for N impls

Differential testing is the established discipline: "**two or more independent implementations
of a specification are exercised on identical inputs, and discrepancies… automatically
flagged**" — "highly effective for domains where writing a complete test oracle is infeasible"
([Differential testing, Wikipedia](https://en.wikipedia.org/wiki/Differential_testing),
[Emergent Mind overview](https://www.emergentmind.com/topics/differential-testing)). It is the
literal method behind the tsc-go conformance validation (§3A). **Cross-language JSON parsers
are a published differential-testing target** ([ACM AsiaCCS 2024, *Cross-Language Differential
Testing of JSON Parsers*](https://dl.acm.org/doi/10.1145/3634737.3657003)) — i.e. our exact
value-plane problem (rust tape vs TS decode vs `JSON.parse`) is a known-good differential
oracle setup. **This is why the charter frames 6D as the null hypothesis:** behavioral parity
via a shared corpus is the *cheapest* and *most robust* correctness guarantee, and the SPEC
already installs it (GN-4.W5 four-lane oracle, PROVE-14 digest parity). 6D's honest scope =
"the corpus + differential oracle is *mandatory infrastructure regardless of which family
wins* — the open question is only whether layout is *also* mirrored (6A) or left free."

### 3E. The drift problem — named, and IR-emitter is the ecosystem answer

The generic "type drift across languages" pain is well-documented, and the converged solution
is an **IR-with-per-language-emitters** architecture: "emitters only work with the IR, never
with Rust's AST… TypeScript emitters handle TypeScript… Python emitters for Python"
([Type Drift, Medium](https://aarambhdevhub.medium.com/i-got-tired-of-type-drift-so-i-built-a-rust-crate-to-end-it-7754643f7ad1)).
**This is the precise shape of our multi-backend design already** (memory: `multi-backend` —
shared decision driver + Emitter trait for Rust/TS/WASM). It corroborates 6B's mechanism *and*
the standing no-TS-IR feedback (`no-ts-ir`: "AST IS the combinator") — the tension LOCK-6 must
resolve is: **generate the shared IR-level skeleton (types/tapes/oracles) but do NOT interpose
an IR between the TS combinators and their execution.**

---

## 4. TRAPS NAMED IN COMPARABLE SYSTEMS (what to refuse)

1. **The full-crossing trap (6C-killer).** Believing "compile the Rust core to wasm and TS is
   free bindings" — the crossing cost is **linear in payload** (oxc/swc, PROVE-14). Refuse any
   6C claim that a *full materialization* crosses cheaply. The win is *only* partial-access.

2. **The shim-parity self-deception (SPEC row 09).** portable_simd→NEON shim "did not
   reproduce… parity-to-slower." Any 6C/6B story that a shared SIMD/kernel layer crosses
   languages at speed is refuted by our own fleet. NEON kernels do **not** cross; only the
   *tape* crosses.

3. **The asserted-but-unverified isomorphism trap (in-tree already).** parse-that's docs
   *say* "isomorphic layout" (`CLAUDE.md:9`) while the trees diverge at every idiom seam (§1).
   A mirror claim without a **linter that actually fails CI** is decoration. 6A's whole value
   is the *enforced* linter; a manual "we keep them in sync" is the documented failure mode.

4. **The schema-loses-information trap (6B-limit).** typeshare "loses information"; ts-rs
   "can't export dependent types." A grammar-driven generator must emit the *transitive
   closure* (tape + all referenced type shapes + oracle fixtures), or it silently drops the
   fields that matter — the "day-90 missing field" failure (Type Drift, Medium).

5. **The god-module mirror trap.** Mirroring `utils.ts`↔`utils.rs` faithfully **mirrors the
   god module** (R4 §1.2 flags `utils.ts`; PT-3.W2 splits it into `diagnostics/` + `scan.ts`).
   A mirror linter must encode the colocation edict as an invariant, not freeze the current
   (rot-carrying) tree.

6. **The over-mirror-through-idiom-seam trap.** Forcing Rust's `span_parser/` enum-dispatch or
   `bump_slab.rs` arena to have TS twins, or forcing TS's native-`RegExp` path to grow a
   bespoke DFA to "match Rust." The V8-law/LLVM-law seam (Constitution iii) is *where mirroring
   must stop* — the codebase already found this boundary empirically (§1).

7. **The layout-mirror-without-behavior-oracle trap (and its inverse).** tsc-go proves you
   need **both**: 6A alone (mirror, no oracle) can drift semantically while looking identical;
   6D alone (oracle, free layout) passes tests while maintenance cost of two unrelated trees
   compounds. The charter's 6D "null hypothesis must be DEFEATED by measured maintenance cost"
   is answerable *only* with a real drift-cost measurement, which no pass-1 research can
   assert — it must be **measured** (Constitution vii: concrete artifacts or SPEC-ONLY).

---

## 5. SYNTHESIS FOR THE FAMILIES (where the evidence points, no allegiance)

- **What the evidence base has ALREADY DECIDED:** (a) the **tape/oracle/bench-protocol layer
  IS mirrored** — by SPEC decree and PROVE-14 byte-exact crossing; (b) **behavioral parity
  oracles are mandatory infra** (GN-4.W5) regardless of the winning family — so **6D is a
  floor, not a choice**; (c) the **idiom seam (V8 vs LLVM) is where isomorphism lawfully
  ends** — arena, enum-dispatch, NEON, bespoke-DFA are Rust-only; packrat, native-RegExp are
  TS-idiomatic; the codebase discovered this empirically (§1).

- **6A (mirror-tree + linter):** externally validated by tsc-go's file-by-file port; the
  linter must (i) fail CI, (ii) encode a *name map* not just tree shape (Rust `parse.rs` ↔ TS
  `parser.ts` already diverge), (iii) carry the god-module/colocation invariant, (iv) **stop
  at the idiom seam** (an allow-list of language-only modules).

- **6B (spec-generated skeleton):** proven for the *data plane* (protobuf/flatbuffers/ts-rs
  family; FlatBuffers zero-copy ≈ our tape), unproven for combinator logic; our grammar
  already *is* the schema and multi-backend Emitter already exists — 6B's real content is
  "generate types + tape decoders + oracle tables from the grammar; hand-fill idiomatic
  bodies," constrained by zero-runtime-cost (Constitution v) and no-TS-IR (`no-ts-ir`).

- **6C (wasm-core):** the oxc/swc/Biome cohort + PROVE-14 jointly bound its scope — WINS for
  playground/fallback + partial-access (4.4×), LOSES the "one implementation" claim to the
  linear crossing tax on full materialization; TS-native survives where V8 wins (the very
  boundary GN-4.W4 already draws: full-walk = documented non-goal, TS-native decoder ships).

- **6D (oracle-isomorphism):** the settled differential-testing methodology; cross-language
  JSON-parser differential testing is published prior art matching our exact value plane; it
  is **already installed** as the GN-4 close gate. As the null hypothesis it can only be
  *defeated* by a **measured** two-tree maintenance/drift cost — which is a stage-2 prove
  obligation, not a pass-1 assertion.

**The convergent shape the prior art suggests (for synthesis, not a verdict):** the winning
real-world pattern (tsc-go, FlatBuffers, oxc) is **not a single family** but a *layering* —
6D as the mandatory behavioral floor, 6B generating the mirror-able data/oracle surface from
the grammar, 6A's linter enforcing the hand-written-body correspondence *up to the idiom
seam*, and 6C confined to the crossing/playground lanes where its measured win (partial
access) actually lives. Whether that layering collapses to one dominant family or stays a
composite is the measured question the later passes owe.

---

## 6. CITATIONS

**Codebase / evidence base:** SPEC §0 (rows 08/09/14 + refutation laws), SPEC §1/§2/§4;
PORTFOLIO constitution i–ix + LOCK-6 charter 6A–6D; PROVE-14 REPORT + VERIFY (byte-exact
crossing, full-walk parity, stop-early 4.4×); GN-4.md §4 W4/W5 (parity oracles installed);
PT-3.md §W2/W3 (god-module split, zero-runtime-cost inference); R3 (parse-that Rust map),
R4 (parse-that TS map, §1.2 god module, §3 no-TS-regex-engine, §6 V8 surfaces); parse-that
`CLAUDE.md:9`, `docs/playground/overview.md:9` (asserted isomorphism); in-tree tree
comparison (§1). Memory: `multi-backend`, `no-ts-ir`, `backend-agnostic-types`,
`isomorphic-api`, `wasm-subcrate-pattern`, `directory-module-structure`.

**Web:**
- [microsoft/typescript-go — Why a port not a rewrite (#410)](https://github.com/microsoft/typescript-go/discussions/410)
- [VS Magazine — Microsoft Ports TypeScript to Go](https://visualstudiomagazine.com/articles/2025/03/11/microsoft-ports-typescript-to-go-for-10x-native-performance-gains.aspx)
- [oxc benchmarks](https://oxc.rs/docs/guide/benchmarks) · [oxc guide](https://oxc.rs/)
- [Herrington Darkholme — Benchmark TypeScript Parsers (FFI serde cost linear in input)](https://dev.to/herrington_darkholme/benchmark-typescript-parsers-demystify-rust-tooling-performance-2go8)
- [NAPI-RS v3 announcement](https://napi.rs/blog/announce-v3)
- [FlatBuffers vs Cap'n Proto (HN)](https://news.ycombinator.com/item?id=23589402) · [FlatBuffers (Wikipedia)](https://en.wikipedia.org/wiki/FlatBuffers) · [Cap'n Proto (Wikipedia)](https://en.wikipedia.org/wiki/Cap%27n_Proto)
- [ts-rs](https://github.com/Aleph-Alpha/ts-rs) · [specta](https://docs.rs/specta/latest/specta/)
- [Cross-Language Differential Testing of JSON Parsers (ACM AsiaCCS 2024)](https://dl.acm.org/doi/10.1145/3634737.3657003) · [Differential testing (Wikipedia)](https://en.wikipedia.org/wiki/Differential_testing) · [Differential testing overview (Emergent Mind)](https://www.emergentmind.com/topics/differential-testing)
- [Type Drift — IR-emitter architecture (Medium)](https://aarambhdevhub.medium.com/i-got-tired-of-type-drift-so-i-built-a-rust-crate-to-end-it-7754643f7ad1)
</content>
</invoke>
