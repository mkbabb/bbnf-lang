# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): FAMILY SPECS (synthesis, pass 1)

*Synthesis, Fable-tier, pass 1. One cogent spec per family while routes remain incompatible,
then the incompatibility map. Grounded against the extant tree, not only the research prose:
seams marked LIVE were verified present; seams marked PLANNED were verified absent (GN-4.W4
substrate). Elegance earns nothing; a family stalled at a missing primitive equal in difficulty
to the original problem is marked BLOCKED-candidate.*

---

## 0. The plane's coordinate system (what actually decides among the four)

The four families are not four styles of the same thing; they answer two orthogonal questions:

- **Q1 — one implementation or two?** 6C says ONE (Rust→wasm); the TS "implementation" is
  bindings + measured V8-native islands. 6A/6B/6D presuppose TWO artifacts that must be held
  equivalent.
- **Q2 — is the equivalence boundary CODE or DATA?** 6A pins CODE structure (a name map +
  linter). 6B removes the CODE-drift class by GENERATING the shared surface. 6D pins only DATA
  (a corpus + a language-neutral digest) and lets code float free.

6D is the **null hypothesis / floor**: behavioral parity is already installed as the plan-of-
record close gate (`crates/core/tests/{json_canonical_parity,ts_node_execute}.rs` LIVE;
GN-4.W5's four-lane `value_parity`). Every other family is "6D **plus** a structural
enforcement mechanism." The lock's verdict is therefore: *does structural enforcement buy
enough measured drift-avoidance to justify its tax, and on which surface* — the "reasonable
point" the constitution demands be measured, not asserted.

**Extant-state truth (verified, this repo):** the only LIVE cross-language mirror pair in
bbnf-lang is `crates/bbnf-path` ↔ `crates/bbnf-path-ts` (Rust `{lib,registry,path_macro}.rs`,
TS-facing `{schema,compile,fixture,template_tag}.rs` + `ts/{path,index}.ts`), guarded by
`crates/bbnf-path-ts/tests/isomorphic_path_error.rs` (268 LOC, incl. the exhaustive
`reason_variants_are_isomorphic` at `:227-268`). The tape substrate is LIVE at
`crates/core/src/runtime/tape/`. The Rust emitter is LIVE at `crates/core/src/backend/emitter.rs`
+ `crates/core/src/grammar/schema/emit/rust/`. `crates/bbnf-wasm` and `ts/tape` are **absent**
(6C/6B-tape substrate is the GN-4.W4 plan). parse-that's dual Rust+TS core is a **separate repo**
(only `docs/parse-that/*.md` present here) — the two-repo boundary is real, not rhetorical.

---

## FAMILY 6A — MIRROR-TREE (hand-authored both sides + CI isomorphism linter)

**Architecture.** Both trees stay hand-written and idiomatic; a checked-in machine-readable
**name map** (`isomorphism.toml` or a `.bbnf` fragment per constitution i) is the single source
of truth for what mirrors what; a CI **linter** proves the map holds against both *extracted*
surfaces. Structure is pinned at ITEM granularity; file-tree partition is a soft warn (Rust
`combinators/methods/` dir-module ≠ TS flat `parser.ts` monolith is lawful). This is Kotlin
Multiplatform `expect`/`actual` reconstructed across two repos and two languages, with an
external linter standing in for the single KMP compiler no toolchain provides here.

**Mechanism (four tiers, a pair declares which bind it):**
- **T1 name/tree correspondence** — the manifest enumerates `(rust_path, ts_path, {RustItem→tsItem})`;
  the linter asserts every mapped item exists on both sides AND **no surface item lacks a manifest
  entry** (totality; silent omission is RED). Promotes today's *prose* name map (the "mirror of
  `bbnf::path::*`" doc comments in `bbnf-path-ts/src/schema.rs`) to a checked artifact and deletes
  the prose.
- **T2 surface correspondence** — public surfaces snapshotted and diffed *through the name map*:
  Rust via `cargo-public-api` (rustdoc-JSON, committed `public-api.txt`), TS via API Extractor
  `.api.md` or a `ts-morph` surface script. Reports field-rename-on-one-side, variant-present-one-
  side, arity-drift.
- **T3 behavioral correspondence** — one shared fixture set drives a Rust integration test and a
  vitest test; generalizes the LIVE `isomorphic_path_error.rs` to a corpus feeding both.
- **T0 byte tier (tape only)** — the offset-tape mirrors at the BYTE level (PROVE-14); wasm-exported
  planes byte-identical to a Rust `tape_dump` fixture (born-RED, GN-4.W4 gate b). Stricter than any
  name map; composes with T1–T3.

**Integration seams.** LIVE seed: the bbnf-path pair + `isomorphic_path_error.rs:227-268`. Hard
case: the parse-that combinator core (separate repo) — `combinators/methods/` (dir-module) ↔
`parser.ts` (711-LOC monolith); the name map records `Parser::then ↔ Parser.then` at item
granularity without forcing file-tree identity. Tape byte seam: `crates/core/src/runtime/tape/mod.rs`.

**Gates it carries.** (i) Linter goes RED when one field/variant is renamed on one side only.
(ii) Totality: every surface item has a manifest entry or RED. (iii) Idiom-seam allow-list —
Rust-only `bump_slab`/`span_parser`/bespoke-DFA and TS-only `packrat`/native-`RegExp` are named
exempt, NOT forced to grow twins (over-mirror-through-idiom-seam is refused). (iv) God-module
invariant: the linter must encode the colocation edict, not freeze `utils.{rs,ts}` rot.
(v) CI budget fits the proof-gate lane; nightly-rustdoc-JSON demand reported if the greenfield is
stable-only.

**Exact open questions.** (a) The **clean-map ratio** on the combinator core — fraction of items
mapping 1:1 vs needing an idiom escape hatch; this IS 6A's "where reasonable ends" number and is
unmeasured (parse-that is a separate repo). (b) T2's toolchain cost (nightly rustdoc-JSON) —
blocker or fold-in? (c) Cross-repo CI ownership (bbnf-lang vs parse-that) — per-repo manifest
fragments + a vendored conformance corpus.

**Standing.** ADVANCE. Substrate LIVE (the one mirror pair + its exhaustive-parity test);
prior art strong (Microsoft tsc-go's file-by-file port validated by a conformance suite, not by
layout inspection — the winning play was mirror+oracle *together*). Not BLOCKED. The decisive
caveat, proven LIVE in `isomorphic_path_error.rs:129-138`: the `$.statuses[0].nope` path yields
`UnregisteredRule` offline but `UnknownField` via cdylib — identical shapes, divergent semantics.
**T1/T2 pass green here; only T3 catches it, and only if the corpus contains that input.** 6A is
necessary-not-sufficient without T3; T3 needs a coverage argument — which is exactly 6D's content.

---

## FAMILY 6B — SPEC-GENERATED-SKELETON (one grammar-driven spec generates both skeletons)

**Architecture.** A single machine-readable, grammar-derived spec generates BOTH the Rust and
TS *skeletons* — the surfaces that must never diverge (type declarations, tape-plane layout, the
node classifier as a DATA TABLE, oracle/conformance vectors) — while hand code fills idiomatic
bodies (Rust NEON kernels; the TS monomorphic V8 walk). Where a surface is generated, drift is
not detected but **unrepresentable**: a grammar edit regenerates both trees at once, and the
regen fixpoint (LOCK-7 7A) is the proof. The generated/hand boundary is claimed to coincide with
the structural/performance boundary, which coincides with the isomorphism/idiom boundary — that
coincidence, quantified, is the family's measured answer to "where reasonable ends."

**Mechanism — the spec is the compiler's own middle, not a new IDL** (authoring an IDL would
violate grammar totality i). Second consumer of the SAME IR + type-schema the LIVE Rust emitter
already walks:
- **Generated (drift-impossible):** (1) TS type declarations from the grammar-derived schema,
  numeric widths resolved per-backend (`u64/i64 → bigint`, backend-agnostic-types law); (2) the
  tape-decoder skeleton — SoA plane struct, the classify table as an `Int8Array(256)` literal
  (NOT hand control flow), sparse-flag binary search, pointer navigation shape; (3) oracle/
  conformance vectors emitted ONCE, read by both `crates/*/tests` and `ts/*/test`; (4) the wasm
  boundary signature, derivable from the four-plane facts so FFI glue cannot drift.
- **Hand-filled (idiomatic, per-language perf):** Rust NEON skip kernel; the TS monomorphic
  `TapeDoc` class body (four ctor-only fields = one hidden class; the generator supplies the
  classify *table* it reads, never the class *shape* it lives in).

**Integration seams.** Rust emitter to parallel: `crates/core/src/backend/emitter.rs` +
`grammar/schema/emit/rust/` (LIVE). TS emitter = a sibling directory-module
(`.../emit/ts_skeleton/`) producing a second file-map. Type spec: the grammar-derived schema
(the HM/DirectSchema plane). Tape facts: `crates/core/src/runtime/tape/`. Landing: the GN-4.W4
`ts/tape/{tape,load,pointer}.ts` file plan (PLANNED) — 6B recasts W4's "hand-port .mjs→.ts" as
"generate layout/classify/oracle, hand-fill the walk." Regen host: `xtask`, under the same
fixpoint gate as the Rust regen.

**Gates it carries.** (i) Two-consumer emit round-trips through the serde IR (genuinely the
middle, not a fork). (ii) **Drift-impossibility (the family-decider):** add one JSON node kind /
one flag bit → BOTH sides regenerate classify+type surfaces with ZERO hand edits; any surface
needing a hand edit is named OUTSIDE the drift-impossible boundary — that boundary IS the
measured result. (iii) V8 preservation: generated-table + hand-body reproduces PROVE-14 stop-early
≥4× (not mere parity) — proving generation didn't perturb the hidden class. (iv) Width fidelity:
a `u64` field resolves `bigint`, and the oracle catches a lossy `number`. (v) Cross-language
fixpoint: regen∘regen byte-identical for BOTH output trees. (vi) Zero-runtime-cost: generate
types + tables + fixtures, NEVER runtime dispatch (constitution v; no-ts-ir).

**Exact open questions (sharpest of any family).** (a) **Is the spec backend-NEUTRAL?** Verified
absent: no `struct BackendIr` in-tree; per R1 the IR carries Rust type-name strings
(`ty=Some("u64")`, `DirectBuild{shape:String}`). A truly neutral spec (abstract widths) is a
*prerequisite* to correct TS generation — a subset of the isomorphism problem, not equal to it,
so **not BLOCKED**, but convergence gates on this refactor. (b) **Is the generated fraction
non-trivial?** If classify+walk stay hand (perf-sensitive), the most drift-prone surface (the
classifier when a node kind appears) is hand on both sides and 6B degenerates into 6A with extra
machinery — the R4 decisive boundary. The node-kind enum + classify TABLE MUST be in the
generated fraction. (c) The quantitative "reasonable point": of PROVE-14's ~703-LOC TS decoder,
generable ≈ 120–160 LOC (layout + classify/sparse-flag + oracle + type interfaces) vs hand ≈ 200
LOC (byte-hash walk + descent).

**Standing.** ADVANCE, sharpest-uncertainty. Substrate PARTIALLY live (Rust emitter yes; neutral
spec + TS emitter to build). Prior art proven for the DATA plane (serde-generate, FlatBuffers,
tree-sitter node-types, Kaitai, ts-rs) and UNPROVEN for combinator logic — 6B's honest scope is
"generate the mirror-able data/oracle surface, hand-fill idiomatic bodies," exactly matching the
charter. Closest of the four to a structural stall (the backend-neutral-spec prerequisite) but
below the BLOCKED bar.

---

## FAMILY 6C — WASM-CORE (the Rust core compiled to wasm is the one implementation)

**Architecture.** Resolve the plane by COLLAPSE, not mirroring: one implementation (the Rust
generator's emitted parsers + runtime + value-fabric accessors + scan kernels, retargeted to
`wasm32-unknown-unknown` via wasm-bindgen); TS is its binding surface plus a thin idiom layer.
"Isomorphism to a reasonable point" shrinks to ONE data-layout contract (the four SoA tape
planes) verified by differential digest — **no module tree to lint, no skeleton to generate**.
Drift is impossible over the collapsed surface because there is only one source of behavioral
truth. TS-native code is admitted ONLY on measured islands where V8 defeats a wasm crossing; each
island is a documented negative result carrying its number.

**Mechanism.** The crossing is the SoA tape byte-for-byte: wasm-bindgen hands JS
`Uint32Array`/`Uint8Array` **views over wasm linear memory** for `offsets`/`flag_cursors`/
`flag_values` + `source`; kind re-derived JS-side from `source[offsets[cursor]]`; no discriminator
plane; the TS `TapeDoc` is a monomorphic hidden class navigating integer cursors, carrying **no
parsing logic**. Two access modes: **cross-once-navigate-many** (the wasm-core win — stop-early
`get`, materialize only visited leaves, byte-hash ASCII fast path with zero JS-string alloc) and
**full materialization** (the V8-native island — route to a TS-native path or native `JSON.parse`,
documented with its number). The generator itself stays host-side Rust and is NOT shipped to
wasm — only its OUTPUT (emitted parsers) + runtime crosses; that bounds the artifact.

**The central crux — SIMD does not cross.** The §0 headline numbers are NEON (shufti/truffle,
`vshrn` narrow, movemask, SWAR sink); `portable_simd` scalarizes on `wasm32`, and
`core::arch::aarch64` NEON does not compile to wasm at all. So the wasm core has THREE kernel
realities: native NEON, a scalar fallback, and — only if wasm keeps any SIMD — a hand-authored
`core::arch::wasm32` v128 dialect (`i8x16_swizzle`/`i8x16_bitmask`). **6C's honest position: ship
the scalar fallback first.** PROVE-14 measured stop-early 4.4× with a *scalar* JS walk — the win
is "don't touch what you don't read," not SIMD throughput. The SIMD kernels are a native-only
accelerant the wasm core legitimately FORGOES without losing its differentiating lane. A wasm32
retarget of shufti is a named open measurement whose failure is a first-class negative result.

**Integration seams.** ABI export boundary: the four public plane accessors on
`crates/core/src/runtime/tape/mod.rs`. To BUILD (PLANNED, absent today): `crates/bbnf-wasm`
cdylib (`parse(src)->TapeExport`), `ts/tape/{tape,load,pointer}.ts` idiom layer. Precedent +
cost witness LIVE: `playground/src/wasm/bbnf_wasm_bg.wasm` (**3.63 MB** — a real instantiation
surface), which GN-0 archives and GN-6 rebuilds "on the GN-4 seam." Protected out-of-scope
surface: parse-that combinator backbone (separate repo, TS-native by decree; 6C draws its
boundary AROUND parse-that, not through it).

**Gates it carries.** (i) Live wasm export reproduces PROVE-14 byte-exact digest + stop-early
4.4× **from the wasm boundary**, not from `.u32` files, with the plane-copy of R1 counted.
(ii) Detached-buffer failure provoked and cured (copy-on-export vs owned-return ABI) — a
CORRECTNESS gate: retained views detach on any later memory-grow and "parse" to wrong-but-
plausible digests. (iii) Module-size + cold-instantiation budget → the document-size crossover
below which `JSON.parse` wins (the honest lower bound); strip the generator from the wasm build.
(iv) NO SIMD perf claim on the wasm path (§0/09 refutation law). (v) Every TS-native island
carries a digit-for-digit differential test or is FORBIDDEN — the one lint 6C keeps in place of
the deleted mirror linter.

**Exact open questions.** (a) Does the 4.4× survive the ACTUAL wasm→JS handoff (bbnf-wasm doesn't
exist yet)? (b) The document-size crossover (R2). (c) Does `core::arch::wasm32` shufti pay on
wasm — accelerant or negative result? (d) The per-grammar boundary (CSS has no native competitor
→ its full-walk lane may WIN in wasm → no TS-native island; JSON is the *hardest* case, so
JSON-parity is the conservative floor).

**Standing.** ADVANCE but **SCOPE-BOUNDED**, not BLOCKED. 6C is INHERENTLY a *partial* collapse:
by its own charter it excludes parse-that combinators and full-materialization, and forgoes SIMD.
Its "one implementation" claim holds only for the generator's tape/value runtime; it cannot be
the whole plane's answer. The full-crossing tax that bounds it is not a hope but a measured law
(PROVE-14 full-walk ~0.91×, independently reproduced at ecosystem scale by oxc/swc resorting to
`JSON.parse` over FFI). The missing primitive (wasm SIMD = NEON) is NOT equal to the original
problem because 6C explicitly does not need it — hence not BLOCKED — but the SIMD gap caps its
headline and the island-proliferation risk (R4) is the falsifiable boundary at which 6C fails and
6A/6D become the better family.

---

## FAMILY 6D — ORACLE-ISOMORPHISM (behavioral parity only; layout free)

**Architecture.** The rust/ts split is held equivalent by a shared conformance corpus and
differential tests ALONE — no mirrored trees, no generated skeleton, no single core. Layout is
free per language (LLVM-shaped Rust, V8-shaped TS). The isomorphism boundary is DATA (corpus + a
canonical language-neutral digest), never CODE. This is the **null hypothesis of the plane**: it
wins by default and is defeated only if measured drift/maintenance cost exceeds a linter's (6A)
or a generator's (6B).

**Mechanism (three coupled pieces + one offensive detector).**
- **Canonical form** — a total, deterministic, language-neutral value digest. Three realized
  forms ascending in generality: (a) canonical re-emission (LIVE:
  `json_canonical_parity.rs:105`, normalized byte-equality vs `sonic_rs`); (b) the offset-tape
  bytes (PROVE-14, node-for-node); (c) the recommended general form — a SAX-like typed EVENT
  stream over the grammar-derived Value alphabet, folded by a fixed endianness-neutral rolling
  hash (PROVE-14's `fp 986912547`). The hash folds the EVENT stream, never native memory, so Rust
  `u32` planes and V8 typed arrays digest alike. The canonical serialization can itself be
  grammar-defined (constitution i).
- **Differential runner** — one thin conformance runner per language (protobuf-conformance shape):
  read corpus input, parse via the idiomatic path, emit the digest; the harness diffs across the
  impl set (Rust eager · Rust stop-early `get` · Rust `parse_into<T>` · TS `TapeDoc` · a
  third-party reference so no impl is privileged). LIVE: `ts_node_execute.rs` already spawns real
  `node`, walks a path, compares byte-for-byte across four lanes with a no-silent-skip gate
  (`:249`).
- **Shared corpus** — stored once, language-agnostic. Fold in the value/css/csv corpora; ADD a
  JSONTestSuite-style accept/reject corpus partitioned **must-accept / must-reject / may-either**
  so implementation-defined cases (deep nesting, dup keys, number precision) don't flap the
  differential.
- **Grammar-directed differential fuzzing (the teeth)** — sample the bbnf grammar (the source of
  truth) to generate valid + near-miss inputs, feed both impls, diff digests; a mismatch is a bug
  in exactly one side. Csmith-for-grammars — nearly free because the generator is the grammar in
  hand. This converts the plane's one weakness (silent drift in un-exercised behavior) into an
  offensive detector.

**Integration seams.** LIVE: `json_canonical_parity.rs:105`, its number-canon handling at `:74-77`,
the shared normalizer `common/json_normalize.rs`, the real cross-language runner
`ts_node_execute.rs` (`:249/:303/:329`), the sibling parity tests
(`css_l4_canonical_parity`, `sheets_expr_parity`, `bbnf_self_parity`, `serialize_roundtrip`).
PLANNED: GN-4.W5's four-way `value_parity` + `cross-lane.mjs`. 6D's contribution is
**consolidation** — lift the scattered parity asserts into ONE corpus + ONE digest + ONE runner
set, and add the accept/reject corpus + the fuzzer.

**Gates it carries.** (i) 100% digest agreement across all lanes on the full corpus (reproduces
PROVE-14 parity as a *standing law*, not a one-shot). (ii) The behavioral spec DECLARES which
behaviors are in-contract (values, accept/reject) vs out (message wording, span encoding) —
under-specify → false confidence, over-specify → the maintenance cost that defeats 6D.
(iii) Number canonicalization pinned per grammar (`f64` Display vs `Number.toString` on `.0`,
`-0.0`, exponent, 17th digit). (iv) Digest folds an event stream, endianness-safe.

**Exact open questions (the verdict 6D owes the lock — both are MEASURED, not assertable).**
(a) **Drift-escape rate:** inject N deliberate divergences (escape off-by-one, dropped token
class, number-format delta, astral surrogate mishandle); measure the fraction caught by the
static corpus vs the grammar fuzzer, and cycles-to-detection. If corpus+fuzz catches ≈100% within
one cycle, drift cost → ~0 and 6D wins outright; any class that systematically escapes
(diagnostics, astral strings) is exactly where 6A/6B earn their keep. (b) **Marginal maintenance
cost per grammar:** add ONE new grammar (TOML), reach parity under corpus+digest only, record LOC
+ effort — 6D wins iff this is strictly below the hypothetical 6A/6B cost.

**Standing.** ADVANCE, and it is a **FLOOR under every other family** (behavioral parity is
mandatory infra regardless of the winning route — already installed). Substrate the most-LIVE of
the four. Prior art the settled SOTA (protobuf conformance, WPT, sqllogictest, JSONTestSuite,
CommonMark, and a published cross-language JSON-parser differential-testing paper on our exact
value plane). Not BLOCKED. Its one weakness (coverage gap) is precisely what its fuzzer must be
MEASURED to close — that measurement is the null-hypothesis test.

---

## THE INCOMPATIBILITY MAP (which families cannot coexist, and precisely why)

The families are not a flat XOR. The plane partitions into surfaces; incompatibility is
**per-surface**, and 6D is a floor beneath all.

1. **6C ⊥ 6A and 6C ⊥ 6B — over the collapsed surface only.** 6C has ONE implementation
   (Rust→wasm); there is no second hand-written (6A) or generated (6B) TS tree to hold isomorphic
   over the tape/value-fabric runtime. "Collapse to one impl" and "keep two impls isomorphic" are
   mutually exclusive on the same surface. BUT the conflict is *scoped*: 6C explicitly excludes
   parse-that combinators (separate repo, TS-native by decree) and full-materialization
   (TS-native island). Outside the generator's tape/value runtime the three do not contend. So:
   **mutually exclusive per-surface, not globally** — 6C answers a *narrower* question than 6A/6B
   and cannot be the whole-plane answer.

2. **6A ⊥ 6B — on the same surface.** 6A: both trees hand-authored, drift DETECTED by a linter
   (pay the linter tax, keep idiomatic freedom on that surface). 6B: the shared surface GENERATED
   from one spec, drift UNREPRESENTABLE (pay the generator tax, forfeit idiomatic freedom on the
   generated surface). A given surface item is either hand-written-and-linted OR generated — not
   both. They **partition-complement** (generate what is generable, lint the residue), but as
   competing CENTERS for the *same* surface they are exclusive. Crucially, the boundary between
   them — 6B's "generated fraction" vs "hand fraction" — is *the same boundary* 6A calls its
   "clean-map ratio," which is *the same boundary* 6C calls its "V8-native island cut." **All
   three families are measuring the identical seam and naming it differently; the lock's job is to
   locate that one seam with a number.**

3. **6D-as-CENTER ⊥ {6A, 6B, 6C} — decided empirically, by ONE measured number.** 6D-as-FLOOR is
   compatible with all (behavioral parity is installed infra). But 6D-as-CENTER (the null
   hypothesis: layout FREE, NO structural enforcement) contradicts the core claim of 6A/6B/6C that
   structural correspondence is worth enforcing. This incompatibility is **not a priori** — it is
   adjudicated by the drift-escape rate under corpus+fuzz: if ~0, structural enforcement is cost
   without a buyer (6D-center wins); if a class systematically escapes (the `$.statuses[0].nope`
   asymmetry proven LIVE in `isomorphic_path_error.rs:129-138`; astral strings; diagnostics), that
   class needs 6A/6B/6C and 6D-alone loses *there*. The `nope` counterexample is decisive: T1/T2
   structural checks pass green while behavior differs — proving structure alone is
   necessary-not-sufficient AND behavior alone (without a corpus covering that input) is blind. The
   two families are **complementary on different failure modes**, not globally exchangeable.

4. **Fixed constraints that bound every family (not inter-family, but they cap routes):**
   - **Regex-engine non-isomorphism (LOCK-5):** Rust bespoke DFA vs TS native `RegExp`. No family
     may mandate mirroring here — 6A allow-lists it, 6B does not generate a TS regex engine, 6C
     may wasm-core the logic but LOCK-5 governs, 6D is behavioral-only and unaffected.
   - **SIMD/NEON non-crossing (constitution iv, SPEC row 09):** NEON kernels do not cross; only
     the tape crosses. Caps 6C's headline (wasm-scalar or a distinct wasm32 v128 dialect); a
     refuted portable_simd→NEON shim proves no shared SIMD layer crosses at speed.

**The convergent shape the incompatibility map implies (for later passes, not a verdict):** the
real-world winners (tsc-go, FlatBuffers, oxc) are never a single family but a *layering* — 6D the
mandatory behavioral floor; 6B generating the mirror-able data/oracle surface from the grammar;
6A's linter enforcing hand-body correspondence up to the idiom seam; 6C confined to the
crossing/playground lanes where its measured win (partial access, 4.4×) actually lives. Whether
that layering collapses to one dominant center or stays a composite is the measured question the
next passes owe — and it turns on exactly two numbers: **is 6B's generation drift-impossible for
the enum/classify class, and does 6D's corpus+fuzz catch that same class cheaply if it is NOT
generated.** Those two experiments are opposed on the identical drift class and together collapse
the decision space.

---

## BLOCKED-candidate assessment (elegance earns nothing)

- **6A — not BLOCKED.** Substrate LIVE; owes the clean-map ratio (a measurement, not a missing
  primitive).
- **6B — not BLOCKED, closest to a structural stall.** Its prerequisite — a backend-NEUTRAL spec
  (the IR today carries Rust type strings; no `struct BackendIr` verified in-tree) — is a *subset*
  of the isomorphism problem, not equal to it. Convergence gates on that refactor + the
  drift-impossibility experiment.
- **6C — not BLOCKED, SCOPE-BOUNDED.** Inherently a *partial* collapse; the missing primitive
  (wasm SIMD = NEON) is one it explicitly does not need. Cannot be the whole-plane answer.
- **6D — not BLOCKED.** The floor; owes two measured numbers (drift-escape rate, marginal
  maintenance cost). Its coverage-gap weakness is a measurement target, not a missing primitive.

No family is a clean BLOCKED-candidate. The lock's remaining work is entirely *measurement*, which
is the constitution's own demand (iii: measured, not asserted; vii: concrete artifacts).
