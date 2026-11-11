# LOCK-6 / Family 6B — SPEC-GENERATED-SKELETON (research, pass 1)

*Isomorphism by construction where it is declarative; idiomatic freedom where it is hot.
A single machine-readable spec generates both languages' skeletons — layout constants, tape
decoders, type interfaces, oracle tables — and hand code fills only the perf-critical bodies.
Drift is not detected after the fact (6A's linter); it is made impossible for the generated
surface. The line where generation stops and hand-idiom begins is the deliverable, measured,
not asserted.*

---

## 1. The thesis, stated precisely

The rust/ts isomorphism problem has a declarative half and an imperative half.

- **Declarative half (drift-prone, low value-per-line, high correctness stakes):** layout
  constants (node-kind discriminants, flag-bit positions, offset stride/endianness, plane
  names), the byte contract of the offset-tape, type interfaces (`ParserState`, `Span`, the
  `Value` variant set), and conformance/oracle tables (the digest algorithm, the structural
  counts). These are *duplicated by hand today* across three faces — Rust `runtime/tape/
  record.rs`, the wasm-bindgen plane export, and the TS `tape.mjs` decoder — and every
  duplication is a silent-divergence hazard.
- **Imperative half (idiomatic, high value-per-line, V8/LLVM-law-bound):** the actual decode
  walk, the SIMD skip engine on the Rust side, the monomorphic hidden-class hot loop on the TS
  side. These *cannot* be generated without emitting non-idiomatic, deopt-prone code — the
  PROVE-14 `TapeDoc` is a hand-tuned ctor-only monomorphic class; a naively generated decoder
  would churn hidden classes and lose the 4.4× stop-early win.

6B's mechanism: **generate the declarative half from one spec; hand-write the imperative half;
draw and measure the boundary.** The generated surface has zero drift by construction (both
faces are outputs of the same generator run over the same spec); the hand surface is free to be
maximally idiomatic per language. This is strictly stronger than 6A's mirror-tree-plus-linter
on the declarative surface (impossible-vs-detected-after-the-fact) and strictly cheaper than
6C's wasm-core on the imperative surface (native-per-language, no wasm boundary tax on the hot
path). The null hypothesis 6B must defeat: that the declarative surface is too small for
generation to pay for its machinery. Section 6 sizes it.

---

## 2. The spec is the grammar — 6B is a completion, not a greenfield

The constitution (clause i) requires everything grammar-driven; the standing feedback forbids a
separate IR layer for TS ("AST IS the combinator") and mandates backend-agnostic types
(`TypeDesc::Named`, each backend resolves natives). 6B honours both because **the spec already
exists as `GrammarIR` + the shared driver's decisions — there is no new serialized artifact.**

bbnf-lang is *already a multi-backend generator built on exactly this shape*:

- **The shared decision spine.** `crates/core/src/backend/emitter.rs:31` — the `Emitter` trait
  (~60 `emit_*` methods, `type Output`, `type Ctx`). `crates/core/src/backend/driver/mod.rs:1`
  — the target-agnostic driver that "walks `GrammarIR`, makes target-agnostic structural
  decisions … and delegates target-specific code emission to the `Emitter` trait" (its own
  doc-comment). Decisions are made *once*; `compile_*` decides, `emit_*` renders idiom.
- **Three faces already ride it.** `backend/rust/`, `backend/wasm/`, `backend/ts/` — with
  `CompileTarget::{Rust, Ts, Wasm}` at `crates/core/src/pipeline.rs:33-41` and
  `compile_grammar_request(grammar, target)`. The TS emitter already emits `interface
  ParserState { input; offset; … }` and `interface Span { start; end }` from the grammar
  (`crates/core/tests/backend_ts.rs`).
- **An isomorphism gate already exists.** `crates/core/tests/backend_ts_typecheck.rs` emits TS
  for JSON/CSS-L4/Sheets/BBNF into a tempdir and runs `tsc --noEmit --strict` — the generated
  TS is proven build-correct across four grammars from one generator run.
- **A literal 6B instance already ships.** `crates/bbnf-path/` (Rust proc-macro) and
  `crates/bbnf-path-ts/` (cdylib + wasm-bindgen, `ts/path.ts`) are a mirrored pair whose
  "segment alphabet is isomorphic across Rust + TS frontends" *because both consume the same
  `bbnf-ir` `StructRegistry`/`StructLayout` types and the same `bbnf-regex` `lex_path`/
  `PathToken` lexer* (`crates/bbnf-path-ts/Cargo.toml` header). The isomorphism is by
  construction, not by convention. This is the template 6B generalizes to the tape plane.

So the family's real work is **extending the existing `Emitter`/driver to cover the three
declarative faces it does not yet generate** — the tape-layout contract, the tape decoder
skeleton, and the oracle table — and proving the generated output is idiomatic enough that the
hand half shrinks to the measured minimum.

---

## 3. The concrete mechanism (three generated faces, one hand face)

### 3a. Face I — the layout contract (pure generation, zero hand code)

**The drift exemplar, live in-tree right now.** Two incompatible tape dialects coexist in the
campaign:

- The greenfield/current `crates/core/src/runtime/tape/record.rs` is **AoS**: `TapeKind{Open=0,
  Leaf=1}` (packed in `kind_meta` low nibble, `record.rs:4-24`), `LeafType{F64=0,I64=1,U64=2,
  Bool=3,Str=4,Unit=5}` (`record.rs:31-59`), `STRING_BORROW_BIT=0x8000` (`:63`),
  `FLAG_HAS_CHILDREN=1<<6`, `FLAG_META_IDX_HI=1<<7`, `FLAG_BRANCH_MASK=0x3F`, `TAPE_NONE=u32::
  MAX` (`:19,66-91`).
- The PROVE-14/skinny SoA **offset-tape** the GN-4.W4 seed decodes is a *different* alphabet:
  `K_OBJ_OPEN=1 … K_NULL=10` (kind re-derived from `source[offset]`, no stored discriminator),
  `STRING_NEEDS_DECODE=0x01 = OffsetFlags::GRAMMAR_BIT0`, `u32` LE offset stride, four planes
  `offsets/flag_cursors/flag_values/source` (PROVE-14 REPORT §"The layout that crossed";
  `tape.mjs:20-40`).

These constants are hand-authored in Rust *and* hand-transcribed into `tape.mjs` — the precise
divergence hazard 6B abolishes. GN-1 must reconcile the two dialects when it ports the skinny
tape; 6B makes that reconciliation *definitional*: one spec, N faces regenerated in lockstep.

**Generation.** The node-kind alphabet is *derivable from the grammar*, not a hand list: the
kinds are the grammar's structural node kinds; `STRING_NEEDS_DECODE` is a grammar-derived fact
(does this string rule admit escapes / non-ASCII), reachable from `bbnf-ir`'s materialization
analysis (`crates/ir/src/passes/materialization.rs`, `MaterializationClass`, re-exported at
`passes/mod.rs:49`; already a CSP variable, `csp_strategy/mod.rs:252-261`). The `Emitter` gains
one method — `emit_tape_layout(&LayoutSpec, ctx)` — implemented per backend: Rust emits `const
K_OBJ_OPEN: u8 = 1;` etc. into `runtime/tape/record.rs`; wasm emits the plane-export names +
strides; TS emits the `const K_OBJ_OPEN = 1;` block at the head of `tape.ts`. Change one spec
line, `xtask regen`, and all three faces move together. This face is **100% generated, 0 hand
lines** — and it is where drift actually lives.

### 3b. Face II — the type interfaces (generation, already partly built)

The `Value` enum variant set (Object/Array/String/Number/Bool/Null, drawn from GN-3.W1
`DirectTypeKind` sum types), the `ParserState`/`Span` interfaces, and the `TypedPath` shape are
backend-agnostic `TypeDesc` today and already emitted for TS (`backend_ts.rs`, `bbnf-path-ts/
src/schema.rs`). 6B's addition is the *tape-node* interface (`TapeDoc` field declarations,
method signatures) emitted as a **skeleton** whose bodies are hand-filled (3d). Emitting the
field decls in constructor-fixed order is the load-bearing detail — see the V8 risk in §5.

### 3c. Face III — the oracle table (generation, the highest-leverage new face)

GN-4.W4/W5 need a *four-lane parity oracle* (`value_parity.rs` + the TS side) that computes the
same digest — the FNV-1a walk (`strHash`, `tape.mjs`; `mix`, `pointer.mjs`) and the six
structural counts + `codeUnits` + 32-bit fingerprint (PROVE-14 REPORT §"Byte-exact parity").
Today that digest algorithm is hand-written twice (Rust walk ≡ `walkJs`). 6B generates the
oracle from the spec into *both* a Rust `#[test]` fixture and a TS `test/tape-parity.test.ts`,
so "the same value, node-for-node" is true *by construction of the comparator*, not by careful
hand-mirroring. This directly de-risks GN-4.W5's named counterexample R5 ("oracle stubs itself
— `get == get`") and CW's "real four-way check or a self-compare?": a generated comparator
cannot accidentally compare a lane to itself because the four lane-entries are distinct
generated call sites over one generated digest.

### 3d. The hand face — the decode hot loop (never generated)

The imperative walk stays hand-written, idiomatic per language, riding the generated skeleton:
Rust's NEON skip engine (`skip_container`, GN-4.W1) and TS's `TapeDoc.walk`/`getKey`/
`indexArray`/`stringLeaf` monomorphic integer-cursor recursion (`tape.mjs`, ctor-only fields,
allocation-free). The generator emits the *shape* it must satisfy (ctor field order, const
block, method signatures, the `nextSibling` sparse-flag binary-search contract) and the human
writes the body. **This is the "reasonable point": the boundary is drawn at the declarative /
imperative seam, and §6 measures where it lands.**

---

## 4. Integration seams (path:line)

- `crates/core/src/backend/emitter.rs:31` — the `Emitter` trait: add `emit_tape_layout`,
  `emit_tape_decoder_skeleton`, `emit_oracle_table` alongside the ~60 extant `emit_*` methods.
  Each backend (`rust/`, `wasm/`, `ts/`) implements them idiomatically.
- `crates/core/src/backend/driver/mod.rs:1` — the shared driver: the `LayoutSpec` is a
  driver-level decision (target-agnostic), computed once from `GrammarIR` +
  `MaterializationClass`, then handed to every `emit_*`. Layout choices live here, never per
  backend.
- `crates/core/src/backend/ts/mod.rs`, `ts/emitter/grammar.rs:1` — the idiomatic-body home;
  generated tape skeleton lands here; hand bodies fill it.
- `crates/core/src/backend/wasm/` — the plane-export face (the wasm-bindgen boundary GN-4.W4's
  `bbnf-wasm` cdylib needs: `TapeExport{offsets,flag_cursors,flag_values,source}`).
- `crates/core/src/runtime/tape/record.rs:4-91` — the current AoS constants; these become
  **generated output**, and the AoS↔SoA reconciliation with skinny's offset-tape becomes a
  spec edit rather than a two-file hand-sync.
- `crates/ir/src/passes/materialization.rs` (`MaterializationClass`) + `crates/ir/src/passes/
  mod.rs:49` — the grammar-derived source of `STRING_NEEDS_DECODE`-class facts; the spec draws
  its flag bits from here (satisfies constitution clause i — the layout is grammar-driven, not
  a hand constant table).
- `crates/bbnf-path/` + `crates/bbnf-path-ts/` — the **existing 6B proof**: a Rust+TS mirror
  isomorphic-by-shared-IR. Generalize its pattern (shared `bbnf-ir` types + shared `bbnf-regex`
  lexer ⇒ isomorphic frontends) from the path plane to the tape plane.
- `crates/core/tests/backend_ts_typecheck.rs` — the extant isomorphism gate (`tsc --noEmit
  --strict` over generated TS for four grammars); extend with the generated tape-parity oracle
  and a V8 same-map probe (§5).
- `crates/core/src/pipeline.rs:33` — `CompileTarget`; the new faces register as emission
  outputs, not new targets.
- PROVE-14 `crates/bbnf-bench/ts-tape/{tape,load,pointer}.mjs` — the hand decoder that 6B
  splits into {generated consts + skeleton} ⊕ {hand hot bodies}, with the split line recorded.

---

## 5. Prior art (with citations to the SOTA)

The "one spec → isomorphic per-language skeletons over a fixed byte layout, hand-write the
logic" pattern is a settled discipline; 6B is its application to the tape plane.

- **Cap'n Proto / FlatBuffers (Kenton Varda; Google FlatBuffers).** A `.capnp`/`.fbs` schema
  compiles to *zero-copy accessors* in every target language over a **byte-exact wire layout** —
  the canonical exemplar of layout-drift-impossible-by-construction across a language boundary.
  FlatBuffers' generated accessors over a flat buffer are structurally isomorphic to PROVE-14's
  typed-array views over the offset-tape planes. Directly transferable: the schema compiler
  generates the offset/stride math; the human never re-derives it per language.
- **Kaitai Struct (`.ksy`).** A declarative binary-format spec compiles to parsers in Rust, JS,
  Python, C++, … — the single-spec binary-decoder generator, the closest analog to generating
  the tape decoder skeleton in both Rust and TS from one layout description.
- **LLVM TableGen (`.td`) + MLIR ODS.** Instruction/register tables and MLIR op classes +
  verifiers + docs are *generated* from one declarative spec while the pass logic stays
  hand-written C++ — precisely 6B's "generate the tables/skeleton, hand-write the logic," and
  the source of the "reasonable point" discipline (TableGen deliberately does not generate the
  imperative selection logic).
- **serde derive (Rust).** `#[derive(Serialize, Deserialize)]` generates two isomorphic
  skeletons from one struct — the in-language proof that generation eliminates the hand-mirror
  entirely for the declarative surface; bbnf's `bbnf-path` macro is already this pattern.
- **wit-bindgen / the WebAssembly Component Model (WIT).** One interface spec generates typed
  bindings + marshalling into Rust *and* JS — the shared-interface-generates-both-faces pattern
  at the exact rust↔ts/wasm boundary GN-4.W4 crosses. (6B uses it as a *skeleton* generator, not
  as 6C's full wasm-core — the hot body stays TS-native.)
- **tree-sitter `grammar.js` → `node-types.json`.** The grammar generates a *machine-readable
  tree-shape spec* consumed by every language binding; bindings are thin and isomorphic by
  construction. This is bbnf's own model — the grammar is the shared spec — and validates
  clause (i): the spec need not be a new artifact, the grammar already is one.
- **protobuf / gRPC.** `.proto` → per-language messages + the wire format is the industry-scale
  proof that a schema compiler holds N languages in byte-exact agreement across decades of
  drift pressure.

---

## 6. What a prototype must test to move family 6B

The family advances only on measured artifacts (constitution vii). A prototype must:

1. **Prove lockstep regen.** Author the `LayoutSpec` (node-kind alphabet + flag bits derived
   from grammar `MaterializationClass`) and emit its three faces (Rust `record.rs` consts, wasm
   plane export, TS `tape.ts` const block). Change ONE spec line (add a node kind / flip a flag
   bit), `xtask regen`, and show all three faces move together while PROVE-14's byte-exact
   parity (`verify.mjs`: 6 structural counts + `codeUnits` + fp 986912547) still passes.
   **Falsifier:** a drift hand-injected into `tape.ts` fails a generated-const assertion in the
   parity oracle.
2. **Prove the generated oracle cannot self-stub.** Generate the FNV digest comparator + the
   structural-count table into both a Rust `value_parity.rs` fixture and a TS
   `tape-parity.test.ts`; show the four lane-entries are four distinct generated call sites over
   one generated digest. **Falsifier:** GN-4.W5's R5 counterexample (`get == get`) becomes
   *unwriteable* — the generator emits four distinct lane calls; a hand edit to collapse them
   fails a generated-arity check.
3. **MEASURE the boundary (the deliverable).** Emit the TS `TapeDoc` skeleton (ctor-only field
   decls in fixed order, const block, method signatures) and hand-fill only the walk/decode
   bodies. Gate on (a) `backend_ts_typecheck.rs` `tsc --noEmit --strict` GREEN, and (b) a V8
   `%HaveSameMap` probe (per the PT-3.W4 shape-oracle method, `node --allow-natives-syntax`)
   proving the generated skeleton is single-hidden-class shape-stable and the hand body keeps it
   so. **Falsifier:** the generated skeleton churns a hidden class (breaking PROVE-14 VERIFY
   attack 8's monomorphism invariant) or fails `tsc`.
4. **Quantify "reasonable."** Report generated-LOC vs hand-LOC per face across JSON + one more
   grammar (CSS-L4 or Sheets). The measured plateau — where generated coverage saturates and
   hand-idiom dominates — IS the "reasonable point" the constitution (clause iii) demands be
   measured, not asserted. Compare against 6A's linter cost (lines of drift-detection + false-
   positive rate) to defeat or confirm the null hypothesis.

---

## 7. Named risks and counterexamples

- **R1 — the generator produces V8-hostile TS.** The hot body demands a ctor-only monomorphic
  class (R4 §6a-d: `ParserState` field churn, `mapState`'s `Object.create` deopt, the
  megamorphic `.parser` IC). If the generated skeleton emits field-by-field init or post-ctor
  mutation, it breaks the single-hidden-class invariant and loses the 4.4× stop-early win.
  *Counterexample:* a generated `TapeDoc` with fields assigned outside the constructor →
  `%HaveSameMap` FALSE. *Mitigation:* generate ONLY the declarative skeleton (fixed-order field
  decls + const block + signatures); the imperative body is hand-written and shape-audited
  (prototype test 3). The generator is V8-shape-aware for decls, nothing more — this is exactly
  why the body is NOT generated (the boundary is principled, not a shortcut).
- **R2 — over-engineering a separate spec artifact.** The "no-ts-ir / AST-IS-the-combinator"
  law and parsimony forbid a heavyweight serialized `.spec`. *Counterexample:* a hand-authored
  YAML/JSON tape-schema duplicating what `GrammarIR` already holds — a new god-artifact.
  *Mitigation:* the spec IS `GrammarIR` + `MaterializationClass`; `LayoutSpec` is a *derived
  view*, computed in the driver, never hand-written or serialized to disk. Face I is grammar-
  derived (clause i), not a constant table.
- **R3 — the declarative surface is too small to pay for the machinery (the null hypothesis).**
  If only ~40 constants and two interfaces are generated, a 6A mirror-linter might be cheaper.
  *Mitigation:* prototype test 4 sizes it; the oracle table (Face III) and the AoS↔SoA
  reconciliation are the surfaces that tip the balance — the digest comparator + four-lane
  entries + layout constants across Rust/wasm/TS is a non-trivial, correctness-critical, drift-
  prone body. If the measurement shows it is small, 6B honestly reports the null hypothesis
  wins for the tape plane and narrows to the oracle face alone.
- **R4 — generated code fails idiom review even when `tsc`-clean.** `backend_ts_typecheck.rs`
  proves build-correctness, not idiom or V8-shape. *Counterexample:* TS that typechecks but
  allocates per node. *Mitigation:* the gate is extended with the `%HaveSameMap` shape probe
  AND the paired cold bench (PROVE-14 `bench.mjs` method, N≥200 cold children) — generation is
  accepted only if the generated-skeleton + hand-body composite reproduces the 4.4× stop-early.
- **R5 — cross-repo placement law.** The TS decoder seed lands in bbnf-lang's greenfield
  (GN-4.W4 `ts/tape/`), but the parse-that combinator backbone (PT-3) is READ-ONLY to the bbnf
  campaign and landed only by the owner / a parse-that-side session. *Mitigation:* 6B's
  generated faces target the *bbnf-side* GN-4.W4 seed and its wasm boundary; any parse-that-side
  generation rides letter BBNF-PT-G, never a direct write. The two are complementary, non-
  gating (GN-4 §2 interlock / PT-3 §2).
- **R6 — AoS/SoA reconciliation lands wrong under generation.** Generating from a spec that
  still encodes the AoS `TapeRec` dialect while the tape crossed is the SoA offset-tape yields a
  byte-exact-but-wrong export (PROVE-14 R4: endianness/stride mis-align yet ASCII-only leaves
  pass by coincidence). *Mitigation:* the born-RED cross-check (GN-4.W4 gate b) asserts the
  generated wasm planes are byte-identical to the `tape_dump` fixture — a generated layout that
  drifts from the measured layout fails before JSON-parity can mask it.

---

*6B — one spec, both skeletons, drift impossible where it is declarative. The generator bbnf
already is grows three faces — layout contract, decoder skeleton, oracle table — over its
existing `Emitter`/driver spine; the hot body stays hand-idiomatic and shape-audited. The line
between the two is measured (§6.4), and that measurement is the family's verdict.*
