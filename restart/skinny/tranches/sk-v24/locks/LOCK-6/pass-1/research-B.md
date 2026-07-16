# LOCK-6 pass-1 — family 6B: SPEC-GENERATED-SKELETON

## Thesis

A single machine-readable, grammar-driven spec generates BOTH the Rust and the TypeScript
skeletons — the surfaces that must never diverge (type declarations, tape-plane layout, the
node classifier, oracle/conformance vectors) — while hand code fills the idiomatic bodies (the
NEON skip kernels on the Rust side; the V8 monomorphic hot walk on the TS side). Where a
surface is generated, cross-language drift is not merely detected but *unrepresentable*: a
grammar edit regenerates both trees at once, and the regen fixpoint is the proof.

The family's central claim is that the isomorphism is enforced at the single choke point of
generation rather than after the fact. A mirror-tree linter can only flag drift once someone
has already hand-written the divergent line; a behavioral differential test catches drift only
when a corpus happens to exercise the diverged path. Generation from one spec removes the
drift class from the representable space for every surface it covers. The measured question the
family answers is therefore quantitative: *what fraction of the cross-language surface is
generated (drift-impossible) versus hand (drift-possible-but-idiomatic)*, and the boundary
between them is the "reasonable point" the constitution asks LOCK-6 to locate by measurement.

## The mechanism made concrete: the spec already exists

The decisive move is that the "shared machine-readable spec" is not a new hand-authored IDL —
authoring one would violate grammar totality (constitution i). The spec is the compiler's
existing backend-agnostic middle IR, already `serde`-serializable, already grammar-derived:

- **`BackendIr`** — `crates/ir/src/lib.rs:352` — `#[derive(Serialize, Deserialize)]`, carrying
  `grammar_name`, `entry_rule`, `recognizers: Vec<Recognizer>`, `rules: Vec<BackendRule>`,
  `shape_facts`. Its `BackendShape` (`:361`: `EagerTape | OffsetTape | EventTape | SinkOnly |
  CollapsedStage`) names the value lane; its `BackendExpr` (`:376`) is the structural node set:
  `TapeEmit { kind: TapeKind }`, `DirectBuild { shape, fields: Vec<DirectBuildField> }`,
  `RegexProgram { pattern, span_kind }`, `SpanMark`, `Seq`, `Alt`, `RepeatLoop`,
  `ValueProject { target, ty }`, `CallRule`. This is a total, serializable description of what
  the parser builds — the exact object a TS emitter needs.
- **`DirectSchemaSet`** — `crates/codegen/src/direct_schema.rs` — the grammar-derived type
  spec (the HM core, PROVE-08's 221 LOC, validated in ~230µs, ZERO runtime cost). GN-3.W2/W3
  make it authoritative and delete the hand schema `xtask/src/real_typed_schema.rs`. This is
  the type half of the spec.
- **The tape SoA layout facts** (PROVE-14 REPORT lines 53-99, VERIFY §parity): offsets are
  `u32` LE byte-offsets, one per structural token; `flag_cursors` is a sorted `u32` side
  channel, `flag_values` is `u8` with `STRING_NEEDS_DECODE = 0x01 = GRAMMAR_BIT0`; kind is
  re-derived from `source[offsets[cursor]]` (no stored discriminator); separators `,`/`:`
  never receive an offset (`consume_delimiter` advances without emit). PROVE-14 PROVED these
  cross byte-exactly — node-for-node, 6 structural counts + codeUnits + 32-bit fingerprint
  identical across 320 cold children (VERIFY: `fp 986912547`, partial `fp 3431570937`). The
  layout is thus a language-neutral byte contract *by measurement*, not by assertion.

The Rust emitter is already the first consumer of this spec: `codegen::emit(backend:
&BackendIr) -> EmittedSource` (`crates/codegen/src/lib.rs:114`) walks `BackendIr` and writes
Rust source strings into an `EmittedSource` file-map (`lib.rs:63`, `files: BTreeMap<String,
String>`, `write_to_dir`). The concrete templates it drives: `runtime_generator.rs` (522 LOC),
`json_templates/view.rs` (488), `json_typed_direct.rs` (1245). Family 6B introduces a *second*
consumer of the identical `BackendIr` + `DirectSchemaSet`: a TS-skeleton emitter producing a
parallel `EmittedSource` whose files are the drift-prone TS surface.

## The substrate: what generates, what is hand-filled

Generated (drift-impossible), from `BackendIr` + `DirectSchemaSet` + tape-layout facts:

1. **TS type declarations** — `DirectSchemaSet` → `.d.ts`-style interfaces, the exact mirror of
   the Rust `DirectBuild` structs. Numeric widths resolve per-backend (see R1): `u64/i64` →
   `bigint`, `f64` → `number`, per the backend-agnostic-types law (types are abstract
   `TypeDesc::Named`; each backend resolves natively).
2. **The tape-decoder skeleton** — from the layout facts: the SoA plane struct (offsets/flag
   planes and their element types), the node classifier as a *data table* (source-byte →
   `JsonNodeKind`, emitted as an `Int8Array(256)` literal, not hand-transcribed control flow),
   the sparse-flag binary-search over `flag_cursors`, and the `offset_at`/`nextSibling`/pointer
   navigation shape over `offsets`.
3. **The oracle / conformance tables** — the shared corpus digests and structural-count
   expectations (PROVE-14's `objects/arrays/strings/numbers/bools/nulls/codeUnits/fp`), emitted
   ONCE and consumed by both `crates/*/tests` and `ts/tape/test`. One source of truth for the
   differential vectors both sides check against.
4. **The wasm boundary signature** — the `#[wasm_bindgen]` `TapeExport { offsets: Uint32Array,
   flag_cursors: Uint32Array, flag_values: Uint8Array, source: Uint8Array }` (GN-4.W4's
   `crates/bbnf-wasm/`) is itself derivable from the four-plane layout facts, so the FFI glue
   cannot drift from the layout it exports.

Hand-filled (idiomatic, measured, drift-tolerable because it is per-language perf, not shared
structure):

- **Rust**: the NEON skip kernel (`skip_container`, `skip_tail`, the movemask-narrow block
  emit) — the GN-4.W1/W2 hot loop over `scan.rs`.
- **TS**: the monomorphic `TapeDoc` walk body — the byte-hash ASCII fast path that lifted
  PROVE-14 from 0.66× to 0.98× full-walk, and the stop-early `nextSibling` descent that wins
  4.4×. Per R4 §6 this body MUST keep its V8 discipline (four ctor-only fields = one hidden
  class, `Int8Array(128)` dispatch, no post-ctor mutation); the generator supplies the classify
  *table* it reads, never the class *shape* it lives in.

The generated/hand boundary coincides exactly with the structural/performance boundary, which
coincides with the isomorphism/idiom boundary — this is the family's elegance and its measured
answer to "where does reasonable end." PROVE-14 is itself the proof such a boundary exists: the
layout crosses byte-exactly (generate it), but the full-walk hot loop is where V8 laws diverge
from LLVM laws (hand-write it; the TS full-walk stays a documented non-goal at ~0.9×).

### Consumer-less-substrate discharge (constitution viii / Y.13)

The spec `BackendIr` already has a consumer (the Rust emitter), so it is not dangling
substrate. The new TS-skeleton emitter's output is consumed IN the same wave: the generated
oracle vectors are read by `ts/tape/test/tape-parity.test.ts`; the generated classify table and
layout constants are read by the hand `TapeDoc` body; the generated wasm signature is read by
the `bbnf-wasm` bindings. No generated artifact lands dark.

## Integration seams (path:line; skinny is source-of-truth, ports to greenfield `crates/`)

- Spec (the IR): `crates/ir/src/lib.rs:352` (`BackendIr`, serde), `:361` (`BackendShape`),
  `:376` (`BackendExpr` incl. `TapeEmit`, `DirectBuild { shape, fields }`, `ValueProject { ty }`),
  `Recognizer::SimdScan` (the SIMD-site facts, for lane annotation).
- Rust emitter to mirror: `crates/codegen/src/lib.rs:114` (`emit(&BackendIr) -> EmittedSource`);
  the file-map substrate `lib.rs:63` (`EmittedSource { files: BTreeMap<String,String> }`,
  `write_to_dir`); templates `runtime_generator.rs` (522), `json_templates/view.rs` (488),
  `json_typed_direct.rs` (1245). The TS emitter is a sibling module (`codegen/src/ts_skeleton/`,
  directory-module per the colocation edict) emitting into a second `EmittedSource`.
- Type spec: `crates/codegen/src/direct_schema.rs` (`DirectSchemaSet`), authoritative after
  GN-3.W2/W3 (hand schema `xtask/src/real_typed_schema.rs` deleted; grep-gate `CLEAN`).
- Tape layout / what crosses: `crates/runtime/src/tape/mod.rs:120-137`
  (`source()/offsets()/flag_cursors()/flag_values()`, `offset_at`, `flags_at` binary-search);
  plane semantics proven in PROVE-14 REPORT:53-99.
- TS decoder origin (parts to reclassify): `bbnf-bench/ts-tape/{tape.mjs(364), load.mjs(34),
  pointer.mjs(34), verify.mjs(55), bench.mjs(86)}` (PROVE-14, 703 LOC). `load.mjs` (layout),
  the classify + sparse-flag search inside `tape.mjs`, and `verify.mjs` (oracle) move to the
  generated fraction; the `tape.mjs` byte-hash fast path + `pointer.mjs` descent stay hand.
- Wasm boundary: GN-4.W4 `crates/bbnf-wasm/src/tape_export.rs` (`TapeExport`) — its signature
  generates from the four-plane facts.
- Regen host & fixpoint: `xtask` (one regen host, GN-1). The TS-skeleton emit is a new regen
  target under the same fixpoint gate as the Rust regen (LOCK-7 7A: regen∘regen byte-identical);
  generated files are regen-only (clean-regen discipline), never hand-patched.
- Downstream landing target: GN-4.W4's file plan (`ts/tape/src/{tape.ts, load.ts, pointer.ts}`,
  `test/tape-parity.test.ts`) is where the generated skeleton + hand body compose; 6B recasts
  W4's "adaptation: .mjs → typed .ts (hand port)" into "generate the layout/classify/oracle,
  hand-fill the walk."

## Prior art (citations)

- **LLVM/MLIR TableGen** (`.td` → C++): the canonical one-spec-many-consumers generator — op
  definitions, verifiers, docs, rewrite patterns all emitted from one record file; hand C++
  fills custom bodies. The architectural north star for 6B: generate the drift-prone surface,
  hand-fill the logic. 6B's variant replaces the hand `.td` with the grammar-derived `BackendIr`.
- **serde-reflection / serde-generate** (Facebook): reflects Rust types into a
  language-neutral registry, then generates TS/Python/C++/Java type definitions AND
  (de)serializers. This is nearly exactly 6B's type half (`DirectSchemaSet` → TS interfaces);
  the closest existing prior art and a proof the type-declaration generation is tractable.
- **Cap'n Proto / FlatBuffers**: schema → generated multi-language accessors over a fixed
  zero-copy wire layout; cross-language parity holds because the layout is the spec and the
  accessors are generated. Directly analogous to the offset-tape crossing; the distinction is
  that those hand-author an IDL, whereas 6B's layout spec is the compiler's own tape facts
  (grammar-derived).
- **tree-sitter**: grammar → generated C tables (`parser.c`) plus a stable `node-types.json`
  consumed by every language binding; bindings stay thin, tables generated. The precise
  analogue: grammar → generated node-kind spec + decode tables consumed by both Rust and TS.
- **Kaitai Struct** (`.ksy` → parsers in ~11 languages): the purest one-spec-N-language binary
  decoder generator. Direct prior art for the tape-decoder half; its limitation — declarative
  only, no hot-path control — is exactly why 6B splits generate-structure from hand-perf.
- **wasm-bindgen / ts-bindgen / cbindgen**: generate FFI/TS glue from Rust annotations;
  GN-4.W4 already uses wasm-bindgen for the `TapeExport` boundary. 6B extends the same mechanism
  past the FFI line to the decoder skeleton and oracle.
- **regex-automata** serializable DFAs (`Automaton::to_bytes`/`from_bytes`): a compiled
  automaton crosses as a byte spec decoded identically on the far side — the pattern of
  "serialize the compiled artifact, decode it identically" that the tape embodies.

## Named risks and counterexamples

- **R1 — the spec is Rust-shaped, not neutral.** `DirectBuild { shape: String, fields }` and
  `ValueProject { ty: Option<String> }` carry Rust type-name strings; `RegexProgram { pattern }`
  is a Rust-side pattern. Counterexample: a `ty = Some("u64")` field generates TS `number`, but
  values >2^53 silently lose precision — the generated TS type is lossy and the oracle passes on
  small-int corpora. Mitigation: the type spec must carry ABSTRACT widths (backend-agnostic-types
  law; the numeric-width facts already exist in the type plane, PROVE-08/GN-3), and the TS
  backend resolves `u64/i64 → bigint`. Prototype must exercise a wide-int field and prove
  `bigint`, not `number`.
- **R2 — generated TS fights V8.** If the generator emits the `TapeDoc` *class* (getters,
  method-assigned fields) it breaks the R4 §6 monomorphism (single hidden class, ctor-only
  fields). Counterexample: a generated getter or a post-ctor field write forces a hidden-class
  transition → deopt → the 4.4× stop-early win collapses to parity. Mitigation: the boundary is
  strict — generate the classify TABLE (an `Int8Array` literal) and layout CONSTANTS as data;
  hand-write the monomorphic class body. Prototype must re-measure stop-early ≥4× on the
  generated-table + hand-body decoder, not merely prove parity.
- **R3 — endianness / stride drift in the generated layout.** A zero-copy `Uint32Array` view
  can mis-align (endianness, stride) and still "parse" ASCII-only leaves (named in GN-4.W4 R4).
  Counterexample: native-endian read on a differing host, JSON-parity passes by coincidence.
  Mitigation: the generated layout carries explicit LE + stride assertions; the generated oracle
  asserts wasm planes byte-identical to a Rust `tape_dump` fixture (the born-RED cross-check),
  not merely `JSON.parse` parity.
- **R4 — the generated fraction is trivial; drift lives in the hand fraction.** If the classify
  and walk stay hand (because perf-sensitive) and only type declarations generate, then the most
  drift-prone surface — the classifier when a new node kind appears — is hand on both sides, and
  6B degenerates into a linter with extra machinery. Counterexample: a new `JsonNodeKind` variant
  is added to the grammar; Rust classify regenerates, TS classify is hand-edited → drift. This is
  the family's decisive boundary: the node-kind enum and the classify table MUST be in the
  generated fraction. Prototype must add a node kind and show zero hand edits on either side.
- **R5 — bidirectional fixpoint staleness.** The regex grammar is bootstrapped (constitution i);
  if the skeleton spec is itself grammar-generated, the fixpoint must span the TS emit too.
  Counterexample: a grammar edit changes the Rust skeleton, but the TS regen target is not wired
  into `xtask`, so TS silently lags one commit. Mitigation: the TS-skeleton emit is a first-class
  `xtask` regen target under the same fixpoint gate (LOCK-7 7A); CI fails RED if TS regen is
  dirty. regen∘regen must be byte-identical for BOTH output trees.

## What a prototype must test to move this family

1. **Two-consumer emit.** Extend `codegen` with a `ts_skeleton` emitter consuming the SAME
   `BackendIr` + `DirectSchemaSet` the Rust emitter consumes, producing a second `EmittedSource`
   with (a) TS interfaces, (b) the classify table + SoA layout constants, (c) the shared oracle
   vectors. Prove the spec round-trips through serde `BackendIr` (it is genuinely the middle IR,
   not a fork).
2. **Drift-impossibility (the family-deciding experiment).** Add one JSON node kind (or one flag
   bit) to the grammar; regen; show BOTH Rust and TS classify/type surfaces change with ZERO
   hand edits and the oracle regenerates its expected vectors. Any surface needing a hand edit is
   named as outside the drift-impossible boundary — that boundary IS the family's measured result.
3. **V8 preservation.** Build `TapeDoc` as generated-table + hand-monomorphic-body; re-measure
   PROVE-14 stop-early — must reproduce ≥4× (not parity), proving generation did not perturb the
   hidden-class discipline. Full-walk stays the documented ~0.9× non-goal.
4. **Width fidelity.** A grammar with a `u64`/`i64` field: prove generated TS resolves `bigint`,
   Rust `u64`, and the oracle catches a lossy `number` regression (R1).
5. **Cross-language fixpoint.** Wire the TS emit as an `xtask` regen target; prove regen∘regen is
   byte-identical for both output trees (the LOCK-7 fixpoint extended across the language line).

### The measure the family must report

The isomorphism "reasonable point" is quantitative: of PROVE-14's 703-LOC TS decoder, the
generable fraction is `load.mjs` layout (34) + the classify/sparse-flag portion of `tape.mjs` +
`verify.mjs` oracle (55) + generated type interfaces ≈ 120-160 LOC; the hand fraction is the
`tape.mjs` byte-hash walk + `pointer.mjs` descent ≈ 200 LOC. That ratio — generated-structure
over hand-perf — is the concrete, measured boundary this family exists to establish, and the
prototype's job is to make it real and defend it against R2/R4.
