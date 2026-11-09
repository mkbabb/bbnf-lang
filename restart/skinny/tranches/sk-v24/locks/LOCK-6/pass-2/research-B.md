# LOCK-6 · Family 6B — SPEC-GENERATED-SKELETON (pass-2 research)

A shared machine-readable spec — grammar-derived, per constitution clause (i) — emits BOTH the
Rust and TS skeletons: the type face, the tape-decoder layout face, the classify table, and the
oracle/config constants. Hand code fills only the idiomatic walk bodies. Where a fact is emitted,
drift between the two languages is impossible by construction. This report makes the mechanism
concrete, bounds the declarative surface in-tree (the null-hypothesis test), separates
type-generation from layout-generation as two distinct claims, cites the exact multi-language
codegen prior art, and names what a prototype must falsify.

---

## 1. The mechanism, made concrete

The generator already emits a per-grammar **skeleton of exactly 8 Rust files** —
`config.rs, generated.rs, host.rs, mod.rs, parser.rs, value.rs, view.rs, visitor.rs` — enumerated
as a compile-time roster at `crates/codegen/src/grammar_profile.rs:3-12`, with sink-only and
general-descent variants at `:14-33`. This roster IS the skeleton. Family 6B's move is to make the
emitter **language-parametric**: from one `GrammarProfile` + `DirectSchemaSet`, emit the Rust
roster it emits today AND a mirror TS roster (`config.ts`, the tape decoder, the classify table,
the interface). The generator is the single source; both language skeletons are its outputs; the
seam that today stops at Rust extends one language further.

Three concrete generation targets, in ascending leverage:

- **The config/layout-fact face.** `crates/codegen/src/json_templates/config.rs` holds the entire
  machine-derivable constant set for JSON: `STRUCTURAL_BYTES = b"{}[],:\""`,
  `STRING_NEEDS_DECODE = OffsetFlags::GRAMMAR_BIT0`, `TINY_STRING_CAP=16`,
  `DIRECT_TINY_STRING_CAP=8`, and the `true/false/null` literals — **7 constants + 2 helper fns**
  (`config.rs:3-19`). Emitting the byte-identical `config.ts` from the same template kills the
  hand-copied `0x01`/`GRAMMAR_BIT0` re-declaration that any TS decoder must otherwise author twice.
- **The classify face.** The shared layout invariant that binds Rust and TS is the byte→kind map:
  `JsonNodeKind::at_cursor` at `crates/runtime/src/grammars/json/value.rs:29-44` — an **11-arm
  match** (`b'{' => ObjectOpen … b'-'|b'0'..=b'9' => Number … b'n' => Null`). Both languages
  re-derive kind from `source[offsets[cursor]]` with no stored discriminator; both must agree on
  this table or the tape mis-decodes silently. It is grammar-derivable: `STRUCTURAL_BYTES` (already
  in config) plus the leaf-terminal first-bytes are a bijection over the JSON structural alphabet.
- **The type face.** `DirectSchemaSet` (`crates/codegen/src/direct_schema.rs:3-9`) already carries
  the grammar-`->`-derived struct shapes: `DirectRootSchema`, `DirectTypeSchema`,
  `DirectTypeKind::Struct{fields, ignored_fields, unknown_fields}`, `DirectFieldSchema{key_literal,
  rust_field, ty, presence, duplicate}` (`:44-67`), carrying a `schema_hash` (`:8`). This is a rich
  structure, not a constant — the exact input a ts-rs-class type exporter consumes to emit a TS
  `interface`. Lowering is at `crates/codegen/src/lower/schema_direct.rs` (`lower_program` validates
  and rosters the direct shapes).

The tape's cross-language layout — 4 planes (`offsets u32-LE`, `flag_cursors u32`, `flag_values
u8`, `source`), separators never receive an offset, escapes marked in a sparse sorted side-channel
— is proven to cross byte-exactly in `prototypes/PROVE-14` (node-for-node parity across 320 cold
children; all 6 structural counts + codeUnits + 32-bit fingerprint identical). The plane accessors
that a wasm/ABI export hands to JS are `Tape::{source,offsets,flag_cursors,flag_values}` at
`crates/runtime/src/tape/mod.rs` (~95-137); the assembler that builds them
(`crates/runtime/src/tape/assembler.rs:45-120`, `flag_cursors`/`flag_values` push at `:98-112`) is
the writer whose reader every language must mirror. 6B generates that reader on both sides from the
plane descriptor, so the writer↔reader contract cannot skew.

**In one line:** 6B = *ts-rs for the schema face* + *a Kaitai-Struct-style layout compiler for the
tape/classify/config face*, both fed by one grammar-derived `GrammarProfile`, emitting into the
existing 8-file roster extended by a mirror TS roster.

---

## 2. Q3 — bounding the declarative surface (the null hypothesis, tested in-tree)

Q3's null hypothesis: *if the declarative surface reduces to a small constant/interface set, a
per-name linter beats a generator, and generation should be confined to the highest-leverage face.*
The seam-check it demanded first: the prior campaign cited "a `materialization.rs` pass" and "a
`STRING_NEEDS_DECODE` constant" as the grammar→layout-fact derivation, and warned they *did not
check out under `crates/`.* Adjudicated by grep:

- `materialization.rs` — **does not exist** anywhere under `skinny/crates/` (verified: no file by
  that name). The claimed pass is a phantom citation. There is **no `grammar → "this flag = bit 0"`
  derivation pass.**
- `STRING_NEEDS_DECODE` — **exists, but is a hand-authored constant, not a derivation.** It is
  `= OffsetFlags::GRAMMAR_BIT0` at `codegen/src/json_templates/config.rs:4` and mirrored at
  `runtime/src/grammars/json/config.rs:5`; the TS/bench side re-declares the same bit
  (`bbnf-bench/src/track2/json.rs:10` `const STRING_NEEDS_DECODE_FLAG = OffsetFlags::GRAMMAR_BIT0`).
  The bit *assignment* is a human decision authored (today) in one template and hand-copied to each
  consumer. What the grammar genuinely derives is *which strings can carry the flag* (the escape
  class) and *the schema* — not the bit number.

So Q3's suspicion is correct and now precise: **the layout-fact surface is a small hand-authored
constant set, and the linter that guards it already half-exists.** The bound, exhaustively for
JSON:

| Fact class | Where | Size | Grammar-derived? |
|---|---|---|---|
| Config constants | `json_templates/config.rs:3-9` | 7 consts + 2 fns | No — hand-authored policy |
| Flag-bit assignments | `OffsetFlags::GRAMMAR_BIT0` etc. | ~3 bits in use | No — hand-authored |
| Classify byte→kind | `value.rs:29-44` | 11-arm match | Yes — from structural alphabet |
| Node-kind enum | `value.rs:12-27` | 11 variants | Yes — from grammar terminals |
| Tape plane descriptor | `tape/mod.rs:95-137` | 4 planes, fixed | No — fixed runtime layout |
| Struct schema | `DirectSchemaSet` | rich, per-`->` | **Yes — already generated** |

The whole per-grammar layout surface is **~35 facts**, of which the constant/bit/plane rows
(~15 facts) are fixed policy a human authors once and rarely touches. The generator already proves
it *consumes* them coherently: the test `json_config_policy_fields_are_consumed`
(`codegen/src/lib.rs:499-521`) walks a **hand-authored `(field, consumer)` pairing list** —
`("STRING_NEEDS_DECODE","config::STRING_NEEDS_DECODE")`, 8 rows — asserting each emitted config
field has a generated consumer. **That pairing list is already a mirror-linter in embryo.**

**Verdict for the family (where generation earns its cost vs. over-builds):**

- *Over-build risk (the null hypothesis, conceded for one face):* generating the 11-arm classify
  and the 7 config constants for a **single** grammar in a **single** language is not worth a
  generator — a per-name linter (extend the `(field, consumer)` list at `lib.rs:505` to also assert
  the TS `config.ts` and TS `classify` match the Rust source) beats it, because the surface is a
  fixed constant set that a human edits once.
- *Where generation earns its cost:* the surface is small **per grammar-language cell** but the
  matrix is **5 grammars (json/css_l4/csv/toml/sheets) × 2 languages**, and the highest-leverage
  cell — `DirectSchemaSet` → interface — is **already a rich generated structure**, not a constant.
  Generation earns its keep on (a) the schema/type face (rich, per-`->`, already derived — extend it
  to TS), and (b) the *multiplication*: the classify table and config constants, trivial for JSON,
  are non-trivial for `css_l4` (`css_scan_direct/mod.rs`, its own `GRAMMAR_BIT0=AT_RULE_FLAG`) and
  will grow per new grammar. The linter guards the constant *policy*; the generator owns the
  *schema* and the *per-grammar-scaled* classify/config emission. **The two are complementary, not
  rivals: linter for the fixed constant set, generator for the schema + the N-grammar layout face.**

The defensible split: 6B generates the type face and the classify/plane face; a lightweight
mirror-linter (the extended pairing list) *guards* the ~15 hand-authored constant/bit facts rather
than generating them, because generating a constant that a human authors once and a linter can
pin is the over-build Q3 warns against.

---

## 3. Q4 — type-generation vs layout-generation (two distinct claims)

Q4 (sharpest cross-lock, owed to LOCK-4/LOCK-8): the documented flattening of conditional types by
`ts-rs`/`typeshare`/`specta` bears on **type**-generation. Does it bear on **layout**-generation —
a shared-instruction / shared-runtime-layout stream (LOCK-4C's VM formulation, tied by LOCK-8C to
4A/4C)? **Adjudicated: these are two distinct generation claims with distinct prior art and distinct
failure modes.**

**Claim A — type-generation (the interface face).** Rust struct/enum → TS `interface`/union. Prior
art is mature: `ts-rs`, `typeshare`, `specta` all take `#[derive]`-annotated Rust types and emit TS
declarations. Their documented weakness is **conditional/mapped-type flattening**: they emit
structural interfaces and cannot reproduce TS conditional types (`T extends U ? …`), so a Rust enum
with data becomes a flat discriminated union and any type-level *computation* is lost. This bears on
6B's schema face — `DirectSchemaSet` → TS interface — where it is a **non-issue**: `DirectTypeKind`
is presently `Struct{…}` only (`direct_schema.rs:52-58`), a product with string enums
(`DirectStringEnumVariant`, `:75-79`); it maps to a plain TS interface with no conditional-type
demand. When GN-3.W1 adds sum types, the union stays a *value* discriminated union (a runtime tag),
not a type-level conditional — exactly the shape ts-rs handles. **The ts-rs flattening limitation
does NOT bind 6B's schema face**, and this is precisely the claim owed to LOCK-8 (whose 8B
type-level-totality family is where TS *type-level inference* lives — 6B generates the interface
skeleton; 8B decides how much type-level computation rides atop it). The type face is zero-runtime
by construction (the interface erases), satisfying constitution clause (v).

**Claim B — layout-generation (the tape/classify/instruction face).** Here ts-rs/typeshare/specta
are **irrelevant**: they generate *types*, never a *decoder*. The layout face is a runtime byte
contract — "cursor is a u32 into `offsets`; kind = classify(`source[offsets[cursor]]`); escapes in
the sparse `flag` channel." No type generator emits that walk. The prior art for Claim B is a
different lineage entirely:

- **Kaitai Struct** — a declarative `.ksy` binary-format spec compiled to *parsers* in
  C++/Python/JS/Rust/Java/… that all decode the identical byte stream. This is the **exact analog**
  of a tape decoder: one layout spec, N language readers, provable byte-parity. 6B's plane
  descriptor + classify table is a Kaitai-shaped spec specialized to the offset-tape.
- **FlatBuffers / Cap'n Proto** — a schema compiler emits *zero-copy accessors* over a shared wire
  layout in every target language. The tape is precisely a zero-copy layout (offsets + source, no
  materialization); 6B's generated `TapeDoc`/`Tape` readers are its FlatBuffers-style accessors.
- **Protobuf** — schema→multi-language, but *not* zero-copy (it materializes); cited as the
  boundary case where layout-gen degrades into type-gen-plus-serde.
- **MLIR ODS / LLVM TableGen** — one `.td` declarative record emits C++ op classes *and* docs *and*
  verifiers; the precedent that a single machine-readable spec can drive heterogeneous outputs
  (code + oracle + docs) — the model for driving Rust + TS + the conformance corpus from one source.

**LOCK-4C's VM formulation is a THIRD, more aggressive form of layout-generation**, and Q4's tie is
real: if combinators lower to a compact bytecode VM with **one instruction set shared by Rust and
TS** (4C), then the *instruction stream* is the shared layout, isomorphic by construction — the
strongest possible 6B, because there is no per-language body to hand-fill at all (both sides
interpret the same bytes, exactly as the tape's four planes are interpreted the same bytes). This
is layout-generation at its limit: the "skeleton" is not source but a running byte format. **The
adjudication owed jointly to LOCK-4/LOCK-8:** type-gen (Claim A, ts-rs-class, the interface face)
and layout-gen (Claim B, Kaitai/FlatBuffers-class, the tape/VM face) are **orthogonal**; the ts-rs
conditional-flattening result constrains only Claim A and is a non-issue for 6B's product-shaped
schema; Claim B's isomorphism-by-construction is what 4C's VM and 6B's tape both instantiate, and
8C ("the TS plane as lawful mirror, shared instruction shape") is the consumer that binds them. 6B's
minimal defensible form is Claim A (schema→interface) + Claim B-lite (config/classify/plane
descriptor → both language decoders); 4C's VM is Claim B-full and, if it lands, subsumes the
hand-filled walk bodies 6B otherwise leaves idiomatic.

---

## 4. Substrate & integration seams (path:line)

The family is buildable from extant organs — no phantom passes required:

- **Skeleton roster (the emitter's list of files to mirror):** `codegen/src/grammar_profile.rs:3-33`
  (`COMPILED_RUNTIME_FILES`, `REQUEST_FACTS_RUNTIME_FILES`, `GENERAL_DESCENT_RUNTIME_FILES`).
- **The generator entry / determinism guarantee:** `codegen/src/lib.rs` `emit_from_source`, with
  `emission_is_deterministic` (`:485-496`) proving byte-stable output (the regen-fixpoint the mirror
  linter would gate). Extend to emit a parallel TS roster.
- **The config template (constant face):** `codegen/src/json_templates/config.rs:3-19`; the
  runtime-side generated mirror `runtime/src/grammars/json/config.rs:5-19`; the CSS analog
  `codegen/src/css_scan_direct/mod.rs:95` (`AT_RULE_FLAG = GRAMMAR_BIT0`).
- **The consumer-pairing mirror-linter (extend for TS):** `codegen/src/lib.rs:505-520`
  (`(field, consumer)` list) — the natural home to add TS-side assertions.
- **The classify face (byte→kind):** `runtime/src/grammars/json/value.rs:12-44`.
- **The type face (schema):** `codegen/src/direct_schema.rs:3-79`;
  `codegen/src/lower/schema_direct.rs:11-27` (`lower_program`). `schema_hash` at `direct_schema.rs:8`
  is the drift key a generated TS interface would carry to prove same-source.
- **The tape plane descriptor / accessors:** `runtime/src/tape/mod.rs:95-137`;
  writer `runtime/src/tape/assembler.rs:45-120`.
- **The proven cross-language decoder (hand-written baseline to generate-and-match):** PROVE-14
  (`prototypes/PROVE-14/patch.diff`, 703 LOC) — `tape_dump.rs:78` (export shape), the `.mjs`
  decoder/verify/bench. GN-4.W4 already schedules `bbnf-wasm` cdylib + `ts/tape/` as the productionized
  home; 6B's amendment is to make `config.ts` + the classify table + the interface **generated**,
  not hand-typed, in that same wave.
- **CI regen discipline (the drift-impossibility gate):** the deterministic-emission test above +
  the clean-regen law (generated files are output of fresh regen, never hand-patched).

---

## 5. Named risks & counterexamples

- **R1 — Over-build (the Q3 null hypothesis, conceded).** Generating a 7-constant `config.ts` and an
  11-arm classify for *one* grammar in *one* language is more machinery than a linter. *Counterexample:*
  a generator whose emitted `config.ts` is byte-identical to what a human would type once, and never
  changes — pure ceremony. *Cure:* generate the schema face (rich, N-grammar-scaled) and the classify
  (grows per grammar); *lint* the fixed constant/bit set via the extended `(field,consumer)` pairing.
- **R2 — The mirror generator compares one source against itself (tautology).** If both Rust and TS
  come from one emitter, a "parity" test that checks generated-Rust against generated-TS proves only
  that the emitter is deterministic, not that either is correct. *Cure:* the non-tautological oracle
  is the *external* one — PROVE-14's `JSON.parse` differential (the TS decoder vs a foreign reference),
  and sonic-rs on the Rust side. Generation removes rust↔ts drift; the corpus removes emitter-bug drift.
- **R3 — Generated TS loses V8 monomorphism.** PROVE-14's hand decoder is monomorphic by care
  (4 ctor-only fields, single hidden class, `Int8Array` dispatch — R4 §6). A naive emitter can produce
  shape-unstable TS (optional-field hidden-class transitions, megamorphic classify). *Counterexample:*
  generated `TapeDoc` with post-ctor field assignment → deopt, and the 4.4× stop-early win evaporates.
  *Cure:* the emitter templates must bake the R4 §6 discipline; the prototype gate is a `%HaveSameMap`
  shape oracle on generated vs hand TS.
- **R4 — Conditional-type demand (Q4/Claim A).** If a future grammar needs a TS *conditional* type,
  ts-rs-class generation flattens it. *Cure:* keep the schema product/discriminated-union shaped
  (it is, `direct_schema.rs:52`); type-level computation is LOCK-8B's domain atop the generated
  interface, not 6B's — 6B generates the skeleton, 8B decides the type-level layer.
- **R5 — Bit-fold / float non-portability (bleeds from the oracle sub-family).** Any generated digest
  used to prove parity must fold bit-identically across languages; an 8-byte IEEE-754 fold diverges on
  NaN-canonicalization / signed-zero. PROVE-14 sidesteps this by folding *code units*, not doubles
  (fp 986912547 matched over ASCII+UTF-16 units). *Cure:* the generated parity oracle must fold the
  same language-neutral quantity (bytes/code-units), never raw f64 bits.
- **R6 — The flag-bit assignment is authored twice today.** `STRING_NEEDS_DECODE = GRAMMAR_BIT0`
  appears in `codegen` config, `runtime` config, and `bbnf-bench/track2` (grep: 3+ sites). Until the
  emitter owns it, every consumer hand-copies the bit. *Counterexample:* a new grammar assigns
  `GRAMMAR_BIT1` for one flag on the Rust side, TS still reads bit 0 → silent mis-decode that ASCII
  corpora pass by luck. *Cure:* the bit assignment is a single generated `config.{rs,ts}` fact.

---

## 6. What a prototype must test to move this family

The family advances only on a running artifact (constitution vii). The decisive prototype:

1. **Extend the emitter to a second language for the JSON cell.** From the JSON `GrammarProfile` +
   `DirectSchemaSet`, emit `config.ts` (the 7 constants, byte-mirror of `config.rs`), a TS classify
   table (mirror of `value.rs:29-44`), and a TS `interface` (from `DirectSchemaSet`). No hand-typed
   constants in the TS decoder.
2. **Wire it into PROVE-14's decoder.** Replace the hand-declared TS constants/classify with the
   generated ones; the walk bodies stay hand-idiomatic. Re-run PROVE-14's parity oracle
   (`verify.mjs`): all 6 structural counts + codeUnits + 32-bit fingerprint must stay identical to
   `JSON.parse` over the corpus. **Pass = the generated skeleton is byte-behaviorally identical to
   the hand skeleton.**
3. **Prove drift-impossibility as a gate.** Mutate the Rust `STRING_NEEDS_DECODE` bit (or a classify
   arm) and re-regen; the generated `config.ts`/classify must move in lockstep and the extended
   `(field,consumer)` linter must go RED if a hand consumer fails to track — demonstrating the
   *impossible-by-construction* claim is real, not aspirational.
4. **Prove no V8 regression from generation.** A `%HaveSameMap` shape oracle + the PROVE-14
   stop-early bench (cold paired, N≥200) on the *generated* TS decoder must reproduce ≥2× (measured
   4.4×) and same-hidden-class — the generated code must be as monomorphic as the hand code, or
   generation is a perf regression disguised as an isomorphism win.
5. **Bound the type-face separately.** Emit the TS interface from `DirectSchemaSet` and confirm it
   type-checks against the tape decoder's return shape (the Claim-A face), with a conditional-type
   negative test proving 6B does *not* attempt type-level computation (that is 8B's plane).

The falsifier that returns the family: if step 2 needs *any* hand-authored constant in the generated
TS to pass parity, the spec is incomplete (a layout fact escapes derivation); if step 4 shows the
generated decoder is measurably slower or shape-unstable vs hand TS, generation costs more than the
drift it prevents, and the family collapses toward the Q3 linter-only null hypothesis for that face.

---

## 7. Cross-lock adjudications (proposals, not decrees)

- **Emitter-derived name map — LOCK-3 owns the grammar-derived half.** The schema/type face
  (`DirectSchemaSet` → names) is the transposition spine's product (LOCK-3, `codegen/lower/`); 6B
  *consumes* it to emit the TS mirror. The grammar-derived name is authored once by LOCK-3's lowering;
  6B's isomorphism plane maps it across languages. The non-tautological surface (Q2's fork) is the
  external corpus/`JSON.parse` oracle, not the rust↔ts comparison.
- **Regex exemption — deferred to LOCK-5's ONE-HIR verdict.** A Rust-DFA ↔ TS-`RegExp` mirroring is a
  layout-face divergence (two engines, not one generated reader). 6B should NOT freeze an allow-list
  exemption before LOCK-5 decides whether regex rides one HIR; if it does, the regex face is generated
  like the tape face, not exempted.
- **Freeze ordering (the DAG constraint).** The digest alphabet 6B generates IS LOCK-1's
  grammar-derived Value ADT (the `DirectTypeKind`/sum-type set); the reference-of-last-resort is
  LOCK-7's meta-circular oracle. **This plane freezes AFTER LOCK-1 (its schema is 6B's type-face
  input) and AFTER LOCK-7 (its oracle is 6B's non-tautological parity source).** 6B is a
  downstream weave: it cannot fix the generated skeleton until the ADT it mirrors and the oracle it
  differentials-against are themselves frozen.
