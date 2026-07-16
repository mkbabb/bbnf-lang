# LOCK-6 · Family 6B — SPEC-GENERATED-SKELETON · Pass-3 Research

The isomorphism plane's drift problem, framed positively: a single machine-readable layout
spec — grammar-derived, per the constitution — emits the tape-layout skeleton (byte layout +
per-node decoder + parity oracle) into BOTH Rust and TypeScript, so the one surface where the
two runtimes must agree byte-for-byte is generated on both sides and drift is impossible where
generated. Hand code fills only idiomatic bodies. This report makes that mechanism concrete
against the substrate at HEAD, measures the generated-vs-hand split on the tape face across two
grammars, and demonstrates that one spec already expresses two discriminator dialects.

---

## 1. The mechanism made concrete — `LayoutSpec` + the three `emit_*` faces

The substrate already has 90% of this machine standing; it is Rust-only and undocumented as a
spec. The emit spine at `skinny/crates/codegen/src/runtime_generator.rs` is the shared entry:
`emit_from_request` (line 26) → `lower_for_runtime` (40) computes a neutral
`LoweredArtifact` sum (line 19: `SinkOnly` | `Scan`) by a shape probe — never a grammar name,
never a config field (lines 34-51) — then `render` (78) dispatches the variant to one of three
concrete faces:

- `emit_sink_only` (89) → JSON family (`has_rule("value")`, line 98), assembles from six
  `include_str!` templates under `json_templates/` (config/generated/parser/value/view/visitor).
- `emit_general_descent` (147) → the Sheets family (sink-only, no `value`-dispatch), a distinct
  render path with its own config renderer (`render_general_descent_config`, 190).
- `emit_scan` (211) → CSS (`@token`/`@pretty`/`->` grammars the sink lowering rejects), a
  balanced byte-skip scanner carrying **no** `BackendShape`.

**The spec exists implicitly as the config file.** `skinny/crates/runtime/src/grammars/json/
config.rs` (31 LOC, `@generated`) is already the machine-readable byte-layout descriptor:
`STRUCTURAL_BYTES = b"{}[],:\""`, `STRING_NEEDS_DECODE = OffsetFlags::GRAMMAR_BIT0`,
`TINY_STRING_CAP`, the literal tables, and the `W7_*` backend-shape policy triad. Sheets emits
a different config from the same renderer path. **The `LayoutSpec` this family names is that
config, promoted from a Rust `.rs` string-template output to a backend-agnostic data structure
that a Rust emitter AND a TS emitter both consume.** No TS emitter exists today (grep of
`skinny/crates/codegen/src` for `.ts|Uint32Array|TapeDoc|wasm` returns nothing) — the six
`json_templates/*.rs` are `include_str!`'d Rust source, normalized and policy-substituted. That
Rust-source-as-template mechanism is exactly what forbids a second backend: the "spec" is Rust
syntax, not data. 6B's mechanism replaces `include_str!("json_templates/value.rs")` (line 281,
etc.) with `emit_value(&layout_spec, Backend::Rust)` / `emit_value(&layout_spec, Backend::Ts)`.

**The one axis the current `LoweredArtifact` lacks is the discriminator-provenance axis** — the
variable that distinguishes the two dialects §2 proves already coexist. The concrete `LayoutSpec`
is:

```
LayoutSpec {
  structural_alphabet: &[u8],              // JSON "{}[],:\""; CSS the ruleBlock set
  flag_plane: FlagSemantics,               // the polymorphic side-channel (see §2)
  discriminator: Discriminator,            // FromSource | FromFlagTag | None(Scan)
  leaf_decoders: Vec<LeafDecoder>,         // string(escape via flag), number, literal
  kinds: Vec<(KindName, KindTag)>,         // the JsonNodeKind / GoogleSheetsKind roster
}
Discriminator =
  | FromSource(alphabet_to_kind: Map<u8, KindTag>)   // JSON: classify(source[offset])
  | FromSourceThenFlag(byte_map, [(FlagBit->Kind)], default) // CSS L4: byte, else flag, else default
  | FromFlagTag(kind_table: Vec<KindName>)           // Sheets: KIND_TABLE[flag_byte - 1]
  | Scan                                             // general-descent: no per-node kind
```

**Correction/enrichment vs a first reading of the CSS path as pure `Scan`:** the
`css_l4_declaration_values` grammar does NOT lower to a kind-less scan — it ships a live
per-node classify, `CssNodeKind::at_cursor` (`grammars/css_l4_declaration_values/generated.rs:51-68`,
emitted by `codegen/src/css_scan_direct/projection.rs:26-41`), which reads `source[offset]==':'`
→ `Declaration`, else consults the sparse `AT_RULE_FLAG` (`GRAMMAR_BIT0`) → `AtRule`, else
`QualifiedRule`. This is a **third, hybrid provenance** — `FromSourceThenFlag` — where the
source byte is a *partial* discriminant completed by the flag plane. It sits strictly between
JSON's pure `FromSource` and Sheets' pure `FromFlagTag`, and it proves the discriminator axis is
a genuine spectrum, not a binary. The kind-less `Scan` artifact is the general-descent/Sheets
prelude path, not the CSS L4 value classify.

Each backend has one emitter per face — `emit_config`, `emit_kinds`, `emit_classify`,
`emit_walk`, `emit_oracle` — parameterized by `LayoutSpec` and `Backend`. The classify face is
the load-bearing one: it is the exact function the Rust and TS runtimes must agree on, and
today it is hand-mirrored (§3).

---

## 2. Q2 — one spec, BOTH dialects: the flag plane is the polymorphic slot (worked demonstration)

The premise names an "AoS-stored-discriminator dialect" and an "SoA-derived-from-source dialect"
and asks whether one layout spec expresses both. **It does, and both already ship in-tree over
the identical physical tape.** The reconciliation is that "AoS vs SoA" was never a storage
question — the tape is *always* SoA planes (`Tape` = `offsets: Vec<u32>` + sparse
`flag_cursors: Vec<u32>` / `flag_values: Vec<u8>` + `PayloadArena`, at
`skinny/crates/runtime/src/tape/mod.rs:92-99`). The real axis is **discriminator provenance**,
and the sparse flag plane is a per-grammar-typed slot that carries whichever provenance the
grammar needs.

**Dialect A — derived-from-source (JSON).** Kind is re-derived, never stored:
`JsonNodeKind::at_cursor` (`grammars/json/value.rs:29-47`) reads `source()[offset]` and matches
the structural byte (`b'{' => ObjectOpen`, `b'"' => String`, `b'-'|b'0'..=b'9' => Number`, …).
The flag plane here is a *1-bit escape marker only*: `STRING_NEEDS_DECODE = GRAMMAR_BIT0`, set
via `push_offset(start, needs_decode_flags())` solely for strings carrying escapes (312 of
18,099 on twitter — PROVE-14). `flags_at` binary-searches the sparse plane
(`tape/mod.rs:142-148`). Works because JSON's structural bytes are self-distinguishing.

**Dialect B — stored-discriminator (Sheets).** Kind is stored and recovered by table:
`GoogleSheetsNode::kind` (`grammars/sheets/view.rs:119-128`) reads `tape.flags_at(cursor)?.bits()`
as a 1-based rule-order tag and indexes `KIND_TABLE[(tag-1)]` (view.rs:51-91). The emitter writes
that tag into the **same sparse flag plane** JSON uses for the escape bit:
`tape.push_offset(start, OffsetFlags(kind))` (`grammars/sheets/generated.rs:161,195`). Sheets
*must* store a discriminator: its operator tower (`ComparisonExpr`, `ConcatExpr`, `AddExpr`,
`MulExpr`, `UnaryExpr` — view.rs:52-91) has no self-distinguishing first source byte; an
"add expression" cannot be classified by the character at its offset. So the flag byte, a
1-bit escape flag in JSON, becomes a `u8` rule-order discriminator in Sheets — **one physical
slot, two typed meanings.**

**Dialect A′ — derived-from-source-THEN-flag (CSS L4 declaration values).** Kind is a
*partial* source-byte discriminant completed by the sparse flag: `CssNodeKind::at_cursor`
(`grammars/css_l4_declaration_values/generated.rs:51-68`) tests `source[offset]==':'` →
`Declaration`, else `flags_at(cursor).contains(AT_RULE_FLAG)` → `AtRule`, else `QualifiedRule`.
The at-rule flag is the SAME sparse plane, a third typed meaning of the shared slot (a
1-bit structural-role tag). This is the reconciling case that proves `Discriminator` is a
spectrum: JSON reads byte only, CSS reads byte then flag, Sheets reads flag only — three points,
one flag plane, one `at_cursor` skeleton shape.

**Dialect C — no per-node discriminator (general descent / Sheets prelude).** The `Scan`
artifact is a balanced byte-skip with no `BackendShape` and no kind roster
(`runtime_generator.rs:22-23`, `emit_scan`).

**The worked expressibility result:** one `LayoutSpec` field, `Discriminator`, ranges over
`{FromSource(alphabet_map), FromFlagTag(kind_table), Scan}`; one `FlagSemantics` field types
the shared plane as `EscapeBits` or `DiscriminatorTag`. The Rust `emit_classify` face lowers
`FromSource` to a `match source[offset]` (JSON) and `FromFlagTag` to a `KIND_TABLE` index
(Sheets) — both are *the code already at HEAD*. The TS `emit_classify` face lowers the same two
variants to `classify(c){ return CLASSIFY[this.source[this.offsets[c]]] }` and
`classify(c){ return KIND_TABLE[this.flagAt(c) - 1] }`. **No reconciliation blocker is filed:
the spec expresses both because both are already the same tape with two projections of one
sparse plane.** The only spec obligation is to declare the flag plane's semantics per grammar,
which `config.rs` already does implicitly (`STRING_NEEDS_DECODE` for JSON; the tag table is in
`view.rs` for Sheets and must move into the spec).

---

## 3. Q1 — generated-vs-hand-authored LOC on the tape-layout face (two grammars)

Measured on the tape face specifically (byte layout `config.rs` + per-node decoder skeleton
`value.rs`/`view.rs`), **not** the schema/type interface, at HEAD:

| grammar | face files (all `@generated`) | LOC | hand-authored on this face |
|---|---|--:|--:|
| JSON | `config.rs` 31 + `value.rs` 172 (kinds/classify/`JsonValue` union) + `view.rs` 488 (the lazy walk) | **691** | **0** |
| Sheets | `config.rs` 4 + `view.rs` 279 (kinds/`KIND_TABLE`/lazy cursor) | **283** | **0** |
| CSS L4 | `generated.rs` `CssNodeKind` + `at_cursor` + `CssNode` view (`:44-120+`) — a real `FromSourceThenFlag` decoder skeleton, ~65+ | **65+** | **0** |

Every tape-face file carries `// @generated by skinny bbnf-codegen; do not edit by hand.` The
only hand-authored files in `grammars/json/` are `scan.rs` + `sink.rs` (469 LOC, "JSON-owned …
not part of the generated roster") — the SIMD scanner and the direct sink, which are the *parse*
plane, not the tape decoder skeleton. **On the Rust side the tape-layout face is ~100%
generated across both grammars.** The generator that produces it is `json_templates/{config,
value,view}.rs` (31+172+488 = 691 template LOC, write-once, amortized) plus the two config
renderers in `runtime_generator.rs` — generator cost, not per-grammar hand cost.

**The TS side is 0% generated and 100% hand-authored — this is the drift surface 6B closes.**
PROVE-14's decoder is 703 LOC of hand-written `.mjs` (`tape.mjs` 364, `load/pointer/child/
verify/bench` the rest — PROVE-14 REPORT table). Its `TapeDoc.classify` (patch.diff:420-421,
`const b = this.source[this.offsets[cursor]]`) is a **hand-mirror** of Rust
`JsonNodeKind::at_cursor` (value.rs:33). Its `needsDecode` binary search (patch:438-442)
hand-mirrors `Tape::flags_at` (mod.rs:142-148). Two hand-authored copies of one classify law is
precisely the isomorphism-drift risk; the spec-generated skeleton makes both sides fall out of
`emit_classify(spec, Rust)` and `emit_classify(spec, Ts)`. **Net 6B claim: convert 703 hand LOC
(and any 2nd/3rd-grammar TS decoder that would follow) into generated output; drift becomes a
regen diff, not a code review.**

---

## 4. Q3 — idiomatic (non-faithful) consumer: adapter cost vs faithful-union baseline

The faithful-union baseline is `JsonValue<'doc,'input>` (value.rs:69-76): a 6-variant enum of
lazy views over the tape, kind recovered per-node by `value_from_ref` (value.rs:143-172). The
idiomatic consumer is a native Rust struct (e.g. `TwitterSearch`) filled by `parse_into::<T>`.

**The adapter is generated, and it is a net win, not a cost.** The idiomatic path is
`json_typed_direct.rs` (1245 LOC emitter) driven by a grammar-derived `DirectSchemaSet` /
`DirectTypeKind` sum-type carrier (`direct_schema.rs:4-`), lowered through
`lower::schema_direct::TypedDirectProgram`. It emits a *fused single-pass filler* straight from
source into the struct — no intermediate `JsonValue` union, no fold-over-cursor (dead by law).
Measured (SPEC §0 row 07, CONFIRMED): direct-to-struct beats sonic-typed 0.746-0.786× paired
(≈1.27-1.34× faster) and serde on all three corpora; citm 1.85-1.92× vs serde.

So both directions:
- **faithful → idiomatic** (union to struct): the generated fused sink — *faster than the
  faithful union walk itself*, because it skips union materialization. The "adapter" is the
  1245-LOC generated emitter, amortized to zero per-grammar hand LOC; the schema is grammar-
  derived (GN-3), so an idiomatic type costs only its `->` projections in the grammar.
- **idiomatic → faithful** (struct back to a value): re-serialize via `to_canonical_string`
  (value.rs:90-101) — the only direction that pays, and only if a consumer needs it.

**The finding: for an idiomatic consumer the adapter cost is negative on the hot direction** —
the thin adapter is a generated straight-line sink that outperforms the faithful union, because
the faithful union pays for a generality (any-shape lazy access) the idiomatic consumer does not
use. Caveat measured on Rust only; the TS side has no idiomatic-struct adapter yet (PROVE-14
ships only the faithful `TapeDoc` walk + stop-early get). A TS `emit_direct_sink` face is the
symmetric obligation, and it is the same `DirectTypeKind` spec feeding a TS emitter.

---

## 5. Q4 — shared bytecode instruction stream as a generation target (routing to LOCK-4/LOCK-8)

**Argument: yes, live, and a type-generation refutation does not bind it.** 6B's mechanism is
proven on a *data* layout — the offset tape crosses byte-exactly to V8 (PROVE-14: 320 cold
children, node-for-node, fp 986912547 identical). A combinator VM bytecode (LOCK-4C VM-CORE) is
a *code* layout with the identical cross-language property: a flat array of integer opcodes +
operands, decoded by a monomorphic interpreter on each side. The reasons a type-generation
refutation (types that will not cross cleanly across the Rust/TS type systems) cannot bind a
bytecode target:

1. **Bytecode carries no generic type parameters to erase.** The tape's `u32` cursors and the
   VM's `u32` opcodes are the same monomorphic integer substrate; there is no
   `JsonValue<'doc,'input>` / `Parser<T>` lifetime-or-generic mismatch to reconcile. The V8
   monomorphism that made `TapeDoc` a single hidden class (four ctor-only integer fields) is the
   *same* discipline a bytecode interpreter's state object wants.
2. **The spec-generated-skeleton machine is layout-shape-agnostic.** `LayoutSpec` +
   `emit_{config,decoder,oracle}` does not care whether the plane it describes is
   `offsets[cursor] → node` (tape) or `code[pc] → instruction` (VM). One `emit_decoder` face
   generates a tape walk; a sibling `emit_dispatch` face generates the VM's opcode switch — same
   generator, same cross-language parity oracle (a differential of the two interpreters over a
   corpus, exactly like `verify.mjs`).
3. **The refutation that *does* bind is the full-walk one, and it is orthogonal.** PROVE-14
   proved cold-JIT JS cannot beat native `JSON.parse` on full materialization (0.91×); a shared
   VM's value is likewise *not* raw throughput but layout isomorphism + stop-early amortization.
   That is a perf-non-goal, not a generation blocker.

**Routing:** LOCK-4 owns the instruction-set *design* (which opcodes; 4C's "one instruction set
shared by rust and ts, layout isomorphism by construction"). LOCK-8 owns the TS interpreter
*shape* (8C isomorphic mirror; the shape-stable monomorphic VM state per 8A). 6B contributes the
*generator*: the shared `LayoutSpec`/`emit_*` machine and the differential oracle that proves the
two interpreters agree. The joint obligation owed to LOCK-4/LOCK-8 is one prototype: emit a
trivial two-opcode stream from a spec into both a Rust `match pc` and a TS `switch(pc)`, run the
same corpus through both, assert identical output digest — the bytecode analogue of PROVE-14.

---

## 6. Q5 — ownership of `LayoutSpec`, `emit_*`, and `MaterializationClass`

The split is already latent in `runtime_generator.rs` and should be ratified along the existing
seam between *deciding* and *emitting*:

- **`LayoutSpec` (the type) + the `emit_*` faces → LOCK-3 (IR transposition spine).** These are
  the "backend shapes → emission" stage of LOCK-3's charter ("grammar AST → typed HIR → mid-IR →
  backend shapes → emission … multi-backend over a backend-agnostic middle"). `LayoutSpec` *is*
  the backend-agnostic middle for the tape face; `emit_{config,classify,walk,oracle}(spec,
  backend)` are the transpositions to Rust and TS. This is where `render` (line 78) and the three
  `emit_*` functions live today. The backend-agnostic constitution clause (backend-agnostic
  types; each backend resolves natives) makes `LayoutSpec` a LOCK-3 artifact by construction.

- **`MaterializationClass` (the variable) → LOCK-2 (the graph engine / decision driver).** The
  *choice* of which class a grammar resolves to is a decision, not an emission. It exists at HEAD
  as `LoweredArtifact` (`SinkOnly | Scan`, line 19) selected by the shape probe `lower_for_runtime`
  (40) — "the variant is the OUTPUT of the shape-derived lowering pipeline, never a grammar tag."
  That probe is a decision-driver seat: LOCK-2C's "variables are per-site lowering choices." 6B's
  discriminator-provenance axis (§2, `FromSource | FromFlagTag | Scan`) *extends*
  `MaterializationClass` with a second dimension; the driver that assigns it (self-distinguishing
  alphabet ⇒ `FromSource`, else `FromFlagTag`) is a LOCK-2 cost/legality decision, exactly the
  `has_rule("value")` / `derive_program` shape probes already in place (lines 57, 98).

**Rationale against double-homing:** LOCK-3 must not own the *selection* (it would re-fork the
`render` dispatch on a grammar property, reviving the firewalled `target.*`/`contract.*` selection
the current design explicitly forbids, lines 73-76). LOCK-2 must not own the *emitters* (a decision
engine emitting Rust/TS source violates the middle-IR/backend separation). The clean line:
**LOCK-2 hands LOCK-3 a resolved `MaterializationClass` + `LayoutSpec`; LOCK-3 emits both
backends from it.** This mirrors the extant `lower_for_runtime` → `render` handoff exactly.

---

## 7. Substrate & integration seams (path:line)

- Physical tape (shared SoA planes): `skinny/crates/runtime/src/tape/mod.rs:92-99` (`Tape`
  fields), `:120-148` (`source`/`offsets`/`flag_cursors`/`flag_values`/`offset_at`/`flags_at`
  accessors — PROVE-14's export shape), `:14-34` (`OffsetFlags`, the polymorphic flag byte).
- JSON dialect (FromSource): `grammars/json/value.rs:29-47` (`at_cursor` classify),
  `:143-172` (`value_from_ref` union projection), `config.rs:4-9` (byte layout spec).
- Sheets dialect (FromFlagTag): `grammars/sheets/view.rs:51-91` (`KIND_TABLE`), `:119-128`
  (`kind` = `flags_at().bits()` index), `generated.rs:161,195` (`push_offset(_, OffsetFlags(kind))`
  — the discriminator write).
- Emit spine (the emit_* faces + MaterializationClass): `codegen/src/runtime_generator.rs:19-24`
  (`LoweredArtifact`), `:26-71` (`emit_from_request`/`lower_for_runtime`), `:78-87` (`render`),
  `:89-144` (`emit_sink_only` JSON), `:147-208` (`emit_general_descent` Sheets), `:211-278`
  (`emit_scan` CSS), `:280-281` (`render_json_config` via `include_str!` — the Rust-string
  template to be replaced by `LayoutSpec`).
- Idiomatic-consumer adapter (Q3): `codegen/src/json_typed_direct.rs:10` (1245-LOC emitter),
  `codegen/src/direct_schema.rs:4-` (`DirectSchemaSet`/`DirectTypeKind`).
- TS hand-mirror (the drift surface): PROVE-14 `patch.diff:411-546` (`TapeDoc` ctor + `classify`
  + `needsDecode` + `walk`), superseded by GN-4.W4's `ts/tape/` (GN-4.md:310-385).
- GN-4.W4 wasm seam: `crates/bbnf-wasm/src/tape_export.rs` (planned, GN-4.md:321-326) exports the
  four planes as typed-array views for the TS decoder.

---

## 8. Prior art (citations)

- **Cap'n Proto / FlatBuffers** — a schema compiles to per-language decoders over a wire layout
  the languages agree on byte-for-byte; hand code never touches the layout. This is the direct
  analogue: `LayoutSpec` is the `.capnp`/`.fbs`; `emit_*` are the language backends. Key
  difference bbnf must preserve: FlatBuffers stores a vtable/discriminator; bbnf's `FromSource`
  dialect stores *nothing* (kind derived from the parsed source byte) — a stricter zero-overhead
  form FlatBuffers has no notion of.
- **Kaitai Struct** — a declarative binary-layout DSL generating parsers in ~10 languages from
  one `.ksy`; validates that a single layout spec drives many language emitters. Its
  discriminated unions (`switch-on`) are precisely the `Discriminator` axis of §2.
- **LLVM TableGen / MLIR ODS** — one `.td`/`.td`-derived record set generates C++ enums,
  printers, and verifiers; the "spec generates the boilerplate, hand code fills semantics" split
  is the 6B model. `emit_kinds`/`emit_classify` are the analogue of TableGen's enum + `getKind`
  emission.
- **serde `derive` / `#[repr]`** — the idiomatic-consumer story (Q3): a derive macro generates the
  adapter from a type; bbnf inverts it (grammar `->` projections derive the schema, schema derives
  the struct + sink).
- **regex-automata `DFA` serialization** (`to_bytes`/`from_bytes`) — a compiled automaton crosses
  process/arch boundaries as a versioned byte layout with an endianness/alignment contract; the
  exact discipline GN-4.W4's wasm plane export needs (R4 risk: `Uint32Array` view mis-alignment,
  GN-4.md:540-544).
- **CBOR/MessagePack self-describing tags** — the tag-byte discriminator (§2 dialect B) is the
  MessagePack model; JSON's dialect A is closer to a *schema-typed* CBOR where the tag is elided.

---

## 9. Risks & counterexamples

- **R1 — the spec fits JSON, not grammars-in-general.** If `LayoutSpec` can only express
  `FromSource`, it is a JSON-shaped spec dressed as general. *Counterexample that must fail RED:*
  a spec that cannot emit the Sheets `KIND_TABLE` classify. **Mitigation is already proven:**
  Sheets ships `FromFlagTag` at HEAD (§2) — the prototype MUST generate both grammars' classify
  from one spec, or the mechanism is unproven.
- **R2 — the flag plane cannot carry both a discriminator tag AND an escape bit simultaneously.**
  A grammar that needs *both* a stored kind tag *and* a per-node escape flag has one `u8` slot
  (`OffsetFlags`, mod.rs:16) and would collide. *Counterexample:* a grammar with a string leaf
  requiring escape decode inside an operator tower needing a rule tag. **This is a real
  reconciliation risk** — resolvable by bit-partitioning the `u8` (tag in high bits, escape in
  `GRAMMAR_BIT0`) but that partition must be a spec-declared field, not implicit. File it as the
  one named spec obligation beyond HEAD.
- **R3 — generated TS is slower than hand-tuned TS.** A spec emitter may not reproduce PROVE-14's
  hand monomorphism (four ctor-only fields, single hidden class). *Counterexample:* generated
  `TapeDoc` with a post-ctor field assignment ⇒ hidden-class transition ⇒ deopt. **Mitigation:
  the V8 probe below is the gate**, and the emitter must be written to the monomorphism law
  (R4 §6a-e).
- **R4 — the parity oracle stubs itself** (GN-4.md R5): a generated oracle that compares a lane to
  itself gives false green. The oracle must be generated to fold *distinct* lanes (Rust digest vs
  TS digest over one corpus, cross-process).
- **R5 — `include_str!` templates are load-bearing and easy to keep.** The path of least
  resistance is to add a TS `include_str!` template alongside the Rust one — two hand templates,
  the exact drift 6B exists to kill. The prototype must generate from *data*, not a second string
  template.

---

## 10. The V8 monomorphism probe (the pass-3 evidence requirement) + what a prototype must test

PROVE-14 asserted `TapeDoc` monomorphism *by grep* ("zero post-ctor mutation", VERIFY attack 8) —
it did **not** run the natives-syntax probe. The directive requires shape-stability under
`node --allow-natives-syntax`, not `tsc` alone. The exact probe:

```js
// node --allow-natives-syntax probe.mjs
const a = decodeTape(planesA), b = decodeTape(planesB);   // two TapeDoc instances
console.log(%HaveSameMap(a, b));         // MUST be true — single shared hidden class
console.log(%HasFastProperties(a));      // MUST be true — no dictionary-mode fallback
// drive one classify() then re-check, to catch a mid-parse transition:
a.walk(0, 0); console.log(%HaveSameMap(a, b) && %HasFastProperties(a));
```

**What a prototype must test to move family 6B (minimal, decisive):**

1. **One spec, two grammars, both backends.** Hand-write a `LayoutSpec` for JSON (`FromSource`)
   and Sheets (`FromFlagTag`); implement `emit_classify(spec, Rust)` and `emit_classify(spec,
   Ts)`. **Gate:** the Rust output byte-matches the hand `JsonNodeKind::at_cursor` (value.rs:29)
   and `GoogleSheetsNode::kind` (sheets/view.rs:119) after normalization; the TS output
   byte-matches PROVE-14's `TapeDoc.classify` (patch:420). Drift becomes a regen diff.
2. **The V8 gate above passes on the GENERATED TS decoder** — `%HaveSameMap` + `%HasFastProperties`
   true across two parses. Proves generation did not cost the hand-tuned monomorphism.
3. **The generated differential oracle folds distinct Rust↔TS lanes bit-identically** over
   twitter.json (reuse PROVE-14's fp 986912547 as the fixed point) — a real cross-process check,
   not a self-comparison.
4. **Q1 delta measured:** count generated-vs-hand LOC on the TS tape face after generation (target:
   the 703 hand LOC → ~0 hand + one shared spec + two emitter faces). Report the crossover: the
   grammar count at which generator LOC (write-once) is amortized below the hand-mirror LOC it
   replaces (JSON alone = 703 TS + 691 Rust hand-equivalent behind one spec).
5. **R2 resolution:** exhibit a grammar needing tag+escape and show the spec's flag-partition
   field emits a correct classify on both sides, or file R2 as the precise reconciliation blocker
   with the bit-layout it needs.

**Bottom line for 6B.** The spec-generated-skeleton is not speculative: the Rust tape face is
already ~100% generated across two grammars, the byte layout crosses to V8 byte-exactly, and two
discriminator dialects already coexist over one physical SoA tape via the polymorphic flag plane.
The missing 10% is (a) promoting the `config.rs`-shaped spec from Rust-string templates to a
backend-agnostic `LayoutSpec` data structure, (b) adding a TS emitter face beside the Rust one,
and (c) a generated cross-process differential oracle with the V8 monomorphism gate. Drift on the
one surface the two runtimes must agree on becomes impossible by construction where generated;
hand code retreats to the idiomatic bodies (SIMD scanners, sinks) that were never mirrored.
