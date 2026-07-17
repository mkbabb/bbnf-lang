# LOCK-6 · Family 6B — SPEC-GENERATED-SKELETON — Research (pass 3)

## Thesis

One machine-readable `LayoutSpec`, itself grammar-derived, generates BOTH language skeletons of the
*tape-layout face*: the Rust decoder view and the TS `TapeDoc`. The physical byte layout is one
hand-authored substrate shared by every grammar; the per-grammar decoder skeleton and the
cross-language oracle are generated from grammar facts; idiomatic bodies (NEON kernels, consumer
structs) are hand-filled. Where generated, drift is impossible by construction — a `regen∘regen`
fixpoint plus a byte-identity gate reduces it to a boolean. The strongest form of this family is not
aspirational: **on the general-descent path the Rust decoder skeleton — enum, `KIND_TABLE`,
`rule_kind`, and the typed `as_f64/as_bool/as_tag` accessors — is ALREADY programmatically emitted
from grammar rules and `->` projections, in production, at HEAD.** This pass grounds the mechanism on
verified `path:line`, corrects two prior-pass errors of record, and states the exact prototype that
moves the family.

## Two corrections of record (verified at skinny@HEAD `ce51c36fc`)

1. **The JSON view is NOT spec-generated — it is a hand-authored TEMPLATE copied verbatim.**
   `emit_sink_only` splices the JSON decoder from `include_str!("json_templates/view.rs")`
   (`codegen/src/runtime_generator.rs:122-130`); `codegen/src/json_templates/view.rs` is a literal
   488-LOC Rust file carrying the `@generated` header but hand-maintained. "Generated" on the JSON
   path means *copied*, not *emitted from a spec*. This is the special-case 6B's `LayoutSpec` exists
   to abolish (§ mechanism).
2. **The Sheets/general view IS genuinely programmatically emitted from grammar facts.**
   `emit_general_descent` (`runtime_generator.rs:147`) drives `general_descent/emit.rs` (452 LOC) +
   `general_descent/view.rs` (431 LOC), which *write* the decoder with `write!`/`writeln!`:
   `KIND_TABLE` (`view.rs:281`), the `rule_kind` accessor (`view.rs:335`), and the typed accessors —
   each **derived from grammar structure**: `rule_kind_tag = program.rules.position(name)+1`
   (`view.rs:103-110`, a 1-based declaration-index tag), and `build_typed_accessors` emits `as_f64`/
   `as_bool`/`as_tag` guards "DERIVED from the grammar's `->` projections … never from hard-coded
   rule names" (`view.rs:117-135+`). **This is the 6B thesis already true and shipping** for every
   non-JSON grammar.

The dispatch that separates them: `render` matches `LoweredArtifact` variant
(`runtime_generator.rs:78-89`) — `SinkOnly` → `emit_sink_only` (JSON template *or* fall to
`emit_general_descent`, `:99`), `Scan` → `emit_scan` (CSS). Three ad-hoc emitter codepaths; 6B
unifies them. **No `LayoutSpec`, `emit_classify`, `emit_view`, or `emit_ts` symbol exists anywhere
in-tree** (grepped) — the named spec machine is UNBUILT; the general emitter is its in-production
kernel awaiting generalization.

## The substrate: one hand-authored four-plane layout, marginal-per-grammar ≈ 0

`Tape { source, offsets: Vec<u32>, flag_cursors: Vec<u32>, flag_values: Vec<u8>, payloads:
PayloadArena }` (`crates/runtime/src/tape/mod.rs:92-97`; ctor `from_offsets` `:102-115`). A node is a
`u32` cursor into `offsets`; separators (`,` `:`) receive no offset. The flag plane is a *sparse
sorted side-channel*: `flags_at(cursor)` binary-searches `flag_cursors`, returns the paired
`flag_values` byte; the single grammar bit is `GRAMMAR_BIT0 = 0x01` (`tape/mod.rs:20`). This
substrate is hand-authored and shared — 233 LOC paid **once** for all grammars; marginal layout cost
per new grammar ≈ 0. The AoS/SoA storage question is MOOT at HEAD by SPEC §0 law; 6B's contribution
is that both *discriminator strategies* ride that one moot layout, and one emitter can write both.

## The mechanism, made concrete: the discriminator-provenance axis over one layout

The single flag-plane byte is the pivot on which two opposite dialects turn (both view files carry
the `@generated` header at line 1):

- **JSON — discriminator DERIVED-FROM-SOURCE (currently a TEMPLATE).** No stored tag: kind is
  recovered by `JsonNodeKind::at_cursor(tape, cursor)` classifying `source[offsets[cursor]]`
  (`grammars/json/value.rs:12-44` enum + accessor; used `grammars/json/view.rs:146,272-395`). The
  flag byte carries only `STRING_NEEDS_DECODE = GRAMMAR_BIT0`, marking the escaped-string minority
  (312/29,573 tokens on twitter.json, PROVE-14). Sparse flag plane. **488-LOC hand template.**
- **Sheets — discriminator STORED (PROGRAMMATICALLY EMITTED).** The flag byte *is* a 1-based rule
  tag: `rule_kind` reads `tape.flags_at(cursor)?.bits()` and indexes the emitted `KIND_TABLE`,
  `tag-1` → `GoogleSheetsKind` (`grammars/sheets/view.rs:52,123-127,147`). Rule-keyed payload
  decoders `as_f64`/`as_bool`/`as_tag` (`:167,177,195`) are emitted from `->` projections. Dense flag
  plane (a tag per node). **279-LOC, written by the `general_descent` emitter.**

**Worked expressibility (Q2, discharged by the tree).** The *same physical four-plane `Tape`*
expresses a derived-discriminator dialect (JSON) and a stored-discriminator dialect (Sheets) with
zero layout change; the only differences are (a) the kind-recovery function (`classify(source[off])`
vs `KIND_TABLE[flag-1]`) and (b) whether the flag plane is written sparsely or densely. This is a
two-axis parameter, not a reconciliation blocker. A `LayoutSpec` needs exactly:
- `KindRecovery ∈ { FromSourceByte(classify_table), FromFlagTag(kind_table) }`
- `FlagPlaneDensity ∈ { Sparse(grammar_bits), DenseTag }`
- `PayloadDecode[] ∈ { SpanAsF64, SpanAsBool, LiteralTag, ArenaBytes }` — Sheets needs three
  rule-keyed decoders; a spec modeling only discriminator recovery under-generates (R3).

The concrete landing: **`general_descent` already emits `FromFlagTag`+`DenseTag`+payload-decoders
from grammar facts. Add a `KindRecovery::FromSourceByte` arm (classify the first structural byte)
and a `Sparse` flag mode, and the JSON template folds into the same emitter as a data case** — the
488-LOC hand template dies, JSON becomes ≈40 lines of `LayoutSpec` data + config. That single change
is the entire 6B mechanism on the Rust side; the emitter to extend is real and 883 LOC.

## Q1 — generated-vs-hand LOC on the tape-layout face (≥2 grammars, THIS face only)

Measured on the byte-layout + decoder-skeleton + oracle face (NOT the schema/type interface):

| element | JSON | Sheets | authored |
|---|--:|--:|---|
| byte-layout substrate (shared) | — | — | **hand**, `tape/mod.rs` 233 LOC; marginal/grammar ≈ 0 |
| decoder skeleton (`grammars/*/view.rs`) | 488 | 279 | JSON=**template-copy** (hand once); Sheets=**programmatically emitted** |
| the emitter that writes the Sheets skeleton | — | — | `general_descent/{view.rs 431, emit.rs 452}` = 883 LOC, all grammars |
| structural scan kernel (`scan.rs`) | ~349 | (n/a) | **hand**, grammar-owned idiomatic NEON body |
| idiomatic fused sink (CSS) | `sink.rs` **16** | — | **generated** over `generated.rs` (886 LOC) |
| TS cross-language decoder | 364 | 364 | **hand transliteration** (PROVE-14) — the sole real drift surface |

Honest reading: on the **general path (Sheets)** the decoder skeleton is genuinely generated from
grammar facts — per-grammar hand-LOC on the decoder ≈ 0 (only a config profile). On the **JSON path**
the 488-LOC view is a hand template reused only for JSON. So the tape face today is a **mixed** story
— one grammar spec-emitted, one templated — and 6B's crown is exactly to make JSON a data case of the
Sheets emitter, proving both dialects from one machine. The claim "byte-layout face is 100%
generated" is TRUE for the Rust config/offset facts and the general path, and FALSE (template) for the
JSON view and (entirely) for the TS mirror. **The one hard number: TS is 0% generated (364 hand LOC),
proven byte-exact only by hand transliteration (SPEC §0 row 14, fp `986912547`) — this is the drift
surface 6B closes by emitting `tape.ts` from the same emitter.**

## Q2 — AoS-stored-discriminator ⊕ SoA-derived-from-source in one spec — DEMONSTRATED

Both dialects ship over one layout (above). The stored-discriminator dialect is *already*
programmatically emitted (`general_descent`); the derived dialect is *currently* a template. The
expressibility is demonstrated by the two committed views + the shared emitter; the only reconciliation
work is folding JSON's `FromSourceByte` recovery into the general emitter as an axis. **No blocker to
file** — the axis is `KindRecovery`, and the general emitter's `write!`-based structure is
recovery-agnostic (it writes accessor guards keyed on the recovered kind regardless of how the kind is
recovered). Precise residual: CSS-L4 is a *third* provenance — `FromSourceThenFlag` (byte partial +
flag completes) — which the axis must admit as a third `KindRecovery` arm, not two.

## Q3 — idiomatic (non-faithful) consumer: adapter cost vs faithful-union baseline, both directions

Faithful-union baseline = the full lazy view (39-variant `GoogleSheetsKind` / lazy `JsonValue`); every
node reachable, no consumer-shape assumption. An idiomatic consumer wants a native struct (value.js
wants `{r,g,b}`, not a token union). Two directions:

- **faithful-union → idiomatic (fold/adapter over a built view):** materialize the faithful tree, then
  fold to the struct. This is the **dead lane** by SPEC §0 law (fold-over-lazy-cursor 2.3-4.9× behind
  sonic). A "thin" adapter atop the faithful union is the *expensive* direction — not thin once the
  faithful tree is paid for.
- **idiomatic-direct (generated fused sink, no faithful union built):** the CSS `sink.rs`
  (`grammars/css_l4_declaration_values/sink.rs`, 16 generated LOC over 886-LOC `generated.rs`) and the
  `json_typed_direct.rs` emitter (1245 LOC) / `json_sink_direct/` (1213 LOC across 5 files) fill the
  native struct in one pass. Measured (SPEC §0 row 07, CONFIRMED): beats sonic-rs typed **0.746-0.786×
  paired** and serde on all three JSON corpora, **1.85-1.92× vs serde on citm** (twitter-vs-serde 2× bar
  honestly unmet — thin projection). The "adapter" *is* the generated single-pass filler; its cost is
  **below** the faithful baseline because no union is ever built.

**Conclusion.** The idiomatic type is **cheaper than the faithful-union baseline when generated as a
fused sink** (row 07); it is costlier only when the consumer genuinely needs lazy partial access — where
the faithful union is not overhead but the product (stop-early get, row 06, resolves at 0.032% of the
doc; 1.3-1.45× sonic get). The selector between LazyView and DirectSink is per-*consumer*, not
per-grammar, and it already exists in code as `MaterializationDescriptor.kind: TapeKind` (Q5). Only
`struct→value` re-serialization pays the faithful tax in the reverse direction.

## Q4 — shared bytecode instruction stream: a live target the type refutation does not bind

A type-generation refutation (the zero-runtime-cost type-plane invariant; the full-walk-not-2× lesson
that cold native `JSON.parse` beats cold-JIT JS on *full materialization*, SPEC §0 row 14) is about
**values and types**, not a shared instruction *layout*. The offset-tape already proves a byte-exact
runtime layout crosses Rust↔V8 (node-for-node, 320 cold children, row 14). A combinator bytecode is the
isomorphic analogue for the *parser* rather than the *value*: an SoA instruction plane (opcode stream +
operand arena) decoded by a generated interpreter skeleton on each side — structurally identical to the
tape/view mechanism. **The `general_descent` emitter's `write!`-based skeleton generalizes to it with no
new machinery**: swap `KindRecovery` for an `OpcodeDecode` dispatch and `payloads` for an operand arena;
`rule_kind`→`op_at(pc)`, `KIND_TABLE`→an opcode table. Prior art is exact — MLIR ODS generates C++ *and*
Python builders from one op-def; WASM itself is a spec-defined instruction layout with N conformant
decoders.

Its GO/NO-GO is **not 6B's to decide.** It turns on whether a VM beats the closure core in Rust
(LOCK-4C VM-CORE) and whether a TS bytecode interpreter beats native combinator closures under V8's one
megamorphic `.parser(state)` indirect call (`typescript/src/parse/parser.ts:53,82,88…`, R4 §6c — the
central IC dispatching many closure shapes through one function-pointer field; LOCK-8C). **Routing owed
jointly to LOCK-4/LOCK-8:** 6B supplies the generation mechanism + the layout-parity oracle; LOCK-4
measures VM-vs-closure in Rust; LOCK-8 measures interpreter-vs-closure under V8. 6B's standing claim: if
either lock returns a VM win, the shared instruction stream generates from `LayoutSpec` for free; if
both return NO_GO, the target dies as a *measured negative*, not a type-plane consequence — the
refutation never reached it. **The decisive joint probe:** emit a two-opcode stream into a Rust `match
pc` and a TS `switch(pc)` from one spec and assert an identical output digest (the bytecode analogue of
PROVE-14).

## Q5 — ownership of `LayoutSpec`, `emit_*`, and the `MaterializationClass` variable

**The variable the question names already exists, under a different name, in the `passes` crate**
(no `crates/ir/src/types/grammar.rs` exists — a prior-pass path error; verified):
- `MaterializationPlan { descriptors: HashMap<ir::RuleId, MaterializationDescriptor> }`
  (`crates/passes/src/lib.rs:1127`).
- `MaterializationDescriptor { kind: TapeKind, label, shape, fields }` (`:1131-1137`).
- `TapeKind` — the per-rule tape-materialization discriminator (assigned `:1148-1275`).
- Producer `derive_materialization_plan(&GrammarIr) -> MaterializationPlan` (`:1139`, an IR pass over
  the normalized grammar). Consumer `single_plan(…)` folds it via `materialize_rule` into `BackendIr`
  (`:1283-1294`). Decision drivers colocated: `passes/src/decision_csp.rs`, `passes/src/backend_egraph.rs`.

This is the XL-B middle-IR tower made concrete — classifier, decision drivers, and emission fold already
colocated. Proposed assignment:
- **The `MaterializationClass` variable is a LOCK-2 (decision-driver) product, homed on the LOCK-3 IR
  node.** *Which* dialect a rule takes (SoA-derive vs AoS-store; LazyView vs DirectSink; Sparse vs
  DenseTag) is a cost decision — flag-plane density trades scan cost against store cost; faithful-union
  vs fused-sink is a per-consumer cost/legality selection. That is a CSP/e-graph call, and its seat
  *already exists* at `decision_csp.rs`/`backend_egraph.rs`. The type stays `MaterializationDescriptor`/
  `TapeKind` where it lives (`lib.rs:1127-1137`); LOCK-2 extends it with the KindRecovery/
  FlagPlaneDensity/LazyView-vs-DirectSink axes.
- **`LayoutSpec` + the `emit_*` faces belong to LOCK-3 (IR transposition spine / emission tower).**
  `LayoutSpec` is the frozen backend-shape attribute (the product of "materialization is decided"), and
  the emitter is `general_descent` generalized to consume it. Today emitter *selection* is a hard branch
  in `render` (`runtime_generator.rs:78-89`) plus a JSON special-case template; 6B lifts selection into
  a **data variable** carried on the plan, collapsing three ad-hoc emitter entrypoints
  (`emit_sink_only`/JSON template, `emit_general_descent`, `emit_scan`) into one spec-driven emission.
  **The split: LOCK-2 chooses the class (in `decision_csp`/`backend_egraph`); LOCK-3 owns the spec type
  and the `emit_*` tower (`general_descent` extended).** No god module, no orthogonal codepath — the
  selection collapse REMOVES a codepath.

## Integration seams (path:line where extant code serves)

- Shared layout substrate (hand, all grammars): `crates/runtime/src/tape/mod.rs:92-115` (planes+ctor),
  `flags_at` sparse lookup, `:20` (`GRAMMAR_BIT0`); writer `tape/assembler.rs`; nav `tape/cursor.rs`.
- Derive-from-source decoder (JSON, TEMPLATE): `grammars/json/value.rs:12-44` (`JsonNodeKind::at_cursor`),
  `grammars/json/view.rs:146,272-395`; the template itself `codegen/src/json_templates/view.rs` (488 LOC,
  spliced by `runtime_generator.rs:122-130`).
- Stored-tag decoder (Sheets, EMITTED): `grammars/sheets/view.rs:52` (`KIND_TABLE`), `:123-127,147`
  (`rule_kind` via `flags_at().bits()`), `:167,177,195` (`as_f64`/`as_bool`/`as_tag`). The EMITTER:
  `codegen/src/general_descent/view.rs:103-110` (`rule_kind_tag` from rule index), `:117-135+`
  (`build_typed_accessors` from `->` projections), `:281` (`KIND_TABLE` write), `:335` (`rule_kind`
  write); `codegen/src/general_descent/emit.rs` (452 LOC); dispatch `runtime_generator.rs:147`.
- Idiomatic sinks (Q3): `grammars/css_l4_declaration_values/sink.rs` (16 LOC) over `generated.rs`
  (886 LOC); emitters `codegen/src/json_typed_direct.rs` (1245), `codegen/src/json_sink_direct/` (1213).
- The `MaterializationClass` seat (Q5): `crates/passes/src/lib.rs:1127,1131,1139,1283`; drivers
  `passes/src/decision_csp.rs`, `passes/src/backend_egraph.rs`.
- TS mirror to generate (today 364 hand LOC, the sole drift surface): the PROVE-14 `TapeDoc` decoder +
  loader + pointer + `verify.mjs` oracle. Rust plane-export shape proven by `tape_dump.rs` reading
  `Tape::{source,offsets,flag_cursors,flag_values}` (`runtime/src/tape/mod.rs:124-137`). GN-4.W4 home:
  `crates/bbnf-wasm/` (cdylib, four-plane `TapeExport`) + `ts/tape/src/{tape,load,pointer}.ts` +
  `ts/tape/test/tape-parity.test.ts`.

## Prior art (citations)

- **Cap'n Proto / FlatBuffers** — schema-driven codegen emitting a byte layout AND per-language zero-copy
  accessor skeletons from one `.capnp`/`.fbs`: the canonical "one spec → N language skeletons, zero
  drift" precedent. 6B differs by deriving the spec from the *grammar* (not a separate IDL) and by
  carrying multiple discriminator dialects over one layout.
- **Kaitai Struct** — declarative `.ksy` binary-format spec compiled to *parsers* in C++/Python/JS/Rust
  that decode the identical byte stream: the exact analog of a tape decoder (one layout spec, N readers,
  provable byte-parity). 6B's plane-descriptor + KindRecovery axis is a Kaitai-shaped spec specialized to
  the offset-tape.
- **LLVM TableGen** (`.td` → C++ tables/decoders, including `DecoderEmitter` for fixed-width instruction
  decode) — a spec compiled to per-target skeletons with a regen fixpoint; ownership model (spec owned by
  the IR, emitters terminal) mirrors the proposed LOCK-3 seat; `DecoderEmitter` is the direct precedent
  for the Q4 opcode-stream generalization.
- **MLIR ODS (Operation Definition Spec)** — one op-def generates C++ *and* Python builders: direct
  precedent for Q4.
- **regex-automata `DFA::from_bytes` / `to_bytes`** — a byte layout decoded by a generated reader; the
  SoA-plane + sparse side-channel discipline is the same family.
- **simdjson / sonic-rs tape** (stored 64-bit tagged words = AoS stored-discriminator) vs the offset-only
  SoA tape here (derived): the two industrial poles 6B unifies under one `LayoutSpec`.

## Named risks and counterexamples

- **R1 — dense-tag flag plane is not free.** Sheets stores a tag on *every* structural node; JSON stores
  flags on the escaped minority. Forcing `DenseTag` on JSON regresses its win — a `flags_at`
  binary-search per node where source-byte classify is a single load. *Mitigation:* `FlagPlaneDensity` is
  a per-grammar LOCK-2 decision (`decision_csp`), never a global default.
- **R2 — the flag plane cannot carry a discriminator tag AND an escape bit simultaneously** (one `u8`
  slot; `GRAMMAR_BIT0`). A string-escape leaf inside an operator tower needing a rule tag collides — the
  ONE named spec obligation beyond HEAD, resolvable by a spec-declared bit-partition (tag high bits,
  escape `GRAMMAR_BIT0` low), UNBUILT.
- **R3 — spec expressibility gap on payload dialects.** Sheets `as_f64`/`as_bool`/`as_tag` are three
  rule-keyed payload decoders; a `LayoutSpec` modeling only discriminator recovery under-generates.
  *Blocker to file if unmet:* the spec must carry the `PayloadDecode[]` axis, or hand bodies proliferate.
  (The general emitter already derives these from `->` projections, so the axis is realizable.)
- **R4 — generated TS skeleton loses V8 monomorphism.** The hand cross-language decoder is monomorphic
  (4 ctor-only fields, single hidden class, PROVE-14). A generator emitting a post-ctor field assignment,
  or a `switch` V8 cannot lower to a jump table, silently deopts and the 4.4× stop-early evaporates.
  *Counterexample:* a generated `classify` returning a boxed union instead of a small-int megamorphizes
  the call site. *Mitigation:* the Evidence probe below is a hard gate, not a style note.
- **R5 — regen drift undetected / tautology oracle.** If Rust and TS emitters read two copies of the
  spec they drift silently; and a "parity" test of generated-Rust vs generated-TS proves only emitter
  determinism (compares one source against itself). *Mitigation:* one `LayoutSpec` value both emitters
  consume, a `regen∘regen=regen` byte-identity gate, and a NON-tautological external oracle — the
  `JSON.parse` differential (fp `986912547` full / `3431570937` partial) and sonic-rs on the Rust side.
- **R6 — bit-fold non-portability.** Any generated digest proving parity must fold a language-neutral
  quantity; an 8-byte IEEE-754 fold diverges on NaN-canonicalization / signed-zero (this is 6D's live
  bits-vs-spans contradiction — a 6D matter, flagged here for the shared oracle). *Mitigation:* the 6B
  oracle folds code-units, never raw f64 bits.

## What a prototype must test to move this family (the never-built machine)

1. **Fold the JSON template into the general emitter.** Extend `general_descent` with a
   `KindRecovery::FromSourceByte` arm + a `Sparse` flag mode; emit `grammars/json/view.rs` from it and
   assert **byte-identical** to the committed template; keep emitting `grammars/sheets/view.rs`
   unchanged. Proves one emitter, two provenance dialects, from grammar facts — deletes the 488-LOC hand
   template. This is the load-bearing SPEC_ONLY→RAN step 6B still owes (the FAMILY-SPECS' claim that this
   ran as "PROTO-1" is NOT substantiated — the committed PROTO-1 is 6C; no `emit_classify`/`LayoutSpec`
   exists in-tree).
2. **Emit `ts/tape/src/tape.ts` from the same spec** and assert it decodes the crossed planes to the
   identical fingerprint (`986912547` full, `3431570937` partial) — closing the sole real drift surface
   (364 hand LOC → 0).
3. **V8 shape-stability probe (the mandated Evidence).** Run the *generated* `TapeDoc` under
   `node --allow-natives-syntax` and assert `%HaveSameMap(a,b)` across instances, `%HasFastProperties`,
   and `%GetOptimizationStatus` optimized/monomorphic on the hot `classify`/`nextSibling` path —
   matching the hand decoder's confirmed monomorphism, **not** `tsc` type-check alone. A generated
   skeleton failing this is a defect, not a style note.
4. **Regen fixpoint gate.** `regen∘regen=regen`, byte-identical, on both the Rust view faces and
   `tape.ts`, CI-wired. Mutate a Rust flag bit or classify arm; the generated TS must move in lockstep or
   the gate goes RED.
5. **`MaterializationClass` round-trip.** Show `decision_csp` selecting `DirectSink` vs `LazyView` for one
   consumer and the `emit_*` tower producing the corresponding skeleton from one `LayoutSpec`, exercising
   the ownership split (`passes/src/lib.rs:1127` class → emitter) end to end.

Falsifiers that return the family: if step 1 needs *any* hand-authored constant in the generated JSON
view to reach byte-identity, a layout fact escapes derivation (spec incomplete); if step 3 shows the
generated decoder is measurably slower or shape-unstable vs the hand decoder, generation costs more than
the drift it prevents, and the family collapses toward a linter-only null hypothesis for that face.

## Standalone verdict

The tape-layout face is the strongest ground 6B holds, and the ground is real but MIXED: the Sheets
decoder skeleton (`KIND_TABLE`, `rule_kind`, typed accessors) is *already programmatically emitted from
grammar rules and `->` projections* by the 883-LOC `general_descent` emitter — the SPEC-GENERATED-
SKELETON thesis shipping in production — while the JSON view is still a 488-LOC hand template and the TS
mirror is 0% generated (364 hand LOC, the sole real drift surface). The named `LayoutSpec`/`emit_classify`
machine does NOT exist in-tree; the crown remains SPEC_ONLY at the spec level, its in-production kernel
being the general emitter awaiting three additions: a `KindRecovery::FromSourceByte` axis (folds JSON's
template in), a `Sparse` flag mode, and a TS backend (closes the drift surface). AoS↔SoA is discharged by
the tree (two provenance dialects over one layout, one already emitted); Q3's idiomatic type is *cheaper*
than the faithful union when generated as a fused sink (row 07); ownership grounds in real code (LOCK-2
chooses the class in `decision_csp`/`backend_egraph` writing `MaterializationDescriptor`/`TapeKind` at
`passes/src/lib.rs:1127`; LOCK-3 owns the spec + the `general_descent` emitter tower). The shared-
instruction-stream extension is a live target the type refutation does not bind, routed to LOCK-4/LOCK-8
for the VM measurement. **Not BLOCKED** — no missing primitive; the emitter exists, the axes are
enumerated, and step 1 is a bounded extension of shipping code. **Not yet RAN** — the byte-identical
JSON-from-emitter proof and the generated-TS monomorphism probe are the two measurements that convert
6B's crown from SPEC_ONLY to MEASURED.
