# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA (pass-2 research)

*The IR is itself grammar-specified: a bbnf-defined IR grammar whose `->` sum-of-products
projections author the node algebra and its codec; the transpositions of the spine are
grammar-to-grammar maps; the generator eats itself. Homoiousia — "of like substance" — is
the exact claim: the IR is not a foreign data type the generator manipulates, it is another
grammar the generator already knows how to parse, type, and emit.*

Substrate re-pinned to the live tree `skinny/` at repo HEAD `7790134675c` (the sole living
implementation); every `path:line` below was opened on that tree. Designs target greenfield
(GN-1 loom); ports scope organ-level.

---

## 0. The formulation is already half-built on live — and its precedent is bbnf itself

The homoiousia claim is not speculative for this codebase: **bbnf is already self-hosted as a
bbnf grammar.** `grammar/bbnf/{bbnf.bbnf, expressions.bbnf, types.bbnf}` describe bbnf in bbnf,
*with typed `->` projections already in use*:

- `grammar/bbnf/bbnf.bbnf`: `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span ;`, `literal = (...) -> Span ;`,
  `regex = ( "/" , /(\\.|[^\/])+/ , "/" ) -> Span ;` — the meta-grammar projects to Rust leaf types.
- `grammar/bbnf/bbnf.bbnf`: `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ? ;`
  — the `->` : type syntax the whole type plane rests on is itself declared in the self-host grammar.
- `grammar/bbnf/types.bbnf`: `type_name = "u8" | ... | /[_a-zA-Z][_a-zA-Z0-9]*/ ;` with the header
  comment *"Includes primitive types and arbitrary identifiers (for IrNode, etc.)."* — **the grammar
  tier already anticipates `->`-projecting rule outputs onto IR node types.** This textual hook is the
  nearest-to-live evidence that a grammar authoring an IR algebra is the designed intent, not a novum.

And bbnf already describes *other* grammar languages as grammar: `grammar/ebnf/ebnf.bbnf`,
`grammar/bnf/bnf.bbnf`, `grammar/misc/g4.bbnf`, `grammar/misc/regex.bbnf`. The grammar-of-grammars
capability is proven. What none of these do — the exact gap this family names — is project the
*alternation of node kinds* into a single discriminated Rust **enum**. They project per-arm leaves
(`-> Span`) but the sum-of-arms collapses structurally, never into a typed sum. That gap is one
schema construct: `DirectTypeKind::Sum` (§2).

## 1. What "the IR expressed as a grammar" is, concretely

The live IR tower has two node algebras, both hand-written in `crates/ir/src/lib.rs`:

- **`ExprKind`** (front IR, `ir/lib.rs:234-271`) — a **10-arm sum**: `Seq · Alt · Repeat · Optional ·
  Literal · Regex · Ref · Annotation · Project · Discard`. Each arm is a product (struct/tuple).
- **`BackendExpr`** (back IR, `ir/lib.rs:376-413`) — a **13-arm sum**: `Entry · Seq · Alt · RepeatLoop ·
  OptionalBranch · ByteLiteral · RegexProgram · CallRule · SpanMark · TapeEmit · DirectBuild ·
  ValueProject · Return`, plus the leaf enums `TapeKind` (9-arm, `:453`), `DirectBuildField`/`Target`/
  `TypeRef`/`Scalar`/`Presence`/`Cardinality`/`Representation`/`Decode` (`:465-535`).

Homoiousia asserts these algebras are **derivable from a grammar** — an `ir.bbnf` (net-new) whose rules
are the node kinds and whose `->` projections declare the Rust payload types, e.g.:

```
expr  = seq | alt | repeat | optional | literal | regex | ref | annotation | project | discard
        -> ExprKind ;                          // <- the SUM projection (needs DirectTypeKind::Sum)
seq   = "Seq"   , exprids                       -> Seq(Vec<ExprId>) ;
alt   = "Alt"   , branches , mode               -> Alt { branches: Vec<ExprId>, mode: AltMode } ;
regex = "Regex" , pattern                        -> Regex { pattern: String } ;   // opaque leaf, XL-C
...
```

Feed `ir.bbnf` through the *existing* direct-to-struct typed emitter (the PROVE-07 organ:
`emit_typed_from_source`, `codegen/src/lib.rs:130` → `lower::schema_direct::lower_program`,
`codegen/src/lower/schema_direct.rs:11` → `json_typed_direct::render`, `codegen/src/json_typed_direct.rs:10`)
and the generator emits the `ExprKind`/`BackendExpr` Rust definitions **plus their serde codec** —
today hand-written at `ir/lib.rs:234` and `:376` with `#[derive(Serialize, Deserialize)]`. The generator
now generates its own IR types. That is "the generator eats itself" reduced to one concrete artifact.

**Regex stays opaque (XL-C alignment, no ONE-HIR dependency).** Live already agrees: `ExprKind::Regex {
pattern: String }` (`ir/lib.rs:250-252`) is a bare string; the self-host grammar's `regex` rule projects
`-> Span`. The IR grammar carries `Regex(pattern: String)` as an opaque leaf variant of the sum — it does
not absorb regex's internal HIR, so this family no longer hangs on any ONE-HIR verdict.

## 2. The single gate the sum-node algebra requires (the crux, product-only forbidden)

`ExprKind` (10-arm) and `BackendExpr` (13-arm) are **sums**. The schema IR that the typed emitter consumes
is **Struct-only**: `crates/codegen/src/direct_schema.rs:52-58`:

```rust
pub enum DirectTypeKind { Struct { fields, ignored_fields, unknown_fields } }   // one variant
```

The renderer matches only this arm: `json_typed_direct.rs:74` (`match &ty.kind { DirectTypeKind::Struct {..} }`),
`render_construct` at `:313`, `render_field_arm` at `:435` — no sum arm exists. So today the IR-as-grammar can
express only the *product* subset (the per-node field records), never the discriminated union that IS
`ExprKind`. **The directive's warning binds: the product-only, sum-excluding subset does NOT stand for the
formulation.** An IR grammar that emits ten independent structs but not the `enum ExprKind` over them has not
expressed the IR — it has expressed ten leaves and lost the algebra.

Therefore the family's foundational gate is exactly one net-new schema construct:

> **GATE-1 · `DirectTypeKind::Sum { variants: Vec<DirectSumVariant>, unknown }`** — beside the Struct arm
> at `direct_schema.rs:52`; `validate()`/`validate_type_ref` grow a sum arm (each variant payload resolves,
> tags duplicate-free); `json_typed_direct::render` grows a `render_sum_arm` sibling to `render_field_arm`
> (`:435`) emitting a Rust `enum` + a first-byte / tag `match` dispatch reusing the live discriminator idiom.
> **Owner: GN-3.W1.** Carried as a born-RED dependency edge — this family cannot render its own sum without it.

This is not incidental overlap: GN-3.W1 lands `DirectTypeKind::Sum` for `value = object|array|string|number|
bool|null`. That is *structurally the same construct* an IR grammar needs for `expr = seq|alt|...`. Family 3C
is the general consumer of the sum node GN-3 introduces for JSON — which is precisely how a substrate earns a
consumer under Y.13.

## 3. The threaded `->` type at the infer edge — how 3C CONSUMES it (not re-derives)

The shared question: `passes/src/lib.rs:204` — `ExprKind::Project { body, .. } => self.infer_expr(*body)?` —
drops `target` and `ty` via `..`, and the byte-literal role classifier `derive_materialization_roles`
(`passes/src/lib.rs:1403`) independently *re-invents* the JSON node taxonomy from `{`/`}`/`[`/`]`/`true`/`false`/
`null` literal gates. 3C's consumption contract is exact and eliminative:

- The threaded node-type at `:204` (the `Project.ty`) becomes the **payload `type_ref` of a sum variant**.
  When `ir.bbnf` declares `alt = ... -> Alt { branches: Vec<ExprId>, mode: AltMode }`, the `-> Alt{...}`
  *is* the per-arm payload the sum node stores as `DirectSumVariant.payload`. Threading `:204` populates
  that payload; it is never re-derived.
- Because the sum variants carry *declared* payloads, **`derive_materialization_roles` (`:1403`) is RETIRED,
  not relocated** — the byte-literal heuristic classifier exists only because the grammar did not declare its
  node kinds. Once the IR grammar declares them, the role classifier has nothing to classify. This is the
  family's distinctive lever: homoiousia dissolves the heuristic taxonomy by making the taxonomy *declared*.
- Contrast the live lossy path: `general_descent/view.rs:20-46` folds every `->` to a 4-way `ProjectionKind
  {Float,Bool,Tag,Span}`. 3C reads the threaded leaf as a sum-variant payload of arbitrary depth (nested
  refs, cardinality from `Repeat`/`Optional`), never the 4-way fold.

The consumption is the same *wire* GN-3.W2 threads (into `DirectBuildTarget.type_ref`, currently always `None`
at `passes/src/lib.rs:1149-1287`); 3C differs only in reading it as **sum-variant payloads** rather than
struct-field targets. **GATE-2 · thread `Project.{target,ty}` at `:204` into the schema. Owner: GN-3.W2.**

## 4. The transpositions as grammar-to-grammar maps — the edge-DAG on live

The live spine has four labeled transposition edges; each has a grammar source and a grammar (or text) target:

| edge | live site | source → target | homoiousia form |
|---|---|---|---|
| E1 | `grammar::parse_grammar` (R1 §2, `codegen/lib.rs:102`) | `.bbnf` text → `GrammarIr` | grammar → IR (already a parse) |
| E2 | `passes::Infer` (`passes/lib.rs:~200`) | `GrammarIr` → type facts | IR → type plane (LOCK-1) |
| E3 | `lower_expr` + **`materialize_rule`** (`passes/lib.rs:1349`, `:1357-1391`) | `GrammarIr` → `BackendIr` | **IR → IR: the canonical grammar-to-grammar map** |
| E4 | `lower::lower_to_rust` → `runtime_generator` (R1 §2) | `BackendIr` → Rust text | IR → emitted source |

The value levels at E3 are enumerated concretely by `materialize_rule` (verified `passes/lib.rs:1357-1391`):
it splices `SpanMark(Start) · body · SpanMark(End) · TapeEmit{kind} · DirectBuild{shape,fields} · Return`.
This splice **is already a declarative structural map** — a fixed template keyed by a `MaterializationDescriptor`.
Homoiousia recasts it as a bbnf-expressed rewrite `rule -> [SpanMark(Start), body, SpanMark(End), TapeEmit,
DirectBuild, Return]` over the `expr`→`backend_expr` grammars. PROVE-12 (REPORT: GO, byte-identical, 3.7-6.4%
fewer ins/B) is the standing evidence that emit-spine transpositions are *already* partly declarative and
residualizable — its delimiter-set → const-mask fold is a grammar-constant-to-residual map on E4.

**But the transposition-as-rewrite has no live consumer of its own.** `materialize_rule` is imperative Rust
driven by `MaterializationPlan`. A *declaratively-specified* rewrite is consumer-less on live unless an engine
consumes it. Two honest routes, both born-RED with named owners:

> **GATE-3 · the E3 rewrite engine.** Either (a) LOCK-2's IR-rewrite engine (2B/2C) consumes the declared
> rewrite and reproduces the imperative splice byte-identically on the JSON/CSS/sheets IR; or (b) LOCK-7's
> meta-circular oracle consumes it as a differential check against the hand splice. **Owner: LOCK-2 (engine)
> ∥ LOCK-7 (oracle).** Absent either, the *node-algebra* half of 3C (§1-3) stands alone and the *transposition*
> half is deferred, not shipped as consumer-less substrate (constitution viii).

## 5. The named downstream consumer (the Y.13 answer)

The directive demands a named downstream wave that consumes an IR-expressed-as-a-grammar. Three, in order of
nearness-to-live:

1. **The generator's own IR type emission + serde codec.** `ExprKind`/`BackendExpr` and every leaf enum derive
   `Serialize/Deserialize` (`ir/lib.rs:226,233,375`, etc.). Under 3C, `ir.bbnf` + the typed-direct emitter
   generate these definitions and a grammar-generated JSON codec that *replaces `serde_derive` on the IR* — the
   IR becomes just another typed-JSON document whose schema is a grammar, parsed by the same organ that parses
   twitter/citm/canada. Consumer = the generator itself. Near-live caveat: no live regen site currently
   serde-round-trips `BackendIr` through JSON (grep over `xtask/`, `codegen/` finds no `serde_json` over
   `BackendIr`), so this consumer is *compiled-but-unexercised* today — a born-RED edge whose owner is
   GESTALT-B / LOCK-7, not a live loop. Honesty demands: this is the strongest *design* consumer, not yet a
   live one.

2. **LOCK-7 · 7B TOTAL-SELF-HOST** names the consumer verbatim in the constitution's own words: *"bbnf grammar,
   regex grammar, **IR grammar**, emitter templates — all grammar-specified; the generator becomes mostly data."*
   The IR grammar is an explicit member of the 7B self-host plane. **This is the named downstream wave.** 3C is
   LOCK-3's contribution of the *IR grammar* that 7B assembles; the alignment is stated in the 3C charter
   ("bootstrap alignment with LOCK-7").

3. **GESTALT-B** weaves LOCK-1/2/3/7: it is the wave that collapses "one solver home / one e-graph home" and
   consumes the IR grammar as the shared substrate the type plane (LOCK-1), the rewrite engine (LOCK-2), and the
   bootstrap (LOCK-7) all read.

So the family's disposition is: **the node-algebra half (§1-3) has a concrete same-wave consumer via GATE-1's
render arm and GATE-2's thread (both owned by GN-3), and a named downstream consumer in LOCK-7.W-7B / GESTALT-B.
The transposition half (§4) is a born-RED edge owned by LOCK-2/LOCK-7.** No consumer-less substrate ships.

## 6. Integration seams (path:line, live tree)

- Sum gate: `crates/codegen/src/direct_schema.rs:52-58` (Struct-only) · `validate` therein · renderer
  `crates/codegen/src/json_typed_direct.rs:74,313,435` (Struct-only match; `render_sum_arm` absent).
- Typed-direct organ (the emitter the IR grammar rides): `codegen/src/lib.rs:130` `emit_typed_from_source`;
  bridge `codegen/src/lower/schema_direct.rs:11` (validates + copies the shape roster — does NOT derive);
  render `codegen/src/json_typed_direct.rs:10`. `emit(backend)` fail-closed at `codegen/src/lib.rs:114`.
- The hand-authored schema homoiousia dissolves: `xtask/src/real_typed_schema.rs:7` (1014 LOC, the anti-pattern
   — a human writing beside the grammar what the grammar should *be*).
- IR node algebras: `ir/lib.rs:234-271` (`ExprKind` 10-arm) · `:376-413` (`BackendExpr` 13-arm) · `:453-463`
  (`TapeKind`) · `:465-535` (DirectBuild schema) · opaque regex `:250-252`.
- Infer discard + role heuristic + splice: `passes/src/lib.rs:204` (drop) · `:1349` (`Project`→`ValueProject`)
  · `:1357-1391` (`materialize_rule` splice, verified) · `:1403` (`derive_materialization_roles`, retired by 3C)
  · `:1149-1287` (`DirectBuildField.target` hardcoded `None`).
- Self-host precedent (the homoiousia proof-of-concept already in-tree): `grammar/bbnf/bbnf.bbnf`,
  `grammar/bbnf/expressions.bbnf`, `grammar/bbnf/types.bbnf` (the "IrNode, etc." hook); grammar-of-grammars
  `grammar/ebnf/ebnf.bbnf`, `grammar/bnf/bnf.bbnf`, `grammar/misc/{g4,regex}.bbnf`.

## 7. Prior art (SOTA declarative-IR generation)

- **MLIR ODS (Operation Definition Specification) — the closest peer.** IR operations, dialects, verifiers, and
  builders are declared in TableGen `.td` and *generated* into C++ op classes (`mlir-tblgen`). This is precisely
  grammar-as-IR: a declarative op-definition DSL authors the node algebra; hand C++ shrinks to kernels. The
  win MLIR reports — adding an op is data, not code — is exactly 3C's parsimony thesis. Citation for "the IR is
  itself specified and the generator generates its types."
- **LLVM TableGen / SelectionDAG ISel patterns.** `Pat<(dag in),(dag out)>` are *declarative grammar-to-grammar
  rewrites* from the SelectionDAG to machine instrs — the strongest prior art for §4's transposition-as-rewrite:
  ISel is a term-rewriting map compiled to a matcher table, not hand-written per case. It also carries the risk
  lesson (§8): the pattern language needs an engine (the matcher-table generator) — consumer-owned.
- **tree-sitter `node-types.json`.** tree-sitter emits, *from the grammar*, a machine-readable schema of every
  node kind as a tagged sum (`"type": "...", "subtypes": [...]`). This is a shipped instance of "generate a sum
  schema of node kinds from a grammar" — direct precedent for GATE-1's sum schema and the node-algebra emission.
- **egg / egglog.** Rewrite rules over a `Language` term algebra are declarative and engine-consumed — the model
  for §4 route (a): the E3 rewrite as rules, an equality-saturation engine as consumer (LOCK-2 alignment).
- **GHC "Trees That Grow" (Najd & Peyton Jones).** A single AST algebra indexed by compilation phase via type
  families — the reference for a *phase-indexed single IR* whose node set is one specification refined across
  passes, informing whether `ExprKind` and `BackendExpr` are one grammar phase-indexed or two grammars mapped.
- **Racket `#lang` / macro-defined languages.** The canonical "the language eats itself" — a language whose own
  surface and expander are defined in the language. Homoiousia is the same move for bbnf's IR.

## 8. Named risks and counterexamples

- **R-1 · Product-only mirage (the directive's explicit trap).** Emitting the ten node *structs* without the
  `enum ExprKind` over them satisfies a naive test yet is NOT the IR. Guard: the prototype's acceptance asserts a
  top-level `DirectTypeKind::Sum` with exactly 10 (ExprKind) / 13 (BackendExpr) variants; a Struct-only emit is RED.
- **R-2 · Arena recursion, not owned recursion.** Live IR is an arena: `Seq(Vec<ExprId>)`, `ExprId = u32` index —
  *not* `Seq(Vec<Box<Expr>>)`. A naive `seq -> Seq(Vec<Expr>)` projection would emit an owned tree and diverge
  from live. The IR grammar must project `ExprId` as a scalar index type (`DirectTypeRef::Scalar`/`Type("ExprId")`),
  making the schema an **arena schema**. Counterexample that must fail closed: any projection that boxes a child.
- **R-3 · Consumer-less transposition.** §4's grammar-to-grammar rewrite has no live engine. If neither LOCK-2 nor
  LOCK-7 consumes it in-wave, it is forbidden substrate. Guard: ship §1-3 (node algebra, consumer = GN-3 render
  arm) independently; carry §4 as a born-RED edge with LOCK-2/LOCK-7 as owner. Never merge them into one claim.
- **R-4 · Generic-JSON stays heuristic (inherited supersession).** `grammars/json.bbnf` has **zero `->`**; its
  taxonomy is recovered by the byte-literal role classifier (`passes/lib.rs:1403`). Homoiousia of the *emit
  transposition* is reachable only for grammars that *declare* their `->` shapes (the typed grammars GN-3.W3
  authors), not for generic `json.bbnf`. 3C inherits GN-3's supersession verbatim: the refutation ("a generic
  grammar cannot recover document shapes") stands; 3C changes the grammar, not the refutation. The IR grammar
  `ir.bbnf` is itself a *typed* grammar (it declares its `->`), so this does not block the node-algebra half —
  but it does forbid claiming the generic-JSON emit path as homoiousia.
- **R-5 · Byte-identity of the generated IR types.** The emitted `enum ExprKind` must match `ir/lib.rs:234`
  field-for-field (including `#[serde]` attributes and `ExprId` indices), or the generator changes its own ABI.
  Guard: the prototype diffs generated-vs-hand structurally per variant; capacity/ordering canonicalization
  (sum-variant order-insensitivity) is mandatory or derived ≠ hand (the GN-3.W1 R5 order lesson applies).

## 9. What a prototype must test to move this family

- **P1 (RED→GREEN on GATE-1).** Author a minimal `ir.bbnf` declaring `ExprKind`'s 10-arm sum with `-> Ty`
  projections (using the `types.bbnf` IrNode hook and `ExprId` as an arena scalar). Run it through
  `emit_typed_from_source`. Assert the derived `DirectSchemaSet` has a top `DirectTypeKind::Sum` of exactly 10
  variants + 10 Struct arms + scalar `ExprId` refs (not boxed children). RED until `DirectTypeKind::Sum` exists —
  this test *is* the born-RED dependency on GN-3.W1.
- **P2 (the self-eating proof).** Extend `json_typed_direct::render` with `render_sum_arm`; render the P1 schema;
  assert the emitted `enum ExprKind` (a) parses as valid Rust (`syn`/`trybuild`) and (b) is structurally
  byte-identical to the hand `ir/lib.rs:234` enum. GREEN = the generator emitted its own front-IR type.
- **P3 (the live consumer).** Generate the IR's JSON codec from `ir.bbnf` (the typed-direct decode path);
  round-trip a real `BackendIr` (serialize via serde, decode via the grammar-generated parser) and assert parity
  with `serde_derive::Deserialize`. GREEN establishes the *named live consumer* the directive demands — the IR's
  own codec — and the path to retiring `serde_derive` on the IR. This is the single measurement that converts 3C
  from "designed consumer (LOCK-7)" to "exercised consumer (regen round-trip)."
- **P4 (transposition, born-RED, LOCK-2/7 owner).** Express `materialize_rule`'s splice (`passes/lib.rs:1357-1391`)
  as a declarative `expr→backend_expr` rewrite; show an engine reproduces the imperative splice byte-identically
  on the JSON *and* CSS *and* sheets IR (the three live emit paths), with `derive_materialization_roles` (`:1403`)
  deleted. Failure semantics: any non-byte-identical emit or any surviving role heuristic is RED.

**Zero runtime footprint (constitution v).** Every artifact above is compile-time: `ir.bbnf` + the typed emitter
run in the generator; the emitted IR types and codec replace hand-written generator code, not runtime code. The
invariant gate is `git diff --name-only <base>..HEAD | grep crates/runtime/` empty — identical to GN-3's law.
The type plane costs zero at runtime by construction.
