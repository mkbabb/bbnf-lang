# LOCK-3 · PASS-2 · FAMILY 3C — GRAMMAR-HOMOIOUSIA

*The IR is itself grammar-specified; transpositions are grammar-to-grammar maps; the
generator eats itself.* Homoiousia — "of like substance" — is the exact claim: the IR is
not a foreign data type the generator manipulates, it is another grammar of the same
sum-of-products algebra the generator already parses, types, and emits. Developed to
strongest form, alone.

**HEAD pinning.** Recon and every `path:line` below verified against the live working
tree at `bbnf-lang@c93fc43143743ffaf54855c526f6aeab79227352` (skinny is a subdirectory of
that repo, sharing its git). The pass directive names `skinny@b3cb8c16b` as the
measurement substrate; that commit was not checked out (it would disturb the tree). The
three surfaces this family hinges on — `ExprKind` (`ir/lib.rs:234-271`), `BackendExpr`
(`ir/lib.rs:376-413`), `DirectTypeKind` (`direct_schema.rs:52-58`) — are byte-for-byte
those R1/R6 cite at their pins (`7f3be27c2`, `c255baf21`), so the algebra is stable across
these trees; a prototype re-pins to a named frozen tree and states the delta before it
measures (XL-H).

---

## 0. The formulation is already half-built on live — bbnf is self-hosted, with `->`

The homoiousia claim is not speculative for this codebase: **bbnf is already described in
bbnf, with typed `->` projections in live use.** `grammar/bbnf/{bbnf.bbnf, expressions.bbnf,
types.bbnf}`:

- `bbnf.bbnf:9` `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span ;`, `:15`
  `regex = ( "/" , /(\\.|[^\/])+/ , "/" ) -> Span ;` — meta-grammar rules project to Rust
  leaf types via `->`.
- `bbnf.bbnf:42` `mapped_factor = factor , ( "->" ?w , ( value_expr , type_annotation ? ) ) ? ;`
  — the `-> value : type` syntax the entire type plane rests on is itself *declared in the
  self-host grammar*.
- `types.bbnf:2-3` `// Includes primitive types and arbitrary identifiers (for IrNode, etc.).`
  above `type_name = "u8" | "u16" | … | /[_a-zA-Z][_a-zA-Z0-9]*/` — **the grammar tier's
  type vocabulary already names `IrNode` as an intended projection target.** This is the
  nearest-to-live textual evidence that a grammar authoring an IR node algebra is designed
  intent, not a novum.
- bbnf also describes *other* grammar languages as grammar: `grammar/ebnf/ebnf.bbnf`.

**But the honest boundary.** Skinny's *production* front-end is the hand-written
`parse_grammar` (`crates/grammar/src/lib.rs`, 2142 LOC, R1 §1), and `bbnf.bbnf` is consumed
as a **differential oracle** in the outer repo (gorgeous) — the LOCK-7 7D "meta-circular
oracle" pattern — **not** as the production parser or generator. Self-description-as-oracle
is live; self-description-as-generator is not. And there is **no `ir.bbnf`**
(`find … -name '*.bbnf'` → only bbnf/ebnf/domain grammars); the IR node algebra is
hand-defined Rust at `ir/lib.rs:234,376`. So the family's *germ* is live and ratified in
intent; its *reach* is net-new and consumer-gated (§5).

---

## 1. The claim, stated precisely — one algebra, four tiers, one broken rung

Homoiousia here is not homo-**ousia** (one identical IR — 3B's reading). It is that **every
tier of the spine is the same algebra**: a sum-of-products term language over a fixed
constructor set, so a lowering is a structure-preserving map between two grammars of that
algebra, not a procedure that *recovers* structure by heuristic. The spine already carries
that algebra three times, near-isomorphically:

| Tier | Type | Sum | Product | Repeat | Optional | Ref | Leaves |
|---|---|---|---|---|---|---|---|
| grammar AST / HIR | `ExprKind` (`ir/lib.rs:234`) | `Alt{branches,mode}` | `Seq(Vec)` | `Repeat{min,max}` | `Optional` | `Ref{name,target}` | `Literal`/`Regex`/`Project`/`Discard`/`Annotation` |
| backend shapes | `BackendExpr` (`ir/lib.rs:376`) | `Alt{mode,branches}` | `Seq(Vec)` | `RepeatLoop{min}` | `OptionalBranch` | `CallRule{callee}` | `ByteLiteral`/`RegexProgram`/`SpanMark`/`TapeEmit`/`DirectBuild`/`ValueProject`/`Entry`/`Return` |
| value/type schema | `DirectTypeKind`+`DirectTypeRef` (`direct_schema.rs:52,93`) | **ABSENT** | `Struct{fields}` | `Vec{inner,cap}` | `Option` | `Type(String)` | `Scalar`/`MapString`/`MapEntriesVec`/`StringEnum` |

`ExprKind` and `BackendExpr` are the same 5-constructor core (sum, product, repeat,
optional, ref) plus tier-specific leaves. **The schema tier is the broken rung**:
`DirectTypeKind` (`direct_schema.rs:52-58`) has exactly one variant, `Struct` — product,
list, and option are representable; **sum is not**. The recursive `value = object | array
| string | number | bool | null` that is a two-line `Alt` in `ExprKind` has *no
representable image* in the schema tier. This is the single fact the family turns on, and
the directive names it exactly: the sum-node algebra "stays gated on a schema construct
beyond today's Struct-only `DirectTypeKind`."

---

## 2. The broken rung *begets* the name-gates and the heuristic classifier

The two constructs the constitution most wants dead (clause vi: no name-gates; §4:
abrogate before patch) exist **because** the sum cannot flow structurally through the
schema rung. On live:

- `grammars/json.bbnf` carries the sum `value = object | array | string | number | bool |
  null` as an ordinary `Alt`, but has **zero `->` projections** (R6 §0) — the frontend
  attaches no type to the alternation, and the schema tier could not hold one anyway.
- So the JSON value taxonomy is **re-invented downstream by heuristic**:
  `derive_materialization_roles` (`passes/src/lib.rs:1403-1490`) classifies rules by
  byte-literal pattern — `{`+`}`→container, `[`+`]`→sequence, `SpanKind::String`→string,
  `true`&`false`→bool, `null`→null, "≥4 value-target refs ⇒ `value`" (`:1462`). This is the
  sum's variant set, reconstructed by pattern-matching the concrete syntax it was compiled
  *from* — an inverse the structural map makes unnecessary.
- Because two incompatible materialization shapes then exist (JSON's invented sum vs
  Sheets' general fold), a **name-gate** picks between them: `sink_only.has_rule("value")`
  (`runtime_generator.rs:99`) routes on the literal rule name `value`; its comment claims
  "shape-based," it keys off a string.
- Where `->` *does* reach emission (Sheets general-descent) it is folded to a 4-way `enum
  ProjectionKind {Float,Bool,Tag,Span}` (`general_descent/view.rs:20-46`) — the sum
  collapsed to four flat tags because there is nowhere richer to put it.

The family's mechanism is therefore not "add a feature." It is: **close the schema rung so
the sum transposes structurally, and the heuristic classifier + the name-gate + the 4-way
fold have nothing left to do** — retirement (directive's word), not relocation.

---

## 3. The mechanism, made concrete

**M1 — the sum node (the rung).** Add `DirectTypeKind::Sum { variants: Vec<DirectSumVariant>,
unknown: UnknownVariantPolicy }` beside `Struct` at `direct_schema.rs:52-58`, where
`DirectSumVariant { tag_literal, rust_variant, payload: DirectTypeRef }`; extend
`validate()` (`direct_schema.rs:162`) and `validate_type_ref` (`:293`) — every payload
resolves, tags duplicate-free. The four-tier table (§1) now has no broken rung: `Alt ↔ Sum`
is a representable correspondence at every tier.

**M2 — the structural transposition.** With M1, the lowering grammar-`Alt` → typed-`Alt` →
schema-`Sum` → backend-`Alt` is a **fold that preserves the constructor**, not a classifier
that guesses it. Variant *payloads* are branch types; variant *tags* are each branch's
first-set discriminant — the same byte dispatch `JsonNodeKind::at_cursor` already computes
at `grammars/json/value.rs:28-47`. The unification/e-graph engine that reconciles branch
types is LOCK-1/LOCK-2's seat, not ours; this family owns only *that the algebra is closed
under the transposition* and *where the fold sits* (the driver seat, LOCK-3's charter).

**M3 — the retirements (measured, not asserted).** Once M2 carries the sum:
`derive_materialization_roles` (`passes/src/lib.rs:1403`) is **deleted** — the variant set
is read from `Alt`, not pattern-matched; `has_rule("value")` (`runtime_generator.rs:99`) is
**deleted** — routing is on schema shape (`Sum` vs `Struct`), a real datum; the 4-way
`ProjectionKind` fold (`general_descent/view.rs:20-46`) is **deleted** — accessors read the
resolved variant payload. The prototype's bar (§8): prove these are deletions with negative
net-LOC, not code relocated inside a fold; report the emission-extensibility collapse
(directive's 5→2 target — the five `BackendShape` arms' routing becomes a schema-shape
switch).

---

## 4. The consumer question — the directive's central demand

The directive forbids "the product-only, sum-excluding subset standing for the
formulation" and demands *a named downstream wave that would consume an IR expressed as a
grammar, on or near live*. Answered in two layers, honestly separated.

### 4a. The near-live consumer: **GN-3, THE TYPE PLANE** (`docs/tranches/sk-v24/GN-3.md`)

GN-3 is the extant-codex wave that consumes exactly this substrate and supplies the
born-RED dependency edges with owners the directive asks for. **Family 3C is the invariant
GN-3 instantiates** — GN-3 is grammar-homoiousia executed on the value/type plane:

- **GN-3.W1 (The Sum Node)** lands `DirectTypeKind::Sum` — M1 verbatim — and its **in-wave
  consumer** (Y.13 / constitution viii) is `json_typed_direct::render`'s new sum arm: an
  emitted Rust `enum`, one arm per variant, first-byte `match`, exercised by a
  `trybuild`/`syn::parse_file` well-formedness gate (`GN-3 §4 W1`). Born-RED: the test
  names `Sum` and does not compile until the rung lands end-to-end (IR → validate → render
  → parses). **This is the evidence the sum node has a live consumer** — a renderer that
  exists today (`json_typed_direct.rs:10`, 1245 LOC) and can currently emit only structs.
- **GN-3.W2 (The Threaded Arrow)** is the `->` consumption contract (shared directive §"the
  `->` type value at the infer edge"). It replaces the discard at `passes/src/lib.rs:204`
  (`Project{body,..} => infer_expr(body)` throws away `target`+`ty`) with an arm that
  **unifies the projected leaf into the rule's inference variable**, populating
  `DirectBuildField.target` (today hardcoded `None`, R6 §5d). This family **consumes** that
  one threaded node-type as the payload type of the sum variant it belongs to — it does
  **not** re-derive it (the directive's exact requirement: "state how it CONSUMES one
  threaded node-type rather than independently re-deriving it"). In-wave consumer: the
  Sheets general-descent emitter, whose regen byte-identity is W2's gate — deleting the
  4-way fold without the threaded replacement breaks Sheets (the gate's teeth).
- **GN-3.W3 (The Authored Shape)** makes the schema *grammar-derived*: typed grammars
  (`twitter_search.bbnf` &c.) author document shapes as `->` chains, `type_plane::derive`
  emits the `DirectSchemaSet`, and the 1014-LOC hand file `real_typed_schema.rs` is
  **deleted** with the build required to compile. This *is* "the transposition is a
  grammar-to-grammar map": document grammar → schema → emitter, no hand schema between.
  In-wave consumer: `regen_real_typed` (`xtask/src/main.rs:240`) writing the generated
  typed-decode module the bench binary compiles and runs.

Named consumer chain: **GN-3.W1 → W2 → W3**, each with an in-wave consumer and a born-RED
gate, each an edge this family's substrate feeds. The correct disposition of 3C is *not* a
rival subsystem — it is the architectural justification and generalization of GN-3.

### 4b. The far consumer in the shared directive is **greenfield, not live**

The shared directive routes the threaded `->` to `TypeAnalysis::from_ir` at
`backend/driver/analysis.rs` (XL-F). **Verified: this does not exist on live** —
`rg 'TypeAnalysis|fn from_ir'` returns zero hits; no `analysis.rs` exists under any crate.
It is a greenfield target the port must introduce. Honest disposition: the threaded
node-type's *near-live* consumer is `DirectBuildField.target` (§4a W2), an always-`None`
field ready to receive; `TypeAnalysis::from_ir` is a **born-RED edge owned by the GN-1 loom
/ driver-unification wave**, not evidence of a live consumer. This family must not claim it
as standing.

---

## 5. The total-homoiousia reach (`ir.bbnf`) — born-RED edge, not asserted

The strongest form of "the IR is itself grammar-specified / the generator eats itself" is a
**meta-grammar `ir.bbnf`** whose rules *are* the `ExprKind`/`BackendExpr` constructor
algebra, from which codegen emits both the enum definitions and their fold/visitor
skeletons — bootstrap alignment with LOCK-7 (charter). State honestly:

- **The precedent is live** (§0): bbnf-in-bbnf with `->`, and `types.bbnf:2` already names
  `IrNode` as a projection target. The intent is ratified in the source.
- **But there is no consumer.** An `ir.bbnf` that regenerates the enums has *no consumer*
  unless a bootstrap fixpoint gate (`regen∘regen = regen`, byte-identical — LOCK-7 7A)
  consumes it. No such gate exists on the IR tier today. The consumer-less-substrate law
  (constitution viii, Y.13) bites hardest here: an `ir.bbnf` authored without its fixpoint
  owner is precisely the forbidden substrate. Therefore total homoiousia is **carried as a
  born-RED dependency edge owned by LOCK-7 / GN-6 (bootstrap)** — its evidence obligation is
  a named LOCK-7 wave whose fixpoint gate reads the `ir.bbnf`-generated enums. Absent that
  owner, this family declines to author `ir.bbnf` speculatively.

### The XL-C boundary (ratified) — the algebra is closed *modulo the opaque regex leaf*

XL-C keeps the grammar tier's `Regex(StringId)` opaque, dissolving any ONE-HIR dependency.
On live, `ExprKind::Regex{pattern:String}` (`ir/lib.rs:250`) and
`BackendExpr::RegexProgram{pattern,span_kind}` (`:389`) are **opaque leaves** — the regex
sub-language does *not* decompose into the same constructors. So homoiousia is total over
`{Sum,Product,Repeat,Optional,Ref}` and **stops at the regex leaf by ratified design**. A
named boundary, not a defect; any claim of "one algebra everywhere" that elides
`ir/lib.rs:250` is refuted there.

---

## 6. Prior art (with citations)

- **Nanopass (the keystone).** Sarkar–Waddell–Dybvig, *A Nanopass Infrastructure for
  Compiler Education*, ICFP 2004; Keep–Dybvig, *A Nanopass Framework for Commercial Compiler
  Development*, ICFP 2013. Each IR is a **grammar** (`define-language`, extending a prior
  language by delta); each pass is a `define-pass` **grammar-to-grammar map** with catch-all
  boilerplate auto-derived and malformed IR caught by the framework (Chez Scheme, in
  production). This is 3C's exact thesis. **Lesson consumed:** the win is structural maps +
  auto-derived congruence; **the cost is language proliferation** — dozens of near-identical
  languages — colliding with 3B's "fewest transpositions" and parsimony §vi. 3C must close
  ONE algebra over the fixed tower, NOT mint a language per pass.
- **MLIR ODS + PDL/PDLL** (Lattner et al., CGO 2021). Ops declared in TableGen ODS with C++
  **generated**; PDL/PDLL expresses rewrites as a declarative "grammar" of the IR;
  progressive lowering is dialect-to-dialect. Direct evidence an IR node algebra can be
  spec-generated and transpositions can be declarative grammar-to-grammar maps.
- **LLVM TableGen `.td`.** Instruction/register sets are a grammar of the target; C++ is
  generated — "generator eats itself" at industrial scale.
- **tree-sitter `node-types.json`.** The grammar *derives* a machine-readable AST schema —
  the closest external analogue to "the schema is a thing the grammar *is*."
- **GHC Trees That Grow** (Najd–Peyton Jones, 2017). One AST parameterized by phase index,
  extended per-pass via type families — homoiousia by parameterization, a lighter point than
  Nanopass's language-per-pass.
- **egg / egglog** (Willsey et al., POPL 2021; PLDI 2023). The e-graph term language *is* a
  grammar; rewrites are grammar-to-grammar over it — the substrate LOCK-2 owns; cited to
  place the rewrite-as-grammar-map lineage.
- **Spoofax/Stratego + SDF3** (Visser et al.). SDF3 defines concrete syntax and AST schema
  together; Stratego rewrites terms over it — the academic root of grammar-defined-AST +
  transformation-as-rewrite.
- **Counterexample — rustc.** HIR/THIR/MIR are three **hand-written, non-homoiousic** IRs
  with bespoke lowerings sharing no grammar. rustc is proof a world-class compiler declines
  homoiousia — 3C must justify why it wins *here*: bbnf's tiers already ARE one algebra (§1
  table), so closing the rung is cheaper than in a language whose tiers diverge by design.

---

## 7. Risks & counterexamples (named)

1. **The sum-excluding trap (directive-named).** Claiming homoiousia on today's
   `Struct`-only `DirectTypeKind` is dishonest — it is product-only. The family is RED until
   `DirectTypeKind::Sum` lands (GN-3.W1). Any claim measured before the rung exists is
   wrong-artifact.
2. **The generic-grammar information wall (PROVE-08, LAW).** For a generic `json.bbnf` with
   no `->`, the sum's *variant payloads and document shapes* are not recoverable from the
   grammar — they live in the document (field names, nesting, capacity counts `875`/`10_800`).
   Structural transposition retires the heuristic classifier **only for grammars that
   declare their value algebra** (typed grammars, GN-3.W3). Resolution (GN-3's supersession):
   move the information into the grammar; the refutation stands, the grammar changes. The
   sharpest counterexample — it bounds the family's generality precisely.
3. **Capacity hints are corpus facts, not grammar facts** (GN-3 R1). `875`/`10_800`/`184`/
   `5_759` come from counting one document; byte-identical derived≡hand fails unless carried
   as `@cap(N)` annotations colocated with the projection. Decides whether byte-identity is
   honestly reachable.
4. **Nanopass proliferation vs parsimony (§vi; 3B tension).** The literature's cost is a
   language per pass. If a prototype needs N grammars for N lowerings, the family has
   regressed into Nanopass's failure mode and must be re-scoped. 3C closes ONE algebra over
   3A/3B's small fixed tower.
5. **Consumer-less meta-grammar (constitution viii).** `ir.bbnf` without a LOCK-7 fixpoint
   gate is forbidden substrate (§5); carried born-RED, owner LOCK-7/GN-6; never authored
   speculatively.
6. **Regex opacity (XL-C, ratified).** The algebra is closed only modulo the opaque `Regex`
   leaf (`ir/lib.rs:250`); "one algebra everywhere" is false there and must be stated as a
   bounded ratified boundary.
7. **Sum-unification order-sensitivity** (GN-3 R5). `object|array|…` and a reordered
   alternation must canonicalize to the *same* sum or derived≠hand — requires set-equal
   unification + canonical variant ordering; a gate must assert two orderings unify
   identically.

---

## 8. What a prototype must test to move this family

One prototype, three born-RED conjuncts, re-pinned to a named frozen tree:

- **P1 — the rung closes and the heuristics die.** Land `DirectTypeKind::Sum` (M1); drive
  the JSON `value` alternation `ExprKind::Alt → typed sum → DirectTypeKind::Sum →
  BackendExpr::Alt` as a structural fold. **Gate:** `derive_materialization_roles`
  (`passes/src/lib.rs:1403`) AND `has_rule("value")` (`runtime_generator.rs:99`) AND the
  4-way `ProjectionKind` fold (`general_descent/view.rs:20-46`) are **deleted**
  (`grep -c` == 0), JSON+Sheets outputs regen byte-identical, and **net-LOC is negative**.
  Retirement, not relocation. Report the emission-extensibility collapse (5→2).
- **P2 — the `->` thread is consumed, not re-derived.** Thread `Project.{target,ty}` through
  infer (kill the `:204` discard) into `DirectBuildField.target` (was always `None`).
  **Gate:** `target == Some(_)` for every projected leaf rule; the threaded node-type
  appears as the sum-variant payload type **with no independent re-derivation** (assert the
  derive reads the infer result, does not re-lex arrow text). Sheets regen byte-identical.
- **P3 — per-`RuleId` sufficiency + fleet check.** Prove a per-`RuleId` materialization key
  suffices for JSON/CSS/Sheets, or exhibit the grammar that forces per-`ExprId`. **Gate:**
  all six regen/check outputs (regen/check × json/css/sheets) byte-identical under the
  closed algebra; any per-`ExprId` grammar named as a counterexample.

**Out of prototype scope**, carried as born-RED edges (§4b/§5): `ir.bbnf` bootstrap (owner
LOCK-7/GN-6), `TypeAnalysis::from_ir` (owner GN-1 loom). A prototype authoring either
without its owner-gate violates constitution viii.

---

## 9. Integration seams (path:line, live tree `c93fc43143`)

| Seam | Location | Role in 3C |
|---|---|---|
| `ExprKind` sum-of-products | `crates/ir/src/lib.rs:234-271` | source algebra (`Alt`=sum, `Seq`=product, `Regex`=opaque leaf) |
| `BackendExpr` sum-of-products | `crates/ir/src/lib.rs:376-413` | sink algebra; near-isomorphic to `ExprKind` |
| **`DirectTypeKind` (Struct-only)** | `crates/codegen/src/direct_schema.rs:52-58` | **the broken rung — add `Sum` (M1)** |
| `DirectTypeRef` lattice | `crates/codegen/src/direct_schema.rs:93-120` | product+Vec+Option+Map; no sum today |
| `Project.ty` discard | `crates/passes/src/lib.rs:203-204` | the `->` thread source (P2) |
| `DirectBuildField.target` (always `None`) | `crates/passes/src/lib.rs:1149-1287` (R6 §5d) | near-live `->` consumer |
| role classifier (retire) | `crates/passes/src/lib.rs:1403-1490` | heuristic sum-recovery; deleted by M3 |
| `materialize_rule` splice | `crates/passes/src/lib.rs:1362-1391` | candidate grammar-to-grammar map |
| `has_rule("value")` name-gate (retire) | `crates/codegen/src/runtime_generator.rs:99` | routes on rule name; deleted by M3 |
| 4-way `ProjectionKind` fold (retire) | `crates/codegen/src/general_descent/view.rs:20-46` | sum→4 tags; deleted by M3 |
| typed-struct renderer (sum consumer) | `crates/codegen/src/json_typed_direct.rs:10` | emits the sum enum (GN-3.W1 in-wave consumer) |
| byte discriminator for variant tags | `crates/runtime/src/grammars/json/value.rs:28-47` | variant tag = first-byte dispatch |
| self-host precedent (`->`, `IrNode` hook) | `grammar/bbnf/bbnf.bbnf:9,15,42`; `grammar/bbnf/types.bbnf:2-3` | meta-grammar authors leaf/IR types — total-homoiousia germ |
| greenfield `TypeAnalysis::from_ir` | **absent on live** (0 hits) | far consumer; born-RED, owner GN-1 loom |
| `ir.bbnf` meta-grammar | **absent on live** | total homoiousia; born-RED, owner LOCK-7/GN-6 |

**Disposition.** Family 3C's strongest honest form is not a rival subsystem but the
**invariant GN-3 instantiates**: the four-tier spine is one sum-of-products algebra (modulo
the ratified opaque regex leaf); closing the `DirectTypeKind::Sum` rung makes every
transposition a structure-preserving grammar-to-grammar fold and *retires* the
role-classifier, the `value` name-gate, and the 4-way projection fold. Near-live consumer:
real and named (GN-3.W1 renderer, `json_typed_direct.rs:10`); the `->` thread consumed as
variant-payload type, not re-derived. Total homoiousia (`ir.bbnf`, generator-eats-itself)
is genuine, its germ is live in `bbnf.bbnf`, but it is consumer-gated on LOCK-7's bootstrap
fixpoint and carried as a born-RED edge — never asserted.
