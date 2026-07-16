# LOCK-3 · Family 3C — GRAMMAR-HOMOIOUSIA (the IR is itself a grammar)

Live substrate re-pinned to `skinny@c43126422` (the sole living implementation; the
R6 value-plane cartography was cut at `7f3be27c2` and its line numbers are re-verified
below against `c43126422`). Every `path:line` was opened on that tree unless marked as
the main `bbnf-lang` tree (`crates/core/**`, a non-live design ancestor).

The formulation: express each IR level as a bbnf grammar, make the transpositions
grammar-to-grammar maps, and turn the generator on itself (bootstrap alignment with
LOCK-7). The steering question is narrow and answered head-on: **what named downstream
wave consumes an IR-expressed-as-grammar, and does that consumer exist on or near
live?** The honest answer is that no self-standing consumer exists on live; the
formulation reduces to two separable claims of very different strength, and the strong
one is a single threaded-`ty` consumption that stands independent of homoiousia.

---

## 1. The two claims inside "IR-as-grammar", separated

Homoiousia bundles two claims that the substrate treats completely differently:

- **C1 — the IR *data model* is grammar-specified.** The enum/struct shells of the IR
  tower (`ExprKind`, `BackendExpr`, the `DirectBuild*` schema) are emitted from a bbnf
  grammar describing them, rather than hand-written. This is a *schema-generation*
  claim, and it has real prior art (tree-sitter node-types, MLIR ODS, attribute
  grammars).
- **C2 — the IR *transpositions* are grammar-to-grammar maps.** The lowering
  `GrammarIr → BackendIr` (and downward to emission) is itself a grammar/rewrite
  artifact, not a hand fold. This is a *term-transduction* claim, and it has **no
  substrate on live** (§4, R1).

The charter phrase "transpositions are grammar-to-grammar maps" is C2; the phrase "the
generator eats itself" is C1 executed under LOCK-7's bootstrap. C1 is buildable and
gated; C2 is either vacuous or a re-import of LOCK-2's rewrite engine (an
orthogonal-subsystem breach). Keeping them fused is the family's chief self-deception
risk, so they are costed apart.

---

## 2. The live IR tower is genuinely a sum algebra (why the sum-node gate is load-bearing)

The transposition spine is three sum types plus one product schema:

- `ExprKind` — a **10-arm sum**: `Seq | Alt | Repeat | Optional | Literal | Regex |
  Ref | Annotation | Project | Discard` (`crates/ir/src/lib.rs:234-271`). Already
  `#[derive(Serialize, Deserialize)]` (`:233`) — the front IR is round-trippable data
  today.
- `BackendExpr` — a **13-arm sum**: `Entry | Seq | Alt | RepeatLoop | OptionalBranch |
  ByteLiteral | RegexProgram | CallRule | SpanMark | TapeEmit | DirectBuild |
  ValueProject | Return` (`crates/ir/src/lib.rs:376-413`).
- `TapeKind` — a 9-arm sum (`:453-463`); `BackendShape` a 5-arm sum (`:361-367`).
- `DirectTypeKind` — the typed-schema type constructor — has **exactly one variant,
  `Struct`** (`crates/codegen/src/direct_schema.rs:52-58`). `DirectTypeRef` around it
  carries products and collections (`Type | Scalar | Vec | MapString | MapEntriesVec |
  StringEnum | Option`, `:93-120`) but **no recursive sum**.

To express `ExprKind` (a 10-way alternation) *as a grammar* and round-trip it through
the typed schema, the schema must represent a recursive sum with typed payloads. A bbnf
`Alt` (`value = object | array | …`, `grammars/json.bbnf:11`) IS a sum; but the emitter
can only lower it to a Rust `enum` with typed arms if `DirectTypeKind` gains a `Sum`
variant. It does not have one. **The product-only, sum-excluding subset the directive
forbids treating as the formulation is exactly the subset a grammar with no `Alt`
rules describes — and that subset cannot express `ExprKind` or `BackendExpr` at all,
since both are irreducibly sums.** There is a second, shallower enum surface —
`ShapeFacts::add_enum` → `Shape::Enum { variants: Vec<String> }`
(`crates/ir/src/lib.rs:560-579`), fed by the hardcoded 6-arm `JsonValue` list
(`crates/passes/src/lib.rs:285-291`) — but its variants are bare name strings with no
recursive payload schema; it is the lazy-view taxonomy, not a typed sum, and using it
to claim "enums already render" is precisely the sum-excluding trap.

**Gate, as a born-RED dependency edge:** `DirectTypeKind::Sum` @
`direct_schema.rs:52-58`, **owner = GN-3.W1** ("The Sum Node", `GN-3.md` wave manifest,
+420 LOC, born-RED). Homoiousia's first executable step (grammar-derive the IR's own
type) is BLOCKED behind that edge and does not stand alone.

---

## 3. The consumer question — the crux, answered without evasion

Constitution clause viii (Y.13): consumer-less substrate is forbidden, in designs as in
code. A grammar that describes `ExprKind` produces a *type module*; that module needs a
same-wave reader.

**On live there is none.** `skinny` ships exactly one grammar — `grammars/json.bbnf`
(18 lines, structural, zero `->`) — and no `ir.bbnf`, no meta-grammar, no self-hosted
IR. The generator consumes grammars-of-documents, never a grammar-of-itself. So a
grammar-expressed IR has zero consumer on or near live.

**The only named downstream consumer is LOCK-7 · 7B (TOTAL-SELF-HOST)**, whose charter
literally enumerates "bbnf grammar, regex grammar, **IR grammar**, emitter templates —
all grammar-specified; the generator becomes mostly data" (`PORTFOLIO.md`, LOCK-7 7B).
3C's C1 output *is* the substrate 7B consumes; 7B's bootstrap engine (`xtask`/regen) is
its reader. Therefore the disposition the directive asks for — "the consumer named, or
each gate carried as a born-RED dependency edge with its owner" — is:

| edge (born-RED) | seam | owner |
|---|---|---|
| `DirectTypeKind::Sum` (render Alt→typed enum) | `direct_schema.rs:52` | **GN-3.W1** |
| `ir.bbnf` consumed by regen; fixpoint holds | `xtask` regen lane; no live `ir.bbnf` | **LOCK-7 · 7B** |
| the emit fold reads the grammar-derived IR module | `runtime_generator.rs:84` | **LOCK-7 · 7B / GESTALT-B** |

C1 is thus a **substrate whose sole consumer is an unlanded LOCK-7 wave** — admissible
only as a born-RED edge under GESTALT-B, never as a self-standing 3C deliverable. This
is the family's defining structural fact, and stating it plainly is the family at its
strongest: homoiousia is a bootstrap-tier claim, not an IR-tier one.

---

## 4. What C2 (grammar-to-grammar transposition) actually requires — the sharpest counterexample

The live transposition `GrammarIr → BackendIr` is `lower_expr` — a hand-written tree
fold (`crates/passes/src/lib.rs:1329-1359`; the `Project → ValueProject` arm at
`:1349-1353` carries `ty` verbatim). The emit-order framing `SpanMark(Start) · body ·
SpanMark(End) · TapeEmit · DirectBuild · Return` is `materialize_rule`
(`:1362-1389`), also a hand fold keyed by a `MaterializationPlan`.

bbnf grammars map **bytes → tape** (recognizers over input). They do **not** express
**tree → tree** transductions. `lower_expr` and `materialize_rule` consume an
already-parsed `GrammarIr` and emit `BackendExpr`; there is no byte stream to recognize,
so there is no bbnf grammar that *is* this map. Realizing "grammar-to-grammar maps"
therefore demands a term-rewriting / attribute-evaluation facility bbnf lacks —
i.e. an e-graph rewrite system (LOCK-2's `egraph`/`egraph-derive` in-tree) or a
DRR/PDL-style pattern DSL. Importing one into 3C is either (a) an orthogonal-subsystem
violation (`no-orthogonal-codepaths`, `system-cohesion`) or (b) a collapse of 3C into
LOCK-2. **C2 has no honest home inside 3C's charter as written.** The strongest form of
the family concedes C2 and keeps only C1 (data model) + the threaded-`ty` consumption
(§5).

---

## 5. The `->` type value at the infer edge — the one consumption that stands on live

The shared directive: `crates/passes/src/lib.rs:204` drops `ty` via `..`
(`ExprKind::Project { body, .. } => self.infer_expr(*body)?`); the projected type never
reaches inference. In the homoiousia reading, `Project.ty`
(`crates/ir/src/lib.rs:261-265`, `ty: Option<String>`) is a **named reference into the
IR grammar's own type namespace** — a `-> Type` where `Type` is a grammar rule name.

**How this formulation CONSUMES one threaded node-type rather than re-deriving it:**
today the taxonomy is *re-invented structurally* by `derive_materialization_roles`
(`crates/passes/src/lib.rs:1403-1491`) — a byte-literal role classifier that sniffs
`b"{"`/`b"}"`→container (`:1437-1438`), `b"["`/`b"]"`→sequence (`:1445-1446`),
`b"true"`+`b"false"`→bool (`:1419-1422`), `b"null"`→null (`:1430`), the `>=4`
value-ref heuristic → the `value` rule (`:1454-1465`) — plus the hardcoded 6-variant
`JsonValue` fallback in `shapes::derive_shape_facts` (`:270-294`) and the
`has_rule("value")` name-gate (`crates/codegen/src/runtime_generator.rs:98`). Homoiousia
**consumes the threaded `ty`**: resolve `Project.ty` as a `DirectBuildTypeRef::Named {
type_id }` and write it onto `DirectBuildField.target`
(`crates/ir/src/lib.rs:466-490`; the field is `Option<DirectBuildTarget>` and is
**always `None`** on the offset-tape path — the plan at `:1136-1287` never fills it).
Once the grammar's own `->`-named sum *is* the taxonomy, the byte-literal classifier,
the shape fallback, and the name-gate are **dead code, retired not relocated** — that
is the concrete "one threaded node-type consumed, not re-derived" evidence, and it does
not depend on any of homoiousia's harder claims.

**Cross-tree caveat (must be stated).** The directive names the downstream consumer as
`TypeAnalysis::from_ir` at `backend/driver/analysis.rs`. That symbol lives only in the
**non-live main tree** (`crates/core/src/backend/driver/analysis.rs:22`, signature
`from_ir(ir: &GrammarIR)` — it takes a *reference-resolved grammar*, not a typed IR with
`node_types`, so it cannot receive a threaded projection type as-is). On the **live
skinny tree** the consuming edge is `passes::extract::derive_materialization_plan`
(`:1136-1287`) → `shapes::derive_shape_facts` (`:259`) → the codegen emit
(`runtime_generator.rs:84`). Any thread must target the live edge; `from_ir` is a stale
pointer for this lock.

---

## 6. Prior art (citations)

- **tree-sitter** — a grammar generates both a parser and a typed CST schema
  (`node-types.json`) consumed by language bindings. This is C1 realized at scale:
  the *type surface* is grammar-derived; the transformation engine is separate,
  hand-written C. Confirms "schema yes, transduction no."
- **MLIR ODS (Operation Definition Specification, TableGen)** — dialect ops and types
  declaratively specified, generated to C++. But rewrite patterns (DRR/PDL) are backed
  by a C++ pattern engine, not the ODS grammar. The industrial precedent that the DATA
  MODEL is spec-generated while TRANSDUCTION is a distinct engine — exactly §4's split.
- **Attribute grammars (Knuth 1968, "Semantics of context-free languages")** — the `->`
  projection *is* a synthesized attribute; a grammar-specified IR is an attribute
  grammar whose synthesized attributes are the IR nodes. Silver/Copper (U. Minnesota)
  generate compilers from AG specs — the true theoretical home of C1, and evidence that
  even there the attribute *equations* are a separate evaluation layer (echoing C2's
  gap).
- **GHC "Trees That Grow" (Najd & Peyton Jones, 2017)** — one AST, phase-indexed
  extension fields: one-IR-many-phases without grammar generation. Relevant to
  LOCK-3 3B, and a caution that phase richness resists a single generated shell.
- **egg / egglog** — rewrite rules as data over an e-graph: the transduction-as-data
  facility bbnf lacks (§4). Its existence is precisely why C2 belongs to LOCK-2, not 3C.
- **Bootstrapping precedent (rustc, GHC self-host)** — the fixpoint `regen∘regen = regen`
  byte-identical is LOCK-7 · 7A's gate; "the generator eats itself" is this bootstrap,
  and its consumer is the bootstrap engine, confirming §3.
- **PROVE-12 (in-tree, worktree base `d2b32f6e3`)** — the residualization model
  homoiousia must obey: a grammar constant const-evaluated at generation into a
  const-generic mask, zero runtime instruction, byte-identical parity, +18 generated
  LOC. Any grammar-expressed IR must be an equally build-time artifact.

---

## 7. Named risks / counterexamples

1. **C2 has no substrate (sharpest).** bbnf maps bytes→tape; `lower_expr`
   (`passes:1329`) and `materialize_rule` (`:1362`) are tree→tree folds with no grammar
   form. "Grammar-to-grammar maps" is vacuous unless a rewrite engine is imported —
   which is either orthogonal-subsystem breach or a merge into LOCK-2.
2. **The analyses aren't grammar-expressible.** `nullability` (`ir:320-349`),
   `first_bytes`, `cost.rs`, and `derive_materialization_roles` are hand folds. LOCK-7
   7B's "generator becomes mostly data" is bounded to *type shells*; the fold logic
   stays hand-code, so the self-hosting percentage claim must be measured, not asserted.
3. **The sum-node gate.** `ExprKind` is a 10-arm sum but `DirectTypeKind` is Struct-only
   (`direct_schema:52`). Homoiousia cannot express its own IR until GN-3.W1 lands `Sum`;
   the product-only subset excludes every `Alt` rule = excludes `ExprKind` itself.
4. **Consumer-less on live.** Only `json.bbnf` exists; no `ir.bbnf`. Constitution viii
   forbids the substrate absent a consumer; its sole consumer is unlanded LOCK-7 7B —
   a born-RED edge, never a self-standing 3C product.
5. **no-ts-ir / zero-runtime (XL-D, Constitution v).** The grammar-expressed IR must
   stay a build-time emitter input. If it becomes a runtime IR-walker it breaks the
   zero-runtime-cost invariant; PROVE-12 is the compliance model (const-eval at
   generation, `git diff` over `crates/runtime/` empty).
6. **Fixpoint fragility.** `regen∘regen = regen` byte-identical (LOCK-7 7A) is
   unreachable if the hand `ir/src/lib.rs` carries derives, doc-comments, and impl
   blocks a grammar cannot emit — the same "information must move into the grammar"
   burden GN-3.W3 hit with capacity hints (`GN-3.md` R1). Byte-identity of a
   grammar-generated `ir/src/lib.rs` is the sharpest engineering gate and likely forces
   per-derive/per-impl annotations that bloat the IR grammar.

---

## 8. Exactly what a prototype must test to move 3C

- **P1 — Sum render.** Author `ir.bbnf` expressing `ExprKind` as a 10-arm `Alt` with one
  product rule (`Project = body target ty -> Project`). Run the codegen enum renderer on
  it. RED until `DirectTypeKind::Sum` (`direct_schema:52`) can lower an `Alt` to a
  typed-payload Rust `enum`. Evidence: the emitted enum shell byte-matches the hand
  `ExprKind` (`ir:234-271`) modulo derives/impls — measure the exact residual delta.
- **P2 — Fixpoint gap quantified.** Regen `ir/src/lib.rs` from `ir.bbnf`; diff vs
  committed. Report the byte-identity gap as a line/character budget (derives,
  doc-comments, `impl` blocks) — this *is* the LOCK-7 7A cost, made concrete.
- **P3 — Consumer edge, executed or carried.** Either land a LOCK-7 7B micro-wave that
  reads the emitted IR module in-build, or commit the born-RED edge table of §3 with its
  owners. No P3 pass = homoiousia stays a bootstrap-tier design input, not a 3C product.
- **P4 — the `ty` thread (the win that stands alone).** Fill `DirectBuildField.target`
  (`ir:470`, today `None`) from `Project.ty` (`ir:264`) as `DirectBuildTypeRef::Named`;
  retire `derive_materialization_roles` (`passes:1403`), `shape_for` fallbacks
  (`passes:270-294`), and the `has_rule("value")` name-gate
  (`runtime_generator.rs:98`); prove `json` regen byte-identical (the heuristic taxonomy
  == the grammar's `->` declaration). This is the RED→GREEN transition on `passes:204`
  and the one consumption independent of C1/C2.
- **P5 — zero-runtime gate.** `git diff --name-only <base>..HEAD | grep crates/runtime/`
  must be empty on every step (PROVE-12 / GN-3 invariant); the IR-as-grammar is a
  schema producer, never a runtime.

**Disposition.** 3C's strong core is C1 (grammar-specified IR *data model*) carried as a
born-RED edge into LOCK-7 · 7B, gated on `DirectTypeKind::Sum` @ GN-3.W1, plus the
self-standing P4 `ty`-thread that retires the byte-literal role classifier. C2
(grammar-to-grammar transposition) is refuted on live for want of a tree-transduction
substrate and is ceded to LOCK-2. The family is honest only when C1 and C2 are costed
apart and the consumer is named as LOCK-7, not asserted as present.
