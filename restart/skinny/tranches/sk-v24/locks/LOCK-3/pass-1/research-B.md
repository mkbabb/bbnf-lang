# LOCK-3 / Family 3B — SINGLE-IR-MULTIPASS (the LLVM shape)

The IR tower is one rich arena refined in place by annotating passes. Backend selection,
tape kind, residual masks, and types are attribute columns on the ONE grammar arena — not a
second IR. The frontend→backend transposition is deleted; the emitter reads
`(GrammarIr, AttributeStore)` directly. Two transpositions survive (parse: source→arena;
emit: arena+attrs→Rust), zero middle IR rebuilds.

## 1. The finding that founds the family

Skinny already has the substrate for single-IR and does not use it. `GrammarIr`
(`ir/src/lib.rs:33-39`) is a **dense flat arena**: `exprs: Vec<Expr>` addressed by
`ExprId(usize)` (`:19`), `rules: Vec<Rule>` by `RuleId(usize)` (`:16`), appended via
`add_expr`/`add_rule` (`:51`,`:57`). This is the exact shape LLVM/MLIR/Cranelift use for the
one-IR-many-passes model. The pass outputs are ALREADY side tables keyed by that arena:
`LayoutFacts` (`passes/src/lib.rs:90-100`) holds `node_types: HashMap<ExprId,Type>`,
`rule_types`, `backend_shape: HashMap<RuleId,BackendShape>`, `cost_facts`,
`per_grammar_policy`, `same_substrate_union` — a proto-attribute-store, one map per pass
output. `nullability` (`ir/src/lib.rs:320`) already returns a `Vec<bool>` column indexed by
`ExprId.0`. The annotation architecture is 80% present.

The ONE thing that breaks single-IR is a redundant transposition: `lower_expr`
(`passes/src/lib.rs:1318`) walks `GrammarIr` and **rebuilds it** as `BackendExpr`
(`ir/src/lib.rs:376-413`) — a `Box`-recursive tree that (a) duplicates `ExprKind` variant
for variant, (b) **discards `ExprId`** (so the ExprId-keyed side tables can no longer be
joined against the tree the emitter walks), and (c) LOSES information: `Discard{keep_left}`
is flattened to `Seq(vec![left,right])` with a comment admitting "the keep_left
discard-semantic is metadata for a later phase" (`:1354-1358`) — a live no-value-discard
violation baked into the transposition itself. `materialize_rule` (`:1362`) then SPLICES
synthetic nodes (`SpanMark`/`TapeEmit`/`DirectBuild`/`Return`, `ir/src/lib.rs:396-412`) into
that boxed tree. Family 3B's mechanism is: **delete `BackendExpr`/`BackendIr`; the emitter
consumes the arena plus attribute columns.**

## 2. The mechanism made concrete

### 2a. The single spine
`GrammarIr` is the IR at every level. No `BackendIr`. Passes never build a new tree; they
write attribute columns keyed by `ExprId`/`RuleId`. The pipeline `compile`
(`passes/src/lib.rs:32-68`) — today: infer → layout → materialization → shapes →
recognizers → **`single_plan` (builds BackendIr)** → backend-shape → returns
`PipelineOutput{ grammar, layout_facts, backend_ir, diagnostics }` — becomes: infer →
… → backend-shape, returning `AnnotatedIr{ grammar, attrs }`. The `single_plan`/`lower_expr`
step (the only `BackendIr` producer, `passes/src/lib.rs:1283,1318`) is excised.

### 2b. The AttributeStore substrate (columnar, dense)
The store is Struct-of-Arrays over the arena, not a bag of `HashMap`s:

```
struct AttributeStore {
    // per-ExprId columns (dense Vec indexed by ExprId.0; grows with the arena)
    nullable:     Column<bool>,               // producer: ir::nullability (ir/src/lib.rs:320)
    node_type:    Column<Type>,               // producer: layout::types::infer (passes:149)
    recognizer:   Column<Recognizer>,         // producer: derive_recognizers (passes:338)
    type_ref:     Column<DirectBuildTypeRef>, // producer: infer's Project arm (GN-3 W2)
    residual:     Column<ResidualMask>,       // producer: emit-spine pass (PROVE-12 generalized)
    // per-RuleId columns
    backend_shape: Column<BackendShape>,      // producer: derive_backend_shape (passes:398)
    cost_facts:    Column<CostFacts>,         // producer: derive_backend_shape (passes:398)
    emit_plan:     Column<EmitPlan>,          // producer: materialization (replaces splice)
    policy:        Column<PerGrammarPolicyFacts>,
}
type Column<T> = Vec<Option<T>>;  // ExprId/RuleId are dense arena indices → Vec, not HashMap
```

`Column<T> = Vec<Option<T>>` (or Cranelift's `SecondaryMap<K,T>` — grow-on-write,
default-on-read) is chosen over `HashMap` for three reasons: dense keys make it O(1) index
not hash; iteration is arena-ordered → **deterministic serialization** (the current
`LayoutFacts` HashMaps have nondeterministic iteration, a regen-determinism hazard under
§53); and it mirrors the entity-component pattern that is proven Rust prior art (§3).

### 2c. BackendShape / emit-directives AS attributes (the charter's core clause)
`BackendShape` (`ir/src/lib.rs:361`) stops being a tag on a whole `BackendIr` and IS a
per-RuleId attribute column — already half-true (`backend_shape: HashMap<RuleId,_>`,
`passes/src/lib.rs:96`); 3B formalizes it. The synthetic emit nodes `materialize_rule`
splices become an **`EmitPlan` attribute** on the RuleId:

```
struct EmitPlan { pre: Vec<EmitOp>, post: Vec<EmitOp> }  // SpanMark(Start).. / ..TapeEmit,DirectBuild,Return
```

The emitter, walking RuleId `r`: run `emit_plan[r].pre`, walk `rule.body` over the arena,
run `emit_plan[r].post`. No tree is rebuilt to hold these; they are annotations. This is
literally "BackendShape as attributes; the fewest transpositions that suffice."

### 2d. Emitter dispatch off the arena
`codegen/src/lower/mod.rs:16-19` already dispatches lowering by `CostFacts` via a
`ShapeLowering` trait (`select_lowering(cost)`) — backend-shape-as-attribute driving emit is
already the pattern; it just currently walks `BackendExpr`. Rewire `lower_to_rust`
(`lower/rust.rs`; `LowerCtx` already carries `backend_shape`/`cost_facts`/`per_grammar_policy`,
R1 §2) and the `BackendExpr` consumers (`lower/sink_only.rs`, `lower/tape_plan.rs`,
`lower/css_scan.rs`, `json_sink_direct/mod.rs`) to walk `grammar.expr(id).kind` reading
columns. `Discard{keep_left}` is read directly at emit — the flattening loss (§1) simply
vanishes: single-IR preserves it (no-value-discard satisfied by construction).

### 2e. Structural rewrites without a new IR (RAUW column)
Passes that genuinely restructure (left-factor an `Alt`, inline a `Ref`, dedup) do it the
LLVM/e-graph way: append new `Expr`s to the arena and record forwarding in a
`canonical: Column<ExprId>` union-find column, resolved on read (LLVM `replaceAllUsesWith`;
egg e-class union). The arena is append-only already; this makes in-place rewrite lawful
without mutating existing parents. Fixed-point passes use the LLVM `Changed`-flag convergence
(memory: changed-flag-convergence) already visible in `nullability`'s `changed` loop
(`ir/src/lib.rs:322-347`).

### 2f. Types are a column, not a third IR (folds GN-3, ties LOCK-1)
GN-3 W2's move — thread `ExprKind::Project.ty` (`ir/src/lib.rs:261`) into
`DirectBuild.type_ref` instead of discarding it at the `infer_expr` Project arm
(`passes/src/lib.rs:203-204`) — IS a single-IR annotation write to the `type_ref` column.
`DirectSchemaSet` (`codegen/src/direct_schema.rs`) becomes an **extraction view** projected
from the `type_ref`/`emit_plan`/cardinality columns over the arena, not a separately-authored
IR. 3B thus GENERALIZES GN-3's point-fix into the architectural law: every `->`, every pass
output, is a column on the one arena; the hand schema (`xtask/src/real_typed_schema.rs`) and
the `BackendExpr` re-projection are both instances of the same anti-pattern — a parallel
structure kept in sync by hand. **Named re-cut (constitution ix):** GN-3 threads one attribute
through a surviving `BackendExpr`; 3B holds that `BackendExpr`'s existence is the wall GN-3
works around — a supersession candidate for GESTALT-B, not a contradiction of GN-3's waves
(which land unchanged under 3B, W2 becoming trivially a column write).

## 3. Prior art (with citations)

- **LLVM** — one `Module`/`Function`/`Instruction` IR; passes are Analysis (results cached in
  `AnalysisManager`, keyed by IR unit — the attribute-store analog) or Transform (mutate in
  place); `llvm::Attribute` hangs off values/functions; `Value::replaceAllUsesWith` (RAUW) is
  the in-place rewrite primitive; `verifyModule` enforces well-formedness. 3B's AttributeStore
  = AnalysisManager made columnar because our keys are dense.
- **MLIR** (Lattner et al., "MLIR: Scaling Compiler Infrastructure for Domain Specific
  Computation", CGO 2021) — one `Operation` graph, **progressive lowering by rewriting ops +
  attributes across dialects**, `Attribute`/`Type` as uniqued first-class side data, a per-op
  verifier. "BackendShape as attributes; gradual lowering, one IR" is the MLIR thesis verbatim.
  The dialect idea answers "don't pollute the frontend enum": keep `ExprKind` pure, carry
  lowering directives (`EmitPlan`) as attributes, as MLIR keeps op structure and hangs lowering
  intent on attributes.
- **Cranelift `cranelift-entity`** — `EntityRef`, `PrimaryMap<K,V>` (the arena),
  `SecondaryMap<K,V>` (dense grow-on-write attribute columns). This is the **directly
  applicable Rust prior art** for `AttributeStore`: `ExprId`/`RuleId` are `EntityRef`s,
  `GrammarIr.exprs` is a `PrimaryMap`, each column is a `SecondaryMap`. Recommend mirroring its
  `SecondaryMap` default-on-read semantics.
- **rustc MIR** — `rustc_mir_transform` applies a pass list to ONE `Body` in place (contrast
  the HIR→THIR→MIR tower, which is family 3A's CONTRACT-TOWER); MIR passes annotate/mutate, not
  rebuild. Confirms single-IR-multipass is the middle-end norm once the tower has bottomed out
  into an optimizable form.
- **GHC Core** — Core-to-Core passes iterate over one `CoreExpr` (the simplifier refines in
  place); annotations ride on binders.
- **egg / egglog** (Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL 2021)
  — one e-graph, rewrite in place, **e-class analysis data as attribute columns on e-classes**;
  union = RAUW. Ties LOCK-2: 3B's `canonical` column IS an e-graph union restricted to a tree;
  the AttributeStore is the substrate a LOCK-2 e-graph island would annotate.

## 4. Integration seams (path:line)

| Concern | Seam | Action |
|---|---|---|
| Arena substrate (keep) | `ir/src/lib.rs:33-39,16-19,51-57` | `GrammarIr` becomes the single IR; ExprId/RuleId are the EntityRefs |
| Proto attribute store (consolidate+densify) | `passes/src/lib.rs:90-100` `LayoutFacts` | HashMaps → `AttributeStore` columnar `SecondaryMap` |
| Redundant transposition (DELETE) | `passes/src/lib.rs:1283` `single_plan`, `:1318` `lower_expr`; `ir/src/lib.rs:376-413` `BackendExpr`; `:351-358` `BackendIr` | excise; emitter walks arena |
| Splice→attribute | `passes/src/lib.rs:1362` `materialize_rule`; `ir/src/lib.rs:396-412` | synthetic nodes → `EmitPlan` column |
| keep_left loss (fixed by construction) | `passes/src/lib.rs:1354-1358` | read `Discard.keep_left` at emit; no flatten |
| BackendShape as attribute | `ir/src/lib.rs:361`; `passes/src/lib.rs:96,398` | formalize `backend_shape` HashMap as column |
| Cost-driven emit dispatch (already the pattern) | `codegen/src/lower/mod.rs:16-19` `ShapeLowering`/`select_lowering` | keep; source from column not `BackendExpr` |
| BackendIr consumers (rewire) | `codegen/src/lower/{rust,sink_only,tape_plan,css_scan}.rs`, `json_sink_direct/mod.rs` | consume `(GrammarIr,&AttributeStore)` |
| type_ref column (GN-3 fold) | `passes/src/lib.rs:203-204`; `ir/src/lib.rs:261,485` | infer writes `type_ref` column instead of discarding |
| Residual as attribute (PROVE-12 generalized) | `codegen/src/runtime_generator.rs` CSS template; `PROVE-12/patch.diff` | emit-spine pass writes `residual` mask column; emitter const-folds from it — pattern generalizes to every stop-set (PROVE-12 §7) |
| Determinism/serde | `LayoutFacts` HashMaps | columnar Vec → arena-ordered, deterministic under §53 |

## 5. Named risks & counterexamples

1. **Column growth under rewrite.** Append-and-forward (§2e) grows the arena mid-pipeline;
   every column must resize or default-on-read. Mitigation: `SecondaryMap` default semantics +
   a verifier pass (`verifyModule` analog) asserting totality — after backend-selection, every
   reachable RuleId has `backend_shape`; every `Project` ExprId has `type_ref`
   (no-value-discard as a checkable invariant); every emit-requiring rule has an `EmitPlan`.
   Consumer-less columns are forbidden (Y.13) → each column names a same-wave reader.
2. **Does BackendExpr do hidden work?** Beyond copying it computes `backend_span_kind(pattern)`
   (`passes/src/lib.rs:1343`) and flattens Discard. Counterexample audit: `span_kind` becomes a
   node-column write; Discard flattening is a LOSS 3B removes. The proto (§6) MUST diff generated
   output to prove the transposition was otherwise pure — if it wasn't, the residue names exactly
   what an `EmitPlan`/column must carry.
3. **Multi-backend leakage (Rust vs TS).** Columns like `DirectBuildTarget.rust_field`,
   `representation: Borrowed/Owned` (`ir/src/lib.rs:485-527`) are Rust-specific. Risk: the
   "backend-agnostic middle" (charter) leaks. Mitigation: LAYER the columns —
   agnostic (`type_ref`, `cardinality`, `tape_kind`, `backend_shape`, `EmitPlan`) vs
   backend-specific (naming, borrow policy); the TS backend reads agnostic columns + runs its own
   naming/ownership pass (memory: backend-agnostic-types). Concrete realization of "backend-
   agnostic middle over which Rust now, TS later"; ties LOCK-6.
4. **"Renamed, not reduced" objection.** The emitter still lowers arena→Rust (a transposition).
   Honest count: 3B yields TWO transpositions (source→arena, arena→Rust), ZERO middle rebuilds;
   status quo is THREE (adds GrammarIr→BackendIr). The reduction is provable — a whole IR type +
   `lower_expr` + `single_plan` + `materialize_rule` deleted.
5. **HashMap→Vec determinism must actually be exercised.** If any column stays a HashMap the
   regen-idempotency (§53, regen∘regen byte-identical) can silently depend on iteration order.
   Gate: the columnar store's serde is arena-ordered by construction.

## 6. What a prototype must test to move this family

**PROTO-3B-1 (decisive).** Delete `BackendExpr`/`BackendIr`/`lower_expr`/`single_plan`/
`materialize_rule` for the JSON grammar only; introduce `AttributeStore` (columnar) +
`EmitPlan`; rewire `lower_to_rust` and the JSON emitters to walk `(GrammarIr,&AttributeStore)`.
Gates:
- **Parity:** `cargo xtask regen-json` produces `generated.rs` **byte-identical** to HEAD
  (proves the transposition was lossless — or the diff localizes exactly what hidden work
  BackendExpr did, feeding risk 2).
- **Parsimony:** net codegen LOC **negative** (BackendExpr enum + lower_expr + single_plan +
  materialize_rule gone; the store is smaller than the tree it replaces). Report the delta.
- **No-value-discard win:** exhibit a Discard grammar where HEAD's flattening loses `keep_left`
  and 3B preserves it — a behavioral improvement, not just a refactor.
- **Determinism:** regen∘regen byte-identical with the columnar store serialized.
- **Y.13 totality:** the verifier pass asserts every reachable RuleId/Project-ExprId carries its
  mandatory column; a deliberately-missing column fails the build (gate has teeth).

**PROTO-3B-2 (multi-backend proof, ties LOCK-6).** Split columns agnostic/specific (risk 3);
emit a TS skeleton for the SAME JSON arena reading ONLY agnostic columns + a TS naming pass.
Gate: the TS skeleton's structure is derivable with zero reads of Rust-specific columns —
proving the arena+attribute middle is genuinely backend-agnostic, the fewest-transpositions
claim holding across two backends.

Move-the-family criterion: PROTO-3B-1 byte-identical + net-negative LOC + the keep_left win
retires the `BackendExpr` transposition as pure debt; PROTO-3B-2 certifies the middle is
backend-agnostic. Together they establish single-IR-multipass as the LOCK-3 spine with GN-3's
type plane folded in as columns and PROVE-12's residual generalized as a `residual` column.
