# LOCK-3 / Family 3B — SINGLE-IR-MULTIPASS · pass-2

One rich arena, refined in place by annotating passes; backend selection, tape kind, residual
masks, and projected types are attribute *columns* keyed by the arena's own indices, not a
second tree. Emission is one fold `emit(&GrammarIr, &Attributes)`. This pass re-pins every
claim to live `skinny@3f2350f15af1cc886e2968be70e7bee610ae0e2a` (the directive's `b3cb8c16b`
is not this tree's HEAD; the R6 cartography was cut at `7f3be27c2`, and every line below was
re-opened at `3f2350f15`). It answers the one-fold questions with executed data-flow, and
states — for the shared `->` question — the exact ExprId-keyed *consumption* contract by which
the fold reads a threaded node-type rather than re-deriving it.

## 1. The substrate is already an arena with proto-attribute columns

`GrammarIr` is a dense flat arena: `exprs: Vec<Expr>` addressed by `ExprId(usize)`, `rules:
Vec<Rule>` by `RuleId(usize)` (`ir/src/lib.rs:19,16`; append via `add_expr`/`add_rule`). This
is the LLVM/MLIR/Cranelift one-IR-many-passes shape. The pass outputs are ALREADY side tables
keyed by that arena:

- `LayoutFacts.node_types: HashMap<ExprId, Type>` (`passes/src/lib.rs:93`), written by
  `infer_expr` via `self.facts.node_types.insert(expr_id, ty)` (`:212`), memo-read at `:179`.
  This is a live per-ExprId attribute column.
- `nullability(grammar) -> Vec<bool>` (`ir/src/lib.rs:320-349`) is a dense column indexed by
  `ExprId.0` — a Changed-flag fixpoint, the archetypal in-place analysis.
- `backend_shape: HashMap<RuleId, BackendShape>`, `cost_facts`, `per_grammar_policy`,
  `same_substrate_union` — one map per pass, all keyed by arena ids.

The annotation architecture is ~80% present. What breaks single-IR is three redundant tree
*rebuilds* that discard the keys:

1. `lower_expr` (`passes/src/lib.rs:1318-1360`) walks `GrammarIr` and rebuilds it as
   `BackendExpr` (`ir/src/lib.rs:376-413`) — a `Box`-recursive 13-arm tree duplicating
   `ExprKind` variant-for-variant, **discarding `ExprId`** (so the ExprId-keyed columns can no
   longer be joined against the tree the emitter walks), and LOSING data:
   `Discard{keep_left}` flattens to `Seq(vec![left,right])` with the comment "the keep_left
   discard-semantic is metadata for a later phase" (`:1354-1358`) — a standing
   no-value-discard violation baked into the transposition itself.
2. `materialize_rule` (`:1362-1389`) SPLICES synthetic nodes
   (`SpanMark/TapeEmit/DirectBuild/Return`) into that boxed tree.
3. `lower/sink_only.rs:175-238` transposes `BackendExpr` a THIRD time into `SinkOnlyExpr`
   (`TapeEmit`@220, `DirectBuild`@221, `ValueProject`@228).

So the live tower is `GrammarIr → BackendIr(BackendExpr) → SinkOnlyProgram(SinkOnlyExpr) →
Rust`: three rebuilds before emission. Family 3B's mechanism is to delete `BackendExpr`,
`BackendIr`, and `SinkOnlyExpr` as *trees* and let the emit fold walk the `GrammarIr` arena
joined against attribute columns — zero rebuilds, one fold.

## 2. `materialize_rule` is a pure derivation of `GrammarIr` — the data-flow trace

The one-fold question "does `materialize_rule`'s emit-order carry data absent from
`GrammarIr`?" resolves NO, executed by the data-flow chain:

```
materialize_rule(rule_id, body, materialization)          passes/src/lib.rs:1362
  descriptor = materialization.descriptors.get(&rule_id)  :1367   ← keyed by RuleId only
  Seq[ SpanMark{Start,label}, body, SpanMark{End,label},
       TapeEmit{descriptor.kind}, DirectBuild{shape,fields}, Return ]   :1370-1388
descriptors ← derive_materialization_plan(grammar)         :1139   ← sole arg is &GrammarIr
plan fields ← derive_materialization_roles(grammar)        :1142 → :1403  ← sole arg &GrammarIr
```

`descriptor.{kind,label,shape,fields}` are pure functions of `grammar`; the emit-order
`Seq[SpanMark, body, SpanMark, TapeEmit, DirectBuild, Return]` is a fixed shape parameterized
only by `(descriptor, body)`. No configuration, no corpus fact, no external table enters.
Therefore the splice is recomputable *inside* a single `emit` fold, byte-identically, by
looking `descriptor` up per `RuleId` at the moment the fold enters a rule — `materialize_rule`
need not pre-build a tree; it is an emit-time decoration. This is the concrete kernel of the
"fewest transpositions" claim: the middle `BackendExpr` tree exists only to hold a splice that
is a pure function of the arena.

## 3. The one-fold byte-identity question — the honest wall is the STATIC JSON templates

The directive asks whether emission re-expresses as one `emit(&GrammarIr,&Attributes)` fold
with all six outputs (regen/check × json/css/sheets) byte-identical, and warns *mapped ≠
executed*. The executed reality of today's emit is NOT one fold: it is a two-level dispatch
over three emitters:

- Level 1 — `render` (`runtime_generator.rs:78-87`) matches `LoweredArtifact::{SinkOnly, Scan}`
  (a computed lowering variant, not a name): CSS → `Scan` (`emit_scan`@211), everything else →
  `SinkOnly`.
- Level 2 — `emit_sink_only` (`:89-140`) splits on `has_rule("value")` (`:98`): JSON family →
  `json_sink_direct::render` + eight **static** `include_str!("json_templates/*")` files
  (`:101-131`); non-`value` (Sheets) → `emit_general_descent` (`:147-188`), which GENERATES its
  `view.rs` from the program (`general_descent::render_view`@178).

The sharp finding: **the JSON lazy surfaces (`value.rs`, `view.rs`, `visitor.rs`,
`parser.rs`) are verbatim static files, not derived from any IR** (`runtime_generator.rs:107-131`;
confirmed by R6 — `json_templates/view.rs` is copied byte-for-byte to the runtime). A single
`emit(&GrammarIr,&Attributes)` fold cannot *derive* those bytes; it can only splice them. So a
truthful one-fold has exactly two admissible forms, and choosing between them is THE prototype
that moves this family:

- **(a) Retirement.** Point the `general_descent` view generator at `json.bbnf` and make its
  generated `view.rs` byte-identical to the static `json_templates/view.rs`. If achievable,
  the static template set retires and the fold genuinely derives all six outputs. The
  divergence is presently large (different type names — `JsonObject` vs `{Prefix}Node`,
  hand-tuned `next_sibling_cursor` navigation `view.rs:355-382` vs the general flat sweep), so
  this is a real engineering delta, not a rename.
- **(b) Relocation.** The fold carries a per-family *template attribute* (the JSON lazy view as
  a bundled artifact selected by an IR-computed shape fact). This is one fold structurally, but
  the JSON bytes are still not *derived* — honest bookkeeping must label this relocation, not
  retirement, per the directive's insistence.

Neither is executed today. The pass-2 disposition: the offset-tape *parse/emit* core (the
`SinkOnly` program `generated.rs`) IS a clean one-fold candidate — it is already generated by
`json_sink_direct::render(sink_only)` from a lowered program with no static content — and a
prototype should first fold THAT (regen-json + check-json byte-identical against the committed
`generated.rs`), then measure form (a) for the lazy-view templates as the residual gap.

## 4. Name-gate RETIRED; role classifier RELOCATED — the precise split

The directive demands "retirement, not relocation" for two sites, and they differ:

- **`has_rule("value")`@`runtime_generator.rs:98` — RETIRED under the fold.** It keys off the
  literal rule NAME `"value"` (`lower/sink_only.rs:31` — `rules.iter().any(|r| r.name ==
  name)`). But the JSON-family fact it stands for is ALREADY computed structurally and
  name-free: `roles.value` (`passes/src/lib.rs:1454-1465`) is the rule referencing ≥4 value
  targets — no name string. The fold selects the JSON sum path on
  `materialization.roles.value.is_some()` (equivalently, `plan.descriptors` carrying a
  `Container` + `KeyValuePair`), so the string `"value"` disappears from dispatch. This is
  genuine retirement: the name-gate's decision is subsumed by a structural attribute the plan
  pass already emits, with zero relocation of the literal.
- **`derive_materialization_roles`@1403 — RELOCATED, not retired.** It is a byte-literal role
  classifier: `b"{"`+`b"}"`→container (`:1437`), `b"["`+`b"]"`→sequence (`:1445`),
  `SpanKind::String`→string (`:1407`), `b"true"`&`b"false"`→bool (`:1419`), `b"null"`→null
  (`:1430`), ≥4-ref→value (`:1462`). Folding the emitter does NOT remove the need for a role
  *source*; it only moves the classifier inside the fold. Retiring it requires the roles to be
  *declared* by the grammar's `->` projections (grammar-homoiousia / type-plane territory —
  the `DirectTypeKind::Sum` and threaded arrow of the GN-3 charter). Single-IR alone relocates
  it; retirement is a cross-family dependency, and honesty forbids claiming otherwise.

## 5. Per-`RuleId` suffices for structure; per-`ExprId` is required for the typed fleet

`MaterializationPlan.descriptors: HashMap<RuleId, MaterializationDescriptor>` and
`materialize_rule` keys by `rule_id` (`:1367`). For the STRUCTURAL fleet — JSON, Sheets — the
per-RuleId key SUFFICES: every materialized construct is a whole rule
(object/array/pair/string/number/bool/null), and CSS takes the `Scan` artifact with no
materialization at all. So today's live corpus is per-RuleId-clean.

But the `->` projection is inherently per-ExprId, not per-rule: a typed grammar's
`coordinates -> Vec<Vec<Vec<f64>>>` projects an inner expression, and `node_types` is *already*
keyed `HashMap<ExprId, Type>` (`:93`), while `DirectBuildField.target: Option<DirectBuildTarget>`
(`ir/src/lib.rs:466-490`) carries a per-field `type_ref: DirectBuildTypeRef`. The disposition:
**per-RuleId is sufficient for the structural descriptor spine; per-ExprId is required the
moment `Project.ty` is threaded** (the typed-grammar fleet — twitter/citm/canada — projects
sub-expressions). Skinny already owns both keyspaces (RuleId descriptors + ExprId
`node_types`), so 3B's attribute store is `{RuleId → descriptor, ExprId → projected_type}`, a
two-column join, not a re-key.

**Emission-extensibility 5→2 (measured at the emit boundary).** `BackendShape` has 5 variants
(`EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage`, `ir/src/lib.rs:361-367`) and
`decision_csp.rs:68-72,124-128` enumerates all 5 as CSP candidates with a priority order
(`passes/src/lib.rs:605-609`). But at the EMIT boundary only two artifacts exist —
`LoweredArtifact::{SinkOnly, Scan}` (`runtime_generator.rs:19-24`); `EagerTape`, `EventTape`,
`CollapsedStage` reach no distinct emitter (they are cost-model candidates the live path never
lands). So the 5 nominal shapes already collapse to 2 executed emit arms. Single-IR makes this
honest: `backend_shape` becomes an attribute the ONE fold reads, and the emit-extensibility
surface is 2 folds (structural-sink, byte-scan) — or, with form (a) of §3, a single
attribute-parameterized fold. **Net-LOC delta to prototype:** deleting `BackendExpr` (13 arms,
`ir/src/lib.rs:376-413`), `BackendIr`, `single_plan` (`:1283-1316`), `lower_expr`
(`:1318-1360`), the `materialize_rule` tree-splice, and the `SinkOnlyExpr` transposition
(`lower/sink_only.rs`), against +1 `AttributeStore` + emit-time decoration — a measured
deletion the prototype must quantify, NOT asserted here.

## 6. Shared `->` question — the ExprId-keyed CONSUMPTION contract

`infer_expr`@`:204` is `ExprKind::Project { body, .. } => self.infer_expr(*body)?` — it drops
`target`/`ty` (the `Annotation`→`Unit` arm at `:203` is a legitimate untyped directive, left
alone). Family 3B's consumption contract — how the fold reads *one* threaded node-type rather
than re-deriving it:

1. **Write once, as a column.** Extend the infer pass so the `Project` arm, besides returning
   the body's structural `Type` for the lattice, resolves `ty: Option<String>` into a
   `DirectBuildTypeRef` (`Named{type_id}` / `Scalar{kind}`, `ir/src/lib.rs:493-506`) and records
   it in a new attribute column `projected_types: HashMap<ExprId, DirectBuildTypeRef>` beside
   `node_types` (`:93`). One insert, keyed by the Project's `ExprId`.
2. **Read by join, never re-derive.** `derive_materialization_plan`'s field construction
   (`:1149-1287`, where `target: None` is hardcoded at `:1157,1177,1201,1208,1228,1246,1262`)
   READS `attrs.projected_types[expr_id]` to populate `DirectBuildField.target`. The emit fold
   thus consumes the threaded type off the arena node it is already visiting — it performs no
   independent classification.
3. **Delete the re-deriver.** The lossy 4-way `classify_projection`
   (`emit/general_descent/view.rs:20-46`, folding every `->` to `{Float,Bool,Tag,Span}`) is the
   *only* thing that re-derives projection semantics on the live path. Under the contract it is
   deleted; the sheets accessor generation reads the threaded `type_ref`. The consumption is
   proven by `check-sheets` byte-identity (the fold reads the attribute; the fold's output is
   unchanged).

For the OUTER-tree consumer the directive names (`TypeAnalysis::from_ir` in
`backend/driver/analysis.rs`), the isomorphic contract holds: it reads the same ExprId column
rather than re-running inference — but that crate is not part of the live skinny substrate
(`3f2350f15`), so the executed evidence lives at skinny's `DirectBuildField.target` join, and
the outer consumer is a port-scope (organ-level) obligation, not a live thread.

**The `verify` predicate (RED→GREEN, zero emit footprint).** A per-pass predicate `∀ ExprId e
with grammar.expr(e).kind == Project ⇒ attrs.projected_types.contains(e)` reads RED on HEAD
(the column does not exist) and GREEN once the thread lands. Emit footprint is provably zero on
the offset-tape path: `ValueProject` is a passthrough there (nothing reads `target`), and
`DirectBuildField.target` moving `None→Some` changes only the *typed* lane, so regen-json /
check-json emit byte-identical with the predicate on or off — the null-runtime-cost invariant
(SPEC §0 / Constitution v) holds by construction.

## 7. Prior art (the annotate-in-place lineage — citations)

- **LLVM.** One `Module/Function/BasicBlock/Instruction` IR; the new-PM `AnalysisManager` holds
  analysis results in side maps keyed by IR unit (`Value*`), and transform passes read them —
  no IR is rebuilt between passes. This is the exact "one IR + keyed analyses" shape;
  `BackendShape` is an analysis result, not a new IR.
- **MLIR.** `Operation` carries a named **attribute dictionary** (compile-time-known values) and
  passes read/write attributes on the same SSA graph; "dialects" are op/attribute sets over one
  IR. MLIR is the canonical citation for "BackendShape/tape-kind/projected-type as attributes."
- **Cranelift.** `DataFlowGraph` stores entities in dense `PrimaryMap`s and per-pass facts in
  `SecondaryMap<Inst, T>` / `SecondaryMap<Value, T>` — *precisely* the ExprId-keyed attribute
  column. `cranelift_entity` is the direct analogue of `node_types`/`projected_types`.
- **rustc.** `TypeckResults` is a side table keyed by `HirId`; types decorate the HIR rather
  than producing a retyped tree — the literal design of `HashMap<ExprId, Type>`.
- **GHC "Trees That Grow"** (Najd & Peyton Jones, 2017): one AST decorated with per-pass
  extension fields — attribute columns by another name, the formal justification for refining
  in place instead of transposing.

3B's null hypothesis against the sibling e-graph/grammar formulations: plain arena +
side-tables suffice for the live fleet without congruence closure or a grammar-specified IR;
the prototype must DEFEAT this by exhibiting a fleet grammar the flat columns cannot serve.

## 8. Named risks and counterexamples

- **R1 — static-template wall (§3).** The JSON lazy view is verbatim
  (`runtime_generator.rs:107-131`); a truly-single fold either re-derives those bytes (form a,
  a real delta) or bundles them (form b, relocation). The prototype MUST diff
  `general_descent(json.bbnf).view` against `json_templates/view.rs`; a large diff refutes
  effortless one-fold and forces the honest label.
- **R2 — serialization/golden determinism.** `BackendIr`/`BackendExpr` are
  `Serialize/Deserialize` (`ir/src/lib.rs:351,375`) and back golden snapshots. Deleting the
  tree changes the golden artifact; the `AttributeStore` must be serializable and the snapshots
  re-baselined, or determinism/regen gates break. Counterexample: any test snapshotting
  `BackendIr` JSON.
- **R3 — pass-order DAG.** Attribute columns have dependencies (types before materialization
  before shapes before recognizers); the fold needs a small dependency scheduler (the LLVM
  PassManager), else a column is read before written. Counterexample: `derive_shape_facts`
  reads the materialization plan (`shapes::derive_shape_facts(grammar, materialization)`,
  `:259-262`) — order is already load-bearing.
- **R4 — Discard loss is a WIN, not a risk.** The flattening at `:1354-1358` disappears because
  the arena retains `ExprKind::Discard{keep_left}` and the emit fold reads `keep_left` directly;
  single-IR *repairs* a standing no-value-discard violation the transposition introduced.
- **R5 — per-ExprId materialization (§5).** If any fleet grammar materializes an inner
  expression (not a whole rule), the RuleId descriptor key is insufficient; the typed-grammar
  fleet triggers this. Mitigation is already in hand (ExprId `node_types`), but the prototype
  must exhibit at least one per-ExprId materialization to prove the two-column store, not assume
  it.

## 9. What a prototype must test to move family 3B

1. **Fold the offset-tape core.** Implement `emit_sink_only` as `emit(&GrammarIr,
   &AttributeStore)` walking the arena + `{RuleId→descriptor}` column (materialize decoration at
   rule-entry per §2), deleting `BackendExpr`/`single_plan`/`lower_expr`. Gate: `cargo xtask
   regen-json && check-json` byte-identical against the committed `generated.rs`; record net-LOC
   deleted.
2. **The static-template disposition (§3, R1).** Point `general_descent::render_view` at
   `json.bbnf`; diff vs `json_templates/view.rs`; classify the residual as retire-able (form a)
   or relocate-only (form b) with the measured diff size. This decides whether "all six
   byte-identical in one fold" is honestly reachable.
3. **The `->` thread + join (§6).** Add `projected_types: HashMap<ExprId, DirectBuildTypeRef>`
   at infer@204; read it at plan@1149-1287 to fill `DirectBuildField.target`; delete
   `classify_projection`. Gate: the `verify` predicate RED→GREEN; `check-sheets` byte-identical;
   `regen-json`/`check-json` byte-identical (zero emit footprint proven).
4. **The extensibility census (§5).** Count emit arms before/after (5 nominal shapes → 2
   executed artifacts → target 1 attribute-parameterized fold), and the transposition count (3
   tree rebuilds → 0). Report measured LOC and arm counts, not estimates.

## 10. Pointer appendix

- Arena: `ir/src/lib.rs:16,19` (`RuleId`/`ExprId`), `add_expr`/`add_rule`; `nullability`
  `:320-349` (dense column).
- Rebuilds to delete: `lower_expr` `passes/src/lib.rs:1318-1360`; `materialize_rule`
  `:1362-1389`; `single_plan` `:1283-1316`; `SinkOnlyExpr` transpose `lower/sink_only.rs:175-238`.
- Attribute columns (live): `node_types` `passes/src/lib.rs:93,212,179`; `backend_shape`
  map; `nullability` `ir/src/lib.rs:320`.
- Materialization: `derive_materialization_plan` `:1139-1281`; `derive_materialization_roles`
  `:1403-1491`; descriptor `:1131-1137`; `DirectBuildField.target` `None`-fill `:1157,1177,
  1201,1208,1228,1246,1262`.
- Emit dispatch: `render` `runtime_generator.rs:78-87`; `has_rule("value")` `:98`;
  `has_rule` def `lower/sink_only.rs:31`; JSON static templates `:101-131`; general-descent
  `:147-188`; `emit_scan` `:211-247`.
- `->` thread: infer drop `passes/src/lib.rs:204`; lower `:1349-1353`; `classify_projection`
  fold `emit/general_descent/view.rs:20-46`; `DirectBuildTarget` `ir/src/lib.rs:483-490`;
  `DirectBuildTypeRef` `:493-506`.
- Emit-shape enum: `BackendShape` `ir/src/lib.rs:361-367`; CSP candidates `decision_csp.rs:68-72`;
  priority `passes/src/lib.rs:605-609`; live artifacts `runtime_generator.rs:19-24`.
- Residualization corollary: PROVE-12 (`const fn delim_mask` + const-generic mask, byte-identical,
  +18 LOC) — a per-site residual the single fold hosts as an emit-time const-fold attribute.
