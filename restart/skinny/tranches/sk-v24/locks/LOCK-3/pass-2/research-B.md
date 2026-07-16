# LOCK-3 · FAMILY 3B — SINGLE-IR-MULTIPASS · PASS-2 RESEARCH

One rich IR refined in place by annotating passes (the LLVM shape); `BackendShape` as
attributes; the fewest transpositions that suffice. Base pinned to `skinny @ c43126422`
(HEAD 2026-07-16 14:21), the sole living implementation substrate. The outer bbnf-lang tree
`crates/` is cited as prior art only. Measurement claims are marked UNEXECUTED where they name
a prototype obligation (mapped ≠ executed).

---

## 0. The thesis, and why it is nearly already true

Skinny is not a five-transposition tower; it is **one attribute-annotated compile stage
followed by a fractured emission stage**. `passes::compile` (`crates/passes/src/lib.rs:32`)
already runs the LLVM shape: `layout::run` produces `LayoutFacts` (`:36`), then successive
passes **mutate that one facts bundle in place** —
`layout_facts.backend_shape = shape_plan.backend_shape` (`:54`), `.cost_facts` (`:55`),
`.per_grammar_policy` (`:56`), `.same_substrate_union` (`:57`). `LayoutFacts` already carries
`node_types: HashMap<ExprId, Type>` (`:91-93`). This is exactly an attribute plane keyed by
node identity, refined by annotating passes — the family's own architecture, already load-
bearing on live for shape/cost/policy.

The fracture is downstream, in emission alone: `runtime_generator::emit_sink_only`
(`crates/codegen/src/runtime_generator.rs:88`) **re-dispatches** through a name-gate —
`if !sink_only.has_rule("value") { return emit_general_descent(...) }` (`:99`) — into three
orthogonal emitters (JSON `include_str!` templates + `json_sink_direct::render` at `:106`;
`general_descent::render` for Sheets at `:159`; CSS via `css_scan::derive_program` at `:57`).
Family 3B's entire surface area is therefore **emission**, not the tower: collapse the three
name-gated emitters into one fold over the already-annotated IR, and thread the last dropped
attribute (`Project.ty`) into the plane the fold consumes.

The decisive prior-art witness that this converges: the outer `crates/` tree already completed
this move for the mature grammar plane. `TypeAnalysis::from_ir`
(`crates/core/src/backend/driver/analysis.rs:22`) is a **pure projection** of pre-annotated IR
— `rule_types: ir.types.clone()`, `scratch_types: ir.type_map…scratch_types()` — with zero
re-derivation. `analyze_grammar` (`:132`) is a linear pass pipeline each `&mut ir`-annotating
one IR: `project_types(ir)` (`:136`, writes `ir.type_map`), `run_path_check` (`:146`),
`compute_payload_layouts_with_resolver` (`:163`), `compute_push_fingerprint` (`:172`),
`mine_dedup_eligible_rules` (`:181`). Every backend then eats one `PreparedGrammar{ir, prep}`
(`:73`). Skinny's greenfield spine is this shape, minus the emission fracture.

---

## 1. The single fold — mechanism made concrete

**The fold.** Replace the three-way name-gate at `runtime_generator.rs:99` with one
`emit(program: &SinkOnlyProgram, attrs: &EmitAttributes) -> EmittedSource` whose only dispatch
is on **structural attributes already computed by `compile`**, never a rule name or file-family
branch. `EmitAttributes` is not new IR; it is the existing `LayoutFacts` sub-planes surfaced to
emission: `backend_shape` (per-RuleId `BackendShape`, `ir/src/lib.rs:361`, 5 variants),
`MaterializationPlan.descriptors` (`passes/src/lib.rs:1128`), the grammar-derived `ScanAlphabet`
(CSS stop-set, already a `const &[u8]` feeding `css_scan_direct` — the staged-residual
dependency, declared not re-authored), and `node_types` (`:93`). The fold walks the
`SinkOnlyProgram` rules once; for each rule it consults `attrs.backend_shape.get(rule_id)` and
`attrs.materialization.descriptors.get(rule_id)` to select the emit strategy — JSON lazy-tape
surface, eager sink, general-descent view, or CSS scan driver — as **data-driven arms of one
match**, not three top-level entry functions.

**What retires vs relocates (directive Q3, answered exactly).**
- `has_rule("value")` (`:99`) — the name-gate — is **RETIRED, not relocated**. Its structural
  meaning ("this grammar has a recursive sum-typed value rule") is already computed by
  `derive_materialization_roles` as `roles.value: Option<RuleId>` (the ≥4-value-ref heuristic,
  `passes/src/lib.rs:1451-1465`). The fold branches on `roles.value.is_some()` — a fact carried
  in `MaterializationPlan` — so the literal name `"value"` never appears in emission. Genuine
  retirement: the string `"value"` becomes deletable from `runtime_generator.rs`, and the
  declarative `RuntimeFrontendRequirements` (`grammar_provider.rs:38`) subsumes residual routing.
- `derive_materialization_roles` (`:1403`), the **byte-literal role classifier** (`b"{"`/`b"}"`
  → container `:1441`, `b"true"`&`b"false"` → bool `:1414`, `b"null"` → null `:1425`), is a
  harder case. Inside the fold alone it is merely **relocated** (codegen name-gate → attribute
  pass) — honest, but not retirement. Its true retirement is **born-RED on the type plane**:
  once `Project.ty` threads (§2) and a sum-type schema node exists (`DirectTypeKind::Sum` —
  today `direct_schema.rs:52` is `Struct`-only), the sum `value = object | array | string |
  number | bool | null` is *declared*, and the heuristic is superseded by inference. 3B's
  disposition: **relocate now (unblocked), retire on the GN-3 sum-node dependency edge** —
  carried as a named RED gate owned by the type plane, never asserted retired absent that node.

**Is `materialize_rule` recomputable inside the fold, byte-identically (directive Q2)?**
YES, provably, and it carries **no data absent from `GrammarIr`**. `materialize_rule`
(`passes/src/lib.rs:1361`) is a pure splice `Seq[ SpanMark(Start,label), body,
SpanMark(End,label), TapeEmit{kind}, DirectBuild{shape,fields}, Return ]` whose every input is
a field of `MaterializationDescriptor` (`label`, `kind`, `shape`, `fields`, `:1132`), and every
descriptor field derives from `GrammarIr` alone (`derive_materialization_plan`, `:1136-1280`,
reads only rules/literals/regex-kinds). Nothing in the splice consults input text, a cursor, or
a name. So the emit-order framing is a **pure derivation over `(GrammarIr, MaterializationPlan)`**
recomputable inside `emit` by a descriptor lookup. The prototype obligation is the byte-identity
witness: reconstruct the splice from `attrs.materialization` inside the fold and diff the six
regen outputs (regen×{json,css,sheets}) against HEAD — UNEXECUTED; the family's move-gate.

**Can emission become one fold with six byte-identical outputs (directive Q1)?** This is the
terminal claim and is **UNEXECUTED — mapped, not executed**. The map is above; the executed
result is the required evidence. Risk surface in §5.

---

## 2. Threading `Project.ty` — the shared `:204` question, for a single-fold consumer

**The drop.** `passes/src/lib.rs:204` — `ExprKind::Project { body, .. } => self.infer_expr(*body)?`
— returns the *body's* structural type and discards `ty: Option<String>`. The next lines
memoize: `self.facts.node_types.insert(expr_id, ty.clone())` (`:212`) — so skinny **already has
a per-ExprId type plane**, and `:179` **already consumes it** (`if let Some(ty) =
self.facts.node_types.get(&expr_id)` short-circuits re-inference). The consume-the-annotation
plumbing is present; only the Project arm writes the wrong value.

**The end-to-end thread (single-fold form) — CONSUMES one node-type, does not re-derive.**
1. **Carrier.** Skinny's `Type` (`passes/src/lib.rs:236-244`) has `Rule(RuleId)` but no
   `Named` arm — it cannot name a projected type. Add `Type::Named(StringId)` mirroring the
   outer tree's `TypeDesc::Named` (`backend-agnostic-types` law: the name resolves per backend,
   not in the IR).
2. **The write.** At `:204`, when `ty = Some(name)`, produce `Type::Named(name)` (unified
   against the body's structural type for soundness); the existing `:212` insert records it at
   the Project `ExprId`. Nothing else in infer changes.
3. **The consumer contract.** `DirectBuildTarget.type_ref: DirectBuildTypeRef`
   (`ir/src/lib.rs:485`) exists; `DirectBuildField.target` (`:470`) is `Option` and is **`None`
   at every one of the ~9 descriptor sites** (`passes/src/lib.rs:1157,1177,1201,1208,1228,1246,
   1262,1280`). The thread fills it: `derive_materialization_plan` reads
   `facts.node_types[project_expr_id]` and lowers `Type::Named → DirectBuildTypeRef`. The fold
   then **consumes** that annotation by map lookup, exactly as `TypeAnalysis::from_ir`
   (`analysis.rs:22`) consumes `ir.type_map` without re-deriving — the precise contract the
   directive demands: emission reads one threaded node-type, not a re-classified byte-literal.
4. **The downstream wave that consumes it** (Constitution viii): the direct-to-struct emitter
   `json_typed_direct::render` (`json_typed_direct.rs:10`, 1245 LOC) via
   `schema_direct::lower_program` (`lower/schema_direct.rs:11`). Its only missing input is a
   *grammar-derived* `DirectSchemaSet` — currently hand-authored at `xtask/src/real_typed_schema.rs:7`
   (1014 LOC). The threaded `type_ref` is the wire that lets a `derive_direct_schema` pass replace
   the hand file. Named, on-live, GN-3's charter — so no substrate without a consumer.

**Verifier predicate (RED→GREEN + zero emit footprint).** A per-pass `verify` over
`ExprKind::Project@204`: `assert node_types[project_expr] carries the projected type, not the
body type`. HEAD reads **RED** (arm inserts the body type). With the thread it reads **GREEN**.
The **emit footprint is zero** until a schema consumer reads `type_ref`: the JSON path never
reads `ty` (`json_sink_direct/first_set.rs:84` recurses into body); the Sheets path folds `ty`
to a 4-way `ProjectionKind` (`general_descent/view.rs:20-46`) independent of `node_types`. So
regen is **byte-identical with the predicate/thread on or off** — the born-RED gate flips GREEN
without moving one emitted byte, the clean threading proof the directive asks for. UNEXECUTED —
the RED→GREEN transition + byte-identity diff is the prototype's first deliverable.

---

## 3. The edge-DAG and the materialization key (Q4 + edge enumeration)

**Labeled transposition edges of the live spine.** From `compile` + emission:
`GrammarIr` --(`layout::run`)--> `LayoutFacts{node_types}` --(`extract::single_plan`, `:40`)-->
`BackendIr` --(`recognizers::derive_backend_shape`, `:48`)--> `LayoutFacts{backend_shape,
cost_facts,policy}` --(`lower::lower_to_rust` → `emit_sink_only`)--> `EmittedSource`. **Distinct
value levels: three** — the structural `Type` plane (`node_types`, ExprId-keyed), the
`BackendExpr`/`SinkOnlyProgram` materialized plane (the `SpanMark/TapeEmit/DirectBuild/Return`
splice), and emitted Rust text. `SpanMark/TapeEmit/DirectBuild/Return` is not a fourth level; it
is the **instruction encoding of the one materialized plane**, and `SinkOnlyProgram` is that
plane's container. So "fewest transpositions that suffice" is concrete: the tower is already 3
levels; the ambition is not to add levels but to make level-2→3 (emission) one fold rather than
three gated emitters. UNEXECUTED: emit the edge-DAG as a machine artifact; confirm no hidden
fourth level inside `css_scan`.

**Per-RuleId vs per-ExprId (Q4).** Skinny keys materialization **per-RuleId**
(`MaterializationPlan.descriptors: HashMap<RuleId, _>`, `:1128`; `materialize_rule(rule_id,…)`,
`:1361`). Sufficient for JSON and Sheets, where each rule materializes one shape. The
**counterexample class** is any rule whose body holds two distinct anonymous sub-productions
that materialize different shapes — CSS L4's inline component-value alternatives are the live
candidate. The prior-art tell is decisive: the outer tree keys attribute planes by **`NodeId`,
not `RuleId`** — `TypeMap.node_types: HashMap<NodeId, TypeDesc>`
(`crates/ir/src/passes/types/type_map.rs:33`), `alt_strategies`, `delim_scan_configs`,
`key_dispatch_configs` all `HashMap<NodeId, _>` (`analysis.rs:57-68`) — because one rule holds
many distinct Seq/Alt nodes each carrying its own decision. **Disposition:** per-RuleId is the
JSON/Sheets special case; the general key is per-ExprId (the outer tree's `NodeId`). Skinny
already keys `node_types` by `ExprId` (`:93`) — so the migration reuses skinny's existing node
identity. UNEXECUTED: the fleet check (json/css/sheets + `grammar/misc/csv.bbnf`) must report
whether any single rule needs two distinct descriptors — the pass/fail that decides the key.

**Net-LOC and extensibility (5→2).** Today adding a grammar family touches ≥3 emit entry points
(`emit_sink_only` JSON arm, `emit_general_descent` `:145`, the CSS `css_scan` route `:57`) plus
the 5-variant `BackendShape` selection. The single fold makes a new family an **attribute-pass
edit + one fold arm** — the "5→2" extensibility target. Net-LOC is a deletion story: the name-
gate branch and the duplicated roster/validation in `emit_general_descent` collapse into the
shared fold; the `include_str!` template splices (`:100-133`) survive as leaf strategies.
UNEXECUTED — measure the delta on the executed fold, not the mapped one.

---

## 4. Prior art — citations

- **LLVM / rustc annotate-in-place.** LLVM's pass manager refines one `Module` by annotating
  analyses (`AnalysisManager` caches `DominatorTree`/`AAResults` keyed by IR value/BB identity,
  invalidated per pass). rustc's query system memoizes typeck keyed by `DefId`/`HirId` and
  consumes it in MIR lowering without re-deriving — the exact shape skinny's `facts.node_types`
  lookup at `:179` implements at micro-scale and the outer `ir.type_map` at macro-scale. The
  family's claim (one IR, attributes keyed by stable node identity, passes annotate + consumers
  read) is mainstream compiler architecture; its risk is discipline (invalidation, §5), not
  novelty.
- **MLIR attributes/interfaces.** MLIR carries lowering decisions as `Attribute`s on ops and
  dispatches through `DialectConversion` patterns matched on op+attribute, never on an op-name
  string — the precise anti-name-gate discipline directive Q3 demands. The single fold is MLIR's
  "one conversion framework, pattern-matched on structure."
- **The outer bbnf-lang `crates/` tree** is the closest witness: `analyze_grammar`
  (`analysis.rs:132`) is the executed single-IR-multipass pipeline, `TypeAnalysis::from_ir`
  (`:22`) the pure consumer, `TypeMap` (`type_map.rs:33`) the NodeId-keyed type plane. It proves
  the shape converges on real JSON/CSS/Sheets grammars and names the key choice (NodeId).
- **PROVE-12** (`prototypes/PROVE-12/REPORT.md`, verdict GO, base `d2b32f6e3`): CSS
  `find_component_delim` delimiter test const-folded to a `u128` bitmask, measured 0.81–0.98×
  cyc/B and 3.7–6.4% fewer ins/B on `css_canon_bench`, +18 gen LOC, byte-identical parity — a
  stage-0 erasure done inside the emit path. Evidence the single fold and a staging attribute
  coexist: the fold consults a `const`-eval'd grammar constant and residualizes it. 3B consumes
  this as a leaf strategy, not a competing subsystem.

---

## 5. Named risks and counterexamples

1. **Byte-identity is a hard oracle that may fail on incidental text, not semantics.** The three
   emitters differ in header comments, roster order, `BTreeMap` key order
   (`runtime_generator.rs:110`). A fold that reorders emission breaks byte-identity with no
   semantic change. Mitigation: the fold preserves per-family roster order as a descriptor field;
   the prototype attributes every non-identical byte incidental-vs-semantic. Likeliest reason Q1
   reads amber on first execution.
2. **Attribute invalidation.** Refining `LayoutFacts` in place already mutates after
   `derive_backend_shape` (`:54-57`), coupled by `debug_assert` to `facts.chosen` (`:58-61`).
   Widening the plane (threaded `type_ref`, ExprId-keyed materialization) widens the invariant
   surface — a stale attribute is a silent mis-lowering. Mitigation: the `differential(edge,
   corpus)` runner must localize an injected stale attribute to its edge.
3. **CSS may hide a fourth level.** `css_scan::derive_program` (`:57`) + `lower/css_scan.rs`
   (712 LOC) may carry emission state not in `GrammarIr` (grammar-name prefixes, entry-rule
   threading). If so the fold cannot be a pure `emit(&GrammarIr,&Attributes)` for CSS without
   first lifting that state into an attribute — the concrete threat to Q2's "no data absent from
   `GrammarIr`," to trace before Q1 can be claimed.
4. **The sum-node dependency is real and not owned by 3B.** Retiring the byte-literal role
   classifier (Q3) is gated on `DirectTypeKind::Sum` (GN-3.W1). Absent it, 3B ships *relocation*,
   not retirement — and must say so, never assert the name-gate census clean when only the
   `"value"` string is gone.
5. **Per-RuleId→per-ExprId migration touches every descriptor site.** The ~9 `descriptors.insert`
   sites (`:1146-1269`) plus `materialize_rule`'s signature change with the key. A partial
   migration leaving `RuleId` on JSON and `ExprId` on CSS is the orthogonal-codepath defect the
   memory law forbids (`no-orthogonal-codepaths`). One key, fleet-wide, or the migration is
   unsound.

---

## 6. What a prototype must test to move this family

1. **Execute the single fold** — one `emit(&SinkOnlyProgram,&EmitAttributes)` replacing the
   `has_rule("value")` gate + `emit_general_descent` + CSS route — producing the **six regen
   outputs byte-identical** to HEAD (regen×{json,css,sheets}). Executed, not mapped. The move-
   gate; anything less is SPEC-ONLY (Constitution vii).
2. **Execute the `:204` thread**: show the `verify` predicate flip **RED→GREEN with byte-
   identical regen** (zero emit footprint); then fill `DirectBuildField.target` from
   `node_types` and show the derived `DirectSchemaSet` reproduce the hand `real_typed_schema.rs`
   emit byte-for-byte (GN-3's idempotence target) — proving the fold *consumes* the threaded type,
   not re-derives it.
3. **Run the fleet key-check**: does any rule in json/css/sheets/csv need two distinct
   materialization descriptors? Report per-RuleId sufficiency or the per-ExprId requirement with
   the exact counterexample rule.
4. **Build the `differential(edge, corpus)` runner**: generalize the XOR-checksum + `json_parity`
   track1/track2 digests so it (a) reproduces those digests and (b) localizes an **injected**
   stale-attribute / mis-lowering to the edge it was injected on; report zero-witness constructs
   as *unverified*, never *sound*.
5. **Emit the edge-DAG artifact** and confirm no fourth value level hides inside `css_scan`
   (risk 3) — the pure-derivation claim (Q2) stands or falls here.
6. **Measure net-LOC delta and the 5→2 extensibility count** on the executed fold; and, if the
   thread lands with a residual consumer, the cold paired-PMU JSON first-byte dispatch delta —
   measured ins/B, per PROVE-09's law that unmeasured plausibility does not count.
