# LOCK-3 · Family 3B — SINGLE-IR-MULTIPASS (the LLVM shape)

**Live pin.** One repo, HEAD `06e70053f` (the directive's stated base `b3cb8c16b` is
an earlier lock-minting ancestor; the crate sources cited below are byte-identical
between them). Two IR trees coexist under this HEAD and the whole family turns on
their difference:

- **skinny** (`skinny/crates/{ir,passes,codegen,runtime}`) — the SIMD/measurement
  substrate (PROVE-01/03/07/12 landed here). It carries a **second IR**:
  `GrammarIr`(`ExprKind`) → `BackendIr`(`BackendExpr`) via `lower_expr` +
  `materialize_rule`. Types are a monotone structural fold; `->` is dropped.
- **root** (`crates/core`, `crates/ir` = `bbnf_ir`) — the mature type-plane tree.
  **It is already a single-IR-multipass in the LLVM shape.**

## The family already exists — in the root tree, live and compiling

`crates/core/src/backend/driver/analysis.rs:79` `prepare_grammar(mut ir, …)` is the
canonical realization of 3B: **one `GrammarIR`, refined in place by a pass sequence,
BackendShape carried as attributes.** `analyze_grammar` (`:132`) runs
`compute_sp_method_rules` → `project_types` (`:136`) → `run_path_check` (`:146`) →
`compute_payload_layouts_with_resolver` (`:163`) → `compute_push_fingerprint`
(`:172`) → `mine_dedup_eligible_rules` (`:181`), each writing a side-table onto the
same `ir`: `ir.types`, `ir.type_map`, `ir.materialization`, `ir.payload_layouts`,
`ir.dedup_eligible_rules`, `ir.delim_scan_configs`, `ir.key_dispatch_configs`,
`ir.node_facts`, `ir.eclass_facts`. `BackendPreparation` (`analysis.rs:52-69`) then
**clones the authoritative decisions straight off the IR** (`:89-90`:
`ir.delim_scan_configs.clone()`), keyed by `NodeId`. There is no second AST, no
splice, no re-lowering. This is precisely "BackendShape as attributes; the fewest
transpositions that suffice."

Skinny is the anti-pattern the family retires. Its `materialize_rule`
(`passes/src/lib.rs:1362`) **materializes** an intermediate —
`Seq[SpanMark(Start), body, SpanMark(End), TapeEmit{kind}, DirectBuild{shape,fields},
Return]` (`ir/src/lib.rs:396-412`) — whose every field is a member of `descriptor`,
a pure function of `RuleId` via the plan. The root tree proves this need not be a
node at all: it is `ir.materialization: HashMap<NodeId, MaterializationClass>`
(`crates/ir/src/passes/materialization/classify.rs:88`, written at `:151`), an
attribute the emitter reads.

## Answering the one-fold formulation on live

**materialize_rule is pure derivation, recomputable in the fold, byte-identically.**
Its output carries no datum absent from `GrammarIr` + `MaterializationPlan`
(`passes/src/lib.rs:1139`). The `BackendExpr::{SpanMark,TapeEmit,DirectBuild,Return}`
arms are eliminable; the emit-order template inlines at the fold site. Evidence
that this is safe: the root tree emits from `PreparedGrammar{ir, prep}` with **no**
such nodes, under the standing byte-identical-regen invariant.

**`emit(&GrammarIr,&Attributes)` as one fold.** Re-express `single_plan`
(`passes/src/lib.rs:1283`, which builds `BackendIr` via `lower_expr`+`materialize_rule`)
as a fold over `grammar.rules` consulting `Attributes{ materialization, types,
recognizers, shape_facts }`. "One fold" unifies **emitter selection**, not the three
emitter bodies — `json_sink_direct::render` (`runtime_generator.rs:103`),
`emit_general_descent` (`:147`), `css_scan_direct::render` (`:228`) are genuinely
distinct backends and stay. All six outputs (regen/check × json/css/sheets) must
come out byte-identical; that is the gate, not a claim.

**has_rule("value")@98 and derive_materialization_roles@1403.**
`sink_only.has_rule("value")` (`runtime_generator.rs:98`) is an emitter-selection
**name-gate** — it must be **RETIRED**, replaced by a shape predicate over
`ir.materialization` (the root tree selects by shape fact, never rule name; the
zero-grammar-name-predicate grep-gate demands it). `derive_materialization_roles`
(`passes/src/lib.rs:1403`) **RELOCATES**: its structural role assignment becomes a
`classify_materialization`-shaped annotating pass writing `GrammarIr.materialization`.
But its byte-literal / ≥4-value-ref heuristics (`:1419-1463`) only fully retire where
the grammar declares types via `->` — and `json.bbnf` has **zero** `->`, so on the
shipped JSON grammar the structural classifier survives as a lawful shape pass, not a
name-gate. Retirement of the *gate*, relocation of the *classification*.

**Per-RuleId vs per-ExprId — settled empirically on live: BOTH, and both required.**
Per-RuleId suffices for the type/value plane: `ir.types: Vec<(RuleId, TypeDesc)>`
(`analysis.rs:17`), skinny's `MaterializationPlan: HashMap<RuleId, _>`. Per-**NodeId**
(≈ExprId) is **mandatory** for the scan/recognizer/strategy plane —
`ir.materialization`, `ir.delim_scan_configs`, `ir.key_dispatch_configs`,
`alt_strategies` are all `HashMap<NodeId, _>` because one rule body holds many nodes,
each with its own scan/Alt decision (CSS `find_component_delim`'s five distinct
delimiter masks; PROVE-12). skinny's per-RuleId `materialize_rule` is sufficient only
because JSON's value taxonomy happens to be rule-granular. **Net-LOC:** delete
`BackendExpr` (37-line enum), `BackendIr`, `lower_expr` (~45), `materialize_rule`
(~30), the `single_plan` build; add the fold + a `classify_materialization` pass +
`project_types`. Emission-extensibility 5→2: `BackendShape`'s five variants
(`ir/src/lib.rs:361`: EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage) collapse
toward attribute-driven {tape-emit, sink-emit} dispatch.

## The shared `->` type value, consumed not re-derived

The consumption contract already runs in the root tree and is what 3B threads:
`project_types` (`crates/ir/src/passes/types/mod.rs`) populates `ir.types`;
`TypeAnalysis::from_ir` (`analysis.rs:22`) **reads** it (`ir.types.clone()`, `:24`) —
no independent re-derivation. `TypeDesc` (`crates/ir/src/types/type_desc.rs:13`) is a
real lattice (`Option`, `Vec`, `Tuple`, `Scalar`, `Named(StringId)@:47`), and it is a
**stage-0 residual**: erased to zero runtime bytes, consumed only at codegen —
Constitution (v) as a corollary of the single fold.

In skinny the identical chain is **missing its `project_types`**. Infer drops `ty`:
`passes/src/lib.rs:204` `ExprKind::Project { body, .. } => self.infer_expr(*body)?`.
`ty` is carried verbatim into `BackendExpr::ValueProject` (`:1349`) but is a no-op on
JSON and folded to a 4-way enum on sheets. The rich schema slot that *would* consume
it — `DirectBuildTarget{type_ref,…}` (`ir/src/lib.rs:483`) — is `target: None` at
**every** descriptor (`passes/src/lib.rs:1157,1177,1206,1228…`): a **consumer-less
substrate today (Y.13 flag).** The end-to-end thread for 3B: (1) a `project_types`-shaped
pass writes `GrammarIr.types` from the `:204` node-type; (2) `derive_materialization_plan`
reads `ir.types[rule]` into `DirectBuildTarget.type_ref` instead of `None`; (3) the
already-built `json_typed_direct` renderer (the real consumer, fed today by the
hand-authored `xtask/.../real_typed_schema.rs`) reads it. The consumer is
`TypeAnalysis::from_ir`-shaped: a struct reading `ir.types`, which becomes the single
fold's `Attributes.rule_types`. `direct_schema.rs:52` `DirectTypeKind` is **Struct-only**
today — sums stay a born-RED gate (a `Sum`/`Named` arm) owned by the type plane, not
by 3B.

## Named risks / counterexamples

1. **The two-tree schism (load-bearing).** The single IR that *has* the type plane
   (root `bbnf_ir`) is **not** the one that *has* the SIMD wins (skinny). 3B's "it
   already exists" is true of root, but PROVE-01/03/07/12 were measured on skinny.
   The real work is a migration — port skinny's emit organs onto root's
   pass-annotation spine, or port `project_types`/`classify_materialization` onto
   skinny's `GrammarIr`. Neither tree is complete; the family must pick a home and
   name it, not assume convergence.
2. **json.bbnf has no `->`** — the role classifier cannot be *fully* deleted without
   authoring JSON types; structural classification is retained (relocated), gate
   retired.
3. **Byte-identical regen across six outputs** is an unforgiving gate; any
   reordering of the inlined SpanMark/TapeEmit template breaks parity.
4. **PROVE-12 base is stale** (`d2b32f6e3`, raw-string `CSS_GENERATED_RS`); on live
   the const-mask residual already homes in the structured `css_scan_direct` emitter
   / root `ir.delim_scan_configs` — a per-NodeId stage-0 attribute, i.e. 3B's
   "BackendShape as attributes" already delivering the F3 win.

## What a prototype must test to move the family

In **skinny**: delete `BackendExpr::{SpanMark,TapeEmit,DirectBuild,Return}` +
`materialize_rule`; add a `classify_materialization`-shaped pass writing
`GrammarIr.materialization` (relocated roles) and a `project_types`-shaped pass
writing `GrammarIr.types` (threaded `:204` `ty`); re-express `single_plan` as one
`emit(&GrammarIr,&Attributes)` fold that inlines the emit-order template. Prove: (a)
all six regen/check outputs (json/css/sheets) byte-identical, reproducing the
XOR-checksum + json_parity track1/track2 digests; (b) `DirectBuildTarget.type_ref`
now populated from `ir.types` and reaching `json_typed_direct` (a real consumer, Y.13
cleared); (c) `has_rule("value")` and the ≥4-ref heuristic gone from codegen
(grep-gate green); (d) regen byte-identical with the type attribute on/off = type
plane is stage-0-erased, zero runtime footprint; (e) net-LOC delta and
emission-extensibility count reported.
