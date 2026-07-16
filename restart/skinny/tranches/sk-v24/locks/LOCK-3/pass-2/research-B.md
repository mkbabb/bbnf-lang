# LOCK-3 · PASS-2 · FAMILY 3B — SINGLE-IR-MULTIPASS

Re-pinned to `skinny@c93fc4314` (the checked-out live tree; the pass directive names
`b3cb8c16b`, which is not this HEAD — every `path:line` below is re-verified against
`c93fc4314`, and any prototype must re-pin + re-cite before measuring). The R1/R6 seeds
carry line drift (`ir/lib.rs` 678 lines, not 1020; `passes/lib.rs` 2096, not 2750); the
pointers here are the drifted-correct ones. All paths under
`/Users/mkbabb/Programming/bbnf-lang/skinny/`.

## 0. Thesis

One rich IR — the existing `GrammarIr` (`ir/lib.rs:234` `ExprKind`, 10 arms) — refined in
place by annotating passes; every derived plane is an attribute table keyed by
`RuleId`/`ExprId` over that one tree; emission is a single generation-time fold
`emit(&GrammarIr, &Attributes)`. The live pipeline is ALREADY three-quarters this shape and
does not know it: `LayoutFacts` (`passes/lib.rs:91-99`) is already a bundle of eight
per-`RuleId`/`ExprId` attribute maps annotating one `GrammarIr` — `rule_types`, `node_types`,
`layout_policies`, `hot_call_graph`, `backend_shape`, `cost_facts`, `per_grammar_policy`,
`same_substrate_union`. The charter's "BackendShape as attributes" is ALREADY literally true:
`backend_shape: HashMap<RuleId, BackendShape>` (`:96`). What 3B removes is the *tower below
the attributes* — the two structurally-redundant transposition IRs (`BackendExpr`,
`SinkOnlyExpr`).

This is a **generation-time** fold producing straight-line Rust, NOT a runtime lazy-cursor
fold — SPEC §0's "fold-over-cursor dead" refutation governs the runtime and does not touch
it.

## 1. The enumerated edge-DAG on live (3 IR levels · 2 transpositions · 1 emit)

| Level | Type (arms) | Home | Producer |
|---|---|---|---|
| L0 | `GrammarIr`/`ExprKind` (10) | `ir/lib.rs:234` | `grammar::parse_grammar` |
| — attrs — | `LayoutFacts` (8 maps) | `passes/lib.rs:91` | `layout::run` (`:107`) + `derive_backend_shape` |
| L1 | `BackendIr`/`BackendExpr` (13) | `ir/lib.rs:352,376` | passes `lower_expr` (`:1318`) + `materialize_rule` (`:1362`) |
| L2 | `SinkOnlyProgram`/`SinkOnlyExpr` (13) | `lower/sink_only.rs:20,69` | `lower_program`→`lower_expr` (`:174`) |
| emit | Rust text | `runtime_generator.rs:78,89` | `render`→`emit_sink_only` → 3 emitter families |

**Transposition-1** (passes `lower_expr`, `:1318-1360`): a **1:1** structural re-projection
`ExprKind→BackendExpr` (Seq→Seq, Alt→Alt, Repeat→RepeatLoop, Literal→ByteLiteral,
Regex→RegexProgram, Ref→CallRule, Discard→Seq, Project→ValueProject carrying `ty` verbatim
at `:1349`) — then `materialize_rule` (`:1362`) splices the emit-order frame.

**Transposition-2** (`sink_only.rs:174-234`): `BackendExpr→SinkOnlyExpr`, again **near-1:1**;
the only deltas are lossy — `TapeEmit{kind}` drops its kind to a bare `TapeEmit`
(`:220`), `DirectBuild{shape,fields}` boxes into `DirectShape` (`:223`), `SpanMark` drops
its `SpanMarkKind` (`:217`). It is the same 13-arm sum re-declared a third time.

**The four synthetic emit-order value levels** (SpanMark/TapeEmit/DirectBuild/Return — the
framing the directive asks to reconcile) are NOT distinct IR tiers. They are five nodes
injected by ONE function, `materialize_rule` (`:1362-1389`), as `Seq[SpanMark(Start), body,
SpanMark(End), TapeEmit{kind}, DirectBuild{shape,fields}, Return]`, all sourced from one
`MaterializationDescriptor` keyed by `RuleId` (`descriptors.get(&rule_id)`, `:1367`). They
ride verbatim through transposition-2 into the emitter. So the true count of value levels
is: **one AST sum (`ExprKind`), re-declared twice, with a five-node emit frame spliced at
the first transposition** — reconciling `SinkOnlyProgram` (a copy) with the
SpanMark/TapeEmit/DirectBuild/Return framing (a spliced template, not a level).

## 2. The mechanism made concrete — `emit(&GrammarIr, &Attributes)`

Delete L1 and L2. The attribute bundle already mostly exists:

```
Attributes {
    layout:          LayoutFacts,           // passes/lib.rs:91 — ALREADY EXISTS (8 maps)
    materialization: MaterializationPlan,   // RuleId→descriptor, passes/lib.rs:1139
    recognizers:     Vec<Recognizer>,       // ir/lib.rs:416 SimdScan
    shape_facts:     ShapeFacts,            // ir/lib.rs:357 (in BackendIr today)
    scan:            Option<ScanAttr>,      // the CSS stop-set island (§4)
}
```

`emit` is one ENTRY generalizing `runtime_generator::render` (`:78`) — which today already
dispatches on a COMPUTED artifact variant and reads NO `target.*` field (the firewall). The
fold walks `GrammarIr` directly: at each rule it reads `attrs.layout.backend_shape[rule.id]`
to pick the emit family and `attrs.materialization.descriptors.get(&rule.id)` to decide
framing; at each `ExprKind` node it match-emits inline. `BackendExpr`/`SinkOnlyExpr` arms
become **match arms in the fold visitor**, not heap-allocated trees.

**`materialize_rule` is a pure derivation, recomputable inside the fold byte-identically.**
Data-flow trace: it reads only `body` and `materialization: &MaterializationPlan`, doing
`descriptors.get(&rule_id)`; the plan is built solely from the grammar
(`derive_materialization_plan(grammar)`, `:1139`, over roles from
`derive_materialization_roles`, `:1403`); the splice is a fixed 6-node template
parameterized by `(label, kind, shape, fields)`, all from the descriptor, all from
`GrammarIr`. It carries NO datum absent from the grammar. So the fold, on entering a
materialized rule, emits span-start, recurses the body, then emits span-end/tape-emit/
direct-build/return — without ever allocating `BackendExpr::Seq[...]`. Byte-identity holds
because the node ORDER is the same fixed template.

**The honest data-flow caveat.** `SinkOnlyProgram` collects five facts BY SIDE-EFFECT during
the L2 walk (`SinkOnlyFacts`, `sink_only.rs:264`): `direct_shapes`, `span_kinds`, `literals`,
`dispatch_alt_count`, `policy_summary`. `policy_summary` is pure attribute re-export
(`RuntimePolicySummary{backend_shape,…}`, all already in `LayoutFacts`). The other four are
TRAVERSAL ACCUMULATIONS — recomputable, but they prove the transposition currently carries
*collection state*, not just shape. The single fold must accumulate them in its one pass
(LLVM's one-walk-collects-all). Verified recomputable: none of the four reads anything
outside `GrammarIr` + `backend_shape` + the descriptor map.

## 3. The directive questions, adjudicated on live

**Are `has_rule("value")`@`:98` and `derive_materialization_roles`@`:1403` RETIRED or
relocated?**
- `has_rule("value")` (`runtime_generator.rs:98`; def `sink_only.rs:31`) is a literal-NAME
  gate (constitution vi forbids name-gates). Under 3B the json-family-vs-general choice
  becomes `attrs.layout.backend_shape[rule.id]` / an emit-strategy attribute — the name test
  is RETIRED. BUT `derive_materialization_roles` (`:1403-1490`) itself elects the `value`
  role by a ≥4-value-ref heuristic + byte-literals (`b"{"`,`b"["`,`b"true"`, `:1417-1462`;
  R6 §5c). Retiring `:98` relocates the decision INTO that classifier, which is ALSO a
  heuristic. TRUE retirement — strategy falling out of a SHAPE fact, not a heuristic —
  requires the role to be a projection of the inferred SUM type (`value = object|array|
  string|number|bool|null`), which `direct_schema.rs:52` cannot represent today (Struct-only)
  and which is LOCK-1's `DirectTypeKind::Sum` (GN-3 W1). **Disposition: 3B retires the `:98`
  name-gate into an attribute; the classifier heuristic is retired only under a BORN-RED
  edge owned by LOCK-1 — carried, never hidden.**

**Per-`RuleId` sufficiency vs per-`ExprId`.** The tape splice is intrinsically per-RULE on
the live fleet (JSON/Sheets/CSS give every value-kind its own rule; `descriptors:
HashMap<RuleId,_>`, consumed `materialize_rule(rule_id,…)`), so per-`RuleId` suffices for the
SPLICE. But the TYPE fact is per-`ExprId`: `Project.ty` (`ir/lib.rs:264`) and `ValueProject`
(`:407`) attach to an EXPRESSION; two inline projections `( a -> X | b -> Y )` can share a
rule. No live fleet grammar does this — it is the exact counterexample a prototype must
fabricate. The bundle already carries BOTH indices (`LayoutFacts.rule_types` and
`node_types`), so no new machinery is needed.

**Net-LOC delta & extensibility 5→2.** Deleting `BackendExpr`+`BackendRule`
(`ir/lib.rs:369-413`, ~45 LOC), passes `lower_expr` (`:1318-1360`, ~42 LOC), and
`SinkOnlyExpr`+its `lower_expr` (`sink_only.rs:69-235`, ~166 LOC) against a bundle that
mostly EXISTS (`LayoutFacts`) is a net removal in the low hundreds. Extensibility today:
adding one emit construct edits its `ExprKind` arm, `BackendExpr` arm, passes `lower_expr`
arm, `SinkOnlyExpr` arm, `sink_only::lower_expr` arm, then the emitter — five mirror-arms
before the emitter. Under one fold: the `ExprKind` arm + the fold arm — **two**. Measured by
adding one construct and counting edited sites.

**One fold, all six regen/check outputs byte-identical?** Scope honestly. Four of six
(regen/check × json/sheets) are the SinkOnly family; both already terminate in a walk
(`json_sink_direct::render` / `general_descent::render`) that one attribute-dispatched fold
reproduces — these collapse cleanly. CSS is a DIFFERENT artifact: `css_scan::derive_program`
(`runtime_generator.rs:57`; `lower/css_scan.rs`, 712 LOC) is a whole-grammar STOP-set
alphabet derivation (`ScanAlphabet::derive_alphabet` → `const &[u8]` → `css_scan_direct`),
NOT a per-node re-encode; it stays a sibling emit strategy keyed by `attrs.scan` under the
SAME `emit` entry (mirroring `render`'s variant dispatch at `:78`). "One fold" = one ENTRY
with attribute-selected strategy, not one recursive walk over CSS's alphabet. The honest
claim is deletion of the two SUM RE-ENCODINGS, not that CSS folds away.

## 4. The staged-residual island as a stage-0 attribute (bridge, not re-author)

The grammar-derived stop-set already ships (`ScanAlphabet::derive_alphabet` → const → SIMD
`css_scan_direct`); PROVE-12 residualized the CSS component-delimiter test to a const `u128`
bitmask (REPORT: +18 LOC, ins/B −3.7…6.4%, cyc/B 0.81…0.98×, byte-identical). In 3B this is
`attrs.scan: ScanAttr` — a stage-0 fact attached to the ONE IR that the fold reads to emit
the const. It is the concrete precedent that an emit decision is an attribute over the one
IR, computed once, consumed at the fold — not a separate lowering.

## 5. Shared question — threading `Project.ty`@`:204` into the ALREADY-EXISTING side-table

The home already exists. `Infer` already records per-node types into a side-table:
`self.facts.node_types.insert(expr_id, ty.clone())` (`passes/lib.rs:212`), harvested into
`LayoutFacts.node_types: HashMap<ExprId, Type>` (`:93`). The defect is narrow:
`ExprKind::Project { body, .. } => self.infer_expr(*body)?` (`:204`) returns the BODY type
and drops the DECLARED `ty` (`ir/lib.rs:264`, `Option<String>`). It is dropped a second time
structurally: `ty` survives into `ValueProject{ty}` (`:1349`) and `SinkOnlyExpr::
ValueProject{ty}` (`sink_only.rs:228`) but no JSON emitter reads it; on general-descent it is
RE-DERIVED to a 4-way enum by `classify_projection` (string-matching `"f64"/"Span"/…`,
`general_descent/view.rs:20-46`) — a re-derivation, not a consumption.

**Consumption contract (one threaded node-type, not re-derived).** At `:204`, resolve the
declared `ty` (HM per PROVE-08's 221-LOC core, unifying the projection annotation against the
body type) and RECORD it — into `node_types[expr_id]` or a companion `proj_types`. Then the
LIVE consumer: `derive_materialization_plan` (`:1139`) fills `DirectBuildField.target`
(`ir/lib.rs:470`, TODAY always `None`) → `DirectBuildTarget.type_ref` (`:485`) from that
attribute; the fold, at a materialization node, READS it to emit the struct field type —
directly replacing the `classify_projection` re-derivation with an attribute read. That is
the directive's "consumes one threaded node-type rather than independently re-deriving it"
contract. The greenfield `TypeAnalysis::from_ir` (XL-F) has ZERO live hits — it is a design
consumer, not extant; the real same-wave consumer satisfying constitution viii is
`json_typed_direct::render` (`codegen/src/json_typed_direct.rs:10`, 1245 LOC, already built),
whose only missing wire is the derived type (R6 §8b).

**Verify predicate.** A per-fold `debug_assert` differential: fold output with `attrs` ON ==
the frozen regen artifact (regen∘regen = regen, byte-for-byte). Reads RED on an injected
mis-lowering, GREEN clean; zero emit footprint = generated bytes identical with the predicate
on/off. Each attribute table is independently diffable against the tree, so an injected
mis-lowering localizes to its edge. This generalizes the existing XOR-checksum + `json_parity`
track1/track2 digests: the same differential runner over the six-output corpus reproduces
those digests AND localizes the injected fault to the attribute it perturbed.

## 6. Prior art (SOTA, cited)

- **GHC "Trees That Grow"** (Najd & Peyton Jones, IFL 2017) — the canonical solution to "one
  AST refined in place by annotating passes": ONE `Expr<P>` parameterized by a phase index,
  each phase decorating nodes with a type-family extension field (`XProject` = `()`
  pre-inference, `ResolvedType` post). Strongest theoretical match to 3B's mechanism. Rust
  lacks type families, so the faithful port is heavy (phantom phase + associated-type
  extension trait); the pragmatic Rust form is the side-table below — which skinny already
  has.
- **rustc `TyCtxt`/`TypeckResults`** — HIR→THIR→MIR is a multi-IR tower (the anti-3B), but
  rustc keeps TYPES as a `HirId`-keyed side-table (`TypeckResults`), never re-encoding them
  into nodes. This is EXACTLY skinny's `LayoutFacts.node_types: HashMap<ExprId, Type>`
  (`:93`) — the pragmatic Trees-That-Grow. 3B's directive is: stop dropping `Project.ty` at
  `:204` and let the existing `node_types` table carry it, rustc-style.
- **LLVM `PassManager`/`AnalysisManager`** — one Module mutated in place; analysis passes
  attach results addressed by `Value*`, transform passes read via `getResult<T>()`; no
  per-pass re-encode. `emit(&GrammarIr,&Attributes)` is the emit analogue: one IR, results as
  attributes.
- **MLIR** — attributes are first-class `DictionaryAttr` side-data on ops; progressive
  lowering legalizes ONE op tree in place. MLIR's own guidance warns of "IR proliferation"
  cost — direct support for minimizing transpositions. (Its multi-dialect model is the part
  3B declines: skinny's `BackendExpr`/`SinkOnlyExpr` are exactly two proliferated dialects
  that re-declare `ExprKind` with no new algebra.)
- **egg / egglog e-class analysis** — a lattice attribute per e-class via monotone
  `make`/`merge`. Relevant only if the materialization-role attribute must be
  fixed-point-computed; the base 3B mechanism needs no saturation. (Shared engine with
  LOCK-2 if roles ever become a saturated fact.)

## 7. Named risks / counterexamples a prototype must break

1. **CSS resists the per-node fold.** `css_scan` is a whole-grammar alphabet derivation, not
   a tree re-encode; it survives as an `attrs.scan`-keyed sibling strategy. "One recursive
   fold over all six" is false — the honest claim is deletion of the two SUM re-encodings
   under one emit ENTRY.
2. **Static-template files are not folded.** 5 of 8 JSON files
   (`value.rs`/`view.rs`/`visitor.rs`/`parser.rs`) are `include_str!` copies
   (`runtime_generator.rs:107-131`), not emitted from any tree. The byte-identity target is
   the GENERATED parser (`generated.rs`), never the templates.
3. **Name-gate retirement is skin-deep without LOCK-1's sum.** Retiring `has_rule("value")`
   relocates the heuristic into `derive_materialization_roles` unless the role is a projection
   of the inferred sum type — a BORN-RED edge, owner LOCK-1.
4. **Per-RuleId splice vs per-ExprId type.** Both indices exist in `LayoutFacts`, but a
   fabricated one-rule/two-inline-projection grammar breaks any pure per-RuleId key — the
   fold must key the type read by `ExprId`.
5. **SinkOnlyFacts is collection state, not shape.** The four accumulated facts
   (`direct_shapes`/`span_kinds`/`literals`/`dispatch_alt_count`) must be gathered in the
   fold's single pass; if any turns out to read a datum not in `GrammarIr` + `backend_shape`
   + descriptor, the transposition is NOT pure and the deletion fails.

## 8. What a prototype must test to move family 3B

1. Build `Attributes` (reusing `LayoutFacts`) + one `emit(&GrammarIr,&Attributes)` fold;
   delete `BackendExpr`+`SinkOnlyExpr`; regen json+sheets; assert regen/check × json/sheets
   BYTE-IDENTICAL to committed generated files (4/6). Proves the two encodings are pure
   re-projections with zero emit footprint.
2. Include CSS: prove `attrs.scan` dispatch reproduces regen/check-css byte-identical (the
   honest "all six").
3. Thread `Project.ty`@`:204` into `node_types`/`proj_types`; fill
   `DirectBuildField.target`; regen one typed grammar through `json_typed_direct`; assert the
   struct equals the hand schema (`xtask/src/real_typed_schema.rs`) — the RED→GREEN of
   node-type CONSUMPTION, replacing `classify_projection` re-derivation.
4. Wire the per-fold verify `debug_assert`; show RED on an injected mis-lowering, GREEN clean,
   zero byte-diff on/off; confirm it reproduces the XOR/json_parity digests and localizes an
   injected fault to its attribute.
5. Measure net-LOC delta (encodings deleted − bundle growth, near-zero since `LayoutFacts`
   exists) and the emission-extensibility count (add one construct; count edited sites;
   target 5→2).
</content>
