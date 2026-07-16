# LOCK-3 · PASS-2 · FAMILY 3B — SINGLE-IR-MULTIPASS

Re-pinned to `skinny@779013467` (the pass-2 launch tree; base `ce51c36fc`). Every
`path:line` opened on this tree; the R1/R6 seeds carried line drift (`ir/lib.rs`
678 not 1020, `passes/lib.rs` 2096 not 2750) — the pointers below are re-verified.

## 0. Thesis

One rich IR — the existing `GrammarIr` (`ir/lib.rs:234` `ExprKind`, 10 arms) —
refined in place by annotating passes; every derived plane is an attribute table
keyed by `RuleId`/`ExprId` over that one tree; emission is a single generation-time
fold `emit(&GrammarIr, &Attributes)`. The live pipeline is ALREADY three-quarters
this shape and does not know it: `LayoutFacts` (`passes/lib.rs:91-100`) is already
a bundle of seven per-`RuleId`/`ExprId` attribute maps annotating one `GrammarIr`
(`rule_types`, `node_types`, `backend_shape`, `cost_facts`, `per_grammar_policy`,
`same_substrate_union`, `hot_call_graph`). The charter's "BackendShape as
attributes" is already literally true: `backend_shape: HashMap<RuleId, BackendShape>`
(`:96`), filled at `passes/lib.rs:54`. What 3B removes is the *tower below the
attributes* — the two structurally-redundant transposition IRs.

This is a **generation-time** fold producing straight-line Rust, NOT a runtime
lazy-cursor fold — the SPEC §0 "fold-over-cursor dead" refutation governs the
runtime and does not touch it.

## 1. The enumerated edge-DAG on live (3 IR levels, 2 transpositions, 1 emit)

| Level | Type (arms) | Home | Producer |
|---|---|---|---|
| L0 | `GrammarIr`/`ExprKind` (10) | `ir/lib.rs:234` | `grammar::parse_grammar` |
| — attrs — | `LayoutFacts` (7 maps) | `passes/lib.rs:91` | `layout::run` + `derive_backend_shape` (`:36,48`) |
| L1 | `BackendIr`/`BackendExpr` (13) | `ir/lib.rs:352,376` | `extract::single_plan` (`passes/lib.rs:1283`) |
| L2 | `SinkOnlyProgram`/`SinkOnlyExpr` (13) | `lower/sink_only.rs:20,69` | `lower_program` (`sink_only.rs:126`) |
| emit | Rust text | `runtime_generator.rs:89` | `emit_sink_only` → 3 emitter families |

**Transposition-1** (`single_plan`, `passes/lib.rs:1283-1316`): loops
`grammar.rules`, calls `lower_expr` (`passes/lib.rs:1318`) — a **1:1** structural
re-projection of `ExprKind`→`BackendExpr` (Seq→Seq, Alt→Alt, Repeat→RepeatLoop,
Literal→ByteLiteral, Regex→RegexProgram, Ref→CallRule, Project→ValueProject) — then
`materialize_rule` (`:1362`) splices the emit-order frame.

**Transposition-2** (`lower_program`→`lower_expr`, `sink_only.rs:126,174-234`):
`BackendExpr`→`SinkOnlyExpr`, again **near-1:1** (the only deltas: `TapeEmit{kind}`
drops its kind to a bare `TapeEmit`; `DirectBuild{shape,fields}` boxes into
`DirectShape`; `SpanMark` drops its `SpanMarkKind`). It is the same 13-arm sum
re-declared a third time.

**The four synthetic emit-order value levels** (the SpanMark/TapeEmit/DirectBuild/
Return framing the directive asks to reconcile) are NOT distinct IR tiers — they
are five nodes injected by ONE function, `materialize_rule` (`:1362-1389`), as
`Seq[SpanMark(Start), body, SpanMark(End), TapeEmit{kind}, DirectBuild{shape,
fields}, Return]`, all sourced from a single `MaterializationDescriptor` keyed by
`RuleId` (`descriptors.get(&rule_id)`, `:1367`). They ride verbatim through
transposition-2 (`sink_only.rs:217-233`) into the emitter. So the true count of
value levels is: **one AST sum (`ExprKind`), re-declared twice (`BackendExpr`,
`SinkOnlyExpr`) with a five-node emit frame spliced at the first transposition.**

## 2. The mechanism made concrete — `emit(&GrammarIr, &Attributes)`

Delete L1 and L2. The attribute bundle is:

```
Attributes {
    layout:         LayoutFacts,              // passes/lib.rs:91 — already exists
    materialization: MaterializationPlan,     // RuleId→descriptor, passes/lib.rs:1139
    recognizers:    Vec<Recognizer>,          // ir/lib.rs:416
    shape_facts:    ShapeFacts,               // ir/lib.rs:537
    proj_types:     HashMap<ExprId, Option<String>>,  // NEW (§3)
}
```

The emitter walks `GrammarIr` directly. At each rule it reads
`attrs.layout.backend_shape[rule.id]` to pick the emit family, and
`attrs.materialization.descriptors.get(&rule.id)` to decide framing; at each
`ExprKind` node it match-emits inline. `materialize_rule`'s splice is a **pure
derivation** — `descriptor` is a function of the grammar alone
(`derive_materialization_plan(grammar)`, `passes/lib.rs:37`) — so it is recomputed
inside the fold: emit span-start, recurse body, emit span-end/tape-emit/direct-build,
without ever materializing `BackendExpr::Seq[...]`. `BackendExpr`/`SinkOnlyExpr`
arms become **match arms in the fold visitor**, not heap-allocated trees.

**Data-flow caveat (the honest check).** `SinkOnlyProgram` carries five derived
facts collected BY SIDE-EFFECT during the L2 walk (`SinkOnlyFacts`,
`sink_only.rs:264`): `direct_shapes`, `span_kinds`, `literals`,
`dispatch_alt_count`, `policy_summary` (`sink_only.rs:23-27`). `policy_summary` is
pure attribute re-export (`RuntimePolicySummary{ backend_shape, substrate_target,
… }`, `:47-53` — all already in `LayoutFacts`). The other four are traversal
accumulations — recomputable, but they prove the transposition currently carries
**collection state, not just shape**. The fold must accumulate them in its single
pass (LLVM's one-walk-collects-all). This is verifiable by a data-flow trace: none
of the four reads anything outside `GrammarIr` + `backend_shape` + the descriptor
map; therefore recomputable inside the fold, byte-identically.

## 3. Threading the `:204` `ty` — the consumption contract

`ExprKind::Project { body, .. }` at `passes/lib.rs:204` returns `self.infer_expr(
*body)` — the projection's declared `ty` (`ir/lib.rs:264`, `Option<String>`) is
dropped. It is dropped a *second* time structurally: it survives into
`BackendExpr::ValueProject{ty}` (`passes/lib.rs:1349`) and `SinkOnlyExpr::
ValueProject{ty}` (`sink_only.rs:228`) but no emitter reads it on the JSON path,
and on the general-descent path it is re-folded to a 4-way enum by
`classify_projection` (string-matching `"f64"/"Span"/…`, `general_descent/
view.rs:20-46`) — a re-derivation, not a consumption.

**Single-IR consumption contract.** Record the node's declared type once, into a
new attribute `proj_types: HashMap<ExprId, Option<String>>`, populated at
`infer_expr` when it reaches `Project` (`:204`) — the map already has a sibling,
`node_types: HashMap<ExprId, Type>` (`passes/lib.rs:93`), so the attribute plane
exists. Every downstream formulation then **reads `proj_types[expr]`** where it
today re-derives:
- the emit fold reads it when it reaches `ExprKind::Project`, instead of
  `classify_projection`;
- `derive_materialization_plan` (`:1139`) reads it to set `DirectBuildTarget.
  type_ref` (`ir/lib.rs:485`, today always `None` on the offset-tape path — R6 §5c);
- the greenfield driver seat `TypeAnalysis::from_ir` reads it.

**Born-RED edge (honesty).** `TypeAnalysis::from_ir` / `backend/driver/analysis.rs`
does **not exist on live** (`rg TypeAnalysis` → 0 hits; `find analysis.rs` → none).
It is the greenfield L1→L2 consumer named XL-F — the "driver seat" LOCK-3 owns as a
*fact contract*, not an engine (Boundaries §29). The dependency edge is therefore:
attribute `proj_types` (produced this wave) → consumer `TypeAnalysis::from_ir`
(same-wave greenfield deliverable, owner: the driver). The single-IR shape is
exactly this: the type is *one more attribute table* over the one IR, consumed
where materialization is decided — never a second lattice re-derived from bytes.

**Verify predicate (executed evidence to demand).** A per-pass `verify` over
`Project`@204: assert `proj_types.contains_key(&expr)` for every `Project` node.
RED on HEAD (map absent); GREEN once threaded; **zero emit footprint** — the six
regen outputs stay byte-identical with the predicate on/off (the type plane costs
zero, Constitution v).

## 4. `has_rule("value")` and `derive_materialization_roles` — RETIRED vs relocated

- **`has_rule("value")`** (`runtime_generator.rs:98`, name-gate selecting JSON
  templates vs general-descent) — **RETIRED**. Under the fold the emit family is
  selected by the per-`RuleId` `backend_shape` attribute (already computed:
  `OffsetTape`/`SinkOnly`/… `ir/lib.rs:361`), a fact not a name. `has_rule` def
  (`sink_only.rs:31`) deletes with L2.
- **`derive_materialization_roles`** (`passes/lib.rs:1403-1491`, the byte-literal
  role classifier: `b"{"`/`b"}"`→container, `b"true"`&`b"false"`→bool, `>=4`
  value-refs→`value`) — **retirement is CONDITIONAL**, and this is the family's
  sharpest counterexample. `json.bbnf` has **zero `->` projections** (R6 §0) — the
  JSON taxonomy is *recovered* by these heuristics, never declared. So retiring the
  classifier in favour of the `proj_types` attribute REQUIRES the grammar to carry
  the roles (JSON gains `->` annotations or a schema), else the role plane is merely
  *relocated* into a structural pass, not retired. TRUE retirement = the grammar is
  authoritative for materialization role. The prototype must decide: annotate
  `json.bbnf`, or accept relocation as the floor. (This interlocks LOCK-7 grammar
  totality: the classifier is a name-gate/heuristic the Constitution vi forbids;
  its removal is a grammar-totality obligation, not a pure IR refactor.)

## 5. Materialization key — per-`RuleId` vs per-`ExprId`; LOC; 5→2

Today the key is per-`RuleId` (`materialize_rule` `descriptors.get(&rule_id)`,
`:1367`; `backend_shape: HashMap<RuleId,_>`). **Counterexample: sheets.** The Google
Sheets grammar carries `->` at *multiple sub-expression positions within one rule*
(`grammar/google-sheets/google-sheets.bbnf:6,12,17-18,34-42,97`, R6 §0) — two
distinct materialization decisions inside one rule body defeat a per-`RuleId` key.
**Recommendation: key materialization per-`ExprId`** (`ExprId→descriptor`),
subsuming per-`RuleId`; the attribute plane is already `ExprId`-capable
(`node_types`, `:93`). The prototype must enumerate every fleet rule with >1
projection/materialization decision to confirm.

- **Net-LOC delta (deletions):** `BackendIr`+`BackendExpr`+the L1 sum
  (`ir/lib.rs:352-413`, ~60 LOC) · `single_plan`+`lower_expr`+`materialize_rule`
  (`passes/lib.rs:1283-1389`, ~110 LOC) · `SinkOnlyExpr`+L2 `lower_expr`
  (`sink_only.rs:69-234`, ~170 LOC). Against: one fold visitor + `proj_types`
  threading (~120 LOC). Net **≈ −220 LOC**, one sum-type instead of three.
- **Emission-extensibility 5→2:** `BackendShape` is 5 arms (`EagerTape/OffsetTape/
  EventTape/SinkOnly/CollapsedStage`, `ir/lib.rs:361`; `all_backend_shapes()`
  `cost.rs`). Under the attribute-fold the live fleet exercises two families —
  {tape-building, sink} — the shape becomes a 2-valued attribute × orthogonal
  policy facts, not a 5-way emitter fork. The 5→2 count is the extensibility win to
  measure.

## 6. One fold, six outputs byte-identical — the EXECUTED gate

The GO/NO-GO is `cargo xtask` regen×check across {json, css, sheets} + the typed
track — **byte-identical before/after** (regen-json/check-json,
regen-css/check-css-l4-declaration-values, regen-sheets/check-sheets,
regen-real-typed/check-real-typed; dispatch `xtask/src/main.rs:39-52`). A mapped
equivalence is not the evidence; the executed regen diff is.

**Differential runner (shared verifier question).** One `differential(edge, corpus)`
generalizes the extant digests: `direct_struct.rs:421/428` `track1_digest`/
`track2_digest` + `real_typed_struct::typed_checksum` (`:461-482`) +
`fnv_quarantine.rs:23-26` (track1/track2/serde/sonic typed checksums cross-checked
`:53`) + the CSS `CSS_CANON_PROFILE` XOR-fold (PROVE-12 §5). The runner (a) reproduces
all four digests from the single-fold output, and (b) localizes an INJECTED
mis-lowering to the edge it was injected on by re-running per-edge. Corpus coverage
metric: fraction of `ExprKind` × `BackendShape` product actually exercised;
zero-witness constructs (grammar arms no corpus reaches) reported **unverified**
(distinct from **sound**), never silently green.

## 7. CSS is a separate island (declare, don't re-author)

The CSS path does NOT flow through L2 at all: `render` dispatches on
`LoweredArtifact::Scan` (`runtime_generator.rs:57` `css_scan::derive_program` →
`css_scan_direct::render`, `:228`) — a fourth emitter with its own STOP-set scan
program. The single fold either (a) subsumes the scan-program derivation as one more
`backend_shape` value, or (b) CSS stays a named residual island (the grammar-derived
stop-set + `css_scan_direct` SIMD scan, already shipping — the staged-residual
dependency the directive says to declare, not re-author). Family 3B's minimal form
takes (b): the fold governs {json, sheets, typed}; CSS is a declared consumer of the
same attribute plane via a distinct terminal emitter. Collapsing CSS into the fold is
a stretch the prototype may test but must not assume.

## 8. Prior art (with the specific lesson)

- **GHC Core** — the strongest single-IR precedent: one small typed IR refined by
  Core-to-Core passes; annotations ride as `Tickish`/`Note`, not new IRs. Lesson:
  a whole optimizer over ONE sum type is production-proven.
- **LLVM** — mutable IR + `AnalysisManager` caching analyses keyed by IR value;
  transforms mutate in place. `LayoutFacts` IS an analysis cache keyed by `RuleId`/
  `ExprId`; the charter's "LLVM shape" is exact. Lesson: attributes-as-cache is the
  discipline; invalidation on mutation is the cost.
- **MLIR** — one IR, attributes/properties on ops, progressive in-place lowering;
  BUT multiple dialects coexist. Counter-evidence: some transpositions earn their
  keep when the target algebra genuinely differs. Here L1/L2 do NOT differ from L0
  (near-1:1, §1) — so they do not earn it.
- **rustc HIR→THIR→MIR** — three DISTINCT lowerings, each a real algebra change
  (desugaring, type-elaboration, CFG). Counter-evidence to over-collapsing: a tower
  is justified when levels change algebra. The 3B claim is falsifiable precisely
  here — measure whether `BackendExpr`/`SinkOnlyExpr` change algebra (they do not:
  `lower_expr` is 1:1) vs rustc's (they do). rustc's per-`DefId` query cache is
  again the attribute model.
- **egg/egglog e-class analysis** — monotone analyses attached to one e-graph; the
  attribute-over-one-IR shape, shared with LOCK-2 (which puts the analysis on an
  e-graph rather than the AST). Boundary: 3B annotates the AST; 2B/1D annotate an
  e-graph — same discipline, different substrate.

## 9. Named risks / counterexamples

1. **JSON has no `->`** (R6 §0) → retiring `derive_materialization_roles` needs a
   grammar change or degrades to relocation (§4). SHARPEST risk; interlocks LOCK-7.
2. **Per-`RuleId` insufficient for sheets** multi-projection rules → per-`ExprId`
   key (§5). Must be enumerated on the fleet, not assumed.
3. **L2 carries collection state**, not just shape (`SinkOnlyFacts`, §2) → the fold
   must accumulate `direct_shapes`/`literals`/`span_kinds`/`dispatch_alt_count` in
   one pass; data-flow trace required to prove recomputability.
4. **CSS bypasses the tower** (§7) → the fold's minimal form does not govern CSS;
   overclaiming a single fold over all four emitters is the failure mode.
5. **Attribute invalidation** — `normalize` (`passes/lib.rs:33`) mutates the grammar
   before facts are computed; if any pass mutates `GrammarIr` after attributes are
   built, the LLVM invalidation problem appears. Live currently builds facts once,
   post-normalize (`:35-57`) — the fold must preserve "annotate a frozen IR", not
   "mutate + re-annotate".

## 10. What a prototype must test (to move 3B)

- **P1 (GO/NO-GO):** implement `emit(&GrammarIr,&Attributes)` folding {json, sheets,
  typed}; EXECUTE regen×check — all outputs byte-identical (§6). Mapped ≠ executed.
- **P2 (type thread):** add `proj_types`; wire the `verify`@204 predicate; show
  RED→GREEN + zero emit footprint (six regen byte-identical, predicate on/off).
- **P3 (key):** enumerate fleet rules with >1 materialization decision → per-`RuleId`
  vs per-`ExprId` verdict; report net-LOC delta and the 5→2 extensibility count.
- **P4 (differential):** the `differential(edge,corpus)` runner reproduces
  track1/track2 + CSS-XOR digests AND localizes one injected mis-lowering to its edge;
  report coverage + zero-witness (unverified vs sound).
- **P5 (retirement proof):** show `has_rule("value")`@98 and
  `derive_materialization_roles`@1403 GONE (deleted, not moved) — OR name the
  grammar-totality obligation that blocks it (JSON `->` annotations), carried as a
  born-RED edge to LOCK-7.
