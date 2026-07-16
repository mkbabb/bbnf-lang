# LOCK-3 · Family 3B — SINGLE-IR-MULTIPASS (pass-2 research)

One rich IR, refined in place by annotating passes; `BackendShape` and the materialization
plan become *attributes hung on that one IR*, not fresh tiers; the emitter is one fold that
*reads* those attributes. The LLVM shape. This report develops that formulation alone,
concrete, against the sole living tree.

**Live pin.** `skinny@061c79fa7` (`master` HEAD at read time; the directive's `b3cb8c16b` is
an ancestor in the same lineage — every line number below is re-verified against `061c79fa7`,
the checked-out working tree). Outer-repo witness code is `bbnf-lang@061c79fa7` `crates/core`.
All figures herein are structural/LOC facts read off live; the one dynamic delta
(residualization) is PROVE-12's measured result, cited, not re-run.

---

## 0. The mechanism made concrete

Today skinny runs *three* transpositions with data invented at each seam:

1. `GrammarIr` (`ir/src/lib.rs:34-39`: `name, source_hash, rules, exprs` — **no attribute
   slot of any kind**) → `BackendIr` via `lower_expr` (`passes/src/lib.rs:1318-1360`) +
   `materialize_rule` (`:1362-1389`).
2. A parallel, name-gated *plan* — `derive_materialization_roles` (`:1403-1491`) heuristically
   re-invents the JSON value taxonomy from byte-literals/regex-kinds; `derive_materialization_plan`
   (`:1139`) turns roles into `MaterializationDescriptor{kind,label,shape,fields}`
   (`:1132-1137`), keyed `HashMap<RuleId, …>` (`:1128`).
3. Emission — `runtime_generator::render` matches `LoweredArtifact` (`runtime_generator.rs:76-88`),
   then `emit_sink_only` splits JSON-templates vs general-descent on the **name gate**
   `sink_only.has_rule("value")` (`runtime_generator.rs:98`; def `lower/sink_only.rs:31-33`).

3B collapses (2) into (1) and turns (3) into one attribute-reading fold. The single IR is
`GrammarIr` extended with **attribute side-tables keyed by `RuleId`/`ExprId`** — exactly the
shape the *outer* mature tree already runs (§2). Passes *annotate* (write the tables); the
emitter *folds* (reads the tables). No second node type is minted for backend facts; no
heuristic re-derivation; no name gate.

---

## 1. The single fold: `emit(&GrammarIr, &Attributes)` — byte-identical over the six?

**Where the fold already half-exists.** `render` (`runtime_generator.rs:76-88`) is *already*
one grammar-agnostic entry that matches a computed `LoweredArtifact` variant and reads its
output-shape *from the artifact* ("reads NO `target.*`/`contract.*` selection field — the
firewall", `:69-75`). And `lower_for_runtime` (`:40-73`) *already* selects Scan(CSS) vs SinkOnly
**purely by shape** — the CSS harvest `lower::css_scan::derive_program` succeeds ⇒ `Scan`, else
`SinkOnly` (`:57-66`; "no grammar NAME, no config field", `:50-51`). Two of the three dispatch
decisions are already fold-clean and attribute-derived.

**The one surviving fork inside the fold** is `emit_sink_only`'s `has_rule("value")` (`:98`) →
static `include_str!("json_templates/*")` splice (`:101-133`) vs `emit_general_descent` (`:147`).
Under 3B this becomes: the fold reads a per-`RuleId` materialization descriptor and emits
tape-op + build-op from it uniformly. JSON's lazy `value/view/visitor` surfaces (the 3 templated
files) are *not* recomputed by the fold — they are the **fixed runtime library** the fold's tape
references, so they stay `include_str!` verbatim and remain byte-identical by construction. What
the fold *generates* (the recursive-descent `parse_*_direct` in `json_sink_direct::render`; the
general-descent `emit`/`view`) becomes one walk over `BackendExpr` reading
`TapeEmit{kind}`/`DirectBuild{…}` nodes that `materialize_rule` already splices.

**Byte-identity claim, honestly bounded.** The six outputs are regen/check × {json, css, sheets}.
CSS routes through `Scan`, untouched (byte-identical trivially). JSON and Sheets both lower to
`SinkOnly` (`:59-66`); the only risk to byte-identity is the *order* of the spliced
`Seq[SpanMark,body,SpanMark,TapeEmit,DirectBuild,Return]` (`:1370-1388`) and the `shape`/`label`
strings — all a function of the descriptor (§3). A fold reading the same descriptor emits the
same bytes. **This is the prototype's first gate** (§8): regen under the fold, `git diff` the
three generated dirs → empty. Mapped ≠ executed; the claim is falsifiable and cheap.

---

## 2. The substrate — one IR + attribute tables (the outer tree is the existence proof)

3B's substrate is not speculative: the *mature* bbnf-lang core already runs it. `TypeAnalysis`
(`crates/core/src/backend/driver/analysis.rs:16-19`) is
`{rule_types: Vec<(RuleId, TypeDesc)>, scratch_types: Vec<TypeDesc>}`, and `from_ir` (`:22-31`)
is a **pure clone of `ir.types` + `ir.type_map.scratch_types()`** — zero re-derivation. That
`GrammarIR` carries `types` and `type_map` as **annotations hung on the one IR**;
`BackendPreparation` (`:52-69`) likewise hangs `alt_strategies`, `delim_scan_configs`,
`key_dispatch_configs` as `HashMap<NodeId, …>` side-tables "cloned directly from `ir.*`
(populated upstream by `bbnf_ir::passes::recognizers::*::collect`)" (`:59-68`). That is the
LLVM shape verbatim: one IR, many keyed attribute tables, consumers that *read* them.

Skinny lacks this: `GrammarIr` (`ir/src/lib.rs:34-39`) has no table, and there is no `TypeDesc`
in skinny at all (R6 §5). 3B's substrate move is therefore precise and small: extend `GrammarIr`
(or a sibling `Attributes` bundle threaded beside it) with `materialization: HashMap<RuleId,
MaterializationDescriptor>` and `node_types: HashMap<ExprId, TypeDesc>` — reusing the outer
tree's proven layout. `MaterializationPlan` (`passes/src/lib.rs:1127-1130`) is *already* exactly
this table; today it is a transient argument threaded into `materialize_rule` rather than a
durable IR annotation. The 3B change is to promote it from a threaded transient to an IR
attribute and delete the second tier.

---

## 3. `materialize_rule`'s emit-order: pure derivation, recomputable inside the fold

**Verdict: pure derivation; carries no datum absent from the descriptor.** `materialize_rule`
(`passes/src/lib.rs:1362-1389`) is `descriptors.get(&rule_id)` (`:1367`) → a fixed splice
`Seq[SpanMark(Start,label), body, SpanMark(End,label), TapeEmit{kind}, DirectBuild{shape,
fields}, Return]` (`:1370-1388`). Every field — `label`, `kind`, `shape`, `fields` — comes from
the `MaterializationDescriptor` (`:1132-1137`); the *order* is a constant template. There is
**no input** to `materialize_rule` beyond `(rule_id, body, plan)`. Data-flow trace: `plan` ←
`derive_materialization_plan(&normalized)` (`:37`) ← `derive_materialization_roles(grammar)`
(`:1403`) ← the `GrammarIr` alone. The whole splice is thus a function of `GrammarIr`, and a
fold carrying the descriptor table recomputes the identical `BackendExpr` in place. The
emit-order framing is not a tier; it is a `match` arm.

Corollary for §1: because the splice is deterministic in the descriptor, the only way the fold
diverges is if the *descriptor* differs — reducing byte-identity to "does the fold build the
same descriptor table?" (§4).

---

## 4. Retirement (not relocation) of the name-gate and the heuristic classifier

The directive's hard test: are `has_rule("value")` (`runtime_generator.rs:98`) and
`derive_materialization_roles` (`passes/src/lib.rs:1403`) **retired**, or merely moved inside
the fold?

- **`has_rule("value")` — retired.** Under 3B the JSON-vs-general split is not a rule-name test;
  it is "does this `RuleId` carry a materialization descriptor rooted at `TapeKind::Container`?"
  — a *shape* fact `derive_materialization_plan` already computes. The gate disappears because
  the fold branches on the descriptor's `kind` (`ir/src/lib.rs:453`, `TapeKind`), never on the
  string `"value"`. This extends the already-clean `lower_for_runtime` Scan/SinkOnly firewall
  (`:50-51`) one level down.

- **`derive_materialization_roles` — retired *iff* the descriptor is sourced from the grammar's
  own `->`, not re-invented.** This is where 3B *consumes the threaded `:204 ty`* rather than
  re-deriving. Today roles are byte-literal heuristics (`{`+`}`→container `:1433-1440`; regex
  `SpanKind::String`→string `:1404-1408`; `≥4 value-refs`→`value` `:1454-1465`). They exist
  *only because* json.bbnf carries zero `->` (R6 §0) and the type never reaches emission. If the
  descriptor's `shape`/`fields`/`kind` are instead read from the projection annotations the
  grammar declares — `ExprKind::Project{target, ty}` (`ir/src/lib.rs:261-265`) — the classifier
  has nothing left to compute and is deleted, not relocated. **Honest caveat:** json.bbnf has no
  `->`, so *for JSON specifically* retirement needs either (a) Sheets/typed grammars as the
  driver of the descriptor-from-`->` path while JSON keeps a minimal structural recovery, or (b)
  json.bbnf gaining projections (a grammar edit — a LOCK-7 grammar-totality dependency). 3B's
  disposition: retire the classifier on any grammar that declares its shapes; carry JSON's
  structural recovery as a **born-RED edge owned by LOCK-7** until json.bbnf projects.
  Relocation-not-retirement is the failure mode this names and refuses.

---

## 5. Per-`RuleId` vs per-`ExprId`; LOC delta; the 5→2 count

**Key granularity.** The live materialization table is *already* per-`RuleId` (`HashMap<RuleId,
MaterializationDescriptor>`, `:1128`; lookup `:1367`) and it suffices for JSON and Sheets: each
value taxon is a distinct *rule* (`string`, `number`, `object`, `array`, `pair`, `value`). The
counterexample forcing per-`ExprId` is a **single rule whose body carries two differently-typed
projection sites inline**. Sheets projects at rule granularity (R6 §0), so per-`RuleId` holds for
the current fleet. **But the *type* attribute must be per-`ExprId`**: `ExprKind::Project` is an
expression node and `classify_projection` folds per projection site (general_descent, R6 §5b).
So 3B's substrate is **materialization per-`RuleId`, node-types per-`ExprId`** — matching the
outer tree's own split (`rule_types: Vec<(RuleId,TypeDesc)>` *and* `scratch_types: Vec<TypeDesc>`,
`analysis.rs:17-18`). A single key would be wrong for one of the two facts.

**LOC delta (structural estimate; prototype confirms via `lint-loc`).** Deletions: the
`derive_materialization_roles` heuristic (`:1403-1491`, ~88 LOC) and the `shape_for` fallback
ladder (`:270-283`, ~14 LOC) once shapes come from annotations. Additions: an `Attributes`
struct + two `HashMap` fields on/beside `GrammarIr` (~15 LOC) and one annotate-pass writing `ty`
(§6, ~20 LOC). Net is **negative** on the classifier axis, small-positive on substrate —
consistent with extreme-parsimony.

**Extensibility 5→2.** `BackendShape` has five variants (`ir/src/lib.rs:361-367`:
EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage) and emission has three generators (Scan /
json-templates / general-descent); adding a family today can touch all of them. Under one fold a
new family adds **two** things — a `TapeKind` (or reuses one) and a `DirectBuild` field-source —
both *data* in the descriptor table, read by the existing fold, zero new emit surface. Prototype
measures it by adding CSV (grammar exists, `grammar/misc/csv.bbnf`, no generated output — R1 §6)
and counting touched emit sites: target ≤2.

---

## 6. Threading `:204 ty` — the consumption contract (not re-derivation)

The shared question, answered *for this formulation*. `infer_expr`'s `ExprKind::Project{body,..}
=> self.infer_expr(*body)?` (`passes/src/lib.rs:204`) discards `ty` from the type lattice (`:203`
`Annotation`→`Unit` is legitimate, untouched). 3B does **not** re-derive the projection type
downstream; it threads *one* node-type from here to a single consumer.

**End-to-end thread on live:**
1. At infer, resolve `ExprKind::Project.ty: Option<String>` (`ir/src/lib.rs:264`) into a
   `TypeDesc` and **write it into the per-`ExprId` `node_types` table** (the new attribute, §2) —
   the pass *annotates*; it does not return a richer `Type`.
2. `lower_expr` already carries `ty` verbatim into `BackendExpr::ValueProject{body,target,ty}`
   (`passes/src/lib.rs:1349-1353`) — the string survives to the backend today; 3B replaces the
   string with a table lookup so the *resolved* type, not the raw lexeme, flows.
3. The single consumer is the descriptor builder: `DirectBuildField.target` (`ir/src/lib.rs:466`;
   `DirectBuildSource` `:474`) — **currently `None` on the offset path for every field** (R6 §5c)
   — is populated by *reading* `node_types[expr_id]`. The fold then emits the typed field with no
   second classification.

The consumption contract is exactly the outer tree's: `TypeAnalysis::from_ir` clones `ir.types`
(`analysis.rs:24`). Skinny gains the same one-line consumer — `node_types.get(&expr_id)` — and
`classify_projection`'s 4-way fold (R6 §5b) is *the thing retired*, because the fold now reads a
resolved `TypeDesc` instead of re-string-matching `"f64"/"Span"/…`. A node with no threaded type
reports **unverified** (zero-witness), never silently defaulting (`no-silent-epsilon`). Evidence
the prototype must produce: the RED→GREEN of a `verify` predicate over `Project`@204 (RED on HEAD
— `node_types` empty; GREEN after the annotate-pass), with regen byte-identical when the
predicate toggles (zero emit footprint — the type plane costs zero at runtime, Constitution v).

---

## 7. Prior art (citations)

- **LLVM** — one `Module`/`Function` IR mutated in place by passes; analyses (`DominatorTree`,
  `LoopInfo`, `AAResults`) are *attributes* cached on the IR via `AnalysisManager`, invalidated
  by a `PreservedAnalyses` set. 3B's exact shape; the outer tree's `BackendPreparation`
  side-tables (`analysis.rs:52-69`) are a direct transcription.
- **MLIR** — operations carry a discardable **attribute dictionary**; passes attach/read named
  attributes on the *same* op rather than lowering to a new type. Descriptor-as-attribute on
  `RuleId` is MLIR's discardable-attribute pattern.
- **rustc** — `TyCtxt` query system: `ty::TypeckResults` is a per-body table consumers *query*,
  never re-derive; `node_types: HashMap<ExprId, TypeDesc>` (§6) is `TypeckResults.node_types` in
  miniature.
- **GHC Core** — a single small typed IR (`CoreExpr`) refined by many passes in place; the
  "fewest transpositions that suffice" doctrine — argues against a per-level tier tower (3A) and
  for one annotated IR.
- **egg/egglog** — `Analysis::Data` attached per e-class, maintained by passes: the
  annotate-in-place discipline. (Model only; 3B needs no e-graph — that is LOCK-2's engine, kept
  distinct per the boundary clause.)

---

## 8. Risks, counterexamples, and exactly what the prototype must test

**Risks / counterexamples.**
- **json.bbnf has no `->`** (R6 §0): full retirement of `derive_materialization_roles` cannot be
  shown on JSON alone; retirement is *conditional* on grammars declaring shapes (LOCK-7
  dependency). If the prototype cannot source JSON's descriptor from annotations without editing
  json.bbnf, 3B carries that as a named born-RED edge, not a retirement claim. **Sharpest
  over-claim guard.**
- **Per-`ExprId` inline mixed projections** would break the per-`RuleId` materialization key; the
  current fleet doesn't exercise it, but CSV/typed grammars might — test before asserting
  per-`RuleId` sufficiency.
- **Attribute invalidation** (LLVM's hardest problem): skinny's linear pipeline (`compile`,
  `passes/src/lib.rs:37`) makes invalidation trivial *today*, but promoting `MaterializationPlan`
  to a durable table introduces a write-after-normalize ordering constraint
  (`derive_materialization_plan(&normalized)`, `:37`): the annotate-pass must run after
  normalization or it emits wrong shapes.
- **Byte-identity of `include_str!` surfaces**: the 3 JSON template files must stay verbatim
  (runtime library, not fold output). If the fold over-eagerly regenerates `view.rs`/`visitor.rs`,
  parity breaks. The fold's boundary is exactly the recursive descent, not the lazy view.

**What the prototype must test (ranked).**
1. **Fold byte-identity gate.** Replace `materialize_rule` + `emit_sink_only`'s name-gate with
   one attribute-reading fold; `cargo xtask regen-{json,css,sheets}` then `git diff` the three
   generated dirs → **empty**. Load-bearing, cheap, falsifiable (§1, §3).
2. **`verify(Project@204)` RED→GREEN** with zero emit footprint: predicate reads
   `node_types[expr_id]`, RED on HEAD, GREEN after the annotate-pass; regen byte-identical with
   predicate on/off (§6). Proves the type thread is real and runtime-free.
3. **Classifier retirement delta**: delete `derive_materialization_roles` (`:1403-1491`) on a
   grammar that declares its shapes (Sheets, or json.bbnf+`->`); descriptor sourced from
   `node_types`; existing XOR-checksum + `json_parity` digests unchanged. Reports
   retired-vs-relocated honestly (§4).
4. **5→2 extensibility count**: add CSV (`grammar/misc/csv.bbnf`), count touched emit sites;
   target ≤2 (one `TapeKind`, one `DirectBuild` source), no new `BackendShape` arm (§5).
5. **`lint-loc` net delta**: confirm the classifier deletion outweighs the substrate addition.

**Dependency declarations (do not re-author).** The grammar-derived STOP-set residualization
already ships in the CSS emit spine — `find_component_delim` const-generic `u128` mask, +18
generated LOC, byte-identical, measured 0.81–0.98× cyc/B (PROVE-12 REPORT §2-4); the single fold
*consumes* that residual pattern, it does not re-invent it. The `ScanAlphabet`-derived SIMD scan
(`css_scan_direct`) is likewise a declared dependency, not a 3B deliverable. The driver's *seat*
(where the fact contract sits at the L1→L2 edge) is LOCK-3's; the *engine* that fills the
descriptor from a cost model is LOCK-2's — 3B reads the descriptor, it does not choose it.
