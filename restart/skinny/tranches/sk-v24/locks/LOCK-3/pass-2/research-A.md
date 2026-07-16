# LOCK-3 · Family 3A — CONTRACT-TOWER (pass-2 research)

Re-pinned to live `skinny@7790134675c6` (branch master; skinny is its own repo — R6's
`7f3be27c2` and the directive's `b3cb8c16b` are stale ancestors, and `passes/src/lib.rs`
is now 2096 LOC, not 2750). Every `path:line` below opened on this HEAD. Paths absolute
under `/Users/mkbabb/Programming/bbnf-lang/`.

The family thesis: a **small fixed three-level tower**, each edge a **total function**
carrying a **checkable per-level contract** (parity / round-trip oracle), CI-enforced. The
family's product is NOT new IR levels — it is a verifier lattice welded onto the spine
edges that already exist, plus the **one typed table** the L1→L2 contract obliges the
emitter to CONSUME instead of re-derive.

---

## 1. The enumerated edge-DAG of the live spine (directive Q3)

Five labelled transposition edges carry the value plane:

- **E0** `.bbnf` → `GrammarIr`: `grammar::parse_grammar`. AST-IR; `ExprKind` 10-arm; rules
  index `ExprId` (`skinny/crates/ir/src/lib.rs`, `ExprKind::Project { body, target, ty }`).
- **E1 ANALYSIS** `GrammarIr` → typed facts + layout: `passes::compile`
  (`skinny/crates/passes/src/lib.rs:32`). Produces `type_facts.node_types:
  HashMap<ExprId,Type>` (`:93`, written `:212`) and `layout_facts` (backend_shape / cost /
  CSP / egraph). **The `->` `ty` is dropped here at `:204`.**
- **E2 MATERIALIZE** `GrammarIr` → `BackendExpr` (13-arm) = `backend_ir`:
  `passes::extract::single_plan`. `lower_expr` maps `ExprKind::Project → BackendExpr::
  ValueProject { body, target, ty }` at `:1349` (the *raw* `Option<String>` `ty` survives,
  never the inferred type); `materialize_rule` (`:1362`) splices the value-node quintet.
- **E3 SINK** `BackendExpr` → `SinkOnlyProgram`/`SinkOnlyExpr`:
  `codegen::lower::lower_to_rust` (`skinny/crates/codegen/src/lower/sink_only.rs:20,69`).
- **E4 EMIT** `SinkOnlyProgram` → Rust source: `runtime_generator::emit_sink_only`
  (`skinny/crates/codegen/src/runtime_generator.rs:89`); five emitter faces (JSON
  templates / `json_sink_direct` / `general_descent` / `css_scan_direct` /
  `json_typed_direct`).

**How many distinct value levels actually exist — reconciling the framing.**
`materialize_rule` (`passes/src/lib.rs:1362-1391`) emits, per materialized rule, ONE
`BackendExpr::Seq` of six nodes that braid **three** value-abstraction strata:

1. `SpanMark{Start,label}` — **SPAN stratum**: byte-offset open event, no type.
2. `body` — the recognizer subtree.
3. `SpanMark{End,label}` — SPAN stratum close event.
4. `TapeEmit{kind: TapeKind}` — **TAPE stratum**: the coarse structural discriminator (9
   `TapeKind` tags: Container/Sequence/KeyValuePair/StringValue/NumberValue/BoolValue/
   NullValue/Member/Element). This is the level that actually reaches the offset-tape.
5. `DirectBuild{shape, fields: Vec<DirectBuildField>}` — **STRUCT stratum**: the rich
   per-field typed schema (the `DirectBuildTarget` lattice in `ir/src/lib.rs`). Populated
   ONLY on the typed path; on the offset-tape path every field's `target` is `None`
   (R6 §5c — verified `derive_materialization_plan` builds `target: None` throughout).
6. `Return` — control marker.

So the answer is **three braided materialization strata (span / tape / struct)** over one
`BackendExpr`, exposed on the offset path as span+tape (struct present-but-empty), on the
typed path as all three. `SinkOnlyProgram` is the E3 flattening of that `BackendExpr` into
emit-order form the generator walks. **The "small fixed tower (three levels)" is not an
invention — it is this latent stratification, made into total functions with contracts.**
The stratum primitives already exist as walk helpers (`contains_direct_build` `:973`,
`contains_tape_kind` `:993`, `alt_branch_count` `:1015`) — the contract layer reuses them.

---

## 2. The three-level contract tower (mechanism made concrete)

Each level Lₖ is a total function already on the spine; the family ADDS a checkable
contract Cₖ (an oracle predicate) and CI-enforces it.

- **L1 = ANALYSIS** `GrammarIr → TypedIr`. Total fn: `infer` (`passes:187`). **C1**: every
  `ExprId` reachable from an entry rule has a `facts.node_types` entry, and every
  `Project` ExprId's node-type is the RESOLVED projected type, not the bare body type
  (no-value-discard, no-silent-epsilon). Oracle = §4's `verify_project_types`.
- **L2 = MATERIALIZE** `TypedIr → BackendExpr`. Total fn: `lower_expr` + `materialize_rule`
  (`:1349,:1362`). **C2** (stratum consistency + CONSUME): every `DirectBuild.shape` has a
  sibling `TapeEmit.kind` in its `Seq`, and `DirectBuild.fields[*].target == Some(t)` **iff**
  L1 recorded node-type `t` for that ExprId — i.e. the struct stratum is READ from L1, never
  re-derived by `derive_materialization_roles` (`:1403`, the byte-literal classifier). C2 is
  a checkable equality: re-derivation is a RED (see §3).
- **L3 = EMIT** `BackendExpr → SinkOnlyProgram → Rust`. Total fn: `lower_to_rust` +
  `emit_sink_only`. **C3** (parity): the emitted parser's tape columns are byte-identical to
  the independent track2 oracle and canonical-string equal. This oracle EXISTS and CI already
  runs it: `parity::assert_parity` (`skinny/crates/bbnf-bench/src/parity.rs:23`).

The tower is FIXED at three levels, each lowering total, each contract an executable
predicate run at regen. This IS the 3A charter, expressed over the live spine rather than a
greenfield rewrite.

---

## 3. Threading the `:204` `ty` — the CONSUME contract (Shared Q)

**Current state (live).** Infer at `:204` does `ExprKind::Project { body, .. } =>
self.infer_expr(*body)?` — drops `ty`; `node_types[project_expr]` = the body's structural
`Type`. Lower at `:1349` DOES carry the raw `ty: Option<String>` into
`BackendExpr::ValueProject`, but that is the syntactic annotation string, never a
resolved/composed type, and on the JSON path nothing reads it (`json_sink_direct/
first_set.rs` recurses body). The struct taxonomy is instead RE-DERIVED by
`derive_materialization_roles` (`:1403`) — a byte-literal heuristic (`{`/`}`→container,
regex `SpanKind::String`→string, `true`/`false`→bool, ≥4 value-refs→`value`) that never
consults `node_types`. That re-derivation is exactly what the CONSUME contract retires.

**The thread, end-to-end, minimal, on live:**
1. `:204` — compose instead of drop: resolve `ty` to a `Type` (e.g. `Type::Named(name)` /
   projected) and insert THAT as `node_types[project_expr]`. One resolved node-type per
   `Project` ExprId. Totality: unknown `ty` name → `PassError`, never silent Epsilon.
2. Carry `type_facts.node_types` forward. Today `compile` (`:32`) hands codegen only
   `backend_ir` + `layout_facts`; `node_types` is consumed by `layout::run`/`extract` and
   dropped (**verified**: no `node_types` reference anywhere in `crates/codegen/src`). Add a
   projected `Vec<(RuleId, ResolvedType)>` to the facts codegen receives — the exact shape
   the legacy driver `TypeAnalysis::from_ir` reads (`crates/core/src/backend/driver/
   analysis.rs:22`: `rule_types: ir.types.clone()` — it READS a pre-populated table, never
   re-derives; that is the consume-pattern to mirror).
3. `derive_materialization_plan` (`:1136`) populates `DirectBuildField.target` (today always
   `None`) by READING `node_types[field_expr]`. The byte classifier is retired for the typed
   path (it may survive ONLY as the JSON structural-recovery fallback, since `json.bbnf` has
   zero `->` — R6 §0).
4. `emit_typed_from_source` (`codegen/src/lib.rs:130`) then no longer needs a hand-authored
   `DirectSchemaSet`: a `derive_direct_schema` reads `node_types → DirectSchemaSet`, and the
   already-full-fidelity `json_typed_direct::render` consumes it (R6 §8b).

**How the CONTRACT-TOWER CONSUMES one threaded node-type (not re-derives).** C2's oracle
asserts, per `Project` ExprId, `DirectBuild.field.target == node_types[expr]`. If the
emitter ever re-derived via the byte classifier, the two diverge on any grammar whose `->`
disagrees with the byte heuristic — the oracle flags RED. The contract therefore *forces*
consumption: re-derivation is definitionally a contract violation, not a stylistic choice.
This is the family's answer to "consume vs independently re-derive" — the equality IS the
edge.

---

## 4. The `verify` predicate over `Project@204` (directive Q1)

`verify_project_types(grammar, facts) -> Result<(), Vec<ExprId>>`: for each ExprId `e` with
`grammar.expr(e).kind == ExprKind::Project{ ty: Some(name), .. }`, assert `facts.node_types[e]`
is the resolved projected type (`Type::Named(name)`-equivalent), NOT identical to
`facts.node_types[body]`.

- **RED on HEAD**: for every projected ExprId, `:204` makes `node_types[project] ==
  node_types[body]`, so the assertion fails. Executable witness = the Sheets grammar, the
  only live grammar exercising `->` (`grammar/google-sheets/google-sheets.bbnf:6,12,17-18,
  34-42,97`). (JSON exercises none — R6 §0.)
- **GREEN once §3.1 threads `ty`**: the projected node-type differs from the body type.
- **Zero emit footprint**: the predicate is read-only over `facts` and emits nothing. Proof
  = the regen content-hash: `xtask` `check_target` (`skinny/xtask/src/regen.rs:37`) hashes
  rel-path + bytes (`:177-184`); `generated.rs` digest is byte-identical with the predicate
  compiled IN vs OUT. (Caveat, stated: threading `ty` in §3.1 *does* change typed-direct
  output where the byte-heuristic and the true `->` disagree; the **predicate alone** does
  not — that is what the zero-footprint claim covers.)

---

## 5. The `differential(edge, corpus)` runner (directive Q2)

Three digest families exist and must fold into one runner:
- **Structural (JSON)** `parity::assert_parity` (`parity.rs:23`): compares track1 tape
  columns `{offsets, flag_cursors, flag_values}` (`:27-33`), asserts zero payload
  writes/allocs (`:34-49`, the lazy invariant), and `to_canonical_string()` equality
  (`:52-58`). track1 = generated tape path; **track2 = an independent hand-written parser**
  (`bbnf-bench/src/track2/json.rs:12`) — a genuine two-implementation differential (McKeeman
  1998; Csmith), not a self-check.
- **Value-plane (CSS)** the `CSS_CANON_PROFILE` XOR-fold checksum (PROVE-12 §5): XOR every
  parse result across corpora → one integer; byte-identical output ⇒ identical checksum.
- **SIMD (scalar↔NEON)** `gate.rs` `parity_ok` / `simd_parity_ok` /
  `scalar_parity_hash_twitter` (`gate.rs:34-35,201,414`) — the checkasm-style kernel
  parity, gated before throughput (`classify` `:214`).

**One runner** `differential(edge: EdgeId, corpus) -> DiffReport`, `EdgeId ∈ {E0..E4}` of
§1. (a) It reproduces the existing digests by selecting the oracle apt to the edge —
tape-column eq + canonical-string at E4, XOR-fold for CSS, node-type coverage at E1, stratum
consistency at E2. (b) It localizes an INJECTED mis-lowering: inject a fault at a chosen edge
(swap a `TapeKind` at E2, drop a `SpanMark`, mis-populate a `target`), run the fleet, and
report the LOWEST edge whose contract Cₖ breaks — that is the localization. Because each edge
carries its own oracle, a single fault lights the specific edge, not a global "output wrong".

**Corpus-coverage metric** = per-edge CONSTRUCT COVERAGE: `covered_arms / total_arms` of the
edge's sum type (`ExprKind` 10-arm at E0/E1, `BackendExpr` 13-arm at E2, `TapeKind` 9-tag at
E2's tape stratum, emitter faces at E4), witnessed by the fleet `{json, css_l4, sheets}`. An
arm with zero witnesses is reported **UNVERIFIED (sound-unknown)** — distinct from
VERIFIED-SOUND — and NAMED (e.g. `TapeKind::Member`/`Element` if no fleet grammar emits
them). Zero-witness constructs are never silently passed; the coverage bound is disclosed.

**Injected-fault evidence to run**: mutate `materialize_rule` to emit `TapeKind::Element`
where `Sequence` is due → `assert_parity` RED on JSON at E4, C2 stratum-consistency RED at E2
(`TapeEmit.kind` ≠ `DirectBuild.shape`), C1 GREEN → localizes to **E2**.

---

## 6. Prior art (citations)

- **rustc MIR `Validator`** (`rustc_const_eval::transform::validate`) runs after *every* MIR
  pass under `-Zvalidate-mir`; `-Zverify-llvm-ir` verifies each stage. The literal "each
  lowering total + a checkable contract, CI-enforced" precedent — L1/L2/L3 = validator-per-
  pass.
- **LLVM `-verify-each` / the `Verifier` pass**: module verifier after each pass; `lit` +
  FileCheck = the corpus-runner analogue.
- **MLIR dialect conversion** (`ConversionTarget` + `TypeConverter`, `-verify-diagnostics`):
  each transposition edge is a **total legalization** that is legal-or-error; the type
  *materialization hooks* are the exact analogue of `DirectBuild.target` consumption. The
  strongest structural match to the CONTRACT-TOWER thesis.
- **Translation validation / CompCert** (Pnueli–Siegel–Singerman 1998; Necula 2000): validate
  each *run's* output against its input by a checker rather than proving the compiler.
  `differential(edge, corpus)` is translation validation localized per edge.
- **Differential testing**: McKeeman 1998; Csmith (Yang et al. 2011) — the track1/track2 two-
  implementation differential.

---

## 7. Named risks / counterexamples

- **R1 — JSON has no `->`.** `json.bbnf` contains zero projections (R6 §0); the whole `ty`
  thread + C1 RED→GREEN is exercised ONLY by Sheets on live. The certified JSON hot lane sees
  NO change from §3. The family must NOT claim JSON perf from type-threading — the JSON
  contribution is C3 (parity, already CI) and retiring the `has_rule("value")` name-gate
  (`runtime_generator.rs:99`).
- **R2 — three-level tower must not re-introduce IR levels** (no-ts-ir; single-IR bias). The
  three levels ARE the already-braided span/tape/struct strata inside `materialize_rule`; the
  tower adds *contracts*, not *types*. If a downstream reads "three IR structs," that is the
  failure mode. It is three CONTRACTS over one `BackendExpr`.
- **R3 — vacuous CONSUME.** If `derive_materialization_roles` is kept as a "fallback" the
  CONSUME contract passes trivially (relocation, not retirement). C2 must make the node-type
  the SOLE producer on the typed path; the byte classifier is deleted there (survives only as
  the JSON no-`->` structural-recovery fallback).
- **R4 — L1 totality under recursion.** A projected type may reference a rule that itself
  projects (`a -> b`, `b -> c`); node-types must compose to a fixpoint, never silent Epsilon.
  Guard with a transitive-resolution test on a synthetic Sheets-style chain.
- **R5 — zero-witness `TapeKind`s** (Member/Element/KeyValuePair may be unreachable on the
  fleet) → C2 coverage < 100%; reported UNVERIFIED. A disclosed bound, not a defect.

---

## 8. What a prototype must test to move this family

Single minimal prototype, on live:
1. Thread `:204` `ty` into `node_types` (compose `Type::Named`); carry `node_types` into
   `PipelineOutput` → codegen; populate `DirectBuildField.target` from `node_types` at
   `derive_materialization_plan` (`:1136`).
2. Wire `verify_project_types` as a regen-time predicate; capture RED (HEAD) → GREEN
   (post-thread) on the Sheets grammar — EXECUTED, not mapped.
3. Prove zero emit footprint of the PREDICATE ALONE: `regen.rs` content hash byte-identical
   with predicate on/off.
4. Build `differential(edge, corpus)`: reproduce `assert_parity`'s tape-column digest AND
   PROVE-12's XOR-fold; inject one mis-lowered `TapeKind` and show localization to E2 (C1
   green / C2 red / C3 red).
5. Report per-edge construct coverage over `{json, css_l4, sheets}`; NAME every zero-witness
   arm.

**GO gate**: RED→GREEN executed; predicate-only regen digest byte-identical; injected fault
localized to its edge; coverage table with named unverified arms. NO perf claim is owed (the
family is correctness infrastructure) — but the schema-derived typed-direct output must remain
parity-equal to the hand-authored `DirectSchemaSet` baseline (`xtask/src/real_typed_schema.rs`)
by regen byte-diff, closing the loop that R6 §8b left open.

### Integration seams (path:line, live)
- Infer edge: `skinny/crates/passes/src/lib.rs:204` (drop), `:212` (insert), `:93`
  (`node_types` field).
- Lower/materialize: `:1349` (Project→ValueProject, raw `ty`), `:1362` (`materialize_rule`),
  `:1403` (`derive_materialization_roles` — retire on typed path), `:1136`
  (`derive_materialization_plan` — populate `target`).
- Pipeline export → codegen: `passes/src/lib.rs:32` (`compile`); `codegen/src/lib.rs:101,
  106-109` (entry, facts consumed).
- Sink/emit: `codegen/src/lower/sink_only.rs:20,69`; `codegen/src/runtime_generator.rs:89`.
- Oracles: `bbnf-bench/src/parity.rs:23`; `bbnf-bench/src/track2/json.rs:12`;
  `bbnf-bench/src/gate.rs:34,214`; PROVE-12 XOR-fold (`CSS_CANON_PROFILE`).
- Regen digest gate: `skinny/xtask/src/regen.rs:37` (`check_target`), `:177-184` (hash).
- Legacy consume-pattern reference: `crates/core/src/backend/driver/analysis.rs:22`
  (`TypeAnalysis::from_ir` reads `ir.types`, does not re-derive) — the shape the live L1→L2
  contract mirrors.
