# LOCK-3 · PASS-2 · Family 3A — CONTRACT-TOWER

Base pin: `skinny@06e70053f` (14 commits past the frozen amendment base `b3cb8c16b`; `b3cb8c16b`
verified `git merge-base --is-ancestor` of HEAD). Every `path:line` re-opened on this tree; the
seed line counts (ir 1020 LOC / passes 2750 LOC) are stale — live is `ir/src/lib.rs` 678 LOC,
`passes/src/lib.rs` 2096 LOC. Every claim below is the live artifact, not the seed's.

## 0. The family thesis, made concrete

3A is a **fixed tower of total lowerings, each guarded by a per-edge contract that is a value-parity
oracle** — the a-posteriori, corpus-bounded analogue of CompCert's per-pass simulation proof
(Leroy, *Formal verification of a realistic compiler*, CACM 2009) and the Nanopass discipline
(Sarkar/Waddell/Dybvig, ICFP 2004; Keep & Dybvig, ICFP 2013), where a compiler is many tiny total
passes each with a machine-checkable input/output-language predicate. 3A supplies what the live
spine lacks: the spine already *is* a sequence of total lowerings, but **no edge carries a checkable
contract, and one rung is entirely untyped** — the `->` value dies at the first edge.

The mechanism is three artifacts, all buildable on live:
1. a threaded node-type at the infer edge (the shared question), consumed by
2. a per-edge `verify` predicate (RED on HEAD → GREEN when threaded), enforced by
3. a `differential(edge, corpus)` runner that generalizes the two parity digests already shipping.

## 1. The live spine — the enumerated edge-DAG (answers contract-question 3)

`passes::compile` (`passes/src/lib.rs:32-68`) is the spine. Its transposition edges:

| Edge | Site | Function | Value level crossed |
|---|---|---|---|
| **E0** normalize | `passes:33` `normalize` | GrammarIr → GrammarIr | none (canonicalize) |
| **E1** infer | `passes:35` `layout::types::infer` → `infer_expr` (`:183-213`) | ExprKind → `Type` (7-arm: Var/Builtin/Seq/Alt/List/Option/Rule, `:234-243`) | **structural type** |
| **E2** lower | `single_plan`→`lower_expr` (`:1349` `Project→ValueProject`) | ExprKind → BackendExpr (13-arm) | **value ops** |
| **E3** materialize | `materialize_rule` (`:1362-1389`) | BackendExpr → `Seq[SpanMark(Start), body, SpanMark(End), TapeEmit{kind}, DirectBuild{shape,fields}, Return]` | **emit-order splice** |
| **E4** sink-project | `lower/sink_only.rs:126` `lower_program` | BackendIr → `SinkOnlyProgram` (`:19-28`: entry, rules, `direct_shapes`, `span_kinds`, `literals`, dispatch count, policy) | **backend roster** |
| **E5** emit | `runtime_generator.rs:98` gate → `json_sink_direct` / `json_templates` / `general_descent` | SinkOnlyProgram → Rust source | **byte-exact text** |

**Reconciliation of SpanMark/TapeEmit/DirectBuild/Return + `SinkOnlyProgram`:** these are NOT four
levels — they are four *arms of the single BackendExpr sum*, all produced by one edge (E3);
`SinkOnlyProgram` (E4) then flattens them into rosters (`direct_shapes`, `span_kinds`, `literals`).
So the spine has **≥5 labeled edges but only ~4 distinct value levels** (structural-type,
backend-value-op, backend-roster, emitted-text). The grammar `->` *type value enters none of them* —
dropped at E1 (`:204`), carried verbatim-but-unread through E2's `ValueProject.ty`. That untyped
rung is 3A's target. A "small fixed tower (three levels)" is therefore a **design collapse claim,
not a live fact**: E3 is a pure derivation (see §6.2), collapsible into E2/E4; E0 is idempotent
canon. The honest tower is L0 GrammarIr → L1 (BackendIr + `LayoutFacts`) → L2 SinkOnlyProgram.

## 2. The shared question — threading the `:204` `ty`, end-to-end on live

The live fact the seeds obscure: **`LayoutFacts` already stores `node_types: HashMap<ExprId, Type>`**
(`passes:93`), populated at `self.facts.node_types.insert(expr_id, ty.clone())` (`:212`) and carried
in `PipelineOutput.layout_facts` (`:85`). So `:204` is **not a missing store — it is a *replacement***:
at `Project`, the body's structural type is inserted for the Project node and the declared
`ty: Option<String>` (`ExprKind::Project{body,target,ty}`, `ir/src/lib.rs`) is discarded by the `..`.
Threading is surgical, not architectural — three touch-points:
- `:204` — when `ty = Some(name)`, resolve `name` to a `Type` and insert *that* into
  `node_types[ProjectExprId]`, while still recursing the body (the body ExprId keeps its structural
  type). Carrier: a new `Type::Named(String)` arm (`:234`) — KISS, and `->` targets are abstract per
  the backend-agnostic-types law; each backend resolves the name. (`Type::Rule` reuse only when the
  `ty` names an existing rule.)
- `:1349` — `ValueProject.ty` already carried verbatim into BackendExpr; no change.
- **Consumption (load-bearing clause):** the threaded `node_types[ProjectExprId]` is consumed by the
  §3 `verify` predicate and by a schema-derivation pass (R6 §8b `derive_direct_schema`), **not**
  re-derived. The directive-named `TypeAnalysis::from_ir` lives in the LEGACY outer tree
  (`crates/core/src/backend/driver/analysis.rs:22`) and consumes `ir.types: Vec<(RuleId, TypeDesc)>`
  + `ir.type_map.scratch_types()` — a `TypeDesc` shape **skinny does not possess** (`rg TypeDesc` in
  skinny → 0). So 3A's honest consumption contract: the *live* consumer is the level contract
  (verify + differential digest); `TypeAnalysis::from_ir` is the **greenfield L1 consumer** the
  tower grows, carried as a **born-RED edge** owned by the schema-derivation pass, keyed off the same
  ExprId → `node_types[ExprId]` fed into a `TypeDesc` builder. 3A never re-derives the node-type at
  the consumer.

## 3. The `verify` predicate over `Project`@204 (answers contract-question 1)

`fn verify_project_types(g:&GrammarIr, f:&LayoutFacts) -> bool`: for every `ExprId` whose kind is
`Project{ ty:Some(_), .. }`, assert `f.node_types[id]` is the *resolved declared type* — **not**
structurally equal to `f.node_types[body_id]`. On HEAD this is **RED for every projected node** (both
entries are the body's structural `Type`; `:204` copies it). Post-thread it is **GREEN**. This is
exactly rustc `-Zvalidate-mir` / the MIR `Validator`, LLVM `-verify-each`, and MLIR per-op verifiers
(predicates run at every pass boundary) — but *stronger*: those check well-formedness (invariants);
the 3A per-edge contract checks **refinement/parity** (translation validation, Pnueli/Siegel/
Singerman, TACAS 1998; Necula, TVOC).

**Zero-emit-footprint witness (the delicate part):** JSON's grammar has **zero `->`**
(`grammars/json.bbnf`, 18 lines pure EBNF, R6 §0), so on the JSON corpus the predicate is **vacuously
GREEN — never RED** (no Project nodes). The RED→GREEN transition is witnessable **only on Sheets**,
the sole fleet grammar with projections (`grammar/google-sheets/google-sheets.bbnf:6,12,17-18,34-42,97`),
or a synthetic projection corpus. On Sheets the emit path *does* read `Project.ty` — via
`general_descent/view.rs` `classify_projection(target, ty)` → `ProjectionKind{Float,Bool,Tag,Span}`.
So byte-identical regen (the zero-footprint requirement) holds **iff the thread writes a *parallel*
`node_types` fact and leaves `classify_projection` reading `Project.ty` directly** — the new type is
a new consumer's input, not a replacement of the emit input. Evidence a prototype owes: regen/check
× {json,css,sheets} all six byte-identical with the predicate wired on vs off.

## 4. `differential(edge, corpus)` — generalizing the two live digests (answers question 2)

Two parity digests already ship and must be *reproduced*, not re-authored:
- **`parity::assert_parity`** (`bbnf-bench/src/parity.rs:23-62`): track1(generated) vs track2(handcoded)
  on `offsets()` ∧ `flag_cursors()` ∧ `flag_values()` ∧ payload counters (writes = allocs = 0, the
  lazy-tape invariant) ∧ `to_canonical_string()` (key-sorted, `ParityError::Serialization` reports
  the divergent pair).
- **`direct_struct`** track1/track2 `strict_product` (checksum) + `digest`
  (`bbnf-bench/benches/json_parity.rs:198-266`) — the typed-path digest.

A unifying `differential(edge, corpus)` runner: for each labeled edge E of §1, define a per-edge
digest `d_E(grammar, input)` — E1: a sorted `(ExprId → Type)` fingerprint of `node_types`; E3: the
`(SpanMark, TapeEmit.kind, DirectBuild.shape)` splice sequence per rule; E4: the `SinkOnlyProgram`
roster hash; E5: the two shipping digests (offset-stream + canonical + strict_product). Parity = all
`d_E` agree between a **committed golden per edge** and the live lowering.
- **(a) reproduces the digests:** E5's `d_E` *is* `assert_parity`'s conjunction + `strict_product`
  by construction — the golden E5 digest = today's track2. The runner subsumes both.
- **(b) localizes an INJECTED mis-lowering:** inject a fault at E_k (swap a `TapeEmit{kind}`, drop a
  `SpanMark(End)`, mis-resolve a `ty`), run the corpus; the **first edge whose `d_E` diverges from
  its golden localizes the injection**. TapeEmit swap → d_E3 diverges while d_E1/d_E2 stay equal
  (E3-localized); `ty` mis-resolution → d_E1 diverges first (E1-localized). This is Alive2-style
  bounded translation validation (Lopes/Menendez/Nagarakatte/Regehr, PLDI 2021) specialized to fixed
  edges instead of SMT-checked peepholes, and EMI/Csmith differential testing (Le/Afshari/Su, PLDI
  2014; Yang et al., PLDI 2011) with the pipeline edge as the differential axis.

**Coverage metric & zero-witness reporting:** edge E is **COVERED** iff ≥1 corpus input drives a node
reaching E with a non-degenerate value (E3-`TapeEmit::NullValue` is covered only if the corpus
contains `null`). Uncovered edges are reported **UNVERIFIED (sound-by-vacuity ≠ sound)** — the EMI
equivalence-modulo-inputs completeness gap named explicitly: a mis-lowering on a zero-witness edge
survives. This is why the §3 `->` predicate is UNVERIFIED on JSON and must run on Sheets. Coverage
bound = per-edge witness count over `data/json/*`, `corpora/css-l4-sk-v14/*`, `data/sheets/*`.

## 5. Prior art (citations)

- **Nanopass** (Sarkar/Waddell/Dybvig ICFP'04; Keep/Dybvig ICFP'13) — the canonical 3A ancestor: a
  compiler as many total passes, each with a `define-language` grammar and an auto-generated boundary
  checker. 3A = Nanopass conformance predicate **+ value-parity differential** (Nanopass checks
  output-language membership; 3A additionally checks refinement).
- **CompCert** (Leroy, CACM'09) — per-pass forward-simulation *proofs*; 3A is the *validated*
  (per-run, corpus-bounded) analogue — a deliberate, weaker, cheaper contract.
- **Translation validation** (Pnueli/Siegel/Singerman TACAS'98; Necula PLDI'00 TVOC) — validate the
  run, not the compiler: the per-edge contract's philosophy.
- **Alive/Alive2** (Lopes et al. PLDI'21) — bounded translation validation for LLVM; the SMT sibling
  of the corpus-bounded `differential(edge,·)`.
- **LLVM `-verify-each` / MLIR op-verifiers / rustc `-Zvalidate-mir`** — inter-pass invariant
  predicates; the engineering template for the `verify` predicate, strengthened here to parity.
- **EMI / Csmith** (Le/Afshari/Su PLDI'14; Yang et al. PLDI'11) — differential compiler testing and
  the equivalence-modulo-inputs completeness gap naming the zero-witness UNVERIFIED problem.

## 6. Named risks & counterexamples

1. **Vacuous RED on JSON** — the `->` predicate has no Project node to fail on JSON; the RED→GREEN
   witness *requires Sheets or a synthetic projection corpus*. A prototype proving RED→GREEN only on
   JSON proves nothing (Constitution vii: concrete-or-SPEC-ONLY).
2. **The three-level count is contestable** — the live spine has ≥5 edges; collapsing to 3 rests on
   E3 being a pure derivation. It is: `derive_materialization_plan(grammar)` (`passes:1139`) takes
   **only** `grammar` (no layout/cost input — verified at call site `:37-38`), so `materialize_rule`'s
   splice is a pure function of GrammarIr, recomputable inside E4. No external data on E3 → the
   collapse is sound. But then *the contract must attach per-edge, not per-level*, or it cannot
   localize (§4).
3. **per-RuleId vs per-ExprId key** — `materialize_rule` keys by `RuleId` (`:1367`); `node_types` and
   `Project` are per-`ExprId` (`:93`). Counterexample: a grammar where two rules project the *same*
   referenced sub-rule to *different* target types → a per-RuleId descriptor cannot carry both
   node-types; the tower needs per-ExprId carriage for `ty`. JSON/CSS never hit this (no `->`);
   Sheets is single-projection-per-target today, so the collision is **latent, not live** — a
   prototype must construct the two-rule case to prove the key must be ExprId.
4. **The named consumer is legacy** — `TypeAnalysis::from_ir` (outer `crates/core`) consumes a
   `TypeDesc`/`ir.types` shape skinny lacks. 3A cannot "thread to that consumer on live" without
   *introducing* `TypeDesc` into skinny. Honest disposition: the live consumer is the level contract;
   `TypeAnalysis::from_ir` is a born-RED greenfield edge. Any claim the thread reaches an *existing*
   skinny type consumer is false — there is none (R6 §5: no TypeDesc).
5. **Golden drift** — the differential runner needs a committed golden per edge; goldens are regen
   output, so the clean-regen law binds them (never hand-patched). A stale golden reads as a spurious
   localization.

## 7. What a prototype must test to move 3A

1. **Thread + predicate + zero-footprint** — add `Type::Named` (`passes:234`); thread `ty` at `:204`
   into `node_types` as a parallel fact; add `verify_project_types`; execute on Sheets: record RED
   (HEAD) → GREEN (threaded); assert all six regen/check outputs byte-identical with predicate on/off.
   Deliverable: the executed transition + the six digests.
2. **`differential(edge, corpus)`** — implement per-edge `d_E`; prove (a) d_E5 reproduces
   `assert_parity` + `strict_product` on `data/json/{twitter,canada,citm_catalog}`; (b) inject a
   `TapeEmit{kind}` swap → *only* d_E3 diverges (localization); inject an `ExprKind::Project.ty`
   mis-resolution → d_E1 diverges first. Report per-edge coverage; flag every zero-witness edge
   UNVERIFIED. Deliverable: the divergence table + coverage census on the live corpus.
3. **Edge-collapse / key-width** — trace `derive_materialization_plan`'s inputs to confirm E3 is
   pure-over-grammar (collapsible), and construct the two-rule same-sub-rule/different-`ty`
   counterexample to fix whether the `ty` carrier keys on ExprId (yes) vs RuleId. Measure net-LOC of
   thread + predicate + runner (parsimony budget) and the emission-extensibility count (does the
   contract shrink the 5-shape emit fan?). Deliverable: the pure-derivation trace + key-width verdict
   + LOC/extensibility numbers.
