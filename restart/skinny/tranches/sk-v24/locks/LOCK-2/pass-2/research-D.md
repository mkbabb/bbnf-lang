# LOCK-2 · FAMILY 2D — PROFILE-CLOSED-LOOP (research-D, pass 2)

**Thesis.** Optimization is a measured feedback loop, not a one-shot cost guess. Per-feature
cost coefficients are *fitted* from cold-paired PMU captures (cyc/B), *committed* as a
determinism-preserving **integer** fact table keyed **per-(class, caller-context)**, and
*consumed* by the decision driver, which re-runs at regen. A provenance-stamped **freshness
gate** goes RED when the grammar rule or kernel the coefficient describes mutates out from
under it. The loop — capture, fit, commit, select, re-measure — is the architecture; the cost
model is a regenerable artifact, not a hand-tuned constant.

The family's hardest constraint, and its distinguishing move, is that the loop must earn a
*real decision flip* against a reigning null result. It cannot on the surface the engine's
**float extraction weights** expose (that surface is proven inert). Pass-2's sharpened
discovery: the profile objective must be **relocated** off the (inert, float, IR-canon)
extraction weights and onto the (live, integer, kernel-crossover) discrete lowering choices —
and those choices are today **not modeled in the middle IR at all**; they live in a runtime
dispatch. Relocation is therefore two coupled moves: (i) *promote* the competing site into the
IR so the cost objective can see it, and (ii) key the fitted coefficient on the **caller
context**, because the engine's own admissibility predicate is already context-sensitive.

---

## 1. The substrate already exists — inert — and names its own blocker

The engine ships the *complete shape* of a profile-closed loop, wired and populated, but fed by
hand-set floats and a placeholder provider. The seams are cut, the fact fields typed, the
determinism replay runs. Only the profile is a stub.

- **The committed cost fact table is FLOAT.** `crates/egraph/src/cost_weights.rs:175`
  `pub const CALIBRATED_WEIGHTS: CostWeights` — eleven `f64` dimensions (`structural: 1.0`,
  `alt_per_branch: 1.5`, `dispatch_bonus: -2.0`, `call_overhead: 4.0`,
  `inline_body_size_penalty: 0.5`, `tape_push: 1.0`, `dispatch_branch: 0.5`,
  `dispatch_table: 3.0`, `prettify_emission: 2.0`, `cross_module_coercion: 1.5`,
  `:176-191`). This IS the committed table a fit would supersede; every production consumer reads
  it through `CostWeights::default()` (the doc-stated "exactly one slot to update when a future
  calibration produces a non-trivial result", `:169-174`). **It is float** — the calibration
  surface that returned null (below), and the surface a profile fit must *not* land on for the
  determinism reason of §5/Q3.

- **The null result that guards the door** (`cost_weights.rs:134-167`). A 54-configuration grid
  sweep over `{structural, alt_per_branch, dispatch_bonus, ref_cost}` on the 4-grammar corpus
  produced **zero** DTA-state-count variance: JSON 51, BBNF 476, CSS-L4 2875, Sheets 191 — the
  same across *all* 54 configs (`:147-152`, verified verbatim). The current rewrite set
  (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
  `CommonSuffixFactor`, `:154-156`) "produces e-classes with at most one semantically-distinct
  canonical form … so the cost model has no choice to exercise" (`:157-158`). Extraction
  multiplicity ≈ 1. **A profile-fed extraction coefficient flips nothing.** The sweep artifact
  `docs/benchmarks/cost-weights-sweep.json` is the reusable evidence; the sweep script named at
  `:137` is the only-once harness.

- **The live freshness scaffold.** `skinny/crates/passes/src/backend_egraph.rs` computes, per
  rule, a real `ActiveCostFacts`: `cost_formula_version = FORMULA` where `FORMULA =
  "sk-v13-w6-integer-v1"` (`:8` — the live cost is **integer**, hand-set);
  `rewrite_set_id = REWRITE_SET = "sk-v15-w7-direct-sink-normalization-v1"` (`:9,119`);
  `candidate_cost_stale_rate_bps` from the stale fraction (`:91-96,124`);
  `selected_cost_freshness = "fresh"` iff `selected.score.freshness_rank == 0` (`:127`), where
  `freshness_rank = u32::from(candidate.stale)` (`:169`); `determinism_replay_status = "pass"`
  (`:139`); `selection_trace_hash = stable_hash_hex(&trace)` (`:142`) — a deterministic tie-break
  trace; `cascade_fallback_status = "fail-closed"` (`:145`). The `DecisionCost` carries an
  **integer** `perf_cost: u32` (`:21,170,284`).

- **The named blocker.** `skinny/crates/passes/src/decision_csp.rs:183`
  `static_profile_provider_status: "static-template-blocker"` (also modeled on the IR type,
  `ir/src/cost.rs:243`). At HEAD there is *no* profile provider — the field declares it blocked.
  2D's mechanism is: **replace the static-template-blocker with a real fact-table-backed
  provider**, wire it into `perf_cost` and `candidate.stale`, keep `determinism_replay_status`
  green.

- **The evidence slot on every rejected alternative.** `skinny/crates/ir/src/cost.rs:289`
  `struct RejectedAlternative { … evidence: Vec<Measurement> (:293) }`; `:306`
  `struct Measurement { cycles_per_byte_x1000: Option<u64> (:311), source: EvidenceSource (:314),
  source_ref: String (:315) }`; `:319` `enum EvidenceSource { BenchProbe, RedressBackfill,
  AuthorDeclared, StaticAnalysis }`. The IR *already* models per-alternative measured cyc/B as
  **fixed-point integer** (×1000) — the determinism-safe encoding is a decision the substrate
  already made. Today the option is `None` (`:37` builds `evidence: Vec::new()`); 2D fills it
  with `BenchProbe` facts whose `source_ref` is the fact-table commit.

**Reading:** the loop is scaffolded end to end; only the profile is a stub, and it must be
relocated off the float weights (inert) onto the integer decision surface (live).

---

## 2. The mechanism made concrete — the five-stroke loop

1. **CAPTURE.** For each *competing site* (§3) run the standing cold-paired PMU protocol
   (`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, N≥200, ≥3 windows, per-block loadavg, ins/B as
   the load-invariant anchor) at **real value-start positions** on the fleet corpora — never
   synthetic buffers. Emit cyc/B per candidate lowering, **per caller-context** (§4).

2. **FIT.** Solve the small linear system relating whole-grammar cyc/B to per-feature
   coefficients by **A/B-differential isolation** (§6): each paired binary differs in exactly one
   feature; the cyc/B delta *is* that feature's coefficient. Quantize to `u64` fixed-point
   (×1000), matching `Measurement.cycles_per_byte_x1000`.

3. **COMMIT.** Write the fitted coefficients as a fact table — successor to
   `FORMULA="sk-v13-w6-integer-v1"`, *not* the float `CALIBRATED_WEIGHTS* — stamped with a
   **provenance hash** over `{rule-IR-subtree digest, kernel-set digest, caller-context
   discriminant, formula version, corpus set}`. A reviewed source artifact, never an env read.

4. **SELECT.** `backend_egraph.rs` reads the table: `perf_cost` ← fitted integer coefficient;
   `candidate.stale` ← provenance-hash mismatch. The driver re-runs at regen; the trace hash
   certifies determinism. HashMap iteration inadmissible — extraction ties break on the sealed
   total order `(perf_cost asc, freshness_rank asc, candidate_id lex)`.

5. **RE-MEASURE / GATE.** A follow-on capture on the emitted parser confirms the fit predicted
   the winner; on mutation the provenance hash diverges, `stale=true`, `stale_rate_bps` rises,
   `selected_cost_freshness = "stale"`, gate RED — the loop demands recapture before that site's
   coefficient is trusted again.

The loop is **offline** (bounded to regen), not online — the deliberate departure from
database-style continuous learners (§8): byte-identical regen forbids a coefficient that drifts
between compiles.

---

## 3. The decision surface must be RELOCATED — and it is not yet in the IR (Q5)

The null result (§1) is fatal *only* to profiling the float e-graph extraction weights. The
live objective is the set of **discrete kernel crossover thresholds** where two lowerings
genuinely compete and the winner is corpus-dependent — precisely where cyc/B, not structure,
decides. **Pass-2 finding (Q5): at HEAD these competing sites are NOT modeled in the middle IR.**
The live kernel competition lives in a *runtime* dispatch:

- **Runtime classifier switch (live, cost-shaped, but NOT in IR).**
  `skinny/crates/bbnf-simd/src/dispatch.rs:12` `enum SelectedBackend { Scalar, NeonTbl4 }`;
  `:89` `fn select_backend(alphabet: &[u8; 64])`; `:101` `fn lo6_table_admissible`. `NeonTbl4`
  iff the alphabet admits a lo6 table, else `Scalar`. This is a real discrete choice with ≥2
  survivors — but it is decided at *parser init from the alphabet*, downstream of the IR, so the
  middle-IR cost objective cannot see it. The admissibility predicate is *structural* today; a
  fitted coefficient makes it *cost-conditioned* — admit the table only where its measured cyc/B
  beats scalar on the fleet.

- **The IR models ONE scan mode.** `ir/src/lib.rs:425` `enum SimdMode { Exact }` (single variant)
  and `SimdSite { PreEntry }` — `Recognizer::SimdScan` carries no competing alternative. So the
  answer to Q5 is: **no fleet grammar today presents ≥2 scan/self-loop sites modeled IN the IR
  with binding static-size + cyc/B costs.** The competition exists physically (dispatch.rs) but
  is invisible to the tower's cost objective.

- **The IR extension that models it — and who owns building it.** The greenfield loom's
  scan-lowering wave promotes the site into the IR: `SimdMode::{Exact, Prefilter}` +
  `SimdSite::{PreEntry, SelfLoop { terminators: StructuralAlphabet }}` (the `ir/src/lib.rs:425/430`
  extension). **LOCK-3 (IR transposition) owns building the IR node; LOCK-2's cost objective
  consumes it.** This resolves the XL-F supersession relation with evidence: the scan-lowering
  predicate **waits-on** the IR extension (no competing site until the enum widens), and the
  profile cost **wraps** it (the structural predicate `complement cardinality ∈ [1,16]` decides
  *whether* a prefilter applies; the fitted cyc/B decides *whether it is worth applying* — the
  quantity the structural predicate lacks). It does not *replace* the predicate: the predicate is
  a legality filter, the cost is the objective over the legal set.

- **Corpus-dependent flips that already exist (the family's proof of life).**
  - *CSS shufti vs scalar.* `CSS_SHUFTI_MIN_SPAN` — the span below which the vector classify is
    not amortized and scalar wins — is explicitly a cost-model value, not a heuristic literal
    (GN-2.W3 charter). animate.css (keyframe-dense, 5–10-byte components) regresses +13.4% cyc/B
    / +29.9% ins under an un-guarded vector classify while tailwind wins **4.16×**: *the same
    kernel, opposite verdicts, decided by span length.* The canonical corpus-dependent flip a
    fitted `MIN_SPAN` resolves.
  - *N\* kernel crossover* (the empirical kernel-length threshold) and the `PriorityStep` cascade
    `P1..P8` (`ir/src/cost.rs:264-276`) — an 8-step precondition ladder a cost objective
    supersedes only where the preconditions tie (§7).

**Seam to plug the objective in:** `skinny/crates/passes/src/lib.rs:398`
`derive_backend_shape` → `:407` `_with_diagnostics` → `BackendShapePlan.cost_facts`. The fitted
coefficient enters as the `perf_cost` `backend_egraph.rs` extracts over, and as `BenchProbe`
`Measurement` evidence on each `RejectedAlternative` (`ir/src/cost.rs:289,306`).

**Consequence:** 2D succeeds *iff* the competing site is promoted into the IR and the objective
targets these crossovers; it inherits the null result if it targets the float extraction
weights. In the tower-ownership frame the profile objective lives at the
**kernel/backend-selection decision point**, downstream of canonicalization, where multiplicity
is genuinely ≥2 because two kernels *both* recognize the same language at different measured cost.

---

## 4. The coefficient is CONTEXT-KEYED — the engine's own predicate proves it (Q4)

Pass-2 audit of `admits_sink_only` (Q4's named target) finds a **live caller-context conflict**,
and it is exactly the surface that forces the fitted coefficient to be keyed on context, not on
the rule alone:

- `skinny/crates/passes/src/lib.rs:954` `fn admits_sink_only(rule_ir, target) =
  contains_direct_build(&rule_ir.expr) && (target.direct_only_output || target.direct_build_consumer)`.
- `TargetFeatures` (`:369-373`) carries **two distinct caller contexts**: `direct_only_output`
  and `direct_build_consumer` (`:372-373`).
- The **same** `rule_ir` (a sub-rule whose `expr` contains a `DirectBuild`, per
  `contains_direct_build`, `:967-985`) yields a **different** shape admissibility under a
  sink-only-output caller vs a direct-build-consumer caller. This is the caller-context conflict
  Q4 asks after: one per-class shape cannot serve both when the two contexts diverge in which
  backend shape is legal/cheapest. Used at the selection sites `:524` (candidate generation) and
  `:661` (guard against an illegal SinkOnly under a non-admitting context).

**What follows for 2D (the identity-preserving repair):** extraction is *not* context-free. The
fitted cyc/B coefficient must be keyed on **(rule-IR-subtree, TargetFeatures discriminant)**, and
the provenance hash (§5) must include the `TargetFeatures` context. The repair's complexity is
bounded — the context is a small finite discriminant (today two booleans → ≤4 contexts), so the
fact table is (rule × context) rows, not a combinatorial blowup, and byte-identity is preserved
because the context enters the provenance digest deterministically. A single per-class shape that
ignored context would either mis-cost the DirectBuild sub-rule under the sink-only caller or under
the consumer caller — the concrete failure the context key prevents.

---

## 5. Determinism through cost-guided extraction — float vs integer (Q3)

A committed coefficient describes a *specific* rule lowered by a *specific* kernel under a
*specific* formula and caller-context. When any mutates, the coefficient is stale and must not
silently steer a decision. Pass-2 verifies the float/integer boundary that makes this safe:

- **The fit lands on the INTEGER surface, not the float weights.** `CALIBRATED_WEIGHTS` is `f64`
  (`cost_weights.rs:176-191`) — and its calibration returned null (§1). The live extraction
  `perf_cost` is `u32` (`backend_egraph.rs:21,170,284`), and the IR's per-alternative evidence is
  `cycles_per_byte_x1000: Option<u64>` (`cost.rs:311`). 2D routes the fit through the **integer
  perf_cost / fixed-point cyc/B**, never through the float weights — *because* any float in
  `perf_cost` or its hashing reintroduces iteration-order nondeterminism. `FORMULA =
  "…integer-v1"` is not a limitation; it is the invariant. The float weights remain the fallback
  cost the fail-closed cascade uses when the fit is stale (below).

- **The total-order tie-break (Q3's explicit ask).** Extraction ties break on
  `(perf_cost asc, freshness_rank asc, candidate_id lex)`, sealed by `selection_trace_hash`
  (`backend_egraph.rs:142`) with `determinism_replay_status = "pass"` (`:139`). HashMap iteration
  order never touches the selection. This is what makes `regen∘regen` byte-identical through
  cost-guided extraction: the fitted `u64` cyc/B feeds `perf_cost`, the integer tie-break is
  total, and the trace hash certifies replay equivalence.

- **The freshness gate — the AutoFDO discipline.** Stamp each fact-table entry with
  `provenance = stable_hash(rule-IR-subtree ∥ kernel-set ∥ TargetFeatures ∥ FORMULA ∥ corpus)`.
  At regen, recompute the digest; `candidate.stale = (stored != recomputed)`. Gate RED when
  `candidate_cost_stale_rate_bps > 0` on any site whose decision the table steers
  (`backend_egraph.rs:91-96,124,127`). This is exactly LLVM AutoFDO/CSSPGO **stale-profile
  matching**: a profile against an older revision is detected by hash-matching anchors (loose→
  strict) and triggers conservative fallback rather than a wrong annotation. Our anchor is the
  IR/kernel/context digest; our fallback is the hand-set float `CALIBRATED_WEIGHTS` /
  structural `PriorityStep` cascade (`cascade_fallback_status = "fail-closed"`,
  `backend_egraph.rs:145`).

---

## 6. Attribution — A/B-differential, because whole-grammar cyc/B underdetermines (Q8)

A single whole-grammar cyc/B number is one equation in many unknowns (`tape_push`,
`dispatch_branch`, `call_overhead`, …): the per-feature system is underdetermined. Two
disciplines resolve it, both already exercised by the fleet's measurement corpus:

- **One-lever paired isolation.** Build two binaries differing in *exactly one* feature and
  interleave them back-to-back per window; the cyc/B delta is that feature's coefficient. This is
  how the const-mask residualization win was attributed (identical hunks, ins/B −4.8/−6.8%
  deterministic, cyc/B 0.81–0.98× per corpus) and how the CSS animate regression (+13.4% / +29.9%)
  was isolated from the tailwind win (4.16×) via flag ON/OFF on the same binary family. The fit is
  a **least-squares over the one free variable per pair**, not a global regression over a
  confounded whole.

- **ins/B as the load-invariant anchor.** cyc/B carries host-load noise; ins/B agrees across
  windows to <0.2% and is the certified floor. The fit is *stated* in cyc/B (the decision metric)
  but *validated* against the deterministic ins/B delta — a coefficient whose ins/B moves the
  wrong way is rejected regardless of a favorable cyc/B window.

This mirrors **uops.info** (§8): isolate the quantity under measurement in a controlled harness,
measure it directly rather than infer it from a confounded aggregate, emit a machine-readable
table. The grammar-feature analogue of "per-instruction latency" is "per-feature cyc/B"; the
analogue of its generated microbenchmark is the paired one-lever fleet binary.

**The env fence (Q8: env-source fenced from regen).** `crates/egraph/src/cost_config.rs:83`
`CostConfig::from_env` reads `BBNF_COST_*` (`BBNF_COST_STRUCTURAL`, `_ALT_PER_BRANCH`,
`_DISPATCH_BONUS`, `_EGRAPH_ITER_LIMIT`, `_EGRAPH_NODE_LIMIT`, `_PARALLELISM_THRESHOLD`,
`:74-95`). This surface is for **benchmarking the sweep only.** The pipeline is strictly:
`env override → sweep harness → proposed fitted table → human review → committed const → regen`.
Regen reads *only* the committed const, never `from_env`. The provenance hash is computed over the
*committed* table + IR/kernels/context — it explicitly excludes env, so no env value can alter the
freshness verdict. Enforce with a grep-gate: zero `from_env` on codegen/regen paths, mirroring the
zero-name-gate CI. `from_env` is a proposal instrument; the reviewed source diff is the only path
into regen.

---

## 7. The budget is a SOLVER budget, not a size knapsack (Q7)

Pass-2 finding: the only "budget" the engine models is a **solver node-count limit**, not a
generated-size knapsack. `decision_csp.rs:90` `node_budget: Some(NODE_BUDGET)`; `:103,148-151`
`csp_budget_status ∈ {"budget-exceeded", …}`; `backend_egraph.rs:100-107,138`
`egraph_budget_status ∈ {"budget-hit", …}` — these bound saturation/search *effort*, not emitted
LOC. There is **no `bounded_sum` in `crates/csp-solver/src/`**. The generated-LOC budget is an
*external* doc-gate (`lint-loc` xtask; the per-wave `generated_loc_budget`), not a constraint in
the store.

**So the `bounded_sum` budget-knapsack-as-constraint is SPEC-ONLY at HEAD.** For 2D this fixes the
relation cleanly: *if* a sibling family builds the knapsack in the constraint store, the profile
fit supplies its **value** (per-site cyc/B) and generated-LOC supplies its **weight**;
selection-equivalence with the `PriorityStep` cascade (`cost.rs:264-276`, `P1..P8`, `ALL` at
`:276`) is the obligation — a fitted objective supersedes the cascade *only where the
preconditions tie*, so the equivalence proof is: (a) on non-tied inputs the cascade's precondition
order already forces the choice and the objective must agree; (b) on tied inputs the objective is
the tie-break the cascade lacks. One solver home coordinates the store; 2D contributes numbers,
not a second solver.

**Interlock to the solver-shape question (Q6).** For a near-tree decision graph (independent
sites + budget coupling + few coherence edges), the fitted per-site cyc/B is exactly the PBQP
per-node cost *vector*; the pairwise cost *matrices* carry the coherence 2D can name concretely —
e.g. GN-2.W3's **hoisted LUT** shared across CSS component-scan sites makes two sites' costs
non-independent (build-once amortization), a genuine off-diagonal term. 2D's deliverable to Q6 is
the measured populated matrix, not the solver; whether PBQP or B&B consumes it is 2C's verdict.

---

## 8. Prior art (with citations)

- **uops.info** — Abel & Reineke, ASPLOS '19 (arXiv:1810.04610). Automatically generated
  microbenchmarks yield machine-readable per-instruction latency/throughput/port tables. Direct
  precedent for *measured, committed, machine-readable cost fact tables*; the A/B-differential
  paired fit is the grammar-feature analogue of its per-instruction isolation.
- **LLVM AutoFDO / CSSPGO / Stale-Profile Matching** — Chen et al. CGO '16; Ayupov & Panchenko,
  Meta (arXiv:2401.17168, LLVM D147456). Multi-level hash matching (loose→strict) with
  flow-conservation inference detects stale profiles; pseudo-instrumentation anchors let PGO "see
  through source changes." Precedent for the **freshness gate**: provenance-anchored staleness with
  conservative fallback.
- **LLVM `SchedMachineModel` (TableGen `.td`)** — committed per-microarchitecture latency/uop
  tables consumed by the scheduler / `llvm-mca`. Precedent for the *deterministic,
  hand-committable* form of a cost table — contra learned throughput predictors (**Ithemal**,
  Mendis et al. ICML '19), which the determinism law forbids.
- **DB2 LEO — the LEarning Optimizer** — Stillger et al., VLDB '01. A feedback loop correcting a
  cost model from measured execution. The closed-loop precedent — but *online*; 2D takes the loop
  shape and bounds it to *offline regen* to preserve byte-identity.
- **Halide autoscheduler** (Adams et al., SIGGRAPH '19) and **TVM Ansor** (Zheng et al., OSDI '20)
  — fitted cost models ranking discrete schedules from on-device measurement: the canonical
  measure→fit→rank→re-measure loop. 2D adopts the loop, rejects the model class
  (learned/non-deterministic) for a committed integer table.
- **egg `LpExtractor` / `CostFunction`** (Willsey et al., POPL '21) — extraction is cost-ranked,
  but the cost function is *static and user-supplied* (`AstSize`). 2D's contribution over egg: the
  cost function is a *regenerable measured artifact* with a staleness gate and a context key.
- **PBQP register allocation** — Scholz & Eckstein (2002), Hames & Scholz LCTES '06 — the pairwise
  discrete cost model 2D's fit populates (Q6 interlock).

---

## 9. Named risks & counterexamples

- **R1 — the null result generalizes (family-killer).** If, after promotion into the IR and
  relocation to the crossover thresholds (§3), *no* fleet site presents ≥2 kernels with binding,
  corpus-dependent cyc/B ordering, the loop has nothing to flip and 2D collapses to a static
  table. **Counterexample that saves it:** CSS shufti vs scalar on animate (loss +13.4%) vs
  tailwind (win 4.16×) — same kernel, opposite verdicts by span length — is a *proven*
  corpus-dependent flip. The prototype (§10) must exhibit at least one such flip end-to-end or
  return a documented negative result (2D degenerates to a committed static table + freshness gate
  — still a valid, smaller family).
- **R2 — the competing site never enters the IR.** If LOCK-3 does not widen `SimdMode` beyond
  `Exact`, the objective has nothing to range over and the fit is stranded at the runtime
  `select_backend`, invisible to the tower. **Mitigation:** 2D's IR-extension dependency (§3) is a
  hard interlock, declared, not assumed — the family is blocked until the enum widens; state it as
  a π-obligation on LOCK-3.
- **R3 — attribution collapse.** If a paired binary cannot isolate a single feature (two features
  co-vary because the emitter fuses them), the fit is confounded. **Mitigation:** require a genuine
  one-lever diff (a template edit touching exactly one lowering); refuse a fit whose ins/B delta is
  inconsistent with its cyc/B delta.
- **R4 — context under-keying.** If the coefficient ignores the `TargetFeatures` context (§4), a
  DirectBuild sub-rule is mis-costed under the sink-only vs consumer caller. **Mitigation:** the
  fact-table key and provenance digest both include the context discriminant; a context-free key is
  a build-time reject.
- **R5 — freshness false-negative.** A digest hashing too coarsely (whole-grammar) marks every
  site stale on any edit (gate permanently RED); too fine (per-byte) and a cosmetic reformat trips
  it. **Mitigation:** hash the *lowered kernel identity + the rule's IR subtree + context*, not
  source text — the AutoFDO CFG-anchor lesson.
- **R6 — determinism leak via float.** Any float in `perf_cost` or its hashing reintroduces
  iteration-order nondeterminism. **Mitigation:** the ×1000 fixed-point `u64` encoding is mandatory
  and already present (`cost.rs:311`); the fit never touches the float `CALIBRATED_WEIGHTS` on the
  regen path.
- **R7 — env leak.** A careless `from_env` on the regen path silently de-determinizes the build.
  **Mitigation:** the fence of §6 plus a grep-gate (zero `from_env` in codegen/regen).
- **R8 — budget.** The provider must not blow the per-parse compile budget (~230 µs order). The fit
  is *not* per-parse — it runs at *regen* (offline); at compile time the provider is a table lookup
  keyed by the provenance digest, cached on the rule.

---

## 10. What a prototype must test to move the family

A single end-to-end prototype on one competing site — strongest candidate: **CSS shufti vs scalar
crossover**, whose corpus-dependent flip is already proven:

1. **Promote + fit.** Widen the IR to model the competing scan site (or, minimally, stub the IR
   node and populate `perf_cost` from it); sweep `CSS_SHUFTI_MIN_SPAN` (or the
   `lo6_table_admissible` cost condition), capture cold-paired cyc/B at real value-start positions
   across animate/tailwind/bootstrap/material, fit the crossover span as a `u64` fixed-point
   coefficient keyed per caller-context.
2. **Commit + wire.** Emit the coefficient as a fact-table entry with a provenance hash (including
   the `TargetFeatures` discriminant); replace `static_profile_provider_status =
   "static-template-blocker"` with a provider setting `perf_cost` and `candidate.stale`; confirm
   `derive_backend_shape` selects through it.
3. **(a) Flip.** Show the fitted coefficient selects a *different* `SelectedBackend`/guard than the
   hand default on ≥1 corpus (animate → scalar guard, tailwind → vector) — a real per-site decision
   flip, not a re-derivation of the same choice.
4. **(b) Win cold-paired.** The fitted decision beats the hand default cold-paired at real
   value-start positions: animate returns to ≤1.0× and tailwind holds ≥4.0× lcss ratio,
   interleaved both orders, ins/B leading.
5. **(c) Gate RED on mutation.** Mutate the kernel (or the rule's IR subtree, or the caller
   context); confirm the provenance digest diverges, `candidate.stale` flips true,
   `candidate_cost_stale_rate_bps > 0`, `selected_cost_freshness = "stale"`, and the gate refuses
   the coefficient (falls to `cascade_fallback_status = "fail-closed"`).
6. **Determinism.** `regen∘regen` byte-identical through the fitted extraction;
   `rewrite_order_variance_pct = 0`; `selection_trace_hash` stable across replays; no `from_env` on
   the regen path.

Pass = a fitted cyc/B coefficient flips a real per-site decision, wins cold-paired, and a mutation
drives the freshness gate RED — the loop demonstrated closed, context-keyed, integer, offline. A
documented negative result (no site flips) is the honest fallback and still advances the family by
fixing 2D's floor at "committed static table + freshness gate, no live fit."

---

## 11. Integration seam index (path:line)

- `crates/egraph/src/cost_weights.rs:175` `CALIBRATED_WEIGHTS` (**float**, fit-fallback target);
  `:134-167` the null-result guard; `:169-174` consumer contract; `:176-191` the eleven float dims.
- `crates/egraph/src/cost_config.rs:83` `from_env` — the env fence point (`BBNF_COST_*`, `:74-95`).
- `skinny/crates/passes/src/backend_egraph.rs:8` `FORMULA="sk-v13-w6-integer-v1"`; `:9,119`
  `REWRITE_SET`; `:21,170,284` `perf_cost: u32`; `:91-96,124` `stale_rate_bps`; `:100-107,138`
  `egraph_budget_status` (solver budget); `:127` `selected_cost_freshness`; `:139`
  `determinism_replay_status`; `:142` `selection_trace_hash`; `:145` `cascade_fallback_status`;
  `:169` `freshness_rank` — the provider seam.
- `skinny/crates/passes/src/decision_csp.rs:90,148-151` `NODE_BUDGET` / `csp_budget_status` (solver
  budget, NOT size knapsack); `:183` `static_profile_provider_status: "static-template-blocker"` —
  the blocker to replace.
- `skinny/crates/passes/src/lib.rs:369-373` `TargetFeatures { direct_only_output,
  direct_build_consumer }` — the caller contexts; `:954-957` `admits_sink_only` (context-sensitive
  predicate, Q4); `:524,661` its selection call sites; `:398,407` `derive_backend_shape[_with_diagnostics]`;
  `BackendShapePlan.cost_facts` — where the objective plugs in.
- `skinny/crates/ir/src/cost.rs:243` `static_profile_provider_status` (typed field); `:264-276`
  `PriorityStep` cascade (the objective supersedes on ties); `:289-315` `RejectedAlternative` +
  `Measurement { cycles_per_byte_x1000: Option<u64>, source, source_ref }`; `:319-323`
  `EvidenceSource::{BenchProbe, RedressBackfill, AuthorDeclared, StaticAnalysis}` — the
  per-alternative measured-evidence slot (integer, fixed-point).
- `skinny/crates/bbnf-simd/src/dispatch.rs:12` `SelectedBackend { Scalar, NeonTbl4 }`; `:45,89`
  `select_backend`; `:101` `lo6_table_admissible` — the live cost-shaped kernel switch, **runtime,
  not IR** (the relocation target).
- `skinny/crates/ir/src/lib.rs:425` `SimdMode { Exact }` (single variant at HEAD); `:430` `SimdSite
  { PreEntry }` — the IR enums LOCK-3 must widen to `Prefilter`/`SelfLoop` before the objective can
  range over a competing site.
- `docs/benchmarks/cost-weights-sweep.json` — the reusable sweep artifact.
