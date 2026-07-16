# LOCK-2 · FAMILY 2D — PROFILE-CLOSED-LOOP (research-D)

**Thesis.** Optimization is a measured feedback loop, not a one-shot cost guess. Per-feature
cost coefficients are *fitted* from cold-paired PMU captures (cyc/B), *committed* as a
determinism-preserving integer fact table, and *consumed* by the decision driver, which
re-runs at regen. A provenance-stamped **freshness gate** goes RED when the grammar or kernel
the coefficient describes mutates out from under it. The loop — capture, fit, commit, select,
re-measure — is the architecture; the cost model is not a hand-tuned constant but a
regenerable artifact.

This family's distinguishing claim, and its hardest constraint, is that the loop must earn a
*real decision flip* against the reigning null result. It does not on the surface the extant
engine exposes. Family 2D's central discovery is that the profile loop must be *relocated* from
the (inert) e-graph extraction weights onto the (live) discrete kernel crossover thresholds —
and once relocated, the entire fact-table + freshness scaffold the engine already carries
becomes load-bearing.

---

## 1. The substrate already exists — inert — and names its own blocker

The engine ships the *complete shape* of a profile-closed loop, wired and populated, but fed by
hand-set integers and a placeholder provider. This is the family's greatest asset: the seams are
cut, the fact fields are typed, the determinism replay runs. What is missing is the profile.

- **The committed cost fact table.** `crates/egraph/src/cost_weights.rs:175` `CALIBRATED_WEIGHTS`
  — a `const CostWeights` with twelve dimensions (`structural`, `alt_per_branch`,
  `dispatch_bonus`, `call_overhead`, `inline_body_size_penalty`, `tape_push`, `dispatch_branch`,
  `dispatch_table`, `prettify_emission`, `cross_module_coercion`). This IS the committed table a
  fit would supersede. Every production consumer reads it via `CostWeights::default()`
  (`:112`), so there is "exactly one slot to update when a future calibration produces a
  non-trivial result" (the file's own consumer contract, `:169`).

- **The null result that guards the door** (`cost_weights.rs:134-167`). A 54-configuration grid
  sweep over `{structural, alt_per_branch, dispatch_bonus, ref_cost}` on the 4-grammar corpus
  (JSON/BBNF/CSS-L4/Sheets) produced **zero** DTA-state-count variance: JSON stayed at 51 states,
  CSS-L4 at 2875, Sheets at 191, BBNF at 476, across *all* 54 configs. The current rewrite set
  (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
  `CommonSuffixFactor`) "produces e-classes with at most one semantically-distinct canonical form
  … so the cost model has no choice to exercise." Extraction multiplicity ≈ 1. **A profile-fed
  extraction coefficient flips nothing.** (The sweep artifact `docs/benchmarks/cost-weights-sweep.json`
  is present; the driver script named in the doc header is absent — the JSON is the reusable
  evidence.)

- **The live freshness scaffold.** `skinny/crates/passes/src/backend_egraph.rs` computes, per
  rule, a real `ActiveCostFacts`: `cost_formula_version = FORMULA` where `FORMULA =
  "sk-v13-w6-integer-v1"` (`:8` — the cost is *integer*, hand-set);
  `candidate_cost_stale_rate_bps` from the stale-candidate fraction (`:91-96,124`);
  `selected_cost_freshness = "fresh"` iff `selected.score.freshness_rank == 0` (`:127`), where
  `freshness_rank = u32::from(candidate.stale)` (`:169`); `determinism_replay_status = "pass"`
  with `rewrite_order_replay_count = 2`, `rewrite_order_variance_pct = 0` (`:143-146`); and
  `selection_trace_hash = stable_hash_hex(&trace)` (`:147`) — a deterministic tie-break trace.
  The `DecisionCost` carries an integer `perf_cost` (`:170`).

- **The named blocker.** `skinny/crates/passes/src/decision_csp.rs:183`
  `static_profile_provider_status: "static-template-blocker"`. At HEAD there is *no* profile
  provider — the field declares it blocked. Family 2D's entire mechanism is: **replace the
  static-template-blocker with a real fact-table-backed provider**, wire it into `perf_cost` and
  `candidate.stale`, and keep `determinism_replay_status` green.

- **The evidence slot on every rejected alternative.**
  `skinny/crates/ir/src/cost.rs:305` `Measurement { cycles_per_byte_x1000: Option<u64>, source:
  EvidenceSource, source_ref: String }`, with `EvidenceSource { BenchProbe, RedressBackfill,
  AuthorDeclared, StaticAnalysis }` (`:318`). The IR *already* models per-alternative measured
  cyc/B as **fixed-point integer** (×1000) — the determinism-safe encoding is a decision the
  substrate already made. Today the option is `None`; family 2D fills it with `BenchProbe` facts
  whose `source_ref` is the fact-table commit.

**Reading:** the loop is scaffolded end to end; only the profile is a stub. The work is not to
invent the architecture but to *feed and gate* it — and to move its objective onto a surface
where a fitted coefficient has a decision to flip.

---

## 2. The mechanism made concrete — the five-stroke loop

1. **CAPTURE.** For each *competing site* (§3), run the standing cold-paired PMU protocol
   (`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, N≥200, ≥3 windows, per-block loadavg, ins/B as
   the load-invariant check) at **real value-start positions** on the fleet corpora — not
   synthetic buffers. Emit cyc/B per candidate lowering.

2. **FIT.** Solve the small linear system relating whole-grammar cyc/B to per-feature coefficients
   by **A/B-differential isolation** (§4): each paired binary differs in exactly one feature; the
   cyc/B delta *is* that feature's coefficient. Quantize to `u64` fixed-point (×1000), matching
   `Measurement.cycles_per_byte_x1000`.

3. **COMMIT.** Write the fitted coefficients as a fact table — the successor to
   `CALIBRATED_WEIGHTS` and to `FORMULA = "sk-v13-w6-integer-v1"` — stamped with a **provenance
   hash** over `{grammar-IR digest, kernel-set digest, formula version, corpus set}`. The commit
   is a reviewed source artifact, never an env read.

4. **SELECT.** `backend_egraph.rs` reads the table: `perf_cost` ← fitted coefficient;
   `candidate.stale` ← provenance-hash mismatch. The decision driver re-runs at regen; the trace
   hash certifies determinism. HashMap iteration is inadmissible — extraction ties break on the
   total order `(perf_cost asc, freshness_rank asc, candidate_id lex)` sealed by
   `selection_trace_hash`.

5. **RE-MEASURE / GATE.** A follow-on capture on the emitted parser confirms the fit predicted the
   winner; on mutation the provenance hash diverges, `stale=true`, `stale_rate_bps` rises,
   `selected_cost_freshness = "stale"`, and the gate is RED — the loop demands a recapture before
   that site's coefficient is trusted again.

The loop is **offline** (bounded to regen), not online — the deliberate departure from
database-style continuous learning optimizers (§7): determinism and byte-identical regen forbid a
coefficient that drifts between compiles.

---

## 3. The decision surface must be RELOCATED (the load-bearing move)

The null result (§1) is fatal *only* to profiling the e-graph extraction weights. The profile
loop's real objective is the set of **discrete kernel crossover thresholds** where two lowerings
genuinely compete and the winner is corpus-dependent — precisely the surface where cyc/B, not
structure, decides:

- **Classifier backend switch.** `skinny/crates/bbnf-simd/src/dispatch.rs:12`
  `enum SelectedBackend { Scalar, NeonTbl4 }`; `:89` `fn select_backend`; `:101`
  `fn lo6_table_admissible`. A real, live, cost-shaped dispatch: `NeonTbl4` iff the alphabet
  admits a lo6 table, else `Scalar`. The admissibility predicate is *structural* today; a fitted
  coefficient makes it *cost-conditioned* — admit the table only where its measured cyc/B beats
  scalar on the fleet.

- **Prefilter gate-16 crossover.** The skip-heavy/dense split is empirically ~7.5× vs
  ~1.03–1.05×; the crossover between prefilter-skip and membership-scan is a *measured* function
  of the corpus's inert-run-length distribution at value-start positions. A fit sets the gate per
  grammar rather than by a single hand constant.

- **CSS short-span guard.** `CSS_SHUFTI_MIN_SPAN` — the span below which the vector classify is not
  amortized and the scalar walk wins — is explicitly "a cost-model value, not a heuristic literal."
  animate.css (keyframe-dense, 5–10-byte components) regresses +13.4% cyc/B / +29.9% ins under an
  un-guarded vector classify while tailwind wins 4.16×: **the same kernel, opposite verdicts,
  decided by span length.** The canonical corpus-dependent flip a fitted `MIN_SPAN` resolves.

- **The N\* kernel crossover** (the empirical kernel-length threshold) and the `PriorityStep`
  cascade `P1..P8` (`skinny/crates/ir/src/cost.rs:264`) — an 8-step *precondition* ladder a cost
  objective would supersede where the preconditions tie.

**Seam to plug the objective in:** `skinny/crates/passes/src/lib.rs:398`
`derive_backend_shape` → `:407` `derive_backend_shape_with_diagnostics` →
`BackendShapePlan.cost_facts` (`:390-392,97`). The fitted coefficient enters as the `perf_cost`
that `backend_egraph.rs` extracts over, and as `BenchProbe` `Measurement` evidence on each
`RejectedAlternative` (`ir/src/cost.rs:289,306`).

**Consequence for the family:** 2D succeeds *iff* it targets these crossovers, and inherits the
null result if it targets extraction weights. In the tower-ownership frame ("at which decision
point"), the profile objective lives at the **kernel/backend-selection decision point**,
downstream of canonicalization, where multiplicity is genuinely ≥2 because two kernels *both*
recognize the same language at different measured cost.

---

## 4. Attribution — A/B-differential, because whole-grammar cyc/B underdetermines

A single whole-grammar cyc/B number is one equation in many unknowns (`tape_push`,
`dispatch_branch`, `call_overhead`, …): the per-feature system is underdetermined. Two disciplines
resolve it, both already exercised by the fleet's measurement corpus:

- **One-lever paired isolation.** Build two binaries differing in *exactly one* feature and
  interleave them back-to-back per window; the cyc/B delta is that feature's coefficient. This is
  how the const-mask residualization win was attributed (baseline vs modified, identical hunks,
  ins/B −4.8/−6.8% deterministic, cyc/B ratio 0.81–0.98× per corpus) and how the CSS tokenizer's
  animate regression was isolated from its tailwind win (flag ON/OFF, same binary family). The fit
  is a **least-squares over the one free variable per pair** — not a global regression over a
  confounded whole.

- **ins/B as the load-invariant anchor.** cyc/B carries host-load noise; ins/B agrees across
  windows to <0.2% and is the certified floor. The fit is stated in cyc/B (the decision metric)
  but *validated* against the deterministic ins/B delta — a coefficient whose ins/B moves the
  wrong way is rejected regardless of a favorable cyc/B window.

This mirrors the automated-microbenchmark methodology of **uops.info** (§7): isolate the quantity
under measurement in a controlled harness, measure it directly rather than infer it from a
confounded aggregate, and emit a machine-readable table. The grammar-feature analogue of
"per-instruction latency" is "per-feature cyc/B"; the analogue of its generated microbenchmark is
the paired one-lever fleet binary.

---

## 5. The freshness gate — provenance hashing, the AutoFDO discipline

A committed coefficient describes a *specific* grammar rule lowered by a *specific* kernel under a
*specific* cost formula. When any mutates, the coefficient is stale and must not silently steer a
decision. The engine already carries the anchors:

- `candidate.stale: bool` → `freshness_rank` → `selected_cost_freshness ∈ {"fresh","stale"}`
  (`backend_egraph.rs:127,169`).
- `candidate_cost_stale_rate_bps` — the fraction of ranked candidates whose coefficient is stale,
  in basis points (`:91-96`).
- `cost_formula_version = FORMULA` (`:8,117`) and `rewrite_set_id = REWRITE_SET` (`:118`) — the
  formula/rewrite provenance stamps.
- `selection_trace_hash` + `determinism_replay_status` (`:143-147`) — the replay certifies the
  *same* inputs produce the *same* selection (2 replays, 0% variance).

**Mechanism.** Stamp each fact-table entry with `provenance = stable_hash(grammar-IR digest ∥
kernel-set digest ∥ FORMULA ∥ corpus digest)`. At regen, recompute the digest from the current IR
and kernels; `candidate.stale = (stored_provenance != recomputed_provenance)`. The gate is RED
when `stale_rate_bps > 0` on any site whose decision the table steers. This is exactly LLVM
AutoFDO/CSSPGO **stale-profile matching**: a profile collected against an older revision is
detected by hash-matching anchors (loose then strict hash), and a mismatch triggers conservative
fallback rather than a wrong annotation. Our anchor is the IR/kernel digest; our fallback is the
hand-set `CALIBRATED_WEIGHTS` / structural `PriorityStep` cascade (`cascade_fallback_status =
"fail-closed"`, `backend_egraph.rs:148`).

**Determinism invariant.** `perf_cost` is *integer* (`FORMULA = "…integer-v1"`) precisely so the
extraction order is byte-stable — no float-hash nondeterminism. The fitted cyc/B enters as `u64`
×1000 fixed-point (the `Measurement.cycles_per_byte_x1000` encoding). The total-order tie-break
`(perf_cost, freshness_rank, candidate_id)` is what makes `regen∘regen` byte-identical through
cost-guided extraction; HashMap iteration order never touches the selection.

---

## 6. The env fence — the profile must never leak into regen

`crates/egraph/src/cost_config.rs:83` `CostConfig::from_env` reads `BBNF_COST_*`
(`BBNF_COST_STRUCTURAL`, `BBNF_COST_ALT_PER_BRANCH`, `BBNF_COST_DISPATCH_BONUS`,
`BBNF_COST_EGRAPH_ITER_LIMIT`, …). This surface is for **benchmarking the sweep only.** If an env
var could reach the emitter, regen would cease to be byte-deterministic — a fatal violation of the
generated-files-clean-regen law.

**The fence is the commit.** The pipeline is strictly:
`env override → sweep harness → proposed fitted table → human review → committed const
(CALIBRATED_WEIGHTS successor) → regen`. Regen reads *only* the committed const, never `from_env`.
The provenance hash (§5) is computed over the *committed* table and the IR/kernels — it explicitly
excludes env, so no env value can alter the freshness verdict either. `from_env` is a proposal
instrument; the reviewed source diff is the only path into regen. (Same posture as the const-mask
residualization: the transform is done at generation time and `xtask regen` reproduces the file
bit-for-bit — env participates in *choosing* the constant, never in *emitting* it.)

---

## 7. Prior art (with citations)

- **uops.info** — Abel & Reineke, ASPLOS '19 (arXiv:1810.04610). Automatically generated
  microbenchmarks yield machine-readable per-instruction latency/throughput/port tables "to
  facilitate the rapid construction of optimizing compilers." The direct precedent for
  *measured, committed, machine-readable cost fact tables*; our A/B-differential paired fit is the
  grammar-feature analogue of its per-instruction isolation.
- **LLVM AutoFDO / CSSPGO / Stale-Profile Matching** — Chen et al. CGO '16; Ayupov & Panchenko,
  Meta (arXiv:2401.17168, LLVM D147456). Multi-level hash matching (loose→strict) with
  flow-conservation inference detects profiles collected against stale binaries; pseudo-instrumentation
  anchors link code to samples so PGO "sees through source changes." The precedent for the
  **freshness gate**: provenance-anchored staleness detection with conservative fallback.
- **LLVM `SchedMachineModel` (TableGen `.td`)** — committed per-microarchitecture latency/uop
  tables consumed by the scheduler / `llvm-mca`. The precedent for the *deterministic,
  hand-committable* form of a cost table — contra learned throughput predictors (**Ithemal**,
  Mendis et al. ICML '19), which the determinism law forbids.
- **DB2 LEO — the LEarning Optimizer** — Stillger et al., VLDB '01. A feedback loop correcting a
  cost model from measured execution. The closed-loop precedent — but *online*; we take the loop
  shape and bound it to *offline regen* to preserve byte-identity.
- **Halide autoscheduler** (Adams et al., SIGGRAPH '19) and **TVM Ansor** (Zheng et al., OSDI '20)
  — fitted cost models ranking discrete schedules from on-device measurement: the canonical
  measure→fit→rank→re-measure loop. We adopt the loop, reject the model class
  (learned/non-deterministic) in favor of a committed integer table.
- **egg `LpExtractor` / `CostFunction`** (Willsey et al., POPL '21) — extraction is cost-ranked
  (greedy or ILP), but the cost function is *static and user-supplied*. Family 2D's contribution
  over egg is that the cost function is a *regenerable measured artifact* with a staleness gate,
  not a fixed `AstSize`.

---

## 8. Named risks & counterexamples

- **R1 — The null result generalizes (the family-killer).** If, after relocation to the crossover
  thresholds (§3), *no* fleet site presents ≥2 kernels with binding, corpus-dependent cyc/B
  ordering, the profile loop still has nothing to flip and 2D collapses to a static table.
  **Counterexample that saves it:** CSS shufti vs scalar on animate (loss) vs tailwind (win) —
  same kernel, opposite verdicts by span length — is a *proven* corpus-dependent flip. The
  prototype (§9) must exhibit at least one such flip end-to-end or return a documented negative
  result (the honest disposition: 2D degenerates to a committed static table with a freshness gate
  but no live fit — still a valid, smaller family).
- **R2 — Attribution collapse.** If a paired binary cannot isolate a single feature (two features
  co-vary because the emitter fuses them), the fit is confounded and the coefficient is a fiction.
  **Mitigation:** require a genuine one-lever diff (template edit touching exactly one lowering);
  refuse a fit whose ins/B delta is inconsistent with its cyc/B delta.
- **R3 — Freshness false-negative.** A digest that hashes too coarsely (whole-grammar) marks every
  site stale on any edit — gate permanently RED, useless. Too fine (per-byte) and a
  semantically-irrelevant reformat trips it. **Mitigation:** hash the *lowered kernel identity +
  the rule's IR subtree*, not the source text — the AutoFDO CFG-anchor lesson (probe/CFG carriers
  see through cosmetic source change).
- **R4 — Determinism leak via float.** Any float in `perf_cost` or its hashing reintroduces
  iteration-order nondeterminism. **Mitigation:** the ×1000 fixed-point `u64` encoding is
  mandatory and already present; the integer FORMULA is not a limitation, it is the invariant.
- **R5 — Env leak.** A careless `from_env` on the regen path silently de-determinizes the build.
  **Mitigation:** the fence of §6 — `from_env` reachable only from the sweep harness, plus a
  grep-gate (zero `from_env` in codegen/regen paths), mirroring the zero-name-gate CI.
- **R6 — Budget.** The provider must not blow the per-parse compile budget (~230 µs order). The fit
  is *not* per-parse — it runs at *regen* (offline); at compile/parse time the provider is a table
  lookup keyed by the provenance digest, cached on the rule.

---

## 9. What a prototype must test to move the family

A single end-to-end prototype on one competing site — strongest candidate: **CSS shufti vs scalar
crossover**, whose corpus-dependent flip is already proven:

1. **Fit.** Sweep `CSS_SHUFTI_MIN_SPAN` (or the `lo6_table_admissible` cost condition), capture
   cold-paired cyc/B at real value-start positions across animate/tailwind/bootstrap/material, fit
   the crossover span as a `u64` fixed-point coefficient.
2. **Commit + wire.** Emit the coefficient as a fact-table entry with a provenance hash; replace
   `static_profile_provider_status = "static-template-blocker"` with a provider setting `perf_cost`
   and `candidate.stale` from it; confirm `derive_backend_shape` selects through it.
3. **(a) Flip.** Show the fitted coefficient selects a *different* `SelectedBackend`/guard than the
   hand default on ≥1 corpus (animate → scalar guard, tailwind → vector) — a real per-site decision
   flip, not a re-derivation of the same choice.
4. **(b) Win cold-paired.** The fitted decision beats the hand default cold-paired at real
   value-start positions: animate returns to ≤1.0× and tailwind holds ≥4.0× lcss ratio, interleaved
   both orders, ins/B leading.
5. **(c) Gate RED on mutation.** Mutate the kernel (or the rule's IR subtree); confirm the
   provenance digest diverges, `candidate.stale` flips true, `candidate_cost_stale_rate_bps > 0`,
   `selected_cost_freshness = "stale"`, and the freshness gate refuses the coefficient (falls to
   `cascade_fallback_status = "fail-closed"`).
6. **Determinism.** `regen∘regen` byte-identical through the fitted extraction;
   `rewrite_order_variance_pct = 0`; `selection_trace_hash` stable across replays; no `from_env` on
   the regen path.

Pass = a fitted cyc/B coefficient flips a real per-site decision, wins cold-paired, and a mutation
drives the freshness gate RED — the loop demonstrated closed. A documented negative result (no site
flips) is the honest fallback, and it too advances the family by fixing 2D's floor at "committed
static table + freshness gate."

---

## 10. Integration seam index (path:line)

- `crates/egraph/src/cost_weights.rs:175` `CALIBRATED_WEIGHTS` — the committed table (fit target);
  `:134-167` the null-result guard; `:112` `Default` → consumer contract.
- `crates/egraph/src/cost_config.rs:83` `from_env` — the env fence point (`BBNF_COST_*`).
- `skinny/crates/passes/src/backend_egraph.rs:8` `FORMULA="sk-v13-w6-integer-v1"`; `:91-96`
  `stale_rate_bps`; `:117-147` `ActiveCostFacts` construction; `:169` `freshness_rank`; `:170`
  `perf_cost` — the provider seam.
- `skinny/crates/passes/src/decision_csp.rs:183` `static_profile_provider_status:
  "static-template-blocker"` — the blocker to replace.
- `skinny/crates/ir/src/cost.rs:305-316` `Measurement { cycles_per_byte_x1000, source, source_ref }`
  + `:318` `EvidenceSource::BenchProbe` — the per-alternative measured-evidence slot; `:173`
  `ActiveCostFacts` type; `:243` `static_profile_provider_status`; `:264` `PriorityStep` (the
  cascade an objective supersedes).
- `skinny/crates/passes/src/lib.rs:398,407` `derive_backend_shape[_with_diagnostics]`;
  `:390-392,97` `BackendShapePlan.cost_facts` — where the objective plugs in.
- `skinny/crates/bbnf-simd/src/dispatch.rs:12` `SelectedBackend`; `:89` `select_backend`; `:101`
  `lo6_table_admissible` — the live cost-shaped kernel switch (the relocation target).
- `docs/benchmarks/cost-weights-sweep.json` — the reusable sweep artifact.
