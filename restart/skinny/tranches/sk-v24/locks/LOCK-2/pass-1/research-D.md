# LOCK-2 · Family 2D — PROFILE-CLOSED-LOOP (research, pass 1)

**Thesis.** The cost model is not authored — it is *measured*. Cost coefficients are fitted
from cold paired canon-bench PMU captures, committed as a content-addressed fact table, and
read (never re-measured) by the decision solver at regen. The loop — measure → fit → commit →
solve → re-measure-to-validate — is the architecture; the solver's objective ceases to be a
hand-tuned float vector and becomes a regression estimate the corpus itself produced. This
family owns the *coefficient plane* under whatever candidate generator (2A islands, 2B
saturation, 2C CSP) sits above it: it is the answer to "where do the numbers in the cost
lattice come from, and why are they trustworthy and deterministic."

---

## 1. The mechanism made concrete

### 1.1 The dead dimension this family resurrects

The decision substrate already exists end to end but its perf dimension is **hardwired to
zero**. Every backend-shape candidate is minted with `perf_cost: 0`
(`skinny/crates/passes/src/lib.rs:590`; also `:2023`, `:2035`, `:2062`,
`decision_csp.rs:247`,`:259`). The only live cost dimensions are static-authored integers —
`candidate_static_size_cost` (a fixed per-shape rank, `lib.rs:603-611`) and
`candidate_capacity_cost` (a boolean, `lib.rs:599`). The e-graph extractor
(`backend_egraph.rs:239-267`, `DecisionCost` lattice) orders candidates lexicographically by
`(freshness, perf_cost, capacity, static_size, shape_rank, hash)` — so with `perf_cost ≡ 0`
the *performance* term is inert and selection is decided entirely by the hand-authored
`shape_rank` table (`lib.rs:566`,`:573`). The decision CSP downstream
(`decision_csp.rs:44-47`) then assigns cost `0.0` to the already-selected candidate and `1.0`
to every other — a **rubber stamp**, not an optimizer. Its own provenance field admits this:
`static_profile_provider_status: "static-template-blocker"` (`decision_csp.rs:183`) and
`row_move_toward_sota_status: "measured_architectural_block"` (`:186`). 2D is the removal of
that block.

### 1.2 The four-stage loop

1. **MEASURE.** The canon benches capture per-process PMU cyc/B via
   `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` (`json_canon_bench.rs:59`,`:68-80`; mirrored in
   `css_canon_bench.rs`). Today they emit **whole-grammar-lane** rows —
   `PMU corpus lane bytes iters cycles instructions cyc_per_byte cpi mbps`
   (`json_canon_bench.rs:193`,`:212`) — for lanes {tape, fused, sonic, serde}. 2D adds a
   **fact-emit mode** (`--emit-facts`) writing a decision-attributed sample stream: for each
   (grammar, corpus, rule-or-scan-site, chosen shape) a cyc/B + ins/B sample, cold paired,
   ≥3 windows, per-block loadavg recorded — the standing profiling law (SPEC §0/§4).
2. **FIT.** An offline xtask subcommand `calibrate-cost` runs a deterministic regression
   (ridge / closed-form OLS, fixed seed) from a structural feature vector
   `f(rule, shape)` — node count, alt-branch count, tape-push count, simd-scan-site count,
   the very knobs `CostWeights` already names — onto measured cyc/B, producing a coefficient
   vector β. Fit target is **relative ordering / paired ratios**, never absolute cyc/B (the
   law: ratios certified, absolutes directional — fitting absolutes fits load noise).
3. **COMMIT.** β is written to a committed, content-addressed table
   (`cost-coefficients.json`), its hash stamped into `cost_formula_version`
   (`ir/src/cost.rs:174`, today the literal `"sk-v13-w6-integer-v1"` in
   `backend_egraph.rs:8`). Precedent for committed profile fact tables already lives in-tree:
   `profile/yyjson/*.profile.json`. The table carries its training corpus + a held-out
   residual so the fit is auditable.
4. **SOLVE (at regen, deterministic).** Regen reads *only* the committed table — never live
   PMU — and evaluates `perf_cost = predict(f(rule,shape), β)` to populate the dimension at
   `lib.rs:590`. The decision CSP (`decision_csp.rs`) becomes a genuine minimizer: its
   `CostFiniteDomain::new(values, costs)` (`decision_csp.rs:49`) already accepts a per-value
   cost vector — 2D feeds it `predict(...)` instead of the `{0.0, 1.0}` stub. `re-measure`
   then validates the fit predicted the chosen winner; the prediction residual is the gate.

The loop **closes at calibrate-time, not regen-time.** This is the load-bearing determinism
invariant: regen is a pure function of (grammar, committed-β), so `regen∘regen = regen`
byte-identical (the fixpoint gate, precept 53) is preserved. A naive "re-measure at regen"
would make regen load-dependent and shatter the gate — the family's central discipline is
that measurement and regen are *separated in time* by the committed artifact.

### 1.3 Why fitting, not authoring

The single hand-authored coefficient slot, `CALIBRATED_WEIGHTS`
(`egraph/src/cost_weights.rs:175`), carries a documented **null calibration** (`:134-167`): a
54-configuration grid sweep (`scripts/cost-grid-sweep.sh`,
`docs/benchmarks/cost-weights-sweep.json`) over {structural, alt_per_branch, dispatch_bonus,
ref_cost} produced **identical** DTA state counts for every grammar (JSON 51, BBNF 476, CSS
2875, Sheets 191). The authors' own conclusion is the seam this family occupies: the doc says
there is "exactly one slot to update when a future calibration produces a non-trivial result"
(`:169-174`). 2D is that calibration — but done from *measured cyc/B*, not from DTA state
count (a static proxy), and fed into the *perf* dimension the grid sweep never touched.

---

## 2. The substrate (what already serves; consumer-in-wave)

| Concern | Extant substrate (path:line) | 2D mutation |
|---|---|---|
| Cost provenance schema | `ir/src/cost.rs:172` `ActiveCostFacts` (`cost_formula_version`, `selected_cost_freshness`, `determinism_replay_status`, `selection_trace_hash`) | schema fields already serde; add `coefficient_table_hash`, `fit_residual_bps`, `training_corpus_id` |
| CSP provenance | `ir/src/cost.rs:203` `DecisionCspFacts` (`static_profile_provider_status:183`, `row_move_toward_sota_status:186`) | flip `"static-template-blocker"` → `"fitted-coefficient-table-vN"` |
| Perf dimension | `passes/src/lib.rs:590` `perf_cost: 0` | `perf_cost: predict(f(rule,shape), β)` |
| Solver objective | `passes/src/decision_csp.rs:44-49` `{0.0,1.0}` stub into `CostFiniteDomain` | real per-candidate `predict(...)` cost |
| Coefficient slot | `egraph/src/cost_weights.rs:175` `CALIBRATED_WEIGHTS`; `cost_config.rs:83` `from_env` (`BBNF_COST_*`) | read the committed table; keep env override for sweeps |
| Profile-hint seam | `passes/src/lib.rs:1037` `PriorBenchProfile { hot_rule_names }`, consumed `:1052-1058` | widen to `CostProfile { coefficients, per_site_samples }` |
| Measurement source | `bbnf-bench/src/bin/json_canon_bench.rs:59`,`:185`; `css_canon_bench.rs` | add `--emit-facts` decision-attributed sample writer |
| Fit / commit host | `skinny/xtask/src/main.rs` dispatch | new `calibrate-cost` subcommand; committed table dir |

**Consumer-in-wave (constitution viii / Y.13).** The committed coefficient table is
forbidden as consumer-less substrate: it lands in the *same* wave as the `perf_cost`
activation at `lib.rs:590` and the CSP objective at `decision_csp.rs:49` that reads it. A
table that ships while `perf_cost` stays `0` is precisely the dead-substrate pathology the
constitution forbids. The wire is four hops — bench fact stream → fit → committed β → CSP
reads `predict(...)` — all closing in-wave, mirroring the GN-2.W1 four-hop discipline.

---

## 3. Prior art (with citations)

- **AutoFDO** (Chen, Li, Hundt, *CGO 2016*, "AutoFDO: Automatic Feedback-Directed
  Optimization for Warehouse-Scale Applications"). Sample-based FDO: hardware PMU samples,
  not instrumentation, mapped back to source; the compiler re-optimizes reading a committed
  profile. This is the exact template — PMU samples → committed profile → deterministic
  re-compile — and its "samples not instrumentation" stance matches our
  `proc_pid_rusage` cyc/B source.
- **LLVM/GCC PGO** and **BOLT** (Panchenko et al., *CGO 2019*). PGO's `.profdata` is a
  committed fact file; recompilation is a pure function of (source, profile). Our departure:
  we commit *fitted coefficients*, not raw counts, so regen stays byte-deterministic and
  small — the profile does not have to ship at regen granularity.
- **egg / equality saturation** (Willsey et al., *POPL 2021*, "egg: Fast and Extensible
  Equality Saturation"). The extractor picks a min-cost representative — but egg's stock cost
  is *static* (`AstSize`, `AstDepth`). Our family is the measured-cost extension the crate's
  `Extractor`/`CostModel` seam (`backend_egraph.rs:83-88`, `DecisionCostModel:259-267`)
  already admits.
- **TASO / equality saturation with measured cost** (Jia et al., *SOSP 2019*; Yang et al.,
  MLSys). Tensor-graph superoptimization extracts by *actual measured runtime*, not a static
  proxy — direct precedent that measured cost is a legitimate e-graph extraction objective.
- **Ithemal** (Mendis et al., *ICML 2019*) and **uiCA / llvm-mca** — basic-block throughput
  predictors. These are the `predict(features)` component; our linear analog is deliberately
  simpler and deterministic (closed-form β, no learned network), fitted to whole-parse cyc/B
  rather than per-BB throughput because macOS PMU is process-granular (§4).
- **regex-automata prefilter selection** (BurntSushi). Literal/prefilter tier choice uses
  *hand-tuned rarity heuristics*, not fitted coefficients — a named contrast: the same
  decision could be profile-fitted and is not, which is exactly the seam LOCK-2's prefilter
  lowering (GN-2.W1) opens.

---

## 4. Named risks & counterexamples

1. **The null-calibration trap (measured, in-tree).** `cost_weights.rs:134-167` is proof that
   a fitted cost model moves **nothing** when the candidate set has no genuinely distinct
   alternatives — the rewrite set produced one canonical form, so the objective had no choice
   to exercise. 2D is inert without candidate diversity. **This is the family's central
   refutation risk**: a beautifully fitted table that flips zero decisions. 2D therefore has a
   hard dependency on the candidate generator (2A islands / 2B saturation / 2C variables)
   producing legally-distinct shapes per site; alone it is the coefficient plane of an empty
   choice.
2. **Attribution underdetermination.** The bench emits *whole-grammar* cyc/B
   (`json_canon_bench.rs:193`), not per-rule. Fitting per-decision β from whole-parse cyc/B
   over 5 grammars × ~3 corpora is an underdetermined regression (dozens of rule/shape
   features, ~15 equations) → β unidentifiable. **Counterexample:** 40 JSON rules, 3 corpora
   cannot separate per-rule coefficients. The prototype must decide between (a) **A/B
   differential** per decision (compile both legal shapes for a site, diff whole-parse cyc/B
   cold-paired — clean, load-robust, but O(candidate) compiles), or (b) **shape-labeled
   sub-parse probes** (finer attribution, more harness). (a) is the honest primitive; the
   paired-interleave method is already proven in-tree.
3. **Determinism vs. measurement (fixpoint gate).** If the solver reads live PMU at regen,
   regen becomes load-dependent and `regen∘regen ≠ regen`. Guard: fit is a *separate* offline
   step; regen reads only the committed β. The committed table's hash is the determinism
   anchor (`cost_formula_version`); `determinism_replay_status` (`cost.rs:193`) already
   tracks replay. Counterexample to refuse: any design that re-measures inside `xtask regen`.
4. **Corpus overfit.** β fitted on twitter/canada/citm may mis-rank a shape for TOML/CSV
   (grammars with no committed corpus yet — R1 §6). Guard: cross-grammar held-out residual
   committed beside β; a shape flip must survive on a grammar *outside* the fit set.
5. **Refutation laws bind the measurement.** No warm benches (SPEC §0) — samples are cold
   per-parse. Ratios certified / absolutes directional (precepts 43-49) — the fit target is
   relative ordering, not absolute cyc/B. macOS `ri_cycles` is per-process — no per-BB
   counters without Instruments/xctrace (`xctrace_probe.rs` exists but is coarser); the
   attribution ceiling is process-granular, which is *why* the A/B-differential primitive
   (risk 2a) is the safe default.

---

## 5. What a prototype must test (the family-mover)

**PROVE-2D-A — does a measured coefficient table flip any decision, as a measured win?**
Take the live fleet (json/css/sheets, + csv/toml where a grammar exists). For every rule/scan
site with ≥2 *legal* backend shapes, A/B-measure the candidate shapes (compile each legal
shape, cold-paired whole-parse cyc/B, ≥3 windows). Fit β. Populate `perf_cost` at
`lib.rs:590`; re-run the decision CSP (`decision_csp.rs`). **Gate:** ≥1 rule's selected shape
*flips* relative to the hand-`shape_rank` baseline **AND** the flip is a measured cold-paired
win on a real grammar, **AND** regen is byte-identical across the committed table on double
regen. **Failure semantics (honest):** zero flips → 2D is SPEC-ONLY until candidate diversity
exists; the negative result is documented (the null-calibration recurrence), and the family's
disposition becomes "coefficient plane, blocked on 2A/2B/2C candidate generation" — a named
dependency, not a silent pass.

**PROVE-2D-B — determinism & load-robustness of the fit.** Fit under quiet load and under
loadavg 13-19 (the PROVE-12 regime); the committed β must be identical (fit on paired
medians/ratios, load-invariant). Regen twice → byte-identical. Report the prediction residual
(predicted-winner vs measured-winner) as the standing accuracy metric.

**The minimal move:** even one profile-fitted decision that (a) flips, (b) wins cold-paired,
(c) leaves regen byte-identical is the concrete artifact that converts `perf_cost: 0` and
`static-template-blocker` from an architectural admission into a measured closed loop — and
if it does not flip, PROVE-2D-A's negative result is the equally-load-bearing finding that
2D's value is gated on the candidate generator, naming its exact prerequisite.
