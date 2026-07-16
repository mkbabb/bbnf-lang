# LOCK-2 · Family 2D — PROFILE-CLOSED-LOOP (research, pass 1)

**Thesis.** The cost model is not authored — it is *measured*. Cost coefficients are fitted from
cold paired canon-bench PMU captures, committed as a content-addressed fact table, and read
(never re-measured) by the decision solver at regen. The loop — measure → fit → commit → solve →
re-measure-to-validate — is the architecture; the solver's objective ceases to be a hand-tuned
float vector and becomes a regression estimate the corpus itself produced. This family owns the
*coefficient plane* under whatever candidate generator (2A islands, 2B saturation, 2C CSP) sits
above it: it answers "where do the numbers in the cost lattice come from, and why are they
trustworthy and deterministic." The differentiator no parser generator holds: its profiler and its
cost model are one circuit — PEG/LR emit fixed automata (no profile), simdjson's thresholds are
magic constants (hand-tuned, not fitted).

---

## 1. The mechanism made concrete — the loop is BUILT but OPEN

Every organ of the loop exists in-tree, wired to a **stub**. The family's whole content is closing
the cut wires.

### 1.1 The dead perf dimension this family resurrects

The decision substrate exists end to end but its perf dimension is **hardwired to zero**. Every
backend-shape candidate is minted with `perf_cost: 0` (`passes/src/lib.rs:590`; also `:2023`,
`:2035`, `:2062`, `decision_csp.rs:247`,`:259`). The only live cost dimensions are static-authored
integers — `candidate_static_size_cost` (a fixed per-shape rank, `lib.rs:603-611`) and
`candidate_capacity_cost` (a boolean, `lib.rs:599`). The e-graph extractor
(`backend_egraph.rs:239-267`, `DecisionCost` lattice) orders candidates lexicographically by
`(freshness, perf_cost, capacity, static_size, shape_rank, hash)` — so with `perf_cost ≡ 0` the
*performance* term is inert and selection is decided entirely by the hand-authored `shape_rank`
table (`lib.rs:566`,`:573`) and the 8-step `PriorityStep` cascade (`ir/src/cost.rs:264-286`). The
decision CSP downstream (`decision_csp.rs:44-49`) then assigns cost `0.0` to the already-selected
candidate and `1.0` to every other — a **rubber stamp**, not an optimizer. Its own provenance
admits this: `static_profile_provider_status: "static-template-blocker"` (`decision_csp.rs:183`)
and `row_move_toward_sota_status: "measured_architectural_block"` (`:186`). 2D is the removal of
that block.

### 1.2 The four-stage loop

1. **MEASURE.** The canon benches capture per-process PMU cyc/B via
   `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` (`json_canon_bench.rs:59`,`:68-80`; mirrored in
   `css_canon_bench.rs`), paired interleaving, N≥200, ins/B as the load-invariant check — exactly
   the numbers, in exactly the discipline the constitution mandates (SPEC §0/§4). Today they emit
   **whole-grammar-lane** rows (`json_canon_bench.rs:193`,`:212`) for lanes {tape, fused, sonic,
   serde}. 2D adds a **decision-attributed** fact-emit mode (`--emit-facts`): per
   (grammar, corpus, rule-or-scan-site, chosen knob) a cyc/B + ins/B sample, cold paired, ≥3
   windows, per-block loadavg recorded.
2. **FIT.** An offline `xtask calibrate` runs a *deterministic* regression (closed-form OLS/ridge,
   fixed seed) from a structural feature vector `f(rule, shape)` — node count, alt-branch count,
   tape-push count, simd-scan-site count: the very knobs `CostWeights` already names
   (`cost_weights.rs:40-110`) — onto measured cyc/B, producing coefficient vector β. Fit target is
   **relative ordering / paired ratios**, never absolute cyc/B (the law: ratios certified,
   absolutes directional — fitting absolutes fits load noise).
3. **COMMIT.** β is written to a committed, content-addressed table (`profile/coefficients.ron` or
   a per-grammar sidecar under the `generated_root`), its hash stamped into `cost_formula_version`
   (replacing the literal `FORMULA = "sk-v13-w6-integer-v1"`, `backend_egraph.rs:8` →
   `ActiveCostFacts.cost_formula_version`, `cost.rs:174`). **Precedent for committed profile fact
   tables already lives in-tree: `profile/yyjson/*.profile.json`.** The table carries its training
   corpus id + a held-out residual so the fit is auditable, keyed by
   `(host_triple, grammar, site_class, knob_id)`.
4. **SOLVE (at regen, deterministic).** Regen reads *only* the committed table — never live PMU —
   and evaluates `perf_cost = predict(f(rule,shape), β)` to populate the dead dimension at
   `lib.rs:590`. The decision CSP becomes a genuine minimizer: `CostFiniteDomain::new(values,
   costs)` (`decision_csp.rs:49`) already accepts a per-value cost vector — 2D feeds it
   `predict(...)` instead of the `{0.0, 1.0}` stub.

**The loop closes at calibrate-time, not regen-time — the load-bearing determinism invariant.**
Regen is a pure function of `(grammar, committed-β)`, so `regen∘regen = regen` byte-identical (the
fixpoint gate, precept 53) is preserved. All load noise, window variance, and host jitter live in
`xtask calibrate`, off the regen path. A naive "re-measure at regen" makes regen load-dependent
and shatters the fixpoint — the family's central discipline is that measurement and regen are
*separated in time by the committed artifact*. This is precisely the PGO/AutoFDO/BOLT discipline:
the profile is committed data, the build reads it and is reproducible.

### 1.3 Freshness becomes falsifiable, not a stamped string

Today `selected_cost_freshness == "fresh"` iff `candidate.stale == false` (a static bool,
`backend_egraph.rs:127-131`); `determinism_replay_status: "pass"`, `rewrite_order_variance_pct: 0`
(`:139-141`) are hardcoded literals. Yet a real GATE already reads them:
`bbnf-bench/src/report.rs:1742-1758` REJECTS a measurement row unless `cost_formula_version` is
non-empty AND `selected_cost_freshness == "fresh"` AND `determinism_replay_status == "pass"`. The
enforcement machinery is built; it presently checks a string equals a hardcoded string. A third
verb, **`xtask check-calibration`**, recomputes `hash(coefficients.ron)`, asserts
`cost_formula_version == hash(...)`, and asserts the last regen consumed the current table — so
mutating any coefficient diverges the hash and drives the freshness gate RED. The stub fields
become outputs of a re-run-and-diff, not literals.

### 1.4 Why fitting, not authoring — the null-calibration inheritance

The single hand-authored coefficient slot, `CALIBRATED_WEIGHTS` (`egraph/src/cost_weights.rs:175`),
carries a documented **null calibration** (`:134-167`): a 54-config grid sweep
(`scripts/cost-grid-sweep.sh`, `bootstrap/src/bin/cost_grid_sweep.rs`,
`docs/benchmarks/cost-weights-sweep.json`) over {structural, alt_per_branch, dispatch_bonus,
ref_cost} produced **identical** DTA state counts for every grammar (JSON 51, BBNF 476, CSS 2875,
Sheets 191) — "the cost model has no choice to exercise" because the rewrite set collapses each
e-class to one canonical form. The doc's own conclusion names this family's seam: "exactly one slot
to update when a future calibration produces a non-trivial result" (`:169-174`). 2D is that
calibration — from *measured cyc/B*, not DTA state count (a static proxy), fed into the *perf*
dimension the grid sweep never touched.

---

## 2. Where the loop attaches — the knobs that actually have a choice (the null-trap escape)

The grid-sweep NULL (§1.4) proves the backend-shape *lattice* is degenerate: `PriorityStep` +
`shape_rank` force a single candidate per e-class, so fitting shape weights moves nothing. **The
loop must attach to knobs with genuine measured spread — numeric thresholds and crossover points,
precisely what a fitted coefficient is *for*:**

1. **The kernel-selection crossover.** `bbnf-simd/src/dispatch.rs:89` `select_backend` chooses
   `NeonTbl4` vs `Scalar` on `lo6_table_admissible` (`:101`) — a pure *legality* predicate
   (collision-free lo6 slots), NOT a cost decision. The "N\*=3 crossover" the empirical spine names
   is **not wired**: below it, per-call `build_lo6_table` (`:24-30`) is unamortized and scalar
   wins. A textbook fitted threshold: sweep span length on the canon bench, find the cyc/B
   crossover, commit it, gate `select_backend` on it. A genuine multi-candidate decision the loop
   moves.
2. **The CSS short-span guard.** GN-2.W3's `CSS_SHUFTI_MIN_SPAN` (GN-2.md `:333`) — "a NAMED const
   … exposed as a cost-model value, not a heuristic literal." Its purpose is *literally* "a
   threshold tuned against a measured constraint" (erase animate's +13.4% regression while
   retaining tailwind 4.16×). This is 2D's canonical instance; its born-RED gate (animate ≤1.0×
   AND three-corpus wins retained, GN-2.md `:354-363`) is the loop's objective as a falsifiable
   test.
3. **The GATE=16 prefilter fast-path width** (SPEC §0 row 01; GN-2.W1 gate requires `≥0.95× dense`,
   GN-2.md `:245`) — a fitted crossover between skip-benefit and dense-neutrality.
4. **`CapacityPolicy`** (`cost.rs:326-332`): `tiny_string_cap`, `container_initial_capacity` —
   numeric, workload-fitted.

The honest scope of a converged 2D: the loop is REAL and valuable at these
**kernel/threshold/capacity knobs**, and DEGENERATE at the backend-shape lattice until the
candidate generator (2A/2B/2C) grows real per-site alternatives. 2D commits to the former and
names the latter as its exact prerequisite dependency.

---

## 3. The substrate (what already serves; consumer-in-wave)

| Concern | Extant substrate (path:line) | 2D mutation |
|---|---|---|
| Cost provenance schema | `ir/src/cost.rs:172` `ActiveCostFacts` (`cost_formula_version`, `selected_cost_freshness`, `determinism_replay_status`, `selection_trace_hash`) | add `coefficient_table_hash`, `fit_residual_bps`, `training_corpus_id` (fields already serde) |
| Measured-evidence model | `cost.rs:293` `RejectedAlternative.evidence: Vec<Measurement>`; `:305` `Measurement.cycles_per_byte_x1000`; `:318` `EvidenceSource::BenchProbe` | fill the empty vec (`:37 evidence: Vec::new()`) with PMU facts — the data model is ready, zero facts flow |
| CSP provenance | `decision_csp.rs:183` `static_profile_provider_status`, `:186` `row_move_toward_sota_status` | flip `"static-template-blocker"` → `"fitted-coefficient-table-vN"` |
| Perf dimension | `passes/src/lib.rs:590` `perf_cost: 0` | `perf_cost = predict(f(rule,shape), β)` |
| Solver objective | `decision_csp.rs:44-49` `{0.0,1.0}` stub into `CostFiniteDomain` | real per-candidate `predict(...)` cost |
| CSP cost eval seam | `csp-solver/src/solver/optimize.rs:12` `DomainCostEval::cost` | fitted β is the coefficient source its `cost()` reads |
| Coefficient slot | `egraph/src/cost_weights.rs:175` `CALIBRATED_WEIGHTS`; `cost_config.rs:83` `from_env` | add `CostConfig::from_fact_table(path)`; keep `from_env` (`BBNF_COST_*`) for sweeps |
| Profile-hint seam | `passes/src/lib.rs:1037` `PriorBenchProfile { hot_rule_names }`, consumed `:1052-1058` | widen to `CostProfile { coefficients, per_site_samples }` |
| Freshness GATE | `bbnf-bench/src/report.rs:1742-1758` | give it teeth: `cost_formula_version = hash(table)` |
| Measurement source | `bbnf-bench/src/bin/json_canon_bench.rs:59`,`:185`; `css_canon_bench.rs` | add `--emit-facts` decision-attributed writer |
| Fit / commit / check host | `skinny/xtask/src/main.rs` dispatch; `cost_grid_sweep.rs` harness | new `calibrate` / `check-calibration` verbs; committed table dir; `profile/yyjson/*.profile.json` is the precedent home |
| Residualization | `runtime_generator.rs` `CSS_GENERATED_RS` (`const fn delim_mask`, PROVE-12 §3) | a fitted coefficient const-folds into an emitted immediate exactly as the delimiter mask does |

**Consumer-in-wave (constitution viii / Y.13).** The committed coefficient table is forbidden as
consumer-less substrate: it lands in the *same* wave as the `perf_cost` activation (`lib.rs:590`)
and the CSP objective (`decision_csp.rs:49`) that reads it. A table shipping while `perf_cost`
stays `0` is the dead-substrate pathology itself. The wire is four hops — bench fact stream → fit →
committed β → CSP reads `predict(...)` — all closing in-wave, mirroring the GN-2.W1 discipline
(GN-2.md `:40-46`).

---

## 4. Prior art (with citations)

- **AutoFDO** (Chen, Li, Hundt, *CGO 2016*). Sample-based FDO: hardware PMU samples (not
  instrumentation) mapped to source; the compiler re-optimizes reading a committed profile. The
  exact template — PMU → committed profile → deterministic re-compile — and its "samples not
  instrumentation" stance matches our `proc_pid_rusage` cyc/B source.
- **LLVM/GCC PGO** and **BOLT** (Panchenko et al., *CGO 2019*). `.profdata` is a committed fact
  file; recompilation is a pure function of (source, profile). Our departure: we commit *fitted
  coefficients*, not raw counts, so regen stays byte-deterministic and small.
- **AutoTVM / Ansor** (Chen et al., *OSDI 2018*; Zheng et al., *OSDI 2020*) and **Halide
  autoscheduler** (Adams et al., *SIGGRAPH 2019*) — the strongest analogue. A fitted cost model,
  trained on MEASURED runtimes, drives a search over schedules; measurements accumulate in a
  committed "tuning log" replayed deterministically. This *is* "measured feedback into a search's
  cost model, profile committed as an artifact," scoped here to grammar-kernel selection.
- **TASO** (Jia et al., *SOSP 2019*) — tensor-graph superoptimization extracts by *actual measured
  runtime*, direct precedent that measured cost is a legitimate e-graph extraction objective — vs
  **egg** (Willsey et al., *POPL 2021*) whose stock cost is *static* (`AstSize`/`AstDepth`); 2D is
  the measured-cost extension the crate's `Extractor`/`CostModel` seam
  (`backend_egraph.rs:83-88`,`:259-267`) already admits.
- **Ithemal** (Mendis et al., *ICML 2019*), **uiCA / llvm-mca** — basic-block throughput
  predictors: the `predict(features)` component. Our linear analog is deliberately simpler and
  deterministic (closed-form β, no learned net), fitted to whole-parse cyc/B because macOS PMU is
  process-granular (§5.2).
- **regex-automata `rare.rs`** (BurntSushi) — the positive micro-precedent: prefilter rare-byte
  selection reads a hardcoded byte-frequency rank table **fitted from a large corpus, committed as
  source, deterministic**, driving a prefilter decision. This is 2D at micro-scale and proves the
  pattern is SOTA-idiomatic for exactly this engine class — a small fitted table beating a live
  heuristic. (The same engine's tier-choice heuristics elsewhere remain *hand-tuned* — the exact
  seam LOCK-2's prefilter lowering, GN-2.W1, opens to fitting.)

---

## 5. Named risks & counterexamples

1. **The null-calibration trap (measured, in-tree — the central refutation risk).**
   `cost_weights.rs:134-167` proves a fitted cost model moves **nothing** when the candidate set
   has no distinct alternatives. 2D is inert over the backend-shape lattice. **Mitigation (§2):**
   attach the loop to knobs with demonstrated spread — kernel crossover, `CSS_SHUFTI_MIN_SPAN`,
   GATE=16, capacity — where a fitted value FLIPS a decision and MOVES a bench number. A 2D fitting
   coefficients over the degenerate lattice reproduces the null and is refuted; over the threshold
   knobs it has real work.
2. **Attribution underdetermination.** The bench emits *whole-grammar* cyc/B
   (`json_canon_bench.rs:193`), not per-rule; fitting per-decision β from whole-parse cyc/B over 5
   grammars × ~3 corpora is underdetermined (dozens of features, ~15 equations) → β
   unidentifiable. **Counterexample:** 40 JSON rules, 3 corpora cannot separate per-rule
   coefficients. The prototype must choose (a) **A/B differential** per decision — compile both
   legal knob values, diff whole-parse cyc/B cold-paired (clean, load-robust, O(candidate)
   compiles), or (b) **shape-labeled sub-parse probes** (finer, more harness). (a) is the honest
   primitive; the paired-interleave method is proven in-tree.
3. **Determinism vs measurement (fixpoint gate).** If the solver reads live PMU at regen,
   `regen∘regen ≠ regen`. Guard: fit is a *separate* offline verb; regen reads only committed β;
   the table hash is the determinism anchor (`cost_formula_version`). Counterexample to refuse:
   any design that re-measures inside `xtask regen`.
4. **Corpus/host overfit.** β fitted on twitter/canada/citm + one M-series host may mis-rank for
   TOML/CSV (no committed corpus yet — R1 §6), or fit animate at bootstrap's expense (GN-2.W3's
   named risk, GN-2.md `:634`). Guard: `host_triple` keys the table (aarch64-only per x86
   abrogation → single-host *simplifies*, no cross-ISA fit); the objective is a *multi-corpus
   constraint* (retain all wins), not a single-corpus argmin; a cross-grammar held-out residual is
   committed beside β and a flip must survive on a grammar outside the fit set.
5. **Provenance theater.** Stamping `cost_formula_version = hash(table)` without
   `check-calibration` actually re-running/diffing is the same lie the current `"pass"`/`0` stamps
   are. Guard: freshness must go RED on a mutated coefficient (§6, PROVE-2D-B).
6. **Refutation laws bind the measurement.** No warm benches (SPEC §0) — cold per-parse. Ratios
   certified / absolutes directional (precepts 43-49) — fit target is relative ordering. macOS
   `ri_cycles` is per-process — no per-BB counters (`xctrace_probe.rs` exists but is coarser); the
   attribution ceiling is process-granular, which is *why* the A/B-differential primitive (risk 2a)
   is the safe default.

---

## 6. What a prototype must test (the family-mover)

**PROVE-2D-A — does a measured coefficient table flip a decision, as a measured win?** Choose a
knob with demonstrated spread (§2: the `select_backend` kernel crossover `dispatch.rs:89`, or
`CSS_SHUFTI_MIN_SPAN`). A/B-measure the candidate values (compile each, cold-paired whole-parse
cyc/B, ≥3 windows); fit β / the crossover; commit a one-row `coefficients.ron`; wire the decision
driver (populate `perf_cost` at `lib.rs:590` / gate `select_backend`) to read it. **Gate:** the
fitted value differs from the hardcoded/legality-only value AND the fitted-value lane beats the
hardcoded-value lane cold-paired on a real grammar AND regen is byte-identical across the committed
table on double-regen. **Failure semantics (honest):** if no knob's fitted optimum differs from its
current value, 2D is a documented negative result — the null recurrence, now on kernels not the
lattice — and the family's disposition becomes "coefficient plane, blocked on 2A/2B/2C candidate
generation," a named dependency, not a silent pass.

**PROVE-2D-B — determinism & falsifiable freshness.** Fit under quiet load and under loadavg 13-19
(the PROVE-12 regime); committed β must be identical (fit on paired medians/ratios,
load-invariant). Regen twice → byte-identical (fixpoint). Then set `cost_formula_version =
hash(coefficients.ron)` and prove the `report.rs:1750` freshness gate is FALSIFIABLE — mutate one
coefficient, freshness goes RED, `check-calibration` fails. This proves the loop is a real circuit,
not stamped strings, and that its determinism is achieved by *separation* (measure off the regen
path), not by careful timing.

**The minimal move:** even one profile-fitted decision that (a) flips, (b) wins cold-paired, (c)
leaves regen byte-identical, (d) drives freshness RED when perturbed — converts `perf_cost: 0` and
`static-template-blocker` from an architectural admission into a measured, deterministic, auditable
closed loop.
