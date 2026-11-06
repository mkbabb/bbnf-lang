# LOCK-2 · Family 2D — PROFILE-CLOSED-LOOP (research, pass 1)

Optimization as measured feedback: cost coefficients FITTED from canon-bench PMU captures,
committed as deterministic fact tables, the solver RE-RUN at regen. The loop — capture →
fit → commit → re-solve → regenerate → re-capture — is the architecture, not a bolt-on. This
family owns the COEFFICIENT PLANE beneath whatever candidate generator (islands / saturation /
CSP) sits above: it answers where the numbers in the cost lattice come from and why they are
trustworthy AND deterministic. The differentiator no parser generator holds: profiler and cost
model are one circuit — PEG/LR emit fixed automata (no profile), simdjson's thresholds are
hand-tuned magic constants, not fitted.

## 0. The thesis and the wall it must break

The graph engine today decides with a STATIC, self-declared-blocked cost model. Three
artifacts prove the loop is stubbed and waiting:

1. `crates/egraph/src/cost_weights.rs:175` — `CALIBRATED_WEIGHTS` is a hardcoded const. Its
   provenance docstring (`cost_weights.rs:134-167`) records that the AW-IV.W5.3 grid-sweep
   over 54 configs × 4 grammars returned **NULL**: identical DTA state counts (JSON 51, BBNF
   476, CSS L4 2875, Sheets 191) at every swept point. Artefact: `docs/benchmarks/
   cost-weights-sweep.json` (fit methodology preserved: per-grammar Pareto over
   (state_count, pipeline_ns), 3 repeats, median). Stated cause: the rewrite set
   (`DeduplicateAltBranches, SupersetAbsorbAlt, UnionMergeAlt, FuseAltRegexBranches,
   CommonSuffixFactor`) yields ≤1 canonical form per e-class — "the cost model has no choice
   to exercise."
2. `skinny/crates/passes/src/backend_egraph.rs:8` — `cost_formula_version = "sk-v13-w6-
   integer-v1"`; `DecisionCost{freshness_rank, perf_cost:u32}` (`:240-242`) and
   `DecisionCostModel::cost` (`:259-264`) score with a hardcoded INTEGER `perf_cost`, never a
   measured cyc/B.
3. `skinny/crates/passes/src/decision_csp.rs:183-186` — the loop's own provenance strings
   NAME the gap: `static_profile_provider_status: "static-template-blocker"`,
   `generated_runtime_diff_status: "absent"`, `row_move_toward_sota_status:
   "measured_architectural_block"`. These are the stub this family activates.

**The wall (the decisive counterexample, faced head-on).** A closed loop that fits
coefficients into e-graph EXTRACTION is consumer-less substrate (Constitution viii / Y.13):
AW-IV already measured extraction as degenerate — better numbers change no decision.
Therefore 2D MUST close the loop NOT at `Extractor` (`crates/egraph/src/extract.rs`, greedy
bottom-up, proven null) but at the decision points where genuine multi-candidate choice
exists. Those points, all extant:
   - the BackendShape priority ladder P1–P8 (`ir/src/cost.rs:264-286`), 5 shapes
     (`all_backend_shapes`, `cost.rs:334`), selected in `passes::derive_backend_shape`
     (R1 §8, `passes/src/lib.rs:398`) then re-ranked by `backend_egraph::select`
     (`passes/src/backend_egraph.rs:36`) and `decision_csp::solve_rule`
     (`decision_csp.rs:29`, a real `Csp<CostFiniteDomain>` with
     ForwardChecking/FailFirst/MinimizeCost, `:85-98`);
   - the SIMD mode Exact↔Prefilter and the N\*=3 kernel crossover (GN-2.W1/W4; `SimdMode`
     `ir/src/lib.rs:425`; the shufti-on-measured-n guard, GN-4.md/PT-1.md);
   - the CSS `CSS_SHUFTI_MIN_SPAN` short-span guard (GN-2.W3, a named cost-model value);
   - the capacity policy `tiny_string_cap`/`container_initial_capacity`
     (`ir/src/cost.rs:326-332`).
These are real forks. Measured cyc/B can flip them; static integers cannot know which way.

## 1. The mechanism, made concrete

**A committed fact table + a gated fit harness + a table-reading provider + re-solve at
regen.** Five organs:

**(a) The capture.** `json_canon_bench` already emits the exact schema this loop ingests:
`# schema: PMU corpus lane bytes iters cycles instructions cyc_per_byte cpi mbps`
(`skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs:193,211`; `css_canon_bench` the CSS
twin; `ri_cycles` via `proc_pid_rusage(RUSAGE_INFO_V5)`, `:59,209`; paired-interleaved mode
with 95% CI, `:224-259`). The §0 certified swath IS a fact table already: PROVE-12 CSS cyc/B
per corpus×lane (bootstrap 0.848/0.812, tailwind 0.954/0.934, material 0.979/0.909, animate
0.955/0.964; ins/B −3.7…−6.4% deterministic), PROVE-01 skip floor 0.115 cyc/B and ~1.03
cyc/B membership ceiling, PROVE-03 twitter parse_tape 0.898×, PROVE-05 tailwind lcss 4.16×.
Cold paired, multi-window, ratios certified — the profiling standing law (GN-2 §1.2 precepts
43–49) IS the capture protocol.

**(b) The fit.** A new `xtask fit-cost` (the resurrected/relocated `cost-grid-sweep`; the
`.sh` is GONE — only `docs/benchmarks/cost-weights-sweep.json` survives, R1/grep confirm no
live `scripts/` harness) reads the PMU stdout, regresses per-decision-site cyc/B (lead with
ins/B, load-proof) onto the site's STATIC features (node count, alt arity, span length,
member-set cardinality — never corpus identity, no name-gate) and emits fitted coefficients.
The fit is ordinary least-squares over committed capture rows — no live bench in the regen
path.

**(c) The committed table.** `skinny/crates/ir/cost-facts/<grammar>.toml` — a versioned,
human-readable, content-addressed fact table: per-decision-site coefficient rows keyed by
`(grammar, rule_id, decision_kind)`, each carrying fitted cyc/B + ins/B, capture provenance
(window count, loadavg band, bench commit hash), and a `cost_formula_version` bump. This is
"committed as fact tables (determinism preserved)" literally. It supersedes the hardcoded
`CALIBRATED_WEIGHTS` const and the `"sk-v13-w6-integer-v1"` integer formula;
`CostConfig` (`crates/egraph/src/cost_config.rs:56`) grows a `from_facts(path)` constructor
beside its `from_env` (`:83`, the existing `BBNF_COST_*` tuning surface), reading the table
into `weights` + per-site perf costs. The `@cost` grammar directive named "future tranche"
in `cost_config.rs:29` reads the same struct — one slot.

**(d) The re-solve.** At regen (`xtask regen-json|regen-css|…`, R1 §5) the decision driver
re-runs `backend_egraph::select` + `decision_csp::solve_rule` with table-sourced perf costs
replacing `candidate.perf_cost` (`backend_egraph.rs:21,170`). The DecisionCspFacts provenance
flips from stub to fact: `static_profile_provider_status → "fitted-fact-table@<version>"`,
`generated_runtime_diff_status → "present:<capture-ref>"`, `row_move_toward_sota_status →
"<measured cyc/B delta>"` (`decision_csp.rs:183-186`). `selected_cost_freshness`
(`backend_egraph.rs:127`) and `candidate_cost_stale_rate_bps` (`cost.rs:181`) become LIVE: a
row older than its grammar's last bench is STALE, prompting re-fit — "solver re-runs at
regen" with freshness teeth. `determinism_replay_status` + `selection_trace_hash`
(`backend_egraph.rs:139,142`) verify the table→decision map replays byte-identically.

**(e) The re-capture gate.** After regen, `json_canon_bench`/`css_canon_bench` re-run in CI
confirms the fitted decision did not regress (born-RED non-regression). A decision the table
flipped must beat the decision it displaced on the same corpus, cold paired, or the fit is
REVERTED and the row flagged. This is the loop closing on itself — GN-2.W6's adversarial
re-measure discipline made standing infrastructure.

## 2. Substrate (extant vs net-new)

EXTANT (consume, do not rebuild):
- `ir::ActiveCostFacts`/`DecisionCspFacts` (`ir/src/cost.rs:173,203`) — the provenance spine,
  already carrying `cost_formula_version`, `candidate_cost_stale_rate_bps`,
  `selected_cost_freshness`, `determinism_replay_status`, `rewrite_order_variance_pct`,
  `selection_trace_hash`, `static_profile_provider_status`, `generated_runtime_diff_status`,
  `row_move_toward_sota_status`. These fields ARE the closed-loop schema; 2D fills them.
- `egraph::CostConfig`/`CostWeights` (`cost_config.rs`, `cost_weights.rs`) + `from_env`
  overrides — the tuning surface; extend with `from_facts`.
- `backend_egraph::select` + `decision_csp::solve_rule` — the CSP over `CostFiniteDomain`
  (`decision_csp.rs:48-98`) that already ranks candidates; swap its cost source only.
- `json_canon_bench`/`css_canon_bench` PMU mode — the capture, schema-stable.
- `docs/benchmarks/cost-weights-sweep.json` — the fit-harness precedent + input format.

NET-NEW (small, colocated, §1.5 parsimony): `xtask fit-cost` command (+ module under
`xtask/src/`); `ir/cost-facts/*.toml` committed tables; `CostConfig::from_facts`; the
provenance-string activation in `decision_csp.rs`/`backend_egraph.rs`; a CI re-capture gate
bin. No new crate — the loop rides the existing cost/decision spine (Constitution vi).

## 3. Prior art (SOTA, cited)

- **rustc `-Cprofile-use` / PGO** (LLVM `llvm-profdata` merge → `.profdata` at build).
  Exactly this shape: an out-of-band profiling run produces a committed fact file steering
  later codegen; profile is deterministic INPUT, not live measurement. Lesson 2D inherits:
  version + staleness-check the file; builds without it fall to static heuristics (our
  `from_env`/default path — graceful degradation).
- **LLVM `BlockFrequencyInfo` + `MachineBlockPlacement`** — a fitted cost (PGO branch
  weights) drives a discrete layout fork. Precedent that the measured coefficient must reach a
  REAL discrete decision to matter; the AW-IV null is the counter-lesson (coefficient without
  a fork = no effect).
- **egg/egglog cost models + `LpExtractor`** (Willsey POPL'21; "Better Together" 2023).
  Confirms greedy bottom-up extraction (`extract.rs`) is cost-INSENSITIVE when a class has one
  canonical form — the field 2D must avoid. egglog scheduling/rulesets show where measured
  cost CAN steer (rule firing order, saturation budget) — maps to our `CspScheduler`
  iter/node caps (`csp_scheduler.rs:175-201`, `CostConfig` limits `:42-46`).
- **MiniZinc/OR-Tools fitted objective** — `decision_csp`'s
  `OptimizationMode::MinimizeCost` (`:89`) is the standard fitted-objective CSP; the fact
  table is its coefficient vector.
- **regex-automata prefilter cost heuristics** (BurntSushi) — the Teddy/memchr crossover is a
  fitted membership-scan threshold; direct analogue to our N\*=3 kernel crossover
  (GN-2.W4/SimdMode) as a profile-fitted decision.
- **`fast_float`/Lemire** — canada float-conversion tax (+1.48 cyc/B, GN-2.W5) is a fact-table
  row that could select deferred-vs-eager materialization by measured lane cost.

## 4. Named risks + counterexamples

- **R1 (the wall, primary): the fitted coefficient changes NO decision.** If every real fork
  is already forced by hard preconditions (`ShapeRationale`, `cost.rs:250`; the P1–P8 ladder
  fires deterministically), measured cost is decorative and 2D is Y.13-dead like the
  extraction sweep. Counterexample to survive: ≥1 grammar site where the static P-ladder picks
  shape A but measured cyc/B favors B (candidate: OffsetTape vs SinkOnly on a thin projection —
  PROVE-07's honestly-unmet twitter 2× bar, §0 row 07). If none exists across
  json/css/csv/sheets/toml, the family RETIRES with a documented negative result and LOCK-2
  converges on the candidate-generating families instead.
- **R2: determinism leak.** If regen ever runs a bench, byte-identical regen (precept 53)
  breaks under load noise. Structural mitigation: regen reads the committed table ONLY;
  fitting is a separate gated command; `determinism_replay_status` + `selection_trace_hash`
  verify replay.
- **R3: overfit to a corpus.** A twitter-fitted coefficient mis-steers canada. Mitigation:
  rows keyed per-(grammar, corpus-class); fit regresses on STRUCTURAL features, not corpus
  identity (no name-gates); the re-capture gate (§1e) catches cross-corpus regression.
- **R4: staleness churn.** Every kernel edit (GN-2 waves) invalidates rows → re-fit thrash.
  Mitigation: `candidate_cost_stale_rate_bps` is WARN not hard-fail; stale rows fall to static
  default (rustc-PGO-without-profile shape).
- **R5: load-noise in the fit.** cyc/B carries loadavg noise (§0: absolutes directional).
  Mitigation: fit the COEFFICIENT on ins/B (deterministic, load-proof, PROVE-12 §4), keep
  cyc/B as the ratio-certified cross-check — the standing law's load-invariant column.

## 5. What a prototype MUST test to move this family (the falsifier)

**PROVE-2D — "the fork exists and the profile flips it."** One prototype, born-RED, decisive:
1. Enumerate every BackendShape/SIMD/capacity decision site across the 5 grammars; count how
   many are precondition-FORCED vs genuinely cost-ranked (`ranked.len() > 1` in
   `backend_egraph::select`, `:50-58`). If ZERO sites have ≥2 admissible candidates, the
   family is dead on arrival — report the null (mirrors AW-IV.W5.3), retire.
2. For ≥1 cost-ranked site, hand-author a `cost-facts.toml` with measured cyc/B (an actual
   `json_canon_bench pmu` / `css_canon_bench` run, ≥3 windows) and show the driver selects a
   DIFFERENT shape than `CALIBRATED_WEIGHTS`/integer `perf_cost` does.
3. Regenerate; confirm the flipped decision (a) produces a byte-different generated parser,
   (b) beats the static decision on that corpus cold-paired (the born-RED win), (c) leaves
   `cargo xtask regen` byte-idempotent (table-read only, no bench in regen).
4. Flip the three stub strings (`static_profile_provider_status`,
   `generated_runtime_diff_status`, `row_move_toward_sota_status`) to fact-bearing values and
   assert them in a `tests/` provenance oracle.

PASS ⇒ 2D is a live, consumer-bearing loop with a measured win. FAIL at step 1 ⇒ the family
is architecture-without-consumer and yields to sibling families. The prototype's single job is
to answer the AW-IV null: **does a measured coefficient govern any real decision on any real
grammar?** Everything else in this family is plumbing around that one falsifiable question.
