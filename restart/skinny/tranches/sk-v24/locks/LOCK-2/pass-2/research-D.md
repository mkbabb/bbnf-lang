# LOCK-2 · Family 2D — PROFILE-CLOSED-LOOP (research, pass 2)

*Optimization as measured feedback: cyc/B coefficients fitted from canon-bench PMU
captures, committed as an in-tree fact table, re-consumed at regen with determinism
preserved and a content-addressed freshness gate. The loop is the architecture — but the
loop earns its keep only where a real decision exists to be flipped, and this family's first
duty is to prove that site exists (Constitution viii; Y.13).*

---

## 1. The mechanism made concrete

The closed loop is four organs over one already-present dimension.

**CAPTURE.** The canon benches (`bbnf-bench/src/bin/json_canon_bench.rs`,
`css_canon_bench.rs`) already emit per-corpus cyc/B via per-process `ri_cycles`
(`proc_pid_rusage(RUSAGE_INFO_V5)`), paired interleaving, cold per-parse N≥200, ≥3 windows.
These are the fitting instrument, not new code. Each capture yields a per-(rule, workload)
cyc/B point estimate with a CI.

**FIT.** An offline regression turns captures into integer-scaled coefficients. The
attribution problem — whole-grammar cyc/B underdetermines per-feature coefficients — is
solved by the **A/B-differential** design already proven in the residualization prototype: a
base binary and a one-feature-mutated binary run interleaved back-to-back per window (a
paired block), so the coefficient for that feature is the *difference* holding grammar and
corpus constant. One capture isolates one coefficient. cyc/B ratios 0.81–0.98× at loadavg
13–19 were recovered this way with ins/B (deterministic, windows agree <0.2%) as the
load-invariant parity check.

**COMMIT.** The fitted coefficients land in an in-tree `ProfileFactTable` (serde JSON,
mirroring the existing `docs/benchmarks/cost-weights-sweep.json` sweep artefact) — a vector
of `Measurement` rows keyed by `(RuleId, workload)`, plus a `source_hash` and a
`cost_formula_version`. **The row schema already exists**: `ir/src/cost.rs:305`
`Measurement { throughput_mbps_x1000, cycles_per_byte_x1000: Option<u64>, hot_leaf_count,
source: EvidenceSource, source_ref }`. The cyc/B is stored **integer-scaled (×1000)** — no
float in the committed fact, so regen consuming it is bitwise deterministic.
`EvidenceSource::BenchProbe` (`cost.rs:320`) is the fact's provenance tag.

**CONSUME.** At regen the cost model reads the *committed table only* — never live PMU,
never env — and writes the fitted cyc/B into the one dimension that is today a hardcoded
zero: `crates/passes/src/lib.rs:590` sets `perf_cost: 0` for **every** backend candidate.
The `DecisionCost` lattice (`backend_egraph.rs:239`) orders on
`(freshness_rank, perf_cost, capacity_cost, static_size_cost, shape_rank)`; extraction picks
the minimum (`Extractor::best_node`, `backend_egraph.rs:85`). With `perf_cost ≡ 0` the whole
decision tie-breaks on `shape_rank` — the static P1–P8 priority order re-expressed as a
number (`lib.rs:545,552,558,564,572`). A fitted `perf_cost` is the *first real content* that
dimension has ever carried; because it outranks `static_size_cost` and `shape_rank`, it can
flip the choice wherever ≥2 candidates survive hard-prune and their measured cyc/B disagrees
with the static cascade.

**The freshness gate** is the loop's closure and its safety. `source_hash` covers exactly
`{grammar source bytes ∪ kernel-set id ∪ cost_formula_version ∪ rewrite_set_id}` — all four
already have version constants (`backend_egraph.rs:7-9`: `SCHEMA`, `FORMULA`, `REWRITE_SET`).
A new `xtask check-profile-fresh` subcommand (sibling of `check-json`/`lint-loc`,
`xtask/src/main.rs:41,49`) recomputes the hash at regen and goes RED on any mismatch, forcing
re-capture. The `stale` bit (`lib.rs:589`, today always `false`) is set from that comparison;
`stale=true` lifts `freshness_rank` to 1 (`backend_egraph.rs:169`), and because
`freshness_rank` is the lattice *top* a stale coefficient loses to a fresh one — graceful
degradation, not a wrong decision (see risk R4 for the uniform-stale refinement).

---

## 2. The substrate — what already exists (path:line)

The closed loop is ~80% plumbed and blocked at one named seam.

- **The unpopulated dimension.** `passes/src/lib.rs:590` `perf_cost: 0`, `:589` `stale: false`
  — literally hardcoded. This is the family's entire integration surface: fill these two
  fields from the fact table. Nothing else in the decision path changes.
- **The lattice that already ranks on it.** `backend_egraph.rs:239-257` `DecisionCost` +
  `Lattice`; `:127` emits `selected_cost_freshness = "fresh"|"stale"` from `freshness_rank`.
  The extraction (`:83-88`) is a real e-graph extraction over the candidate class.
- **The fact-row schema.** `ir/src/cost.rs:305` `Measurement` (integer-scaled, serde) +
  `:296` `RejectedAlternative { evidence: Vec<Measurement> }` + `:318` `EvidenceSource`. The
  compile already *asserts* measurement-backed evidence: `lib.rs:446` raises
  `cost_facts_missing_evidence` when `rejected.len() < 4 || !has_measurement_evidence`. The
  loop's job is to make that evidence *fitted-real* rather than backfilled
  (`EvidenceSource::RedressBackfill`, `lib.rs:738`).
- **The single commit slot + fitting harness.** `crates/egraph/src/cost_weights.rs:175`
  `CALIBRATED_WEIGHTS` is the "exactly one slot to update when a future calibration produces a
  non-trivial result"; `scripts/cost-grid-sweep.sh` + `docs/benchmarks/cost-weights-sweep.json`
  is the sweep apparatus. The `ProfileFactTable` extends this pattern from global weights to
  per-rule coefficients.
- **The named blocker.** `passes/src/decision_csp.rs:183`
  `static_profile_provider_status: "static-template-blocker"`, `:186`
  `row_move_toward_sota_status: "measured_architectural_block"`. The profile provider is a
  static template; the closed loop is *disclosed-absent*. This family is that provider.
- **The env-fence.** `egraph/src/cost_config.rs:83` `CostConfig::from_env` reads `BBNF_COST_*`
  — documented **benchmark-only**. Regen (`xtask regen`) constructs `CostConfig::default()`
  (→ `CALIBRATED_WEIGHTS`), never `from_env`. This fence is the determinism guarantee and must
  be asserted in the regen gate: an env-poisoned regen must be byte-identical.

---

## 3. Directive Q8, answered with evidence

**(a) Can a fitted coefficient flip a real per-site decision?** Only where the ranked
candidate set has ≥2 survivors. `backend_candidates` (`lib.rs:508-596`) builds five specs;
`hard_pruned = !ranked` (`:588`). A rule that both `admits_sink_only` and is not `eager_forced`
yields SinkOnly (`shape_rank 10`) **and** OffsetTape (`shape_rank 40`) both ranked — a genuine
≥2-survivor site. Today OffsetTape can never win it (static `shape_rank`); a fitted `perf_cost`
that measures SinkOnly's direct-sink lane as *more* cyc/B on a given corpus would flip it. The
flip is real *iff* such a site with disagreeing cyc/B exists in the fleet — **P1 below is the
load-bearing test.**

**(b) Win cold-paired at real value-start positions?** The measurement substrate is the canon
benches at value-start (json_canon `parse_tape`/`fused` lanes; css_canon `track1` lanes). A
fitted coefficient is *admissible* only if its winning candidate also wins cold-paired cyc/B
at those positions, CI-excluding parity in the same direction across ≥3 windows — the same bar
the residualization prototype cleared (cyc/B 0.81–0.98×, won in both interleave slots).

**(c) Freshness gate RED on mutation?** Yes, mechanically. Mutating one grammar source byte
changes `source_hash` → `check-profile-fresh` RED → `stale=true` → `freshness_rank=1` →
`selected_cost_freshness="stale"` (`backend_egraph.rs:127`). The gate is falsifiable and
cannot silently pass a stale table.

**Attribution under underdetermination.** The A/B-differential (§1 FIT) is the answer: whole-
grammar cyc/B cannot separate two features, but a paired base-vs-one-feature-mutated capture
isolates one coefficient per run. This is not theory — it is the paired-block method that
recovered the residualization deltas.

**Which knobs a fit supersedes.** The hand-set literals in `cost_weights.rs:112-131` /
`CALIBRATED_WEIGHTS` — `tape_push: 1.0`, `dispatch_table: 3.0`, `dispatch_branch: 0.5`,
`call_overhead: 4.0`, `dispatch_bonus: -2.0` — and any N\*=3 kernel-crossover threshold. A
non-trivial fit replaces these defaults *per (rule, workload)* via `perf_cost`; the global
`CostWeights` remain the fallback when no fitted row applies.

**Env-source fenced from regen.** `from_env` (`cost_config.rs:83`) is bench-only; regen reads
the committed table. The fence is testable (P4): regen with `BBNF_COST_*` set to garbage must
produce a byte-identical tree.

---

## 4. Prior art (citations)

- **rustc / LLVM PGO** — the canonical committed-profile loop: `-fprofile-generate` captures,
  a `.profdata` artefact is committed/versioned, `-fprofile-use` re-consumes at build. **Its
  staleness detector is precisely this family's freshness gate**: LLVM's SampleProfileLoader
  hashes each function (`FunctionSamples`/`getFunctionHash`) and *drops* a profile whose hash
  mismatches, emitting `-Wprofile-instr-out-of-date`. Content-addressed profile → RED-on-drift
  is an established, shipping mechanism, not a novelty.
- **AutoFDO** (Chen, Li, Hundt, CGO 2016) — continuous PGO from production PMU (LBR) samples,
  fitted offline, re-consumed; drift handled by explicit staleness metrics. The precedent for
  "capture from PMU, commit, tolerate/measure drift."
- **BOLT** (Panchenko et al., CGO 2019) — post-link optimizer driven by `perf.data`; the
  profile is a first-class committed artefact re-consumed by a later pass.
- **egg / egglog cost extraction** — `egg::CostFunction` greedy extraction is optimal for
  *tree-structured* e-classes; DAG sharing needs ILP (Yang et al., "Equality Saturation for
  Tensor Graph Superoptimization," MLSys 2021). The corollary bites here (§5, R1): if class
  multiplicity ≈ 1 the cost model has *no choice to exercise* and greedy is trivially optimal —
  which is exactly what the 54-config weight sweep found.
- **regex-automata / rebar** — literal-prefilter selection is cost-driven by measured match
  rates; the "profile picks the accelerator" pattern for scanner-tier decisions.
- **Reproducible-builds discipline** — a committed profile is *source*, not build-time capture;
  determinism demands the coefficient be an in-tree integer, never a live measurement.

---

## 5. Named risks & counterexamples

- **R1 (near-fatal): multiplicity ≈ 1 — the loop may have no consumer.**
  `cost_weights.rs:145-161` records the settled null result: a 54-configuration sweep over
  {structural, alt_per_branch, dispatch_bonus, ref_cost} left the DTA state count **invariant**
  for JSON (51), BBNF (476), CSS L4 (2875), Sheets (191). The rewrite set produces e-classes
  with at most one canonical form, so *no fitted weight changed any decision*. A profile loop
  feeding a dimension with no choice is a consumer-less substrate (Constitution viii). The
  family's honest disposition if P1 finds no ≥2-survivor-with-disagreement site: **provider
  wired, born-RED, no consumer in the current fleet** — SPEC-only until a ≥2-survivor site
  exists, not a silent pass.
- **R2: profile can pick the loser (the digit-kernel refutation, now law).** A candidate showed
  −37/−48% instructions yet +50% `parse_tape` because CPI inverted 0.13→0.30. A coefficient
  fitted on **ins/B would have chosen the losing kernel.** Design constraint: the loop's
  objective is **cyc/B** (the certified, CPI-inclusive metric); ins/B is only the load-invariant
  *parity* check, never the objective. P5 constructs this exact adversarial case.
- **R3: measurement noise vs regen determinism.** cyc/B carries load noise (loadavg 13–19). A
  raw-float coefficient makes regen non-deterministic. Mitigation: commit the **integer ×1000
  median over ≥3 windows** with its CI; `cycles_per_byte_x1000: Option<u64>` is already the
  integer slot. Regen reads the committed integer and never re-measures — determinism is
  structural, not statistical.
- **R4: uniform-stale table still drives on stale numbers.** `freshness_rank` de-ranks a stale
  candidate only against a *fresh* one; if every candidate comes from one stale table they share
  `freshness_rank=1` and the tie falls through to stale `perf_cost`. Refinement: on
  `source_hash` mismatch, **reset `perf_cost` to the neutral default (0)** so a stale table
  degrades to the static `shape_rank` cascade rather than driving on rotten coefficients. The
  freshness gate then only *gates CI*, and the runtime decision is always either fresh-fitted or
  safe-default.
- **R5: freshness hash mis-scoped.** Too narrow (misses a kernel swap) → a stale coefficient
  reads GREEN; too broad (formula bump re-REDs everything) → gate noise. The hash covers exactly
  the four versioned inputs (§1); a kernel-set id must be part of it precisely because a NEON
  kernel change alters cyc/B without touching grammar bytes.

---

## 6. What a prototype must test (to move this family)

1. **P1 — the Y.13 audit (load-bearing, run first).** Across the fleet
   (json/css_l4/sheets, and csv when it lands), enumerate every rule whose
   `backend_candidates` ranked set (`lib.rs:508`, `hard_pruned=false`) has ≥2 survivors, and
   for each measure whether the survivors' fitted cyc/B *disagree with the static `shape_rank`
   order*. If ≥1 such site: the loop has a consumer (proceed to P2). If none: the family is
   SPEC-only/born-RED — report it as R1, do not force a synthetic site.
2. **P2 — the flip + win.** For one P1 site, fit a cyc/B×1000 coefficient from a canon-bench
   A/B-differential capture, write it to `perf_cost`, and prove the extraction now selects the
   other shape **and** that shape wins cold-paired cyc/B at value-start (CI-excluding parity,
   ≥3 windows, both interleave orders). (Q8a+b)
3. **P3 — freshness RED + graceful degradation.** Mutate one grammar source byte; assert
   `check-profile-fresh` goes RED, `selected_cost_freshness="stale"`, and (per R4) the decision
   falls back to the static cascade rather than the stale coefficient.
4. **P4 — the env-fence / determinism proof.** Run `xtask regen` twice, once with `BBNF_COST_*`
   set to adversarial values; assert byte-identical generated output both times and against the
   committed tree.
5. **P5 — the cyc/B-over-ins/B guard.** Construct a candidate pair where candidate A has lower
   ins/B but higher cyc/B than B; assert the loop's objective picks B. Prevents re-committing
   the digit-kernel refutation.

**Cross-lock note.** The `perf_cost` this family fits is the same objective dimension a
LOCK-2 budget-knapsack constraint (`bounded_sum` at named sites) and any PBQP/CSP selector
would minimize; the fact table is the *cost source* those solvers read, independent of which
solver shape wins. This family owns the *feed*, not the solver.
