# LOCK-2 pass-2 — Family 2D: PROFILE-CLOSED-LOOP

*Optimization as measured feedback: cost coefficients fitted from canon-bench PMU captures,
committed as fact tables (determinism preserved), the solver re-runs at regen — the loop is the
architecture. Developed alone to its strongest form. Primary steering: Q8 (profile-fed cost); Q7
(budget knapsack objective) and Q3 (regen determinism) where the loop touches them.*

---

## 1. The mechanism, made concrete

The family is a **three-phase committed-fact-table loop** whose determinism fence is that regen
is a pure function of *committed constants*, never of live measurement or environment. The loop
is the AutoFDO/`-fprofile-sample-use` shape recast onto the grammar generator, with a staleness
gate borrowed from `-Wprofile-instr-out-of-date`.

- **Phase A — FIT (offline, out of band from regen).** An `xtask fit-cost` host runs the canon
  benches (`json_canon_bench`, `css_canon_bench`) under the profiling standing law
  (`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, paired interleaving, cold N≥200, ≥3 windows,
  per-block loadavg). It does **not** fit N coefficients from one whole-grammar number — that is
  underdetermined (Q8's attribution problem: whole-grammar cyc/B is a single confounded
  observation). It fits by **A/B-differential**: each paired arm toggles exactly ONE lever
  (feature ON/OFF), and the *delta* in ins/B **is** that lever's coefficient. This is precisely
  what the PROVE fleet's paired interleaving already isolates (PROVE-01 skip-vs-scalar,
  PROVE-12 mask-immediate-vs-slice, PROVE-03 kernel-vs-scalar). The number of coefficients
  fittable is bounded by the number of independently-toggleable levers with a paired bench — a
  disclosed, honest ceiling, not a free-for-all regression.

- **Phase B — COMMIT (human-gated).** The fitted vector is written as a **committed const fact
  table**, bumping `cost_formula_version`. The existing slot is `CALIBRATED_WEIGHTS`
  (`crates/egraph/src/cost_weights.rs:175`); the existing version string is `FORMULA =
  "sk-v13-w6-integer-v1"` (`crates/passes/src/backend_egraph.rs:8`). Regen reads only this const
  (via `CostConfig::default()` → `CALIBRATED_WEIGHTS`, `cost_config.rs:65`). The commit is the
  loop's only mutation point; between commits the generator is deterministic.

- **Phase C — FRESHNESS GATE (CI, born-RED).** A gate re-runs the fit and compares each fitted
  coefficient's CI against the committed value. Drift beyond the band → RED ("mutation detected,
  re-commit `cost_formula_version`"). This is the field `selected_cost_freshness`
  (`backend_egraph.rs:127`) and `determinism_replay_status` (`:139`) — today hardcoded `"pass"`
  — wired to a real check. It is the direct analog of LLVM's function-hash profile-staleness
  warning: the profile is stale when the thing it measured changed shape.

**The consumer (Constitution viii / Y.13).** The loop's *output* lands at the emit spine as a
residualized constant, exactly the PROVE-12 mechanism (`crates/codegen/src/runtime_generator.rs`,
`CSS_GENERATED_RS` template): a fitted coefficient that selects a residualization depth or a
span-guard threshold emits a *different const-folded immediate*, disasm-verifiable, regen
bit-for-bit (PROVE-12 §5, §6). A profile coefficient with no residual it changes is a
consumer-less substrate and is forbidden.

---

## 2. The substrate that already exists (integration seams, path:line)

The family is ~70% substrate-present; the missing 30% is the *fit harness*, the *freshness
check body*, and — critically — a *live decision for a coefficient to flip* (§5).

- **The fact table is schema-complete.** `ActiveCostFacts` (`skinny/crates/ir/src/cost.rs:172`)
  already carries `cost_formula_version`, `selected_cost_freshness`, `determinism_replay_status`,
  `rewrite_order_replay_count`, `rewrite_order_variance_pct`, `selection_trace_hash`. `Measurement`
  (`cost.rs:305`) stores `throughput_mbps_x1000` and `cycles_per_byte_x1000` as **u64
  fixed-point** with `EvidenceSource::{BenchProbe, RedressBackfill, StaticAnalysis}` (`:318`).
  The fixed-point convention (×1000) is already the house style — the fit MUST emit
  milli-cyc/B integers, never raw `f64` (§4, float-tie determinism).
- **The total-order tie-break is already correct** (answers Q3's "HashMap iteration
  inadmissible"). `DecisionCost` (`crates/passes/src/decision_csp.rs:239`) is a derived-`Ord`
  tuple `{ freshness_rank, perf_cost, capacity_cost, static_size_cost, shape_rank, candidate_hash
  }` ending in `candidate_hash: u64` = FNV-1a of the candidate id (`stable_hash`,
  `decision_csp.rs:304`). The content hash makes the lexicographic order **total** — no ties —
  so the greedy extractor's `cost < existing` (`crates/egraph/src/extract.rs:156`, strict `<`
  keeps first-seen) is deterministic regardless of `FxHashMap` iteration. `selection_trace` sorts
  its rows before hashing (`decision_csp.rs:291`). This is the admissible tie-break; the family's
  job is to **preserve** it under fitted coefficients (§4).
- **`static_profile_provider_status: "static-template-blocker"`** (`decision_csp.rs:183`) is the
  tell: the profile provider is a *blocker* at HEAD — no closed loop exists, the perf coefficients
  (`perf_cost: u32`, `capacity_cost`, `static_size_cost`, `decision_csp.rs:20-23`) are hand-set
  integers. Family 2D's terminal state flips this string to `"fitted-committed-v1"`.
- **The env-fence is nearly clean already** (answers Q8's "how is env-source fenced from regen").
  `CostConfig::from_env` reads `BBNF_COST_*` (`cost_config.rs:83`) but the ONLY live `from_env`
  caller on any path is `CapacityPlan::from_env` at `crates/codegen/src/json_templates/parser.rs:17`
  — and that is a *runtime* generated-code probe (`BBNF_TAPE_CAPACITY`), not the regen path. The
  fence is: regen calls `CALIBRATED_WEIGHTS`/`CostConfig::default()`; `from_env` is bench-only. A
  CI grep-gate (mirroring GN-5.W3's zero-name-predicate gate) forbidding `from_env` on any
  `codegen`/`passes` regen path closes it hard.
- **The fit-harness precedent exists** as a null result. `scripts/cost-grid-sweep.sh` +
  `docs/benchmarks/cost-weights-sweep.json` (AW-IV.W5.3): a 54-config grid over `{structural,
  alt_per_branch, dispatch_bonus, ref_cost}` on the 4-grammar corpus. **The harness is the
  reusable artifact; its result is the family's central risk (§5).**
- **The residual consumer is proven.** PROVE-12 (`prototypes/PROVE-12/REPORT.md`): const-fold at
  generation time → immediate `and`-mask, +18 generated LOC, cyc/B 0.81-0.98×, ins/B −4.8% mean
  deterministic, regen bit-for-bit. This is the shape every fitted-coefficient output takes.
- **The IR cost lever seam for a *new* decision** (R1 §8.3): `BackendShapePlan.cost_facts`
  (`passes/src/lib.rs:390`), `derive_backend_shape` (`:398`), the N*=3 kernel-crossover threshold
  the SIMD-scan cost lever would encode; `SimdMode`/`SimdSite` (`ir/src/lib.rs:425/430`);
  `CSS_SHUFTI_MIN_SPAN` named-const guard (GN-2.W3, a cost-model value by construction).

---

## 3. Q7 — budget knapsack as constraint (where the loop feeds it)

The profile loop *feeds the objective* the knapsack optimizes; it does not own the solver. The
`bounded_sum` generated-size budget (`loc_budget`, `Cargo.toml` metadata; per-wave
`generated_loc_budget`) folds as a constraint at named sites in the one constraint store
(LOCK-1 1B). Selection-equivalence with the priority cascade **it already replaced** is
witnessed at HEAD: `PriorityStep` P1–P8 (`cost.rs:263`) is retired — `priority_data_role:
"evidence-only"`, `p1_p8_fallback_status: "non-admission"`, `choose_backend_shape_status:
"csp-finalized"` (`decision_csp.rs:174-180`), gated by `gate_decision_csp_cascade_contract`
(`same_wave_consumer_class`, `:172`). The cascade → CSP migration *was* the selection-equivalence
proof. The family's contribution: the fitted cyc/B coefficients become the *objective
coefficients* the knapsack minimizes subject to the size constraint — replacing the hand-set
integer `perf_cost`. One solver home = the vendored `csp-solver` crate; Family 2D is a
*consumer* of it, coordinated across locks by LOCK-1's ONE-CONSTRAINT-STORE.

---

## 4. Q3 — regen determinism under fitted coefficients (the float hazard)

The tie-break is total *today* because the cost is an integer tuple with a content-hash suffix.
A naive profile fit reintroduces non-determinism two ways, both with concrete fences:

1. **Float ties.** A fitted `f64` coefficient can make two candidates' costs bit-equal
   (`0.115 * n == 0.115 * n`), and float equality is fragile across compilers/rounding. Fence:
   **fit in fixed-point integer milli-units** (the `Measurement::cycles_per_byte_x1000: u64`
   convention already in `cost.rs:311`), keep the cost tuple integer, keep the `candidate_hash`
   suffix. Order stays total; `regen∘regen` byte-identity holds because the coefficient is a
   frozen `u64` const, not a re-measured float.
2. **Greedy-extraction non-monotonicity.** Note first the reassuring HEAD fact: the decision
   nodes are *childless* (`DecisionNode { children: [Id; 0] }`, `backend_egraph.rs:158`), so on
   the backend-selection graph greedy bottom-up degenerates to a plain argmin and **is optimal** —
   the extractor's warning does not bite the decision the loop feeds today. The hazard is latent,
   for a *future* coupled cost. `extract.rs` is explicitly the egg greedy bottom-up
   extractor and its docstring warns it is *not optimal for sharing-dependent cost models*
   (`extract.rs:9-11`). A profile coefficient that depends on *shared subterm reuse* (e.g., "this
   kernel is cheaper the second time it's inlined") is non-monotone and silently breaks greedy
   extraction. Fence: **the fitted cost must be per-node-local and monotone** (child costs sum
   upward, `AstSize` shape, `extract.rs:104`); a sharing-dependent coefficient is out of scope
   for greedy extraction and would demand ILP extraction (egg/egglog open problem — cite §6). Keep
   the loop monotone or it is not this extractor's to feed.

Make the *asserted* replay real, cheaply. `determinism_replay_status`,
`rewrite_order_replay_count: 2`, `rewrite_order_variance_pct: 0` are hardcoded string/integer
literals today (`backend_egraph.rs:139-141`). The decision graph is ≤5 childless nodes, so the
gate can actually *compute* replay: run the argmin twice under two candidate-insertion orders
(forward and reversed) and assert the two `selection_trace_hash` values (`backend_egraph.rs:142`,
which already sorts its rows before hashing) are equal. Equality → the literal `"pass"` becomes a
computed result; inequality → RED, proving a fitted coefficient did not smuggle order-dependence
in. This is the concrete artifact behind Q3's determinism claim, not a promise.

Determinism budget on the fit itself: the freshness gate compares **ins/B** (deterministic to
<0.2% per PROVE-12 §4), never cyc/B (load-noisy). cyc/B is the ratio-certified directional check;
ins/B is the load-proof floor the gate keys on. The quiet-host cyc/B re-capture (GN-6 carried
residual) is where absolute coefficients get their final stamp.

---

## 5. Named risks & counterexamples (the honest negative results)

- **RISK-1 — the multiplicity-1 wall (BINDING, the family's gating dependency).** The AW-IV.W5.3
  sweep is a *null result*: across all 54 configs the DTA state count was **identical** for every
  grammar (JSON 51, BBNF 476, CSS L4 2875, Sheets 191). Cause, verbatim: "the current e-graph
  rewrite set … produces e-classes with at most one semantically-distinct canonical form … so
  the cost model has no choice to exercise" (`cost_weights.rs:154-161`). **A fitted coefficient
  cannot flip a decision that has no alternatives.** This directly answers Q8(a) with a
  counterexample: *on the fleet at HEAD, no fitted coefficient flips any per-site decision,
  because extraction multiplicity ≈ 1.* The closed loop is therefore a consumer-less substrate
  UNLESS a real competing-site choice exists first. **Family 2D must co-land with the first
  decision that has ≥2 real survivors** — the SIMD-scan N*=3 kernel crossover
  (`passes/src/lib.rs:398`), the `CSS_SHUFTI_MIN_SPAN` span-guard (GN-2.W3), or an
  offset-tape-vs-sink choice — introduced by the candidate-generating families (widening
  rewrites, IR-modeled sites). The loop's architecture is sound; its *bite* is downstream of
  candidate multiplicity. Shipping the fit harness without a live decision violates Y.13.
  - **XL-F characterization (evidence, not assumption).** The GN-2.W1 scan-lowering predicate
    (`passes::derive_recognizers` emitting `SimdMode::Prefilter`, `passes/src/lib.rs:338`,
    `SPEC.md §2 GN-2.W1`) stands to the profile loop in a **wraps** relation, not *replaces* and
    not *waits-on*. Evidence: the predicate is *candidate generation / legality* — it decides
    whether a self-loop scan over a member set is eligible to emit a second `SimdMode` variant
    (`ir/src/lib.rs:425`, today only `Exact`); the profile loop is *objective / selection* — the
    fitted `perf_cost` ranks `Exact` vs `Prefilter` at the N* crossover (`backend_egraph.rs:170`
    reads the slot; `decision_csp.rs:247` shows it vacant at `0`). Neither subsumes the other: the
    predicate cannot pick (it only manufactures the second survivor that RISK-1 requires), and the
    loop cannot generate (a coefficient over one candidate flips nothing). So the loop *wraps* the
    predicate's output — it is precisely the consumer that turns the predicate's new-found
    multiplicity into a measured selection. This is also why the two must co-land: the predicate is
    RISK-1's cure and the loop is the predicate's objective.
- **RISK-2 — attribution underdetermination (Q8).** N coefficients cannot be fit from one
  whole-grammar cyc/B. Mitigation is structural, not statistical: the A/B-differential isolates
  one lever per paired arm (§1 Phase A). The ceiling on fittable coefficients = the count of
  independently-toggleable levers with a paired canon bench. Disclose it; do not regress a
  confounded whole.
- **RISK-3 — the CPI/latency hazard (PROVE-02 is LAW).** A coefficient fitted on cyc/B can be
  latency-bound and non-transferable: PROVE-02's digit kernel was −48% instructions yet CPI
  inverted 0.13→0.30 and *regressed the product lane*. A fit that optimizes ins/B blindly can
  pick a lever that loses on cyc/B. Fence: fit *and validate* — ins/B leads (deterministic),
  cyc/B is the certified check; a coefficient whose ins/B win does not carry to a cyc/B win is
  rejected, not committed.
- **RISK-4 — freshness-gate flapping.** A gate that re-fits cyc/B on a busy CI host goes RED
  spuriously (load noise). Fence: gate on ins/B only (§4). Loadavg per window recorded; the gate
  is a *hash/CI-band* check on deterministic instruction counts, not a wall-clock threshold.
- **RISK-5 — env leak into regen.** If any regen path reads `BBNF_COST_*`, `regen∘regen` is not
  reproducible across shells. Fence: the CI grep-gate of §2 (`from_env` forbidden on regen paths;
  today only the runtime `CapacityPlan::from_env` at `json_templates/parser.rs:17` calls it, and
  that is not regen).
- **RISK-6 — committed table drift from the code that reads it.** If `cost_formula_version` is
  bumped but a consumer still reads an old field, the fact stream lies. Fence: `cost_formula_version`
  is asserted equal across `ActiveCostFacts` and the emit-spine consumer in a born-RED contract
  gate (the pattern of `gate_decision_csp_cascade_contract` already exists).

---

## 6. Prior art (citations)

- **AutoFDO / `-fprofile-sample-use`** (Chen, Li, Moseley et al., CGO 2016; LLVM SampleProfile):
  committed sample profile (`.prof`), compiler reads deterministically — the exact "commit the
  measurement, regen is a pure function of it" shape. **`-fprofile-generate`/`-fprofile-use`**
  (`.profdata` via `llvm-profdata merge`) is the instrumented variant.
- **`-Wprofile-instr-out-of-date` / function-hash staleness** (Clang/LLVM `InstrProfReader`): the
  profile is stale when the CFG hash changed — the literal model for the freshness gate (Phase C).
- **BOLT** (Panchenko, Auler, Nell, Ottoni, CGO 2019) and **Propeller** (Google): post-link
  reoptimization from a committed `perf` fdata file; deterministic given the committed input.
- **rustc MIR inline cost model** (`compiler/rustc_mir_transform/src/inline.rs`, `InlineThreshold`,
  `CostChecker`): hand-tuned *integer* thresholds — the current-state analog of the hand-set
  `perf_cost` this family replaces with a fitted table.
- **egg / egglog greedy vs optimal extraction** (Willsey et al., POPL 2021; the "extraction with
  sharing is NP-hard" folklore, and ILP/`SmoothE`-style optimal extractors): grounds RISK-4's
  monotonicity constraint — `extract.rs` is greedy bottom-up and only sound for monotone,
  per-node-local costs; a profile coefficient must respect that or move to ILP.
- **Ithemal / uops.info / BHive** (Mendis, Renda, Amarasinghe, Carbin, ICML 2019): measured
  basic-block throughput cost models — the *learned* analog. Family 2D deliberately chooses the
  committed-const table over a learned model (Constitution: KISS, zero-runtime-cost, no
  orthogonal subsystem); the measurement is the input, the fit is a fixed-point vector, not a net.
- **MiniZinc `.dzn` data / OR-Tools committed parameter files**: the model (solver) is separated
  from committed data — the family's fact-table-vs-solver split.
- **Linux `perf record` / Intel VTune → committed profile**: the capture side of Phase A; the
  house harness is `ri_cycles`-based per the macOS profiling standing law (SPEC §4).

---

## 7. What a prototype MUST test to move this family

A single born-RED prototype, base-pinned (`baseCommit`), HEAD named, cold-paired PMU:

1. **Mint or borrow ONE live decision with ≥2 real survivors** (defeat RISK-1): the cleanest is
   the `CSS_SHUFTI_MIN_SPAN` span-guard threshold (GN-2.W3) or the SIMD N*=3 kernel crossover —
   a site where scalar-vs-vector genuinely competes at a value-start position. Prove multiplicity
   ≥ 2 at that class (not the multiplicity-1 wall).
2. **Fit its coefficient** from a paired A/B canon-bench sweep, ins/B-led, in fixed-point
   milli-cyc/B (reuse `cost-grid-sweep.sh` shape; emit into a `CALIBRATED_WEIGHTS`-style const).
3. **Show the fit FLIPS the decision** (Q8a): the fitted coefficient selects the *other* survivor
   than the hand-set integer default at a real value-start position.
4. **Show the flip WINS cold-paired** (Q8b): the flipped selection beats the default lane on the
   canon bench, ins/B strictly lower and cyc/B CI-certified in the same direction across ≥3
   windows (defeat RISK-3: ins/B win must carry to cyc/B).
5. **Drive the freshness gate RED on mutation** (Q8c): mutate the grammar (add a feature that
   shifts the fitted value beyond its CI band) and show the Phase-C gate flips RED demanding a
   `cost_formula_version` re-commit.
6. **Prove `regen∘regen` byte-identity** with the committed coefficient (determinism), and the
   `from_env` grep-gate GREEN (env fenced from regen).

If step 1 cannot find a real ≥2-survivor decision anywhere in the fleet, the honest finding is
**SPEC-ONLY**: the profile loop is architecturally complete but has no consumer until a
candidate-generating family lands one — a named, disclosed dependency, not a silent defer.
