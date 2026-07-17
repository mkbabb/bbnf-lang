# LOCK-2 · Family 2D — PROFILE-CLOSED-LOOP (research, pass 2)

Optimization as measured feedback: cost coefficients FITTED offline from cold-paired PMU
captures, committed as a content-addressed const fact table, read (never re-measured) by the
decision driver at regen. The loop closes at *calibrate-time, not regen-time* — regen stays a
pure function of `(grammar, committed-β)`, so `regen∘regen = regen` survives while all load
noise lives in an offline `xtask fit-cost` off the regen path. This is the AutoFDO/BOLT
discipline (committed profile as deterministic INPUT + a staleness gate) transposed onto a
parser generator's cost model. Family 2D owns the COEFFICIENT PLANE beneath whichever candidate
generator decides; it answers where the numbers in the cost lattice come from and why they are
both trustworthy and deterministic. The differentiator no parser generator holds: profiler and
cost model are one circuit — PEG/LR emit fixed automata (no profile), simdjson's thresholds are
hand-tuned magic constants, not fitted; rustc/LLVM have PGO but no *cost-model* fit at the
generator level.

Pass-1 established the loop's shape and its wall: a fitted coefficient is inert unless the
decision it feeds has ≥2 discriminable survivors. Pass-2 must do three things the pass-1 report
could not: (1) name the *exact* decision whose coefficients a fit governs, now that the widening
rewrite is built and confluent but the shipping model cannot discriminate its twin; (2) make the
attribution mechanism concrete under whole-grammar underdetermination (the A/B-differential); (3)
prove the freshness gate can go RED, else the anti-theater claim is itself provenance theater.

---

## 0. The decisive pass-2 finding: the crossover coefficients ALREADY EXIST, calibrated, and their ratio gives N\*=3

The lock's whole crux — "does a *single fixed* calibrated vector pick DISPATCH at large N and
SEQUENTIAL at small N (a structural crossover), not the forbidden `dispatch_bonus` sign-flip" —
turns out **not to require inventing a cost term at all.** The engine's `CostWeights` struct
(`crates/egraph/src/cost_weights.rs:40`) already carries, from Tranche AF.2, exactly the three
coefficients a branch-count-dependent `Alt` cost needs:

```
CALIBRATED_WEIGHTS (cost_weights.rs:175):
  alt_per_branch  = 1.5     // per-branch SEQUENTIAL first-set test
  dispatch_branch = 0.5     // per-branch DISPATCH (O(1) table entry)
  dispatch_table  = 3.0     // fixed DISPATCH setup (jump/first-byte table build)
  dispatch_bonus  = -2.0    // the SCALAR the Alt arm uses today
```

But the `Alt` arm of `GrammarCostModel::cost` (`crates/ir/src/egraph/cost.rs:109-116`) consumes
**only** `alt_per_branch` (slope) and the scalar `dispatch_bonus` (a constant in N):

```rust
GrammarENode::Alt(children, dispatch) => {
    let base = structural + self.weights.alt_per_branch * children.len() as f64 + child_sum;
    if dispatch.is_some() { base + self.weights.dispatch_bonus } else { base }   // <- constant in N
}
```

`dispatch_branch`/`dispatch_table` are wired only to the AF.2 *call/inline* driver, not to the
`Alt` dispatch decision. So `cost(dispatch) − cost(seq) ≡ dispatch_bonus` — a scalar, N-invariant,
whose *sign* alone flips the pick. That is PROTO-1's named FAIL-cause, verbatim, and the fix is a
**two-line rewire**, not a new primitive:

```rust
// SEQUENTIAL(N) = structural + alt_per_branch·N + child_sum
// DISPATCH(N)   = structural + dispatch_table + dispatch_branch·N + child_sum
if dispatch.is_some() {
    structural + self.weights.dispatch_table
        + self.weights.dispatch_branch * children.len() as f64 + child_sum
} else {
    structural + self.weights.alt_per_branch * children.len() as f64 + child_sum
}
```

**With the already-committed AF.2 values this yields a real crossover:**
`N* = dispatch_table / (alt_per_branch − dispatch_branch) = 3.0 / (1.5 − 0.5) = 3.0`.
A 2-branch `Alt` → SEQUENTIAL; an 8-branch `Alt` → DISPATCH; the pick flips at N=3 *from a single
fixed weight vector, from structure not sign.* This is exactly the crossover Brief 1 asks for, and
it is not hypothetical: the coefficients are present and calibrated. **This reframes 2D's blocker.
The blocker is not "no cost term exists" — it is "the existing term is not wired into the Alt arm
and its three coefficients were never fitted to measured DISPATCH-vs-SEQUENTIAL cyc/B."** 2D's
executed fit is precisely: rewire the arm, then fit `{dispatch_table, dispatch_branch,
alt_per_branch}` from PMU so N\* lands where the fleet's real dispatch sites actually pay.

Why the fit matters over the AF.2 defaults: those 3.0/0.5/1.5 were "calibrated to preserve
pre-AF.2 behaviour" (`cost_weights.rs:180-183`) — an author-declared preservation constant, not a
measurement of table-build vs first-set-scan cost at real value-starts. If measured dispatch setup
is cheaper than 3.0× structural, or sequential first-set testing costs more than 1.5× per branch,
N\* shifts and *more* (or fewer) fleet sites flip to DISPATCH; the fit is what earns the number and
the flip.

**Fleet straddle (does N\* bind real sites?).** JSON's hot value dispatch (`json_sink_direct/
dispatch.rs`, "the 91.5% hot leaf", first-byte-keyed `derive_arms`) is a 7-way Alt
(object/array/string/number/true/false/null) → N=7 > 3 → DISPATCH; CSS token/component
alternations are wider still; a 2-branch optional or a binary sum → N=2 < 3 → SEQUENTIAL. The
fleet demonstrably straddles N\*=3, so a fitted move of N\* changes the pick on real sites — the
fork is not synthetic.

---

## 1. The FIT mechanism, made concrete — the A/B-differential that resolves attribution

The pass-1 open question (Q8's hard half): whole-grammar cyc/B underdetermines per-feature
coefficients — a single stylesheet's cyc/B is one scalar against three unknowns. Resolution: a
**controlled 4-arm paired design** that isolates each coefficient with a single toggled lever — the
uops.info methodology (automatically-generated microbenchmarks, one structural variable per arm,
PMU-read) transposed to grammar sites.

Build two child-bearing `Alt`s differing ONLY in branch count — small (N=2) and large (N=8) — and
lower **each** under both shapes. Four arms, measured cold-paired at value-start positions:

| arm | shape | branches | measured |
|---|---|---|---|
| A | SEQUENTIAL | 2 | `s₂` cyc/B |
| B | SEQUENTIAL | 8 | `s₈` |
| C | DISPATCH | 2 | `d₂` |
| D | DISPATCH | 8 | `d₈` |

The three coefficients fall out by finite differences — each isolated by ONE lever:

```
alt_per_branch  = (s₈ − s₂) / 6                                     // SEQ slope: branch lever, one shape
dispatch_branch = (d₈ − d₂) / 6                                     // DISPATCH slope: branch lever, one shape
dispatch_table  = (d₂ − s₂) − (dispatch_branch − alt_per_branch)·2  // shape lever at fixed N
```

Two points per lane × two lanes = **four arms identify three coefficients** with one degree of
freedom left for the CI. This is the ONLY sound attribution when the aggregate metric is a scalar:
never regress on corpus identity (that is a name-gate); regress cyc/B (led by load-proof ins/B)
onto the site's STATIC structural features — here the single feature *branch count N*. The
**attribution ceiling is disclosed as a hard number**: the count of independently-toggleable
structural features that have a paired bench. For the `Alt` decision that count is 1 (branch
count), the model is affine in N, and 4 arms suffice. Adding a second feature (child depth, member
cardinality) requires its own orthogonal lever and its own paired arm — the fit never claims a
coefficient it did not isolate.

**Lead metric = ins/B; certified check = cyc/B (PROVE-02 law).** Coefficients are fitted on
instructions/byte (deterministic, load-proof, windows agree <0.2% — PROVE-12 §4); cyc/B is the
ratio-certified cross-check. An ins/B win that does not carry to cyc/B is rejected, not committed
(the PROVE-02 latency-inversion refutation is why: −37% ins can regress the product lane via a
CPI inversion). This fences load noise out of the fitted number itself: the committed coefficient
is a function of a load-invariant column.

---

## 2. The loop, its three phases, and the determinism invariant

**FIT** (offline `xtask fit-cost`): reads committed PMU capture rows (the
`json_canon_bench`/`css_canon_bench` stdout schema `# schema: PMU corpus lane bytes iters cycles
instructions cyc_per_byte cpi mbps`), runs the §1 differential OLS per decision kind, emits fitted
`{dispatch_table, dispatch_branch, alt_per_branch, …}`. No live bench on the regen path; fitting is
its own gated command.

**COMMIT** (human-gated): write the fitted vector into `CALIBRATED_WEIGHTS` (or a sibling
content-addressed `ir/cost-facts/<grammar>.toml` the const reads), bump `cost_formula_version`
(today `"sk-v13-w6-integer-v1"`, `backend_egraph.rs:8`). The commit is a reviewed diff; the fit's
capture provenance (window count, loadavg band, bench commit) rides in the table.

**FRESHNESS** (CI, born-RED): a `check-calibration` gate recomputes the selection under the
committed vector and compares `selection_trace_hash` (`cost.rs:196`); any drift, or a row older
than its grammar's last canon-bench, goes RED and demands re-fit. This is BOLT's conservative
discard-on-mismatch made a hard gate (Ayupov & Panchenko 2024: discard the profile whenever
basic-block/jump counts mismatch).

**The determinism invariant (load-bearing).** Regen is `pure(grammar, committed-β)`. The loop
closes at calibrate-time; the only stochastic organ (`xtask fit-cost`) never runs on the regen
path. Three fences already correct in the substrate keep this true: cyc/B is stored as **u64
fixed-point ×1000** (`Measurement.cycles_per_byte_x1000`, `cost.rs:311`) — no float-tie
nondeterminism; the extraction tie-break's last coordinate is a stable content hash
(`candidate_hash` FNV-1a suffix — the total order making extraction a total min independent of
HashMap iteration, answering Q3's tie-break); and `from_env` (`cost_config.rs:83`) reads only a
runtime benchmarking probe. The env-fence sharpens in pass-2: the fitted crossover coefficients
`dispatch_branch`/`dispatch_table` are **not even exposed** in the `BBNF_COST_*` surface
(`cost_config.rs:77-82` lists only `ALT_PER_BRANCH`/`DISPATCH_BONUS`/`STRUCTURAL` +
iter/node/parallelism limits), so the crossover cannot be perturbed from the environment at all; a
CI grep-gate forbidding any `from_env` call on a regen code path closes the residual three hard.

---

## 3. The freshness gate CAN go RED — the anti-theater falsifier (Q8c)

The pass-1 report asserted the loop replaces stub provenance strings
(`static_profile_provider_status: "static-template-blocker"`, `decision_csp.rs:183`;
`generated_runtime_diff_status: "absent"`; `row_move_toward_sota_status`) with fact-bearing values.
Pass-2's sharper obligation: a gate that only ever stamps `"pass"`/`0` is itself theater. The
falsifiable test, now concrete because the crossover exists:

Mutate one committed coefficient — push `dispatch_table` from a fitted 3.0 to 6.0. N\* moves from 3
to 6. Every fleet `Alt` with 4–5 branches (present in CSS component alternations) flips
DISPATCH→SEQUENTIAL. That flip changes the extracted candidate → changes `selection_trace_hash`
(`cost.rs:196`) → `check-calibration` reports RED and `regen` emits a byte-different parser. If the
gate does NOT go RED under this mutation, the provenance is a lie and the family is rejected. This
negative control is a prototype deliverable: the gate's ability to FIRE is the product, not the
green stamp.

---

## 4. Integration seams (path:line)

- Engine cost arm to rewire: `crates/ir/src/egraph/cost.rs:109-116` (the `Alt` arm — consume
  `dispatch_table`/`dispatch_branch` instead of scalar `dispatch_bonus`).
- Fitted coefficient home: `crates/egraph/src/cost_weights.rs:175` (`CALIBRATED_WEIGHTS`); the
  54-config null docstring (`:137-172`) records the *prior* null — the loop had no discriminable
  fork; the rewire supplies one, so the sweep is no longer degenerate.
- Coefficient struct (fields already present, Tranche AF.2): `cost_weights.rs:40` `CostWeights`
  (`alt_per_branch`, `dispatch_branch`, `dispatch_table`).
- Provenance schema (fill, don't build): `skinny/crates/ir/src/cost.rs:173` `ActiveCostFacts`
  (`cost_formula_version:175`, `selected_cost_freshness:185`, `determinism_replay_status:193`,
  `rewrite_order_variance_pct:195`, `selection_trace_hash:196`, `candidate_cost_stale_rate_bps:182`)
  and `cost.rs:203` `DecisionCspFacts` (`static_profile_provider_status:243`,
  `generated_runtime_diff_status:245`, `row_move_toward_sota_status:246`).
- Measurement carrier: `cost.rs:306` `Measurement` (`cycles_per_byte_x1000:311`, u64 fixed-point);
  `cost.rs:319` `EvidenceSource` (`BenchProbe`/`RedressBackfill`/`AuthorDeclared`/`StaticAnalysis`).
- Decision consumer: `skinny/crates/passes/src/backend_egraph.rs` (`FORMULA:8`, `perf_cost:21`,
  `selected_cost_freshness:127`) and `decision_csp.rs:183` (stub string → `"fitted-committed-v1"`).
- Fit/gate: NET-NEW `xtask fit-cost` + `xtask check-calibration` (small, colocated); reads the
  canon-bench PMU schema (`bbnf-bench/src/bin/{json,css}_canon_bench`); the fit precedent
  `docs/benchmarks/cost-weights-sweep.json` (methodology preserved).
- Env-fence: `cost_config.rs:74-83` (`from_env`, the `BBNF_COST_*` surface — grep-gate off regen).

**Substrate census:** ~70% present (the AF.2 coefficient fields, the provenance schema, the u64
fixed-point fence, the candidate-hash total order, the canon-bench PMU capture, the fit-precedent
JSON). NET-NEW ~30%: the 2-line `Alt` rewire, the `fit-cost`/`check-calibration` bodies, and the
one executed fit that turns author-preserved 3.0/0.5/1.5 into measured coefficients.

---

## 5. Prior art (SOTA, cited)

- **AutoFDO** (Chen, Li, Moseley, CGO 2016) — sample-based FDO: the profile is a committed,
  deterministic INPUT to codegen, not live measurement; ~85% of instrumented-FDO gains despite
  sampling imprecision. 2D's exact discipline: fit offline, commit, read at build.
  <https://research.google.com/pubs/archive/45290.pdf>
- **Stale Profile Matching** (Ayupov & Panchenko, Meta, 2024) — quantifies staleness (3-mo binary
  <3% stale, 6-mo 12.8%) and BOLT's conservative *discard-on-basic-block/jump-mismatch*. The direct
  model for 2D's freshness gate: a structural mismatch (row older than its grammar's last bench, or
  a changed selection hash) goes RED and forces re-fit rather than silently steering with stale β.
  <https://arxiv.org/pdf/2401.17168>
- **uops.info** (Abel & Reineke, ASPLOS 2019) — infers per-instruction latency/throughput/port
  usage from *automatically-generated microbenchmarks* read by performance counters, isolating one
  quantity per benchmark. Precisely 2D's A/B-differential (§1): controlled paired arms, one
  structural lever each, PMU-read, to identify coefficients an aggregate metric underdetermines.
  <https://arxiv.org/abs/1810.04610>
- **How Good Are Query Optimizers, Really?** (Leis et al., VLDB 2015) — the cautionary precedent:
  tuning CPU cost parameters (scaled by 50×) improved cost/runtime correlation but was *overshadowed*
  by the discriminating input (cardinality). The parser-generator instance of 2D's wall: fitting β
  matters only when the decision structure discriminates — the AW-IV null and PROTO-1 sign-flip are
  our cardinality-dominates-coefficient. This is why 2D must co-land with the §0 crossover (a real
  fork), never ship as coefficients atop an inert decision.
  <https://www.vldb.org/pvldb/vol9/p204-leis.pdf>
- **rustc `-Cprofile-use` / LLVM PGO + BlockFrequencyInfo→MachineBlockPlacement** — a fitted branch
  weight drives a discrete layout fork; version + staleness-check the `.profdata`; builds without it
  fall to static heuristics (our default `CALIBRATED_WEIGHTS` path — graceful degradation).
- **egg/egglog `LpExtractor`** (Willsey POPL'21; "Better Together" 2023) — confirms greedy
  bottom-up extraction is cost-insensitive when a class has one canonical form (the field 2D must
  avoid closing the loop in); measured cost steers rule-firing/saturation budget — our `CspScheduler`
  iter/node caps.

---

## 6. Named risks + the decisive counterexample

- **R1 (primary, the wall — now with a resolution path):** the fitted coefficient flips no
  decision. Pass-1 faced this abstractly; pass-2 answers it: the §0 crossover IS a real fork (N\*=3,
  fleet straddles it). The counterexample 2D must survive is now sharp — *after the Alt rewire and
  the fit, does at least one fleet `Alt` pick DIFFERENTLY under fitted β than under scalar
  `dispatch_bonus`, and does that flipped lowering win cyc/B cold-paired at value-starts?* If the fit
  moves N\* but no fleet Alt's branch count crosses the move, the flip is decorative — RETIRE with
  the disclosed straddle census (the honest null, mirroring AW-IV.W5.3).
- **R2 (the Leis lesson):** the coefficient is dominated by a mis-estimated structural feature. If
  branch count N is the wrong driver (the real cost is first-set *overlap* depth, not arity), the
  affine-in-N model mis-fits and the flip mis-steers. Mitigation: the freshness re-capture (§3)
  catches a flip that regresses cyc/B; the fit discloses per-coefficient R² and refuses to commit a
  coefficient whose lever did not move the measured metric.
- **R3 (determinism leak):** regen runs a bench → byte-identical regen breaks under load. Fenced
  structurally: regen reads the committed const ONLY; `fit-cost` is separate and gated; u64
  fixed-point + candidate-hash total order + `from_env` grep-gate verify replay
  (`determinism_replay_status`/`selection_trace_hash`).
- **R4 (overfit):** a twitter-fitted β mis-steers canada. Mitigation: fit on STRUCTURAL features
  (branch count), never corpus identity; the re-capture gate catches cross-corpus regression; a flip
  must survive on a held-out grammar (aarch64-only, per the standing law).
- **R5 (staleness churn):** every GN-2 kernel edit invalidates rows → re-fit thrash. Mitigation
  (AutoFDO precedent): `candidate_cost_stale_rate_bps` WARNs, stale rows fall to the
  `CALIBRATED_WEIGHTS` default (PGO-without-profile shape) rather than hard-failing the build.

---

## 7. What the prototype MUST test — PROVE-2D, co-landed with the crossover

One prototype, born-RED, decision-grade, in the tree holding `GrammarCostModel` (top-level
`crates/ir/src/egraph/`; heed the three-tree caveat — run in the reconciled skinny tree if the
substrate reconcile lands first). It is the union of 2D's fit with Brief 1's crossover, because
neither is decision-grade alone (a crossover with author-preserved weights is not a *fit*; a fit
with no fork is inert):

1. **Rewire** `cost.rs:109-116` so the `Alt` arm nets `dispatch_table + dispatch_branch·N` against
   `alt_per_branch·N`. Assert, under the *unmodified* AF.2 `CALIBRATED_WEIGHTS`, a 2-branch Alt
   extracts SEQUENTIAL and an 8-branch Alt extracts DISPATCH — the structural crossover from ONE
   fixed vector (not the `dispatch_bonus` sign-flip). Reuse PROTO-1's `ManufactureDispatchAlt` +
   all-narrowing set through `CspScheduler`.
2. **Fit** `{dispatch_table, dispatch_branch, alt_per_branch}` from the §1 four-arm differential:
   lower each of {N=2,N=8}×{SEQ,DISPATCH}, cold-paired PMU (N≥200 iters, ≥3 windows, ins/B
   load-invariant, structural parity before timing) at value-start positions only. Fit on ins/B,
   cross-check cyc/B; disclose the attribution ceiling (features isolated = 1).
3. **Flip proof:** show ≥1 real fleet `Alt` (JSON 7-way value dispatch, or a CSS component
   alternation) picks a DIFFERENT shape under fitted β than under scalar `dispatch_bonus`, that the
   flipped lowering's cyc/B 95% CI excludes 1.00× same-direction at value-starts (born-RED win), and
   that `cargo xtask regen` stays byte-idempotent (const-read only; `regen∘regen`).
4. **Freshness negative control:** mutate `dispatch_table` 3.0→6.0, assert `check-calibration` goes
   RED and `selection_trace_hash` changes (anti-theater proof, §3); drop the candidate-hash tie-break
   coordinate and assert extraction goes non-deterministic (Q3 tie-break control).

**PASS** ⇒ 2D is a live, consumer-bearing loop with a measured flip and a falsifiable gate; it
co-lands as the coefficient plane under whichever generator wins (composable, not exclusive), and
supersedes the author-preserved AF.2 constants and the hand-tuned GN scanner knobs
(`CSS_SHUFTI_MIN_SPAN`, `GATE=16`) as a NAMED supersession the spec declares. **FAIL at step 1**
(no crossover from any single fixed vector) ⇒ extraction-as-decider REFUTED, LOCK-2 collapses to
canonicalization + the `BackendShape` driver and 2D retires with it. **FAIL at step 3** (fit moves
N\* but no fleet Alt crosses, or CI includes 1.00×) ⇒ the fork is decorative; RETIRE with the
disclosed straddle census (the honest AW-IV-shaped null). Decision-grade either way — the single
falsifiable question 2D answers is *does a measured coefficient govern a real decision on a real
grammar*, and the crossover is where, at last, it can.
