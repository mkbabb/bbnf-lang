# LOCK-2 · FAMILY 2C — CSP-CENTRAL (the decision driver IS a CSP)

*The graph-opt engine, developed at the CSP center: variables are per-site lowering
choices, constraints are legality + the generated-size budget, the objective is a cost
lattice fed by PMU facts; e-graphs only manufacture candidates. Pointer-dense; every
claim carries a `path:line` or a citation. Concrete-artifact / SPEC-ONLY labelled.*

---

## 0. The thesis in one line, and where it stands today

The CSP is the arbiter; the e-graph is a candidate factory. **The engine already exists
and is production-grade; the MODEL that would make it decide is a rubber-stamp.** The whole
of family 2C is the excision of that stamp and the erection of a real per-site CSP over a
budget-coupled cost lattice — turning a provenance schema that already *declares* cascade
retirement into code that *performs* it.

**The stamp, exhibited.** `passes/src/decision_csp.rs:29-98` builds one CSP variable per
rule with a hand-rigged cost vector — `0.0` for the already-selected candidate, `1.0` for
every other (`decision_csp.rs:44-47`) — then adds five *unary* `LambdaConstraint`s
(`:52-82`) that merely re-validate the pre-picked candidate, calls `solve_optimized`, and
reports the answer it was handed. The **actual** decision is the P1–P8 priority cascade run
as an e-graph extraction in `passes/src/backend_egraph.rs:60-88`: candidates become
childless `DecisionNode`s (`:151-159`), one `NormalizeDirectSinkCost` rewrite fires
(`:191-237`), and `Extractor::best_node` picks the lexicographically-minimal `DecisionCost`
tuple (`:239-267`). The CSP never overturns it. The schema `DecisionCspFacts`
(`ir/src/cost.rs:202-248`) already asserts the destination — `priority_data_role:
"evidence-only"`, `p1_p8_fallback_status: "non-admission"`,
`legacy_cascade_admission_status: "blocked"`, `choose_backend_shape_status:
"csp-finalized"` (`decision_csp.rs:174-180`) — **but the cascade in `backend_egraph.rs`
still decides.** 2C makes those strings true.

---

## 1. The mechanism, made concrete

### 1.1 Variables — the per-site lowering choices, lifted into the store

Today the only modelled variable is `BackendShape` per rule (5-way:
`EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage`, `ir/src/cost.rs:334-342`). The
charter's *other* levers exist in the IR but are chosen by imperative passes, not solved:

- **prefilter tier / scan mode** — `Recognizer::SimdScan { mode: SimdMode, site: SimdSite }`
  (`ir/src/lib.rs:416-430`; `SimdMode` today only `Exact`, `SimdSite` only `PreEntry`). Set
  by `passes::derive_recognizers` (`passes/src/lib.rs:338`), never solved. GN-2.W1 wants
  `SimdMode::Prefilter` emitted at self-loop scan sites (`GN-2.md:34-36`, SPEC §2 GN-2.W1).
- **capacity plan** — `CapacityPolicy { tiny_string_cap, container_initial_capacity }`
  (`ir/src/cost.rs:326-332`); `CapacityPlan` in `runtime/src/tape/assembler.rs`.
- **residualization depth** — PROVE-12's const-fold/monomorphization depth (delimiter mask
  as `u128` const-generic, `PROVE-12/REPORT.md:44-76`); today a fixed template transform,
  not a per-site knob.

**2C lifts each into a CSP variable** with a `CostFiniteDomain` (`csp-solver/src/domain/
cost_finite.rs:59-107`): value = candidate index, cost = the fitted objective (§1.3). The
variable set is per-site, not per-rule — the granularity at which the levers actually
compete. This is the "IR extension models `SelfLoop`/`Prefilter`" that Q5 asks for; **LOCK-3
owns building the IR node, 2C owns the variable/constraint/objective over it.**

### 1.2 Constraints — legality (unary) + the budget knapsack (the one n-ary edge)

Legality today is five unary predicates (`decision_csp.rs:52-82`): recognizer non-empty,
substrate admissible, simd-vs-collapsed, capacity ≤ 1. These are correct in *kind* but
tautological in *content* because each pins the single pre-chosen variable. Made real over
per-site variables they become genuine domain filters.

The **missing** constraint — and the load-bearing one — is the **generated-size budget as a
`bounded_sum` knapsack**. The budget is real and enforced today only as a post-hoc gate:
`loc_budget 4000` per grammar (`skinny/Cargo.toml` metadata, R1 §1), the `lint-loc` xtask
target (`xtask/src/main.rs:49`), and per-wave `generated_loc_budget` (GN-2.md §1.2 precept
53). Folding it as `sum_i static_size_cost(x_i) ≤ B` couples the otherwise-independent
per-site choices into ONE optimization — a resource-constrained min-cost selection. **This
constraint does not exist in the crate:** `csp-solver/src/constraint/` ships
`all_different`, `not_equal`, `implication`, `lambda`, `all_different_except` — no linear
sum. 2C's concrete substrate build is a `LinearSumLeq` (a.k.a. `bounded_sum`) constraint
with a `revise` that prunes any domain value whose minimum forced contribution would breach
`B` given the other variables' current lower bounds (a knapsack-style propagator, cf. the
`ParentDirtyProp` monotone-`revise` pattern at `egraph/src/csp_scheduler.rs:141-169`).

### 1.3 Objective — the cost lattice, fed by PMU facts

Today the cost is an integer lexicographic tuple: `DecisionCost { freshness_rank, perf_cost,
capacity_cost, static_size_cost, shape_rank, candidate_hash }` (`backend_egraph.rs:239-247`),
all hand-assigned `u32`s. The B&B kernel already consumes an `f64` cost via
`DomainCostEval` (`csp-solver/src/solver/optimize.rs:12-19`) and cost-sorts branches
(`search.rs:430-450`). 2C replaces the hand ranks with a **coefficient fitted from measured
cyc/B**: PROVE-12 (CSS delimiter residual, 0.81–0.98× cyc/B, `PROVE-12/REPORT.md:133-142`),
PROVE-03 (string scan 0.898–0.931×), PROVE-05 (CSS token −14.6…−23.6%) are the calibration
corpus. The fit is **committed as a fact table** (determinism law; the
`ActiveCostFacts.cost_formula_version`/`selection_trace_hash` seam at `cost.rs:174-198` is
the provenance slot), env-source fenced from regen. `static_profile_provider_status:
"static-template-blocker"` (`decision_csp.rs:183`) is the honest admission that no live
provider exists yet — that string is the RED gate 2C turns green.

---

## 2. The substrate — what to build, on what

**Reuse (production-grade, already there).** The unified B&B search kernel
`csp-solver/src/solver/search.rs:456-516` (`branch_and_bound`): node-budget guard
(`:212-217`), AC-3 / forward-check / AC-FC pruning (`:136-173`), optimistic-bound cutoff
(`:388-398`, `node_prune` at `:423-428`), cost-sorted value ordering (`:430-450`).
`SolveConfig { pruning, ordering, max_solutions, optimization_mode: MinimizeCost,
node_budget }` (`decision_csp.rs:85-97`). `CostFiniteDomain` with cached `min_cost`
(`cost_finite.rs:164-175`) for cheap bounds. `LambdaConstraint` for the unary legality
predicates. This is the ONE solver home the constitution mandates (Constitution ii/§1B
"one-constraint-store"; the crate is shared with LOCK-1) — 2C adds no second engine.

**Build (net-new, small).**
1. `csp-solver/src/constraint/linear_sum.rs` — the `bounded_sum`/`LinearSumLeq` knapsack
   propagator (§1.2). This is the single genuinely-new mechanism.
2. `passes/src/decision_csp.rs` rewrite — real per-site variables, real costs, the budget
   edge; **delete the cost-0/cost-1 rig** (`:44-47`) and the pre-selection lookup
   (`:34-42`).
3. `passes/src/backend_egraph.rs` — demote the cascade extraction to a *candidate
   generator* (its e-graph + `NormalizeDirectSinkCost` rewrite stay as the widening/
   normalizing candidate factory) and route selection through the CSP. `select`
   (`:36`) returns candidates; `finalize_rule` (`decision_csp.rs:16`) becomes the arbiter,
   not the stamp.
4. A fitted cost fact-table + loader (§1.3), committed, regen-fenced.

---

## 3. Answers to the steering questions (2C's evidence)

**Q5 — IR-modelled competing sites.** At HEAD the fleet models exactly ONE competing axis
in the IR (`BackendShape` per rule); the scan/self-loop axis lives as `SimdMode`/`SimdSite`
enums *set imperatively*, not as solved variables (`ir/src/lib.rs:416-430`;
`passes/src/lib.rs:338`). So the honest state: **≥2 IR-modelled competing sites with real
binding costs do NOT yet exist** — GN-2.W1 creates the first (`SimdMode::Prefilter` at
self-loop sites). The IR extension that models them is LOCK-3's `SelfLoop`/`Prefilter` node;
2C's obligation is the CSP variable + static-size + cyc/B cost over it. **Prototype must
prove at least one grammar where prefilter-vs-exact and a capacity choice bind
simultaneously under the budget** (candidates: JSON string self-loop; CSS component-value
scan, PROVE-12's site).

**Q6 — Solver shape (PBQP vs B&B vs e-graph extraction).** The decision graph is
*near-tree*: per-site choices are independent UNARY today; the only coupling is (a) the
global budget knapsack (n-ary, NOT pairwise) and (b) context-sensitivity coherence edges — a
sub-rule reached under a sink-only caller AND an offset-tape caller must pick ONE shape (Q4),
a genuine pairwise equality edge. **PBQP** (Scholz & Eckstein 2002; SSA regalloc,
Buchwald & Zwinkau 2011; LLVM `RegAllocPBQP`; DNN primitive selection, Barrett et al. CGO
2018) is *exactly* per-node discrete choice with pairwise cost matrices, NP-complete,
solved by a reduction heuristic (R0/R1/R2 + heuristic RN). **The verdict for THIS graph:**
PBQP earns its pairwise cost-matrix machinery only when coherence edges are DENSE; on a fleet
where multiplicity ≈ 1 and edges are sparse the CSP degenerates to per-site argmin and PBQP
is over-apparatus. The budget knapsack is *not* pairwise, so the problem is not pure PBQP —
it is **constrained min-cost selection = exactly what the existing B&B kernel solves**
(`branch_and_bound` + `LinearSumLeq`). *Recommendation for 2C: keep the generic B&B kernel;
adopt PBQP only if Q4/Q5 return dense coherence edges — and prove it head-to-head on
solve-µs, not asserted.* E-graph DAG extraction is separately NP-complete (Stepp/lobste.rs;
"E-graphs as Circuits, treewidth", Goharshady et al. 2024) and cannot express the budget
side-constraint at all — which is precisely why 2C keeps e-graphs to candidate generation
and moves selection into the CSP.

**Q7 — Budget knapsack & selection-equivalence.** Folding `sum static_size_cost ≤ B` as a
constraint must preserve **selection-equivalence with the P1–P8 cascade it replaces**
(`PriorityStep::ALL`, `ir/src/cost.rs:275-286`). The oracle is concrete and already wired:
**`regen∘regen` byte-identity** (the `check-json`/`check-css-l4-declaration-values` xtask
gates, R1 §5; `selection_trace_hash` at `cost.rs:196`). Differential: run cascade-select and
CSP-select over every fleet rule; assert identical `BackendShape` where the budget is
non-binding, and a *named, byte-diffed* divergence where it binds (the cascade has no budget
awareness, so a binding budget is where the CSP legitimately *improves* on it — that
divergence is the win, not a regression). ONE solver home: the csp-solver crate; the budget
store is shared with LOCK-1's constraint store per Constitution §1B.

**Q8 — Profile-fed cost.** A cyc/B-fitted coefficient can (a) flip a real decision only where
two candidates are legal and the fit reverses their static-rank order — the prototype must
EXHIBIT one such site, not assume it; (b) the win must be cold-paired at real value-start
positions (PROVE-12's paired base-vs-mod interleave, `REPORT.md:124-151`, is the method);
(c) a freshness gate goes RED on `.bbnf` mutation via `selected_cost_freshness`/
`candidate_cost_stale_rate_bps` (`cost.rs:184,181`). **Attribution under whole-grammar
underdetermination:** whole-grammar cyc/B cannot separate per-feature coefficients — resolve
by **A/B-differential** (preserve baseline binary, mutate one lever, interleave — the exact
PROVE-12 protocol). The fit **supersedes** the hand ranks `perf_cost`/`shape_rank`
(`backend_egraph.rs:239-247`); env-source is fenced from regen by committing the fit as a
fact table keyed by `cost_formula_version` (`cost.rs:174`) — regen reads the table, never the
environment. This is the `DecisionCspFacts` `static_profile_provider_status` transition from
`"static-template-blocker"` to a live provider.

---

## 4. Risks & counterexamples

- **The degeneracy risk (primary).** If saturated multiplicity ≈ 1 and coherence edges are
  empty (Q1/Q4 may confirm), the CSP has nothing to decide — each variable is a singleton
  argmin and `branch_and_bound` reduces to a sort. Then 2C's value is *architectural*
  (cascade retirement, one provenance, budget-awareness) not *algorithmic*. The report must
  say so honestly rather than manufacture a decision. **Counterexample sought:** any fleet
  rule where the budget forces a non-greedy pick (a cheap-per-site but budget-hungry
  candidate rejected in favor of a globally-cheaper set) — that is the *only* case where the
  knapsack coupling earns the solver.
- **Selection-equivalence break masquerading as a win.** A CSP that diverges from the cascade
  must be diffed byte-for-byte; an unexplained divergence is a bug, not an optimization. The
  `regen∘regen` gate is the fence (feedback: changed-flag-convergence, clean-regen-discipline).
- **Determinism.** HashMap iteration is inadmissible for tie-break (Q3). The CSP's value
  ordering is cost-sorted then must fall to a **total tie-break** — use the existing
  `candidate_hash` (FNV, `backend_egraph.rs:304-311`) as the final key, never map order.
  `branch_and_bound` sorts by `f64` cost then keeps insertion order (`search.rs:500-514`);
  ties must be broken by the stable hash before that sort, or replay variance (`cost.rs:194`)
  goes non-zero.
- **Per-parse budget.** The CSP runs at *compile/regen* time, not per-parse (candidate count
  is per-rule, ≤ 5 shapes × a few site knobs), so the ~230 µs per-parse budget (Q3) is not at
  risk from the solver itself; the node-budget guard (`search.rs:212`, default 10 000 in
  `decision_csp.rs:14`) bounds solve cost. The teardown cost is the e-graph candidate
  factory's, measured in `egraph_memory_peak_bytes` (`cost.rs:191`).
- **PBQP premature adoption.** Building PBQP cost matrices before Q4 proves dense coherence
  edges is apparatus without a consumer (Constitution viii, consumer-less substrate
  forbidden). Ship the B&B path; bank PBQP behind an evidence gate.

---

## 5. What a prototype must test to move 2C

1. **Multiplicity/edge census** (feeds Q1/Q4/Q6): instrument `backend_egraph::select` and the
   recognizer pass to log, per fleet rule, `#legal candidates` and any sub-rule reached under
   ≥2 caller shapes. Deliverable: a table proving (or refuting) that a real decision exists.
2. **The `LinearSumLeq` knapsack + a binding instance**: add the constraint to csp-solver,
   author/find one grammar where `sum static_size ≤ B` forces a non-greedy pick, and show the
   CSP selects the globally-cheaper set while the cascade does not. *This is the single
   experiment that proves the CSP-central thesis has teeth.*
3. **Selection-equivalence differential**: cascade-select vs CSP-select over json + css_l4 +
   sheets; assert `regen∘regen` byte-identity where budget non-binding; name every divergence.
4. **Profile flip**: fit a coefficient from PROVE-12/03/05 cyc/B, exhibit ONE site where it
   reverses the static-rank pick, and cold-pair-measure the resulting parser (PROVE-12
   protocol) to confirm the flip is a real win, not noise.
5. **PBQP head-to-head (conditional on #1 returning dense edges)**: model the coherence-edge
   subgraph as PBQP, solve with a reduction heuristic, compare solve-µs and optimality against
   `branch_and_bound` on the same instance. Wins stay; a loss is a named negative result and
   PBQP is banked.

---

## 6. Prior art (citations)

- **PBQP:** Scholz & Eckstein, *Register Allocation for Irregular Architectures* (LCTES
  2002) — origin, extends the Quadratic Assignment Problem, NP-complete, heuristic R0/R1/R2+RN
  solver. Buchwald & Zwinkau, *SSA-Based Register Allocation with PBQP* (CC 2011). LLVM
  `RegAllocPBQP`. Barrett et al., *Optimal DNN Primitive Selection with PBQP* (CGO 2018) —
  per-node discrete choice + pairwise transition costs, the closest analogue to per-site
  kernel selection.
- **E-graph extraction hardness:** DAG-cost extraction is NP-complete (Stepp; EGRAPHS
  community); ILP encodings with acyclicity + coverage (He, *Improving Term Extraction with
  Acyclic Constraints*, EGRAPHS 2023); ASP for egg extraction (arXiv 2606.10644); treewidth
  approach (Goharshady et al., *E-Graphs as Circuits*, arXiv 2408.17042). Confirms the budget
  side-constraint is inexpressible in pure extraction — validates 2C's split (e-graph
  generates, CSP selects).
- **Constrained selection / knapsack in CSP:** the `bounded_sum` / linear-sum-≤ propagator is
  standard MiniZinc/Gecode fare; the design mirrors the crate's existing monotone-`revise`
  propagation (`egraph/src/csp_scheduler.rs:141-169`).
- **In-tree substrate:** csp-solver B&B kernel (`search.rs`), `CostFiniteDomain`
  (`cost_finite.rs`), the CSP-backed egraph scheduler (`csp_scheduler.rs`) proving the crate
  already drives saturation via constraint propagation — the isomorphism the constitution's
  one-store clause wants.

---

## 7. Disposition

Family 2C is **architecturally ready, algorithmically unproven**. The engine (B&B kernel,
cost domain, config) is production-grade and shared; the model is a stamp to be excised; the
one net-new mechanism is the `bounded_sum` knapsack constraint. The thesis stands or falls on
**one experiment**: a grammar where the budget forces a non-greedy pick the cascade cannot
make (§5.2). If that instance exists, CSP-central is the decision driver and the cascade
retires (the schema already says so). If it does not, 2C's honest value is architectural
unification and provenance, and the algorithmic claim is banked as SPEC-ONLY until a fleet
grammar makes the budget bind. PBQP is banked behind the coherence-edge evidence gate (Q4).
