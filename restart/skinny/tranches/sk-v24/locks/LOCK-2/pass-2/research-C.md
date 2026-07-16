# LOCK-2 · Family 2C — CSP-CENTRAL: the decision driver IS a constraint problem

*The graph engine as candidate generator, the CSP as the arbiter. Variables are per-site
lowering choices; constraints are legality plus the generated-size budget; the objective is a
cost lattice fed by PMU facts. E-graphs saturate only to enumerate the domain each variable
ranges over. This report develops that thesis to its strongest concrete form, and locates the
one keystone precedent already living inside the solver crate.*

---

## 1. The claim, and the degeneracy it must overturn

Family 2C asserts that the pipeline's terminal act of choosing — which backend shape, which
kernel, which capacity plan, which prefilter tier, which residualization depth each site takes —
is a **constraint optimization problem over discrete per-site variables**, and that the
e-graph's job ends at producing the candidate set each variable selects from. This is not
greenfield invention: the extant tower already routes every backend decision through a
`Csp<CostFiniteDomain>` at `passes/src/decision_csp.rs`. It does so **degenerately**, and the
family's work is to make that CSP load-bearing.

**Evidence the CSP is a notary today.** `solve_rule` (`decision_csp.rs:29`) builds exactly
`csp_variable_count: 1`, `csp_constraint_count: 5` (`:142-143`). The solve config is already the
right one — `Ordering::FailFirst` (`:87`), `OptimizationMode::MinimizeCost` (`:89`),
`node_budget: Some(NODE_BUDGET=10_000)` (`:90,14`) — but the sole variable's cost vector prices
the *already-selected* index at 0 and all others positive, so the CSP can only echo the shape
that `backend_egraph::select` already picked. Its own committed facts confess this:
`priority_data_role: "evidence-only"`, `priority_hard_prune_status: "not-used"`,
`priority_objective_status: "not-used"` (`:178-180`), and the crate's own test pins those exact
strings (`:272-274`). The decision is *made* upstream by an 8-step lexicographic priority
cascade: `PRIORITY_TABLE: [PriorityStep; 8] = PriorityStep::ALL` (`passes/src/lib.rs:473`),
`eager_forced` short-circuiting first (`:515,538-545`), then `P2SinkOnlyConsumer`,
`P3CollapsedStage`, `P4EventTapeAltDensity`, `P7OffsetTapeDefault` falling through fixed
`shape_rank` tiers (`:551-579`). Family 2C's thesis: this ordering is **inverted** — the cascade
should become the candidate generator's admissibility filter, and the CSP should become the
decision.

---

## 2. The mechanism made concrete

### 2.1 The variable set — per lowering site, not per rule

The current CSP has one variable per *rule* over backend shapes. The family's variable set is
**per lowering site**, each with a heterogeneous domain:

- **`shape[r]`** ∈ `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per rule
  (`enum BackendShape`, `ir/src/lib.rs:361`; `all_backend_shapes()`, `cost.rs:334`) — the one
  variable modeled today.
- **`kernel[s]`** ∈ `{Scalar, NeonTbl4, …}` per classify/scan site — the cost-driven dispatch
  switch `enum SelectedBackend` (`bbnf-simd/dispatch.rs:12`), gated by `lo6_table_admissible`
  (`dispatch.rs:101`), presently a runtime `OnceLock` selection (`dispatch.rs:59-60`,
  `select_backend :89`), never a compile-time solved variable.
- **`prefilter[s]`** ∈ `{Exact, Prefilter{terminators}}` per self-loop scan site — the
  `SimdMode`/`SimdSite` enums (`ir/src/lib.rs:425,430`) inside `Recognizer::SimdScan`
  (`:417-422`), the seam GN-2.W1 wires (`passes::derive_recognizers`).
- **`capacity[r]`** ∈ `{None, tiny_string_cap, container_initial_capacity}` — `CapacityPolicy`,
  today a boolean `capacity_cost ≤ 1` lambda notary.
- **`residual_depth[site]`** ∈ `{runtime, const-fold-mask, fully-inlined}` — the residualization
  ladder PROVE-12 proved concrete: `find_component_delim` residualizes a runtime `&[u8]`
  membership to a `const DELIMS: u128` immediate mask, and the `;}`/`:{};` monomorphs inline
  *fully* into callers (a strictly deeper depth), while the shared `";{}"` monomorph survives as
  one symbol (PROVE-12 §3). The depth is a genuine discrete choice with a measured cost (cyc/B
  0.81–0.98×, ins/B −3.7…−6.4% deterministic, +18 generated LOC, byte-identical parity) —
  exactly a CSP variable carrying a size penalty on its domain.

Each variable's **domain is the extraction product of an e-graph island** (the 2A/2B seam this
family consumes but does not own): the grammar-tier e-graph (`crates/egraph`, `GrammarCostModel`)
saturates, and extraction (`extract.rs`, `compute_best` at `:134`) emits the *set* of
cost-competitive survivors, not a single winner. Where multiplicity ≈ 1 (§8), the domain is a
singleton and the variable is pre-assigned by unit propagation — the CSP degrades gracefully to
the e-graph's own pick, the honest answer to directive Q1. This is also 2C's relation to Q2
(widening rewrites): a rule that *manufactures/widens* only ever **enlarges a variable's domain**;
confluence and termination of the widening set are the candidate generator's obligation (2A/2B),
never the arbiter's — 2C requires only that the domain be a finite enumerated set, which
extraction guarantees regardless of how the class was saturated.

### 2.2 The constraints — legality is unary, coupling is the budget

Three constraint classes, ascending arity:

1. **Unary legality** (per-variable domain pruning): admissibility predicates already written as
   Rust — `admits_sink_only` (`passes/src/lib.rs:954`), `admits_collapsed_stage` (`:959`),
   `requires_eager_tape` (`:762`), `lo6_table_admissible` (`dispatch.rs:101`), and the W1
   self-loop predicate (complement cardinality ∈ [1,16], GN-2.md §4.W1). These become domain
   filters run once as AC-3 node-consistency, not the five post-hoc lambda notaries of
   `decision_csp.rs`.
2. **Coherence edges** (binary, sparse): `same_substrate_union`, a
   `HashMap<RuleId, SameSubstrateUnionFacts>` threaded through the whole emit spine
   (`codegen/src/lib.rs:173,199,612,698,1174…`) — rules that must share a tape shape because they
   interleave on one tape. These are the *only* non-unary structural constraints, and there are
   few: the decision graph is **near-tree** (directive Q6's premise).
3. **The budget knapsack** (global, N-ary): `Σ static_size_cost[shape[r]] +
   generated_delta[residual_depth[site]] ≤ generated_loc_budget`. The per-shape weights already
   exist (`candidate_static_size_cost`, `passes/src/lib.rs:603`) and the per-wave budget is
   already normative (GN-2.md §1.2 precept 53: waves carry a `generated_loc_budget`; PROVE-12's
   +18 was checked against it). This is the one constraint that **couples** otherwise-independent
   site variables.

### 2.3 The objective — a cost lattice, PMU-fed

The objective minimizes `Σ perf_cost[assignment]` where `perf_cost` is a **lattice**, not a
scalar — the substrate already anticipates this. `extract.rs` ships a `Lattice` trait (`:36`)
with `dominated_by` (`:40,59`) and a `Scalar<T>` newtype (`:49-64`), plus `Lattice for f64`
(`:66`) and `for usize` (`:74`), documented precisely so "richer Pareto fronts can land later"
(`:27-33`); the extraction cost type is `type Cost: Lattice` (`:89`). The family's objective is
the Pareto/lexicographic composite of `(cyc/B, ins/B, generated_LOC)`: cyc/B from PMU captures
(§4), ins/B the load-invariant deterministic axis (GN-2.md §1.2 precepts 43–49), generated_LOC
the budget axis. `CostFiniteDomain` (`csp-solver/domain/cost_finite.rs:59`) supplies the
per-value cost table with a `Cell`-cached `min_cost` (`:70,164`) that the branch-and-bound solver
reads for its lower bound (`:161`) — the exact substrate for a cost-ranked selection variable.

---

## 3. The solver: PBQP is the right shape, and the crate already proves the pattern (Q6)

The decision graph is: **many discrete per-site variables, each with a small domain (2–5),
pairwise cost matrices on the sparse coherence edges, near-tree structure with one global budget
coupling.** This is the exact signature of **Partitioned Boolean Quadratic Programming (PBQP)**
(Scholz & Eckstein 2002), generalized to optimal instruction/pattern selection.

**The keystone: the crate already ships a reduction-to-closed-form front-end.** The csp-solver's
assignment builder (`csp-solver/builder/assignment.rs`) recognizes that **group-free / pin-free
instances are a pure linear-assignment problem and solves them in closed form by a hand-rolled
Kuhn-Munkres solver — "proven-optimal and never budget-blows"** (`:13-17`); only with pins/groups
does it degrade to branch-and-bound, where `n=20` budget-blows at ~1M nodes
(`DEFAULT_NODE_BUDGET = 1_000_000`, `:26,66`). This is precisely the PBQP architecture in
miniature: recognize the tractable subclass, dispatch it to a linear-time exact reduction,
hand only the irreducible residue to search. PBQP's R0/R1/R2 (degree-0/1/2) reductions are the
same move for the *quadratic* selection problem, and they clear the vast majority of real
register-allocation instances optimally (Hames & Scholz 2006).

- **Why PBQP over generic B&B.** On graphs that reduce fully by R0/R1/R2 — near-tree instances —
  PBQP returns a **certified optimum in linear time**; only irreducible (R3+) nodes trigger
  heuristic branching. The skinny decision graph (independent sites + budget coupling + few
  coherence edges) is R1/R2-dominated, so PBQP is essentially unit propagation + O(1) local
  minimization, where the generic B&B search tree hits the same node-budget cliff the assignment
  builder documents at n=20.
- **Why PBQP over e-graph extraction as decider.** Greedy bottom-up extraction (`compute_best`,
  the Egg fixed point at `extract.rs:134`) minimizes a *monotone bottom-up* cost — it cannot
  express the **budget coupling** (global N-ary) or a **pairwise coherence penalty** (two rules
  on one tape). DAG-optimal extraction is NP-complete (min-set-cover reduction); ILP is exact but
  scales poorly. PBQP is the middle path: it models the pairwise interactions greedy extraction
  cannot, without the ILP blowup. This is the load-bearing separation — **e-graphs generate
  candidates (the local cost vectors), PBQP decides under coupling.**
- **The guarantee the prototype must confirm.** On the R0–R2-reducible subgraph PBQP is exact and
  linear; the family must **measure the reducibility fraction** of real fleet decision graphs. If
  ≥95% of nodes are R2-reducible (the RA precedent), the ~230 µs compile budget (directive Q3) is
  untroubled.

**Recommendation:** implement PBQP's R0/R1/R2 reduction as a specialized pre-solve *inside the
csp-solver crate*, mirroring the assignment builder's Kuhn-Munkres/B&B split — fully reduce
(return certified optimum) or hand the irreducible residue to the existing `branch_and_bound`
kernel. This preserves "one solver home" (directive Q7): PBQP is a reduction front-end on the
csp-solver, not a rival engine, exactly as Kuhn-Munkres already is. Precedent: LLVM ships
`RegAllocPBQP.cpp`; Anderson et al. (CGO 2018) solve optimal DNN primitive selection with PBQP —
a *selection-under-cost* problem structurally identical to backend-shape/kernel selection.

---

## 4. Profile-fed cost — the coefficient that flips a decision (Q8)

The objective's coefficients are **fitted from measured cyc/B** and committed as fact tables (the
2D closed loop, which 2C consumes as its objective source). The fit mechanism already exists in
skeletal form: `cost_weights.rs` ships `CALIBRATED_WEIGHTS` documented as
"grid-sweep-calibrated extraction weights" (`:134`), with the honest note that "non-trivial
calibration … is outside" current scope (`:160`) — i.e. calibration is acknowledged as
present-but-shallow, exactly the gap 2C/2D fill.

- **Where a fitted coefficient flips a real decision (Q8a).** `candidate_static_size_cost`
  (`passes/src/lib.rs:603`) is presently *ordinal invention* (SinkOnly=1 … EagerTape=5). A cyc/B
  fit replaces it with measured per-shape byte cost. The flip that *can* happen: `SinkOnly` vs
  `OffsetTape` for a rule that `admits_sink_only` — today SinkOnly always wins on static-size
  (1 vs 2), but PROVE-07 measured direct-to-struct fused sink at 0.746–0.786× sonic typed *only
  on thick projections* (twitter's thin projection honestly missed the 2× bar, SPEC §0 #07). A
  fitted coefficient makes the SinkOnly/OffsetTape choice **projection-thickness-dependent** — a
  real per-site flip the ordinal ladder cannot express.
- **Attribution under whole-grammar underdetermination (the A/B differential).** Whole-grammar
  cyc/B cannot isolate a per-feature coefficient — the canonical identifiability problem. The
  answer is the **paired-differential** the standing law already mandates: build two arms
  differing in exactly one lowering choice, interleave back-to-back, and the cyc/B *delta* is the
  coefficient. PROVE-12 is the template — baseline vs const-mask binaries interleaved per window
  (N=3×800, 4 corpora × 2 lanes, loadavg 13–19), yielding 0.81–0.98× cyc/B and −3.7…−6.4% ins/B
  *deterministic*. ins/B is the load-invariant identifier (windows agree <0.2%); cyc/B carries
  the load noise. The fit reads `Measurement { cycles_per_byte_x1000: Option<u64>, source:
  EvidenceSource }` (`ir/src/cost.rs:306-319`) inside `evidence: Vec<Measurement>` (`:293`) — the
  fact-table schema is already in the IR. This paired-differential is what *supersedes* the
  crude grid sweep behind `CALIBRATED_WEIGHTS`.
- **The freshness gate RED on mutation (Q8c).** `ActiveCostFacts` carries `cost_formula_version`
  (`cost.rs:175`), `candidate_cost_stale_rate_bps` (`:182`), `selected_cost_freshness` (`:185`) —
  the mutation-detection substrate exists. A fit is fenced from regen determinism by committing it
  as a **versioned const fact table**: `CostConfig::from_env` (`egraph/cost_config.rs:83`) reads
  `BBNF_COST_*` env and is a **benchmark-only affordance** (`:26`); the committed path reads
  `CALIBRATED_WEIGHTS` (`:31,65`). The env source MUST stay fenced from regen so `regen∘regen`
  is byte-identical (directive Q3) — the fit lands as a committed const; the env override never
  touches the generated file.
- **Which knobs a fit supersedes.** `candidate_static_size_cost`'s ordinal ladder, the boolean
  capacity cost, the `shape_rank` tie-values (`lib.rs:551-579`), and the N*=3 kernel-crossover
  threshold (R1 §8.3). All four are presently invented integers; all four are measurable.

---

## 5. The budget knapsack as a constraint (Q7)

Folding the generated-size budget as a `bounded_sum` constraint at named sites **preserves
selection-equivalence with the priority cascade only conditionally** — characterizing that
condition is the family's sharpest result.

- **What exists / the substrate gap.** The csp-solver constraint library is `all_different`,
  `all_different_except`, `not_equal`, `implication`, `lambda` (verified: `ls
  csp-solver/src/constraint/`; grep for `bounded_sum|knapsack|LinearSum` returns **nothing**).
  `bounded_sum` must be **built** — the named substrate gap. It belongs in the csp-solver crate
  (the "one solver home") as a propagator over `Σ w_i·x_i ≤ B` with bounds-consistency filtering:
  prune any value whose forced minimum contribution exceeds slack (standard knapsack propagation).
- **Selection-equivalence characterization.** The priority cascade is **lexicographic** (P1 ⊐ P2
  ⊐ …, `PriorityStep::ALL`, 8 steps); a budget knapsack is **cardinal**. They coincide iff the
  budget never binds — the cascade's lexicographic winner already satisfies `Σ cost ≤ B`. When
  the budget *binds* (some corpus's cascade-preferred shapes overflow the generated-LOC ledger),
  knapsack and cascade **diverge**, and the knapsack is the correct arbiter (it is the actual
  engineering constraint precept 53 enforces). The prototype's obligation: **exhibit a fleet
  grammar where the budget binds** (the cascade over-picks EagerTape/CollapsedStage past the
  ledger). If none exists, the knapsack is inert and selection-equivalence holds trivially — a
  reportable negative result (the budget is slack on the current fleet, so `bounded_sum` is a
  *guard*, not a *driver*).
- **One solver home coordination.** The constraint store is shared with LOCK-1's type solver
  (the 1B "one-constraint-store" thesis): both the type-inference CSP and the decision CSP are
  `Csp<D>` instances over the *same* csp-solver crate (vendored at skinny `Cargo.toml:36-37` as
  `../crates/csc411`). The `bounded_sum` propagator, once built, serves both.

---

## 6. Context-sensitive extraction and IR-modeled competing sites (Q4, Q5)

- **Q4 — the `admits_sink_only` caller-context conflict.** `admits_sink_only`
  (`passes/src/lib.rs:954`) depends on `TargetFeatures`, a **whole-compile** flag, not a
  per-caller context; `contains_direct_build` recurses `CallRule` as leaf-false, so a shared
  sub-rule's admissibility is computed from its *own* body, and every caller inherits the same
  shape. **Audit result: the conflict is latent but unrealized** — extraction is context-FREE per
  class by construction, and `shape[r]` is rule-indexed, not (rule,caller)-indexed. The
  CSP-central consequence: if a future grammar *does* present the conflict (a `->` sub-rule
  interleaved on both a sink and an offset tape), the identity-preserving repair is to **split the
  variable** into `shape[r, caller-class]` and add a coherence constraint — O(callers) blowup,
  expressible in PBQP as per-caller nodes with pairwise edges. No such grammar is in the current
  fleet (json/css/sheets/csv); reported as a *contingent* extension, not a present need (honest
  per constitution vii).
- **Q5 — IR-modeled competing sites.** The IR presently models **one** SIMD site kind
  (`SimdSite`, `ir/src/lib.rs:430`, today only `PreEntry`) with **one** mode (`SimdMode`, `:425`,
  today only `Exact`), inside `Recognizer::SimdScan { mode, alphabet, site }` (`:417-422`). There
  are **no ≥2 competing scan/self-loop sites with real binding cyc/B costs in the IR today** — the
  `SelfLoop` site and `Prefilter` mode are the GN-2.W1 extension (GN-2.md §4.W1, and W1's
  four-hop consumer law §1.2), not yet built. Honest answer: the competing-site variable
  `prefilter[s]` has domain size 1 until W1 lands `SimdSite::SelfLoop{terminators}` with per-site
  static-size + cyc/B facts attached (the `evidence: Vec<Measurement>` vector, `cost.rs:293`).
  **The IR extension that models competing sites is exactly `SimdMode`/`SimdSite`, and LOCK-3 (the
  IR transposition spine) owns building it** — 2C consumes it as a variable domain. This is the
  binding CROSS-LOCK dependency and the sequencing risk (§8.5).

---

## 7. Organ port budget & determinism (Q3)

- **Compile budget.** The decision CSP solves in `csp_solve_us` microseconds
  (`decision_csp.rs:146`, `NODE_BUDGET=10_000`), negligible against the ~230 µs per-grammar
  compile the type plane already spends (SPEC §0 #08). A PBQP reduction over a fleet grammar's
  ~50–200 rules with domains ≤5 is R2-dominated and linear; the assignment builder's Kuhn-Munkres
  precedent shows the tractable subclass never touches the node budget at all. The e-graph
  build+saturate+extract+**teardown** (the 2A organ) is the heavier cost, but it is amortized
  across all variables' domain generation, not per-variable.
- **Determinism (HashMap tie-break inadmissibility).** Extraction's tie-break is strict `<`
  (`improved = cost < *existing`, `extract.rs`), so on equal cost the first-seen node wins and
  node order within a class is `Vec` insertion order (deterministic); the `best` `FxHashMap` is
  never iterated for output — reconstruction follows child pointers. Extraction is deterministic
  **iff** `egraph.classes()` yields a stable order, which the prototype must audit. For the CSP
  layer, the total order that makes selection deterministic is **(cost, then a stable candidate
  id)**: `BackendCandidate.id` is a deterministic `format!("rule-{}-shape-{:?}-priority-{:?}")`
  (`passes/src/lib.rs`), a total order breaking cost ties without HashMap iteration. `regen∘regen`
  byte-identity then holds because the objective is a committed fact table (§4) and the tie-break
  is a stable string order — the same discipline PROVE-12 proved bit-for-bit.

---

## 8. Named risks & counterexamples

1. **The CSP is decoration, not decision (the strongest risk).** If every fleet variable has
   domain-multiplicity 1 after e-graph saturation (Q1), the CSP notarizes rather than chooses —
   exactly as `decision_csp.rs` does today. **Counterexample needed:** a fleet grammar where ≥2
   shapes/kernels survive extraction at a single site with genuinely competing cyc/B. If none
   exists, 2C collapses to "plain cost-ranked canonicalization" and the PBQP machinery is
   unjustified. The multiplicity probe MUST run before the solver is built.
2. **The budget never binds.** §5 — if the generated-LOC ledger is slack on every fleet grammar,
   `bounded_sum` is inert (unfalsified-but-unexercised).
3. **PBQP irreducibility.** If real decision graphs carry dense coherence edges (many
   `same_substrate_union` interleavings — a stylesheet where many rules share one tape), R2
   reduction stalls and PBQP degrades to heuristic branching, no optimality certificate. Must
   measure the reducibility fraction.
4. **Profile coefficient fails to flip anything (Q8a).** If the fitted cyc/B ladder induces the
   *same* selection as the invented ordinal ladder on all fleet grammars, the loop is real but
   idle — a CONFIRMED_DELTA-shaped outcome, reported as such.
5. **Cross-lock vacuity.** 2C's `prefilter`/`kernel` variables are singletons until LOCK-3 lands
   the richer `SimdSite` IR (§6, Q5). Building the solver before the IR models competing sites
   yields a solver with nothing to solve — the sequencing risk; 2C gates on LOCK-3.

---

## 9. What a prototype must test to move this family

1. **Multiplicity probe (gates everything).** Instrument `extract.rs::compute_best` to count, per
   site across json/css/sheets/csv, how many candidates survive within ε of the best cost;
   report average/max survivor count at child-bearing vs childless nodes. **If max ≈ 1, the
   family is refuted — report and stop.** If ≥2 at real sites, proceed.
2. **`bounded_sum` propagator.** Build the knapsack constraint in
   `crates/csp-solver/src/constraint/bounded_sum.rs` with bounds-consistency `revise`; unit-test
   selection-equivalence against the priority cascade on a synthetic binding-budget instance.
3. **PBQP reduction front-end.** Implement R0/R1/R2 reduction in the csp-solver as a pre-solve
   mirroring `builder/assignment.rs`'s Kuhn-Munkres/B&B split; measure the reducibility fraction
   on real fleet decision graphs (target ≥95% R2-reducible for a certified linear-time optimum).
4. **The paired-differential coefficient fit.** Two-arm interleaved bench (PROVE-12 protocol)
   isolating one lowering coefficient (SinkOnly vs OffsetTape on a thin-vs-thick projection),
   proving a cyc/B delta that flips the CSP's selection while `regen∘regen` stays byte-identical.
5. **Determinism audit.** Confirm `egraph.classes()` order stability and stamp a stable
   (cost, candidate-id) total order into the CSP's selection so tie-breaks never read HashMap
   iteration order.

---

## 10. Prior art (citations)

- **PBQP.** B. Scholz, E. Eckstein, *Register Allocation for Irregular Architectures* (2002) —
  the QAP-restricted formulation. L. Hames, B. Scholz, *Nearly Optimal Register Allocation with
  PBQP* (2006) — R0/R1/R2 reductions, near-optimal on real instances. LLVM
  `lib/CodeGen/RegAllocPBQP.cpp` — production implementation. A. Anderson et al., *Optimal DNN
  Primitive Selection with Partitioned Boolean Quadratic Programming*, CGO 2018 — PBQP as
  selection-under-cost, the structural analogue of backend/kernel selection.
- **Assignment / Kuhn-Munkres.** H. W. Kuhn, *The Hungarian Method for the Assignment Problem*
  (1955); J. Munkres (1957) — the closed-form linear-assignment solver the csp-solver already
  ships (`builder/kuhn_munkres.rs`), the in-crate precedent for reduction-then-search.
- **E-graph extraction complexity.** DAG-optimal e-graph extraction is NP-complete (set-cover
  reduction). Sun et al., *E-Graphs as Circuits, and Optimal Extraction via Treewidth* (2024) —
  beats ILP for treewidth < 10; e-boost (ICCAD 2025) warm-starts ILP heuristically. These bound
  *why* 2C keeps extraction greedy (candidate generation) and moves coupling into PBQP rather
  than solving optimal extraction directly.
- **Instruction selection as PBQP / QAP.** Blindell, *Survey on Instruction Selection* (2013);
  Lozano & Schulte, *Survey on Combinatorial Register Allocation and Instruction Scheduling*
  (2014) — the combinatorial-optimization framing 2C inherits.

---

## 11. Verdict for the family

CSP-central is **architecturally sound and partially instantiated** — the tower already routes
decisions through a `Csp`, the cost lattice trait exists (`extract.rs:36`), the fact-table schema
exists (`cost.rs:306`), the paired-differential fit is proven (PROVE-12), and the crate itself
already demonstrates the winning solver architecture: **reduce the tractable subclass to a
linear-time exact solve (Kuhn-Munkres), search only the residue.** Its strongest form: **PBQP
reduction over per-site variables, e-graph extraction as domain generator, a `bounded_sum`
knapsack as the one coupling constraint, coefficients fitted by paired-differential and committed
as versioned fact tables.** Its load-bearing risk is empirical, not architectural: **does
saturated multiplicity ever exceed 1 on the real fleet?** Until the multiplicity probe answers
yes, the family is a well-founded hypothesis whose central mechanism has no work to do beyond
what cost-ranked extraction already performs. The probe is the gate; the solver is built only
after it clears. Two substrate gaps are named and concrete: the `bounded_sum` propagator (absent
from csp-solver) and the competing-site IR (`SimdSite::SelfLoop`, owned by LOCK-3). Concrete
artifacts throughout; the multiplicity claim is **SPEC-ONLY until the probe runs.**
