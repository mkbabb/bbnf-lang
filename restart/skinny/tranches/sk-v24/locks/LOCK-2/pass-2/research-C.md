# LOCK-2 · Family 2C — CSP-CENTRAL: the decision driver IS a constraint problem

*The graph engine as candidate generator, the CSP as the arbiter. Variables are per-site
lowering choices; constraints are legality plus the generated-size budget; the objective is a
cost lattice fed by PMU facts. E-graphs saturate only to enumerate the domain each variable
ranges over. This report develops that thesis to its strongest concrete form.*

---

## 1. The claim, and why the substrate already leans this way

Family 2C asserts that the pipeline's terminal act of choosing — which backend shape, which
kernel, which capacity plan, which prefilter tier, which residualization depth each site takes
— is a **constraint optimization problem over discrete per-site variables**, and that the
e-graph's job ends at producing the candidate set each variable selects from. This is not a
greenfield invention: the extant tower already routes every backend decision through a
`Csp<CostFiniteDomain>` at `passes/src/decision_csp.rs`. But it does so **degenerately** — one
variable, one rule, five lambda constraints that merely *re-validate* a choice the priority
cascade already made. The family's work is to make that CSP **load-bearing**: multi-variable,
budget-coupled, cost-ranked over candidates the e-graph emits, and to retire the 8-step
`PriorityStep` cascade (`ir/src/cost.rs:264`, `passes/src/lib.rs:508-620`) that presently
front-runs it.

**Evidence the CSP is degenerate today.** `solve_rule` (`passes/src/decision_csp.rs:29`) builds
exactly `csp_variable_count: 1` with `csp_constraint_count: 5` (`:142-144`); the sole variable's
cost vector prices the *already-selected* index at `0.0` and every other at `1.0`
(`:44-47`) — the CSP cannot do anything but return the shape `backend_egraph::select` already
picked. Its own facts admit this: `priority_data_role: "evidence-only"`,
`priority_objective_status: "not-used"`, `priority_hard_prune_status: "not-used"`
(`:180-182`). The decision is *made* by the priority cascade in `backend_candidates`
(`passes/src/lib.rs:508`) — `eager_forced` short-circuits everything, then `sink_only`,
`collapsed_stage`, `event_tape`, `offset_tape` fall through fixed `shape_rank` tiers
(`:535-575`) — and the CSP is a downstream notary. Family 2C's thesis is that this ordering is
inverted: the cascade should become the *candidate generator's admissibility filter* and the
CSP should become the *decision*.

---

## 2. The mechanism made concrete

### 2.1 The variable set — per-site, not per-rule

The current CSP has one variable per *rule* ranging over backend shapes. The family's variable
set is **per lowering site**, and each site's domain is heterogeneous:

- **`shape[r]`** ∈ `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` per rule `r`
  (`BackendShape`, `ir/src/lib.rs:361`; `all_backend_shapes()`, `cost.rs:334`). This is the
  one variable the tower models today.
- **`kernel[s]`** ∈ `{Scalar, NeonTbl4, …}` per classify/scan site `s` — the cost-driven
  dispatch switch `enum SelectedBackend` at `bbnf-simd/dispatch.rs:11-16`, gated by
  `lo6_table_admissible` (`dispatch.rs:101`). Presently a runtime `OnceLock` selection
  (`dispatch.rs:58`), never a compile-time solved variable.
- **`prefilter[s]`** ∈ `{Exact, Prefilter{terminators}}` per self-loop scan site — the
  `SimdMode`/`SimdSite` extension enums (`ir/src/lib.rs:425,430`), the seam GN-2.W1 wires
  (`passes::derive_recognizers`, `passes/src/lib.rs:338`).
- **`capacity[r]`** ∈ `{None, tiny_string_cap, container_initial_capacity}` — `CapacityPolicy`
  (`ir/src/cost.rs`), today a boolean `capacity_cost ≤ 1` lambda constraint
  (`decision_csp.rs:79`).
- **`residual_depth[site]`** ∈ `{runtime, const-fold-mask, fully-inlined}` — the
  residualization ladder PROVE-12 proved concrete: `find_component_delim` residualizes a
  runtime `&[u8]` membership to a `const DELIMS: u128` immediate mask, and the `;}`/`:{};`
  monomorphs inline *fully* into their callers (deeper depth). The depth is a genuine discrete
  choice with a measured cost (cyc/B 0.81-0.98×, +18 generated LOC, byte-identical parity) —
  exactly a CSP variable carrying a size penalty on its domain.

Each variable's **domain is the extraction product of an e-graph island** (the 2A/2B seam this
family consumes but does not own): the grammar-tier e-graph (`crates/egraph`, `GrammarCostModel`)
saturates and extraction (`extract.rs::Extractor`) emits the *set* of cost-competitive
survivors, not a single winner. Where multiplicity ≈ 1 (see §5/§8), the domain is a singleton and
the variable is pre-assigned by unit propagation — the CSP degrades gracefully to the e-graph's
own pick, which is the honest answer to directive Q1.

### 2.2 The constraints — legality is unary, coupling is the budget

Three constraint classes, in ascending arity:

1. **Unary legality** (per-variable domain pruning): the admissibility predicates already
   written as Rust — `admits_sink_only` (`passes/src/lib.rs:954`: `contains_direct_build` ∧
   consumer present), `admits_collapsed_stage` (`:959`), `requires_eager_tape`
   (`lib.rs:516`), `lo6_table_admissible` (`dispatch.rs:101`), and the W1 self-loop predicate
   (complement cardinality ∈ [1,16], GN-2.md §4.W1). These become domain filters, executed
   once as AC-3 node-consistency (`crates/csp-solver/src/solver/ac3.rs`), not the five
   post-hoc lambda notaries of `decision_csp.rs:52-81`.
2. **Coherence edges** (binary, sparse): `same_substrate_union` (`LowerCtx.same_substrate_union`,
   `codegen/src/lib.rs:177-184`; a `DecisionCspFacts` field) — rules that must share a tape
   shape because they interleave on one tape. These are the *only* non-unary structural
   constraints, and there are few: the decision graph is **near-tree** (directive Q6's premise).
3. **The budget knapsack** (global, N-ary): `Σ static_size_cost[shape[r]] +
   generated_delta[residual_depth[site]] ≤ generated_loc_budget`. The `static_size_cost`
   weights already exist (`candidate_static_size_cost`, `passes/src/lib.rs:600-608`:
   SinkOnly=1 … EagerTape=5) and the per-wave budget is already normative (GN-2.md §5 ledger:
   W2 ≤ +40, W3 ≤ +25 generated LOC; `generated_loc_budget`, precept 53). This is the one
   constraint that **couples** otherwise-independent site variables.

### 2.3 The objective — a cost lattice, PMU-fed

The objective minimizes `Σ perf_cost[assignment]` where `perf_cost` is a **lattice**, not a
scalar — the substrate already anticipates this. `extract.rs` ships a `Lattice` trait (the
"AA.3 cost lattice marker", `extract.rs:24-40`) with `dominated_by`/`join` and a `Scalar<T>`
newtype, precisely so "future multi-objective extraction can reason about dominance without
collapsing to a scalar first." The family's objective is the Pareto/lexicographic composite of
`(cyc/B, ins/B, generated_LOC)` — cyc/B from PMU captures (§4), ins/B the load-invariant
deterministic axis (GN-2.md §1.2 precepts 43-49), generated_LOC the budget axis.
`CostFiniteDomain` (`crates/csp-solver/src/domain/cost_finite.rs`) supplies the per-value cost
table with O(log n) lookup and a `Cell`-cached `min_cost` for the B&B lower bound — the exact
substrate for a cost-ranked selection variable. `OptimizationMode::MinimizeCost` +
`Pruning::Ac3` + `Ordering::FailFirst` is the solve configuration already in use
(`decision_csp.rs:88-92`).

---

## 3. The solver: PBQP is the right shape, and why (directive Q6)

The decision graph is: **many discrete per-site variables, each with a small domain (2-5),
pairwise cost matrices on the sparse coherence edges, near-tree structure with one global
budget coupling.** This is the *exact* signature of **Partitioned Boolean Quadratic Programming
(PBQP)**, introduced by Scholz & Eckstein (2002) for register allocation and generalized to
optimal instruction/pattern selection.

- **Why PBQP over generic B&B.** PBQP is QAP-restricted: each variable gets a **local cost
  vector** (per-choice cost) and each edge a **cost matrix** (pairwise interaction). On graphs
  that reduce fully by the R0/R1/R2 (degree-0/1/2) reductions — i.e. near-tree instances —
  PBQP solves in **provably linear time and returns the optimum**; only R3+ (irreducible)
  nodes trigger heuristic branching. The register-allocation literature (Hames & Scholz,
  "Nearly Optimal Register Allocation with PBQP") reports the reductions clear the vast
  majority of real instances optimally. The skinny decision graph — independent sites + budget
  coupling + "few coherence edges" — is dominated by R1/R2-reducible nodes, so PBQP returns a
  **certified optimum in linear time** where generic branch-and-bound
  (`csp-solver/src/solver/search.rs:456 branch_and_bound`) explores a search tree whose node
  budget (`NODE_BUDGET: 10_000`, `decision_csp.rs:14`; the assignment B&B blows at n≈15-18,
  `builder/assignment.rs:22-28`) is a cliff.
- **Why PBQP over e-graph extraction as the decider.** E-graph greedy extraction
  (`extract.rs::compute_best`, the Egg bottom-up fixed point at `:134-170`) minimizes a
  *monotone bottom-up* cost — it cannot express the **budget coupling** (a global N-ary
  constraint) or a **pairwise coherence penalty** (two rules sharing a tape). DAG-optimal
  extraction is **NP-complete** (reduction from minimum set cover); ILP formulations "suffer
  significant scalability issues, frequently exceeding realistic time limits." PBQP is the
  *middle path*: it models the pairwise interactions greedy extraction cannot, without the ILP
  blowup. This is the load-bearing separation — **e-graphs generate candidates (the local cost
  vectors), PBQP decides under coupling.**
- **The optimality/complexity guarantee the prototype must confirm.** On the R0-R2-reducible
  subgraph PBQP is exact and linear; the family's obligation is to **measure the reducibility
  fraction** of real fleet decision graphs. If ≥95% of nodes are R2-reducible (the RA
  precedent), the "solver" is essentially unit propagation + O(1) local minimization and the
  ~230 µs compile budget (directive Q3) is untroubled. Precedent for the general approach: LLVM
  ships `RegAllocPBQP.cpp` in production; PBQP solves optimal DNN primitive selection (Anderson
  et al., CGO 2018) — a *selection-under-cost* problem structurally identical to
  backend-shape/kernel selection.

**Recommendation:** implement PBQP's R0/R1/R2 reduction as a specialized pre-solve *inside the
csp-solver crate* that either fully reduces (return certified optimum) or hands the irreducible
residue to the existing `branch_and_bound` kernel. This preserves "one solver home" (directive
Q7) — PBQP is not a rival engine, it is a reduction front-end on the csp-solver.

---

## 4. Profile-fed cost — the coefficient that flips a decision (directive Q8)

The objective's coefficients are not hand-tuned; they are **fitted from measured cyc/B** and
committed as fact tables (the 2D closed loop, which 2C consumes as its objective source).

- **Where a fitted coefficient flips a real decision (Q8a).** The `static_size_cost` ladder
  (`passes/src/lib.rs:600`) is presently *ordinal invention* (SinkOnly=1…EagerTape=5). A cyc/B
  fit replaces it with measured per-shape byte cost. The candidate flip that *can* happen:
  `SinkOnly` vs `OffsetTape` for a rule that `admits_sink_only` — today SinkOnly always wins on
  static-size (cost 1 vs 2), but PROVE-07 measured direct-to-struct fused sink at 0.746-0.786×
  sonic typed *only on thick projections* (twitter's thin projection honestly missed the 2×
  bar, SPEC §0 #07). A fitted coefficient makes the SinkOnly/OffsetTape choice
  **projection-thickness-dependent** — a real per-site flip the ordinal ladder cannot express.
- **Attribution under whole-grammar underdetermination (Q8, the A/B differential).**
  Whole-grammar cyc/B cannot isolate a per-feature coefficient — the canonical identifiability
  problem. The answer is the **paired-differential** the standing law already mandates: build
  two arms differing in exactly one lowering choice, interleave back-to-back, and the cyc/B
  *delta* is the coefficient for that feature. PROVE-12 is the template — it isolated the
  residualization coefficient by interleaving baseline vs const-mask binaries per window
  (0.81-0.98× per corpus, ins/B −4.8/−6.8% deterministic). ins/B is the load-invariant
  identifier (windows agree <0.2%, PROVE-12 §4); cyc/B carries the load noise. The fit reads
  `Measurement { cycles_per_byte_x1000, source: BenchProbe }` (`ir/src/cost.rs`
  `Measurement`/`EvidenceSource`) — the fact-table schema is already in the IR.
- **The freshness gate RED on mutation (Q8c).** `ActiveCostFacts` carries
  `selected_cost_freshness`, `cost_formula_version`, `candidate_cost_stale_rate_bps`
  (`ir/src/cost.rs:173-197`) — the mutation-detection substrate exists. A coefficient fit is
  fenced from regen determinism by committing it as a **versioned fact table** (not read from
  env at regen): `CostConfig::from_env` (`egraph/src/cost_config.rs`) is BENCHMARK-ONLY; the
  committed path reads `CALIBRATED_WEIGHTS` (`cost_weights.rs`). The env source must be **fenced
  from regen** so `regen∘regen` stays byte-identical (directive Q3) — the fit lands as a
  committed const; the env override is a bench affordance that never touches the generated file.
- **Which knobs a fit supersedes.** The fit supersedes: `candidate_static_size_cost`'s ordinal
  ladder, `candidate_capacity_cost`'s boolean (`lib.rs:596`), the `shape_rank` tie-values
  (`lib.rs:535-575`), and the N*=3 kernel-crossover threshold (R1 §8.3). All four are presently
  invented integers; all four are measurable.

---

## 5. The budget knapsack as a constraint (directive Q7)

Folding the generated-size budget as a `bounded_sum` constraint at named sites **preserves
selection-equivalence with the priority cascade only conditionally** — and characterizing that
condition is the family's sharpest result.

- **What exists.** The csp-solver crate has **no linear/sum/knapsack constraint** — its
  constraint library is `all_different`, `all_different_except`, `not_equal`, `implication`,
  `lambda` (`crates/csp-solver/src/constraint/`). `bounded_sum` must be **built** — it is the
  named substrate gap. It belongs in the csp-solver crate (the "one solver home"), as a
  propagator over `Σ w_i·x_i ≤ B` with bounds-consistency filtering (standard knapsack
  propagation: prune any value whose forced minimum contribution exceeds slack).
- **Selection-equivalence characterization.** The priority cascade is a **lexicographic** order
  (P1 dominates P2 dominates …, `PriorityStep::ALL`, `cost.rs:276`); a budget knapsack is a
  **cardinal** constraint. They coincide iff the budget never binds — i.e. the cascade's
  lexicographic winner already satisfies `Σ cost ≤ B`. When the budget *binds* (some corpus's
  cascade-preferred shapes overflow the generated-LOC ledger), knapsack and cascade **diverge**,
  and the knapsack is the correct arbiter (it is the actual engineering constraint precept 53
  enforces). The prototype's obligation: **exhibit a fleet grammar where the budget binds** (the
  cascade over-picks EagerTape/CollapsedStage past the ledger). If none exists, the knapsack is
  inert and selection-equivalence holds trivially — itself a reportable negative result (the
  budget is slack on the current fleet, so the constraint is a *guard*, not a *driver*).
- **One solver home coordination.** The constraint store is shared with LOCK-1's type solver
  (the 1B "one-constraint-store" thesis) — the coordination point is that both the
  type-inference CSP and the decision CSP are `Csp<D>` instances over the *same* csp-solver crate
  (`crates/csp-solver`, vendored at skinny `Cargo.toml:36-37`). The `bounded_sum` propagator,
  once built, serves both.

---

## 6. Context-sensitive extraction and IR-modeled competing sites (Q4, Q5)

- **Q4 — the `admits_sink_only` caller-context conflict.** `admits_sink_only`
  (`passes/src/lib.rs:954`) is `contains_direct_build(expr) ∧ (direct_only_output ∨
  direct_build_consumer)` — it depends on `TargetFeatures`, a **whole-compile** flag, not a
  per-caller context. A sub-rule reached under both a sink-only caller and an offset-tape caller
  would need *two* shapes, but the tower assigns one `BackendShape` per rule (`shape[r]` is
  rule-indexed, not (rule,caller)-indexed). **Audit result:** the conflict is *latent but
  unrealized* — `contains_direct_build` recurses through `CallRule` as leaf-false
  (`lib.rs:970-984`), so a shared sub-rule's admissibility is computed from its *own* body, not
  its callers, and every caller inherits the same shape. This is context-FREE per-class
  extraction by construction. The CSP-central consequence: if a future grammar *does* present the
  conflict (a `->` sub-rule interleaved on both a sink and a tape), the repair is to **split the
  variable** into `shape[r, caller-class]` and add a coherence constraint — an identity-
  preserving duplication, O(callers) blowup, expressible in PBQP as a per-caller node. No such
  grammar is in the current fleet; reported as a *contingent* extension, not a present need
  (honest per constitution vii).
- **Q5 — IR-modeled competing sites.** The IR presently models **one** SIMD site kind
  (`SimdSite::PreEntry`, `ir/src/lib.rs:430`) with **one** mode (`SimdMode::Exact`, `:425`).
  There are **no ≥2 competing scan/self-loop sites with real binding cyc/B costs in the IR
  today** — the `SelfLoop` site and `Prefilter` mode are the GN-2.W1 extension (GN-2.md §4.W1),
  not yet built. So the honest answer: the competing-site variable `prefilter[s]` has domain
  size 1 until W1 lands `SimdSite::SelfLoop{terminators}`. **The IR extension that models
  competing sites is exactly `SimdMode`/`SimdSite`, and LOCK-3 (the IR transposition spine) owns
  building it** — 2C consumes it as a variable domain. This is the CROSS-LOCK dependency: 2C's
  `prefilter`/`kernel` variables are vacuous until LOCK-3 gives the IR the richer `SimdSite` enum
  with per-site static-size + cyc/B facts attached (the `Measurement` vector on
  `RejectedAlternative`, `cost.rs`).

---

## 7. Organ port budget & determinism (directive Q3)

- **Compile budget.** The decision CSP as it stands solves in `csp_solve_us` microseconds
  (`decision_csp.rs:146`) with `NODE_BUDGET: 10_000` — negligible against the ~230 µs
  per-grammar compile the type plane already spends (SPEC §0 #08). A PBQP reduction over a fleet
  grammar's ~50-200 rules with domains ≤5 is R2-dominated and linear; the budget is not at risk.
  The e-graph *build+saturate+extract+teardown* (the 2A organ) is the heavier cost, but it is
  amortized across all variables' domain generation, not per-variable.
- **Determinism (HashMap tie-break inadmissibility).** Extraction's tie-break is
  `improved = cost < *existing` — **strict** less-than (`extract.rs:154`) — so on equal cost the
  **first-seen node wins**, and node order within a class is `class.iter()` insertion order (a
  Vec, deterministic). The `best` map is `FxHashMap` but it is *never iterated for output* — the
  reconstruction walk (`extract.rs:192-201`) is directed by child pointers, not map iteration.
  So extraction is deterministic **iff** `egraph.classes()` yields a stable order; that must be
  audited (under strict-`<` tie-break + deterministic node order the fixed point converges to the
  same result regardless of visitation order — visitation affects *how many* passes, not the
  *result*). For the CSP layer, the total order that makes selection deterministic is: **rank
  candidates by (cost, then a stable candidate id)** — `BackendCandidate.id` is already a
  deterministic `format!("rule-{}-shape-{:?}-priority-{:?}")` (`lib.rs:582`), a total order that
  breaks cost ties without HashMap iteration. `regen∘regen` byte-identity then holds because the
  objective is a committed fact table (§4) and the tie-break is a stable string order.

---

## 8. Named risks & counterexamples

1. **The CSP is decoration, not decision (the strongest risk).** If every fleet variable has
   domain-multiplicity 1 after e-graph saturation (directive Q1), the CSP never *chooses* — it
   notarizes, exactly as `decision_csp.rs` does today. **Counterexample needed:** a fleet
   grammar where ≥2 shapes/kernels survive extraction at a single site with genuinely competing
   cyc/B. If none exists, 2C collapses to "plain cost-ranked canonicalization" and the PBQP
   machinery is unjustified overhead. The prototype MUST measure saturated multiplicity before
   building the solver.
2. **The budget never binds.** §5 — if the generated-LOC ledger is slack on every fleet grammar,
   `bounded_sum` is inert and the knapsack thesis is unfalsified-but-unexercised.
3. **PBQP irreducibility.** If real decision graphs carry dense coherence edges (many
   `same_substrate_union` interleavings), R2 reduction stalls and PBQP degrades to heuristic
   branching — no optimality certificate. Counterexample: a stylesheet grammar where many rules
   share one tape. Must measure the reducibility fraction.
4. **Profile coefficient fails to flip anything (Q8a).** If the fitted cyc/B ladder induces the
   *same* selection as the invented ordinal ladder on all fleet grammars, the closed loop is real
   but idle — a CONFIRMED_DELTA-shaped outcome (the loop works, the flip is absent), reported as
   such.
5. **Cross-lock vacuity.** 2C's `prefilter`/`kernel` variables are singletons until LOCK-3 lands
   the richer `SimdSite` IR (§6, Q5). Building the solver before the IR models competing sites
   yields a solver with nothing to solve — the sequencing risk.

---

## 9. What a prototype must test to move this family

1. **Multiplicity probe (gates everything).** Instrument `extract.rs` to count, per site across
   the json/css/sheets/csv fleet, how many candidates survive within ε of the best cost. Report
   average/max e-class survivor count at child-bearing vs childless nodes. **If max ≈ 1, the
   family is refuted** — report and stop. If ≥2 at real sites, proceed.
2. **`bounded_sum` propagator.** Build the knapsack constraint in
   `crates/csp-solver/src/constraint/bounded_sum.rs` with bounds-consistency `revise`; unit-test
   selection-equivalence with the priority cascade on a synthetic binding-budget instance.
3. **PBQP reduction front-end.** Implement R0/R1/R2 reduction in the csp-solver as a pre-solve;
   measure the reducibility fraction on real fleet decision graphs (target ≥95% R2-reducible for
   a linear-time certified optimum).
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
- **E-graph extraction complexity.** DAG-optimal e-graph extraction is NP-complete (set-cover
  reduction). ILP formulations are exact but scale poorly; treewidth-based extraction (Sun et
  al., *E-Graphs as Circuits, and Optimal Extraction via Treewidth*, 2024) beats ILP for
  treewidth < 10; e-boost (ICCAD 2025) warm-starts ILP with heuristics. These bound *why* 2C
  keeps extraction greedy (candidate generation) and moves coupling into PBQP rather than solving
  optimal extraction directly.
- **Instruction selection as PBQP / QAP** — Blindell, *Survey on Instruction Selection* (2013);
  Lozano & Schulte, *Survey on Combinatorial Register Allocation and Instruction Scheduling*
  (2014) — the combinatorial-optimization framing 2C inherits.

---

## 11. Verdict for the family

CSP-central is **architecturally sound and partially instantiated** — the tower already routes
decisions through a `Csp`, the cost lattice trait exists, the fact-table schema exists, the
paired-differential fit is proven (PROVE-12). Its strongest form: **PBQP reduction over per-site
variables, e-graph extraction as domain generator, a `bounded_sum` knapsack as the one coupling
constraint, coefficients fitted by paired-differential and committed as versioned fact tables.**
Its load-bearing risk is empirical, not architectural: **does saturated multiplicity ever exceed
1 on the real fleet?** Until the multiplicity probe answers yes, the family is a well-founded
hypothesis whose central mechanism (the CSP as *decider*) has no work to do beyond what
cost-ranked extraction already performs. The probe is the gate; the solver is built only after it
clears. Two substrate gaps are named and concrete: the `bounded_sum` propagator (absent from
csp-solver) and the competing-site IR (`SimdSite::SelfLoop`, owned by LOCK-3). Concrete artifacts
throughout; the multiplicity claim is **SPEC-ONLY until the probe runs**.
