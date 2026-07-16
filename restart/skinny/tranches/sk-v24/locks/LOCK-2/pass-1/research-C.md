# LOCK-2 · Family 2C — CSP-CENTRAL (pass 1, research-C)

**The thesis.** The decision driver IS a constraint-satisfaction/optimization problem. Variables
are per-site lowering choices spanning the whole grammar (backend shape, prefilter/kernel tier,
capacity plan, residualization depth). Constraints are legality couplings plus the
generated-size budget. The objective is a cost lattice fed by PMU fact tables. The e-graph is
demoted to a **candidate generator** — it enumerates the per-site alternative set; it does not
select. Selection is the global solve.

This is a genuine architectural inversion of the code at HEAD, where the e-graph selects and the
CSP rubber-stamps. 2C makes the CSP the actual optimizer and gives it the one thing greedy
extraction structurally cannot honor: **a global resource budget that couples sites**.

---

## 1. What exists at HEAD, and why it is the pathology this family cures

The skinny pipeline already contains both organs, wired in the wrong order:

- **Candidate generation + e-graph selection** —
  `skinny/crates/passes/src/backend_egraph.rs`. `backend_candidates`
  (`passes/src/lib.rs:508`) emits ≤5 `BackendCandidate`s per rule (one per `BackendShape`), each
  carrying `perf_cost`, `capacity_cost`, `static_size_cost`, `shape_rank`, a `hard_pruned` flag.
  `select` (`backend_egraph.rs:38`) builds a tiny `EGraph<DecisionNode>`, unions the candidates
  into one class, runs one normalization rewrite (`NormalizeDirectSinkCost`), and extracts the
  cheapest by a **lexicographic tuple** `DecisionCost { freshness, perf, capacity, size, rank,
  hash }` (`backend_egraph.rs:298`). This IS the real selection today.

- **The rubber-stamp CSP** — `skinny/crates/passes/src/decision_csp.rs`. Per rule it builds a
  `Csp<CostFiniteDomain>` with **one variable, five `LambdaConstraint`s, one objective**
  (`decision_csp.rs:49-83`). The cost vector is rigged: the already-selected index costs `0.0`,
  every other candidate `1.0` (`decision_csp.rs:44-48`). The solve cannot change the answer — it
  re-confirms the e-graph's pick and emits `DecisionCspFacts` (`ir/src/cost.rs:206`) with
  `csp_variable_count: 1`, `csp_constraint_count: 5` hardcoded. This is a **consumer-less
  substrate** in the sense the constitution forbids (viii / Y.13): a CSP whose decision nobody
  actually consumes, a status-report organ (vii).

Two facts make the pathology concrete and quotable:
- `perf_cost: 0` is **hardcoded** at every candidate site (`passes/src/lib.rs:585`). The cost
  lattice is presently **unfed by any measurement** — there is no PMU fact table. 2C's objective
  clause is currently vapor.
- The generated-size budget is a **post-hoc lint**, not a constraint: `lint_loc`
  (`xtask/src/main.rs:259`) checks per-crate LOC and generated-JSON ≤ 4000
  (`skinny/Cargo.toml:67` `generated_loc_budget = 4000`) *after* codegen, failing the build if
  overrun. The solver never sees the budget. It cannot trade a cheaper-here lowering against it.

**The cure (the family):** invert. Keep `backend_candidates` as the enumerator (it already
produces the per-site alternative set with cost fields). Delete the lexicographic e-graph
selection and the per-rule stamp. Replace both with **one global `Csp<CostFiniteDomain>` over all
sites**, where the per-rule choice is a variable, the size budget is a genuine coupling
constraint, and `OptimizationMode::MinimizeCost` branch-and-bound picks the globally cheapest
budget-feasible assignment.

---

## 2. The mechanism, made concrete

### 2.1 Variables and domains
One CSP variable per lowering **site**. A site is a rule (backend-shape choice) and, at finer
grain, a scan leaf (prefilter tier), a container (capacity plan), a delimiter test
(residualization depth — PROVE-12). The variable's domain is the candidate index set; its cost
vector is per-candidate cost. `csp-solver`'s `CostFiniteDomain`
(`crates/csp-solver/src/domain/cost_finite.rs`) is purpose-built for exactly this: `i32` values
(with `-1` as a natural "no-lowering/abstain" sentinel), a sorted `(value, cost)` table with
O(log n) `cost()`, and a `Cell`-cached `min_cost()` lower bound the branch-and-bound issues at
every node (`cost_finite.rs:150`). Enum lowering choices channel to `i32` indices.

### 2.2 Constraints — the couplings greedy extraction cannot honor
Three of these bind sites to each other; that binding is the whole justification for a solver
over a per-site argmin.

1. **Global generated-size budget (the crown constraint).** `Σ_site static_size_cost(choice) ≤
   B`, with `B` the `generated_loc_budget` (`Cargo.toml:67`). This is a knapsack/bin-capacity
   resource constraint. Greedy per-rule extraction (`extract.rs` bottom-up, or the lexicographic
   tuple) is **provably budget-blind** — it minimizes each site independently and can only detect
   overrun after the fact (the current `lint_loc`). Only a global solver can pick a
   *slightly-costlier-here* lowering to buy budget for a *much-cheaper-there* one. **This
   constraint is the reason 2C is not decorative.**
2. **Same-substrate-union** (`SameSubstrateUnionFacts`, `ir/src/cost.rs:189`): rules unified into
   one substrate must share a `BackendShape` — an all-equal channel across their variables.
3. **No-double-emit mutual exclusion** (the GN-2.W1 ⇄ PT-1.W5 interlock, `docs/tranches/sk-v24/
   GN-2.md` §2.4): a self-loop scan site lowers to `PrefilterSet::find_member` (bbnf-simd)
   **XOR** `dfa.find` (bbnf-regex), never both. This is a per-site mutual-exclusion constraint —
   exactly a CSP channel, and today enforced only by prose.
4. **Legality guards** already computed as candidate predicates in `decision_csp.rs:56-82`
   (recognizer non-empty, substrate admissible, simd-eligibility, `capacity_cost ≤ 1`) become
   unary domain filters (hard-prune the illegal indices before solve, per
   `backend_egraph.rs:47` `hard_pruned`).

### 2.3 Objective — a cost lattice fed by PMU facts
`OptimizationMode::MinimizeCost` (`csp-solver/src/config.rs:47`) via `solve_optimized`
(`csp/solve.rs:282`) → `branch_and_bound` (`solver/search.rs:456`). The per-candidate cost is
**not** the current `0`; it is read from a committed **cost fact table** keyed by
`(BackendShape, kernel-tier, corpus-class)` → cyc/B, sourced from the §0 certified numbers
(SPEC §0: e.g. prefilter skip-floor 0.115 cyc/B, membership scan ~1.03, CSS residualized
0.81-0.98×). This is where 2C touches 2D but **keeps the facts as frozen committed input** so
regen stays deterministic — the loop is a build-time table read, not a live re-measure.
`CostWeights` (`crates/egraph/src/cost_weights.rs`) already carries the structural knobs
(`static_size_cost` ↔ `structural`, `dispatch_bonus`, `tape_push`, `dispatch_branch/table`); 2C
adds the measured cyc/B axis and feeds it into the domain cost vectors, not a scalar tuple.

### 2.4 The solve shape and the decisive scaling fact
The natural model is a **resource-constrained assignment problem**. Where the size budget is the
only coupling and choices are otherwise independent, it is a linear assignment / min-cost
selection — `csp-solver` already has the closed-form path: `AssignmentBuilder`
(`builder/assignment.rs`) dispatches group-free/pin-free instances to a hand-rolled
**Kuhn-Munkres** O(n³) LAP (`builder/kuhn_munkres.rs`), proven-optimal, microseconds at n=200.
**But the general grouped/coupled CSP B&B is proven-optimal only to n≈15-18 and budget-blows at
n≈20 (~1M nodes)** (`builder/assignment.rs` doc, verbatim). CSS L4 has 2875 DTA states
(`cost_weights.rs` table); a naive global CSP over thousands of sites cannot go through B&B. The
architecture must therefore **decompose**: independent sites solve by closed-form argmin/LAP; the
solver runs only over the *coupled* sub-CSP (the sites the budget or a union actually binds),
which is small. This decomposition is the family's real engineering content, not an afterthought.

---

## 3. Prior art (with citations)

- **Unison** (Lozano, Carlsson, Blindell, Schulte, *"Combinatorial Register Allocation and
  Instruction Scheduling"*, ACM TOPLAS 2019; unison-code.github.io). The flagship: a **constraint
  model** (Gecode/MiniZinc + a portfolio incl. CP-SAT) doing integrated instruction selection,
  register allocation, and scheduling as ONE combinatorial optimization, beating LLVM/GCC on code
  size and often speed. Direct precedent for "the backend decision IS a CSP over per-site choices
  with a global objective." 2C is Unison's thesis applied to parser-generator lowering.
- **LLVM PBQP register allocation** (Scholz & Eckstein, "Register allocation for irregular
  architectures," 2002; Hames & Scholz PBQP solver in LLVM). Per-node choice + **pairwise cost
  coupling** minimized as Partitioned Boolean Quadratic Programming — the exact "per-site
  candidate cost + coupling constraint + cost objective" structure. The size-budget generalizes
  PBQP's pairwise couplings to an n-ary resource constraint.
- **Optimal e-graph extraction as ILP/CP.** The egg/egglog community's settled result: greedy
  bottom-up extraction (`egraph/src/extract.rs`, the "Egg greedy" this repo ships) is **suboptimal
  under sharing and resource constraints**; optimal extraction is an ILP/MaxSAT over the e-graph
  (Coward, Constantinides, Drane, "Automating Constraint-Aware Datapath Optimization," and the
  "Extraction Gym" / `egraph-serialize` ILP extractors, 2023). This is the canonical justification
  for 2C's split: **e-graph generates candidates, CSP/ILP extracts optimally**. Cite as the reason
  the e-graph stays but stops selecting.
- **OR-Tools CP-SAT / MiniZinc global constraints.** The generated-size budget is a `cumulative` /
  `bin_packing` global constraint; the lexicographic priority table (`PriorityStep::ALL`,
  `ir/src/cost.rs:299`) is a lex-objective. These are stock in CP; `csp-solver` lacks them today
  (§4).
- **rustc / MIR:** greedy, no global cost solver — the contrast case. Instruction selection in
  LLVM SelectionDAG is greedy DAG covering with local costs; the negative baseline 2C beats.

---

## 4. Substrate: what is ready, and the one real gap

**Ready in `csp-solver`:** `CostFiniteDomain` (per-value cost, cached lower bound), `Csp<D>`,
`solve_optimized` → `branch_and_bound` (`search.rs:456`), `OptimizationMode::MinimizeCost`,
`SolveConfig { pruning: ForwardChecking|Ac3, ordering: FailFirst, node_budget, max_solutions }`
(`config.rs:54`), `LambdaConstraint`/`AllDifferentExcept`/`ImplicationConstraint`/`NotEqual`
(`constraint/mod.rs:12-17`), GAC + bipartite `matching.rs`, closed-form LAP
(`kuhn_munkres.rs`). `DecisionCspFacts` already reserves the fields
(`csp_variable_count`, `csp_constraint_count`, `csp_objective_count`) for the real N.

**The gap (named substrate need):** there is **no bounded-sum / knapsack / cardinality
constraint** in `constraint/mod.rs` — only the all-different family, implication, not_equal,
lambda. The global size-budget `Σ static_size_cost ≤ B` can be *expressed* by a
`LambdaConstraint` but it will **not propagate a bound**: lambda only checks a full/partial
assignment for legality, it prunes no domain from a running partial sum, so B&B degenerates to
weak forward-checking and hits the n≈18 cliff faster. 2C requires a new **`SumLeq` propagator
with bounds consistency** (prune index `v` from site `i` when `min_prefix + cost_i(v) +
Σ_{j>i} min_cost_j > B`). This is a small, general, colocated addition to the csp-solver
constraint module (LOCK-1's one-constraint-store shares it). It is the single piece of new
substrate this family must build; everything else is rewiring.

**Cost-fact table:** new. The committed `(shape,tier,corpus)→cyc/B` table, sourced from SPEC §0,
lives beside `CostWeights` (`egraph/src/cost_weights.rs` already models "CALIBRATED_WEIGHTS from
a grid sweep" — the same committed-fact pattern) and feeds the domain cost vectors.

---

## 5. Integration seams (path:line)

- **Enumerator stays:** `passes/src/lib.rs:508` `backend_candidates` — keep; it is the candidate
  generator 2C's charter names. Populate its `perf_cost` (today `0`, `:585`) from the cost table.
- **Selection inverts:** `passes/src/lib.rs:504-505` — replace the per-rule
  `backend_egraph::select` + `decision_csp::finalize_rule` pair with a single **global**
  `finalize_all(grammar, all_rules_candidates)` that builds one CSP over every rule's candidate
  set. `backend_egraph::select`'s lexicographic extraction (`backend_egraph.rs:73-88`) is deleted;
  the e-graph's role narrows to candidate enumeration (or is removed here — `backend_candidates`
  already enumerates without it).
- **CSP driver rewritten:** `decision_csp.rs` whole — from 1-var stamp to N-var global optimize;
  `csp_variable_count`/`_constraint_count` (`ir/src/cost.rs:212-213`) become real.
- **Budget seam:** `xtask/src/main.rs:259` `lint_loc` + `Cargo.toml:67` `generated_loc_budget`
  become CSP **inputs** (a `SumLeq` bound), with `lint_loc` demoted to a post-solve assertion
  that must now always pass (the solver guarantees it) — a determinism cross-check.
- **No-double-emit:** the mutual-exclusion constraint lands in the same solve, feeding
  `codegen/src/lower/recognizer/` dispatch (GN-2.W1 file plan, `GN-2.md` §4) — one lowering per
  site by construction, retiring the prose interlock.
- **Residualization depth:** PROVE-12's `find_component_delim<DELIMS>` const-generic
  (`prototypes/PROVE-12/REPORT.md`) is a per-site depth choice — a natural CSP variable once the
  cost table prices "residualize vs runtime slice" (measured 0.81-0.98× cyc/B there).

---

## 6. Named risks and counterexamples

1. **The null-result risk (decisive).** If no coupling constraint actually binds — if the
   per-site argmin already fits the budget and no union forces a shared substrate — the global CSP
   degenerates to independent argmin and **2C is decorative**, the exact fate of the
   `CALIBRATED_WEIGHTS` grid sweep (`cost_weights.rs`: 54 configs, *identical* DTA state counts
   for JSON/BBNF/CSS/Sheets — "the cost model has no choice to exercise"). 2C earns its keep ONLY
   where the budget binds. The prototype's first duty is to exhibit one grammar where it does.
2. **The B&B cliff (n≈15-18).** A monolithic global CSP over thousands of CSS sites budget-blows
   (`assignment.rs` doc). Mitigation is mandatory decomposition: closed-form LAP/argmin for
   independent sites, B&B only over the small coupled sub-CSP. If the coupled set is itself large,
   2C needs a linear-relaxation / Lagrangian bound, not raw B&B — a real scope risk.
3. **Determinism vs best-so-far.** `branch_and_bound` under `budget_exceeded` returns
   *best-so-far*, which is search-order dependent (`assignment.rs` doc; `SolveStats.budget_exceeded`).
   Non-determinism breaks regen bit-for-bit (a hard law, GN-1 gate). 2C must stay in the
   proven-optimal regime or impose a total-order deterministic tie-break; a budget-exceeded solve
   is a build failure, not a silent best-effort.
4. **Cost-fact staleness.** The committed cyc/B table is frozen; if a kernel changes without a
   re-capture the objective misranks. Guard: the table is provenance-stamped (like §0) and a stale
   entry is a gate, mirroring `candidate_cost_stale_rate_bps` (`ir/src/cost.rs:180`).

---

## 7. What a prototype must test to move this family

1. **Prove the budget binds.** Construct or find one grammar (CSS L4 is the richest surface,
   2875 states) where the unconstrained per-site argmin **exceeds** `generated_loc_budget`, so the
   CSP is forced to pick a cheaper-size / costlier-cyc lowering somewhere. Show a **concrete
   assignment the greedy lexicographic extractor cannot produce** — different sites, lower total
   objective, budget-feasible. Without this, the family is a null result and must say so (vii).
2. **Build and measure the `SumLeq` propagator.** Implement bounds-consistent `Σ ≤ B` in
   `csp-solver/constraint/`, and show it keeps the coupled solve under `node_budget` on the target
   grammar (i.e., it prunes enough to dodge the n≈18 cliff). Compare against the `LambdaConstraint`
   encoding to quantify the propagation win.
3. **Measure the selected set on a canon bench.** The CSP-selected lowering set vs the
   e-graph-greedy set, cold paired PMU (SPEC §0 / GN-2 profiling law), on `css_canon_bench` /
   `json_canon_bench`. The objective must be fed by real fact-table costs and the win (or its
   absence) reported as a number, not asserted.
4. **Prove regen determinism.** `regen ∘ regen` byte-identical with the CSP driver installed —
   proven-optimal solve or deterministic tie-break; a budget-exceeded run fails the gate.
5. **Decomposition scale test.** Show the coupled sub-CSP stays small (≤ the B&B-optimal regime)
   after independent sites are peeled to closed-form, across all four canon grammars.
