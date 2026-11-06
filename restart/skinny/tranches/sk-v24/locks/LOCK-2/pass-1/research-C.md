# LOCK-2 / Family 2C — CSP-CENTRAL: the decision driver IS a CSP

**Charter.** The decision driver is a constraint-optimization problem: variables are per-site
lowering choices (kernel, capacity plan, prefilter tier, residualization depth); constraints are
legality plus the generated-size budget; the objective is a cost lattice fed by PMU facts;
e-graphs generate candidates only. This report makes that mechanism concrete against the extant
substrate, shows where the substrate already *claims* it and does not deliver it, cites prior art,
names the risks, and states the one experiment that moves — or refutes — the family.

---

## 1. The extant driver is the inverse of 2C — and already advertises 2C

The pipeline names a CSP as the backend-shape authority, but the code inverts it: the e-graph
*selects* and the CSP *rubber-stamps*.

- `passes/src/backend_egraph.rs:36` `select(rule_id, candidates)` builds an
  `EGraph<DecisionNode, NoAnalysis>` (`:60`), unions every candidate into one class (`:62-70`;
  each `DecisionNode` carries `children: [Id; 0]`, `:158` — a flat forest, no recursion), runs one
  rewrite (`NormalizeDirectSinkCost`, `:191-237`), then an `Extractor` picks the single cheapest
  node (`:84-87` `extractor.best_node(root)`). **The extractor is the decider.**
- `passes/src/decision_csp.rs:29` `solve_rule` then builds a **one-variable** CSP
  (`csp_variable_count: 1`, `:142`) whose winning domain index is *pre-computed* from the
  already-chosen candidate (`:34-42`), with `add_selected_constraint` (`:52`, `:199-212`) forcing
  the variable to that index. Costs are synthetic — `0.0` for the pre-chosen index, `1.0`
  otherwise (`:44-47`). The returned shape is `active.shape` verbatim (`:158`), never the CSP's
  own output. The five constraints (recognizer / substrate / simd / capacity, `:53-81`) evaluate
  legality on the *already-selected* candidate — validity theater, not a search.
- The P1–P8 priority cascade (`ir/src/cost.rs:263` `PriorityStep`) is demoted to
  `priority_data_role: "evidence-only"` (`decision_csp.rs:178`); `choose_backend_shape_status`
  reads `"csp-finalized"` (`:174`). **The codebase already asserts CSP-centrality in its fact
  schema (`ir/src/cost.rs:202` `DecisionCspFacts` — fields for variable/constraint/objective
  counts, `csp_solve_us`, `csp_node_budget`) while the code delivers greedy e-graph extraction
  wrapped in a single-variable confirmation solve.** 2C's strongest form is exactly the closure of
  this gap: the CSP becomes the joint, load-bearing selector; the e-graph is demoted to candidate
  generation — honoring the fact schema the pipeline already emits.

Entry seam: `passes/src/lib.rs:479` `choose_backend_shape` → `:504` `backend_egraph::select` →
`decision_csp::finalize_rule` (`decision_csp.rs:16`). Downstream consumer:
`codegen::lower::rust::lower_to_rust` (`backend_egraph.rs:11`).

---

## 2. The mechanism, concrete

**One global constraint-optimization problem per compile, not per rule.** The per-rule
single-variable model is the defect; the thesis requires cross-site coupling, which a per-rule
model structurally cannot express.

**Variables — one per lowering decision site.** The site taxonomy, each mapped to an extant seam:

| site kind | domain (candidate lowerings) | extant seam |
|---|---|---|
| backend shape / rule | the 5 `BackendShape` (`ir/src/lib.rs:361`) | today's only decision (`backend_egraph.rs`) |
| recognizer / scan site | `{Scalar, PrefilterFindMember, RegexDfaFind}` | `SimdMode {Exact, Prefilter}` × `SimdSite {PreEntry, SelfLoop}` (`ir/src/lib.rs:425`); GN-2.W1 `codegen/src/lower/recognizer/` dispatch; PT-1.W5 `dfa.find` |
| capacity plan / container | `{GrowOnly, Sampled, Exact, OneShotSimd}` | `CapacityPlan` (`runtime/src/tape/assembler.rs`, `from_env` A/B/C/D); `CostFacts.capacity_policy` (`ir/src/cost.rs:12`) |
| residualization depth / site | `{RuntimeSlice, ConstGenericMask, FullyInlined}` | PROVE-12's const-generic `find_component_delim` mask; "12's const-mask generalized to every grammar-derived stop-set" (SPEC GN-5.W4) |

Each variable's **domain is the candidate set the e-graph extracted for that site** — indices into
a `CostFiniteDomain` (`csp-solver/src/domain/cost_finite.rs`), each index priced by the
candidate's scalarized cost as its per-value `f64`. This is "e-graphs only generate candidates"
made literal: saturation + K-best extraction populate domains; the CSP chooses across them.

**Constraints — legality (per-site and coupling).**

- *Exactly-one-lowering per scan site* — GN-2.W1's invariant "emits exactly one lowering per site,
  self-loop `find_member` XOR regex-leaf `dfa.find`, never both" (GN-2 §2.4). Trivial as a per-site
  domain restriction; it *couples* only where two sites must share one kernel instantiation
  (dispatch-table coherence, the first-byte keys in `codegen/src/json_sink_direct/dispatch.rs`) —
  an `AllDifferent`/`Implication` across the sharing group (`csp-solver/src/constraint/`).
- *Prefilter admissibility* — complement cardinality ∈ [1, 16] (GN-2 §7 risk); a domain prune.
- *Same-substrate union* — rules sharing a substrate must pick compatible shapes
  (`ir/src/cost.rs:159` `SameSubstrateUnionFacts`): an all-equal / admissible-set constraint over
  the group. Couples otherwise-independent rule variables.
- **The generated-size budget — the constraint that makes the problem irreducibly a solver.**
  `Σ_site static_size_cost(chosen) ≤ LOC_budget`. Independent per-site argmin — exactly what the
  greedy `Extractor` computes (`egraph/src/extract.rs:134` `compute_best`, bottom-up single-best)
  — **cannot** honor a global budget: the budget couples every site. With it the model is a
  **Multiple-Choice Knapsack Problem** (one must-pick group per site, one global capacity),
  NP-hard, and the standing generated-size-budget law (GN-2 per-wave `generated_loc_budget`; the
  LOC ledger, GN-2 §5) makes the budget *binding*, not decorative. This is the single load-bearing
  justification for a solver over a fold.

**Objective — a cost lattice fed by PMU facts.** Minimize `Σ_site perf_cost(chosen)` with
`perf_cost` populated from the §0 certified PMU tables (PROVE-12 CSS cyc/B 0.81–0.98×, ins/B
−4.8/−6.8%; PROVE-01 skip floor 0.115 cyc/B). Multi-objective is already scaffolded:
`egraph/src/extract.rs:36` `Lattice` + `Scalar<T>` mark the extraction cost as a lattice so
"future multi-objective extraction can reason about dominance"; 2C consumes that seam —
lexicographic (perf, then size-as-constraint, then a deterministic tie-break) or a Pareto frontier.
`CostFiniteDomain::min_cost` (`cost_finite.rs:164`, `Cell`-cached) is the branch-and-bound lower
bound the search already issues per node.

**Solve.** `Csp<CostFiniteDomain>::solve_optimized` → `OptimizationMode::MinimizeCost`
(`csp-solver/src/config.rs:47`) → `branch_and_bound` (`csp-solver/src/solver/search.rs:456`,
tracks `best_cost: f64`), bounded by `node_budget` + cooperative `cancel` (`config.rs:78-85`) for a
hard compile-time ceiling. GAC all-different (Régin, default-ON) serves the union / exactly-one
groups (`csp.add_all_different`, `csp/mod.rs:60`).

**e-graph, demoted to candidate generator.** The e-graph saturates *per-site alternatives* via
rewrites — the residualization ladder (RuntimeSlice → ConstGenericMask → FullyInlined, the PROVE-12
depth chain), kernel-substitution rewrites (Scalar → find_member / dfa.find), capacity-plan
rewrites — under `CspScheduler`, which *already uses a CSP to drive saturation*
(`egraph/src/csp_scheduler.rs`: proof that CSP-drives-e-graph is idiomatic in-tree). Extraction
then enumerates the **K-best / Pareto frontier per class**, not the single best, filling each CSP
variable's domain. The e-graph never picks a winner; the CSP does.

---

## 3. Substrate: present vs. to-build

**Present (reuse verbatim).** `Csp<CostFiniteDomain>` + `solve_optimized`/`branch_and_bound`;
`CostFiniteDomain` with per-value `f64` + cached `min_cost` B&B bound (`cost_finite.rs`);
`LambdaConstraint`, `AllDifferent`, `AllDifferentExcept`, `Implication`, `NotEqual`
(`csp-solver/src/constraint/`); the `DecisionCspFacts` schema threaded end-to-end
(`ir/src/cost.rs:202`); the `CspScheduler` precedent (`egraph/src/csp_scheduler.rs`); the `Lattice`
multi-objective marker (`extract.rs:36`).

**To build (named substrate gaps — each a consumer-bearing addition, Y.13).**

1. **K-best / Pareto extraction.** `Extractor::compute_best` (`extract.rs:134`) yields the *single*
   best per class. 2C needs top-K or the Pareto frontier per class to fill domains. New `Extractor`
   method; consumer = the CSP domain builder.
2. **Multi-variable joint CSP.** Replace the one-variable per-rule solve (`decision_csp.rs:48`)
   with a whole-grammar variable vector. Consumer = `lower_to_rust`.
3. **A linear-sum / knapsack constraint.** The constraint zoo has `all_different` / `not_equal` /
   `implication` / `lambda` — **no linear `Σ w_i·x_i ≤ C` propagator.** The generated-size budget
   needs one (a bounds-consistency `LinearLeq` over the site-var scope; stopgap: a
   `LambdaConstraint` over all site vars, which forfeits propagation strength). This is the
   sharpest missing piece and belongs in `csp-solver` proper (the crate-internal-isomorphism law),
   not bbnf-lang.
4. **PMU-cost population.** `BackendCandidate.perf_cost` (`backend_egraph.rs:23`) exists but is fed
   synthetically (`0.0/1.0`, `decision_csp.rs:44-47`); 2C's objective is vacuous until 2D's fact
   tables populate it. **2C owns the solver/variables/constraints; 2D owns the coefficient source.**

---

## 4. Prior art (cited, structural)

- **LLVM `RegAllocPBQP` — Partitioned Boolean Quadratic Programming** (Scholz & Eckstein 2002;
  Hames & Scholz). PBQP = per-node discrete choice with pairwise coupling cost matrices, solved
  globally — the exact structural twin of 2C (per-site lowering choice + cross-site coupling:
  same-substrate union, kernel sharing, budget). A production existence proof that
  per-node-choice-with-coupling is *solved*, not folded, in a real compiler. Eckstein/Krall "Code
  Instruction Selection Based on SSA-Graphs" is the instruction-selection-as-PBQP companion.
- **Optimal e-graph extraction is NP-hard** with shared subterms / global constraints (egg /
  egglog literature). egg's greedy bottom-up extractor — which `extract.rs` mirrors — is optimal
  *only* for monotone, per-node-separable cost; `extract.rs:5-11` says so ("not optimal … cost
  models that depend on sharing require a more sophisticated extractor"). The `extraction-gym` ILP
  extractor and Coward–Constantinides–Drane's ILP/CP extraction are direct precedent: **once a
  global constraint (a budget, shared structure) couples the choice, extraction must be ILP/CP —
  i.e. exactly this family.** The budget converts extraction from a fold into a solve.
- **Multiple-Choice Knapsack** (Pisinger 1995; Kellerer–Pferschy–Pisinger). The
  budget-plus-per-site-choice model IS MCKP; its branch-and-bound / DP theory bounds compile cost
  and supplies the dominance pruning `CostFiniteDomain::min_cost` already implements.
- **CP-SAT / MiniZinc** — the idiomatic modelling target (one `int` var per site, `table`
  constraints for legality, a `Σ ≤ C` budget, `minimize`). A dispositive check: if the model
  solves in microseconds under CP-SAT, the bespoke `csp-solver` B&B suffices (constitution: one
  solver home).
- **regex-automata engine selection** (PikeVM / backtrack / lazy-DFA / one-pass by a
  feasibility+cost gate) is the small-scale analogue for the prefilter-tier variable; **tree-
  sitter** and **MLIR transform/PDL** cost-driven lowering are lowering-choice analogues, both
  greedy today — the negative precedent (no parser generator solves lowering globally), which is
  precisely the differentiator SPEC §1 claims.

---

## 5. Named risks and counterexamples (make-or-break)

1. **The budget never binds ⇒ the CSP decomposes to greedy ⇒ 2C is theater** (the exact failure the
   extant code embodies). If `Σ static_size_cost ≪ budget` on every real grammar, each site's
   optimum is independent and the per-site argmin (the e-graph extractor) yields the identical
   selection — the solver earns nothing. **This is the family's survival condition.** The
   counterexample the prototype must *produce*: a real grammar (or the joint 5-grammar fleet
   compiled together) where the standing LOC budget forces a non-greedy trade — site A takes its
   2nd-best cheap-size lowering so site B can afford its expensive-but-fast kernel. If no realistic
   budget produces divergence, 2C is honestly REFUTED and the correct disposition is: the CSP is a
   legality gate, the e-graph extractor is the selector (2A/2B win) — a named negative result, not
   a silent retreat.
2. **Determinism under branch-and-bound** (clean-regen law). Cost ties let B&B return different
   optima across runs → regen not bit-identical. Mitigation: deterministic tie-break — the
   `candidate_hash` already the final lattice axis in `DecisionCost` (`backend_egraph.rs:246`); a
   fixed variable order. Counterexample: two lowerings of equal (perf, size) → nondeterministic
   pick → regen diff → CI red.
3. **Compile-time blowup.** MCKP is NP-hard; CSS L4 (15 modules, many sites × candidates) could
   exceed `node_budget`. `node_budget = 10_000` is already set (`decision_csp.rs:14`); the RED gate
   must bound `DecisionCspFacts.csp_solve_us`. Counterexample: joint CSS-L4 solve exceeds budget →
   fallback → non-optimal AND non-deterministic.
4. **Vacuous objective.** Costs are literally `0.0/1.0` today (`decision_csp.rs:44-47`); without
   2D's PMU tables the objective ranks by static heuristics (`shape_rank`) and the "optimum" is
   noise. 2C is meaningful only atop non-vacuous coefficients — a hard dependency to declare.
5. **Only two constraints truly couple** (budget, same-substrate-union). The exactly-one-per-scan-
   site kernel constraint is per-site trivially satisfiable and adds nothing to solver-necessity.
   If risk 1 fires (budget slack) and same-substrate-union is rare, the coupling surface collapses
   and the joint solve degenerates to a partition of independent trivial picks.

---

## 6. What a prototype must test (the one experiment)

Build the joint MCKP model over the real fleet (json / css-l4 / csv, extending toward toml /
sheets) with per-site candidate domains extracted K-best from the e-graph, `static_size_cost` from
the *actual* generated-LOC deltas (PROVE-12 +18; GN-2 per-wave generated budgets), `perf_cost` from
the §0 PMU tables. Then:

- **T1 (the crux) — does the budget ever change the selection?** Sweep the LOC budget from slack to
  tight; find the budget at which the CSP's selection vector diverges from independent per-site
  argmin. Divergence at a *realistic* budget (the standing per-tranche LOC ceilings) ⇒ 2C is
  load-bearing and the family advances. Divergence only at an absurd budget ⇒ 2C is refuted → the
  CSP demotes to a legality gate (documented negative result).
- **T2 — determinism.** Solve twice; assert byte-identical selection vector; regen bit-identical
  (clean-regen law). Pin the `candidate_hash` tie-break.
- **T3 — compile cost.** Report `csp_solve_us` on the CSS-L4 joint model under `node_budget`; gate
  below a stated ms ceiling with no budget-exceeded status.
- **T4 — the objective is real.** With `perf_cost` from PMU, the CSP-chosen vector's predicted
  `Σ cyc/B` must beat the greedy vector's on ≥1 corpus, confirmed cold-paired per the §0 protocol —
  proving the solver's optimum is a measured product win, not a paper-optimal reshuffle.

T1 is dispositive: it either exhibits the coupling only a solver can resolve, or it proves the
decision separates and the family yields to 2A/2B. The prototype's first output is that
budget-sweep divergence curve.
