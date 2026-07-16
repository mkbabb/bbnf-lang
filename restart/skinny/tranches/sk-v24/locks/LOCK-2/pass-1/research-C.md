# LOCK-2 pass-1 research-C — family 2C: CSP-CENTRAL (the decision driver IS a CSP)

The decision driver is a single constraint-optimization problem over the whole grammar. Its
variables are per-site lowering choices; its constraints are legality plus the generated-size
budget; its objective is a cost lattice fed by PMU facts. E-graphs are demoted from decider to
candidate generator — they populate the variables' domains and canonicalize candidates, and
nothing more. This family's load-bearing claim: **only a global COP can treat the
generated-size budget (`generated_loc_budget`) as a HARD constraint at decision time**;
per-rule or greedy-extraction selection can only lint size after the fact and can never trade a
size-cheap shape at rule A to afford a fast-but-fat kernel at rule B.

## 1. The defect this family cures (the current inversion)

At HEAD the e-graph decides and the CSP rubber-stamps — exactly backwards from the family
thesis. `passes::backend_egraph::select` (`skinny/crates/passes/src/backend_egraph.rs:36`)
builds a one-e-class-per-candidate graph, runs one normalization rewrite
(`NormalizeDirectSinkCost`, `:191`), and picks the winner by `Extractor::best_node`
(`:84`) over a lexicographic `DecisionCost` tuple (`:239`: freshness→perf→capacity→size→rank→
hash). Then `passes::decision_csp::finalize_rule` (`skinny/crates/passes/src/decision_csp.rs:16`)
solves a **degenerate 1-variable, 5-constraint CSP whose only purpose is to confirm the pick
the e-graph already made**: the cost vector is `0.0` for the pre-selected index and `1.0` for
every other (`decision_csp.rs:44-47`), so `MinimizeCost` trivially reproduces the extractor's
answer. The CSP is a fact-emitter (`DecisionCspFacts`, `ir/src/cost.rs:203`), scoped
`"rule-local-backend-candidates"`, one variable, `csp_constraint_count: 5` hardcoded. It has no
cross-rule variables, no budget coupling, no PMU-fed cost — it cannot express the one problem
that would justify a CSP. Family 2C makes this CSP the driver and removes the extractor from the
decision path (`backend_egraph.rs:84` `best_node` folds into the CSP's per-value cost).

## 2. The mechanism made concrete

**One global COP per grammar, solved once at compile.** `passes::compile` builds a single
`Csp<CostFiniteDomain>` whose variable set is the union of every decision site across all rules,
not a fresh CSP per rule.

**Variables (per decision site).** Each site is one `CostFiniteDomain`
(`crates/csp-solver/src/domain/cost_finite.rs:91`) whose values are candidate indices and whose
per-value `f64` cost is the site's cost-lattice reading:
- *Backend shape*, one per rule: the 5 `BackendShape` values (`ir/src/lib.rs:361`,
  `all_backend_shapes()` `ir/src/cost.rs:334`). Domain = the ranked (non-hard-pruned) candidates
  `backend_egraph::select` currently produces.
- *Recognizer / kernel*, one per scan site: `Recognizer::SimdScan { mode, site }`
  (`ir/src/lib.rs:416`) — `SimdMode ∈ {Exact, Prefilter}` (`:425`), `SimdSite ∈ {PreEntry,
  SelfLoop}` (`:430`, the GN-2.W1 extension). A self-loop with complement cardinality in `[1,16]`
  admits a `Prefilter`/`find_member` candidate; the CSP chooses Exact vs Prefilter by cost.
- *Capacity plan*, one per container: `CapacityPolicy { tiny_string_cap,
  container_initial_capacity }` (`ir/src/cost.rs:327`) — the `capacity_cost` field
  (`backend_egraph.rs:23`) becomes a per-value cost, not a post-hoc `≤ 1` predicate
  (`decision_csp.rs:79`).
- *Residualization depth*, one per delimiter/STOP-set scan: PROVE-12's const-generic
  `delim_mask` fold is a depth-0/1 choice (interpretive `&[u8]` vs const-folded `u128` mask);
  deeper residualization trades generated LOC for straight-line codegen — precisely a
  size-vs-speed variable the budget constraint must arbitrate.

**Constraints.**
- *Legality* (`LambdaConstraint`, `crates/csp-solver/src/constraint/lambda.rs`, as already used
  at `decision_csp.rs:204/221`): a shape's precondition (first-set overlap forces EagerTape; a
  sink consumer forces SinkOnly; complement cardinality gates Prefilter). The P1–P8 priority
  cascade (`PriorityStep`, `ir/src/cost.rs:264`) is RETIRED into these constraints — matching the
  extant `priority_data_role: "evidence-only"` disposition (`decision_csp.rs:178`). Each Pn
  becomes one legality or soft-preference constraint, not a hardcoded fallthrough.
- *Cross-site coherence*: sibling rules sharing a substrate must agree
  (`SameSubstrateUnionFacts`, `ir/src/cost.rs:160`) — an `Implication`
  (`constraint/implication.rs`) or `AllDifferentExcept` (`constraint/all_different_except.rs`)
  over the shape variables. This is a genuine cross-rule constraint the per-rule CSP cannot state.
- **The generated-size budget — the one genuinely global constraint.** `Σ_site
  static_size_cost(chosen) ≤ generated_loc_budget` (`skinny/Cargo.toml:67` = 4000). Today this is
  a POST-HOC lint (`cargo xtask lint-loc`, `skinny/xtask/src/main.rs:49`); 2C makes it a
  solve-time bounded-sum (knapsack) constraint coupling every site. This is the family's teeth:
  the budget turns GREEN by construction, and the solver spends LOC where it buys the most cyc/B.

**Objective.** `OptimizationMode::MinimizeCost` via `solve_optimized`
(`crates/csp-solver/src/csp/solve.rs:282`) over the `branch_and_bound` kernel
(`crates/csp-solver/src/solver/search.rs`), with `Pruning::ForwardChecking`/`Ordering::FailFirst`
/`node_budget` (`decision_csp.rs:85-97`). The per-value cost is the cost lattice fed by PMU
facts: `Measurement.cycles_per_byte_x1000` with `EvidenceSource::BenchProbe`
(`ir/src/cost.rs:306/319`) — integer-quantized cyc/B, already the schema. Lexicographic
tie-breaking (freshness→perf→size, today's `DecisionCost`) is preserved as a scalarized/ordered
objective the B&B kernel already supports; `min_cost` (`cost_finite.rs:164`) is the B&B lower
bound.

**E-graphs demoted to candidate generators.** `backend_egraph::select` keeps its e-graph +
`NormalizeDirectSinkCost` rewrite (`backend_egraph.rs:60-81`) but returns a candidate SET into the
CSP variable's domain instead of an extracted winner; `Extractor::best_node`
(`crates/egraph/src/extract.rs`) leaves the decision path. Candidate *canonicalization* (the
direct-sink normalization) stays an e-graph job — the family boundary is crisp: e-graphs prove
candidates equal and prune dominated ones; the CSP chooses across sites under the global budget.

## 3. Substrate: what exists (reuse) vs. the one new piece

Reuse — the csp-solver is a full COP engine, not a toy:
- `Csp<CostFiniteDomain>`, `solve_optimized` (`csp/solve.rs:282`), `branch_and_bound`
  (`solver/search.rs`), AC-3 + forward-checking + fail-first + `node_budget` + `restarts`/`cancel`
  (`config.rs`; the csc411 W1 field changes are already absorbed at `decision_csp.rs:91-96`).
- `CostFiniteDomain` (`domain/cost_finite.rs`): per-value `f64` cost, O(log n) `cost`, cached
  `min_cost` lower bound for B&B — exactly the "cost lattice" substrate.
- Constraint library: `LambdaConstraint`, `AllDifferent`, `AllDifferentExcept`, `Implication`,
  `NotEqual` (`crates/csp-solver/src/constraint/`).
- Fact plumbing already schema'd: `DecisionCspFacts` (`ir/cost.rs:203`), `Measurement`/
  `EvidenceSource` (PMU cyc/B), `CapacityPolicy`, `SameSubstrateUnionFacts`.

**The single new constraint type: a bounded/linear SUM (knapsack) propagator.** csp-solver has
`all_different` but NO linear-sum constraint — home it at
`crates/csp-solver/src/constraint/bounded_sum.rs` with a lower-bound relaxation (Σ min-cost
residuals) so B&B prunes on the budget. This is the whole net-new substrate the family needs;
everything else is rewiring. Placing it in csp-solver (not bbnf-lang) honors the
`regex-generalized`/`general-infra-crates` edicts: a bounded-sum COP constraint is general.

## 4. Prior art (with citations)

- **PBQP (Partitioned Boolean Quadratic Problem)** is the exact archetype: "each node represents
  options, exactly one must be chosen; edges are interdependencies; nodes carry cost VECTORS,
  edges carry cost MATRICES." LLVM ships a PBQP register allocator (`RegAllocPBQP.cpp`) and PBQP
  has been used for INSTRUCTION SELECTION on SSA graphs — the same "one lowering choice per site +
  pairwise coherence" shape as 2C. Near-optimal in linear time on sparse graphs, **optimal on
  trees** — decisive, because backend-selection sites are near-independent (the budget is the only
  dense coupler), so the reduction stays cheap. Cites: beza1e1.tuxen.de/articles/pbqp.html;
  llvm.org/doxygen/RegAllocPBQP_8cpp_source.html; "Optimal DNN Primitive Selection with PBQP"
  (arXiv 1710.01079) — primitive/kernel selection by PBQP, directly analogous to per-site kernel
  choice.
- **ILP extraction for e-graphs** validates the demotion of `best_node` INTO the objective:
  optimal DAG-size extraction (common-subterm-aware) is NP-hard; egg's LP extractor is optimal on
  DAG size but exponential; recent exact/heuristic solvers (e-boost, arXiv 2508.13020; HKUST
  OOPSLA24 sparse-e-graph extraction, treewidth ≤10 exact) mark the frontier. 2C's global CSP with
  a shared-substrate cost IS a DAG-aware extraction expressed as a COP — so the NP-hardness caveat
  transfers and is why `node_budget` (`decision_csp.rs:14` = 10_000) and PBQP-tree-reduction
  matter. Cites: dl.acm.org/doi/10.1145/3434304 (egg); arXiv 2508.13020; cse.hkust OOPSLA24a.
- **CP-SAT / νZ optimization**: MiniZinc/OR-tools CP-SAT and Z3's optimizing solver are the COP
  engines whose bounded-sum + lexicographic-objective idiom 2C mirrors in-tree — standard CP, not
  novel, which is the point: the mechanism is well-trodden.

## 5. Named risks and counterexamples

- **NP-hardness of the global knapsack.** A large grammar (css_l4, many rules) with a tight
  budget is subset-sum-hard. Counterexample: 4000 sites + a binding budget → exponential B&B.
  Mitigation: the decision graph is near-tree (PBQP-reducible — sites independent save the budget
  and a few coherence edges); `node_budget` already caps and yields `budget-exceeded`
  (`decision_csp.rs:103`); when the budget does NOT bind, the problem decomposes to independent
  per-site minima (parity with today's per-rule pick, provable).
- **Determinism / bit-for-bit regen (constitution ix, iv).** B&B over `f64` costs with
  `restarts` can reorder equal-cost solutions → regen diff. Counterexample: two candidates with
  identical PMU cost → nondeterministic winner. Mitigation: total-order tie-break on
  `candidate_hash` (already in `DecisionCost`, `backend_egraph.rs:246`); costs are
  integer-quantized (`cycles_per_byte_x1000`); `restarts→false` is already pinned
  (`decision_csp.rs:95`). The prototype must PROVE regen idempotence, not assert it.
- **Consumer-less substrate (constitution viii / Y.13).** A global plan nothing reads is dead.
  Mitigation: the plan IS what `codegen::lower::rust::lower_to_rust` reads
  (`same_wave_consumer_path`, `decision_csp.rs:171`); the budget constraint's consumer is
  `lint-loc` going GREEN by construction. Same-wave consumer is mandatory in the first prototype.
- **Missing PMU cost table.** The objective needs per-site cyc/B facts; §0 certifies per-CORPUS
  numbers, not per-site cost coefficients — no committed fact table exists yet (that closed loop is
  family 2D's territory). Without it the objective degenerates to `static_size_cost` (today's
  integer proxy). Named gap: 2C must ship at least a stub cost table so the objective is genuinely
  PMU-fed, or it is a CSP-shaped priority cascade with extra steps.
- **Over-generalization vs. KISS (`kiss-perf-bias`).** If the budget never binds on any real
  grammar, the global COP buys nothing over per-rule minima and is unjustified sprawl. The
  prototype must EXHIBIT a grammar where the budget binds and the global plan beats the greedy
  per-rule plan, or the family is refuted for the current corpus.

## 6. What a prototype must test to move this family

1. **Add `bounded_sum` to csp-solver** and build ONE global `Csp<CostFiniteDomain>` over ≥2 real
   grammars (json + css_l4) with `generated_loc_budget` as a live sum constraint. Show it selects
   a DIFFERENT, legal, cheaper-or-budget-feasible plan than the current per-rule
   `backend_egraph::select` when the budget binds, and a BYTE-IDENTICAL plan when it does not
   (the parity floor). Falsifiable: exhibit the binding grammar or concede the budget is slack.
2. **Determinism**: regen bit-for-bit across two runs of the global solver; equal-cost tie-break
   stable; `restarts=false`. Gate = `cargo xtask check-json`/`check-css` idempotent.
3. **Solve cost**: the global COP over the css_l4 rule set stays within `node_budget` and
   `csp_solve_us` bounded (today <1ms/rule; the global solve must stay ~O(rules)·ms, reported).
4. **Closed-loop seam (to family 2D)**: the objective consumes at least one PMU fact (stub cost
   table); flipping a cyc/B coefficient must CHANGE the selected plan — proving the objective is
   measurement-fed, not a relabeled static heuristic.

Family verdict-shape: 2C stands iff prototype (1) exhibits a budget-binding grammar where the
global plan strictly dominates the greedy per-rule plan under legality + regen determinism; it is
refuted-for-corpus iff the budget is provably slack everywhere, in which case the per-rule minimum
(today's path minus the rubber-stamp) is the honest KISS answer.
