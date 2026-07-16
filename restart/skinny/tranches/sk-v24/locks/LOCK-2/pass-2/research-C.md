# LOCK-2 · Family 2C — CSP-CENTRAL — pass-2 research

*The decision driver IS a constraint-satisfaction problem: per-site lowering choices are the
variables, legality + the generated-size budget are the constraints, a PMU-fed cost lattice is
the objective, and e-graphs are demoted to pure candidate generators. This report makes that
mechanism concrete against the living substrate, names its one missing constraint primitive,
fixes the solver class from the decision-graph shape, and states exactly what a prototype must
prove.*

---

## 1. The substrate as it stands — the decision driver is a rubber-stamp, not a decision

Three organs make the backend-shape decision today; only the first actually decides.

1. **The priority cascade (the real decider).** `passes/src/lib.rs:407`
   `derive_backend_shape_with_diagnostics` loops rules, calling `choose_backend_shape` per rule
   — an 8-step priority ladder `PriorityStep::P1EagerForced … P8EagerFallback`
   (`ir/src/cost.rs:263-286`). The first firing step names the shape. This is a *fixed priority
   cascade*, exactly the mechanism 2C exists to replace with search.

2. **The e-graph (a one-class wrapper + lexicographic extract).** `passes/src/backend_egraph.rs:36`
   `select` builds `EGraph<DecisionNode, NoAnalysis>`, `union`s every ranked candidate for the
   rule into ONE e-class, runs a single cosmetic rewrite `NormalizeDirectSinkCost`
   (`:191`, zeroes `static_size`/`shape_rank` for an already-chosen direct-sink), then
   `Extractor::best_node` picks the minimum of a **total-order cost tuple** `DecisionCost`
   (`:239-247`: `freshness_rank, perf_cost, capacity_cost, static_size_cost, shape_rank,
   candidate_hash`). `DecisionNode.children = [Id;0]` — childless leaves. So the e-graph is a
   degenerate min-over-a-set; its saturated multiplicity equals the ranked-candidate count
   (≤ 5 shapes, `ir::all_backend_shapes` `cost.rs:334`), and the sole rewrite adds at most one node.

3. **The "CSP" (a rubber-stamp).** `passes/src/decision_csp.rs:29` `solve_rule` builds
   `Csp<CostFiniteDomain>` with **one variable** whose domain is the candidate indices, costed
   `0.0` on the already-selected index and `1.0` on all others (`:44-47`), then adds a hard
   `LambdaConstraint` forcing `selected == selected_index` (`:199-212`) plus four unary legality
   lambdas (recognizer/substrate/simd/capacity, `:52-81`). `solve_optimized(MinimizeCost)` **must**
   return the pre-chosen index by construction. It confirms a contract; it decides nothing.
   `DecisionCspFacts` (`ir/src/cost.rs:203-248`) is ~45 string "status" fields — scaffolding around a
   solver that has not yet been given a decision to make (`fallback_invoked: false`,
   `priority_data_role: "evidence-only"`, `choose_backend_shape_status: "csp-finalized"`).

**The structural verdict for 2C.** The decision graph today is a *pile of independent per-rule
choices with the coupling amputated*. The one coupling that matters — the whole-grammar
generated-size budget (`Cargo.toml:54-70` `loc_budget = 4000`; per-wave `generated_loc_budget` in
the kernel-plane ledger) — is enforced *nowhere in the solver*; it is a post-hoc lint that
"blocks the wave." That is the master defect 2C corrects: **lift the budget into the solver as a
real cross-site constraint, invert e-graph and CSP roles, and let one global solve make the
coupled decision.**

## 2. The mechanism, made concrete

### 2.1 Variables — per-site lowering choices, domains from e-class candidates

One CSP variable per lowering decision site, its finite domain the candidates extracted from that
site's e-class:

| site | domain (candidate lowerings) | domain source at HEAD |
|------|------------------------------|-----------------------|
| backend shape (per rule) | `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` | `ir/src/lib.rs:361`; ranked set from `backend_egraph::select` |
| scan/prefilter tier (per self-loop site) | `{Exact, Prefilter(shufti/teddy), dfa.find}` | `ir/src/lib.rs:425` `SimdMode`/`SimdSite` — **Exact only at HEAD**; `Prefilter`+`SelfLoop{terminators}` are IR extensions the kernel-plane owes |
| capacity plan (per container rule) | `{GrowOnly, Cap(n)}` | `CapacityPolicy` `ir/src/cost.rs:326`; `capacity_cost` on the candidate |
| residualization depth (per delimiter/STOP scan) | `{runtime-slice, const-mask-immediate}` | the const-fold `delim_mask` seam (CSS `find_component_delim`) |

Domains are small (≤ 5); the e-graph's job shrinks to *emitting the legal candidate set per site*
— its rewrites manufacture alternatives (a `SinkOnly` normalization, a `Prefilter` alternative to
`Exact`), and extraction stops being "the decision" and becomes "enumerate the e-class."

### 2.2 Constraints — legality (unary/binary) + the budget knapsack (global, the new primitive)

- **Legality, unary.** The four existing lambdas (`decision_csp.rs:52-81`) become genuine domain
  filters (prune illegal candidates), not tautologies over a pinned variable.
- **Coherence, binary.** A sub-rule reached under two callers with incompatible substrates cannot
  hold one shape — model it as a binary equality/compatibility edge between the two site
  variables. `SameSubstrateUnionFacts` (`ir/src/cost.rs:160`) and the `admits_sink_only` audit
  are exactly this edge. These are the "few coherence edges" — the graph stays near-tree.
- **The generated-size budget — a `bounded_sum` knapsack, GLOBAL.** `Σ static_size_cost[v] ≤
  loc_budget`, one linear inequality over *every* site variable, coupling the whole grammar. **This
  constraint type does not exist in the solver.** The constraint library (`csp-solver/src/constraint/`)
  holds `all_different`, `all_different_except`, `implication`, `not_equal`, `lambda` — all
  combinatorial, none arithmetic-linear. `bounded_sum` must be BUILT (§4).

### 2.3 Objective — a PMU-fed cost lattice; e-graphs only generate

Minimize `Σ cost(v)`, `cost` a lattice fed by measured cyc/B. The substrate is present:
`Measurement.cycles_per_byte_x1000` with `EvidenceSource::BenchProbe` (`ir/src/cost.rs:306-324`),
the B&B optimizer `CostModel::cost -> f64` (`csp-solver/src/solver/optimize.rs:14`) driving the
`BranchBound` search policy (`solver/search.rs:367-501`, optimistic-bound cutoff + cost-sorted
value order), over `CostFiniteDomain` (`domain/cost_finite.rs:59`, sorted `Vec<(i32,f64)>` +
`Cell`-cached `min_cost`). At HEAD the cost is *static* — `perf_cost: u32` on the candidate,
`static_profile_provider_status: "static-template-blocker"` (`decision_csp.rs:183`). 2C's objective
replaces that static integer with a committed, fitted cyc/B coefficient (§5).

## 3. Solver shape (Q6) — a near-tree ⇒ PBQP is the benchmark, B&B is the pragmatic home

The decision graph is: N independent per-site nodes, small finite domains, a handful of binary
coherence edges, plus one global budget knapsack. Strip the knapsack and it is **series-parallel /
near-tree** — the exact graph class **PBQP** (Partitioned Boolean Quadratic Programming) was built
for: per-node cost *vector* + per-edge cost *matrix*, solved by R0/R1/R2 graph reductions that are
**provably optimal and linear-time on tree and series-parallel graphs**, RN (partial-min
heuristic) only for irreducible nodes. Precedent: Scholz & Eckstein, *Register allocation for
irregular architectures* (LCTES 2002 — PBQP origin); Hames & Scholz, *Nearly optimal register
allocation with PBQP* (CC 2006 — LLVM's `RegAllocPBQP`); Eckstein/König/Scholz PBQP instruction
selection (DAG covering). Our per-site-choice-with-pairwise-coherence is isomorphic to
register-choice-with-interference: PBQP is the natural, optimality-guaranteed model for the
uncoupled core.

The wrinkle: the budget is a **global N-ary** constraint, which pairwise PBQP cost matrices cannot
express directly. Two routes, and the evidence favors the second as the *home* with the first as
the *optimality yardstick*:

- **Route A — PBQP + Lagrangian dual of the budget.** Dualize `Σ size ≤ B` into the node cost
  vectors (add `λ·size` to each node cost), solve PBQP, adjust `λ` by subgradient until the budget
  binds. Optimal on the near-tree class; but needs a PBQP engine the workspace does not have, and
  the multiplier loop reintroduces iteration.
- **Route B — the existing B&B, budget as `bounded_sum` (favored).** The graph is tiny (json ≈ a
  dozen rules; css a few dozen sites), so `Csp<CostFiniteDomain>` B&B with forward-checking, the
  budget as a bounds-consistency-propagated `bounded_sum`, and Régin GAC on the coherence
  all-different (already in `solver/gac/`) solves to *proven* optimum in microseconds — well inside
  the ~230 µs per-parse compile budget (the current one-variable solve is sub-µs). It **reuses the
  sole solver** (`csp-solver/src/lib.rs`: "This is the sole solver in the workspace"), needs no new
  engine, and expresses the knapsack natively. Generic B&B's exponential worst case never bites at
  N ≈ 10–40 with domains ≤ 5 and one binding budget.

**Recommendation (evidence, not position):** ship Route B; keep PBQP (Route A) as the prototype's
optimality oracle — solve the same instance both ways, confirm identical assignment, and separate
solve-cost. If B&B ever exceeds budget on a large fleet grammar, PBQP+Lagrangian is the named
escape hatch, not a from-scratch invention.

## 4. The one missing primitive — `bounded_sum`, and one solver home (Q7)

`bounded_sum(vars, weights, bound)` — prune any candidate whose *minimum completion*
(its own weight + the sum of the cheapest remaining choice at every unassigned site) exceeds
`bound`. This is the standard knapsack bounds-consistency propagator (Trick, *A dynamic-programming
approach for consistency and propagation for knapsack constraints*, 2003; the Global Constraint
Catalogue `bin_packing`/`knapsack`). It lands as a new `constraint/bounded_sum.rs` implementing the
crate's `Constraint` trait (`constraint/traits.rs`), GAC-filtered like the existing Régin
all-different — a colocated peer, not a god-module bolt-on.

**Selection-equivalence with the cascade it replaces.** The priority cascade never optimized the
budget — the budget is a *post-hoc lint* at HEAD (overflow "blocks the wave"). Folding it in as a
constraint therefore cannot lose a decision the cascade made; it can only make feasible-vs-budget
decisions the cascade deferred to a human. Equivalence is proven by: on every current fleet
grammar the budget is slack (json 4000-line budget vs ~5153 generated across three grammars, none
individually near cap), so the `bounded_sum` is non-binding and the CSP reproduces the cascade's
per-site optimum exactly. The constraint only *changes* an outcome when the budget binds — which is
precisely where a human lint fires today.

**One solver home across locks.** `csp-solver` is already the sole solver, shared with LOCK-1's
type/constraint plane ("one constraint store"). LOCK-2's lowering variables and LOCK-1's type
variables live in the same `Csp<D>` container with different domain types (`FiniteDomain` /
`CostFiniteDomain` / `LatticeDomain`); the constraint store is unified, the objective is per-solve.
The coordination point is the domain-type seam, not two engines.

## 5. Profile-fed cost (Q8) — the coefficient that flips residualization

**Can a fitted coefficient flip a real decision?** Yes, at the residualization-depth site. The CSS
component-delimiter scan has a measured const-fold win over the runtime-slice form —
straight-line const-mask immediate vs `<[u8]>::contains` per byte, cyc/B ratio ~0.81–0.98 across
corpora, ins/B −3.7…−6.4% (deterministic). A cost coefficient fitted from that measured delta,
attached to the `{runtime-slice, const-mask}` domain, makes the CSP *choose* residualization on
cost rather than the generator doing it unconditionally — and would *not* choose it where the
scan is cold (the immediate bakes generated LOC the budget must then afford, the
`bounded_sum` coupling). That is a real flip: hot delimiter scan → const-mask; cold →
runtime-slice to save budget.

**Attribution when whole-grammar cyc/B underdetermines per-feature coefficients (A/B-differential).**
A single whole-grammar cyc/B number cannot separate per-site coefficients. The coefficient for a
feature is the **paired A/B delta** — build with the feature ON vs OFF at that one site,
cold-paired interleaved PMU, the coefficient = the measured cyc/B difference, not a global
regression. This is the only sound attribution and it is already the house measurement method.

**Determinism / freshness / env-fencing.** Fitted coefficients commit as a **fact table** (the
`eisel_lemire/table.rs` pattern — data, not environment reads). The freshness gate is
`ActiveCostFacts.selected_cost_freshness` / `candidate_cost_stale_rate_bps` (`ir/src/cost.rs:180`):
when a `.bbnf` mutates and the committed coefficient no longer matches the site, the gate goes RED
and the fit must re-run. **The fit runs offline; regen reads only the committed table, never the
PMU or environment** — otherwise `regen∘regen` byte-identity breaks. Env-source is fenced by
construction: the generator's input is the fact table file, a tracked artifact.

## 6. Determinism & tie-break (Q3)

Extraction's total order ends in `candidate_hash = stable_hash(id)` (`backend_egraph.rs:174,239`) —
a deterministic tie-break, admissible; `extract.rs:118` uses `FxHashMap` only as a memo keyed by
`Id` (sequential), the cost total-order settles ties, so per-rule `regen∘regen` is byte-identical
today. **The new risk is the GLOBAL solve:** two whole-grammar assignments can tie on total cost
while differing per-site (one site trades a shape for another under the budget). B&B `max_solutions:
1` returns the first optimum in search order — so the tie-break must be a *total order over
assignments*, not HashMap iteration. Mitigation: variable ordering by stable `RuleId`, value order
by the same `candidate_hash` total order the extractor uses, `CostFiniteDomain`'s sorted `Vec`
keeping value order deterministic. The prototype MUST assert `regen∘regen` byte-identity on the
*global* solve, not just per-rule.

## 7. Multiplicity, context, IR-modeled sites (Q1, Q4, Q5) — the 2C reading

- **Q1 multiplicity.** Per-rule e-class size = ranked-candidate count (≤ 5, usually 1–3, at
  childless leaves). Genuine ≥2-survivor choice occurs only in the offset-tape speculative band
  (P5/P6/P7). *Follow-on:* because nodes are childless and multiplicity is small, extraction-as-
  decision is trivial and the wrong locus — the real decision complexity is the **cross-site
  coupling** (budget + coherence) the per-rule e-graph structurally cannot see. This is the
  argument FOR 2C: the e-graph generates, the CSP couples.
- **Q4 context-sensitive extraction.** `admits_sink_only` and `SameSubstrateUnionFacts`
  (`cost.rs:160`) are the audit target: a sub-rule under both a sink-only and an offset-tape caller
  is a coherence edge (binary constraint) or a context-split (one variable per calling context).
  The CSP models it as a constraint rather than a cascade special-case; the identity-preserving
  repair is "duplicate the variable per context, add an equality edge only where substrates must
  agree" — linear in the number of shared sub-rules.
- **Q5 IR-modeled competing sites.** At HEAD there are NOT ≥2 scan sites modeled in the IR with
  binding static-size+cyc/B: `SimdMode` is `Exact`-only, `SimdSite` is `PreEntry`-only
  (`ir/src/lib.rs:425`). The prefilter-tier variable has a real domain only once the kernel-plane
  extends `SimdMode::Prefilter` + `SimdSite::SelfLoop{terminators}` with a per-site cost. **LOCK-2
  owns the SELECTION over these sites; the IR extension that BUILDS them is owed by the kernel-plane
  / IR-transposition lock.** 2C is consumer-blocked on that extension for the scan-tier variable.
- **Supersession XL-F — the scan-lowering predicate: 2C WRAPS it.** The kernel-plane predicate
  (self-loop complement cardinality ∈ [1,16] ⇒ emit `Prefilter`) is *unconditional* at HEAD — it
  fires and lowers. Under 2C it is **demoted from decider to candidate-generator**: it emits the
  `Prefilter` candidate into the site's domain, and the CSP decides `Prefilter` vs `Exact` under
  the budget + coherence + cost objective (it may *reject* the prefilter if the immediate blows the
  budget or a coherence edge forbids it). Relation: **wraps** (not replaces, not waits-on) — the
  predicate keeps generating; the CSP is the new decision layer above it. Evidence: the predicate
  has no notion of the global budget or of a competing site, so it cannot be the decider once a
  second competing site or a binding budget exists.

## 8. Named risks & counterexamples

- **The budget never binds → the CSP is indistinguishable from the cascade** (selection-equivalence
  holds but the mechanism looks inert). Counter-mitigation: the prototype must include a grammar
  where the budget binds (author a wide grammar, or shrink `loc_budget`) so a decision *flips* and
  the CSP earns its consumer. A CSP that never changes an outcome is a consumer-less substrate.
- **Global tie non-determinism** breaks `regen∘regen` (§6). Prototype gate: byte-identical global
  solve across two runs.
- **B&B blows the per-parse budget on a large fleet grammar.** Mitigation named: PBQP+Lagrangian
  (Route A). Prototype must record solve-µs vs grammar size to know if/when the yardstick matters.
- **Fitted coefficient contaminates regen with environment state** → non-reproducible generated
  files. Mitigation: offline fit, committed fact table, env-fenced regen (§5). Prototype gate:
  regen reads only the table.
- **E-graph extraction is NP-hard with sharing** (Willsey et al., *egg*, POPL 2021; optimal
  extraction is NP-hard, motivating ILP/CSP extractors). Not a live risk here — childless leaves,
  no sharing — but it is the theoretical reason candidate-generation-then-CSP is sound: the CSP is
  the right tool the moment sharing or coupling appears, which the global budget introduces.

## 9. What a prototype must test to move family 2C

1. **Build `bounded_sum`** (`constraint/bounded_sum.rs`, GAC knapsack propagator) — unit-test
   bounds consistency vs a brute-force oracle.
2. **Replace the rubber-stamp CSP** (`decision_csp.rs`) with a **global** `Csp<CostFiniteDomain>`:
   one variable per site across the whole grammar, real legality filters, coherence edges from
   `SameSubstrateUnionFacts`, the `bounded_sum` budget, PMU-fed objective. E-graph reduced to
   candidate emission.
3. **Flip a real decision under a binding budget** — a grammar where the CSP chooses a different
   per-site lowering than the cascade because the budget binds; measure the resulting parser cold-
   paired to prove the flip is a win (or a named loss), not cosmetic.
4. **`regen∘regen` byte-identity on the global solve** — the determinism gate (§6).
5. **Solve-cost separation** — B&B-µs vs PBQP-µs (or PBQP+Lagrangian) on the same instances, both
   returning the identical assignment (optimality confirmation), scaling with grammar size.
6. **Fitted-coefficient flip at the residualization site** — fit the const-fold cyc/B delta as a
   committed coefficient, show the CSP selects const-mask on a hot scan and runtime-slice on a cold
   one, with the freshness gate going RED on `.bbnf` mutation.

**Integration seams (path:line).** Decision driver: `passes/src/lib.rs:407`,
`passes/src/backend_egraph.rs:36`/`:191`/`:239`/`:261`, `passes/src/decision_csp.rs:29`/`:44`/`:199`.
IR facts: `ir/src/cost.rs:203` (`DecisionCspFacts`), `:263` (`PriorityStep`), `:306` (`Measurement`),
`:160` (`SameSubstrateUnionFacts`), `ir/src/lib.rs:361` (`BackendShape`), `:425` (`SimdMode`/`Site`).
Solver: `csp-solver/src/lib.rs`, `constraint/` (add `bounded_sum.rs`), `solver/optimize.rs:14`,
`solver/search.rs:367`, `domain/cost_finite.rs:59`, `solver/gac/` (Régin). Budget: `Cargo.toml:54`
(`loc_budget`). Residualization site: the CSS `find_component_delim` const-fold seam.

**Prior art.** PBQP: Scholz & Eckstein LCTES 2002; Hames & Scholz CC 2006 (LLVM `RegAllocPBQP`).
Knapsack propagation: Trick 2003; Global Constraint Catalogue (`knapsack`/`bin_packing`). GAC:
Régin 1994 (in-crate). E-graph extraction hardness: Willsey et al. *egg* POPL 2021; ILP extraction
(Peggy, Tate et al. 2009). Instruction selection as covering/PBQP: Eckstein/König/Scholz.
Lagrangian relaxation of resource constraints in selection: standard subgradient dual.
