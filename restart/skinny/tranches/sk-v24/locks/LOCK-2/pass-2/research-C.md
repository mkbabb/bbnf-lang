# LOCK-2 · Family 2C — CSP-CENTRAL (pass 2, research-C)

The decision driver IS a constraint problem. Variables are per-site lowering choices; constraints
are legality plus the generated-size budget; the objective is a cost lattice fed by measured PMU
facts. E-graphs (family 2A/2B) only *generate* the candidate domains this solver ranks. This
report makes that concrete against the living substrate, names the solver of record, and states
exactly what a prototype must falsify.

---

## 1. The mechanism, made concrete

### 1.1 The seam as it stands — a degenerate single-variable CSP

The "decision driver as CSP" already exists, but as theatre. `passes::decision_csp::solve_rule`
(`skinny/crates/passes/src/decision_csp.rs:29-98`) builds, **per rule**, a `Csp` with exactly
**one variable** (`:48-49`, `csp_variable_count: 1` at `:142`) whose `CostFiniteDomain` prices the
already-selected candidate `0.0` and every other `1.0` (`:44-47`). Five `LambdaConstraint`s
(`:52-81`) are unary predicates over that same single variable — recognizer/substrate/simd/capacity
legality plus a `selected-active-cost-candidate` pin (`:199-212`) that *forces* the variable back to
the index the priority cascade already chose. It then `solve_optimized` (`:98`) and reports facts.

This is a solver that cannot change any outcome: one variable, a pin constraint to the incumbent,
a cost that rewards the incumbent. `priority_data_role: "evidence-only"`, `priority_objective_status:
"not-used"` (`:178-180`) confess it. The real selection is the **eight-step priority cascade**
`choose_backend_shape` (`skinny/crates/passes/src/lib.rs:479-572`, `PriorityStep::ALL` at
`ir/src/cost.rs:276`), a lexicographic total order (P1EagerForced … P8EagerFallback) with no budget
awareness and no cross-site coupling. Family 2C's mechanism is to **invert the roles**: the cascade
becomes candidate-generation/legality data; the CSP becomes the deciding objective solve.

### 1.2 The genuine formulation — per-site variables, real coupling, budget constraint

- **Variables.** One per decision site, not one per rule × one candidate. A site is a lowering
  choice with ≥2 admissible values: `BackendShape` (`ir/lib.rs:361`, 5 values), the prefilter/self-
  loop kernel tier (`SimdMode` `ir/lib.rs:425` — today `{Exact}`, GN-2.W1 adds `Prefilter`;
  `SimdSite` today `{PreEntry}` adds `SelfLoop`), the capacity plan (`CapacityPolicy`
  `ir/cost.rs:326` — `tiny_string_cap`, `container_initial_capacity`), and residualization depth
  (PROVE-12's `const DELIMS: u128` monomorph-vs-runtime `&[u8]` choice). Each is a
  `CostFiniteDomain` over its admissible indices.

- **Constraints (legality).** The cascade's precondition tests become constraints, not a cascade:
  `admits_collapsed_stage` gated on `collapsed_stage_author_declared` (`passes/lib.rs:495`), first-
  set disjointness (`ShapeRationale::FirstSetDisjoint`), consumer presence
  (`DirectBuildNoConsumer`). These are exactly the `LambdaConstraint` predicates already written
  (`decision_csp.rs:57-81`) — reused, but now over genuinely-free variables.

- **Constraint (budget) — the coupling that makes it a *problem*.** The per-grammar `loc_budget`
  (4000 for JSON, `Cargo.toml` metadata via R1 §1) and GN-2's per-wave `generated_loc_budget`
  (≤ +50/+40/+25 LOC, GN-2 §5) are today enforced **outside** the solver by the `lint-loc` xtask
  gate (R1 §5). Folded inside as a `bounded_sum` constraint — Σ over selected sites of
  `static_size_cost` ≤ budget (`BackendCandidate.static_size_cost` already exists,
  `decision_csp.rs:249`) — the budget becomes the single global edge that couples otherwise-
  independent sites. This is the whole reason a *solver* is needed rather than per-site argmax.

- **Objective (cost lattice fed by PMU).** Replace the binary `0.0/1.0` placeholder
  (`decision_csp.rs:45-47`) with a cost fitted from measured cyc/B. `Measurement`
  (`ir/cost.rs:305`) already carries `cycles_per_byte_x1000` + `EvidenceSource::BenchProbe`
  (`:318`); `ActiveCostFacts.cost_formula_version` (`:174`) is the determinism fence. The lattice
  is `f64` total cost summed over sites — exactly `BranchBound::assignment_cost`
  (`csp-solver/src/solver/search.rs:376`).

- **E-graphs generate, do not decide.** `backend_egraph` emits `Vec<BackendCandidate>`
  (`decision_csp.rs:1,19`); the e-graph's job ends at the candidate domain. The CSP ranks. This is
  the constitutional Y.13 shape: the e-graph island's same-wave consumer is the CSP.

---

## 2. The substrate

### 2.1 What already carries the load

The vendored `csp-solver` crate (`crates/csp-solver/`, outer-repo path-dep per R1 §1) is
production-grade and is the solver of record:

- **Search kernel** `branch_and_bound` (`solver/search.rs:456`) — one monomorphized tree search,
  `optimistic_bound` pruning (`:388`), cost-sorted value ordering (`order_values` `:431`), off a
  shared `Trail` for O(removed) undo. `Pruning::Ac3` (MAC) + `Ordering::FailFirst` is the ratified
  default (`config.rs:88-103`). Node budget `Some(1_000_000)`, `budget_exceeded` flag for
  best-so-far (`config.rs:78`, `search.rs:212-217`).
- **Cost domain** `CostFiniteDomain` (`domain/cost_finite.rs`) — finite `i32` values + parallel
  cost vector, O(log n) `cost()`, `Cell`-cached `min_cost()` for the B&B lower bound (`:158-175`),
  `-1` unmatched sentinel.
- **Assignment COP** `AssignmentBuilder` (`builder/assignment.rs`) — closed-form Kuhn-Munkres
  (`kuhn_munkres.rs`, O(n³), **proven-optimal, never budget-blows**, `:11-22`) for group/pin-free
  linear assignment; general `Csp<CostFiniteDomain>` B&B for grouped/pinned. Documents the honest
  cliff: B&B is proven-optimal only to **n ≈ 15-18**, budget-blows at n=20 ~1M nodes
  (`assignment.rs:24-29`). Decision-site counts per grammar are far under that.
- **Lattice/monotonic path** `BitsetLatticeDomain` + `propagate_monotonic` — the AC-3 fixed-point
  used by the e-graph `CspScheduler` (`egraph/src/csp_scheduler.rs`), shape-identical to Lock-1's
  type/FIRST-FOLLOW propagation. One store, two consumers.

### 2.2 What must be built

- **A `bounded_sum` / knapsack constraint.** `csp-solver` has `AllDifferent`, `NotEqual`,
  `Implication`, `AllDifferentExcept`, `Lambda` (`constraint/`) — **no linear/sum constraint
  exists**. The budget knapsack (Σ static_size ≤ B) is net-new: a `BoundedSum` constraint carrying
  per-value weight vectors and a bound, with a `revise` that prunes values whose min-completion
  exceeds the bound. This is the single new primitive Family 2C owns in the solver crate.
- **PMU-fed cost table.** A committed fact table mapping (site-kind, candidate) → cyc/B coefficient,
  versioned by `cost_formula_version` (`ir/cost.rs:174`), regenerated only from banked
  `Measurement` rows — never read from env at regen (`static_profile_provider_status:
  "static-template-blocker"`, `decision_csp.rs:183` is the existing fence).

---

## 3. Answers to the tagged directive questions (evidence, not position)

### Q6 — Solver shape: PBQP vs generic B&B vs e-graph extraction

The bbnf decision graph is **near-tree**: mostly independent sites (degree 0), one global budget
edge, a few coherence edges (e.g. two rules sharing `same_substrate_union`, `ir/cost.rs:160`).
**PBQP** (Partitioned Boolean Quadratic Problem; Scholz & Eckstein 2002, reduction solver by
Hames & Scholz 2006, production in LLVM `RegAllocPBQP` since 2008) is the principled model:
per-node discrete choice with a local cost vector, per-edge n×m cost matrix. Its solver reduces the
graph by **R0** (isolated node — argmin of its cost vector, trivially optimal), **R1** (degree-1),
**R2** (degree-2 — both optimal, linear-time), falling to the **RN heuristic** only for degree-≥3
nodes. **On trees and series-parallel graphs R0/R1/R2 alone solve PBQP optimally in linear time** —
precisely the bbnf topology. Guarantees: PBQP and QAP are NP-complete in general (heuristic RN for
the residual); Hames-Scholz's B&B closed 97.4% of SPEC2000 functions optimally, the reduction
solver near-linear in practice.

Relation to the incumbent: `csp-solver`'s `CostFiniteDomain` B&B (`search.rs:456`) is the *right
engine* but is fed a graph with **no edges** — every variable independent, so it degenerates to
per-site argmin and the "solve" is vacuous. PBQP's value here is not a new crate; it is the
**recognition that coherence + budget ARE the edges**, and that with edges present the search stays
cheap because the graph is near-tree. Recommendation: keep the B&B engine (it already does
bound-pruned tree search with `FailFirst`+MAC), add pairwise-cost and `bounded_sum` edges, and rely
on MAC to reproduce PBQP-reduction efficiency at these tiny scales — do not import a separate PBQP
solver. E-graph extraction (2A/2B) is **rejected as the decider**: top-down cost extraction handles
neither the global cardinality budget nor coherence edges without an ILP lift (egg-as-ILP), and it
re-imports the per-class multiplicity question — 2C's stance is extraction *produces candidates*,
the CSP *decides*.

Prior art: [SSA-Based Register Allocation with PBQP (Buchwald/Zwinkau)](https://beza1e1.tuxen.de/pdfs/buchwald11cc.pdf),
[Nearly Optimal Register Allocation with PBQP, Hames & Scholz 2006](https://link.springer.com/content/pdf/10.1007/11860990_21.pdf),
[LLVM RegAllocPBQP source](https://llvm.org/doxygen/RegAllocPBQP_8cpp_source.html),
[Scholz PBQP page](https://www.complang.tuwien.ac.at/scholz/pbqp.html),
[Optimal DNN Primitive Selection with PBQP](https://arxiv.org/pdf/1710.01079) — the closest analogue:
per-layer kernel selection with pairwise data-layout conversion costs, isomorphic to per-site kernel
selection with substrate-crossing costs.

### Q7 — Budget knapsack as constraint; selection-equivalence with the cascade

The `bounded_sum` budget folded at the named sites **cannot** preserve selection-equivalence with
the priority cascade *in general* — and that is the point. The cascade (P1…P8,
`ir/cost.rs:264-286`) is a lexicographic order with **zero** budget awareness; it will select a set
whose Σ static_size may exceed budget, which today only fails downstream at `lint-loc`. A
`bounded_sum` constraint makes selection budget-feasible **by construction**, diverging from the
cascade exactly when the budget binds and agreeing when it does not (a directly testable invariant:
disable the constraint ⇒ recover the cascade pick on every unbudgeted grammar). Selection-
equivalence therefore holds **conditionally** (budget slack) and is *intentionally violated* under
pressure — the cascade's silent over-budget picks are the defect the constraint cures.

ONE solver home coordinates the shared store: the vendored `csp-solver` crate, synced by
`sync-csp-solver-vendor.sh --verify` (`config.rs:90-92` names the two live
`finalize()+solve_optimized()` consumers). Lock-1 (width/overload post-solve) and Lock-2 (lowering
decision) share that crate; the `bounded_sum` primitive lands there once, consumed by both.
Coordination is the vendor-sync gate, not a second solver.

### Q8 — Profile-fed cost: can a fit flip a decision, and how is attribution fenced?

Today no coefficient can flip anything — the cost is the binary placeholder
(`decision_csp.rs:45-47`). A real cyc/B coefficient replaces it and *can* flip a per-site decision
wherever two candidates are legality-tied and the incumbent was chosen only by cascade position —
e.g. `OffsetTape` vs `SinkOnly` where P2 fires on a consumer that a measured sink-cost would
outrank. **Attribution under whole-grammar underdetermination**: whole-grammar cyc/B
(json_canon_bench, PROVE-12's paired method) cannot isolate a per-feature coefficient, so fit each
coefficient from a **paired build with the feature on vs off** (PROVE-01/03/05 interleaved-
both-orders, GN-2.W6), attributing the delta to that one lowering. **Env-source fencing**: the
fitted table is committed, versioned by `cost_formula_version` (`ir/cost.rs:174`) and
`selection_trace_hash` (`:196`); regen reads the committed table, never an environment variable —
`static_profile_provider_status: "static-template-blocker"` (`decision_csp.rs:183`) is the fence and
stays RED to env sources. `determinism_replay_status` + `rewrite_order_variance_pct` (`:193-194`)
are the regen-byte-identity guard: a fit that changed the pick must still replay bit-identically.
**Knobs a fit would supersede**: the hardcoded `shape_rank` and `capacity_cost ≤ 1` threshold
(`decision_csp.rs:79, 249`), the `CSS_SHUFTI_MIN_SPAN` guard (GN-2.W3), and the N\*=3 kernel
crossover (R1 §8) — all today literals, all candidates for a fitted coefficient.

### Q5 — IR-modeled competing sites (does the fleet have ≥2 binding sites?)

Yes, and the clearest instance is already measured: CSS `find_component_delim` has **5 call sites**
with distinct delimiter sets (`b";{}"`, `b"{};"`×2, `b";}"`, `b":{};"`, PROVE-12 §1), each with a
real static-size cost (the monomorph LOC, +18 total) and a real cyc/B (PROVE-12 §4: ratio
0.81-0.98× per corpus). PROVE-12 residualized them **independently** (const-generic per site) — no
budget contention because +18 LOC fit trivially. The IR extension question: these sites are **not
yet modeled as competing IR nodes** — `SimdMode`/`SimdSite` (`ir/lib.rs:425-430`) carry
`{Exact}`/`{PreEntry}` only; the self-loop/prefilter sites GN-2.W1 introduces
(`SelfLoop { terminators }`) are where per-site static-size + cyc/B first become **binding** under
`generated_loc_budget`. **Which lock owns building the IR node**: Lock-3 (IR transposition spine)
grows the `SimdSite::SelfLoop`/`Prefilter` node and its cost fields; Lock-2 attaches the cost lattice
and solves. This is the XL-B "within the tower at a named decision point" division — 2C does not
redraw the IR, it decides over the sites Lock-3 models. Absent a fleet grammar with genuinely
budget-contending sites, the prototype authors a minimal multi-site `.bbnf` (GN-2.W1's mitigation
posture, §7) so the budget edge binds honestly.

---

## 4. Integration seams (path:line)

- `skinny/crates/passes/src/decision_csp.rs:29-98` — the degenerate solve to **promote** to multi-
  variable; `:48-49` (single var), `:52-81` (unary legality constraints to reuse), `:45-47`
  (binary cost to replace with PMU fit), `:199-212` (the incumbent-pin to **delete**).
- `skinny/crates/passes/src/lib.rs:479-572` — `choose_backend_shape` cascade to **demote** to
  candidate-generation/legality; `:495` (`admits_collapsed_stage` gate), `:541-572` (the P1-P8
  emission).
- `skinny/crates/passes/src/lib.rs:407-471` — `derive_backend_shape_with_diagnostics`, the loop
  that hosts the solve (currently per-rule at `:419`; the budget constraint spans the loop, so the
  solve moves *outside* it into one global CSP over all sites).
- `skinny/crates/passes/src/backend_egraph.rs` — `BackendCandidate` domain generation (the
  e-graph-only-generates seam).
- `skinny/crates/ir/src/cost.rs:202-248` — `DecisionCspFacts` (the schema the real solve fills:
  `csp_variable_count` must exceed 1; `csp_constraint_count` gains the budget row); `:305-324`
  `Measurement`/`EvidenceSource` (the PMU carrier); `:174` `cost_formula_version` fence.
- `skinny/crates/ir/src/lib.rs:361` `BackendShape` (5-value domain), `:416-430`
  `Recognizer::SimdScan`/`SimdMode`/`SimdSite` (the site kinds to model, Lock-3-owned).
- `crates/csp-solver/src/constraint/` — home of the net-new `BoundedSum`; `builder/assignment.rs`
  + `kuhn_munkres.rs` — the closed-form path for any pure-assignment sub-instance; `config.rs:90`
  the vendor-sync gate coordinating the one store.
- `crates/egraph/src/csp_scheduler.rs:22-23` — proof the two tiers already share the `LatticeDomain`
  / `Constraint::revise` shape.

---

## 5. Named risks and counterexamples

- **The graph has no real edges ⇒ the CSP is still theatre.** If every fleet site is budget-slack
  and coherence-free, the solve degenerates to per-site argmin and PBQP/B&B buys nothing.
  Counterexample needed: a grammar where Σ static_size over the const-generic monomorphs breaches
  `loc_budget` so the budget edge binds two sites against each other. Mitigation: the prototype
  **must** construct or find this instance; if none exists in the fleet the honest verdict is
  "budget coupling is SPEC-only," recorded as such (constitution vii).
- **PMU underdetermination defeats the fit.** If A/B-differential cyc/B deltas are inside noise for
  the tie-broken sites, no coefficient flips any decision and the objective stays cascade-equivalent
  — a legitimate negative result, not a failure to hide.
- **Non-determinism from f64 cost ties.** `BranchBound::order_values` sorts by `f64` cost
  (`search.rs:436-449`); equal costs leave order to sort stability over `domain.iter()`. HashMap
  iteration is inadmissible (Q3). The total-order tie-break must be an explicit
  `(cost, candidate_id, shape_rank)` lexicographic key materialized before the solve — `shape_rank`
  (`decision_csp.rs:249`) is the existing deterministic discriminator to fold in.
- **Budget-blow best-so-far poisons regen determinism.** If a grammar reached B&B's n≈18 cliff
  (`assignment.rs:24`), `budget_exceeded` returns a best-so-far pick that could vary — fatal for
  `regen∘regen` byte-identity. Mitigation: assert site-count ≪ 18 (true for the fleet) and
  fail-closed if exceeded rather than shipping a best-so-far selection.

---

## 6. What a prototype must test to move this family

1. **Promote `decision_csp` to a genuine multi-site global solve** over one grammar (JSON or CSS),
   ≥3 real variables with ≥2 admissible values each; confirm `csp_variable_count > 1` and a pick
   identical to the cascade when budget is slack (selection-equivalence baseline).
2. **Add `BoundedSum` to csp-solver** and prove it **flips** a selection on a constructed
   budget-binding grammar — the pick differs from the cascade's over-budget choice, and Σ
   static_size ≤ budget by construction. The load-bearing existence proof for Q7.
3. **Fit one cyc/B coefficient A/B-differentially** (paired both-orders, PROVE-12 method) and show
   it flips one legality-tied per-site decision the cascade decided by position alone (Q8), with the
   fitted table committed + versioned and `regen∘regen` byte-identical afterward.
4. **Measure the solve inside the per-parse compile budget** (~230 µs order, Q3): `csp_solve_us`
   (`decision_csp.rs:101`) for the real multi-variable instance must stay well under budget — B&B at
   ≤10 sites should be sub-microsecond; record it against `csp_nodes_explored`.
5. **Determinism harness**: run the solve twice with the explicit `(cost, id, shape_rank)` tie-break
   and assert byte-identical `selection_trace_hash` (`ir/cost.rs:196`); then perturb one cost by ε
   across the tie boundary and assert the pick flips deterministically (the freshness gate goes RED
   on mutation, Q8c).
