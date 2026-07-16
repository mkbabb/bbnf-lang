# LOCK-2 · Family 2A — EGRAPH-ISLANDS · pass-2 research

*A directed pass pipeline with bounded e-graph islands at named decision points; each
island has a same-wave consumer and a committed cost model; a CSP selects over the
extracted candidates. The IR is NOT itself an e-graph (that is a sibling center); the IR
is a tower, and graph optimization is a set of local, teardown-bounded organs planted at
enumerated seams within it.*

## 0. The thesis, made concrete

The mechanism is four small e-graphs, not one large one. Each is a *canonicalizing organ*:
`build → saturate(budgeted) → extract(total-order) → teardown`, planted at a named decision
point, reading a cost model that is a committed fact table, and feeding exactly one same-wave
consumer. Above the islands sits a *selection layer* — a CSP over the candidates the islands
extract — that resolves the cross-site couplings the local islands cannot see (a shared
generated-size budget, pairwise coherence between adjacent scan sites). The islands GENERATE;
the CSP SELECTS. This split is the family's spine and the reason it is not the whole-IR
e-graph: local canonicalization is cheap and confluent-enough to saturate in microseconds,
and the genuinely global, coupled decisions are few and belong to a solver, not to saturation.

The substrate for this already exists in miniature. `skinny/crates/passes/src/backend_egraph.rs`
is a working island: it builds an `EGraph<DecisionNode>` over 2–5 ranked backend-shape
candidates (`backend_egraph.rs:60`), unions them into one root class, saturates one narrowing
rewrite under `BackoffScheduler` (`:73-81`), extracts the cheapest via `Extractor`
(`:84-88`), and emits `ActiveCostFacts` for the same-wave consumer
`codegen::lower::rust::lower_to_rust` (`SAME_WAVE_CONSUMER`, `:11`). The island is real,
consumer-attached, and deterministic today. 2A's work is to (a) name the *other three*
islands, (b) give the selection layer a proper solver, and (c) make the cost model
profile-fed without breaking regen determinism.

## 1. The four named islands (the decision points)

| island | node kind | multiplicity | extraction | consumer (same-wave) |
|---|---|---|---|---|
| **I1 backend-shape** (extant) | childless leaf | 2–5 (post-prune) | cost-ranked canon suffices | `lower_to_rust` |
| **I2 regex-HIR canon** | child-bearing | genuine ≥2 | real bottom-up extraction | engine-tier NFA build (XL-C, parse-that) |
| **I3 scan-plan selection** | leaf-per-site + budget edge | ≥2 *sites*, few *forms* | extract per site, PBQP across sites | `lower/recognizer/` → `find_member`/`dfa.find` |
| **I4 projection canon** | child-bearing | small | bottom-up extraction | `schema_direct` / tape emitter |

The seam that hosts I3 already has its IR node stubbed: `ir/src/lib.rs:416` `enum Recognizer`,
`:425` `SimdMode` (today only `Exact`), `:430` `SimdSite` (today only `PreEntry`). The
recognizer vector is *produced* by `passes::derive_recognizers` and today *consumed only as a
header comment* (`runtime/src/grammars/json/generated.rs:10`). I3 turns that dead surface into a
consumed selection (this is the GN-2.W1 wiring, `docs/tranches/sk-v24/GN-2.md` §4). XL-F: the
GN-2.W1 scan-lowering predicate is **wrapped, not replaced** — the grammar-structural detection
(Kleene over a class whose complement cardinality ∈ [1,16]) becomes the *candidate generator*
feeding I3; the island adds the cost-ranked *tier* choice (Exact vs Prefilter vs SelfLoop) the
predicate had no vocabulary for.

## 2. Directive answers (evidence-first)

### Q1 — Saturated multiplicity → islands split by node kind
On the substrate, I1's `DecisionNode` is **childless** (`children: [Id; 0]`, `backend_egraph.rs:158`).
After priority hard-pruning (`hard_pruned` filter, `:50-53`) the ranked survivor count is the
candidate count — measured 2–5 (the five shapes of `all_backend_shapes()`, `ir/src/cost.rs:334`,
minus pruned). The single union collapses them into one e-class; e-class size after "saturation"
= ranked count, and the one rewrite (`NormalizeDirectSinkCost`) adds at most one node. So for
**childless islands multiplicity is small and extraction is a min-reduction, not a search** —
plain cost-ranked canonicalization is provably sufficient there, and the e-graph is arguably
overkill (it earns its keep only as the uniform substrate + provenance emitter). The genuine
choice among ≥2 survivors at **child-bearing** nodes lives in I2/I4, where a subterm can be
rewritten many ways (regex `a{2}` ↔ `aa`, projection `Seq[x]` ↔ `x`) and the cheapest depends
on shared-child costs — there the greedy bottom-up extractor (`extract.rs:134` `compute_best`)
is load-bearing. **Follow-what:** the family keeps ONE `egraph` crate but two island
*profiles* — a "leaf island" fast path (no scheduler loop, direct min) and a "tree island"
(saturate + extract). This is a measurable claim: instrument `class.iter().count()` at
extraction across the fleet; SPEC-ONLY until run.

### Q2 — Widening rewrites coexisting with narrowing
Every extant rewrite is narrowing (`NormalizeDirectSinkCost` only zeroes cost fields,
`backend_egraph.rs:216-220`). A *widening* rewrite manufactures a specialized alternative —
residualization introducing a monomorph (PROVE-12's const-generic `find_component_delim`,
`prototypes/PROVE-12/REPORT.md` §2), or I3 adding a `SelfLoop` variant beside `Exact`. The
e-graph is *precisely* the structure that makes widening + narrowing coexist without a
confluence proof: rewrites are **unions**, never replacements, so (a) **confluence is not
required** — saturation explores all orderings and extraction picks the survivor (the egg
insight; egg §"rebuild"); (b) **termination** is bounded operationally by the node/iter caps
already present (`CspScheduler.node_limit`/`iter_limit`, `csp_scheduler.rs:177-180`;
`egraph_node_limit`/`egraph_iter_limit` in `CostConfig`) — a widening rule that could loop is
stopped by the budget, and the growth guard (`csp_scheduler.rs:280`) makes the stop explicit;
(c) **semantics preservation** is a per-rule proof obligation: the widening rewrite must be a
*sound equivalence* (PROVE-12 discharges it — `(DELIMS>>byte)&1 == contains` for `byte<128`,
the guard makes high bytes equal too, REPORT §5). Precedent for widen+narrow coexisting in
shipping engines: egg/egglog ship expansion rules (`x → x·1`, distributivity) beside
contraction, relying on cost extraction to re-narrow; the risk they name is *node blow-up*, not
unsoundness — which is why 2A confines widening to islands with hard node budgets rather than
the whole IR.

### Q3 — Organ port budget (build+saturate+extract+**teardown**) and regen byte-identity
The teardown term is the real risk and the substrate already documents it: `egraph.rs:66-88`
warns that without `with_capacity` a ~300-node grammar leaves a 1024-bucket `FxHashMap` whose
**teardown dominates the compile profile**. The extant island obeys this
(`EGraph::with_capacity(ranked.len())`, `backend_egraph.rs:60`); every 2A island must. With
2–5 nodes and one iteration, I1's build+saturate+extract+teardown is comfortably inside the
~230 µs-order per-parse compile budget (the type-plane organ hits ~230 µs deriving a whole
`DirectSchemaSet`, SPEC §0 row 08); the leaf islands are an order below that. The tree islands
(I2/I4) are the ones to *measure* against the budget — SPEC-ONLY until a fleet timing runs.
**`regen∘regen` byte-identity through cost-guided extraction** turns entirely on a **total-order
tie-break**, because the greedy extractor updates on strict `<` (`extract.rs:155-158`) so ties
resolve to whatever the `classes()`/`class.iter()` order happens to be — inadmissible if that
order derives from HashMap iteration. The substrate solves this correctly: `DecisionCost`
derives `Ord` with a **content-hash final coordinate** (`candidate_hash: stable_hash(id)`,
`backend_egraph.rs:174,246`, FNV-1a), making the cost a *total* order with no ties (ids are
unique per rule×shape×priority, `:581-584`). This is the pattern to enforce fleet-wide: **the
last coordinate of every island's cost lattice is a stable content hash of the node's identity**,
so extraction is a total min independent of container iteration. Class *storage* is already a
`Vec` indexed by canonical id (`egraph.rs:29`), so the only HashMap-order exposure is the tie —
closed by the hash coordinate.

### Q4 — Context-sensitive extraction: audit of `admits_sink_only`
`admits_sink_only(rule_ir, target)` (`passes/src/lib.rs:954-957`) is a function of the rule's
own expr (`contains_direct_build`) **and** `target` (`TargetFeatures.direct_only_output |
direct_build_consumer`). `target` is **grammar-global**, and each island is built
**per-(grammar, rule.id)** (`derive_backend_shape_with_diagnostics` is called per rule,
`lib.rs:504`). Therefore, within one grammar, a shared sub-rule cannot be reached under both a
sink-only and an offset-tape caller *with conflicting shape requirements from the target* — the
target is fixed for the whole grammar, and the shape is decided per rule, not per call-site. The
only way to manufacture the conflict is cross-grammar (two grammars with different targets), and
those are **distinct islands** (fresh `EGraph` per grammar), so no single per-class shape must
serve two contexts. **What follows:** context-free per-class extraction is sound on the current
fleet; the island boundary (per grammar-rule) *is* the context boundary. If a future grammar
inlined a shared sub-rule into two callers demanding different shapes *within one grammar*, the
identity-preserving repair is **sub-rule duplication/specialization** — emit the sub-rule twice
(one per caller-context), which is trivially identity-preserving because the two copies are
distinct `RuleId`s and each gets its own island. This is cheaper and more honest than a
context-tagged e-class (the "e-class per (node, context)" blow-up that MLIR/rustc avoid by
cloning at inline sites). SPEC-ONLY confirmation owed: scan the fleet for a sub-rule with ≥2
callers of differing selected shape *inside one grammar* — expected empty.

### Q5 — IR-modeled competing scan sites
Today the IR models **one** scan mode (`SimdMode::Exact`) at **one** site (`SimdSite::PreEntry`,
`ir/src/lib.rs:425-430`) — so there are *zero* IR-modeled competing sites with binding costs at
present; the competition lives in grammar source, not the IR. That is exactly the dead surface
2A activates. The IR extension that models it: `SimdMode{Exact, Prefilter}` +
`SimdSite{PreEntry, SelfLoop{terminators: StructuralAlphabet}}` (GN-2.md §4 file-plan,
`ir/src/lib.rs:425`), each carrying a **static-size cost** (the generated `PrefilterSet::new`
table + call site) and a **cyc/B cost** drawn from certified measurement (PROVE-01 skip floor
0.115 cyc/B, dense-neutral 1.03–1.05×, SPEC §0 row 01). A single JSON grammar already presents
**≥2 real sites**: the whitespace-skip self-loop (complement = 6 structurals + quote,
cardinality ≤16 → qualifies), the string-content self-loop, and the structural pre-entry scan —
so the competing-site premise is satisfiable on the fleet the moment the IR carries it.
**Ownership:** the *node shape* (`Recognizer`/`SimdMode`/`SimdSite`) is LOCK-3's IR-transposition
territory (it is a tower node); the *island that selects a tier per site under a shared budget*
is LOCK-2/2A. The seam is clean: LOCK-3 builds the node in `derive_recognizers`
(`passes/src/lib.rs:338`), 2A reads the recognizer vector into I3 and returns the selection.

### Q6 — Solver shape: PBQP for the near-tree selection graph
The cross-site selection graph — independent per-site discrete choice (which tier), pairwise
coherence between adjacent sites (a shared alphabet or dispatch table amortizes across two sites),
plus a global size budget — is a near-tree with few coherence edges. This is the **canonical
PBQP shape**: per-node cost *vector* (the site's tier choices), per-edge cost *matrix* (the
coherence between two adjacent sites' choices). PBQP (Scholz & Eckstein 2002; Hames & Scholz,
*Nearly Optimal Register Allocation with PBQP*, 2006) is the SOTA formulation for exactly
"per-node discrete choice with pairwise transfer costs," and its use in **instruction selection
on DAGs** (pattern choice per node + edge cost for shared subtrees) is a near-isomorphism to
per-site kernel/tier selection — see also *Optimal DNN Primitive Selection with PBQP* (CGO 2018),
which is our problem almost verbatim (choose a primitive per layer; edge cost = data-format
conversion between adjacent layers). **Guarantees on this graph class:** PBQP is NP-hard in
general, but optimal solvers interleave **R0/R1/R2 reductions** (eliminate any variable coupled
to ≤2 others by non-zero cost) with B&B; a graph of bounded treewidth — trees and series-parallel
graphs — is solved to **provable optimality in linear time** by reductions alone, with B&B (RN)
firing only on irreducible cores. Our "few coherence edges over otherwise-independent sites" is
reducible almost entirely, so PBQP solves it optimally and fast; a production implementation
lineage exists (LLVM `RegAllocPBQP` since 2008). **Separation from the alternatives:** generic
B&B ignores the pairwise structure and re-derives it as search; whole-IR e-graph extraction with
sharing-aware cost is itself NP-hard and *lacks* the reduction guarantee (greedy bottom-up,
`extract.rs`, is non-optimal precisely when costs depend on sharing — its own doc-comment
`:8-11`). So the clean division is **e-graph islands for candidate generation (local, per-site),
PBQP for the coupled selection** — each tool on the graph class where it is optimal. Solve-cost
evidence is SPEC-ONLY until the fleet's site count and edge count are measured; the prototype
must report reduction depth vs B&B fallback count.

### Q7 — Budget knapsack as a constraint; ONE solver home
PBQP's cost matrices are *pairwise*; the generated-size budget (`Σ static_size_cost ≤ B`) is a
*global* constraint PBQP cannot natively express. **Substrate gap, named:** the `csp-solver`
crate has **no** sum/linear/knapsack constraint today — its constraint set is
`{all_different, all_different_except, dispatch, implication, lambda, not_equal, scratch}`
(`crates/csp-solver/src/constraint/`), and optimization is B&B via `solver/search.rs`
+ `solver/optimize.rs`. So 2A must **add a `bounded_sum` constraint** (a linear-≤ propagator with
a lower-bound `Cost` for B&B pruning, mirroring `optimize.rs`'s `lower`/`upper` bounds
vocabulary). Selection-equivalence with the P1–P8 priority cascade (`ir/src/cost.rs:264-286`):
the cascade is a **lexicographic preference** already encoded as the `shape_rank` coordinate
(`backend_egraph.rs:172`, `candidate_static_size_cost` 1–5, `lib.rs:603-611`); folding the budget
as `bounded_sum` **preserves the cascade's selection exactly in the budget-slack regime** (when
the preferred pick fits, the constraint is inactive and the lexicographic min is unchanged) and
**correctly overrides it in the budget-tight regime** (the cascade has no size awareness — that
is the improvement, not a violation). So it is a conservative extension: equal where the cascade
was right, better where it was blind. **ONE solver home:** the `csp-solver` crate is the single
constraint store shared with LOCK-1 (type widths/overloads) — `bounded_sum` lives there once and
is consumed by both the size budget (LOCK-2) and any capacity constraint (LOCK-1). PBQP enters as
a **specialized propagator/objective** *inside* that store for the coherence subgraph — not a
second solver. This is the coordination point GESTALT-B will ratify: one store, PBQP-reductions
as a plug-in propagator, `bounded_sum` as the global coupler.

### Q8 — Profile-fed cost without breaking regen
Today `perf_cost` is a **hardcoded 0** (`backend_egraph.rs:590`) — the profile slot is *empty*,
which is the honest gap: the current island ranks on `static_size_cost`/`shape_rank` only. The
home for a fitted coefficient already exists: `egraph::CostWeights`/`CALIBRATED_WEIGHTS`
(`crates/egraph/src/cost_weights.rs`) is a **committed fact-table** of shared knobs
(`structural`, `alt_per_branch`, `dispatch_bonus`) — the exact vehicle for a cyc/B-fitted
coefficient. (a) **Can a fit flip a real decision?** Yes in principle — a `perf_cost` fitted so
that Prefilter's 0.115 cyc/B floor beats a membership scan's 1.03 cyc/B flips I3's tier choice on
a skip-heavy site; this is falsifiable at the GN-2.W1 born-RED gate (≥8× or RED). (b) **Cold-paired
win at real value-start positions:** the PROVE benches are already cold, paired, interleaved
(SPEC §0 evidence standard; PROVE-12 REPORT §4 is the template — two binaries interleaved
back-to-back per window). (c) **Freshness gate RED on mutation:** `ActiveCostFacts` already carries
`selected_cost_freshness` + `candidate_cost_stale_rate_bps` (`ir/src/cost.rs:180-183`,
`backend_egraph.rs:127-131`) — a grammar mutation that invalidates the fitted coefficient sets
freshness=stale and the gate goes RED. **Attribution under underdetermination:** whole-grammar
cyc/B cannot separate per-feature coefficients, so use the **A/B-differential** the PROVE fleet
already runs — two binaries differing in exactly one feature, the cyc/B delta *is* that feature's
coefficient (PROVE-12 isolates the delimiter-mask residual this way; PROVE-01/03 isolate one kernel
each). **Knobs a fit would supersede:** the hardcoded `candidate_static_size_cost` ladder (1–5),
the `shape_rank` order, and the N*=3 kernel-crossover threshold (R1 §8) — all become fitted table
entries. **Env-source fenced from regen:** the coefficients commit as a `const`/`CALIBRATED_WEIGHTS`
table (like the extant one), **never read from env at regen** — determinism is preserved because
the table is a committed artifact and the fit is an *offline* step whose output is a code-review'd
constant. This is the boundary that keeps 2A ("islands + committed cost tables") distinct from a
live closed-loop that re-measures at every regen.

## 3. Substrate map (integration seams, path:line)

- **Islands host:** `crates/egraph/src/{egraph,extract,scheduler,csp_scheduler,cost_weights}.rs`;
  `Language`/`Rewrite`/`CostModel`/`Extractor` are the whole consumer API (`lib.rs:46-57`).
- **Extant island I1:** `skinny/crates/passes/src/backend_egraph.rs` (build `:60`, saturate
  `:73-81`, extract `:84-88`, total-order cost `:239-247`, consumer path `:11`).
- **Selection layer:** `skinny/crates/passes/src/decision_csp.rs` (`finalize_rule` `:? →`
  `DecisionCspFacts`, `ir/src/cost.rs:202-248`); **needs** a `bounded_sum` constraint added to
  `crates/csp-solver/src/constraint/` and a PBQP-reduction propagator.
- **I3 IR node:** `skinny/crates/ir/src/lib.rs:416` (`Recognizer`), `:425` (`SimdMode`), `:430`
  (`SimdSite`); produced at `passes/src/lib.rs:338` (`derive_recognizers`); dead-consumed at
  `runtime/src/grammars/json/generated.rs:10`; lowering home to build:
  `codegen/src/lower/recognizer/` (GN-2.W1).
- **Cost fact tables:** `crates/egraph/src/cost_weights.rs` (`CostWeights`/`CALIBRATED_WEIGHTS`);
  per-decision facts `ir/src/cost.rs` (`ActiveCostFacts` `:172`, `DecisionCspFacts` `:202`,
  `PriorityStep` `:263`).
- **Residualization precedent (I4/emit):** `prototypes/PROVE-12/patch.diff` + REPORT — the
  widening-then-const-fold organ, generated-LOC-budgeted (+18), regen bit-identical.
- **Determinism scheduler:** `crates/egraph/src/csp_scheduler.rs` (AC-3 dirty-closure over parent
  edges; the reproducible saturation driver, isomorphic to the grammar-tier csp usage).

## 4. Named risks & counterexamples

1. **Leaf islands don't earn the e-graph.** If I1's multiplicity is always ≤5 childless nodes, the
   `EGraph`+scheduler machinery is heavier than a sorted min. Counter-move: keep the e-graph as the
   *uniform provenance + cost-lattice substrate* (it emits `ActiveCostFacts` the gate reads), but
   run the leaf profile without the scheduler loop. Falsifiable: measure teardown µs of I1 vs a
   direct min — if the e-graph adds >5% of the 230 µs budget on a leaf island, demote it.
2. **PBQP graph is irreducible.** If coherence edges form a dense core (many sites sharing one
   dispatch table), reductions stall and B&B (RN) dominates → super-linear. Counter: cap the core
   size; the fleet's site count is small (JSON ≤3 real sites), so treewidth is bounded in practice —
   but this is SPEC-ONLY until measured.
3. **Budget/cascade divergence is silent.** If `bounded_sum` overrides the cascade on a real site
   and no one notices, selection-equivalence is *claimed* but the emitted parser differs. Counter:
   the gate must diff the budget-active selection against the cascade selection and *name* the
   override in `DecisionCspFacts` (a `budget_overrode_cascade` flag), never fold it silently.
4. **Widening blows the node budget on a tree island.** A residualizing rewrite on I4 that manufactures
   a monomorph per call-site could exceed `egraph_node_limit`. Counter: the growth guard
   (`csp_scheduler.rs:280`) already fails the wave RED; the widening rule must be *gated* by a
   generated-LOC budget (PROVE-12's +18 discipline), not unbounded.
5. **Profile fit is non-reproducible.** If the fitted coefficient is captured on a loaded host it
   shifts run-to-run. Counter: fit on **ins/B** (deterministic, load-proof, SPEC §0) not raw cyc/B,
   and commit the table; cyc/B is the *validation* not the *source*.

## 5. What a prototype must test to move 2A

1. **Multiplicity census (Q1):** instrument `class.iter().count()` at every extraction across the
   full fleet (json/css-l4/sheets/csv); report the distribution childless vs child-bearing. Decides
   whether the leaf/tree island split is real.
2. **I3 end-to-end with PBQP selection (Q5+Q6):** wire `SimdMode::Prefilter`/`SelfLoop` into the IR,
   generate per-site tier candidates from the islands, solve tier-per-site with a PBQP-reduction pass
   over the coherence graph, and lower to `find_member`; gate at GN-2.W1's ≥8×-skip / ≥0.95×-dense.
   Report reduction depth vs B&B-fallback count (the optimality/solve-cost evidence Q6 demands).
3. **`bounded_sum` + selection-equivalence (Q7):** add the constraint to `csp-solver`, run it on a
   grammar with a *binding* size budget, and prove: identical selection to the cascade when slack,
   named override when tight. Artifact: an A/B selection diff.
4. **Regen byte-identity through cost-guided extraction (Q3):** confirm the content-hash tie-break
   makes `regen∘regen` bit-identical on a fleet with deliberate cost ties; break the tie-break (drop
   the hash coordinate) and show it goes non-deterministic — the negative control.
5. **Profile-fed coefficient flips a decision (Q8):** fit `perf_cost` from an A/B-differential cyc/B
   capture, commit it as a `CostWeights` entry, and demonstrate a *real* I3 tier flip that wins
   cold-paired and drives the freshness gate RED under a grammar mutation.

## Sources

- Scholz & Eckstein, PBQP for register allocation (2002); Hames & Scholz,
  [*Nearly Optimal Register Allocation with PBQP*](https://www.researchgate.net/publication/221644839_Nearly_Optimal_Register_Allocation_with_PBQP) (2006);
  [*SSA-Based Register Allocation with PBQP*](https://link.springer.com/chapter/10.1007/978-3-642-19861-8_4).
- [*Optimal DNN Primitive Selection with PBQP*](https://dl.acm.org/doi/10.1145/3168805) (CGO 2018) —
  per-node primitive choice + adjacent-layer conversion edge cost, near-isomorphic to per-site tier selection.
- [PBQP overview & reductions (Zwinkau)](https://beza1e1.tuxen.de/articles/pbqp.html);
  [Survey on Combinatorial Register Allocation and Instruction Scheduling](https://arxiv.org/pdf/1409.7628);
  [Survey on Instruction Selection](https://arxiv.org/pdf/1306.4898).
- egg / egglog (equality saturation, rebuild, cost-guided extraction; non-confluent rules by construction).
