# LOCK-2 · Family 2A — EGRAPH-ISLANDS · pass-2 research

*A directed pass pipeline with e-graph islands at named decision points; every island has a
same-wave consumer and a cost model; CSP selects over extracted candidates. Developed to its
strongest form, grounded in the living substrate.*

---

## 0. Thesis in one paragraph

The pipeline stays a **directed sequence of ordinary IR passes**. At a small, fixed set of
**named decision points** an e-graph is stood up as a bounded *island*: a handful of candidate
terms are inserted, a **narrow rule set** saturates under a capped scheduler, a **local monotone
cost model** extracts one representative, and the extracted term is handed to a **same-wave
consumer** that emits from it. The e-graph never persists across a pass boundary — it is built,
saturated, extracted, and torn down inside one island. This is the egg-as-external-library shape
(equality saturation replacing *a small number of passes*), not the eqsat-dialect shape where the
e-graph is the IR (that is family 2B's claim). The family's whole bet is that the fleet's decision
points are **local-cost and near-tree**, so bottom-up extraction is exact and cheap and no
whole-program solver is needed — the counter-pressure the family must survive is that the single
largest measured optimization win to date (delimiter residualization) needed **no e-graph at all**.

---

## 1. The mechanism made concrete — the island contract

An island is a five-tuple `(insert, rules, cost, extract, consumer)` with a hard budget:

1. **insert** — lower the local decision into ≤ N candidate e-nodes (one class per decision).
2. **rules** — a *named* rewrite set, small (today: one), each rule idempotence-guarded.
3. **cost** — a `CostModel` returning a `Lattice`-valued cost, computed from children's best.
4. **extract** — greedy bottom-up fixed-point (`Extractor`), exact for local monotone cost.
5. **consumer** — a same-wave pass named at construction; the island fails closed if absent (Y.13).

The living instance of exactly this contract already exists and ships: **backend-shape selection**
in `skinny/crates/passes/src/backend_egraph.rs`. It is the proof-of-existence for 2A and the seam
every other island copies.

- `backend_egraph.rs:60` builds `EGraph::<DecisionNode, NoAnalysis>::with_capacity(ranked.len())`.
- `:62-70` inserts one childless `DecisionNode` per surviving candidate, **unioning all into one
  class** (the decision's e-class).
- `:73-81` saturates with `BackoffScheduler` under the `NormalizeDirectSinkCost` rule.
- `:84-87` extracts via `Extractor::new(&graph, &DecisionCostModel).best_node(root)`.
- `:11` names the consumer as a constant: `SAME_WAVE_CONSUMER = "codegen::lower::rust::lower_to_rust"`.
- `:55-58` fails closed: `assert!(!ranked.is_empty(), …)` — a consumer-less / candidate-less island
  is forbidden by construction, satisfying constitution clause viii.

The general substrate under it is a genuine egg-class engine (`crates/egraph/`, 3105 LOC incl.
tests): `EGraph` (`egraph.rs`, 297 LOC), `Extractor` (`extract.rs`), `Lattice`/`CostModel`
(`extract.rs:36-94`), `BackoffScheduler` (`scheduler.rs`), `CspScheduler` (`csp_scheduler.rs`),
`CostConfig`/`CostWeights` (`cost_config.rs`, `cost_weights.rs`), and a proc-macro
`egraph-derive` (343 LOC) deriving `Language` from enums (the recursive-field→`Id` projection,
per the derive-language memory law).

---

## 2. The named decision points (the islands), each with consumer + cost

The tower's shape is not ours to redraw (XL-B); islands sit *at* its existing decision points:

| Island | Decision point (seam) | Cost model | Same-wave consumer | Status on substrate |
|---|---|---|---|---|
| **Backend-shape** | `passes::recognizers::derive_backend_shape_with_diagnostics` → `backend_egraph::select` | `DecisionCostModel` (`backend_egraph.rs:259`) | `codegen::lower::rust::lower_to_rust` | **LIVE** — the only real island |
| **Regex HIR canon** | engine tier, parse-that home (XL-C ratified) | `RegexExtractionCost` (ref'd `cost_weights.rs:23`) | DFA/prefilter derivation (LOCK-5) | cost model exists; island unbuilt |
| **Scan-plan selection** | `passes::recognizers::derive_recognizers` (`passes/src/lib.rs:338`); `SimdMode`/`SimdSite` (`ir/src/lib.rs:425`) | `CostWeights.dispatch_bonus` + static-size | recognizer lowering `codegen/src/lower/recognizer/` | **rule-based today, not an e-graph** |
| **Projection canon** | delimiter/STOP-set residualization (`codegen/src/runtime_generator.rs` `CSS_GENERATED_RS`) | — | `find_component_delim` const-generic call sites | **const-fold, deliberately NOT an e-graph** (§5) |

The 2A pipeline is: `parse → infer → shape[island] → recognizers[island?] → regex-canon[island,
engine tier] → lower → residualize → emit`. Islands are inserted **only where multiplicity > 1
with binding cost separation** — the family's discipline is that an island with multiplicity ≈ 1
degrades to a pure cost-rank (a `sort`, not a saturation), and an island whose consumer is better
served by straight-line const-fold is refused (projection canon).

---

## 3. Answers to the tagged research questions (evidence, not position)

### Q1 — Saturated multiplicity; extraction-as-decision vs cost-rank

On the live backend-shape island, e-nodes are **childless** (`DecisionNode.children: [Id; 0]`,
`backend_egraph.rs:158,182-184`). Class size after saturation = number of surviving candidates
(≤ 5 `BackendShape`s: `EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage`,
`ir/src/lib.rs:361`) **plus** at most one manufactured `#direct-sink-normalized` node per class
(§Q2). So max e-class size is small and bounded; genuine multiplicity (≥ 2 survivors with binding
cost separation) arises **only at childless nodes** — there are no child-bearing decisions in the
backend island. **Consequence:** extraction here is not a tree-search decision; it is a
**total-order min over a flat set**, `O(N log N)`. The `Extractor`'s bottom-up fixed-point
(`extract.rs:134-171`) collapses to a single scan because every node's cost is `node.score`
independent of children (`DecisionCostModel::cost` ignores `child_cost`, `backend_egraph.rs:264`).
This is egg's *local-cost* regime, where the greedy bottom-up traversal is provably optimal (egg
POPL'21 §"Extraction"); the NP-hard exact-extraction case (non-local / sharing-aware cost) does not
arise. **For families that keep cost local, "extraction-as-decision" is a category error — plain
cost-ranked canonicalization is the honest description, and an e-graph is justified only where a
*rewrite* manufactures a candidate the ranker could not have seen (§Q2).**

### Q2 — Widening rewrites (manufacture, not narrow): confluence, termination, semantics

The substrate already ships a **widening/manufacturing** rewrite and proves it can coexist with a
min-extraction discipline: `NormalizeDirectSinkCost` (`backend_egraph.rs:191-237`). Its `apply`
(`:228-236`) *adds a brand-new node* (`egraph.add(matched)`) with a rewritten id
(`…#direct-sink-normalized`) and **zeroed** `static_size_cost`/`shape_rank`
(`:216-220`) — it manufactures a strictly-cheaper candidate that no ranker input contained.

- **(a) Confluence with an all-narrowing set.** The manufactured node **dominates** on the
  `DecisionCost` lattice (a total order, `Ord` derived over the field tuple ending in
  `candidate_hash`, `backend_egraph.rs:239-247`), and extraction is `min`. A widening rule that only
  ever adds lattice-dominating nodes commutes with any narrowing rule under min-extraction: the
  final extracted representative is `min` over the union of both rules' outputs regardless of apply
  order. This is the concrete confluence witness the directive asks for.
- **(b) Termination.** `search` skips any class already containing a normalized node
  (`:204-206`) — an **idempotence guard** giving a one-shot fixed point (each class is widened at
  most once). Termination is by monotone guard, not by a node-count race.
- **(c) Semantics preservation.** The manufactured node carries the *same* `shape` and `rationale`
  (`:216`); only cost fields change. Semantics = the emitted `BackendShape`; cost is advisory. So
  the widening cannot change *what* is emitted, only *which cost-tier* the same shape occupies.

**Precedent for coexisting widen+narrow in shipping engines:** egg's `Rewrite` set routinely mixes
introduction rules (identity/associativity *introductions*, `x → x*1`) with elimination rules;
termination is delegated to the scheduler's node/iteration caps (`BackoffScheduler`,
`CspScheduler` `iter_limit=64`/`node_limit=100_000`, `cost_config.rs:66-67`), not to rule
monotonicity. Cranelift's ISLE + aegraph mid-end likewise admits expansive rules bounded by an
iteration budget. The refinement 2A adds over both: a **lattice-domination invariant on
manufacturing rules** makes confluence with narrowing *structural* rather than budget-dependent.

### Q3 — Organ port budget, regen byte-identity, deterministic tie-break

- **Budget.** Build+saturate+extract+teardown for the backend island is `O(candidates)` on childless
  flat nodes — negligible against the ~230 µs compile-time envelope the type plane already spends
  (SPEC §0 row 08, DirectSchemaSet in ~230 µs). The island's own budget gate is explicit:
  `budget_status` at `backend_egraph.rs:100-108` (`final_nodes ≤ 100_000 && total_nodes ≤ 100_000
  && iterations ≤ 64 && memory_estimate < 1 GiB`), with `memory_estimate` at `:96-99`. Teardown is
  free — the `EGraph` is a stack local dropped at function return; nothing persists.
- **`regen∘regen` byte-identity.** Extraction yields a `BackendShape` enum handed to the emitter;
  the emitter's determinism is proven downstream by PROVE-12's bit-for-bit `cargo xtask regen-css`
  (SPEC §0 row 12, CONFIRMED). Cost-guided extraction preserves byte-identity **iff extraction is a
  deterministic function of candidate inputs** — which it is (next bullet).
- **The total-order tie-break (HashMap iteration inadmissible — solved).** `DecisionCost`
  (`backend_egraph.rs:239-247`) is a **total order** whose final field is `candidate_hash: u64`
  (FNV-1a of the candidate id, `stable_hash` `:304-311`). `Extractor::compute_best` updates a class's
  best **only on strict improvement** `cost < *existing` (`extract.rs:155-158`). A strict-improvement
  update under a *total* order is **independent of the FxHashMap iteration order** — whatever order
  classes/nodes are visited, the argmin is unique and identical. So the HashMap-iteration hazard the
  directive flags is **already neutralized on the substrate**: determinism comes from the total-order
  cost + strict-`<` update, not from stable iteration. The determinism claim is stamped in facts:
  `determinism_replay_status:"pass"`, `rewrite_order_replay_count:2`,
  `rewrite_order_variance_pct:0`, `selection_trace_hash` (`backend_egraph.rs:139-142`). **The prototype
  obligation is to make this a law:** a `debug_assert!` that no two candidates share a `candidate_hash`
  (a collision reintroduces a tie the order could break), and to lift the total-order+strict-`<`
  discipline into a reusable island harness so every future island inherits it rather than re-deriving.

### Q4 — Context-sensitive extraction; audit `admits_sink_only`

Selection is **per-rule**: `select(rule_id, candidates)` (`backend_egraph.rs:36`) runs one island
per `RuleId`. Because the island's nodes are childless, there is **no shared sub-class** across
callers to reconcile *inside* extraction — caller context is pre-baked into candidate generation
*upstream* (each rule presents its own candidate set). A genuine conflict would be: one sub-rule
reached under both a **sink-only** caller (`ShapeRationale::DirectBuildNoConsumer` → `SinkOnly`,
the widening target of `NormalizeDirectSinkCost` `:210-211`) and an **offset-tape** caller, where
one per-rule shape cannot serve both. The audit seam is `admits_sink_only` (the SinkOnly
admissibility predicate the directive names). **If none exists:** context-free per-class extraction
is sound and 2A needs no repair — the childless/per-rule structure *is* the context separation.
**If one exists:** the identity-preserving repair is candidate *duplication* (emit the shared
sub-rule under two `RuleId`s, one per caller shape) — cost `O(callers × candidates)`, no change to
the extraction algorithm. The prototype must grep the fleet (json, css_l4, sheets, csv) for a
sub-rule with ≥ 2 distinct-shape callers and report the count; absent a hit, this question closes in
2A's favor by measurement, not assertion.

### Q5 — IR-modeled competing scan/self-loop sites

**Today: none binding.** `SimdMode` has a single variant `{Exact}` and `SimdSite` a single
`{PreEntry}` (`ir/src/lib.rs:425-430`), so the scan-plan island has multiplicity 1 → a constant
decision, no e-graph warranted. The IR extension that *creates* competition is
`SimdMode::{Exact, Prefilter}` + `SimdSite::{PreEntry, SelfLoop{terminators}}` (the GN-2.W1 spec,
`docs/tranches/sk-v24/GN-2.md` §4.W1). The lock that owns building the enum surface is **LOCK-3 (IR
transposition spine)**; the cost/selection over it is **this lock (LOCK-2)**. The recognizer
*derivation* is `passes::recognizers::derive_recognizers` (`passes/src/lib.rs:338`),
grammar-structural (complement cardinality ∈ [1,16] → self-loop site), zero grammar-name gates.
Until that enum widens, the scan-plan island stays a rule, not a saturation — **2A's discipline
forbids standing up an e-graph where the IR models one choice.** When it widens, the two competing
sites (self-loop `find_member` XOR regex-leaf `dfa.find`, the PT-1.W5 interlock, GN-2.md §2.4)
become the first *child-bearing* island where extraction is a real decision and static-size + cyc/B
costs bind.

### Q7 — Budget knapsack as a constraint; one solver home

The per-rule CSP already carries a **capacity** constraint (`capacity_cost ≤ 1`,
`decision_csp.rs:79-81`), but it is per-rule and single-variable (`csp_variable_count:1`,
`:142`) — there is **no cross-site `bounded_sum` knapsack today**; each rule solves independently and
the generated-size budget (grammar `loc_budget 4000`; per-wave `generated_loc_budget`) is enforced
*post-hoc* by `xtask lint-loc`, not folded into selection. **Selection-equivalence with the priority
cascade:** the CSP presently **rubber-stamps** the e-graph pick — costs are `0.0` for the selected
index and `1.0` for all others (`decision_csp.rs:44-47`), and the priority table is
`"evidence-only"`/`"not-used"` (`:175-179`). So the cascade has *already* been replaced by the
e-graph+CSP pair with selection-equivalence held by construction (the CSP re-derives the same pick).
**What a real knapsack needs:** ONE constraint store spanning all sites in a grammar (a
`Csp<CostFiniteDomain>` with one variable per site, one `bounded_sum` over static-size costs), which
is a *cross-island* solver — that is the seam where 2A hands off to family 2C's central store. 2A's
position: keep per-island extraction local; introduce the shared store **only** at the one budget
edge, coordinated as a single `csp_solver::Csp` home (the vendored `crates/csp-solver`), never
duplicated per lock.

### Q8 — Profile-fed cost

The cost weights are **calibrated constants** today: `CALIBRATED_WEIGHTS` from a grid sweep
(`cost_config.rs:59-65`), with `from_env` applying `BBNF_COST_*` overrides (`cost_config.rs:83-118`).
- **(a) Flip a real decision.** The lever is `dispatch_bonus` (`cost_weights.rs:52-56`, negative =
  reward dispatch-eligible forms): a fitted coefficient that makes the bonus outweigh a shape's
  `static_size_cost` flips `OffsetTape → SinkOnly`. This is a real, reachable flip on the live island.
- **(b) Cold-paired at real value-start positions.** The measurement protocol is PROVE-12's:
  interleaved A/B binaries, per corpus × lane, cyc/B + deterministic ins/B, ≥ 3 windows
  (SPEC §0 row 12; GN-2.md §1.2 precepts 43-49). Warm/cached is inadmissible (refutation law).
- **(c) Freshness gate RED on mutation.** The staleness machinery exists:
  `DecisionCost.freshness_rank` (`backend_egraph.rs:169`, `u32::from(candidate.stale)`) →
  `selected_cost_freshness ∈ {fresh,stale}` (`:127-131`). A committed fact table with a content hash
  drives the gate RED when the grammar mutates but the fitted table was not re-run.
- **Attribution under-determination.** Whole-grammar cyc/B cannot isolate a per-feature coefficient
  (many features, one aggregate). The answer is **A/B-differential**: toggle one feature, measure the
  cyc/B delta at fixed corpus — exactly the interleaved paired design PROVE-12 used, extended to
  per-feature toggles. **Env-source fencing (determinism vs regen).** `from_env` reads process env →
  non-deterministic → it must be **fenced off the regen path**: the emitter reads only
  `CALIBRATED_WEIGHTS`/committed fact tables (`CostConfig::default`), never `from_env`, so
  `regen∘regen` stays byte-identical (the env knobs are a *bench-only* surface, documented as "for
  benchmarking", `cost_config.rs:74`). A fit would supersede the grid-sweep `CALIBRATED_WEIGHTS` const
  and the hand-set `dispatch_bonus`, not the enum structure.

### Q6 (boundary note) — solver shape

2A keeps extraction **local bottom-up** precisely so PBQP/B&B never enter an island: on childless
flat candidates cost-ranking is `O(N log N)` and optimal, and PBQP's pairwise cost matrices (Scholz
& Eckstein 2002; RegAllocPBQP) would model coupling that does not exist within an island. PBQP is
the right tool only for the *cross-site coherence* graph (shared-shape coupling + budget edges) — and
that graph is family 2C's central-CSP claim, not 2A's. 2A's contribution to that debate is the
**null hypothesis**: on the measured fleet the coherence edges may be so few that per-island
extraction + one shared budget constraint already reaches the same optimum a PBQP solve would — a
claim the prototype can settle by counting binding coherence edges (§Q4 audit).

---

## 4. Prior art (citations)

- **egg (Willsey et al., POPL 2021)** — the island substrate is a faithful egg port: e-classes,
  congruence rebuild, `BackoffScheduler`, and greedy bottom-up extraction that is **optimal for
  local cost** and NP-hard only for non-local/sharing-aware cost. 2A stays inside the local regime.
  (dl.acm.org/doi/pdf/10.1145/3434304; sigplan blog 2021-04-06.)
- **ILP / exact extraction (ROVER RTL datapath; e-boost, ICCAD 2025)** — when cost goes non-local
  (area with sharing), extraction becomes MILP for up to 63% area savings. This is the *evidence for
  2A's restraint*: the fleet's decision points are local, so the ILP machinery is unwarranted — but
  it marks the exact boundary at which an island would have to escalate. (csl.cornell.edu eboost;
  arxiv 2508.13020.)
- **eqsat MLIR dialect (EGRAPHS/PLDI 2025) vs egg-as-external-library** — the literature's own split
  maps 1:1 to 2A vs 2B: the *dialect* keeps the e-graph as the IR through the whole flow (2B); the
  *external library* approach "uses equality saturation to replace a small number of passes" —
  **that is 2A, verbatim.** (arxiv 2505.09363.)
- **DialEgg (CGO 2025)** — global egglog beats hand-written greedy MLIR on matrix-chain by
  associating *globally*. This is the **counterexample to over-localizing**: where a decision is
  genuinely global (association across many nodes), islands lose to whole-graph saturation. 2A must
  prove the fleet has no such global decision, or concede that one point to 2B.
  (dl.acm.org/doi/10.1145/3696443.3708957.)
- **Cranelift aegraph / ISLE** — a shipping compiler that runs equality-saturation-style rewriting
  as a bounded mid-end phase with acyclic extraction: precedent for the "bounded island, not
  persistent IR" posture.

---

## 5. Named risks & counterexamples

1. **The biggest win needed no e-graph.** PROVE-12 (SPEC §0 row 12, GO/CONFIRMED, cyc/B 0.80-0.91×)
   residualized the CSS delimiter test by **const-fold + monomorphization at generation time**
   (`runtime_generator.rs` `CSS_GENERATED_RS`), *not* by a projection-canon e-graph. This is a live
   refutation of any 2A claim that "projection canon should be an island": the consumer was better
   served by straight-line const-eval. **Discipline:** 2A must *not* stand up an island at projection
   canon; it names const-fold as the correct mechanism there and reserves islands for points with
   real multiplicity (backend-shape, future scan-plan, regex canon).
2. **Multiplicity ≈ 1 makes the e-graph theatre.** On the live backend island, childless flat nodes
   mean extraction is a `sort`. If the fleet audit (§Q1/Q4) finds every island at multiplicity 1, the
   e-graph is a costly wrapper around `min` and 2A collapses to "cost-ranked canonicalization" —
   honest, but not an *e-graph* family. **Counter:** the widening rule (§Q2) is the one place a
   candidate is *manufactured*, which a plain ranker cannot do; the family lives or dies on whether
   ≥ 1 island needs manufacturing.
3. **The CSP is a rubber stamp.** `decision_csp.rs` re-affirms the e-graph pick (costs 0/1) — it does
   no independent selection. If it stays that way, "CSP selects over extracted candidates" is
   aspirational. **Mitigation:** the CSP earns its place only at the cross-site budget knapsack (§Q7);
   absent that, drop it and admit the e-graph *is* the selector.
4. **Scheduler divergence.** The live island uses `BackoffScheduler` (`backend_egraph.rs:73`) while
   the crate ships a `CspScheduler` (AC-3 dirty-closure, `csp_scheduler.rs`). Two schedulers is a
   latent god-seam. **Mitigation:** pick one per the island's rule-richness (childless one-rule
   islands don't need the dirty-closure; regex canon might) and record the choice as a cost-model fact.
5. **DialEgg global-association counterexample.** If any fleet decision is genuinely global
   (associativity/reassociation across a rule tree), islands underperform whole-graph saturation.
   **Mitigation:** enumerate fleet decisions; if one is global, cede it to 2B and keep 2A for the rest.

---

## 6. What a prototype must test to move this family

1. **Multiplicity census (the load-bearing measurement).** Across json / css_l4 / sheets / csv, at
   each named decision point, count: (a) survivors after saturation, (b) how often ≥ 2 with binding
   cost separation, (c) child-bearing vs childless. Deliverable: a table. If (b) ≈ 0 everywhere, 2A is
   "cost-ranked canonicalization" and must say so.
2. **Widening confluence proof.** Add a *second* manufacturing rule to the backend island and show
   the lattice-domination invariant makes the extracted representative apply-order-independent — a
   running `assert` over both apply orders (extending `rewrite_order_replay_count`).
3. **Determinism law.** `debug_assert!` no `candidate_hash` collision; then perturb FxHashMap seeds
   and show `selection_trace_hash` is invariant — turning §Q3's argument into a test.
4. **`admits_sink_only` conflict hunt (§Q4).** Grep the fleet for a sub-rule with ≥ 2 distinct-shape
   callers; report count. Zero → context-free extraction proven sound by measurement.
5. **Scan-plan island (when SimdMode widens).** Build the first *child-bearing* island over
   `SimdMode::{Exact,Prefilter}` × `SimdSite::{PreEntry,SelfLoop}` and show extraction picks by real
   static-size + cyc/B cost, cold-paired, against the GN-2.W1 self-loop scan site (≥ 8× skip gate).
6. **Budget-knapsack selection-equivalence (§Q7).** Stand up ONE cross-site `Csp<CostFiniteDomain>`
   with a `bounded_sum` over a grammar's static-size costs and show it reproduces the per-rule picks
   when the budget is slack, and re-selects under a tightened budget — proving the knapsack is a real
   constraint, not a rubber stamp.
7. **Profile flip (§Q8).** Fit `dispatch_bonus` from an A/B-differential PMU capture and demonstrate a
   real `OffsetTape → SinkOnly` flip on one fleet rule, with the freshness gate going RED when the
   grammar mutates and the table is stale — cold-paired, regen byte-identical with `from_env` fenced
   off the emit path.
