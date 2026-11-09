# LOCK-2 · Family 2B — IR-IS-EGRAPH (pass-2 research)

*Thesis: the middle IR is an e-graph end to end; compiler passes are rewrite groups
and e-class analyses; equality saturation runs under a schedule; extraction emits
`BackendShape` (and the scan plan) directly from the saturated graph.*

The task is to develop 2B to its strongest form, then locate honestly where it is
distinct from — and where it degenerates into — an island architecture. The evidence
below argues 2B is buildable on the extant substrate with a single structural change
(the middle IR enum becomes `Id`-childed), and that its ONE load-bearing empirical
question is saturated multiplicity: whether extraction over the unified graph is a real
decision or a formality.

---

## 1. The mechanism, made concrete

### 1.1 What exists today is an ISLAND, not the IR

The current e-graph is not the middle IR. `skinny/crates/passes/src/backend_egraph.rs:60`
builds a throwaway `EGraph<DecisionNode, NoAnalysis>` whose nodes are **childless**
(`DecisionNode { children: [Id; 0] }`, `:158`): one leaf per ranked backend candidate,
all `union`ed into one root class (`:64-70`), one rewrite (`NormalizeDirectSinkCost`,
`:191`) that adds at most one alternative per class, then `Extractor` cost-ranks and
`best_node(root)` picks the winner (`:84-87`). The graph carries **no grammar structure**
— it is a cost-ranked bag of ≤5 candidates. That is precisely a 2A island at the
`derive_backend_shape` decision point (`passes/src/lib.rs:407`), sitting beside a second
island, the priority-cascade CSP in `decision_csp.rs:48`.

Meanwhile the ACTUAL middle IR is two owned representations: `GrammarIr` — a flat arena
`Vec<Expr>` indexed by `ExprId(usize)` with `ExprKind` recursion carried as `ExprId`
(`ir/src/lib.rs:234`) — and `BackendIr`/`BackendExpr`, an owned `Box<BackendExpr>` tree
(`ir/src/lib.rs:376`). Neither is hash-consed; repeated subtrees (JSON `value`, `ws`; CSS
component-value) are re-materialized.

### 1.2 The 2B move: intern the IR into the e-graph

`GrammarIr`'s arena is *already* an interned DAG in all but name — `ExprId` is a `Vec`
index, one step short of a hash-cons. 2B closes that step:

1. **`ExprKind`/`BackendExpr` become an `Id`-childed `Language` enum** (call it
   `BbnfNode`), children projected to `egraph::Id`, byte payloads (`Literal.bytes`,
   `ByteLiteral(Vec<u8>)`) and scalars demoted to leaf metadata. `#[derive(Language)]`
   (`egraph-derive/src/lib.rs:49`) already auto-projects `Id` / `Box<[Id]>` / `Vec<Id>` /
   `[Id; N]` fields and rejects scattered `Id`s — so the enum needs `Seq(Vec<Id>)`,
   `Alt { branches: Vec<Id>, mode }`, `Repeat { body: Id, min, max }`, etc. The port is
   mechanical but real: `Box<BackendExpr>` is *not* an `Id`, so each recursive field is
   rewritten (§7 risk 5).
2. **Construction becomes `EGraph::add`** (`egraph/src/egraph.rs:155`) instead of
   `Vec::push`. Structural sharing (hash-cons memo, `:157`) and congruence are free.
3. **Passes become rewrite groups + analyses.** The nullability fixpoint at
   `ir/src/lib.rs:320` is *already* a monotone-lattice fixed point over expressions — it
   maps one-to-one onto `Analysis::make`/`merge`/`modify` (the `Analysis` trait the
   `EClass.data` field and `should_apply` guard at `rewrite.rs` already carry;
   `EClassFacts::first_set` is referenced there). First/follow sets are the same shape.
4. **Saturation runs under a schedule** — `CspScheduler` (`egraph/src/csp_scheduler.rs:204`)
   is the "schedule under a budget": it models the dirty frontier as AC-3 propagation over
   parent edges (`ParentDirtyProp`, `:120`) with `iter_limit`/`node_limit` (`:175`) from
   `CostConfig`. This is exactly the scheduler 2B's saturation needs; it already exists and
   is wired.
5. **Extraction emits `BackendShape` + scan plan directly.** `Extractor` greedy bottom-up
   (`extract.rs:124`) over a `Lattice` cost (`extract.rs:36`, multi-objective ready) reads
   the saturated graph and yields the winning form; the shape falls out of the extracted
   node's kind. `PROVE-12`'s emit-spine residualization then const-folds the extracted form
   (delimiter-mask beta-reduction) — residualization is a *post-extraction* emit pass, not a
   competing mechanism.

The net architectural claim: **one graph subsumes both islands.** `backend_egraph.rs` and
`decision_csp.rs` collapse into a single saturate-then-extract over the grammar-structured
graph; shape selection is no longer a side computation on a candidate bag but the extraction
of the grammar itself.

---

## 2. Substrate readiness (the engine is production-grade)

The `egraph` crate is a faithful egg implementation, not a toy:

- `add` with FxHashMap hash-cons + parent-edge registration (`egraph.rs:155-196`);
- `union` with size-biased merge target and `Changed`-flag analysis merge
  (`egraph.rs:202-243`);
- `rebuild` — deferred invariant restoration to fixpoint via the pending/analysis worklists
  (`egraph.rs:251-296`), the exact egg "rebuild" of Willsey et al. POPL 2021;
- congruence closure (memo re-canonicalization at `:257-274`);
- `union_count` tracked so pure-union work (no new nodes) is not mistaken for saturation
  (`egraph.rs:39-44`) — a subtle correctness point 2B depends on;
- `with_capacity` (`egraph.rs:78`) is **teardown-conscious**: its doc explicitly names the
  300-node-grammar / 1024-bucket-teardown pathology that dominates a small compile profile —
  direct evidence the Q3 budget concern is already engineered against;
- `CspScheduler` dirty-frontier saturation (`csp_scheduler.rs`), `BackoffScheduler`
  alternative, `Rewrite`/`RewriteFn` with `should_apply` analysis guard (`rewrite.rs`).

Nothing in 2B requires new engine primitives; the work is (a) the IR enum port, (b) a
rewrite/analysis library over grammar structure, (c) the extraction→BackendShape map.

---

## 3. Answering the steering questions

### Q1 — Saturated multiplicity (2B's decisive question)

On the substrate *as it stands*, multiplicity ≈ 1: the decision graph's lone rewrite adds
one normalized alternative to at most the SinkOnly/`DirectBuildNoConsumer` classes, so
e-classes reach size 2 only there, and a genuine ≥2 extraction choice exists **only at the
single unioned root** (`backend_egraph.rs:64`). Childless nodes (`ByteLiteral`, `TapeEmit`,
`Return`) are multiplicity-1 by construction; child-bearing nodes (`Seq`/`Alt`/`Repeat`) are
where multiplicity *could* arise but today does not, because no widening rewrite manufactures
alternatives.

This is the pivot for 2B's whole claim: **if the grammar-structured graph also saturates to
multiplicity ≈ 1, then IR-is-egraph buys structural sharing + analysis-fixpoint unification
but extraction-as-decision is vacuous, and 2B is 2A's island wearing a larger coat.** The
only thing that makes extraction a real decision is a rewrite set that widens (§Q2). The
honest finding stands: *this must be measured, not asserted* (PROTO-2B-1, §6). What follows
either way: with multiplicity 1, 2B still wins on the *representation* axis (one graph, no
`BackendExpr` tree duplication, analyses as e-class data) even if its *decision* axis is
empty.

### Q2 — Widening rewrites (the crux of "end-to-end saturation")

For 2B to be more than canonicalization it needs rewrites that *manufacture* forms:
`Alt`-branch reordering, `Seq` associativity/factoring, `dispatch ↔ speculative` (`AltMode`,
`ir/src/lib.rs:274`), `Repeat` unroll-to-k, `Optional`→`Alt(ε, body)`, and the scan-plan
alternatives of §Q5. Precedent for widening + narrowing coexisting in shipping engines is
settled and directly transferable:

- **egg** (Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, POPL 2021) ships
  bidirectional/widening rules (`(* x 2) ↔ (<< x 1)`) beside narrowing (`x*0 → 0`).
  **Confluence is NOT required** — the e-graph represents *all* derivable forms and
  extraction selects; **termination is guaranteed by the SCHEDULER's node/iter budget**, not
  by rule monotonicity. This is exactly the discipline the extant `CspScheduler.node_limit` +
  `rebuild` growth guard already implement.
- **egglog** (Zhang, Wang, Willsey, Tatlock, PLDI 2023) unifies Datalog with equality
  saturation — "passes as rules over a graph that also carries analyses" is egglog's literal
  thesis and the closest precedent to 2B's whole shape.
- **Semantics preservation** is per-rule: each rewrite is a proven parse-language identity,
  and the tranche floor `regen∘regen` byte-identity is the differential oracle that catches
  any non-preserving rule.

2B's answer to Q2 is therefore egg's answer verbatim: widening admissible, termination
scheduler-bounded, confluence unneeded, semantics oracle-checked. The one 2B-specific hazard
is an *unbounded* widening (`Repeat` with `max = None` unrolled naively) — bounded by
`node_limit`, but a rule bug leaves saturation running to the cap every compile (a budget
regression, §Q3, not a correctness break).

### Q3 — Organ port budget, regen byte-identity, deterministic tie-break

Budget: the current island organ is trivial (~5 nodes). The 2B organ is build (~300 nodes
JSON) + saturate (bounded iters) + extract + **teardown**, against the ~230µs compile budget
(SPEC §0 row 08). The teardown line is the sharp edge — `with_capacity`'s own doc
(`egraph.rs:78`) names table teardown as the profile dominator on small grammars; 2B MUST
size the graph to occupancy and prefer aegraph-style bounded elaboration over open saturation
if the fleet census shows the budget threatened (§Q2/§7, Cranelift precedent).

Determinism through cost-guided extraction: the inadmissible-HashMap-iteration hazard is
**not** in extraction — `Extractor::compute_best` iterates `egraph.classes()`, i.e.
`Vec<EClass>` index order (`extract.rs:140`), and within a class node order is
insertion+rebuild-dedup order (deterministic). The leak risk is (a) any rewrite `search` that
iterates a `HashMap`, and (b) a cost tie not broken by a total order. The extant pattern is
the template to generalize: `DecisionCost` is a full `Ord` tuple ending in a content hash —
`candidate_hash: FNV(id)` (`backend_egraph.rs:246`, `:304`) — so ties resolve by node content,
never by map order. **2B's invariant: every extraction cost is a `Lattice` whose final
tie-break key is a content hash of the e-node.** The design already instruments this belief:
`ActiveCostFacts` carries `determinism_replay_status`, `rewrite_order_replay_count: 2`,
`rewrite_order_variance_pct: 0`, `selection_trace_hash` (`cost.rs:193-196`). PROTO-2B-2 must
perturb rewrite order + hasher seeds and prove byte-identity survives.

### Q4 — Context-sensitive extraction (a 2B-specific liability)

This is the sharpest risk that 2B incurs and the islands sidestep. In an e-graph a shared
sub-rule class has ONE extracted representative. If a rule is reached under both a sink-only
caller and an offset-tape caller and one per-class shape cannot serve both, context-free
per-class extraction gives the wrong shape to one caller. Audit target: `admits_sink_only`
(`passes/src/lib.rs:954`) and its callers in `choose_backend_shape` (`:479`) /
`derive_backend_shape_with_diagnostics` (`:407`, `:661`). If the fleet contains such a
conflict, the identity-preserving repair is context-cloning (duplicate the class per caller
context) — cost = graph blowup + it re-fragments the very sharing that motivated 2B. If NO
conflict exists, context-free per-class extraction is sound and 2B is clean. PROTO-2B-1 must
enumerate the JSON/CSS-L4/sheets rule-call contexts and check.

### Q5 — IR-modeled competing sites

The IR extension is `SimdSite::SelfLoop { terminators }` + `SimdMode::Prefilter`
(`ir/src/lib.rs:425-432`, today `Exact`/`PreEntry` only), the GN-2.W1 seam. Today
`Recognizer::SimdScan` (`:416`) lives in a *separate* `Vec<Recognizer>` on `BackendIr`
(`:355`), OUTSIDE any graph — a fact stapled to the side. For 2B these scan-plan alternatives
become e-nodes: a self-loop scan site carries `{Exact-membership, Prefilter-skip}` as two
forms in one class, each with a real static-size + cyc/B cost, and extraction picks scan-plan
AND backend-shape **jointly** over the one graph. That joint extraction is 2B's genuine
value-add over the two-island status quo, which decides recognizer (`derive_recognizers`,
`passes/src/lib.rs:338`) and shape (`derive_backend_shape`, `:407`) in separate passes with no
shared cost. Lock-2 owns building the IR extension; GN-2.W1 owns the kernel consumer
(`find_member`).

### Q6/Q7 — Solver shape / budget knapsack (2B's stance)

For 2B, **extraction IS the solver** — greedy bottom-up over a monotone `Lattice` is optimal
for monotone cost (`extract.rs:9-11` states this precisely) and runs O(nodes) per fixpoint
pass, well inside budget. PBQP / B&B (2C's terrain) earn their keep only when the objective is
*non-monotone* (sharing-dependent, or a global `bounded_sum` budget coupling independent
sites). 2B's honest boundary: greedy extraction cannot honor a global generated-size knapsack
(a non-local constraint), so a budget-coupled decision either (a) folds into the `Lattice` as
a lexicographic axis (admissible only if the budget is separable) or (b) hands the extracted
candidate set to a bounded post-solve — the same seam LOCK-1's "CSP only where unification is
undecidable" draws. 2B does not claim to replace that post-solve; it claims to *generate its
candidates* from one saturated graph rather than a hand-rolled priority cascade
(`PriorityStep::ALL`, `cost.rs:276`).

### Q8 — Profile-fed cost

The `Lattice`/`CostModel` trait (`extract.rs:85`) takes any cost type; a coefficient fitted
from measured cyc/B enters as the `perf_cost` axis of the extraction cost tuple (cf.
`DecisionCost.perf_cost`, `backend_egraph.rs:242`). A fitted coefficient flips a real decision
iff it reorders two survivors in a class — which requires multiplicity ≥ 2 (back to Q1).
Env-source fencing from regen: the fitted table is a committed fact table (determinism
preserved), never read from the environment at compile time — the `cost_formula_version` /
`schema_version` fields (`cost.rs:174-175`) are the provenance fence.

### XL-F — the GN-2.W1 scan-lowering predicate: 2B's relation is **WRAPS**

The self-loop detection predicate (`derive_recognizers`) is not replaced and does not wait on
2B. Under 2B it becomes a *rewrite* that INTRODUCES the `Prefilter`/`SelfLoop` e-node as an
alternative form in the scan-site class; the predicate still fires (structural detection), but
the cost-guided extraction — not the predicate — decides whether `Prefilter` wins over
`Exact`. Hence **wraps**, with evidence: the predicate's structural test survives as the
rewrite's `search`, its verdict survives as a candidate, and the graph+cost owns the decision.

---

## 4. Integration seams (path:line)

- Engine core — `crates/egraph/src/egraph.rs:155` (`add`), `:202` (`union`), `:251`
  (`rebuild`); `with_capacity` teardown note `:78`.
- Extraction + cost lattice — `crates/egraph/src/extract.rs:36` (`Lattice`), `:85`
  (`CostModel`), `:124` (`Extractor`), `:190` (`extract_tree`).
- `#[derive(Language)]` auto-projection — `crates/egraph-derive/src/lib.rs:49`, field
  classifier `:227`.
- Scheduler (the "schedule") — `crates/egraph/src/csp_scheduler.rs:204`; `ParentDirtyProp`
  `:120`; node/iter limits `:175`.
- Rewrite substrate + analysis guard — `crates/egraph/src/rewrite.rs:36` (`Rewrite`),
  `should_apply` guard, `RewriteFn` work-metric.
- Island to be subsumed — `skinny/crates/passes/src/backend_egraph.rs:60-88` (decision graph),
  `decision_csp.rs:48` (cascade CSP).
- Middle IR to re-home into the graph — `skinny/crates/ir/src/lib.rs:234` (`ExprKind` arena),
  `:376` (`BackendExpr` owned tree), `:361` (`BackendShape`, the extraction target),
  `:416-432` (`Recognizer`/`SimdMode`/`SimdSite`, the Q5 extension home).
- Analysis exemplar (already a lattice fixpoint) — `skinny/crates/ir/src/lib.rs:320`
  (`nullability`).
- Compile pipeline entry / decision points — `skinny/crates/passes/src/lib.rs:32` (`compile`),
  `:338` (`derive_recognizers`), `:407` (`derive_backend_shape_with_diagnostics`), `:479`
  (`choose_backend_shape`), `:954` (`admits_sink_only`, Q4 audit).
- Residualization (post-extraction emit) — PROVE-12 REPORT; template
  `crates/codegen/src/runtime_generator.rs` `CSS_GENERATED_RS`.

---

## 5. Prior art (with citations)

- **egg** — Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, "egg: Fast and Extensible
  Equality Saturation," *POPL 2021*. The substrate's exact lineage (rebuild / deferred
  invariant restoration = `egraph.rs:251`; e-class analysis = the `Analysis` trait; greedy
  extraction = `extract.rs`). Establishes scheduler-bounded termination without confluence —
  2B's Q2 answer.
- **egglog** — Zhang, Wang, Willsey, Tatlock, "Better Together: Unifying Datalog and Equality
  Saturation," *PLDI 2023*. Passes-as-rules over a graph carrying analyses = 2B's shape. The
  strongest precedent that an IR *is* an e-graph + a fact store.
- **Cranelift aegraph** — Chris Fallin, "acyclic e-graphs" mid-end (Cranelift, 2023). A
  SHIPPING production compiler that made its mid-IR an e-graph end-to-end — direct precedent
  for 2B — but deliberately chose an **acyclic e-graph with single-pass bottom-up elaboration
  instead of open saturation** because full saturation blew the per-compile (JIT) budget. This
  is simultaneously 2B's precedent and 2B's sharpest warning (§7 risk 2, §Q3).
- **Denali** — Joshi, Nelson, Randall, "Denali: A Goal-directed Superoptimizer," *PLDI 2002*.
  The original "compile via e-graph + matching," historical proof that per-compile e-graph
  optimization is viable at small scale.
- **MLIR PDL/PDLL + greedy pattern-rewrite driver** — the *counter*-precedent: a major IR that
  keeps passes as *destructive* rewrite groups and explicitly does NOT use e-graphs. Frames the
  null hypothesis 2B must beat: does the shared-alternatives graph earn its cost over ordered
  destructive rewriting on grammars this small?

---

## 6. What a prototype must test (to move 2B)

- **PROTO-2B-1 — multiplicity + context census (answers Q1, Q4; decides 2B vs 2A).** Build the
  full JSON / CSS-L4 / sheets middle IR as `EGraph<BbnfNode>`; apply a candidate widening set
  (Alt-reorder, Seq-assoc/factor, dispatch↔speculative, Repeat-unroll≤k, scan-plan
  Exact↔Prefilter); saturate under `CspScheduler`; MEASURE avg/max e-class size, count of
  ≥2-survivor classes at child-bearing nodes, and whether any extraction faces a real cost tie.
  Simultaneously enumerate rule-call contexts and flag any sink-only/offset-tape shared-class
  conflict (Q4). Verdict gate: if multiplicity ≈ 1 fleet-wide, 2B is reported as
  representation-win-only, decision-axis-empty (an honest negative on the "end-to-end
  saturation" claim).
- **PROTO-2B-2 — budget + determinism.** Time build+saturate+extract+**teardown** on the
  three-grammar fleet cold; assert ≤230µs order; run `regen∘regen` and assert byte-identity
  through cost-guided extraction; perturb rewrite application order and Fx hasher seeds to prove
  the content-hash tie-break holds. Head-to-head: open saturation vs aegraph-style single-pass
  elaboration on the budget (the Cranelift question).
- **PROTO-2B-3 — extraction emits BackendShape + scan plan jointly.** Prove one saturated graph
  reproduces the current cascade's selected shapes (parity with `choose_backend_shape`) AND
  jointly selects `SelfLoop`/`Prefilter` where GN-2.W1 qualifies — i.e. the unified graph
  subsumes both `backend_egraph.rs` and `decision_csp.rs` with zero regression in selected
  shapes and a decision `Prefilter` never manufactured by the two-island path.

---

## 7. Named risks and counterexamples

1. **Multiplicity collapse (Q1).** If bbnf grammars saturate to multiplicity ≈ 1,
   extraction-as-decision is vacuous and 2B degenerates to an interned-DAG + analysis-fixpoint
   refactor of 2A. Counterexample to hunt: a grammar where two Seq/Alt orderings genuinely tie
   on the cost lattice.
2. **Budget blow — Cranelift's own finding (Q3).** Full equality saturation on a 300-node IR
   may exceed 230µs; aegraph's single-pass elaboration is the shipping mitigation. Mitigation:
   bound saturation to the `node_limit`/`iter_limit` already present, or adopt acyclic
   elaboration if PROTO-2B-2 shows the budget threatened.
3. **Unbounded widening (Q2).** `Repeat { max: None }` unrolled naively runs to the node cap
   every compile — a budget regression, not a correctness break. Mitigation: unroll rules carry
   an explicit k; the scheduler growth guard (`csp_scheduler.rs:280`) is the backstop.
4. **Determinism leak.** Any rewrite `search` iterating a `HashMap`, or a cost tie without a
   content-hash final key, breaks regen byte-identity. Mitigation: enforce the
   `Ord`-tuple-ending-in-content-hash invariant (the extant `candidate_hash` pattern) as a lint
   on every `CostModel` impl.
5. **`BackendExpr` impedance.** The owned `Box<BackendExpr>` tree and `Vec<u8>` literals must be
   rewritten to `Id`-children + side-tabled byte payloads before the derive macro applies.
   Mechanical but touches every `BackendExpr` construction site in codegen — the port is the
   single largest concrete cost of 2B.
6. **Context-sensitivity (Q4).** Shared sub-rule classes force one shape per class; a
   caller-context conflict requires context-cloning that re-fragments sharing — the exact value
   2B claims. This is the one liability 2B carries that the islands do not.

**Bottom line.** 2B is buildable on a production-grade extant engine with one structural IR
change and a rewrite/analysis library; it is architecturally cleaner than the two-island status
quo and uniquely enables *joint* scan-plan + backend-shape extraction. Its entire
distinctiveness rests on saturated multiplicity being > 1 in the fleet and the per-compile
budget surviving saturation — both measurable, neither yet measured. PROTO-2B-1 and PROTO-2B-2
are the gates that convert 2B from plausible to proven or refute it to a representation-only
refactor.
