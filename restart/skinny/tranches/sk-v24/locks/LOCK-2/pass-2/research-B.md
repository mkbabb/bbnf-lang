# LOCK-2 · Family 2B — IR-IS-EGRAPH · pass-2 research

*Thesis: the middle IR is an e-graph end to end; the pass pipeline is a set of rewrite
groups run under one saturation schedule; extraction emits `BackendShape`/`BackendIr`
directly. This report develops that family to its strongest concrete form, names where the
substrate already carries it, and names exactly where it must be refuted or repaired.*

---

## 1. The mechanism, made concrete

The family's claim is a single structural transposition: the tree IR
`BackendExpr` (`skinny/crates/ir/src/lib.rs:376`, recursive via `Box<BackendExpr>` /
`Vec<BackendExpr>`) is replaced *as the working representation between infer and lower* by an
`EGraph<BackendNode, EClassFacts>`, where `BackendNode` is the **flattened** isomorph of
`BackendExpr` — every `Box<BackendExpr>`/`Vec<BackendExpr>` child becomes an `Id`. This is the
egg discipline (an e-node's children are e-class ids, never sub-trees) and it is exactly the
shape `egraph-derive` already projects: `Id`, `Box<[Id]>`, `Vec<Id>`, `[Id; N]`
(`crates/egraph-derive/src/lib.rs:7-9`). `BackendNode` therefore derives `Language`
mechanically — `Seq(Vec<Id>)`, `Alt{mode, branches: Vec<Id>}`, `RepeatLoop{body: Id, min}`,
`RegexProgram{pattern, span_kind}` (leaf), `CallRule{callee}` (leaf), `TapeEmit{kind}` (leaf,
carrying `BackendShape` as scalar metadata). The `AltMode`/`SpanKind`/`TapeKind` scalars ride
as non-`Id` fields (auto-classified leaves, `egraph-derive/src/lib.rs:8`).

The pipeline (`skinny/crates/passes/src/lib.rs:32` `compile`: infer → `derive_recognizers`
(:338) → `derive_backend_shape_with_diagnostics` (:407) → `decision_csp::finalize_rule` (:505))
collapses into:

1. **Build.** Lower `GrammarIr` rule bodies into the e-graph bottom-up (`EGraph::add`,
   `egraph.rs:155`), one root e-class per rule; `CallRule` leaves knit the rule graph into one
   congruence-closed structure so shared sub-rules are shared e-classes by construction (the
   hash-cons at `egraph.rs:157` interns identical sub-terms once).
2. **Saturate.** Each former pass is a **rewrite group** — a `Vec<&dyn RewriteFn>` — run by the
   `CspScheduler` (`crates/egraph/src/csp_scheduler.rs:204`) to a fixed point: shape-lowering
   rewrites (a `TapeEmit` class gains `SinkOnly`/`OffsetTape`/`EagerTape` alternatives),
   scan-recognition rewrites (a `Seq(ByteLiteral, RepeatLoop<charclass>)` gains a
   `SimdScan{Prefilter}` alternative), regex-canon rewrites at the leaf. The scheduler already
   carries `iter_limit`/`node_limit` termination (`csp_scheduler.rs:178-180`, config at
   `crates/egraph/src/cost_config.rs`).
3. **Extract.** `Extractor::new(&graph, &BackendCostModel)` (`extract.rs:124`) computes best
   per class bottom-up; `extract_tree(root_rule)` (:190) reconstructs the chosen `BackendNode`
   tree, re-inflated to `BackendExpr`/`BackendIr` for the existing lowerer
   (`codegen::lower::rust::lower_to_rust`, the declared same-wave consumer,
   `backend_egraph.rs:11`). Extraction **is** the shape decision — `TapeEmit`'s `BackendShape`
   scalar is read off the extracted node, deleting the separate per-rule decision island.

The family's payoff is unification: `derive_backend_shape`'s bespoke candidate ranking, the
`DecisionNode` island (`backend_egraph.rs:151`, childless `[Id; 0]` nodes), and
`decision_csp`'s finalize all become *one* cost-ranked extraction over *one* e-graph. Congruence
closure gives cross-rule sub-expression sharing for free (a sub-rule used by two callers is one
e-class, costed once); the tree IR re-costs it per occurrence.

## 2. The substrate — what exists, what is missing

**Exists and directly reusable.** `EGraph<N,A>` with hash-cons, union, worklist rebuild
(`egraph.rs`), `with_capacity` teardown-aware sizing (:78, the memo-table-drop concern already
documented); the `Language`/`LanguageChildren` traits (`language.rs`) + `egraph-derive`
(recursive-field projection); `Rewrite`/`RewriteFn` with `search`/`should_apply`/`apply`
(`rewrite.rs:34-70`) and the LLVM-style `Changed`-by-work-delta convergence metric (:91,
node-delta + union-delta — matches memory law *changed-flag-convergence*); `CspScheduler`
(the AC-3 dirty-closure saturation driver, `csp_scheduler.rs`); `Extractor` greedy bottom-up
with the `Lattice`/`CostModel` seam (`extract.rs:36-94`, `Scalar<T>` newtype for future Pareto).
The `Analysis` trait (`egraph.rs:8`, `A::make`/`merge`/`modify`) is the per-class-facts seam.

**Missing (the family's build surface).** (a) `BackendNode` — the flattened isomorph of
`BackendExpr`; net-new enum + `#[derive(Language)]`, plus a total `BackendNode↔BackendExpr`
functor (build direction lowers from `GrammarIr`; extract direction re-inflates). (b)
`EClassFacts` analysis — `first_set`, nullability, `admits_sink_only` (§5 Q4), computed in
`A::make`; the `should_apply` guard (`rewrite.rs:58`) already exists to consult it. (c) The
rewrite groups themselves (shape-lowering, scan-recognition, regex-canon). (d) A deterministic
total-order tie-break for extraction (§5 Q3). The current e-graph consumer
(`backend_egraph.rs`) is a **degenerate island**: ≤5 childless `DecisionNode`s per rule all
unioned into one class, one `NormalizeDirectSinkCost` rewrite, extract-cheapest. It exercises
none of congruence, sharing, or recursive extraction — precisely the machinery 2B activates.

## 3. Integration seams (path:line)

- **Build entry.** Replace the per-rule candidate construction feeding `backend_egraph::select`
  (`passes/src/lib.rs:504`) with a whole-grammar e-graph build from `GrammarIr` rule bodies.
- **Extract → consumer.** `extract_tree(root)` output re-inflated to `BackendIr`
  (`ir/src/lib.rs:352`) hands to `codegen::lower::rust::lower_to_rust`
  (`backend_egraph.rs:11`, the `SAME_WAVE_CONSUMER`).
- **Shape metadata.** `BackendShape` (`ir/src/lib.rs:361`, 5 variants) rides `TapeEmit` as a
  scalar; `all_backend_shapes()` (`cost.rs:334`) enumerates the lowering-rewrite target set.
- **Scan competition.** `Recognizer::SimdScan{mode,alphabet,site}` (`ir/src/lib.rs:416`) with
  `SimdMode`/`SimdSite` today single-variant (`:425/:430`, `Exact`/`PreEntry`); GN-2.W1
  (`docs/tranches/sk-v24/GN-2.md:199`) widens them to `Prefilter`/`SelfLoop` — that widening is
  the *first IR-modeled competing site* a scan-recognition rewrite group can range over.
- **Cost facts.** `CostFacts`/`ActiveCostFacts` (`ir/src/cost.rs:5/173`), `Measurement`
  (`cycles_per_byte_x1000`, `cost.rs:311`), `EvidenceSource` (`BenchProbe`/`AuthorDeclared`/
  `StaticAnalysis`, the regen fence, §5 Q8), `CALIBRATED_WEIGHTS`
  (`crates/egraph/src/cost_weights.rs`) — the `BackendCostModel` reads committed coefficients.
- **Budget.** The post-hoc `budget_status` check (`backend_egraph.rs:100`) is the constraint
  that 2C would fold into extraction (§5 Q7); the grammar `loc_budget` (R1 §1, json=4000).
- **Residualization exemplar.** PROVE-12 (`prototypes/PROVE-12/REPORT.md`) const-folds a
  delimiter membership to a `u128` mask at generation time (+18 LOC, cyc/B 0.81–0.98×,
  disasm-verified) — a *narrowing* rewrite already proven; the shape a lowering rewrite emits.

## 4. Prior art (cited)

- **Cranelift aegraph** — Chris Fallin, *"acyclic egraphs and how to use them"* (2023) +
  Cranelift RFC "egraph-based mid-end". The family's direct precedent and strongest external
  support: a *production* compiler whose entire mid-end IR is an (acyclic) e-graph, rewrites are
  a match/rewrite set, and lowering extracts the cheapest term. Aegraph's two departures from
  egg are load-bearing here: (i) **eager, single-pass rewriting** (no fixed-point rebuild)
  because their rules are largely one-directional/narrowing — directly relevant to §5 Q1
  (multiplicity ≈ 1); (ii) **scoped elaboration** carrying context, the answer template for §5
  Q4 context-sensitive extraction.
- **egg** — Willsey et al., POPL 2021, *"egg: Fast and Extensible Equality Saturation."* Origin
  of the rebuild-deferred worklist (`egraph.rs:251` is this), the `Analysis` seam, and greedy
  bottom-up extraction (`extract.rs`). **egglog** — Zhang et al., PLDI 2023 — Datalog +
  e-graph, relevant if rewrite groups grow relational conditions.
- **Equality saturation** — Tate, Stepp, Tatlock, Lerner, POPL 2009. The founding argument that
  an optimizer can *be* a saturating e-graph with global extraction — the family's thesis in its
  original form; its caveat (extraction is the hard part; a pseudo-boolean/ILP extractor was
  needed) is 2B's §5 Q6 concession.
- **Extraction optimality** — SPORES (Wang et al., VLDB 2020) and Tensat (Yang et al., MLSys
  2021) both use **ILP extraction** because greedy is suboptimal under sharing; extract.rs's own
  doc concedes "not optimal for cost models that depend on sharing" (`extract.rs:9-11`). This is
  the boundary where 2B hands off to a solver (2C's PBQP, or ILP).
- **PBQP** — Scholz & Eckstein, LCTES 2002; Hames & Scholz, CC 2006 (LLVM `RegAllocPBQP`). The
  right model for *independent per-site discrete choice with pairwise coherence costs* — when
  extraction must respect coupling edges greedy can't see. On a near-tree IR with few coherence
  edges PBQP is optimal and near-linear.
- **Colored e-graphs** — Singher & Itzhaky, 2023 — multiple equivalence relations in one
  e-graph; the mechanism for context-sensitive extraction (§5 Q4) without cloning the graph.
- **rustc/MIR & MLIR** as contrast: both keep a *tree/SSA* mid-IR with pass-ordered rewriting,
  *not* an e-graph — the null hypothesis 2B must beat on the ~230 µs budget (§5 Q3).

## 5. The steering questions, answered from the substrate

**Q1 · Saturated multiplicity (the family's decisive risk).** On the substrate the honest
expectation is **e-class size ≈ 1 after saturation** for the current fleet: grammars are ~300
nodes (R1), the only live rewrite is a single normalization, and no *widening* rule exists — so
saturation reaches a normal form with no surviving choice. The degenerate island
(`backend_egraph.rs`, ≤5 leaf candidates) confirms multiplicity is small and concentrated at
*childless* shape-decision nodes, not child-bearing structural nodes. **Consequence for 2B:**
where multiplicity ≈ 1, an end-to-end e-graph is pure overhead versus a cost-ranked
term-rewrite to normal form — the family cannot be justified by the *representation* alone; it
must either (a) manufacture genuine multiplicity via widening rewrites (Q2) at ≥1 real site
whose alternatives have *binding, differentiated* cost, or (b) concede and adopt aegraph's
eager single-pass rewriting (no rebuild loop) as the honest form — an e-graph in name, a
normalizing rewriter in cost. The §6 prototype must *measure* saturated e-class sizes on the
fleet to decide (a) vs (b). This is the family's make-or-break number.

**Q2 · Widening rewrites.** 2B needs at least one rewrite that *adds* a form (a scan-recognition
rule: `Seq(ByteLiteral, RepeatLoop<neg-class, card≤16>)` ⇒ `SimdScan{Prefilter, terminators}`,
the GN-2.W1 pattern, `GN-2.md:203`) coexisting with all-narrowing lowering rules.
(a) **Confluence** with narrowing rules: guaranteed by saturation — order-independence is the
e-graph's defining property; the extractor, not the rewrite order, picks the winner. (b)
**Termination**: the widening rule fires at most once per matching class (guarded by
`should_apply`, `rewrite.rs:58`, aborting when the class already holds the widened form — the
`#direct-sink-normalized` idempotence guard at `backend_egraph.rs:204` is the existing pattern),
plus the scheduler's `node_limit` (`csp_scheduler.rs:280`). (c) **Semantics preservation**: each
rewrite is an *observational parse-equivalence* — extraction may only cross it if parse output is
byte-identical; PROVE-12's parity checksum (`REPORT.md:154`) is the oracle template. Precedent
for widening+narrowing coexistence: egg's `expand`/`simplify` rules routinely coexist; Cranelift
aegraph runs algebraic widenings beside strength-reduction narrowings under one extraction.

**Q3 · Organ port budget + determinism.** The ~230 µs compile budget (SPEC §0 row 08 — HM core
derives a schema in ~230 µs). Build+saturate+extract+**teardown** on a ~300-node grammar: the
named risk is *teardown*, explicitly flagged at `egraph.rs:78` — an un-hinted `FxHashMap` memo
settles at 1024 buckets on a 300-node grammar and its drop "dominates the compile profile."
Mitigation is `with_capacity(expected_nodes)` (:78) sized from `GrammarIr` node count. Whether
the whole organ holds 230 µs is **SPEC-ONLY until §6 measures it** — a hard, falsifiable budget
gate, not an assertion. **Determinism / `regen∘regen` byte-identity:** the extractor iterates
`egraph.classes()` (Vec order, deterministic, `egraph.rs:138`) and keeps the first
strictly-improving node (`extract.rs:155`, `cost < *existing`) — deterministic *iff* e-node
insertion order is deterministic and ties are broken by a **total order**, not `FxHashMap`
iteration. Union is commutative so the *saturated* graph is order-invariant, but the extracted
*representative* on a genuine tie is not unless cost carries a tie-break field. The scheduler's
`DirtyClassSet` (`FxHashSet`, `rewrite.rs:24`) governs apply order — again order-invariant at
the graph level, but a HashMap-iteration tie-break at extraction would be inadmissible. The fix:
extend `BackendCost` with a terminal `stable_hash(node)` field exactly as the island already
does (`DecisionCost.candidate_hash`, `backend_egraph.rs:246` — FNV over the id), making `Cost` a
strict total order. The island already claims replay-determinism
(`determinism_replay_status:"pass"`, `rewrite_order_replay_count:2`, `backend_egraph.rs:139`);
2B must extend that replay gate to the recursive extractor.

**Q4 · Context-sensitive extraction (a real counterexample for 2B).** Greedy bottom-up
extraction (`extract.rs`) picks **one** best node *per class, globally* — it structurally
*cannot* extract a shared sub-rule differently under two callers. If the fleet contains a
sub-rule reached under both a `SinkOnly` caller and an `OffsetTape` caller where one per-class
shape cannot serve both, 2B's extractor is *wrong*, not merely suboptimal. **Audit obligation:**
`admits_sink_only` must be located and its call-graph checked for exactly this diamond (I did
not find `admits_sink_only` in `ir`/`passes` src on this pass — it must be surfaced or shown
absent). If **no** such conflict exists, per-class context-free extraction is sound and 2B is
clean. If it exists, the identity-preserving repairs, cheapest first: (i) **e-class cloning /
context-splitting** — duplicate the shared class per calling context *before* saturation (cost:
loses the sharing that motivated 2B, inflates multiplicity); (ii) **colored e-graphs**
(Singher & Itzhaky) — one graph, per-context equivalence colors, color-aware extraction; (iii)
aegraph **scoped elaboration** — carry caller context into extraction. This is the single
sharpest technical objection to the family; the prototype must resolve it empirically.

**Q5 · IR-modeled competing sites.** Today **no** fleet grammar presents ≥2 scan/self-loop sites
modeled *in the IR* with binding static-size + cyc/B costs: `SimdMode`/`SimdSite` are
single-variant (`ir/src/lib.rs:425/430`), so the IR cannot yet *express* competition. GN-2.W1
(`GN-2.md:199-208`) is the lock that builds the `SelfLoop`/`Prefilter` extension — until it
lands, 2B's scan-recognition rewrite group has an empty range and the competing-site motivation
is prospective. This bounds 2B's near-term scope to the *shape-lowering* rewrite group (which
does have 5 real `BackendShape` alternatives) and defers the scan group to a GN-2-dependent
wave. Ownership: LOCK-2 owns the e-graph/rewrite machinery; GN-2 owns the IR enum widening — 2B
*consumes* it.

**Q6 · Solver shape (where 2B cedes).** For the shape-lowering decision — independent per-rule
discrete choice over 5 shapes with a global size budget and few coherence edges — greedy
extraction is optimal *only when cost is separable* (no cross-site sharing). Under sharing
(Q4's diamond, or a size budget coupling two rules) it is not: extract.rs concedes this
(:9-11), SPORES/Tensat use ILP for exactly this reason. On a near-tree decision graph PBQP
(Scholz–Eckstein) is optimal, near-linear, and strictly separates from B&B on solve cost. **2B's
position:** the e-graph *generates* the candidate space (its true strength — congruence,
sharing, saturation) and hands a clean per-class candidate set to whatever extractor the cost
structure demands: greedy when separable, PBQP/ILP when coupled. 2B does not itself claim the
solver — it claims the *candidate substrate*, and the extractor plugs in (the `CostModel` seam,
`extract.rs:85`, is already the plug point). This is the honest boundary with family 2C.

**Q7 · Budget knapsack.** The generated-size budget is checked *after* selection today
(`backend_egraph.rs:100`, `budget_status`). Folding it as a `bounded_sum` constraint *into*
extraction (a knapsack: choose per-site shape s.t. Σ static-size ≤ ceiling, min Σ perf-cost)
is **not** selection-equivalent to a priority cascade in general — the cascade fixes an order,
the knapsack optimizes density; they coincide only when the cascade order equals greedy-by-
density order. So it is a genuine architecture change, stated as such. One solver home:
`csp-solver` (already the `CspScheduler` engine, `csp_scheduler.rs:30`) hosts the constraint
store; the budget is one global `bounded_sum` over the per-site shape variables the extractor
exposes. 2B keeps candidate generation; the budgeted extraction is 2C's constraint.

**Q8 · Profile-fed cost.** `Measurement.cycles_per_byte_x1000` + `EvidenceSource`
(`cost.rs:311/321`) already model a fitted coefficient; `CALIBRATED_WEIGHTS` (`cost_weights.rs`)
is the committed fact table. A coefficient *can* flip a per-site shape decision (e.g. tip
`OffsetTape`→`SinkOnly` when measured value-start cyc/B favors the fused sink). Attribution when
whole-grammar cyc/B underdetermines per-feature coefficients is an **A/B-differential** problem
(vary one feature, diff the paired cyc/B) — family 2D's core; 2B only *consumes* the fitted
`CostModel`. Regen fence: `EvidenceSource::BenchProbe` (env-derived) is distinguished from
`AuthorDeclared`/committed (`cost.rs:321`); regen must read only committed
`CALIBRATED_WEIGHTS`, never live PMU — the enum *is* the fence, and 2B's determinism gate (Q3)
enforces it (a `BenchProbe`-sourced coefficient reaching extraction would break `regen∘regen`).

**XL-F supersession (scan-lowering predicate).** The GN-2.W1 predicate (self-loop, complement
card ∈ [1,16], `GN-2.md:627`) that 2B's scan-recognition rewrite hosts: it **wraps** the GN-2
predicate (2B expresses it as a `search`/`should_apply` guard over `BackendNode`) rather than
replacing it, and **waits-on** the `SimdMode::Prefilter` IR widening (Q5). Evidence: the
predicate is grammar-structural (`GN-2.md:206-208`), which is precisely a `Rewrite::search`
body — a wrap, not a rewrite of the predicate's semantics.

## 6. What a prototype must test (to move this family)

1. **Multiplicity census (decisive).** Build the whole-grammar `BackendNode` e-graph for
   json/css-l4/sheets, run the shape-lowering rewrite group to saturation, report the
   distribution of saturated e-class sizes and the count of classes with ≥2 survivors, split by
   childless vs child-bearing. If max size ≈ 1 fleet-wide → 2B collapses to aegraph-style eager
   rewriting (report it as the honest form, not a defeat).
2. **Budget gate (falsifiable).** Cold-time build+saturate+extract+teardown per grammar vs the
   ~230 µs HM-core reference and vs a tree-IR normal-form baseline. `with_capacity`-sized memo
   (`egraph.rs:78`). RED if the organ exceeds the tree baseline by a margin the sharing win
   can't repay.
3. **`regen∘regen` byte-identity** through the recursive extractor with the total-order
   tie-break (Q3), replayed ≥2× under shuffled `DirtyClassSet` order — the extracted `BackendIr`
   must be byte-identical.
4. **Context-diamond audit (Q4).** Locate `admits_sink_only`; search the fleet call graph for a
   shared sub-rule under conflicting-shape callers. If found, prototype the cheapest identity-
   preserving repair (clone vs color) and measure its multiplicity/budget cost.
5. **Parity oracle.** Every rewrite carries a byte-identical parse-output check (PROVE-12
   checksum template, `REPORT.md:154`) before its edge may be crossed in extraction.

**Bottom line.** The substrate *structurally* carries IR-IS-EGRAPH — `BackendExpr` flattens to a
derivable `Language`, the scheduler/extractor/analysis seams all exist, and Cranelift's aegraph
is a shipping existence proof. The family lives or dies on **one measured number** (§6.1): does
the fleet exhibit genuine post-saturation multiplicity at a site with binding differentiated
cost? If yes, 2B unifies three decision passes into one saturating e-graph with free cross-rule
sharing. If no, the honest form is aegraph's eager rewriter — an e-graph whose value is the
congruence-shared candidate substrate handed to a separable extractor, not a saturation loop.
Either way the candidate-generation role is real; the saturation-loop role is contingent on §6.1.
