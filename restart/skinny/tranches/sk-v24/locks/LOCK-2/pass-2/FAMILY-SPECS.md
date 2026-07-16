# LOCK-2 — THE GRAPH ENGINE — pass-2 FAMILY SPECS (synthesis)

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity &
schedule decisions, profile-fed cost, deterministic regen. Four families — 2A EGRAPH-ISLANDS ·
2B IR-IS-EGRAPH · 2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP — distilled to one cogent spec each
while routes remain incompatible, then the incompatibility map. Synthesis over
research-{A,B,C,D,priorart} pass-2, the pass-1 registry, and the CROSS-LOCK adjudications
(XL-A..H), which have RESHAPED the fork map since pass 1. Fable-tier, pass 2.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at
runtime (v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios
certified / absolutes directional (iv); deterministic regen (`regen∘regen = regen`
byte-identical); extant GN/PT codex is the baseline, divergences are NAMED supersessions (ix).

---

## 0. What the cross-lock adjudications did to this lock — the fork map has narrowed

Pass 1 named the "primary architectural fork" as intra-lock **2A ⊗ 2B** and then corrected itself
to **LOCK-2 ⊗ LOCK-3** (tower ownership). Three cross-lock rulings, conveyed as facts, now settle
enough of that boundary that the pass-2 landscape is materially different from pass 1's:

- **XL-A (settled) dissolves the port-direction question.** The greenfield loom (GN-1) is the
  tree-of-record-to-be; **skinny@HEAD is the MEASUREMENT SUBSTRATE** — the only living
  implementation; legacy `crates/` archives at GN-0; **ports scope organ-level, never tree-level.**
  Pass 1's fear that "PROVE-2B-PORT's direction inverts" is void: there is no port *between* trees
  to direct. There is one substrate to *measure in* (skinny) and one structure to *target*
  (greenfield). "Adopt-and-prove" reduces to: prove the organ's behavior on the skinny substrate;
  design its home in greenfield layout.
- **XL-B (working, final at GESTALT-B) forecloses 2B's maximal reading.** Graph-opt engines
  operate **WITHIN** LOCK-3's middle-IR tower at **named decision points**; the tower's shape is
  not LOCK-2's to redraw. 2B's signature move — *"the middle IR IS one e-graph container;
  `derive_backend_shape` is deleted; extraction IS the lowering/driver"* — **seizes the decision-
  driver seat XL-B assigns to LOCK-3.** Under XL-B the lawful residual of 2B is "e-graph as the
  in-place node substrate AT named decision points," which is **operationally the 2A island
  posture**. The pass-1 "primary fork 2A⊗2B" therefore degrades from an ownership war to a
  *mechanism difference inside 2A's frame* (discrete build-teardown islands vs an in-place e-node
  substrate spanning the tower). Prior art predicted exactly this: Cranelift's own measurement
  (ægraph, 23%-rebuild-vs-10%-budget, avg e-class 1.13) drove the "IR-is-e-graph end-to-end with
  full saturation" design to the acyclic no-saturation form — *which is 2A*.
- **XL-C (ratified) homes Island A off this lock.** Two domain-fit regex types; the engine tier
  keeps rich `Hir`; the canonical engine home is **parse-that** (the vendored copy = the
  substrate). So 2A's regex-HIR-canon island (I2) is an **engine-tier, parse-that** concern that
  *consumes the shared e-graph rewrite library* — it is cross-lock (LOCK-5), not a bbnf grammar-
  tier island LOCK-2 instantiates. LOCK-2 owns a rewrite set + cost model it could contribute;
  it does not own the site.
- **XL-F names the GN-2.W1 supersession: WRAPS.** Both 2A and 2C research converge (independently)
  that the scan-lowering predicate is **wrapped, not replaced, not waits-on**: the grammar-
  structural detection (Kleene over a class whose complement cardinality ∈ [1,16]) becomes the
  **candidate generator** feeding the scan-plan island; the island/CSP adds the cost-ranked *tier*
  choice (Exact vs Prefilter vs SelfLoop) the predicate had no vocabulary for. Recorded as the
  named supersession clause ix demands.

**The one master gate that survives every adjudication — unchanged and still unresolved.** With
today's **all-narrowing** rewrite set (`DeduplicateAltBranches`, `SupersetAbsorbAlt`,
`UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`, …) the substrate is **inert**:
extraction decides nothing a `min_by_key` would not. This is **triple-confirmed** — the in-tree
54-config `CALIBRATED_WEIGHTS` null (DTA state counts identical across every config: JSON 51, BBNF
476, CSS-L4 2875, Sheets 191), Cranelift's external avg-class-1.13 / 2-picks-in-4M, and the
project's own shipping grammar e-graph rule set being all-narrowing. **Therefore the single fact
gating 2A-I2/I4, 2B, 2C's non-degenerate objective, and 2D's lattice alike is: does a REGISTERED
WIDENING rewrite, fired through the real scheduler on a real fleet grammar in the skinny substrate,
manufacture ≥2 semantically-distinct survivors at a CHILD-BEARING node — AND is it confluent with
the all-narrowing set AND terminating — such that the Extractor picks differently under two
CALIBRATED weight vectors AND the two picks move silicon at real value-start positions?** PROTO-1
(pass 1) got close but (a) ran on the legacy tree, (b) hand-`union`ed the twin rather than firing a
registered `Rewrite` through `CspScheduler`, and (c) never discharged confluence/termination — the
step both critics flagged as the elegant-reduction trap and the project's own all-narrowing
shipping set proves is *not* trivial. **This is Brief 1, and until it fires, the honest LOCK-2 is
"canonicalization + the existing BackendShape driver," with all extraction/CSP-extraction machinery
recorded SPEC-ONLY.**

**The banked asset that is real.** The `bounded_sum` knapsack propagator (PROTO-2: `+345` additive
LOC, wired via the existing `Custom` seam, `cargo test -p csp-solver --test bounded_sum` = 6 passed
/ 0 failed) is BANKED. Confirmed this pass: the main-tree `crates/csp-solver/src/constraint/` set is
still `{all_different, all_different_except, dispatch, implication, lambda, not_equal, scratch}` —
`bounded_sum` lives only in the PROTO-2 worktrees, i.e. built-and-tested, not-yet-merged. It is the
global coupler for the scan-plan CSP that both 2A (Island B) and 2C need, and it is a *conservative
extension* of the priority cascade (equal in the budget-slack regime, better in the budget-tight
regime the cascade is blind to).

---

## Family 2A — EGRAPH-ISLANDS

**Architecture.** An LLVM-shaped **directed pass pipeline** with a small, named set of e-graph
**islands** planted at decision points inside LOCK-3's tower (XL-B: *within*, at named points). Each
island is a *canonicalizing organ* — `build → saturate(budgeted) → extract(total-order) → teardown`
— reading a **committed** cost fact-table and feeding **exactly one** same-wave consumer that
changes emitted code. Above the islands sits a thin **selection layer**: a CSP over the extracted
candidates that resolves the few cross-site couplings the local islands cannot see (the shared
generated-size budget; pairwise coherence between adjacent scan sites). **Islands GENERATE; the CSP
SELECTS.** This split is the family's spine and the reason it is not the whole-IR e-graph: local
canonicalization saturates in microseconds and the genuinely global coupled decisions are few and
belong to a solver, not to saturation. Post-XL-B this is also the lawful home of 2B's surviving
mechanism (see incompatibility map §I).

**Mechanism — two island profiles, four candidate sites.** Research-A's decisive refinement:
islands split by node kind. A **leaf profile** (no scheduler loop; direct total-order min) for
childless decision nodes where multiplicity is provably small; a **tree profile** (saturate +
bottom-up extract) for child-bearing nodes where a subterm rewrites many ways and the cheapest
depends on shared-child cost.

| island | node kind | profile | multiplicity | consumer (same-wave) | home |
|---|---|---|---|---|---|
| **I1 backend-shape** (extant) | childless leaf | leaf | 2–5 post-prune | `codegen::lower::rust::lower_to_rust` | LOCK-2 (skinny) |
| **I2 regex-HIR canon** | child-bearing | tree | genuine ≥2 | engine-tier NFA/prefilter build | **parse-that / LOCK-5 (XL-C)** |
| **I3 scan-plan selection** | leaf-per-site + budget edge | leaf + CSP | ≥2 *sites*, few *forms* | `lower/recognizer/` → `find_member`/`dfa.find` | LOCK-2 selection / **LOCK-3 node shape** |
| **I4 projection canon** | child-bearing | tree | small | `schema_direct` / tape emitter | LOCK-2, **double-homed LOCK-1 (XL-F/1D)** |

I1 is real and consumer-attached today: `passes/src/backend_egraph.rs` builds
`EGraph<DecisionNode>` over 2–5 ranked candidates (`:60`, `with_capacity(ranked.len())`), unions
into one root class, saturates one narrowing rewrite under `BackoffScheduler` (`:73-81`), extracts
the cheapest (`:84-88`), emits `ActiveCostFacts` for the named consumer (`:11`). I3's IR node is
*stubbed but single-valued*: `ir/src/lib.rs` `enum Recognizer`, `SimdMode` (today only `Exact`),
`SimdSite` (today only `PreEntry`); the recognizer vector is *produced* by
`passes::derive_recognizers` and *consumed only as a header comment* today
(`runtime/.../json/generated.rs:10`) — the dead surface I3 activates (GN-2.W1 wiring, **wraps** the
predicate per XL-F).

**Widening + narrowing coexist without a confluence proof** *for correctness* because rewrites are
**unions, never replacements** — saturation keeps both forms, extraction picks the survivor (the egg
insight). Termination is bounded operationally by the node/iter caps already present
(`CspScheduler.node_limit`/`iter_limit`; `egraph_node_limit`/`egraph_iter_limit`); semantics
preservation is a per-rule proof obligation (PROVE-12 discharges the delimiter-mask equivalence).
**But** — the master gate (§0) requires more than "coexist for correctness": it requires the
widening rewrite to actually *manufacture a survivor that changes the pick*, and confluence with the
all-narrowing set is exactly what decides whether `DeduplicateAltBranches` collapses the twin back.
That is a per-rewrite proof 2A owes, not a free consequence of the union discipline.

**Integration seams (path:line, skinny substrate).** Islands host = `crates/egraph/src/{egraph,
extract,scheduler,csp_scheduler,cost_weights}.rs` (`Language`/`Rewrite`/`CostModel`/`Extractor` are
the whole API). I1 = `passes/src/backend_egraph.rs`. Selection layer = `passes/src/decision_csp.rs`
(needs the banked `bounded_sum` + a PBQP-reduction propagator merged into
`csp-solver/src/constraint/`). I3 IR node = `ir/src/lib.rs` (`Recognizer`/`SimdMode`/`SimdSite`),
produced at `passes/src/lib.rs:338` (`derive_recognizers`), lowering home
`codegen/src/lower/recognizer/` (GN-2.W1). Cost tables = `egraph/src/cost_weights.rs`
(`CostWeights`/`CALIBRATED_WEIGHTS`); per-decision facts `ir/src/cost.rs`.

**Gates it carries.** (i) **The island-earns-the-e-graph gate** (can go RED): every island's
rewrites produce nodes *with children* OR its selection is genuinely *constrained* (Island B's
budget) — else write it as a plain cost-ranked choice, do NOT island it. Falsifiable by the
multiplicity census (Brief 1) and the leaf-teardown µs measurement (if the e-graph adds >5% of the
~230 µs budget on a leaf island vs a direct min, demote it). (ii) **Total-order tie-break**: the
last coordinate of every island's cost lattice is a **stable content hash** of the node identity
(the extant `DecisionCost` ends in `candidate_hash: FNV-1a(id)`), making extraction a total min
independent of container iteration → `regen∘regen` byte-identical; the negative control (drop the
hash coordinate) must go non-deterministic. (iii) **Teardown**: `with_capacity(occupancy)` on every
island (the memo-bucket teardown warning is documented in-tree). (iv) GN-2.W1 born-RED: ≥8× cyc/B on
the wired skip site (95% CI excluding 8.0), ≥0.95× dense, ≥3 cold paired PMU windows, ins/B
load-invariant, structural parity before timing. (v) Deletion of the ceremonial ~68-field
provenance apparatus (clause v/vii) once a plan-reading consumer exists.

**Exact open questions.** (1) **[MASTER, Brief 1]** Does I2/I4's tree profile ever face ≥2
survivors at a child-bearing node under a *confluent, terminating, registered* widening rewrite on
a real grammar? If not, I2/I4 are leaf-collapse canonicalizations and the tree profile is dead
code. (2) **[XL-C]** I2 is homed in parse-that/LOCK-5 — does LOCK-5 keep or abrogate the shared
`rules/regex.rs` exemplar? Until LOCK-5 renders the lawful `Hir`, I2 is a WITHHELD cross-lock
contribution. (3) **[XL-D, Brief 2]** Does the fleet present ≥2 *IR-modeled* competing scan sites
(LOCK-3 owes the `SimdMode::Prefilter`/`SimdSite::SelfLoop` node shape; the derivation consumer is
BLOCKED through LOCK-9 9C + LOCK-5) with real binding costs? (4) **[XL-F/1D]** Is I4 a real island
or an always-apply canonicalization double-homed with LOCK-1's `Project`@`passes/src/lib.rs:204`?
By gate (i), `map∘map`/dead-projection that always apply are plain rewrites, not islands. (5) Island
count: honest floor is **N = 1 fully-instantiated in-tree island** (I1, leaf, cost-ranked-suffices)
until the master gate flips; I2 cross-lock, I3 substrate-BLOCKED, I4 probably-not-an-island.

**Verdict.** **ADVANCED — the lock's spine, and the lawful home of 2B's surviving mechanism after
XL-B.** Strongest-aligned with the evidence base and with Cranelift's converged posture. Not
BLOCKED: its gate can genuinely go RED, I1 ships, and Island B stands on CSP+budget with a live
consumer needing no e-class branching. But its *distinctive* extraction value (I2/I4 tree profiles)
rides on the same master gate as 2B, and its first non-I1 instantiation is entirely downstream/
cross-lock work.

---

## Family 2B — IR-IS-EGRAPH

**Architecture (maximal reading — CROSS-LOCK-REFUTED-IN-PART by XL-B).** The middle IR *is* an
e-graph end to end: the whole grammar interns into one `EGraph<BackendExpr, GrammarAnalysis>`;
passes *are* rewrite groups; equality saturation under `CspScheduler`; a single cost-guided
extraction emits per-rule `BackendShape` directly to the lowerer, with `derive_backend_shape`
deleted. **This reading makes extraction the decision driver — precisely the seat XL-B assigns to
LOCK-3.** It is an undeclared supersession of LOCK-3's charter and is not LOCK-2's to instantiate.

**Architecture (lawful residual under XL-B).** The e-graph is the **in-place node substrate at named
decision points within LOCK-3's tower** — acyclic (recursion is a symbolic `Ref`/`CallRule` leaf
resolved by a per-rule root table, so greedy extraction terminates), rules applied largely
on-insert, exploratory saturation only where it provably pays, global cost handed to the one
constraint store. **This residual is operationally the 2A island posture** (Cranelift's own
convergence): the difference that remains is *span* — an in-place e-node substrate persisting across
the tower vs discrete build-teardown islands at chokepoints. That is a real mechanism choice, but it
is a sub-question of 2A's frame, not a rival architecture.

**Mechanism — what is real and what is refuted.** REAL and mostly built: `BackendExpr`
(`ir/src/lib.rs:376`) is already the recursive sum `egraph_derive` projects to `Id`
(`Entry`/`Seq`/`Alt`/`RepeatLoop`/`ValueProject`/`OptionalBranch` + childless leaves); the AC-3
saturation scheduler, the `Lattice`-valued extractor, and a working widening-rewrite *template*
(`NormalizeDirectSinkCost`, the sole in-tree widening precedent, with a one-shot idempotence guard)
all exist. REFUTED: the distinctive thesis. The `CALIBRATED_WEIGHTS` null is a *certified*
counterexample — multiplicity ≈ 1 across the fleet with today's rules, so "IR IS an e-graph
end-to-end" today buys only the dedup a hash-cons already gives. The signature claim "extraction
emits `BackendShape` directly" additionally rests on **context-free per-class shape**, and 2B's
own audit answers the risk: `admits_sink_only` (`passes/src/lib.rs:954`) is a function of the rule
body + the **grammar-global** `TargetFeatures`, and `BackendShape` is stored per-`RuleId` — so
within one grammar there is **no caller-context conflict** (the per-`RuleId` map forbids the
question), and context-free extraction is *sound on the current fleet*. The NP-hard
context-sensitive-DAG-extraction objection is therefore **moot on the fleet as measured** (a
genuine simplification 2B inherits), reactivating only if a future grammar demands two shapes for
one shared sub-rule inside one grammar — whose identity-preserving repair is sub-rule
duplication/specialization (distinct `RuleId`s), bounded and cheap, not the NP-hard primitive.

**The hard architectural boundary (research-B §6, load-bearing).** The greedy bottom-up extractor
"is not optimal for cost models that depend on sharing" (`extract.rs:9-11`). The generated-size
budget is a **global, sharing-sensitive** cost (parser size depends on which sites share a
lowering). Therefore "extraction emits `BackendShape` directly" **cannot honor the budget through
the current extractor.** 2B has exactly two lawful completions, and both dissolve its independence:
(1) replace greedy extraction with ILP/PBQP DAG extraction — *extraction becomes a solver,
converging with LOCK-2's CSP centre*; (2) hand the budget to the one constraint store (the
`decision_csp` seam, shareable with LOCK-1) — *the CSP decides the budget-coupled slice*. Either
way **2B end-to-end is not budget-complete alone**; the strongest honest 2B is "IR-as-e-graph for
canonicalization + candidate generation, with a solver extraction for the budget-coupled final
selection" — which is 2A's island+CSP spine.

**Integration seams.** `egraph-derive/src/lib.rs` (project `Box<Self>`/`Vec<Self>` recursive
positions, or hand-author `Language for BackendExpr`); `egraph/src/analysis.rs` (nullable/first-set
as `Analysis` lattices); `egraph/src/extract.rs` (cyclic guard + the solver-extraction seam);
`passes/src/backend_egraph.rs:11` (the extant named `lower_to_rust` consumer). The `lib.rs:9-10`
doc-claim "grammar IR and regex HIR implement `Language`" is aspirational — the sole `impl Language`
today is `DecisionNode`.

**Gates it carries.** (1) AC operators normal-formed **one-directionally** (flatten `Seq`,
sort/dedup `Alt` by first-set), never exploratory re-associate; confluence a prototype obligation.
(2) e-node/e-class count + iterations bounded under `node_limit` on all live grammars. (3)
`regen∘regen` byte-identical through cost-guided extraction (content-hash tie-break; no `FxHashMap`-
order fold). (4) **Whole-grammar organ (build+saturate+extract+teardown) within ~230 µs** on CSS-L4
(2875 DTA states) — *the single most important open measurement*; the per-rule select builds ≤5
nodes, so no fleet number bounds the end-to-end organ. (5) Extraction crossover: exhibit one grammar
where greedy busts the budget and the solver recovers the legal optimum.

**Exact open questions.** (1) **[MASTER, Brief 1]** Does saturation with widening rules produce ≥2
semantically-distinct survivors *at child-bearing nodes* (`Alt`/`RepeatLoop`/`Seq`), not at costing
leaves? If it stays ≈1, 2B is refuted end-to-end and the lawful reduction is 2A islands — a named
negative result, the substrate deciding. (2) Does the whole-grammar organ hold the compile budget
including teardown? (3) Does extraction flip ≥1 real `BackendShape` vs the cascade on a site where
they disagree — else the e-graph is inert.

**Verdict.** **BLOCKED-candidate, converging into 2A.** The maximal reading is cross-lock-refuted by
XL-B (it seizes LOCK-3's driver seat); the lawful residual is the 2A island posture; the
distinctive value is triple-refuted (multiplicity ≈ 1); and the budget boundary forces it to
terminate in a solver, dissolving its independence into 2A's CSP spine. It is not BLOCKED on a
missing primitive of equal difficulty (the NP-hard objection is moot on the fleet) — it is BLOCKED
on *earning distinct existence over 2A*, which the master gate alone can grant and has not.

---

## Family 2C — CSP-CENTRAL

**Architecture (as-primacy: RETIRED; as-Island-B: LIVE).** The maximal reading — "the decision
driver IS one global constraint-optimization problem over the whole grammar" — is RETIRED (both
pass-1 critics concur): the Unison ceiling (CP proven-optimal only up to ~946–1000 instr/function;
CSS-L4 = 2875 DTA states is off the cliff) forbids the monolith, and the surviving "bounded
post-solve over a small coupled sub-CSP" is *operationally indistinguishable from 2A's Island B*.
What survives and is LIVE: **the scan-plan CSP** — per-site lowering choices as variables, legality
+ the generated-size budget as constraints, a cost lattice as objective, e-graphs demoted to pure
candidate generators. This is the family's real contribution and it is the same seam as 2A-I3.

**Mechanism (the coupled sub-CSP).** Variables: one per lowering decision site (backend shape per
rule; scan/prefilter tier per self-loop site; capacity plan per container; residualization depth per
delimiter scan), domain = the candidates the island's e-class extracts (≤5). Constraints: legality
(the four existing `decision_csp.rs` lambdas become genuine domain filters, not tautologies over a
pinned variable); **coherence** (binary equality/compatibility edges from `SameSubstrateUnionFacts`
where a shared sub-rule under two callers must agree — the "few coherence edges" keeping the graph
near-tree); and **the generated-size budget as a global `bounded_sum` knapsack**
(`Σ static_size_cost ≤ loc_budget`). The `bounded_sum` propagator is the one net-new primitive and
it is **BANKED** (PROTO-2, tested). Objective: `MinimizeCost` via the existing B&B
(`solver/optimize.rs` + `solver/search.rs`), per-value cost fed by `Measurement.
cycles_per_byte_x1000` — replacing the HEAD `perf_cost` hardcoded integer (2D's territory).

**Selection-equivalence with the cascade it replaces** (proven-in-principle, gate-checked): the
priority cascade never optimized the budget (the budget is a post-hoc lint that "blocks the wave"),
so folding it in as a constraint **cannot lose a decision the cascade made** — it reproduces the
per-site optimum exactly in the budget-slack regime (where the constraint is inactive) and
*corrects* the cascade only in the budget-tight regime (where the cascade is blind). A conservative
extension: equal where the cascade was right, better where it was blind.

**Solver shape (Q6 — the OPENED de-crowd question, still SPEC-ONLY).** Strip the knapsack and the
graph is series-parallel / near-tree — the canonical **PBQP** shape (per-node cost vector, per-edge
cost matrix; R0/R1/R2 reductions provably optimal and linear-time on trees/series-parallel graphs,
RN B&B only on irreducible cores; LLVM `RegAllocPBQP` lineage; near-isomorphic to CGO-2018 DNN
primitive selection). Two routes: **Route A** PBQP + Lagrangian dual of the budget (optimal on the
near-tree class, needs a PBQP engine the workspace lacks + a multiplier loop); **Route B** the
existing B&B with the budget as `bounded_sum` (reuses the sole solver, expresses the knapsack
natively, tiny graph N≈10–40 with domains ≤5 → proven optimum in microseconds, well inside ~230 µs).
Research favors **Route B as the home, Route A as the optimality oracle** — solve both, confirm
identical assignment, separate solve-cost. **Nobody has prototyped this**; it is pure citation
today. Brief 2.

**Integration seams.** `passes/src/lib.rs:407` (`derive_backend_shape_with_diagnostics`),
`passes/src/backend_egraph.rs`, `passes/src/decision_csp.rs` (replace the rubber-stamp 1-var CSP);
`csp-solver/src/lib.rs`, `constraint/` (merge the banked `bounded_sum.rs`), `solver/optimize.rs`,
`solver/search.rs`, `domain/cost_finite.rs`, `solver/gac/` (Régin); `ir/src/cost.rs`
(`DecisionCspFacts`, `PriorityStep`, `Measurement`, `SameSubstrateUnionFacts`); budget = `Cargo.toml`
`loc_budget`.

**Gates it carries.** (1) A budget-binding grammar: the CSP forces `Exact` on ≥1
legal-but-unaffordable site under a tight budget AND emits a byte-identical plan under a slack budget
(parity floor). (2) `regen∘regen` byte-identity on the **global** solve (not just per-rule): variable
order by stable `RuleId`, value order by the same `candidate_hash` total order the extractor uses.
(3) Solve cost within budget, reported; PBQP+Lagrangian named as the escape hatch if B&B ever
exceeds. (4) The objective consumes ≥1 real cost fact (2D dependency).

**Exact open questions.** (1) **[XL-D, Brief 2]** Does the fleet present ≥2 *IR-modeled* competing
scan sites with *real binding* costs, so the budget actually binds? Source says yes (css/pretty=15,
css/l4≥10, bbnf=2, csv=2 self-loops); the IR says no (`SimdMode`/`SimdSite` single-valued); the
bridge (the `SelfLoop`/`Prefilter` node shape) is LOCK-3's to build and its derivation consumer is
BLOCKED two locks deep (LOCK-9 9C / LOCK-5). (2) **[Q6, Brief 2]** Is PBQP the right solver shape vs
B&B on the actual near-tree graph — with numbers, not citations? (3) Degenerate without 2D: absent a
fitted cost table the objective is `static_size_cost`, "a CSP-shaped priority cascade with extra
steps." (4) One-solver-home coordination with LOCK-1 1B (XL-G): `bounded_sum` extends the same
csp-solver LOCK-1 centralizes — a GESTALT-B unification point.

**Verdict.** **BANKED (as Island B).** As-primacy RETIRED (Unison); the `bounded_sum` propagator is
a tested banked asset folded into 2A's Island B; the teeth are source-real but IR-latent /
un-budgeted / consumer-BLOCKED two locks deep. Not BLOCKED-candidate on a missing primitive — the
missing primitive (`bounded_sum`) is *built*; it is blocked on a *consumer* (IR-modeled sites) no
single lock currently owns end-to-end.

---

## Family 2D — PROFILE-CLOSED-LOOP

**Architecture.** The cost model is **measured, not authored**: coefficients fitted offline from cold
paired canon-bench PMU captures, committed as a content-addressed **const fact table**, read (never
re-measured) by the decision solver at regen. Three phases — **FIT** (offline `xtask fit-cost`, A/B-
differential, one lever per paired arm — the only sound attribution when whole-grammar cyc/B
underdetermines per-feature coefficients), **COMMIT** (human-gated; write `CALIBRATED_WEIGHTS`, bump
`cost_formula_version`), **FRESHNESS GATE** (CI, born-RED; re-fit and compare CI bands; drift → RED
"re-commit `cost_formula_version`"). **The load-bearing determinism invariant:** the loop closes at
*calibrate-time, not regen-time* — regen is a pure function of `(grammar, committed-β)`, so
`regen∘regen = regen` survives; all load noise lives in `xtask fit-cost`, off the regen path. A
"re-measure at regen" design shatters the fixpoint and is refused. This is exactly the
AutoFDO/BOLT/`-fprofile-sample-use` discipline with a `-Wprofile-instr-out-of-date` staleness gate.

**Mechanism.** ~70% substrate-present. Schema-complete fact table: `ActiveCostFacts` carries
`cost_formula_version`, `selected_cost_freshness`, `determinism_replay_status`,
`rewrite_order_variance_pct`, `selection_trace_hash`; `Measurement` stores
`throughput_mbps_x1000`/`cycles_per_byte_x1000` as **u64 fixed-point** (the fit MUST emit
milli-cyc/B integers — the float-tie determinism fence). The total-order tie-break is already
correct (`candidate_hash` FNV-1a suffix). The env-fence is nearly clean (`from_env` reads only on a
runtime generated-code probe, not the regen path); a CI grep-gate forbidding `from_env` on any
`codegen`/`passes` regen path closes it hard. The residual consumer is proven (PROVE-12: const-fold
at generation time → immediate mask, +18 generated LOC, cyc/B 0.81–0.98×, regen bit-for-bit). The
missing 30%: the fit harness body, the freshness-check body, and — critically — **a live decision
for a coefficient to flip.**

**Integration seams.** `ir/src/cost.rs` (`ActiveCostFacts`, `Measurement`, `EvidenceSource`);
`egraph/src/cost_weights.rs` (`CALIBRATED_WEIGHTS` slot); `passes/src/backend_egraph.rs`
(`perf_cost`, freshness fields today hardcoded); `decision_csp.rs`
(`static_profile_provider_status: "static-template-blocker"` → `"fitted-committed-v1"`); `xtask`
(`fit-cost`/`check-calibration` verbs); the freshness gate in the bench `report.rs`.

**Gates it carries.** (1) A fitted value FLIPS a decision AND the fitted lane beats the hardcoded
lane cold-paired on a real grammar. (2) **ins/B leads, cyc/B is the certified check** (PROVE-02
law: a −48%-instruction kernel inverted CPI 0.13→0.30 and regressed the product lane — an ins/B win
that does not carry to a cyc/B win is *rejected, not committed*). (3) `regen∘regen` byte-identical
across the committed table; `from_env` grep-gate GREEN. (4) **Falsifiable freshness**: mutate a
coefficient → the gate goes RED, `check-calibration` fails (the anti-provenance-theater guard;
today's `"pass"`/`0` stamps are the lie 2D removes). (5) aarch64-only (x86 abrogation → single-host
fit); a flip must survive on a held-out grammar.

**Exact open questions.** (1) **[MASTER, Brief 1 — BINDING]** The multiplicity-1 wall: a fitted
coefficient cannot flip a decision that has no alternatives; on the fleet at HEAD, no coefficient
flips any per-site decision because multiplicity ≈ 1. 2D must **co-land with the first decision that
has ≥2 real survivors** — which Brief 1 either manufactures or refutes. (2) Attribution
underdetermination — resolved structurally by A/B-differential (one lever per paired arm), ceiling =
count of independently-toggleable levers with a paired bench (disclose it; do not regress a
confounded whole). (3) The only *unblocked* plane is cross-lock-owned: `select_backend` kernel
crossover (LOCK-9), `CSS_SHUFTI_MIN_SPAN`/`GATE=16` (GN-2 scanner knobs) — fitting these supersedes
hand-tuned GN literals, a **named supersession (ix)** the spec must declare, not treat as free
attachment.

**Verdict.** **BLOCKED (SPEC-ONLY) — composable, not exclusive.** Sound discipline, correct
precedent, right determinism invariant; zero executed fit. It is a **composable coefficient layer
UNDER whichever generator wins**, not architecturally exclusive with any family. It is
BLOCKED-candidate on the master gate (no ≥2-survivor decision to fit → consumer-less until Brief 1
lands one), and its only unblocked plane touches knobs another lock owns. It must ship ONE executed
fit before it may speak of convergence.

---

## The incompatibility map — which families cannot coexist, and precisely why (pass-2, post-adjudication)

Pass 1 drew a **primary architectural fork {2A, 2B}**, a **primacy conflict {2B, 2C}**, a **scope
conflict {2A, 2C}**, and one **composable layer {2D}**. The cross-lock adjudications have collapsed
two of the three hard forks into sub-questions of a single surviving frame.

### I. 2A ⊗ 2B — DOWNGRADED from HARD fork to mechanism-difference-inside-2A (by XL-B)
Pass 1: mutually exclusive middle-IR architecture (annotated-tree-with-islands vs
e-graph-container-end-to-end). **XL-B forecloses 2B's maximal reading** — extraction cannot be the
decision driver because that seat is LOCK-3's. 2B's lawful residual (acyclic, in-place e-node
substrate, on-insert rewrite, solver extraction for the budget slice) is *operationally the 2A
island posture* — Cranelift's own convergence. What genuinely remains is a **mechanism choice inside
2A's frame**: does the e-graph substrate persist *in place across the tower* (2B-residual) or is it
*rebuilt-and-torn-down at discrete chokepoints* (2A islands)? This is decided by the organ-budget /
teardown measurement (2B gate 4) — an empirical question, not an ownership war. **No longer the
lock's primary fork; it is a tuning of the winning family.**

### II. 2B ⊗ 2C — DISSOLVED (both concede the same seam)
Pass 1: mutually exclusive on decision primacy (extraction-decides vs CSP-decides). Post-adjudication
both routes reach the *same* conclusion: 2B's budget boundary (research-B §6) forces extraction to
terminate in a solver for the sharing-sensitive budget slice; 2C demotes extraction to candidate
generation and makes the CSP decide the coupled slice. **They meet precisely at the `bounded_sum`
knapsack over the near-tree scan-plan graph** — the seam is now singular and BANKED. There is no
live primacy conflict left: extraction canonicalizes/generates locally, the CSP couples globally, in
both readings.

### III. 2A ~ 2C — CONVERGED to identity at Island B (SOFT → resolved)
Pass 1: partial conflict reconcilable at the scan-plan chokepoint. Post-adjudication 2C-as-primacy
is RETIRED (Unison) and its surviving form *is* 2A's Island B — the same variables, the same
`bounded_sum`, the same PBQP/B&B solver question. **Not a conflict; the same route under two names.**
The only residual axis is scope vocabulary (2A "per-island local + thin CSP" vs 2C "one coupled
sub-CSP"), and both agree the sub-CSP is *bounded and small* (peel independent sites to closed form,
solve only the coupled core) — the Unison-ceiling-respecting form.

### IV. 2D — composable with the winner (NOT exclusive), with a prerequisite and a blocked plane
Unchanged from pass 1 and reinforced: 2D is the coefficient plane *underneath* the candidate
generator, orthogonal to *who decides*. Its constraints are dependency, not exclusion: BLOCKED at
the shape lattice (the master gate recurs), unblocked only at threshold/kernel/capacity knobs
another lock owns, and requiring the winning generator to have manufactured ≥2 real survivors first.

### The surviving shared gate (binds all four — unchanged)
Every family's *distinctive* value — 2A's I2/I4 tree profiles, 2B's extraction, 2C's non-degenerate
objective, 2D's lattice attachment — is gated on the **same master fact**: whether a **confluent,
terminating, registered** widening rewrite manufactures ≥2 semantically-distinct survivors at a
child-bearing node on a real fleet grammar, moving a bench at value-starts (Brief 1). The
scan-plan/2C axis carries a **second, orthogonal** gate: whether ≥2 IR-modeled competing sites with
binding costs exist so the budget binds (Brief 2). If Brief 1 fails, the lock collapses to
**canonicalization + the existing BackendShape driver** and every extraction/lattice claim goes
SPEC-ONLY. If Brief 2 fails, 2C/Island-B is refuted-for-corpus and reduces to GN-2.W1's hand-written
detection arm. Both negative results are decision-grade.

**Net pass-2 shape.** One surviving frame — **2A's directed-spine-with-islands**, into which 2B's
lawful mechanism folds and 2C's Island B is the scan-plan seam — with **2D as the composable
coefficient layer beneath it**. The lock is no longer four rival architectures; it is one
architecture with two unresolved empirical gates (the widening/multiplicity gate and the
budget-binding/solver-shape gate) and a set of cross-lock cessions (I2→LOCK-5, I3-node→LOCK-3,
I4→LOCK-1, scan-consumer→LOCK-9) that GESTALT-B must ratify.

---

## Prototype briefs (the two sharpest uncertainties for the next pass)

### Brief 1 — PROVE-2-WIDEN-CONFLUENT (kind: bbnf-worktree)
**Question.** On a real fleet grammar, IN the skinny measurement substrate (XL-A), does a
**registered** widening `Rewrite` — fired through `CspScheduler`, not hand-`union`ed — manufacture
≥2 semantically-distinct survivors at a **child-bearing** node, remain **confluent with the
all-narrowing rule set** (does `DeduplicateAltBranches` collapse the twin back?) and **terminate**,
such that `Extractor` picks *differently* under two CALIBRATED weight vectors AND the two lowerings
move cyc/B at REAL value-start positions? This is the §0 master gate; it de-risks or refutes
2A-I2/I4, 2B-extraction, and 2D-lattice at once. It is the exact step PROTO-1 declared "trivial next
step" and left unbuilt.
**Method.** Skinny worktree, base-pinned (`baseCommit`), HEAD named. Author `ManufactureDispatchAlt`
(disjoint-FIRST-set dispatch-table vs sequential-trial for a JSON/CSS `Alt`) OR self-loop-recognize
(`SimdMode::Prefilter` beside `Exact`) as a real `Rewrite` registered in the scheduler's rule set,
so the `Alt`/scan e-class holds *both* forms. Assert (a) post-saturation the class holds ≥2 distinct
survivors; (b) confluence: re-run saturation with the narrowing set enabled and assert the twin is
NOT deleted; (c) termination: assert fixpoint under `node_limit`/`iter_limit` with margin. Extract
under W1 (favor table) / W2 (favor sequential from `CALIBRATED_WEIGHTS`, not a synthetic sign-flip),
assert extracted nodes differ. Lower each pick; cold-paired PMU (N≥200, ≥3 windows, ins/B
load-invariant, structural parity before timing) at **value-start positions only** (post ws-skip).
**Commands.** `cargo test -p passes widen_confluent`; `cargo bench -p bbnf-bench --bench
css_canon_bench` (CSS-L4, richest); `xtask check-json` twice for `regen∘regen`.
**Pass/fail.** PASS = a *registered* widening rewrite yields ≥2 confluent, terminating survivors at
a child-bearing node AND W1≠W2 extraction AND cyc/B 95% CI excludes 1.00× same-direction at
value-starts. FAIL = twin collapses (confluence fails / R-1 null recurs), OR picks identical, OR CI
includes 1.00× → LOCK-2 collapses to canonicalization + the existing BackendShape driver;
extraction/CSP-extraction recorded SPEC-ONLY. Decision-grade either way.

### Brief 2 — PROVE-2-PBQP-vs-BB (kind: bbnf-worktree)
**Question.** On the near-tree scan-plan selection graph (independent per-site tier choice + a few
pairwise coherence edges + one global size budget), is **PBQP** actually the right solver shape vs
generic **B&B** — with numbers, not citations? Both research reports reach for PBQP; the registry
OPENED it to de-crowd; zero prototype exists. Resolving it settles the 2C/Island-B solver home and
the "one solver, PBQP-as-plug-in-propagator vs B&B-native-knapsack" GESTALT-B coordination point.
**Method.** Skinny worktree, base-pinned. Merge the BANKED `bounded_sum.rs` into
`crates/csp-solver/src/constraint/` (it exists tested in the PROTO-2 worktrees). Construct the
scan-plan graph from the fleet's *actual* site structure (JSON ≤3 real self-loop sites; CSS-L4's
≥10) — one node per site, domain `{Exact, Prefilter}`, a coherence edge where two adjacent sites
share a dispatch table/alphabet, and `Σ static_size_cost ≤ B` as the global budget. Solve TWO ways:
(A) PBQP R0/R1/R2 reductions + Lagrangian dual of the budget; (B) the existing B&B +
`bounded_sum`. The question is *solver behavior*, so PROTO-2's synthetic per-site costs are
admissible here (budget-binding realism is Brief-2-of-registry / XL-D, deferred). Assert both return
the **identical assignment** (optimality confirmation); record solve-µs each, PBQP reduction depth,
and B&B fallback/node count vs graph size.
**Commands.** `cargo test -p csp-solver --test bounded_sum` (6/6 regression floor); `cargo run -p
passes --bin scan_plan_solve -- --grammar css-l4 --solver {pbqp,bb} --budget {tight,slack}`;
`xtask check-json` twice.
**Pass/fail.** PASS = both solvers return byte-identical assignments on every instance (optimality
parity) AND solve-µs is separated with the reduction-depth / fallback-count reported, giving a
defensible "Route B home, Route A oracle" (or the reverse) verdict; `regen∘regen` byte-identical.
FAIL = the two disagree on assignment (an optimality bug in one) OR PBQP shows no reduction win on
the near-tree graph (B&B suffices; PBQP is ceremony) OR the graph is irreducibly dense (both
super-linear; the near-tree premise is false). Decision-grade either way — it either ratifies PBQP
as the near-tree solver or returns B&B-native as the parsimonious home.
