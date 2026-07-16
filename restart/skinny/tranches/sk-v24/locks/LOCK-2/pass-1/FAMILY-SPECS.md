# LOCK-2 — THE GRAPH ENGINE — pass-1 FAMILY SPECS (synthesis)

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity &
schedule decisions, profile-fed cost, deterministic regen. Four families — 2A EGRAPH-ISLANDS ·
2B IR-IS-EGRAPH · 2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP — distilled while routes remain
incompatible, then the incompatibility map. Synthesis over research-{A,B,C,D,priorart}, pass 1.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at
runtime (v); concrete artifacts or SPEC-ONLY (vii); refutation laws — no warm benches, ratios
certified / absolutes directional (iv); deterministic regen (`regen∘regen = regen` byte-identical);
extant GN/PT codex is the baseline (ix).

---

## 0. The convergent finding that governs the whole lock — THE INERT DECISION SUBSTRATE

All five reports independently land on one fact, and it is the pass's most load-bearing result:
**the extant e-graph/CSP substrate is ceremonial. It decides nothing a `min_by_key` would not, and
its cost model has no branch to exercise.** Two independent measurements say so:

- **In-tree (R-1, the null calibration).** A 54-config grid sweep over
  {structural, alt_per_branch, dispatch_bonus, ref_cost} on the 4-grammar corpus produced a DTA
  state count **identical to baseline for every grammar** (JSON 51, BBNF 476, CSS-L4 2875, Sheets
  191 — zero variance across all 54). Root cause: the current rewrite set collapses each e-class to
  **at most one semantically-distinct canonical form**, so extraction is inert
  (`egraph/src/cost_weights.rs:134-191`).
- **External twin (Cranelift ægraph, EGRAPHS 2023 / blog 2026-04-09).** After rewriting, **average
  e-class size = 1.13 e-nodes**; the multi-representation layer was exercised **2 times across ~4M
  value nodes**; the whole aegraph pipeline cost ~7-8% more compile time for ~2% runtime, and the
  *canonicalization* (GVN/CSE/LICM "for free"), not extraction search, did the heavy lifting.

The current wiring embodies the pathology exactly: `passes::backend_egraph::select`
(`passes/src/backend_egraph.rs:36-149`) interns each backend candidate as a **childless**
`DecisionNode` and unions them into **one class** (a bag, not a congruence), then extracts the
cheapest — a `min_by_key` in an e-graph costume. `passes::decision_csp::solve_rule`
(`decision_csp.rs:29-189`) builds a **1-variable CSP with rigged 0/1 costs** and a constraint that
**forces `value == selected_index`** — it re-selects its input. The perf dimension is
**hardwired to zero** (`perf_cost: 0`, `passes/src/lib.rs:590`). ~68 provenance-string fields across
`ActiveCostFacts`/`DecisionCspFacts` are "status report / vague optimism" rendered as code (clause
vii). A consumer that cannot go RED is consumer-less substrate (clause viii / Y.13).

**Therefore the single fact that gates every family is:** *does any rewrite manufacture a genuine
e-class alternative (>1 semantically-distinct candidate) on a real fleet grammar, such that the
Extractor picks differently under two weight vectors AND the two picks produce a measured cyc/B
delta?* Until that is shown, richer extraction (ILP, Pareto, treewidth, differentiable) solves a
problem the fleet has not been shown to have, and the honest LOCK-2 collapses to
**canonicalization + the existing BackendShape driver**, with the extraction/CSP machinery
SPEC-ONLY. This is Brief 1. It de-risks or refutes 2A-Islands-A/C, 2B-extraction, and 2D-lattice
simultaneously.

**The second, orthogonal gate** is the CSP axis, which does *not* need e-class branching: *does a
fleet grammar present a binding generated-size budget over ≥2 competing scan sites, such that a
real N-variable CSP forces `Exact` on a legal-but-unaffordable site* — the one decision the rigged
1-var CSP structurally cannot make? This is Brief 2. It settles 2C's teeth and 2A's Island B
independently of extraction.

---

## Family 2A — EGRAPH-ISLANDS

**Architecture.** An LLVM-shaped **directed pass pipeline** punctuated by e-graph **islands** at a
small, named set of decision points. The middle IR stays an ordinary annotated tree between
islands; the e-graph is a *local tactic at a chokepoint*, never the connective tissue. Each island
is a bounded eqsat run whose extracted candidates feed a CSP that selects one lowering under
legality + the generated-size budget; each island lands same-wave with a real consumer that changes
emitted code, carrying a measured cost model. Precedent: DialEgg (CGO 2025) — egglog invoked at
named points in an MLIR pass pipeline. Between islands: LLVM-style `Changed`-flag convergence.

**Mechanism (three candidate islands, one directed shell).**
- **Island A — regex HIR canonicalization** (egg's canonical use; the island with real
  child-bearing extraction). Alternatives: `a|b|c ⇄ class`, prefix factoring `abc|abd ⇄ ab(c|d)`,
  Kleene normal forms `(a*)* ⇄ a*`, `a a* ⇄ a+`. Cost = downstream automata/prefilter quality
  (`w_nfa·NFA-states + w_prefilter·(1−prefilterable) + w_class·classes`), **local** for the
  bottom-up extractor to be correct (Guided-EqSat monotone-local requirement). CSP thin (tie-break
  only). Consumer: automata/prefilter derivation emitting `dfa.find`. **LOCK-5 owns the lawful HIR
  form; 2A owns the rewrite set + cost model.**
- **Island B — scan-plan selection** (where CSP, not extraction, is load-bearing). Variables per
  scan site: `SimdMode∈{Exact,Prefilter}` × `SimdSite∈{PreEntry,SelfLoop{terminators}}` × kernel ×
  capacity. e-graph enumerates the *lawful* lowerings of a self-loop over a class whose complement
  cardinality ∈[1,16]; CSP legality (`complement≤16`, kernel admissibility) **plus the generated-LOC
  budget as a global knapsack across sites** — the CSP can force `Exact` on a legal-but-unaffordable
  site. Consumer: GN-2.W1's `codegen/src/lower/recognizer/mod.rs` `find_member`.
- **Island C — projection canonicalization** (shared engine with LOCK-1, distinct island). Fuse
  `map∘map ⇄ map`, eliminate identity/dead projections (guarded by no-value-discard via an
  `Analysis` bearing the projected type). Cost = tape-write / materialization-plan size. Consumer:
  the tape emitter (`json_typed_direct.rs`).

The shell runs a fixed order — regex-canon → projection-canon → scan-plan → lower — extraction
collapsing each island before the next pass. No cross-island saturation, no persistent global
e-graph.

**Integration seams.** `passes/src/lib.rs:338` (`derive_recognizers`, Island B home / GN-2.W1
detection arm), `:398` (`derive_backend_shape`), `:1149-1287` (`derive_materialization_plan`,
Island C cost source); IR extension `ir/src/lib.rs:425/430` (`SimdMode`/`SimdSite`),
`:416-422`/`:361` (`Recognizer::SimdScan`/`BackendShape`); cost surface
`egraph/src/cost_weights.rs`, `cost_config.rs` (the `@cost` directive is the author entry); CSP API
already imported at `decision_csp.rs:2-8`; Island A home = LOCK-5 territory.

**Gates it carries.** (i) Every island's rewrites produce nodes *with children* OR its selection is
genuinely *constrained* (Island B's budget) — else write it as a plain cost-ranked choice, do NOT
island it. (ii) GN-2.W1 born-RED: ≥8.0× cyc/B on the wired skip site (95% CI excluding 8.0),
≥0.95× dense on twitter, ≥3 cold paired PMU windows, ins/B load-invariant, structural parity
asserted before timing. (iii) `regen∘regen = regen` byte-identical with a *total* tie-break.
(iv) compile-time delta ≤ noise on the ~230µs budget. (v) Deletion of the ceremonial ~68-field
provenance apparatus + relocation of inline `#[cfg(test)]` (no-inline-tests law).

**Exact open questions.** (1) Does the 5-grammar fleet have a *real* regex leaf (CSS-L4 richest)
whose prefilter/DFA verdict changes under canon? If none, Island A is a LOCK-5-owned negative
result (share of the master gate, §0). (2) Does the fleet ever present ≥2 *competing* self-loop
sites under a binding budget, or does it collapse to one (JSON's whitespace-skip is the sole
self-loop, no contest)? (Brief 2.) (3) Are the islands three, two, or one — with the remainder
written as plain cost-ranked selection?

**Verdict.** ADVANCED-candidate; **strongest-aligned with the evidence base**. Not BLOCKED: Island
B stands on CSP+budget with a live consumer and needs no e-class branching; the design explicitly
permits honest collapse (3→2→1). Islands A/C share the §0 master gate, prototype-answerable — not a
missing primitive equal in difficulty to the original problem.

---

## Family 2B — IR-IS-EGRAPH

**Architecture.** The middle IR *is* an e-graph end to end. `GrammarIr` interns once into a single
`EGraph<IrNode, IrFacts>`; every pass (nullability, first-set, recognizer derivation, backend-shape
selection, projection canon, residualization) is a rewrite group or an `Analysis` lattice; congruence
closure gives grammar-level GVN for free; a scheduled saturation reaches a fixed point; a single
cost-guided extraction emits the `BackendExpr` tree *and* per-rule `BackendShape` directly to the
lowerer. Passes stop being an ordered pipeline (phase-ordering dissolves) and become an unordered
rewrite set closed under equality.

**The honest strongest form is NOT unbounded saturation** — that is refuted on contact by the IR's
own shape (variadic `Seq`/`Alt` → AC blowup; mutually-recursive rules → cyclic; non-local size
budget). Its defensible form is the **Cranelift aegraph discipline**: acyclic (recursion is a
symbolic leaf), rules applied largely on-insert, exploratory saturation only where it provably
pays, global cost delegated to the CSP knapsack.

**Mechanism.** A distinct `IrNode` whose recursive positions are `Id` (today `ExprKind` names
children as arena `ExprId`, `ir/src/lib.rs:19/234`; `BackendExpr` uses owned `Box`/`Vec`,
`:376` — neither interns; the derive classifies `Box<Self>`/`Vec<Self>` as *leaves*,
`egraph-derive/src/lib.rs:7-9`). **`Ref{target:RuleId}` is a LEAF, not an `Id` child** — the single
most important structural decision: it makes each rule *body* an acyclic e-graph, recursion resolved
by a per-rule root table (`RuleId → Id`), so greedy extraction terminates (a cyclic `Id` edge would
make `best_cost` return `None`, `extract.rs:175`). `Discard`'s two `ExprId`s become `[Id;2]` (derive
forces it). Interning = one post-order walk; hash-consing (`egraph.rs:155`) collapses structurally
identical sub-grammars → GVN with zero extra code. Analyses (`nullable` OR-join, `first_set`
union-join, `CostSummary`) replace the hand-rolled nullability worklist (`ir/src/lib.rs:322-348`).
Rewrites carry the equalities (`Optional(x)≡Repeat{x,0,1}`, single-branch `Alt`≡branch,
`MembershipScan(set)≡MaskScan(delim_mask(set))` — PROVE-12 as a rewrite, not a template mutation).
Scheduler = `CspScheduler` verbatim. Extraction = the lowering; monotone-local slice greedy,
**non-monotone global size budget handed to `csp-solver` knapsack** (the seam 2B and 2C touch).

**Integration seams.** `egraph-derive/src/lib.rs` (extend to project recursive positions, or author
`IrNode` by hand); `egraph/src/analysis.rs` (host the fact lattices); `egraph/src/extract.rs`
(non-monotone/global extraction + cyclic guard); `passes/src/backend_egraph.rs:11` (the extant
named `lower_to_rust` consumer). The `lib.rs:9-10` doc-claim "grammar IR and regex HIR implement
`Language`" is aspirational — the sole `impl Language` today is `DecisionNode`; 2B is the work of
making it true.

**Gates it carries.** (1) AC operators normal-formed **one-directionally** (flatten `Seq`, sort/dedup
`Alt` by first-set), never given exploratory re-associate rewrites; confluence is a prototype
obligation. (2) e-node/e-class count + saturation iterations bounded under `CspScheduler`'s
`node_limit` with margin on all live grammars — any grammar exceeding the node budget refutes the
full reading and forces the on-insert retreat. (3) `regen∘regen = regen` byte-identical: the
extractor's `FxHashMap best` and any cost tie resolve by a *total* order (class id → canonical node
order); `HashMap` iteration is not an admissible tie-break. (4) Full-IR-as-e-graph compile (build +
saturate + extract + **teardown**) within the ~230µs budget — the `with_capacity` note warns
teardown can dominate on a ~300-node grammar; report the teardown fraction. (5) Extraction crossover:
exhibit one grammar where per-node greedy busts the size budget and the CSP knapsack recovers the
legal optimum.

**Exact open questions.** (1) Does the full-IR e-graph stay within the compile-time budget including
teardown? (2) Does regen stay bit-for-bit through cost-guided extraction? (3) **Does extraction buy
anything at all** — the aegraph external verdict (1.13 avg class, 2-of-4M, ~0.1% runtime) says the
win is canonicalization, NOT extraction; if the §0 master gate does not flip, 2B's *distinctive*
claim (the IR *becomes* the e-graph; extraction emits BackendShape) reduces to "GVN/hash-consing for
free," which 2A/2C also inherit from the shared engine.

**Verdict.** VALUE-AT-RISK, not strictly BLOCKED. The maximal reading is refuted by the IR shape;
the aegraph reading is coherent and ~80% substrate-built, but its residual distinctive value is
gated on §0 (prototype-answerable) and the aegraph twin predicts it is ~0.1%. If Brief 1 shows
extraction inert, 2B collapses to canonicalization and its "IR-becomes-e-graph" thesis buys the
compile-time cost of an oversized graph for the value 2A gets locally.

---

## Family 2C — CSP-CENTRAL

**Architecture.** The decision driver **IS** a single constraint-optimization problem (COP) over the
whole grammar, solved once at compile. Variables = per-site lowering choices; constraints = legality
+ the generated-size budget; objective = a cost lattice fed by PMU facts. **E-graphs are demoted
from decider to candidate generator** — they populate variable domains and canonicalize/prune
candidates; the `Extractor::best_node` leaves the decision path. Load-bearing claim: **only a global
COP can treat the generated-size budget as a HARD constraint at decision time** — trading a
size-cheap shape at rule A to afford a fast-but-fat kernel at rule B, which per-rule/greedy selection
can only lint after the fact.

**Mechanism.** One `Csp<CostFiniteDomain>` whose variable set is the union of every decision site
across all rules. Variables: backend shape (5 `BackendShape`, one per rule), recognizer/kernel (one
per scan site, `SimdMode×SimdSite`), capacity plan (`CapacityPolicy`, per container),
residualization depth (PROVE-12 depth-0/1). Constraints: legality (`LambdaConstraint`; the P1-P8
`PriorityStep` cascade RETIRED into legality/soft-preference constraints per its existing
`evidence-only` disposition); cross-site coherence (`SameSubstrateUnionFacts` → `Implication` /
`AllDifferentExcept`); **the generated-size budget** `Σ static_size_cost(chosen) ≤
generated_loc_budget` (`skinny/Cargo.toml:67 = 4000`) — today a POST-HOC `xtask lint-loc`, 2C makes
it a solve-time bounded-sum knapsack coupling every site, so the budget turns GREEN by construction.
Objective: `OptimizationMode::MinimizeCost` via `solve_optimized` over `branch_and_bound` with
forward-checking / fail-first / `node_budget`; per-value cost = `Measurement.cycles_per_byte_x1000`
(`EvidenceSource::BenchProbe`), lexicographic tie preserved as scalarized objective.

**Substrate.** The csp-solver is a full COP engine (AC-3 + forward-checking + fail-first +
`node_budget` + restarts/cancel; `CostFiniteDomain` with cached `min_cost` B&B lower bound;
`LambdaConstraint`/`AllDifferent(Except)`/`Implication`/`NotEqual`). **The one net-new primitive: a
bounded/linear SUM (knapsack) propagator** with a lower-bound relaxation (Σ min-cost residuals) at
`crates/csp-solver/src/constraint/bounded_sum.rs` — homed in csp-solver, not bbnf-lang
(general-infra edict). Everything else is rewiring.

**Gates it carries.** (1) A budget-binding grammar: exhibit a grammar where the budget binds and the
global plan **strictly dominates** the greedy per-rule plan (legal + cheaper/feasible), AND a
BYTE-IDENTICAL plan when the budget is slack (parity floor). (2) `regen∘regen = regen`: total-order
tie-break on `candidate_hash`, integer-quantized costs, `restarts=false` (already pinned). (3) Solve
cost: the global COP over CSS-L4 stays within `node_budget` and bounded `csp_solve_us` (reported).
(4) Closed-loop seam: the objective consumes ≥1 PMU fact (stub table); flipping a cyc/B coefficient
must CHANGE the plan.

**Exact open questions.** (1) **Does the budget ever bind on the fleet?** If provably slack
everywhere, the COP decomposes to independent per-site minima (parity with today minus the
rubber-stamp) and 2C is refuted-for-corpus (Brief 2). (2) The Unison ceiling: CSS-L4 has 2875 DTA
states — **above** Unison's proven-optimal ~946-1000 instr/function ceiling (seconds-minutes/function
for 1.1-10% speedup) — so a monolithic global 2C is off the cliff by construction; it is lawful only
as a **bounded post-solve over a small coupled sub-CSP** after independent sites are peeled to
closed-form (coheres with LOCK-1's "bounded post-solve where unification is undecidable"). (3) The
PMU cost table is 2D's territory; without it the objective degenerates to `static_size_cost` (a
CSP-shaped priority cascade with extra steps) — a named dependency on 2D.

**Verdict.** BLOCKED-candidate pending Brief 2. The family's entire teeth is the binding budget; the
counterexample (JSON's one whitespace self-loop, no contest) makes the binding condition an *open
empirical question*, and if it fails, 2C's distinctive value does not exist on the corpus — the
missing primitive (a binding cross-site budget) may be absent from the fleet. Additionally dependent
on 2D for a non-degenerate objective, and structurally constrained to a bounded post-solve (not the
monolithic global driver its maximal reading proposes) by the Unison ceiling.

---

## Family 2D — PROFILE-CLOSED-LOOP

**Architecture.** The cost model is **measured, not authored**. Coefficients fitted offline from cold
paired canon-bench PMU captures, committed as a content-addressed fact table, and read (never
re-measured) by the decision solver at regen. The loop — measure → fit → commit → solve →
re-measure-to-validate — is the architecture. 2D owns the **coefficient plane UNDER** whatever
candidate generator (2A islands, 2B saturation, 2C CSP) sits above it; it answers "where do the
numbers in the cost lattice come from, and why are they trustworthy and deterministic." Precedent:
AutoFDO / PGO / BOLT (committed profile, reproducible build), AutoTVM/Ansor/Halide (fitted cost
model over measured runtimes), regex-automata `rare.rs` (a small fitted byte-frequency table beating
a live heuristic — 2D at micro-scale, SOTA-idiomatic for this engine class).

**Mechanism (the loop is BUILT but OPEN; every organ exists wired to a stub).**
1. **MEASURE.** Canon benches already capture per-process PMU cyc/B via
   `proc_pid_rusage(...).ri_cycles` (`json_canon_bench.rs:59`), paired, N≥200, ins/B load-invariant.
   2D adds a `--emit-facts` decision-attributed writer: per (grammar, corpus, site, knob) a
   cold-paired cyc/B + ins/B sample.
2. **FIT.** Offline `xtask calibrate` runs a *deterministic* closed-form OLS/ridge (fixed seed) from
   a structural feature vector onto measured cyc/B → coefficient vector β. Fit target = **relative
   ordering / paired ratios**, never absolute cyc/B (fitting absolutes fits load noise).
3. **COMMIT.** β → content-addressed table (`profile/coefficients.ron`; precedent
   `profile/yyjson/*.profile.json`), hash stamped into `cost_formula_version`, keyed by
   `(host_triple, grammar, site_class, knob_id)`, carrying training-corpus id + held-out residual.
4. **SOLVE (at regen, deterministic).** Regen reads *only* the committed table — never live PMU —
   and evaluates `perf_cost = predict(f, β)` to fill the dead dimension (`lib.rs:590`), feeding the
   CSP's `CostFiniteDomain` real per-value costs instead of the `{0.0, 1.0}` stub.

**The load-bearing determinism invariant:** the loop closes at *calibrate-time, not regen-time*.
Regen is a pure function of `(grammar, committed-β)`, so `regen∘regen = regen` is preserved; all load
noise lives in `xtask calibrate`, off the regen path. A "re-measure at regen" design shatters the
fixpoint and is refused. This is precisely the PGO/AutoFDO/BOLT discipline.

**Where the loop attaches (the null-trap escape).** The grid-sweep null (§0) proves fitting
*shape-lattice* weights moves nothing. 2D attaches to knobs with demonstrated measured spread:
(1) the `select_backend` kernel crossover (`bbnf-simd/src/dispatch.rs:89` — the unwired "N*≈3"
`build_lo6_table` amortization point); (2) `CSS_SHUFTI_MIN_SPAN` (GN-2.W3 — "a threshold tuned
against a measured constraint," erase animate's +13.4% while retaining tailwind 4.16×); (3) the
GATE=16 prefilter width; (4) `CapacityPolicy` thresholds.

**Integration seams.** `ir/src/cost.rs:172` (`ActiveCostFacts` + new `coefficient_table_hash` /
`fit_residual_bps` / `training_corpus_id`), `:293/305/318` (the empty `Measurement` evidence vec 2D
fills), `passes/src/lib.rs:590` (`perf_cost`), `decision_csp.rs:44-49` (the `{0.0,1.0}` stub →
`predict`), `csp-solver/src/solver/optimize.rs:12` (`DomainCostEval::cost` reads β),
`egraph/src/cost_config.rs:83` (add `from_fact_table`, keep `from_env`),
`bbnf-bench/src/report.rs:1742-1758` (the freshness GATE that already reads
`cost_formula_version`/`selected_cost_freshness`/`determinism_replay_status` — give it teeth via
`cost_formula_version = hash(table)`), `xtask` (`calibrate` / `check-calibration` verbs).

**Gates it carries.** (1) A fitted value FLIPS a decision AND the fitted-value lane beats the
hardcoded-value lane cold-paired on a real grammar. (2) β identical under quiet load and loadavg
13-19 (fit on paired medians/ratios). (3) `regen∘regen` byte-identical across the committed table.
(4) **Falsifiable freshness**: mutate one coefficient → the `report.rs:1750` gate goes RED,
`check-calibration` fails (provenance-theater guard — the current `"pass"`/`0` stamps are the lie
2D removes). (5) `host_triple`-keyed, aarch64-only (x86 abrogation simplifies — single-host fit);
objective is a *multi-corpus constraint* (retain all wins), and a flip must survive on a held-out
grammar outside the fit set.

**Exact open questions.** (1) Attribution underdetermination — the bench emits *whole-grammar*
cyc/B, not per-rule; fitting per-decision β from whole-parse cyc/B over ~15 equations with dozens
of features is unidentifiable. The prototype must choose (a) **A/B differential** per decision
(compile both legal knob values, diff whole-parse cyc/B cold-paired — the honest, load-robust,
O(candidate)-compiles primitive) or (b) shape-labeled sub-parse probes (finer, more harness).
(2) Does any knob's fitted optimum *differ* from its current value with a measured win?

**Verdict.** **BLOCKED-candidate at the backend-shape lattice** — the missing primitive is a
branching rewrite (the §0 master gate, equal in difficulty to the original problem), so a 2D fitting
the degenerate lattice reproduces the null and is refuted. **UNBLOCKED at the threshold/kernel/capacity
plane**, where spread is demonstrated and a fitted value flips a real decision. 2D is a **dependent
composable layer** under whichever of 2A/2B/2C wins — not architecturally exclusive with any — with
the candidate-generator's real per-site alternatives as its exact prerequisite.

---

## The incompatibility map — which families cannot coexist, and precisely why

The four families divide into a **primary architectural fork {2A, 2B}**, a **primacy conflict
{2B, 2C}**, a **scope conflict {2A, 2C}**, and one **composable layer {2D}**.

### I. 2A ⊗ 2B — mutually exclusive middle-IR architecture (HARD)
2A keeps an LLVM-shaped directed pass spine with the middle IR as an **annotated tree**, e-graphs
inserted only as **local island tactics** at named chokepoints; the IR never *becomes* the e-graph
and extraction never emits the backend shape directly. 2B makes the middle IR **one e-graph
container end-to-end**; passes *are* rewrite groups over it and extraction *is* the lowering. The
IR cannot simultaneously be "a tree between islands" and "the e-graph container." They share the
`egraph` engine crate but are incompatible at the level of what the middle IR *is*. **This is the
lock's primary fork.**

### II. 2B ⊗ 2C — mutually exclusive on decision primacy (HARD)
2B has **extraction emit `BackendShape` directly** (greedy bottom-up for the monotone-local slice,
CSP knapsack only for the global size slice). 2C **removes the extractor from the decision path**
(`best_node` folds into the CSP objective) and makes **one global COP the driver**, with e-graphs
demoted to candidate generators. Both cannot be primary: either extraction decides and the CSP
patches the global slice (2B), or the CSP decides and extraction only generates/prunes candidates
(2C). They touch *precisely* at the knapsack — the seam where 2B hands its non-monotone global cost
to the same `csp-solver` bounded-sum 2C makes central — which is why a hybrid is conceivable but
their *primacy* is not co-instantiable.

### III. 2A ~ 2C — partial conflict, reconcilable only at one chokepoint (SOFT)
They **converge** at the scan-plan chokepoint: 2A's Island B *is* a CSP-over-budget, and 2C's
"e-graphs as candidate generators feeding a CSP" is exactly 2A's island shape. They **diverge on
scope**: 2A's CSP is **per-island / local and thin** (extraction still decides at the regex-canon
and projection-canon islands), whereas 2C is **one global cross-site COP per grammar** with the
budget as a hard whole-grammar knapsack and *no* extractor in any decision. A synthesis is possible
— "2A's directed spine with Island B promoted to a global-budget COP" — but only by conceding 2C's
whole-grammar scope OR 2A's per-island locality; they cannot both hold verbatim.

### IV. 2D — composable with all three (NOT exclusive), with a prerequisite and a blocked plane
2D is the **coefficient plane underneath** the candidate generator, orthogonal to the {2A,2B,2C}
choice of *who decides*. It composes with any of them. It is **not incompatible** with any family.
Its constraints are dependency, not exclusion: it is BLOCKED-candidate at the backend-shape lattice
(the §0 null recurs) and only unblocked at threshold/kernel/capacity knobs, and it requires the
chosen generator to have manufactured real per-site alternatives before it can move the shape
decision.

### The shared upstream gate (binds all four)
Every family — 2A's Islands A/C, 2B's extraction, 2C's non-degenerate objective, 2D's lattice
attachment — is gated on the **same §0 fact**: whether a rewrite manufactures a genuine e-class
alternative on the real fleet. 2C and 2A's Island B additionally (and independently) hinge on the
**budget-binding fact**. These two facts are the two orthogonal load-bearing uncertainties, resolved
by Brief 1 (extraction axis) and Brief 2 (CSP axis). If Brief 1 fails, the lock collapses to
canonicalization + the existing BackendShape driver (2A's degenerate-collapse form), and 2B/2D's
lattice claims go SPEC-ONLY. If Brief 2 fails, 2C is refuted-for-corpus and 2A's Island B is
GN-2.W1's hand-written detection arm. Both negative results are decision-grade.

---

## Prototype briefs (the two sharpest uncertainties)

### Brief 1 — `PROVE-2-BRANCH`: does a rewrite manufacture a real e-class alternative that moves a bench? (kind: bbnf-worktree)
**Question.** On a real fleet grammar, does *any* rewrite create an e-class holding >1
semantically-distinct candidate such that the `Extractor` picks **differently** under two weight
vectors, AND the two picks produce a **measured cyc/B delta**? This is the §0 master gate; it
de-risks 2A-Islands-A/C, 2B-extraction, and 2D-lattice at once, or refutes all three.
**Method.** In a skinny worktree, author the canonical branching rewrite the tree already prices but
does not exercise: **dispatch-table vs sequential-trial for a JSON/CSS `Alt`** (the two forms
`egraph/src/cost_weights.rs` prices via `dispatch_branch` / `dispatch_table` / `alt_per_branch`).
Add the rewrite so the `Alt` e-class holds *both* forms (do NOT collapse to one canonical form).
Run `Extractor::best_node` under weight-vector W1 (favor table) and W2 (favor sequential); assert
the extracted node differs. Lower each pick and cold-paired-bench (N≥200, ≥3 windows, ins/B
load-invariant, structural parity asserted before timing).
**Commands.** `cargo run -p bootstrap --bin cost_grid_sweep` (or equivalent regen) to extract under
each weight vector; `cargo bench -p bbnf-bench --bench json_canon_bench` / `css_canon_bench`.
**Pass/fail.** PASS = the two weight vectors extract *different* nodes on a real grammar AND the two
lowerings differ by a cyc/B ratio whose 95% CI excludes 1.00× same-direction. FAIL = the e-class
collapses to one canonical form (R-1 null recurs) OR picks identical OR the cyc/B CI includes 1.00×
— LOCK-2 then collapses to canonicalization + the existing BackendShape driver, extraction/CSP
machinery recorded SPEC-ONLY. Decision-grade either way.

### Brief 2 — `PROVE-2-BUDGET`: does the generated-size budget ever bind over ≥2 competing scan sites? (kind: bbnf-worktree)
**Question.** Does a fleet grammar present ≥2 *competing* self-loop scan sites under a binding
`generated_loc_budget`, such that a real N-variable CSP forces `Exact` on a legal-but-unaffordable
site — the decision the rigged 1-var CSP structurally cannot make? Settles 2C's teeth and 2A's
Island B independently of extraction.
**Method.** (Step 0, cheap falsifier first) audit the 5 fleet grammars for self-loop scan sites
(`SimdSite::SelfLoop`, complement cardinality ∈[1,16]); if <2 competing legal sites exist anywhere,
2C is refuted-for-corpus — record and stop. (Step 1) add the net-new **`bounded_sum` knapsack
propagator** to `crates/csp-solver/src/constraint/bounded_sum.rs` (lower-bound = Σ min-cost
residuals). (Step 2) build ONE global `Csp<CostFiniteDomain>` over a grammar with ≥2 legal sites
(construct a real `.bbnf` if the fleet lacks one), each site's `{Exact, Prefilter}` a variable, with
`Σ static_size_cost ≤ generated_loc_budget` as a live sum constraint; solve with
`OptimizationMode::MinimizeCost` + forward-checking / fail-first.
**Commands.** `cargo test -p csp-solver bounded_sum`; a small skinny bin building the global CSP over
the target `.bbnf`, printing the plan under a tight vs slack budget; `cargo xtask regen` twice.
**Pass/fail.** PASS = under a tight budget the CSP forces `Exact` on ≥1 legal site (budget is a HARD
solve-time constraint) AND under a slack budget the plan is byte-identical to the per-rule pick
(parity floor) AND `regen∘regen` is byte-identical. FAIL = the fleet audit finds <2 competing sites,
or no budget value both binds and changes the plan — 2C refuted-for-corpus, 2A's Island B collapses
to GN-2.W1's hand-written detection arm. Decision-grade either way.
