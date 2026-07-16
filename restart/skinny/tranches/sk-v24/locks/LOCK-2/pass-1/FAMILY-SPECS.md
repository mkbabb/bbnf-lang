# LOCK-2 — THE GRAPH ENGINE — pass-1 SYNTHESIS (FAMILY-SPECS)

*Synthesis agent, pass 1. Four families distilled while routes remain incompatible; then the
incompatibility map; then BLOCKED-candidate dispositions; then the two decisive uncertainties.*

Task recap: where and how graph optimization governs the pipeline — HIR canon, IR rewriting,
kernel/backend selection, capacity/schedule decisions; global vs local CSP; profile-fed cost;
deterministic regen. Substrate: `crates/egraph/` (real egg-style engine, ~289 LOC core + derive +
extractor + `CspScheduler`), `crates/csp-solver/` (real, general — `CostFiniteDomain`,
`branch_and_bound`, `kuhn_munkres` LAP), `egraph/src/cost_weights.rs` `CALIBRATED_WEIGHTS`, and the
decision seam (`passes/src/backend_egraph.rs`, `decision_csp.rs`, `ir/src/cost.rs`).

---

## 0. THE CRUX THAT BINDS ALL FOUR FAMILIES (read first)

Every family, and the prior-art sweep independently, converge on one measured in-tree finding that
governs the whole lock:

**The cost model is inert today because no e-class holds more than one semantically-distinct
candidate.** A 54-configuration grid sweep over {structural, alt_per_branch, dispatch_bonus,
ref_cost} produced DTA-state counts *identical to baseline for every grammar* (JSON 51, BBNF 476,
CSS-L4 2875, Sheets 191 — zero variance across all 54; `cost_weights.rs:134-191`). The rewrite set
(`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor`) yields classes with "at most one semantically-distinct canonical form," so the
extractor never chooses. This is corroborated by Cranelift's ægraph production verdict: average
e-class size **1.13** after rewriting, the multi-option layer contributing **~0.1% execution
improvement at ~0.005% compile cost** — "the ægraph itself may not (yet?) be pulling its weight";
the *canonicalization* (GVN/CSE/DCE for free) does the heavy lifting, not extraction search.

Two independent zero-facts make this concrete and non-negotiable:
- `perf_cost: 0` is **hardcoded** at every candidate site (`passes/src/lib.rs:590`, and mirrored at
  `:2023/:2035/:2062`, `decision_csp.rs:247/:259`). The cost lattice has no measured dimension at all.
- The only e-graph on the compile path (`backend_egraph::select`) interns **childless** nodes
  (`children:[Id;0]`), unions all candidates into one class, and extracts a `min_by_key` wearing an
  e-graph costume; the "decision CSP" downstream is a one-variable solve with rigged `{0.0, 1.0}`
  costs that structurally cannot change the answer (`decision_csp.rs:44-83`). Both are
  **consumer-less substrate** in the constitution's sense (viii / Y.13) — the decision cannot go RED.

**Consequence for every family:** a route that claims cost-model-governed selection is
consumer-less substrate *until a rewrite manufactures a genuine e-class branch on the real fleet*.
The single question that de-risks or collapses the entire lock is: **does any real decision exist to
be made on this fleet?** It splits into two orthogonal decision surfaces — *local extraction
diversity* (does a cost-weight flip pick a different form? — the e-graph question, 2A/2B) and
*global coupling* (does the size budget ever bind sites together? — the CSP question, 2C). These are
the two prototype briefs in §6.

---

## 1. FAMILY 2A — EGRAPH-ISLANDS

**Architecture.** A directed pass pipeline (LLVM-shaped, `Changed`-flag convergence) punctuated by
e-graph **islands** at a small named set of decision points. Each island is a *bounded*
equality-saturation run whose extracted candidates feed a *local* CSP that selects one lowering
under legality + the generated-size budget. Between islands the IR is an ordinary annotated tree —
the e-graph is a local tactic at a chokepoint, never the connective tissue. This is deliberately the
**DialEgg shape** (local egglog optimizer invoked at named points in an MLIR pass pipeline), not the
whole-program e-graph.

**Mechanism — three islands, one fixed shell:**
- **Island A · regex HIR canonicalization** (where eqsat genuinely earns it). Alternatives:
  `a|b|c ⇄` character class; common-prefix factoring `abc|abd ⇄ ab(c|d)`; Kleene normal forms
  `(a*)* ⇄ a*`, `a a* ⇄ a+`; epsilon absorption — precisely what `rust-lang/regex`
  `Hir::alternation()` does *imperatively and lossily*; the e-graph keeps all forms until a
  **downstream-automata cost** picks (`w_nfa·NFA-states + w_prefilter·(1−prefilterable) +
  w_class·classes`, weights from `CALIBRATED_WEIGHTS` + PROVE-10 measurements). CSP is thin here
  (extraction is a monotone-local min). LOCK-5 fixes the lawful HIR; 2A owns the rewrite set + cost.
- **Island B · scan-plan selection** (where CSP, not extraction, is load-bearing). Per scan site:
  `SimdMode × SimdSite × kernel × capacity`. e-graph enumerates lawful lowerings (`Repetition` over
  a class with complement cardinality ∈[1,16] admits `Prefilter/SelfLoop` beside default `Exact`;
  complement>16 or empty admits only `Exact`); the **CSP is the real solve** — legality
  (`complement≤16`, `lo6_table_admissible`) *plus* the `generated_loc_budget` **knapsack across
  sites**, objective a cost lattice fed by measured cyc/B. This is the `CostFiniteDomain` +
  `MinimizeCost` path used *for real* (N>1 vars, non-rigged costs, a budget that can force `Exact`
  on a legal-but-unaffordable site).
- **Island C · projection canonicalization** (shares engine with LOCK-1, distinct island):
  `map∘map ⇄ map`, identity/dead-projection elimination (guarded by an `Analysis` carrying the
  projected type — respects no-value-discard), cost = tape-write / materialization-plan size.
  Boundary vs LOCK-1's 1D: 2A's objective is *emitted-code cost*, 1D's is *type-lattice
  representative*.

**Shell:** fixed order `regex HIR canon → projection canon → scan-plan → lower`; extraction
collapses each island before the next pass. No cross-island saturation, no persistent global
e-graph. Insertion at the `passes::compile` spine `derive_*` seams.

**Integration seams:** `passes/src/lib.rs:39` (`derive_recognizers`), `:338` (Island B home), `:398`
(`derive_backend_shape`), `:1149-1287` (Island C cost source); `ir/src/lib.rs:425/430/416`
(`SimdMode`/`SimdSite`/`Recognizer::SimdScan` extension); `egraph/src/cost_weights.rs`,
`cost_config.rs:56-70` (`BBNF_COST_*`, pluggable); `decision_csp.rs:3-5` (real CSP API, used N>1);
consumer `codegen/src/lower/recognizer/mod.rs` (`find_member`).

**Gates it carries:** (i) an island is admitted **only** if its rewrites produce nodes with
*children* (structural sharing → extraction non-trivial) OR its selection is genuinely
*budget-constrained* (Island B knapsack → CSP non-trivial); a flat childless-union island must be
written as a plain `min_by_key`, not dressed as saturation. (ii) born-RED prefilter gate ≥8× skip /
≥0.95× dense on the wired scan site over ≥3 cold paired PMU windows. (iii) `regen∘regen`
byte-identical (deterministic saturation order + total tie-break). (iv) compile-time delta ≤ noise
on the ~230 µs budget (islands bounded: `egraph_node_limit` 100k, `egraph_iter_limit` 64, small by
construction). (v) deletion of the ceremonial `ActiveCostFacts`/`DecisionCspFacts` string apparatus
(grep-gate: zero readers survive).

**Exact open questions:** (Q-2A-1) Does the five-grammar fleet contain a *real* regex leaf (CSS L4
component-value) whose canon measurably changes its prefilter/DFA verdict? If none, Island A is a
consumer-less rewrite set → deferred to LOCK-5 as a named negative result. (Q-2A-2) Does the
`generated_loc_budget` ever bind ≥2 scan sites so Island B's CSP is more than a per-site argmin?
(Q-2A-3) Is the downstream-automata cost *local* enough for the bottom-up extractor (Guided-EqSat
monotonicity), or must non-local prefilter-tier count route to the CSP layer?

---

## 2. FAMILY 2B — IR-IS-EGRAPH

**Architecture.** The middle IR *is* an e-graph, not a tree an island occasionally consults. Every
`BackendRule` body is interned as e-nodes into one shared `EGraph`; every pass is a rewrite group;
scheduled saturation reaches a bounded fixed point; **one** cost-driven extraction emits both the
concrete lowered term *and* its `BackendShape`. One representation of the middle, one canonicalizer
(congruence closure), one decision surface (extraction). Codegen consumes `extract_tree(root)`.

**Mechanism:** introduce `BackendNode` (the e-node isomorph of `BackendExpr`, deriving `Language`:
`Box→Id`, `Vec→Vec<Id>`, metadata→leaf). `BackendIr` becomes `{graph, roots: HashMap<RuleId,Id>,
recognizers}`. Interning at `passes/src/lib.rs:1291` is the mechanical post-order walk `lower_expr`
already does, `graph.add(node)` instead of `Box::new`. **Cross-rule GVN falls out for free** —
identical whitespace-skip loops / `ByteLiteral(b"true")` / span kernels hash-cons to one class
(aegraph "GVN for free during translation into the e-graph"). Passes become `Rewrite`s guarded by an
`EClassFacts` analysis lattice (nullable + first-set, today tree passes at `ir/src/lib.rs:326-338`):
Normalization group (Seq-flatten, single-branch Alt collapse, `Optional(x)≡Alt{Empty,x}`, dead-branch
prune); Residualization group (PROVE-12's CSS delimiter-mask const-fold generalized to *one rewrite*
over any grammar-constant delimiter set); Recognizer-derivation group (GN-2.W1 self-loop→`SimdScan`
folded into the graph); Materialization/tape group (eager-tape / sink-only / collapsed-stage become
*co-present e-nodes in one class*). Extraction then emits the shape as an **attribute of the
extracted form**, collapsing `backend_egraph.rs` + the candidate machinery
(`passes/src/lib.rs:486-609`) into one pass.

**THE structural blocker (make-or-break):** grammar IR is **cyclic** (JSON `value→array→value`). If
`CallRule` is an `Id` edge, the greedy bottom-up extractor **deadlocks** — `compute_best`
(`extract.rs:143-149`) skips nodes whose children aren't resolved; a class reachable only through
itself never resolves; `best_node(value_root)=None`. The aegraph answer, adopted: **`CallRule` is an
opaque leaf** (a `RuleId` metadata field, not a child edge). The e-graph becomes a forest of
*acyclic per-rule-body regions* + a thin rule-call skeleton outside the graph. **The maximalist
slogan "one e-graph, no boundaries" is false;** the lawful 2B is "one e-graph *engine*, one acyclic
region per rule body, a rule-call skeleton" — which is structurally 2A's islands at rule-body
granularity.

**Schedule:** the measured choice between `CspScheduler` fixpoint saturation and aegraph-style
**eager, acyclic, single-pass** application (Cranelift attains the optimization level in a few
linear passes at 1-5% compile cost; the prior-art sweep flags the `CspScheduler` rebuilds a fresh
`Csp` every iteration — the *opposite* of aegraph's no-rebuild lesson, a scaling risk at CSS-L4).

**Integration seams:** `ir/src/lib.rs:376` (`BackendExpr`→`BackendNode`), `:352`
(`BackendIr`→graph), `passes/src/lib.rs:1291` (intern site), `backend_egraph.rs`+`:486-609`
(subsumed), `extract.rs:134` (cyclic-extraction blocker + tie-break determinism), `csp_scheduler.rs:204`
(schedule vs eager), `codegen/src/lib.rs:176-184` (same-wave consumer reading `extract_tree`).

**Gates:** (i) extraction must survive JSON recursion (opaque-leaf `CallRule`); (ii) generated.rs
**byte-identical** to the current template path (the SPEC hard gate); (iii) determinism — stable
`selection_trace_hash` total order, not `FxHashMap` order; (iv) compile budget must not regress the
~230 µs envelope beyond aegraph's 1-5% ceiling (interning every rule body multiplies e-graph
residency; teardown tax at ~300 nodes already noted); (v) the sharing-sensitive / budget decision is
**delegated** to the bounded CSP (its own §4.2 concession — 2B keeps extraction monotone-local, or
extraction goes NP-hard).

**Exact open questions:** (Q-2B-1) On real grammars, is the GVN dedup ratio (`total_nodes` vs summed
tree nodes) material, or negligible (aegraph 1.13)? If negligible the central payoff is absent.
(Q-2B-2) Does canon-as-rewrite + residualization-as-rewrite buy *measurable* generated-size/perf
over the hand-written passes, or only architectural symmetry (aegraph 0.1%)? (Q-2B-3) Given the
opaque-leaf reduction to per-rule regions, what — if anything — distinguishes lawful 2B from 2A's
islands beyond internment granularity?

---

## 3. FAMILY 2C — CSP-CENTRAL

**Architecture.** The decision driver *is* a constraint-satisfaction/optimization problem, inverting
HEAD (where the e-graph selects and the CSP rubber-stamps). One global `Csp<CostFiniteDomain>` over
**all** sites; the per-rule choice is a variable; the size budget is a genuine coupling constraint;
`OptimizationMode::MinimizeCost` branch-and-bound picks the globally cheapest budget-feasible
assignment. The e-graph is **demoted to candidate generator** — it enumerates the per-site
alternative set (`backend_candidates`, `passes/src/lib.rs:508`, already produces ≤5 `BackendCandidate`s
per rule with cost fields); it does **not** select. This is Unison's thesis (combinatorial
instruction-selection + register-allocation + scheduling as one constraint model) applied to
parser-generator lowering.

**Mechanism:**
- **Variables/domains:** one CSP variable per lowering site (rule → shape; scan leaf → tier;
  container → capacity; delim test → residualization depth). Domain = candidate index set;
  `CostFiniteDomain` (`csp-solver/src/domain/cost_finite.rs`) with sorted `(value,cost)` table,
  `-1` abstain sentinel, `Cell`-cached `min_cost()` lower bound B&B issues at every node.
- **Constraints (the couplings greedy extraction cannot honor):** (1) **global generated-size
  budget** `Σ static_size_cost(choice) ≤ B` (`generated_loc_budget`, `Cargo.toml:67`) — a
  knapsack/bin-capacity resource constraint; **greedy per-rule extraction is provably budget-blind**
  (detects overrun only via the post-hoc `lint_loc`, `xtask/src/main.rs:259`); only a global solver
  trades costlier-here for much-cheaper-there. **This is why 2C is not decorative.** (2)
  same-substrate-union all-equal channel (`SameSubstrateUnionFacts`). (3) no-double-emit mutual
  exclusion (`find_member` XOR `dfa.find`, today prose-only, GN-2.W1⇄PT-1.W5). (4) legality unary
  filters (hard-prune illegal indices).
- **Objective:** `MinimizeCost` via `solve_optimized → branch_and_bound` (`solver/search.rs:456`),
  per-candidate cost read from a **committed cost-fact table** keyed `(shape,tier,corpus)→cyc/B`
  (SPEC §0 certified numbers), kept as frozen committed input so regen stays deterministic (the 2D
  boundary — 2C reads the table, never re-measures).
- **The decisive scaling fact + decomposition (the family's real engineering content):** general
  grouped/coupled CSP B&B is proven-optimal only to **n≈15-18**, budget-blows at n≈20 (~1M nodes;
  `builder/assignment.rs` doc). CSS L4 has 2875 states — a monolithic global CSP cannot go through
  B&B. Architecture must **decompose**: independent sites → closed-form `kuhn_munkres` LAP / argmin;
  B&B runs *only* over the small *coupled* sub-CSP (sites the budget or a union actually binds).

**The one new primitive:** `constraint/mod.rs` has no bounded-sum/knapsack constraint. The budget
`Σ ≤ B` *can* be expressed as a `LambdaConstraint` but it **prunes nothing** from a running partial
sum, so B&B degenerates to weak forward-checking and hits the n≈18 cliff faster. 2C requires a
**`SumLeq` propagator with bounds consistency** (prune index `v` from site `i` when
`min_prefix + cost_i(v) + Σ_{j>i} min_cost_j > B`). Small, general, colocated (LOCK-1's
one-constraint-store shares it; standard CP `cumulative`/`bin_packing`). This is the single piece of
new substrate; everything else is rewiring.

**Integration seams:** `passes/src/lib.rs:508` (enumerator stays; populate `perf_cost` from table),
`:504-505` (replace per-rule `select`+`finalize_rule` with one global `finalize_all`), `decision_csp.rs`
(1-var stamp → N-var global optimize), `xtask/src/main.rs:259`+`Cargo.toml:67` (budget becomes CSP
input, `lint_loc` demoted to a post-solve assertion the solver guarantees), consumer
`codegen/src/lower/recognizer/` (one lowering per site by construction).

**Gates:** (i) **prove the budget binds** — exhibit one grammar where per-site argmin exceeds
`generated_loc_budget`, and a concrete assignment (different sites, lower total objective,
budget-feasible) the greedy lexicographic extractor cannot produce; without it 2C is a null result
and must say so (vii). (ii) build `SumLeq` and show it keeps the coupled solve under `node_budget`
(vs the `LambdaConstraint` encoding). (iii) CSP-selected set vs greedy set, cold paired PMU on
canon benches, fed by real fact-table costs. (iv) `regen∘regen` byte-identical — proven-optimal
regime or deterministic total-order tie-break; a `budget_exceeded` best-so-far solve is a build
failure, not a silent best-effort. (v) decomposition scale — coupled sub-CSP stays ≤ B&B-optimal
regime across all four canon grammars.

**Exact open questions:** (Q-2C-1) Does *any* coupling actually bind on the real fleet, or does the
per-site argmin already fit the budget (→ 2C degenerates to independent argmin, decorative — the
exact fate of the `CALIBRATED_WEIGHTS` null sweep)? (Q-2C-2) After peeling independent sites to
closed-form, is the coupled sub-CSP genuinely small on CSS L4, or does it stay large enough to need
a Lagrangian/LP relaxation (a real scope escalation)? (Q-2C-3) Where do non-zero per-candidate costs
come from before 2D exists — `static_size_cost` alone (a live dimension) or does 2C stall on the
same `perf_cost:0` vapor as everyone else?

---

## 4. FAMILY 2D — PROFILE-CLOSED-LOOP

**Architecture.** The cost model is not authored — it is *measured*. Coefficients fitted from cold
paired canon-bench PMU captures, committed as a content-addressed fact table, read (never
re-measured) by the decision solver at regen. The loop — measure → fit → commit → solve →
re-measure-to-validate — is the architecture; the objective ceases to be a hand-tuned float vector
and becomes a regression estimate the corpus produced. **2D owns the *coefficient plane* under
whatever candidate generator (2A/2B/2C) sits above it** — it does not itself generate candidates.

**Mechanism — the four-stage loop:** (1) **MEASURE** — canon benches capture per-process cyc/B via
`proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles` (`json_canon_bench.rs:59`); add an `--emit-facts`
decision-attributed sample stream (per grammar/corpus/site/shape, cold paired, ≥3 windows, loadavg
recorded). (2) **FIT** — offline xtask `calibrate-cost` runs a deterministic closed-form OLS/ridge
(fixed seed) from a structural feature vector `f(rule,shape)` (node count, alt-branch count,
tape-push count, simd-scan-site count — the knobs `CostWeights` already names) onto measured cyc/B;
fit target is **relative ordering / paired ratios**, never absolute cyc/B. (3) **COMMIT** — β written
to `cost-coefficients.json`, hash stamped into `cost_formula_version` (`ir/src/cost.rs:174`; today
literal `"sk-v13-w6-integer-v1"`), with training corpus + held-out residual for audit. (4) **SOLVE
at regen** — regen reads *only* the committed table, evaluates `perf_cost = predict(f(rule,shape),β)`
at `lib.rs:590`; the decision CSP's `CostFiniteDomain::new(values,costs)` gets `predict(...)` instead
of the `{0.0,1.0}` stub. **The loop closes at calibrate-time, not regen-time** — the load-bearing
determinism invariant: regen is a pure function of (grammar, committed-β), so `regen∘regen=regen` is
preserved; any "re-measure at regen" shatters the gate.

**Why fitting, not authoring:** the one hand-authored slot `CALIBRATED_WEIGHTS`
(`cost_weights.rs:175`) carries a documented **null calibration** (54-config sweep → identical DTA
counts). The doc itself says there is "exactly one slot to update when a future calibration produces
a non-trivial result." 2D is that calibration — from *measured cyc/B*, not from DTA state count (a
static proxy), fed into the *perf* dimension the grid sweep never touched.

**Integration seams:** `ir/src/cost.rs:172/203` (add `coefficient_table_hash`, `fit_residual_bps`,
`training_corpus_id`; flip `static_profile_provider_status:"static-template-blocker"` →
`"fitted-coefficient-table-vN"`), `passes/src/lib.rs:590` (`perf_cost:0` → `predict(...)`),
`decision_csp.rs:44-49` (`{0.0,1.0}` stub → real per-candidate cost), `cost_config.rs:83`
(`BBNF_COST_*` env override retained for sweeps), `passes/src/lib.rs:1037`
(`PriorBenchProfile`→`CostProfile{coefficients, per_site_samples}`), `json_canon_bench.rs`
(`--emit-facts`), `xtask/src/main.rs` (new `calibrate-cost`). Precedent for committed profile fact
tables in-tree: `profile/yyjson/*.profile.json`.

**Gates:** (i) **consumer-in-wave** — the committed table lands in the *same* wave as the `perf_cost`
activation and the CSP objective that reads it; a table shipping while `perf_cost` stays 0 is exactly
the dead-substrate pathology (viii/Y.13). (ii) ≥1 rule's selected shape *flips* vs the hand-`shape_rank`
baseline **AND** the flip is a measured cold-paired win on a real grammar **AND** regen byte-identical
on double regen. (iii) load-robustness — β identical under quiet load and loadavg 13-19 (fit on paired
medians/ratios). (iv) cross-grammar held-out residual committed beside β; a flip must survive on a
grammar outside the fit set. (v) refutation laws — cold per-parse only, ratios certified / absolutes
directional, `ri_cycles` is process-granular (→ the A/B-differential primitive is the safe default).

**Exact open questions:** (Q-2D-1 — the family's central refutation) Does a measured table flip
**any** decision, or is it a beautifully fitted table that flips zero decisions (null-calibration
recurrence)? This has a **hard dependency on the candidate generator producing legally-distinct
shapes per site** — 2D alone is the coefficient plane of an empty choice. (Q-2D-2) Attribution:
whole-grammar cyc/B (`json_canon_bench.rs:193`) over ~5 grammars × ~3 corpora is an *underdetermined*
regression for dozens of per-rule features → must the fit use **A/B differential** per decision
(compile both legal shapes, diff whole-parse cyc/B cold-paired — clean, O(candidate) compiles) or
finer shape-labeled sub-parse probes? (Q-2D-3) Is the process-granular attribution ceiling
(no per-BB counters without Instruments) tight enough to fit meaningful per-site coefficients at all?

---

## 5. THE INCOMPATIBILITY MAP

The four families are not four independent knobs; they are competing answers to **who holds the
middle IR** and **who makes the decision**. Precise conflicts:

**INCOMPAT-1 · 2A ⊥ 2B — on the IR representation.** 2A keeps the middle IR an *ordinary annotated
tree* between three e-graph islands ("the e-graph is a local tactic at a chokepoint, never the
connective tissue — that distinction is the whole boundary against 2B"). 2B makes the middle IR *an
e-graph end to end* (every rule body interned). You cannot simultaneously have a tree-with-islands
and an IR-that-is-an-egraph; the two mint incompatible `BackendIr` types. **Convergence caveat, not a
dissolution:** once 2B accepts the mandatory opaque-leaf `CallRule` reduction, its e-graph is a
forest of acyclic per-rule-body regions + a rule-call skeleton — structurally 2A's islands at
*rule-body granularity*. The true axis of disagreement collapses to **internment granularity** (2A:
3 named decision points; 2B: every rule body), **not** the engine. A pass-2 route may pick a point on
this spectrum, but the two pure endpoints cannot coexist.

**INCOMPAT-2 · 2B ⊥ 2C — on the primary decision surface.** 2B: extraction *emits* the decision
(`BackendShape` falls out of `extract_tree`; the shape is an attribute of the extracted form). 2C:
the *CSP* is the decision driver and the e-graph is demoted to a candidate generator that "does not
select." These are opposed claims about who selects. Reconcilable **only** via 2B's own §4.2
concession — delegate budget-/sharing-coupled decisions to the bounded CSP, keep monotone-local
decisions in extraction — but then *neither is "the" driver*; it is a hybrid in which extraction owns
local canonical choice and the CSP owns the coupled global choice. As pure routes (2B: "one decision
surface, extraction"; 2C: "one decision surface, the global CSP") they are mutually exclusive.

**INCOMPAT-3 · 2A ⊥ 2C — on CSP scope.** 2A's CSP is *local, per-island* (Island B: a budget
knapsack over one scan site's alternatives; extraction decides at Islands A/C). 2C's CSP is *one
global coupled solve* over all sites. Either selection is island-local (2A) or global-coupled (2C) —
not both as the architecture. 2A itself frames its degenerate collapse as *converging toward 2C*
("whether the islands are two — A eqsat + B/C CSP — or one — regex canon — with the rest honestly
written as cost-ranked selection"). The families overlap in *mechanism* (both use egraph+CSP) but
conflict on the *scope of coupling*: 2A denies that the budget couples enough sites to need a global
solve; 2C asserts it does. **The measurement that adjudicates this is INCOMPAT-3's crux and is Brief 2
in §6.**

**INCOMPAT-4 · 2D is subordinate, not a peer — compatible with all, standalone-blocked.** 2D is the
coefficient plane *under* any candidate generator; it composes with 2A, 2B, or 2C without
representational conflict. It is therefore not incompatible with any family. But it is **inert without
candidate diversity** (its own §4.1 null-calibration trap = the §0 crux = the prior-art R-1 finding):
a fitted table that flips zero decisions. Its missing primitive — *a rewrite that manufactures a
genuine e-class branch / a site with legally-distinct shapes* — **is the lock's original problem, not
a smaller sub-problem.** 2D cannot stand as a route; it rides whichever of 2A/2B/2C first manufactures
a real branch.

**CONVERGENCE (not a conflict), worth naming for the gestalt.** All four share the `egraph` engine +
`csp-solver` + `CostWeights` single-source cost surface. The **`SumLeq` bounds-consistent knapsack
propagator** is required by *both* 2A (Island B budget) and 2C (global budget) — and by LOCK-1's
one-constraint-store. It is the *single* new primitive the whole lock needs, and it is small and
standard (CP `cumulative`/`bin_packing`). Building it once serves every budget-coupled route. This is
where the families agree, and it should be built regardless of which route wins.

**THE COLLAPSE SCENARIO (binds the whole map).** If the §0 crux resolves negative — no real e-class
branch, no binding budget coupling, aegraph's 0.1% holds for this fleet — then LOCK-2 collapses to
**canonicalization (2B's non-refuted core: GVN + normal-form rewrites, the heavy lifting) + the
existing `BackendShape` driver**, and the extraction-search / global-CSP / profile-fit machinery of
2A-Island-A / 2C / 2D retires to **SPEC-ONLY** as a named negative result. This is a decision-grade
outcome, not a failure. The two briefs in §6 are precisely the measurements that decide between the
collapse and the full route.

---

## 6. BLOCKED-CANDIDATE DISPOSITIONS

Marking per the rule "a family stalled at a missing primitive equal in difficulty to the original
problem is BLOCKED-candidate; elegance earns nothing on its own."

- **2A EGRAPH-ISLANDS — ADVANCE (conditional).** No primitive-block: Island B is a real solve whose
  prerequisites are `SumLeq` (small, standard) + a measurement (does the budget bind ≥2 sites).
  Island A's payoff is measurement-gated (Q-2A-1) and defers cleanly to LOCK-5 if the fleet has no
  factorable regex leaf. Best-aligned with the evidence (directed pipeline, named same-wave consumer,
  GN-2.W1 ready first island). **Conditional on Brief 1/Brief 2 showing a real decision exists.**
- **2B IR-IS-EGRAPH — ADVANCE (maximalist form RETIRED).** The "one e-graph, no boundaries" slogan is
  *refuted* (recursion + aegraph verdict): its lawful residue is acyclic per-rule regions + skeleton,
  and its decision surface (extraction-emits-shape) is inert per §0 and must delegate the budget to
  the CSP (its §4.2). Not primitive-blocked — the transposition is mechanical and the engine exists —
  but its central *payoff* (GVN dedup, canon-as-rewrite beating hand-passes) is measurement-uncertain
  (aegraph 1.13 / 0.1%). Advances **only as a canonicalization layer**; its extraction-as-decision
  claim is subsumed by INCOMPAT-2's hybrid.
- **2C CSP-CENTRAL — ADVANCE (conditional).** The one named missing primitive (`SumLeq`) is *smaller*
  than the original problem — a standard bounds-consistent knapsack propagator, explicitly "the single
  piece of new substrate; everything else is rewiring." Not blocked on primitive grounds. Gated
  instead on the *null-result measurement* (Q-2C-1: does the budget bind?) and the *decomposition
  scale* (Q-2C-2). Strongest prior art (Unison), bounded by the speed trap to a *post-solve over the
  small coupled sub-CSP*, cohered with LOCK-1's CSP scoping.
- **2D PROFILE-CLOSED-LOOP — BLOCKED-candidate.** Its central refutation risk (null-calibration) is a
  **hard dependency on candidate diversity**, and that missing primitive — a rewrite/site that
  manufactures a genuinely distinct legal alternative on the real fleet — **is the lock's original
  problem** (the §0 crux, prior-art R-1's "the one prototype that settles pass-1"). 2D is the
  coefficient plane of an empty choice until 2A/2B/2C supply the branch. Disposition: **BLOCKED on the
  candidate-diversity primitive; not a standalone route; a rider that activates in the same wave the
  first real branch lands.** (Its four-stage loop mechanism is sound and lawful — the block is on the
  *prerequisite*, not the design.)

---

## 7. THE TWO DECISIVE UNCERTAINTIES (prototype briefs) — see StructuredOutput

Brief 1 tests the *local extraction-diversity* surface (the §0 crux; de-risks or collapses
2A-Island-A / 2B / 2D). Brief 2 tests the *global coupling* surface (INCOMPAT-3; adjudicates 2C vs
2A-local, and whether 2B must delegate). They are orthogonal — together they cover both decision
surfaces the lock contains, and either negative is a decision-grade collapse result.
