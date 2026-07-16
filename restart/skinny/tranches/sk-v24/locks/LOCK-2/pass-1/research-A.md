# LOCK-2 · Family 2A — EGRAPH-ISLANDS · pass-1 research

**Family thesis.** Graph optimization governs the pipeline not as one global e-graph but as a
*directed pass pipeline* punctuated by e-graph **islands** at a small, named set of decision
points. Each island is a bounded equality-saturation run whose extracted candidates feed a CSP
that selects one lowering under legality plus the generated-size budget; each island lands in the
same wave as a real consumer that changes emitted code, and each carries a cost model whose
coefficients are measured, not asserted. Between islands the IR is an ordinary annotated tree —
the e-graph is a *local tactic at a chokepoint*, never the connective tissue. That distinction is
the whole boundary: 2A keeps the LLVM-shaped pass spine and inserts eqsat/CSP only where the
alternatives are structurally distinct enough to earn it; the middle IR never *becomes* the
e-graph, extraction never emits the backend shape directly, and cost coefficients are *committed
constants* re-fit offline, never a live PMU loop inside the compiler.

---

## 0. The load-bearing finding: the extant island is ceremonial, not an optimizer

The seed crates are real and general, but **the only skinny consumer of them decides nothing a
`min_by_key` would not**, and this is the pathology 2A exists to correct. Verified by reading the
code, not inferred:

- `crates/egraph/` (1885 LOC across 13 modules: hash-cons `memo` + union-find + deferred
  `rebuild` in `egraph.rs`, `CspScheduler`/`BackoffScheduler` saturation in
  `csp_scheduler.rs`/`scheduler.rs`, monotone bottom-up `Extractor` with `Lattice`/`CostModel`
  traits in `extract.rs`, an `Analysis` trait in `analysis.rs`, and the shared cost surface
  `cost_config.rs`/`cost_weights.rs`) is a genuine, domain-agnostic egg-style engine. `lib.rs`
  states both grammar IR and regex HIR are intended to implement its `Language` trait.
- Its skinny wiring is `skinny/crates/passes/src/backend_egraph.rs` (311 LOC, backend-shape
  selection) + `decision_csp.rs` (278 LOC, the CSP finalizer). **Both are degenerate — measured
  against their own source:**
  - `backend_egraph::select` (`backend_egraph.rs:36-149`) interns each backend candidate as a
    **childless** `DecisionNode` (`children: [Id; 0]`, `:158/176`) and `union`s them all into
    **one class** (`:62-70`). There is no structural sharing, no congruence — the graph is a bag.
    It runs over `NoAnalysis` (`:60`). The sole rewrite `NormalizeDirectSinkCost` (`:191-237`)
    matches only `SinkOnly`/`DirectBuildNoConsumer` nodes and merely **zeroes two cost fields**
    (`static_size_cost`, `shape_rank`, `:218-220`). Extraction (`:83-88`) is `Extractor::best_node`
    over `DecisionCostModel::cost = node.score` (`:264-266`) — i.e. exactly "the cheapest of N flat
    alternatives," a `min_by_key` wearing an e-graph costume. The ~28-field `ActiveCostFacts`
    (`:115-146`) is provenance bookkeeping — schema strings, `selection_trace_hash`,
    `same_wave_consumer_path`, `egraph_budget_status`, `determinism_replay_status` — not decision
    content.
  - `decision_csp::solve_rule` (`decision_csp.rs:29-189`) builds a **one-variable** CSP
    (`csp_variable_count: 1`, `:142`) whose domain costs are rigged `0.0` for the already-selected
    index and `1.0` for every other (`:43-49`), then adds a `selected-active-cost-candidate`
    constraint that **forces `value == selected_index`** (`:204-211`). The other four constraints
    are `LambdaConstraint`s that return `true` on any unassigned var and otherwise test a
    trivially-satisfiable candidate predicate (`:214-229`, `:52-81`). `csp_status` is therefore
    always `"sat"` and the solver re-selects its input; `MinimizeCost` over a single already-pinned
    variable is a no-op. The 40-field `DecisionCspFacts` (`:137-188`) is a pure provenance shell
    (`resolver_output_piping`, `fused_solver_status`, `cascade_retirement_status`, …).
  - Both files carry inline `#[cfg(test)] mod tests` (`decision_csp.rs:231-278`) — a standing
    no-inline-tests-law violation (precept 16/54) that 2A's rewrite must relocate to `tests/`.
- Net: `passes/Cargo.toml` pulls in `egraph` + `csp-solver` (outer-repo path-deps, `../crates/`),
  but the wiring **cannot fail** — a consumer that cannot go RED is a consumer-less substrate in
  the constitution's sense (clause viii, Y.13). This is the exact "substrate-without-consumer"
  master pathology GN-2 §1.2 names (14/18 dead). **2A's strongest form is defined negatively by
  this: an island earns its place only where (a) its rewrites produce nodes *with children*
  (real structural sharing → extraction is non-trivial) OR (b) its selection is genuinely
  *constrained* (a global budget → CSP is non-trivial), and a mis-decision changes emitted code**
  so the born-RED gate can fail. Where neither holds, the decision is a plain cost-ranked choice
  and must be *written as one* — not dressed as saturation. The ceremonial provenance-string
  apparatus (~68 fact fields across the two structs) is the "status report / vague optimism" the
  constitution clause vii rejects, rendered as code; **deleting it is part of the deliverable.**

---

## 1. The mechanism made concrete — three islands, one directed shell

The charter names three decision points. Each is specified below as {alternatives, rewrites, cost
model, CSP legality, same-wave consumer, born-RED gate}. The shell that sequences them is a fixed
pass order (§1.4), not a global saturation.

### Island A — regex HIR canonicalization (the one where e-graphs genuinely earn it)

This is egg's canonical use and the island with the most to extract. **LOCK-5 fixes the lawful HIR
form; 2A owns the rewrite set + cost model that canonicalizes it.** The nodes here have children,
so extraction is real congruence-driven work, not bag-ranking.

- **Alternatives (real, many-form):** `a|b|c…` over singleton literals ⇄ a single character
  class; common-prefix factoring `abc|abd ⇄ ab(c|d)`; Kleene/repetition normal forms `(a*)* ⇄ a*`,
  `a a* ⇄ a+`; concatenation associativity; empty/epsilon absorption. These are precisely the
  simplifications `rust-lang/regex`'s `Hir::alternation()` performs *imperatively and lossily* at
  build time (it commits to one form); an e-graph keeps all forms until the cost model picks.
- **Cost model (measured, not AST-size):** the extraction objective is *downstream automata /
  prefilter quality*, not node count — class-merged alternations yield `sparse` equal-priority NFA
  states (regex-automata Thompson) and unlock the PROVE-01/PROVE-10 prefilter tier; prefix-factored
  forms shrink the DFA. Concretely `cost = w_nfa·(NFA-state estimate) + w_prefilter·(1 −
  prefilterable) + w_class·(class count)`, weights seeded from `egraph/src/cost_weights.rs`
  (`CALIBRATED_WEIGHTS`) and PROVE-10's `Dfa::find` measurements (8.8–29.5× on prefilterable
  patterns, SPEC §0 row 10). The objective must be **local** (function symbol + child costs) for
  the bottom-up `Extractor` to be correct (Guided-EqSat requirement, §5.3).
- **CSP role:** *thin* — extraction is a monotone-local minimization the `Extractor` already does;
  CSP appears only if two canonical forms tie and a global constraint (e.g. "at most one prefilter
  tier per grammar" from the budget) breaks the tie.
- **Same-wave consumer:** the automata/prefilter derivation that emits `dfa.find` (SPEC §2b PT-N1;
  the shared `codegen/src/lower/recognizer/` dispatch GN-2.W1 opens, into which PT-1.W5 co-lands a
  `dfa.find` branch for regex-leaf sites). A canon form that fails to expose a prefilterable class
  leaves the born-RED prefilter gate RED.

### Island B — scan-plan selection (the GN-2.W1 chokepoint, made a real solve)

The prefilter/scan-plan decision is where CSP, not extraction, is load-bearing: the alternatives
are constrained by legality and compete against a global budget. **This island subsumes and
generalizes GN-2.W1's hand-written detection arm** — with one scan site it degenerates to that
direct arm; with N competing sites under one budget it becomes a genuine solve.

- **Variables (per scan site):** `SimdMode ∈ {Exact, Prefilter}` × `SimdSite ∈ {PreEntry,
  SelfLoop{terminators}}` (`ir/src/lib.rs:425/430`, the enums GN-2.W1 extends) × kernel choice
  (`bbnf-simd` `SelectedBackend`, `dispatch.rs:11-16`) × capacity plan.
- **e-graph role (candidate generation, small):** rewrites enumerate the *lawful* lowerings of one
  self-loop scan site — a `Repetition`/Kleene over a class/negated-set whose **complement
  cardinality ∈ [1,16]** admits a `Prefilter/SelfLoop{terminators}` node beside the default `Exact`
  node (GN-2.W1's detection predicate, `passes/src/lib.rs:338`); complement empty or > 16 admits
  only `Exact` (the counterexample GN-2 §7 names: a `.*` any-char loop has no terminator set → no
  prefilter). The island *preserves both* so the CSP chooses; it does not commit like the
  imperative pass would.
- **CSP legality + objective (the real solve):** per-site lowering choices are the variables;
  constraints are legality (`complement ≤ 16`; kernel admissibility `lo6_table_admissible`,
  `dispatch.rs:101`) **plus the generated-size budget as a global knapsack** — each `find_member`
  lowering costs generated LOC (GN-2.W1 raises the JSON generated budget to +50; GN-2 §5 enforces
  per-wave `generated_loc_budget`), and the budget bounds the sum across sites, so the CSP **can
  force `Exact` on a legal-but-unaffordable site**. The objective is a cost lattice fed by measured
  cyc/B (PROVE-01 0.115 cyc/B skip floor; PROVE-12 residual-mask −4.8/−6.8% ins/B). This is the
  real `CostFiniteDomain` + `OptimizationMode::MinimizeCost` path (`decision_csp.rs:5,89`) used with
  N>1 variables and non-rigged costs — the decision the rigged 1-var CSP structurally cannot make.
- **Same-wave consumer:** GN-2.W1's `codegen/src/lower/recognizer/mod.rs` emits
  `PrefilterSet::find_member`; the born-RED gate (**≥8.0× cyc/B on the skip site, ≥0.95× dense on
  twitter**, GN-2.W1) fails if the CSP mis-selects — an unwired lowering, a backend mis-selection,
  or a gate-16 mis-tune all go RED. Measured over ≥3 cold paired PMU windows, ins/B as the
  load-invariant check (GN-2 §1.2 precepts 43–49).

### Island C — projection canonicalization (shared engine with LOCK-1, distinct island)

`->` projection normalization: fusing projection chains (`map∘map ⇄ map`), eliminating
identity/redundant projections, canonicalizing document-shape projections so the tape emitter sees
one form. Nodes have children → extraction is real.

- **Alternatives:** projection-chain associativity + fusion; `Project` arm normalization
  (`ExprKind::Project` exists at HEAD, GN-3 confirms; threaded into `infer` + `DirectBuild.type_ref`
  by GN-3.W-note); dead-projection elimination — **guarded by the no-value-discard law** (fusion
  must be provably value-preserving, so the rewrite carries an `Analysis` (`egraph/src/analysis.rs`)
  bearing the projected type and gates via `Rewrite::should_apply`/`search`, `rewrite.rs`).
- **Cost model:** emitted tape-write count / materialization-plan size
  (`derive_materialization_plan`, `passes/src/lib.rs:1149-1287`) — fewer redundant projections ⇒
  fewer tape writes.
- **Consumer:** `codegen`'s tape emitter / `json_typed_direct.rs`. **Boundary with LOCK-1:** 2A
  owns the *rewrite+extraction island as a pipeline pass* with an *emitted-code* cost objective;
  LOCK-1's 1D (types-as-eclasses) owns *type-lattice representative choice* by congruence. They
  share the `egraph` engine crate; the family split is the cost objective, not the substrate.

### The directed shell (what makes this 2A, not the whole-program e-graph)

Islands run in a fixed pass order — **regex HIR canon → projection canon → scan-plan selection →
lower** — each consuming the previous pass's annotated tree and handing forward a *chosen* form
(extraction collapses the island's e-graph before the next pass). No cross-island saturation, no
persistent global e-graph, no e-graph as the IR. The pipeline is the `passes::compile` spine
(`passes/src/lib.rs`); islands are inserted at the three named `derive_*` seams. This is
deliberately the DialEgg shape (local egglog optimizer invoked at named points in an MLIR pass
pipeline), governed by an LLVM-style `Changed`-flag convergence between passes (memory:
changed-flag-convergence), not the whole-program e-graph shape.

---

## 2. Substrate — what exists, what is degenerate, what is missing

| Piece | State | Pointer |
|---|---|---|
| e-graph core (hash-cons, union-find, deferred rebuild) | **Real, reusable** | `crates/egraph/src/egraph.rs`, `unionfind.rs`, `eclass.rs` |
| CSP/backoff schedulers | **Real** (dirty-closure + iteration-capped) | `egraph/src/csp_scheduler.rs`, `scheduler.rs` |
| monotone bottom-up extractor + `Lattice`/`CostModel`/`Analysis` traits | **Real**, multi-objective-ready | `egraph/src/extract.rs`, `analysis.rs`, `rewrite.rs` |
| shared `CostConfig`/`CostWeights` (env-tunable + calibrated const) | **Real** | `egraph/src/cost_config.rs`, `cost_weights.rs` |
| `csp-solver` (`Csp`, `CostFiniteDomain`, `LambdaConstraint`, `solve_optimized`, forward-checking, fail-first) | **Real, general** (vendored csc411) | `crates/csp-solver/src/{csp,domain,constraint,solver,ordering}/` |
| backend-shape island | **Degenerate** (childless nodes, one class, min-select, provenance shell) | `passes/src/backend_egraph.rs:36-149` |
| decision CSP | **Degenerate** (1 var, rigged 0/1 costs, forced-reselect constraint, status strings) | `passes/src/decision_csp.rs:29-189` |
| `ActiveCostFacts`/`DecisionCspFacts` provenance (~68 fields) | Real structs, **populated by the degenerate path** | `ir/src/cost.rs:173/203` |
| Island A (regex HIR canon) | **Missing** — no regex-side egraph consumer in skinny | LOCK-5 territory; `parse-that-regex/`, `bbnf-regex/` |
| Island B (scan-plan CSP over budget) | **Missing** — GN-2.W1 names the seam, no solve yet | `passes/src/lib.rs:338`, `ir/src/lib.rs:425` |
| Island C (projection canon) | **Missing** | `passes/src/lib.rs:1149-1287` |

The engine is *built*; the islands that would make it earn its keep are *unbuilt or fake*. 2A's
deliverable is three real islands replacing one ceremonial one, and **deleting** the ceremonial
provenance apparatus (no legacy, precept 6).

---

## 3. Integration seams (path:line)

- **Pipeline insertion points:** `skinny/crates/passes/src/lib.rs` — `derive_recognizers` body at
  `:338` (Island B home; GN-2.W1's second detection arm), `derive_backend_shape` at `:398`,
  `derive_materialization_plan` at `:1149-1287` (Island C cost source). The `compile` spine
  sequences these.
- **IR extension for Island B:** `ir/src/lib.rs:425` (`SimdMode`, today only `Exact` → add
  `Prefilter`), `:430` (`SimdSite`, today only `PreEntry` → add `SelfLoop{terminators:
  StructuralAlphabet}`), `:416-422` (`Recognizer::SimdScan`), `:361` (`BackendShape`, 5 variants),
  `cost.rs:264` (`PriorityStep`), `cost.rs:334` (`all_backend_shapes`). Emitted node is stamped but
  not lowered today: `runtime/src/grammars/json/generated.rs:10`
  (`// recognizer: SimdScan Exact PreEntry alphabet=…`).
- **Cost coefficients (the pluggable surface):** `egraph/src/cost_weights.rs`
  (`CALIBRATED_WEIGHTS`), `cost_config.rs` (defaults + `from_env` `BBNF_COST_*`) — memory:
  pluggable-components; the `@cost` grammar directive is the anticipated author-facing entry.
- **CSP surface for Island B:** `decision_csp.rs:2-8` already imports the real API
  (`CostFiniteDomain`, `LambdaConstraint`, `Csp`, `OptimizationMode`, `Pruning`, `SolveConfig`,
  `Ordering`); 2A uses it with N>1 variables + a budget constraint instead of the rigged 1-var
  form. Note the csc411 W1 sync churn (`SolveConfig` field drift, `decision_csp.rs:91-96`) — the
  vendor gate is live.
- **Same-wave consumers:** GN-2.W1 `codegen/src/lower/recognizer/mod.rs` (`find_member` lowering,
  NEW directory-module); PROVE-12 `codegen/src/runtime_generator.rs` `CSS_GENERATED_RS`
  (const-fold delimiter mask — an extraction result already, un-islanded, disasm-verified);
  `codegen/src/lower/css_scan.rs` (712-LOC STOP-set derivation, an Island-A-adjacent regex-ish
  canon already hand-written).
- **Regex HIR home (Island A):** LOCK-5 territory — `parse-that-regex/` HIR + `bbnf-regex` facade;
  the `egraph` crate is already shared cross-tier by design (`egraph/src/lib.rs` intends both
  grammar IR and regex HIR to implement `Language`).

---

## 4. Prior art with citations

- **egg** (Willsey et al., *Fast and Extensible Equality Saturation*, POPL 2021) — deferred
  `rebuild` + congruence closure the seed crate mirrors; the bottom-up monotone extractor is their
  algorithm. https://dl.acm.org/doi/10.1145/3434304
- **egglog** (Zhang et al., PLDI 2023) — Datalog ⋈ eqsat; lattice-based analyses (the `Analysis` +
  `should_apply` guard mirror egglog merge functions). https://dl.acm.org/doi/10.1145/3591239
- **DialEgg** (Alur et al., CGO 2025) — *dialect-agnostic MLIR optimizer via egglog*: eqsat invoked
  at named points in a pass pipeline, extraction by cost model; ~12 lines egglog vs 120+ C++.
  **The direct architectural precedent for 2A's "islands in a directed pipeline"** and the evidence
  that local eqsat beats hand-written passes on maintainability at a chokepoint.
  https://dl.acm.org/doi/10.1145/3696443.3708957
- **Guided Equality Saturation** (Koehler et al., POPL 2024) — sketches steer extraction; formalizes
  the **monotone + local** cost-function requirement each per-island cost model must satisfy for the
  bottom-up extractor to be correct.
  https://steuwer.info/files/publications/2024/POPL-Guided-Equality-Saturation.pdf
- **rust-lang/regex** — `Hir::alternation()` flattens/factors and rewrites alternation-of-literals
  into a `Class` (issue #1001); regex-automata Thompson uses `sparse` states for equal-priority
  non-overlapping class ranges. **This is Island A's rewrite set — done imperatively/lossily there;
  2A keeps the alternatives in an e-graph and lets the measured cost pick.**
  https://github.com/rust-lang/regex/issues/1001
- **rustc / LLVM** — the pass-pipeline-with-`Changed`-flag convergence (memory:
  changed-flag-convergence) is the shell 2A adopts between islands; islands are the eqsat tactic,
  the spine is LLVM-shaped.
- **MiniZinc / OR-tools knapsack framing** — Island B's generated-LOC budget across N scan sites is
  a 0/1 knapsack with legality side-constraints; `csp-solver`'s forward-checking + fail-first
  (`decision_csp.rs:88-90`) is the exact solver posture such a bounded problem wants.

---

## 5. Named risks & counterexamples

1. **The degenerate-island trap re-forming.** If an island's alternatives are structurally flat
   (all childless nodes unioned into one class, as `backend_egraph.rs:62-70` does today), the
   e-graph adds nothing over `min_by_key` and the family is theater. **Guard:** admit an island only
   if its rewrites produce nodes *with children* (Island A/C) OR its selection is genuinely
   constrained (Island B's budget knapsack) — else write it as a plain cost-ranked choice. If
   neither holds for a candidate decision point, **do not island it.**
2. **No grammar in the fleet exercises Island A.** JSON/CSV/sheets have thin regex leaves; CSS L4 is
   the richest. **Counterexample:** if the five-grammar fleet has no alternation worth factoring,
   Island A is a consumer-less rewrite set. **Guard:** the prototype must find a *real* fleet regex
   leaf (`grammar/css/l4/*.bbnf` component-value grammar) whose prefilter/DFA measurably improves
   under canon, or Island A is deferred to LOCK-5 with a named negative result.
3. **Island B collapses to one site.** If the fleet never presents ≥2 *competing* self-loop scan
   sites under budget pressure, the CSP is a no-op and Island B is GN-2.W1's hand-written detection
   arm with extra ceremony. **Counterexample:** JSON's sole self-loop is whitespace-skip
   (complement = 6 structurals + quote, ≤ 16 → detected) — one site, no contest. **Guard:** the
   prototype must construct a grammar (a real `.bbnf`, not a synthetic buffer) with ≥2 legal sites
   and a budget that binds, and *prove the CSP forces `Exact` on one*; if no fleet grammar ever
   does this, Island B honestly collapses to the direct arm + extraction for Island A only.
4. **Monotonicity/locality violation.** A cost keyed on downstream automata size may be *non-local*
   (a class's cost depends on whole-regex sharing), breaking the bottom-up extractor
   (Guided-EqSat). **Guard:** keep per-island cost local; route any non-local objective (global
   prefilter-tier count) into the CSP layer, not the extractor.
5. **Determinism under the frozen-regen law.** Saturation order + extraction tie-breaks must be
   deterministic for `cargo xtask regen` bit-for-bit idempotency (SPEC §4, precept 53). The seed
   uses stable FxHash iteration and a `stable_hash` tie-break (`backend_egraph.rs:304`,
   `candidate_hash` in the cost lattice `:174`) — every new island must preserve deterministic
   iteration and a *total* tie-break. **Counterexample:** a `HashMap<_, RandomState>` in a new
   island silently breaks regen; the born-RED regen-diff gate catches it.
6. **Cost coefficients drift from measurement (the sibling profile-loop critique bleeding in).** 2A
   consumes *committed* calibrated weights (`CALIBRATED_WEIGHTS`); a guessed weight is "vague
   optimism" (clause vii). **Guard:** every island coefficient cites a SPEC §0 row or a canon-bench
   number; unfitted weights are flagged, not shipped. (2A's boundary: weights are re-fit *offline*
   and committed — the compiler never runs a PMU feedback loop.)
7. **Compile-time blowup.** Three islands × saturation per compile risks regressing the ~230 µs
   compile budget (SPEC §0 row 08). **Guard:** islands are bounded (`egraph_node_limit`,
   `egraph_iter_limit` in `cost_config.rs`; the `budget_status` gate at `backend_egraph.rs:100-108`
   already exists) and *small* by construction (one scan site / one regex leaf, not the whole
   grammar); the prototype measures compile-time delta as a gate.

---

## 6. What a prototype must test to move this family

A single prototype (`PROVE-2A`) suffices, targeting **Island B** because it has a live consumer
(GN-2.W1) and a falsifiable gate already specified, and it doubles as the deletion proof:

1. **Replace the degenerate scan-plan choice with a real N-variable CSP over the budget.** For a
   grammar with ≥2 legal self-loop scan sites, model each site's `{Exact, Prefilter}` as a
   `CostFiniteDomain` variable, add the `generated_loc_budget` knapsack constraint, solve with
   `OptimizationMode::MinimizeCost` + forward-checking/fail-first, and lower via `find_member`.
   **Prove the CSP can force `Exact` on a legal site** when the budget is tight — the decision the
   rigged 1-var CSP structurally cannot make. This is the falsification that the engine decides
   something.
2. **Measure the GN-2.W1 born-RED gate end-to-end:** ≥8.0× cyc/B on the wired skip site (95% CI
   excluding 8.0 same-direction), ≥0.95× dense on twitter, over ≥3 cold paired PMU windows with
   ins/B as the load-invariant check; parity-assert identical structural positions before timing.
   A mis-extraction/mis-solve leaves it RED.
3. **Island A feasibility probe (read-only, no build):** enumerate CSS L4 regex leaves; confirm at
   least one alternation/prefix-factor rewrite changes the prefilterability verdict. If none does,
   record Island A as a LOCK-5-owned negative result and drop it from 2A's scope.
4. **Determinism + compile-cost gates:** `regen` bit-for-bit twice; compile-time delta vs the
   degenerate path ≤ noise on the ~230 µs budget.
5. **Deletion proof:** remove the ceremonial `ActiveCostFacts`/`DecisionCspFacts` string apparatus
   (~68 fields) and the inline `#[cfg(test)]` blocks; grep-gate zero readers of
   `resolver_output_piping`, `same_wave_consumer_path`, `selection_trace_hash` beyond the deleted
   producers — proving the provenance shell was, per clause viii, consumer-less substrate.

**Decision this prototype settles:** whether e-graph *saturation* (Island A's child-bearing
class-merge) and *CSP-over-a-budget* (Island B) each earn their cost on the real fleet, or whether
2A honestly collapses to **CSP-over-a-flat-candidate-set at one chokepoint with the e-graph reserved
solely for regex canon** — i.e. whether the "islands" are three, two, or one, with the remainder
written as plain cost-ranked selection. Every one of those is a real result; the current ceremonial
middle — an e-graph that decides nothing and a CSP that re-selects its input — is neither.
