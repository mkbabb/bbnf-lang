# LOCK-2 · Family 2A — EGRAPH-ISLANDS · pass-1 research

**Family thesis.** Graph optimization governs the pipeline not as one global e-graph but as a
*directed pass pipeline* punctuated by e-graph **islands** at a small, named set of decision
points. Each island is a bounded equality-saturation run whose extracted candidates feed a CSP
that selects one lowering under legality plus the generated-size budget; each island lands in the
same wave as a real consumer that changes emitted code, and each carries a cost model whose
coefficients are measured, not asserted. Between islands the IR is an ordinary annotated tree —
the e-graph is a *local tactic at a chokepoint*, never the connective tissue (that distinction is
the whole boundary against family 2B's IR-is-e-graph).

---

## 0. The load-bearing finding: the extant island is ceremonial, not an optimizer

The seed crates are real and general, but **the only skinny consumer of them decides nothing a
`min_by_key` would not**, and this is the pathology 2A exists to correct.

- `crates/egraph/` (289 LOC core, real hash-cons `memo`, union-find, deferred `rebuild`,
  `CspScheduler` dirty-closure saturation, monotone bottom-up `Extractor`) is a genuine,
  domain-agnostic egg-style engine (`egraph.rs:16-45`, `extract.rs:134-171`).
- Its skinny wiring is `skinny/crates/passes/src/backend_egraph.rs` (backend-shape selection) +
  `decision_csp.rs` (the CSP finalizer). **Both are degenerate:**
  - `backend_egraph::select` (`backend_egraph.rs:60-88`) interns each candidate as a **childless**
    `DecisionNode` (`children: [Id; 0]`, `:158`) and `union`s them all into **one class**
    (`:64-70`). There is no structural sharing, no congruence, no non-trivial saturation — the sole
    rewrite `NormalizeDirectSinkCost` (`:191-237`) only zeroes two cost fields. Extraction
    (`:84-87`) is therefore exactly "the cheapest of N flat alternatives" — a `min_by_key`
    wearing an e-graph costume. The ~30-field `ActiveCostFacts` (`:115-146`) is provenance
    bookkeeping (schema strings, `selection_trace_hash`, `same_wave_consumer_path`), not decision
    content.
  - `decision_csp::solve_rule` (`decision_csp.rs:29-98`) builds a **one-variable** CSP whose costs
    are rigged `0.0` for the already-selected index and `1.0` for all others (`:44-47`); every
    constraint is a trivially-true `LambdaConstraint` that only records a pass/fail *status string*
    (`:52-81`). `csp_status` is always `"sat"`; the solver reselects the input. The 40-field
    `DecisionCspFacts` is a provenance shell (`:137-188`).
- Net: `passes/Cargo.toml:11-12` pulls in `egraph` + `csp-solver`, but the wiring **cannot fail** —
  a consumer that cannot go RED is a consumer-less substrate in the sense of the constitution
  (clause viii, Y.13). This is the exact "substrate-without-consumer" master pathology GN-2 §1.2
  names (14/18 dead). **2A's strongest form is defined negatively by this: an island earns its
  place only where the extracted alternatives are structurally distinct, the cost model is
  measured, and a mis-extraction changes the emitted parser** (so the born-RED gate can fail).

---

## 1. The mechanism made concrete — three islands, one directed shell

The charter names three decision points. Each is specified below as {alternatives, rewrites, cost
model, CSP legality, same-wave consumer, born-RED gate}. The shell that sequences them is a fixed
pass order (§1.4), not a global saturation.

### Island A — regex HIR canonicalization (the one where e-graphs genuinely earn it)

This is egg's canonical use and the island with the most to extract. **LOCK-5 fixes the lawful HIR
form; 2A owns the rewrite set + cost model that canonicalizes it.**

- **Alternatives (real, many-form):** `a|b|c…` over singleton literals ⇄ a single character class;
  common-prefix factoring `abc|abd ⇄ ab(c|d)`; Kleene/repetition normal forms `(a*)* ⇄ a*`,
  `a a* ⇄ a+`; concatenation associativity; empty/epsilon absorption. These are precisely the
  simplifications `rust-lang/regex`'s `Hir::alternation()` performs *imperatively and lossily* at
  build time (it commits to one form); an e-graph keeps all forms until the cost model picks.
- **Cost model (measured, not AST-size):** the extraction objective is *downstream automata /
  prefilter quality*, not node count — class-merged alternations yield `sparse` equal-priority NFA
  states (regex-automata Thompson) and unlock the PROVE-01/PROVE-10 prefilter tier; prefix-factored
  forms shrink the DFA. Concretely: `cost = w_nfa·(NFA state estimate) + w_prefilter·(1 −
  prefilterable) + w_class·(class count)` with weights seeded from `CostWeights`
  (`egraph/src/cost_weights.rs`, `CALIBRATED_WEIGHTS`) and the PROVE-10 `Dfa::find` measurements
  (8.8–29.5× on prefilterable patterns, SPEC §0 row 10).
- **CSP role:** here CSP is *thin* — extraction is a monotone-local minimization the `Extractor`
  already does; CSP appears only if two canonical forms tie and a global constraint (e.g., "at most
  one prefilter tier per grammar" from the budget) breaks the tie.
- **Same-wave consumer:** the automata builder / prefilter derivation that emits `dfa.find`
  (SPEC §2b PT-N1; GN-2.W1's `codegen/src/lower/recognizer/`). A canon form that fails to expose a
  prefilterable class leaves the born-RED prefilter gate (≥8× skip) RED.

### Island B — scan-plan selection (the GN-2.W1 chokepoint, made a real solve)

The prefilter/scan-plan decision is where CSP, not extraction, is load-bearing, because the
alternatives are constrained by legality and compete against a global budget.

- **Variables (per scan site):** `SimdMode ∈ {Exact, Prefilter, …}` × `SimdSite ∈ {PreEntry,
  SelfLoop{terminators}}` (`ir/src/lib.rs:425/430`, GN-2.W1 extends these) × kernel choice
  (`bbnf-simd` `SelectedBackend`, `dispatch.rs:11-16`) × capacity plan.
- **e-graph role (candidate generation):** a *small* island whose rewrites enumerate the lawful
  lowerings of one self-loop scan site — the `Repetition` over a class with complement cardinality
  ∈ [1,16] admits a `Prefilter/SelfLoop` node beside the default `Exact` node (GN-2.W1
  `derive_recognizers`, `passes/src/lib.rs:338`); complement > 16 or empty admits only `Exact`
  (the counterexample GN-2 §7 names). The island *preserves both* so the CSP chooses.
- **CSP legality + objective (the real solve):** variables are per-site lowering choices;
  constraints are legality (`complement ≤ 16`, kernel admissibility `lo6_table_admissible`
  `dispatch.rs:101`) **plus the generated-size budget** (`generated_loc_budget`, GN-2 §5 — a global
  knapsack across sites: each `find_member` lowering costs generated LOC, the budget bounds the
  sum); objective is a cost lattice fed by measured cyc/B (PROVE-01 0.115 cyc/B skip floor,
  PROVE-12 mask-residual −4.8/−6.8% ins/B). This is the `CostFiniteDomain` +
  `OptimizationMode::MinimizeCost` path (`decision_csp.rs:5,88`) used *for real* — multiple
  variables, non-rigged costs, a budget constraint that can force `Exact` on a legal-but-unaffordable
  site.
- **Same-wave consumer:** GN-2.W1's `lower/recognizer/mod.rs` emits `PrefilterSet::find_member`;
  the born-RED gate (≥8× skip, ≥0.95× dense) fails if the CSP mis-selects.

### Island C — projection canonicalization (shared engine with LOCK-1, distinct island)

`->` projection normalization: fusing projection chains (`map∘map ⇄ map`), eliminating identity/
redundant projections, canonicalizing document-shape projections so the tape emitter sees one form.

- **Alternatives:** projection-chain associativity + fusion; `Project` arm normalization
  (`ExprKind::Project` exists at HEAD, GN-3 W-note); dead-projection elimination (respecting the
  no-value-discard law — fusion must be provably value-preserving, so the rewrite is guarded by an
  `Analysis` carrying the projected type, `rewrite.rs:should_apply` `:58`).
- **Cost model:** emitted tape-write count / materialization-plan size (`derive_materialization_plan`,
  `passes/src/lib.rs:1149-1287`) — fewer redundant projections ⇒ fewer tape writes.
- **Consumer:** `codegen`'s tape emitter / `json_typed_direct`. **Boundary with LOCK-1:** 2A owns
  the *rewrite+extraction island as a pipeline pass*; LOCK-1's 1D (types-as-eclasses) owns the
  *type-theoretic* congruence. They share the `egraph` engine; the family split is that 2A's cost
  objective is emitted-code cost, LOCK-1's is type-lattice representative choice.

### The directed shell (what makes this 2A, not 2B)

Islands run in a fixed pass order — **regex HIR canon → projection canon → scan-plan selection →
lower** — each consuming the previous pass's annotated tree and handing forward a *chosen* form
(extraction collapses the island's e-graph before the next pass). No cross-island saturation, no
persistent global e-graph. The pipeline is the `passes::compile` spine (`passes/src/lib.rs:39-48`);
islands are inserted at the three named `derive_*` seams. This is deliberately the DialEgg shape
(local egglog optimizer invoked at named points in an MLIR pass pipeline), not the whole-program
e-graph shape.

---

## 2. Substrate — what exists, what is degenerate, what is missing

| Piece | State | Pointer |
|---|---|---|
| e-graph core (hash-cons, union-find, rebuild) | **Real, reusable** | `crates/egraph/src/egraph.rs`, `unionfind.rs` |
| CSP dirty-closure scheduler | **Real** (AC-3 parent closure) | `egraph/src/csp_scheduler.rs:204-321` |
| monotone bottom-up extractor + `Lattice`/`CostModel` traits | **Real**, multi-objective-ready | `egraph/src/extract.rs:36-215` |
| shared `CostConfig`/`CostWeights` | **Real** (env-tunable, calibrated const) | `egraph/src/cost_config.rs`, `cost_weights.rs` |
| `csp-solver` (`Csp`, `CostFiniteDomain`, `LambdaConstraint`, `solve_optimized`) | **Real, general** | `crates/csp-solver/src/{csp,domain,constraint}/` |
| backend-shape island | **Degenerate** (flat union, min-select, provenance shell) | `passes/src/backend_egraph.rs` |
| decision CSP | **Degenerate** (1-var, rigged costs, status strings) | `passes/src/decision_csp.rs` |
| `ActiveCostFacts`/`DecisionCspFacts` provenance | Real structs, **but populated by the degenerate path** | `ir/src/cost.rs:173-263` |
| Island A (regex HIR canon) | **Missing** — no regex-side egraph consumer in skinny | — |
| Island B (scan-plan CSP over budget) | **Missing** — GN-2.W1 names the seam, no solve yet | `passes/src/lib.rs:338`, `ir/src/lib.rs:425` |
| Island C (projection canon) | **Missing** | `passes/src/lib.rs:1149` |

The engine is *built*; the islands that would make it earn its keep are *unbuilt or fake*. 2A's
deliverable is three real islands replacing one ceremonial one, and **deleting** the ceremonial
provenance-string apparatus (no legacy, precept 6 — the ~70 fact-string fields are the "status
report / vague optimism" the constitution clause vii rejects, rendered as code).

---

## 3. Integration seams (path:line)

- **Pipeline insertion points:** `skinny/crates/passes/src/lib.rs:39` (`derive_recognizers`), `:48`
  (`derive_backend_shape_with_diagnostics`), `:338` (recognizer derivation body — Island B home),
  `:398/407` (`derive_backend_shape`), `:1149-1287` (`derive_materialization_plan` — Island C cost
  source).
- **IR extension for Island B:** `ir/src/lib.rs:425` (`SimdMode`), `:430` (`SimdSite`), `:416-422`
  (`Recognizer::SimdScan`), `:361` (`BackendShape`, 5 variants), `cost.rs:264` (`PriorityStep`),
  `cost.rs:334` (`all_backend_shapes`).
- **Cost coefficients:** `egraph/src/cost_weights.rs` (`CALIBRATED_WEIGHTS`), `cost_config.rs:56-70`
  (defaults + `from_env` `BBNF_COST_*`) — the pluggable cost surface (memory: pluggable-components);
  the `@cost` grammar directive is the future author-facing entry the doc already anticipates
  (`cost_config.rs:29`).
- **CSP surface for Island B:** `decision_csp.rs:3-5` imports (`CostFiniteDomain`, `Ordering`,
  `OptimizationMode`, `Pruning`, `SolveConfig`) — the real API is present; 2A uses it with N>1
  variables and a budget constraint instead of the rigged 1-var form.
- **Same-wave consumers:** GN-2.W1 `codegen/src/lower/recognizer/mod.rs` (`find_member` lowering);
  PROVE-12 `codegen/src/runtime_generator.rs` `CSS_GENERATED_RS` (residual-mask emit — the
  const-fold *is* an extraction result already, un-islanded); `codegen/src/lower/css_scan.rs:*` (712
  LOC STOP-set derivation, an Island-A-adjacent regex-ish canon already hand-written).
- **Regex HIR home (Island A):** LOCK-5 territory — `parse-that-regex/` HIR + `bbnf-regex` facade;
  the engine crate is already shared cross-tier by design (`egraph/src/lib.rs:7-10` states both
  grammar IR and regex HIR implement `Language`).

---

## 4. Prior art with citations

- **egg** (Willsey et al., *Fast and Extensible Equality Saturation*, POPL 2021) — the deferred
  `rebuild` + congruence-closure design the seed crate mirrors; monotone-local cost extraction is
  their bottom-up algorithm (`extract.rs` docstring cites it). https://dl.acm.org/doi/10.1145/3434304
- **egglog** (Zhang et al., PLDI 2023) — Datalog ⋈ eqsat; lattice-based analyses (the `Analysis`
  trait + `should_apply` guard mirror egglog merge functions). https://www.emergentmind.com/topics/egglog
- **DialEgg** (CGO 2025) — *dialect-agnostic MLIR optimizer via egglog*: eqsat invoked at named
  points in a pass pipeline, extraction by cost model; 12 lines egglog vs 120+ C++. **This is the
  direct architectural precedent for 2A's "islands in a directed pipeline"** and the evidence that
  local eqsat beats hand-written passes on maintainability at a chokepoint.
  https://dl.acm.org/doi/10.1145/3696443.3708957
- **Guided Equality Saturation** (Koehler et al., POPL 2024) — sketches steer extraction; formalizes
  the **monotone + local** cost-function requirement 2A's per-island cost models must satisfy for the
  bottom-up extractor to be correct. https://steuwer.info/files/publications/2024/POPL-Guided-Equality-Saturation.pdf
- **rust-lang/regex** — `Hir::alternation()` flattens/factors common prefixes and rewrites
  alternation-of-literals into a `Class` (issue #1001, "merge alternation of Class & Literal into
  Class"); regex-automata Thompson compiler uses `sparse` states for equal-priority non-overlapping
  class ranges. **This is Island A's rewrite set — but done imperatively/lossily there; 2A keeps the
  alternatives in an e-graph and lets the measured cost pick.**
  https://github.com/rust-lang/regex/blob/master/regex-syntax/src/hir/mod.rs · https://github.com/rust-lang/regex/issues/1001
- **rustc / LLVM** — the pipeline-of-passes-with-`Changed`-flag convergence model (memory:
  changed-flag-convergence) is the shell 2A adopts between islands; islands are the eqsat tactic,
  the spine is LLVM-shaped.

---

## 5. Named risks & counterexamples

1. **The degenerate-island trap re-forming.** If an island's alternatives are structurally flat
   (all childless nodes unioned into one class, as `backend_egraph.rs:64-70` does today), the
   e-graph adds nothing over `min_by_key` and the family is theater. **Guard:** an island is
   admitted only if its rewrites produce nodes with *children* (structural sharing) OR its selection
   is genuinely constrained (Island B's budget knapsack) — else it is a plain cost-ranked choice and
   must be *written as one*, not dressed as saturation. Island B's budget is what makes CSP non-trivial;
   Island A's class-merge is what makes extraction non-trivial. If neither holds for a candidate
   decision point, **do not island it.**
2. **No grammar in the fleet exercises Island A.** JSON/CSV/sheets have thin regex leaves; CSS L4 is
   the richest. **Counterexample:** if the five-grammar fleet has no alternation worth factoring,
   Island A is a consumer-less rewrite set. **Guard:** the prototype must find a *real* fleet regex
   leaf (CSS component-value grammar, `grammar/css/l4/*.bbnf`) that measurably improves its
   prefilter/DFA under canon, or Island A is deferred to LOCK-5 with a named negative result.
3. **Monotonicity/locality violation.** A cost model keyed on downstream automata size may be
   *non-local* (a class's cost depends on sharing across the whole regex), breaking the bottom-up
   extractor (Guided-EqSat's requirement). **Guard:** keep per-island cost local (function symbol +
   children costs); route any non-local objective (global prefilter-tier count) into the CSP layer,
   not the extractor.
4. **Determinism under the frozen-regen law.** Saturation order + extraction tie-breaks must be
   deterministic for `cargo xtask regen` bit-for-bit idempotency (SPEC §4, precept 53). The seed
   uses `FxHashMap` (stable, `csp_scheduler.rs:48`) and `stable_hash` tie-breaks
   (`backend_egraph.rs:304`) — 2A must preserve deterministic iteration and a total tie-break in
   every island. **Counterexample:** a `HashMap<_, RandomState>` in a new island silently breaks
   regen determinism — the born-RED regen-diff gate catches it.
5. **Cost coefficients drift from measurement (the 2D critique bleeding in).** 2A's islands consume
   *committed* calibrated weights (`CALIBRATED_WEIGHTS`); if a weight is guessed rather than fitted
   from a canon-bench PMU capture it is "vague optimism" (clause vii). **Guard:** every island cost
   coefficient cites a SPEC §0 row or a canon-bench number; unfitted weights are flagged, not shipped.
6. **Compile-time blowup.** Three islands × saturation per compile risks regressing the ~230 µs
   compile budget (SPEC §0 row 08). **Guard:** islands are bounded (`egraph_node_limit` 100k,
   `egraph_iter_limit` 64, `cost_config.rs:66-67`) and *small* by construction (one scan site / one
   regex leaf, not the whole grammar); the prototype measures compile-time delta as a gate.

---

## 6. What a prototype must test to move this family

A single prototype (`PROVE-2A`) suffices, targeting **Island B** because it has a live consumer
(GN-2.W1) and a falsifiable gate already specified:

1. **Replace the degenerate scan-plan choice with a real N-variable CSP over the budget.** For a
   grammar with ≥2 legal self-loop scan sites, model each site's `{Exact, Prefilter}` as a
   `CostFiniteDomain` variable, add the `generated_loc_budget` knapsack constraint, solve with
   `OptimizationMode::MinimizeCost`, and lower via `find_member`. **Prove the CSP can force `Exact`
   on a legal site** when the budget is tight (the decision the rigged 1-var CSP structurally cannot
   make) — this is the falsification that the engine decides something.
2. **Measure the born-RED gate end-to-end:** ≥8× skip on the wired site, ≥0.95× dense (GN-2.W1
   gate), over ≥3 cold paired PMU windows. A mis-extraction/mis-solve leaves it RED.
3. **Island A feasibility probe (read-only, no build):** enumerate CSS L4 regex leaves; confirm at
   least one alternation/prefix-factor rewrite changes the prefilterability verdict. If none does,
   record Island A as a LOCK-5-owned negative result and drop it from 2A's scope.
4. **Determinism + compile-cost gates:** `regen` bit-for-bit twice; compile-time delta vs the
   degenerate path ≤ noise on the ~230 µs budget.
5. **Deletion proof:** the ceremonial `ActiveCostFacts`/`DecisionCspFacts` string apparatus is
   removed and nothing downstream needs it (grep-gate: zero readers of `resolver_output_piping`,
   `same_wave_consumer_path` beyond the deleted producers) — proving the provenance shell was, per
   clause viii, consumer-less substrate.

**Decision this prototype settles:** whether e-graph *saturation* (Island A's class-merge) earns
its cost on the real fleet, or whether 2A collapses to **CSP-over-a-flat-candidate-set at chokepoints
with the e-graph reserved solely for Island A's regex canon** — i.e., whether the "islands" are two
(A eqsat + B/C CSP) or one (regex canon), with the rest honestly written as cost-ranked selection.
Either answer is a real result; the current ceremonial middle is neither.
