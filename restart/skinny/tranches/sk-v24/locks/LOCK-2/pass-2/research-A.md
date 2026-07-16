# LOCK-2 · Family 2A — EGRAPH-ISLANDS (pass-2 research-A)

*A directed pass pipeline with e-graph islands at named decision points; every island a
same-wave consumer and a cost model; CSP selects over extracted candidates. Developed to its
strongest measured form on the living substrate.*

---

## 0. The thesis, corrected by evidence

The family's premise is plural islands: `regex HIR canon`, `scan-plan selection`, `projection
canon`, each a small e-graph with its own cost model and its own consumer. The substrate already
runs ONE island end-to-end — the per-rule backend-shape selection at
`skinny/crates/passes/src/backend_egraph.rs` — and the general-purpose e-graph it sits on
(`crates/egraph/`) is domain-agnostic and mature (intern → saturate → extract, `Language`,
`Analysis`, `CostModel`, `BackoffScheduler`, `CspScheduler`). So the family is not a green-field
invention; it is a **generalization of one working island into a small fleet of them, under a
parsimony law that forbids the consumer-less ones.**

The single hardest fact this research surfaced governs everything below:
`crates/egraph/src/cost_weights.rs:134-167` records a **measured null calibration**. A 54-config
grid sweep across `{structural, alt_per_branch, dispatch_bonus, ref_cost}` on the four-grammar
corpus (JSON 51 states, BBNF 476, CSS L4 2875, Sheets 191) produced **identical DTA state counts
in every cell** — because "the current e-graph rewrite set (`DeduplicateAltBranches`,
`SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`) produces
e-classes with at most one semantically-distinct canonical form for these grammars, so the cost
model has no choice to exercise." **Saturated multiplicity ≈ 1 across the fleet, at the grammar
tier, measured.** This is the answer to directive Q1 and the axis on which 2A must be honestly
built: islands earn their e-graph only where a *child-bearing* rewrite manufactures ≥2 survivors;
elsewhere the island is a cost-ranked canonicalizer (deterministic argmin) and the e-graph is
SPEC-ONLY until such a rewrite is proven.

---

## 1. The mechanism made concrete

### 1.1 Two island kinds, not one

The extant island (`backend_egraph::select`) reveals the taxonomy the family must own:

- **Flat selection islands** (backend-shape today; scan-plan/kernel-tier tomorrow). Candidates
  are **childless** e-nodes (`DecisionNode { children: [Id; 0] }`, `backend_egraph.rs:158`), all
  unioned into ONE root class (`backend_egraph.rs:62-71`). "Extraction" is a deterministic argmin
  over the class on a scalar-ish lattice (`DecisionCost`, a 6-field lexicographic tuple,
  `backend_egraph.rs:239-247`). Multiplicity = number of non-hard-pruned candidates, which the
  priority cascade routinely collapses to 1 (`backend_candidates` sets `hard_pruned: !ranked`,
  `passes/src/lib.rs:588`; exactly one `PriorityStep` fires per rule, `lib.rs:535-575`). **On a
  flat island the e-graph is machinery without a decision** — an argmin with a total-order
  tie-break is the whole mechanism.
- **Structural canon islands** (regex HIR canon; projection/`->` canon). Nodes are
  **child-bearing** (`Concat`/`Alt`/`Star` in regex HIR; `Seq`/`Map`/`Project` in projections).
  Here congruence closure does real sharing, saturation explores genuinely distinct rewrites, and
  extraction faces a real choice AT child-bearing nodes (branch factoring vs. dispatch-eligible
  form). This is the only place the e-graph's core value — representing many equivalent forms
  compactly and deferring the cost decision — is actually exercised.

The strongest form of 2A is therefore **heterogeneous islands under one substrate**: keep the
`crates/egraph` engine as the shared home, but instantiate an e-graph *only* at structural islands
with a demonstrated child-bearing rewrite; instantiate flat islands as a `CostModel`-ranked
canonicalizer that shares the same `CostWeights`/`Lattice` vocabulary but skips saturation.

### 1.2 The extraction total order (determinism, Q3)

Extraction is already deterministic and regen-safe, and the substrate proves it — this is a
correction to the directive's worry that "HashMap iteration is inadmissible":

- The e-graph stores classes as a **`Vec<EClass>`** (`crates/egraph/src/egraph.rs:29`), iterated
  in insertion order by `classes()` (`:138`); `memo: FxHashMap` (`:32`) is a hash-cons lookup
  never iterated for output; `pending: Vec<Id>` (`:34`) is LIFO-popped in `rebuild` (`:251-253`).
  Iteration order is insertion order — deterministic.
- `Extractor::compute_best` (`extract.rs:134-171`) improves on strict `<`, so a tie never
  displaces the incumbent; the incumbent is whichever node the deterministic class walk saw first.
- `DecisionCost` closes the residual tie with a `candidate_hash` tail (FNV-1a of the candidate id,
  `backend_egraph.rs:246`, `:304-311`) so the lexicographic order `(freshness, perf, capacity,
  static_size, shape_rank, candidate_hash)` is a **total order** — the mechanism the directive
  asks for. **Residual risk:** the tail is a 64-bit hash, not the id itself; two distinct ids that
  collide would reintroduce iteration-order dependence. The parsimonious fix is to tie-break on the
  full id string (already unique by construction, `lib.rs:581-584`), making the order
  collision-free by definition. That is a one-line hardening a prototype must lock in.

### 1.3 The CSP is a witness today; PBQP is the mechanism when sites couple (Q6, Q7)

`decision_csp::solve_rule` (`skinny/crates/passes/src/decision_csp.rs:29-189`) is **not a
selector** — it pins `selected_index` to the e-graph's already-extracted choice
(`add_selected_constraint`, `:199-212`, forces `value == selected_index`) and then "solves" a
1-variable / 5-constraint CSP whose constraints are legality predicates (recognizer, substrate,
simd, capacity, `:52-81`). It re-derives the e-graph's pick and records legality facts. The facts
it emits are candid about this: `static_profile_provider_status: "static-template-blocker"`,
`direct_sink_only_status: "sink-only-static-blocker"`, `row_move_toward_sota_status:
"measured_architectural_block"` (`decision_csp.rs:183-186`). **The CSP has no decision to make
because the sites are independent and multiplicity is 1.**

The mechanism that gives the CSP a real job is **inter-site coupling**, and the right solver for
its graph class is **PBQP**, not the generic forward-checking B&B the substrate currently spreads
(`SolveConfig { pruning: ForwardChecking, ordering: FailFirst }`, `decision_csp.rs:85-97`). The
family's target graph — "independent sites + budget coupling + few coherence edges" — is precisely
the Partitioned Boolean Quadratic Problem shape: each site is a node with a discrete domain (its
admissible shapes/kernels), a per-choice cost **vector**, and a per-edge cost **matrix** for the
few coherence pairs (two adjacent scan sites sharing a prefilter table; a sub-rule whose shape must
agree across two callers). On a near-tree graph PBQP's R0/R1/R2 reductions solve in O(n) and are
**provably optimal when no RN heuristic node arises** (a tree never needs RN). This is the
distinguishing claim: an e-graph cost is per-node-plus-children and **cannot express a pairwise
cost between two siblings' choices**; PBQP can. The honest position: keep argmin (= e-graph
extraction) at a single independent site; introduce PBQP only when ≥2 sites with a real coherence
edge are modeled in the IR (see Q5 below — they are not yet).

The budget knapsack (Q7) is one such coupling: `sum_sites static_size_cost ≤ generated_loc_budget`
folded as a global `bounded_sum` constraint. It is **not** selection-equivalent to the per-rule
priority cascade — the cascade is a local P1..P8 decision (`PriorityStep`, `ir/src/cost.rs:263-286`)
that cannot see a global budget, so a binding budget can flip a locally-preferred shape. The one
solver home that coordinates this is the **`csp-solver` crate** (vendored `crates/csp-solver/`,
consumed by LOCK-1 for types and LOCK-2 for shapes); its constraint store is the single
coordination point, and PBQP would be added as a solver mode there (`SolveConfig`), not a new
subsystem.

### 1.4 Widening rewrites (Q2)

The substrate already ships a **widening (manufacturing) rewrite** — `NormalizeDirectSinkCost`
(`backend_egraph.rs:191-237`) invents a new node `"{id}#direct-sink-normalized"` with lowered
`static_size_cost`/`shape_rank` and unions it in. Its coexistence with an all-narrowing set is
sound by three mechanisms, each demonstrated:

- **Confluence is not required.** Equality saturation keeps every form; the join lattice makes the
  order of rule application irrelevant to the saturated set. This is the phase-ordering solution
  of Tate–Stepp–Tatlock–Lerner, *Equality Saturation* (POPL 2009), realized here.
- **Termination** is by a **fire-once guard** (`search` skips classes already carrying a
  `#direct-sink-normalized` node, `backend_egraph.rs:203-206`) plus the scheduler's hard
  `egraph_iter_limit: 64` / `egraph_node_limit: 100_000` (`crates/egraph/src/cost_config.rs:66-67`).
  This is exactly egg's discipline for identity-introducing rules (guarded/conditional rewrites,
  Willsey et al., *egg: Fast and Extensible Equality Saturation*, POPL 2021).
- **Semantics preservation is the live risk.** `NormalizeDirectSinkCost` is a *cost* rewrite, not
  a behavioral equality — it asserts two candidates decide the same shape at different recorded
  cost. That is sound only if the normalization is a true no-op on the emitted parser. A
  prototype must assert byte-identical emission across the pre/post-normalization candidate, or the
  rewrite is unsound. Precedent for widening+narrowing coexisting in shipping engines: egg's own
  rule sets pair strength-reduction (`(* x 2) → (<< x 1)`) with identity introduction
  (`x → (* x 1)`, guarded); Cranelift's ISLE/aegraph pairs simplification with legalization
  (widening to a target-legal form). The lesson for 2A: every widening rule carries a soundness
  oracle, and the extraction cost must make the widened form *lose* unless it genuinely helps, so
  the class never grows unboundedly with dead alternatives.

---

## 2. The substrate (path:line)

- **The engine** — `crates/egraph/`: `egraph.rs` (297, `EGraph<N,A>`, `add`/`union`/`rebuild`,
  Vec-backed classes), `extract.rs` (215, greedy bottom-up `Extractor`, `Lattice`, `CostModel`,
  `AstSize`, `Scalar<T>`), `rewrite.rs` (`Rewrite`/`RewriteFn`, `run_on_dirty`), `scheduler.rs`
  (`BackoffScheduler`, `RunReport`), `csp_scheduler.rs` (368, `CspScheduler` — AC-3 dirty-closure
  saturation over `ParentDirtyProp`, csp-solver-backed), `cost_weights.rs` (191, shared
  `CostWeights` + the null-calibration record), `cost_config.rs`, `analysis.rs`, `unionfind.rs`.
  `egraph-derive/` (343) auto-derives `Language` from an enum (recursive fields → `Id`).
- **The one live island** — `skinny/crates/passes/src/backend_egraph.rs` (312): `select` builds
  the flat e-graph, saturates with `BackoffScheduler` + `NormalizeDirectSinkCost`, extracts the
  argmin, stamps `ActiveCostFacts`.
- **The witness CSP** — `skinny/crates/passes/src/decision_csp.rs` (279): `finalize_rule` →
  `solve_rule`, pinned single-variable `Csp<CostFiniteDomain>`, emits `DecisionCspFacts`.
- **The decision driver** — `skinny/crates/passes/src/lib.rs`: `compile` (`:33-` pipeline:
  `derive_recognizers` `:39` → `derive_backend_shape_with_diagnostics` `:48`),
  `choose_backend_shape` (`:479-506`), `backend_candidates` (`:508-590`, the priority cascade),
  `PRIORITY_TABLE` (`:473`).
- **The fact schema** (the cost/decision provenance seam) — `skinny/crates/ir/src/cost.rs`:
  `CostFacts` (`:4`), `ActiveCostFacts` (`:172`, e-graph provenance:
  `egraph_node/eclass/iteration/rewrite_count`, `determinism_replay_status`,
  `selection_trace_hash`), `DecisionCspFacts` (`:202`, the ~45-field CSP record),
  `PriorityStep` P1..P8 (`:263-286`), `all_backend_shapes()` (`:334`).
- **The IR nodes selected over** — `skinny/crates/ir/src/lib.rs`: `BackendShape` (`:361`, 5
  variants), `enum Recognizer::SimdScan { mode, alphabet, site }` (`:416`), `SimdMode` (`:425`,
  only `Exact`), `SimdSite` (`:430`, only `PreEntry`), `StructuralAlphabet` (`:435`).
- **The residualization consumer (island #3 target, projection canon)** — PROVE-12's
  `find_component_delim` const-generic mask fold lives in
  `codegen/src/runtime_generator.rs::CSS_GENERATED_RS`; the pattern ("`const fn mask` +
  const-generic seam") generalizes "to any grammar-derived delimiter/STOP-set scan" — the exact
  hook where a projection-canon island's extracted form lowers to a residualized emit.
- **The legacy structural islands** (archived at GN-0, but the *design* of record) — the outer
  `crates/ir/src/egraph/` + `crates/ir/src/rewrites/` carry the child-bearing grammar rewrites
  (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
  `CommonSuffixFactor`) and the `GrammarCostModel`/`RegexExtractionCost` that embed `CostWeights`.
  These are the substrate for a *structural* island; per XL-A the port is organ-level into GN-1.

---

## 3. Integration seams (where islands attach)

1. **Scan-plan island** attaches at `passes::derive_recognizers` (`lib.rs:338`) →
   `ir::Recognizer` (`ir/lib.rs:416`). Today it is multiplicity-1 (only `SimdMode::Exact` /
   `SimdSite::PreEntry`). It becomes a real island the moment GN-2.W1 lands
   `SimdSite::SelfLoop { terminators }` + `SimdMode::Prefilter` (SPEC §2 GN-2.W1): then a rule can
   present PreEntry-whitespace-skip **and** SelfLoop-inert-run-skip as competing scan plans, each
   with a static-size cost and a cyc/B cost. The cost model is the same `CostWeights` vocabulary;
   the consumer is `codegen/src/lower/recognizer/` (GN-2.W1's new dispatch, PT-1.W5 co-lands the
   `dfa.find` branch — the recognizer dispatch emits exactly one lowering per site).
2. **Backend-shape island** already attached (`choose_backend_shape` → `backend_egraph::select` →
   consumer `codegen::lower::rust::lower_to_rust`, named at `backend_egraph.rs:11`). The 2A work
   here is *demotion*: recognize it is flat/argmin and either (a) prove a child-bearing rewrite
   that makes it a real e-graph, or (b) collapse it to a canonicalizer sharing the `Lattice`,
   deleting the saturation call as consumer-less machinery (Y.13).
3. **Projection/`->` canon island** attaches at the type-plane projection lowering (GN-3's
   `ExprKind::Project` thread into `infer` + `DirectBuild.type_ref`; SPEC §GN-3 W3); its extracted
   canonical form lowers through the PROVE-12 residualization seam. This is the island most likely
   to have genuine multiplicity (Seq/Map re-association, dispatch-eligible vs. sequential forms) —
   the one where extraction-as-decision earns over plain canonicalization.
4. **Regex HIR canon island** is engine-tier (XL-C): it lives in parse-that/bbnf-regex, consumes
   the same `crates/egraph` via `RegexExtractionCost`, and is LOCK-5's to own the lawful HIR form;
   2A consumes its verdict and treats it as a fourth island on the shared substrate, not a thing
   2A redraws.

---

## 4. Prior art (citations)

- **egg** — Willsey, Nandi, Wang, Flatt, Tatlock, Panchekha, *egg: Fast and Extensible Equality
  Saturation*, POPL 2021. The `Extractor` here is egg's greedy bottom-up; the guarded widening
  rule is egg's conditional-rewrite discipline; e-class analyses map to `crates/egraph::Analysis`.
- **Equality Saturation** — Tate, Stepp, Tatlock, Lerner, POPL 2009. The order-independence that
  lets a widening rule coexist with narrowing ones without confluence.
- **egglog** — Zhang, Wang, Willsey et al., PLDI 2023 (Datalog ∪ e-graphs, semi-naïve evaluation).
  The reference for scheduling islands as rule groups and for the dirty-frontier saturation the
  `CspScheduler` implements by hand.
- **Denali** — Joshi, Nelson, Randall, *Denali: A Goal-directed Superoptimizer*, PLDI 2002. The
  ancestor of "e-graph generates candidates, a constraint solver extracts the optimum" — exactly
  2A's e-graph→CSP pipeline. The historical proof that the split is a known-good architecture.
- **Extraction is NP-hard** with sharing-aware cost — see the *extraction-gym* corpus and treewidth
  / ILP extractors (Goharshady et al.; Cai et al., *SmoothE*, ASPLOS 2025). Justifies the
  substrate's choice of greedy bottom-up for **monotone/additive** costs (optimal there) and warns
  that any non-monotone cost at a structural island needs ILP/treewidth, not greedy.
- **PBQP** — Scholz & Eckstein, *Register Allocation for Irregular Architectures by Coalescing*
  (2002); Eckstein/Hames PBQP instruction selection; Buchwald & Zwinkau, *Instruction Selection by
  Graph Transformation*. The per-node-choice + pairwise-cost-matrix solver that fits "independent
  sites + few coherence edges"; near-linear + optimal on tree/near-tree via R0/R1/R2 reductions.
- **Cranelift aegraph / ISLE** — Fallin, *ægraphs* (acyclic e-graphs for a compiler backend). The
  precedent that a shipping compiler runs e-graph rewrites at a *bounded* budget on the hot path —
  the empirical answer to Q3's "does the organ hold the per-parse budget."
- **LLVM SelectionDAG/GlobalISel** — the maximal-munch/greedy baseline against which global
  selection (PBQP, e-graph) must justify its solve cost.

---

## 5. Named risks & counterexamples

- **Consumer-less island (Y.13 violation).** The gravest risk: shipping an e-graph at a flat
  island where multiplicity is provably 1. The null-calibration record (`cost_weights.rs:134-167`)
  is the standing counterexample — the machinery ran, changed nothing, on four grammars. Mitigation:
  the family's law is "e-graph only where a child-bearing rewrite yields ≥2 measured survivors";
  otherwise the island is a canonicalizer and the saturation call is deleted, not dormant.
- **Fragmented cost vocabularies.** The legacy structural islands use f64 `CostWeights`
  (`CALIBRATED_WEIGHTS`); skinny's live island uses an integer `DecisionCost`; the grep shows
  **skinny does not consume `CALIBRATED_WEIGHTS` at all**. Two cost languages is drift the
  constitution forbids (`cost_weights.rs:9-11` names splitting them "forbidden"). Mitigation: one
  `Lattice`/`CostWeights` vocabulary across all islands, integer or f64 chosen once.
- **The CSP that decides nothing.** `decision_csp` pinned to the e-graph's pick is substrate
  without a decision — its own facts admit "measured_architectural_block". Mitigation: the CSP
  layer materializes only when ≥2 sites couple (PBQP), else it is deleted from the per-rule path.
- **Widening unsoundness.** `NormalizeDirectSinkCost` is a cost rewrite masquerading as an
  equality; if the two candidates are not behaviorally identical, extraction can pick a wrong
  shape. Mitigation: byte-identical-emission oracle across every widened candidate.
- **Hash-collision non-determinism (Q3).** The `candidate_hash` tie-break tail is 64-bit; a
  collision reintroduces class-iteration dependence and can break `regen∘regen`. Mitigation:
  tie-break on the unique id string.
- **Context-sensitive extraction (Q4).** Backend shape is chosen **per rule_id**, context-free
  (`choose_backend_shape` takes one `rule`, `lib.rs:479`). A sub-rule inlined under a `SinkOnly`
  caller and an `OffsetTape` caller cannot be served by one per-class shape. The fleet does not
  model this today — `admits_sink_only` exists (`lib.rs:524`) but no per-context conflict is built;
  the conflict would demand either context-indexed extraction (an ILP/PBQP formulation, expensive)
  or a duplicate-and-specialize repair (identity-preserving but LOC-growing). Audit result: **no
  binding conflict is presently modeled**, so context-free per-class extraction is sound *for now*;
  the risk is latent and must be re-audited when inlining decisions (LOCK-3) land.
- **Structural islands may still be multiplicity-1.** The sweep null result is not proof the
  *structural* islands are barren — it is proof the *current rewrite set* is confluent-to-one on
  these four grammars. A prototype must show a rewrite (e.g. prefix-factoring that competes with
  dispatch-table emission) that yields a real ≥2 survivor set, or the structural island too is a
  canonicalizer.

---

## 6. What a prototype must test to move this family

1. **Multiplicity census (Q1).** On the four-grammar fleet at HEAD, instrument the saturated
   e-class size at each candidate island (grammar-tier structural + backend-shape + the new
   scan-plan once SelfLoop lands). Report average/max class size and the fraction of extractions
   that face ≥2 survivors, split by node kind (child-bearing vs childless). Success = at least one
   island demonstrates ≥2 survivors at a child-bearing node; failure = the whole fleet is
   multiplicity-1 and 2A reduces to a canonicalizer-fleet (a legitimate, documented outcome).
2. **Child-bearing rewrite that earns extraction.** Author one rewrite (prefix-factor vs.
   dispatch-eligible form, or Seq re-association at a projection) that provably produces two
   distinct extractable forms whose cost ranking flips under a plausible weight change. This is the
   existence proof the null-calibration says is missing.
3. **Budget/PBQP separation (Q6, Q7).** Construct a two-site grammar with a real coherence edge
   (shared prefilter table) and a binding `generated_loc_budget`, and solve it three ways:
   e-graph-extraction-per-site (cannot see the edge), generic B&B (`decision_csp`'s current mode),
   and PBQP. Measure solve-µs and optimality gap. Success = PBQP matches B&B's optimum at O(n) cost
   on the near-tree, and both differ from per-site argmin (proving the coupling is real).
4. **Per-parse budget + teardown (Q3).** Time build+saturate+extract+**teardown** for a structural
   organ on the largest fleet grammar (CSS L4, 2875 states) and confirm it holds the ~230µs
   compile-order budget; confirm `regen∘regen` byte-identity with the id-string tie-break; confirm
   the `Vec`-classes + LIFO-pending path admits no HashMap-iteration dependence.
5. **Profile-fed flip (Q8).** Replace one hand-assigned `perf_cost` integer with a coefficient
   fitted from measured cyc/B (A/B-differential to attribute a per-feature coefficient the
   whole-grammar cyc/B underdetermines), committed as a fact table read at regen (env-fenced), and
   show it flips a real per-site decision and wins cold-paired at real value-start positions. This
   is the only test that turns the `static-profile-provider` blocker green.

**Concrete-artifact status:** items 1, 4 are measurable on the substrate at HEAD (the island and
engine exist). Items 2, 3, 5 are SPEC-ONLY until authored — they are the family's load-bearing
prototypes, and each is falsifiable.
