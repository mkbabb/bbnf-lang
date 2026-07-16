# LOCK-2 — THE GRAPH ENGINE — pass-2 prior-art research

Researcher, no family allegiance. Sweep of web + codebase for the problem class: e-graph +
CSP optimization governing a parser-**generator** pipeline (HIR canon, IR rewrite, kernel/
backend selection, capacity/schedule, cost models, deterministic regen). Families under
adjudication: 2A EGRAPH-ISLANDS, 2B IR-IS-EGRAPH, 2C CSP-CENTRAL, 2D PROFILE-CLOSED-LOOP.

Verdict up front: **the measured evidence in our own tree already refutes the strong forms of
2B and 2C and confirms 2A's local-island posture.** The world's production precedent (Cranelift)
converged independently on the same posture for the same reasons. Citations below.

---

## 1. What exists in-tree (the seeds, read `path:line`)

- **`crates/egraph/`** (3105 LOC incl. tests): a working egg-shaped engine — union-find
  (`unionfind.rs`), congruence closure + deferred `rebuild()` (`egraph.rs:270`), two schedulers
  (`scheduler.rs` BackOff-shaped, `csp_scheduler.rs` CSP-backed), greedy bottom-up extraction
  (`extract.rs`), a `Lattice`-valued `CostModel` trait (`extract.rs:36-94`), shared `CostWeights`
  (`cost_weights.rs`), and `egraph-derive` deriving `Language` from enums.
- **`crates/csp-solver/`** (vendored csc411): AC-3 (`solver/ac3.rs`), branch-and-bound
  `optimize` with `DomainCostEval` (`solver/optimize.rs`), lattice/monotonic domains, Kuhn–
  Munkres assignment, `all_different`. This is the "engine" LOCK-1B/2C both name.
- **The two engines already interlock once** — `egraph/src/csp_scheduler.rs`: eqsat's dirty-set
  is modelled as a CSP with a monotone `DirtyDomain` lattice + `ParentDirtyProp` (AC-3 through
  parent edges) computing the transitive dirty closure. Header claims isomorphism to `bbnf-ir`'s
  `passes/csp_domains.rs`.
- **The decision-driver seams are LOCAL** (R1 §8): `Recognizer::SimdScan{SimdMode,SimdSite}`
  (`ir/lib.rs:416`), `BackendShape` (5 variants, `ir/lib.rs:361`), `select_prefilter_backend`,
  the `N*=3` kernel crossover, `CostFacts`/`ActiveCostFacts`/`DecisionCspFacts` (`ir/cost.rs`).
  These are per-site cost-threshold detections in `passes::derive_recognizers` /
  `derive_backend_shape`, not a global search.

## 2. The two measured refutations our evidence base already imposes (BINDING)

**R-A — the cost model has no optionality to exercise (the CALIBRATED null).**
`egraph/src/cost_weights.rs:134-191` documents a **54-configuration grid sweep** across
{structural, alt_per_branch, dispatch_bonus, ref_cost} over the 4-grammar corpus (JSON 51,
BBNF 476, CSS-L4 2875, Sheets 191 DTA states). **Result: every grammar's state count was
identical across all 54 configs.** The current rewrite set (`DeduplicateAltBranches`,
`SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`) produces
e-classes with **at most one semantically-distinct canonical form** — so the cost model, the
CSP, and the extractor have nothing to choose between. This is the single most important
constraint on LOCK-2: **2B (extract-by-cost) and 2C (CSP-selects-over-candidates) presuppose a
candidate multiplicity our rewrite set does not currently produce.** Any family claiming
cost/CSP centrality must FIRST show it manufactures real optionality (new rewrites that widen
e-classes), or it is consumer-less substrate (constitution viii / Y.13).

**R-B — the largest measured optimization win came from staging, not the graph.**
PROVE-12 (SPEC §0 #12, CONFIRMED, cyc/B 0.81–0.98×, ins/B −4.8/−6.8%, regen bit-identical) is
**const-fold + monomorphization** of a delimiter mask — partial evaluation / residualization,
straight `const`-generics, zero e-graph, zero CSP. The graph engine is not where the certified
wins live; residualization (LOCK-3D) and SIMD kernels (GN-2) are. LOCK-2 must justify its
existence against this, not assert it.

These two facts must anchor every family judgment. They are not opinions; they are in-tree
measurements.

## 3. Web prior art — what won and lost for THIS problem class

### 3.1 Cranelift ægraph — the production precedent that converged on 2A, and REJECTED 2B's strong form
Fallin, *ægraphs: Acyclic E-graphs for Efficient Optimization in a Production Compiler*
(EGRAPHS 2023; blog cfallin.org/blog/2026/04/09/aegraph). The most load-bearing citation:

- Classical full e-graph **rebuild overhead measured at 23% minimum**, against a total mid-end
  budget of ~10% — i.e. saturation's bookkeeping cost *exceeded the entire optimization budget*.
  They therefore chose an **acyclic, single-pass** e-graph: rewrites applied **eagerly** at node
  construction, **no fixpoint saturation**, extraction by bottom-up DP.
- **Average eclass size = 1.13 enodes.** On already-canonical inputs, multi-representation buys
  almost nothing — the exact phenomenon our R-A null measured from the other side.
- Extraction **ignores shared substructure** (NP-hard if honored); the greedy/DP approximation
  cost only **~0.1% execution-time penalty** on Sightglass. Our `extract.rs` makes the identical
  greedy choice.
- Named traps directly portable to a parser-generator IR: (1) **dominance/scope violations** when
  union nodes merge values valid in different scopes → needs an "available block" analysis;
  (2) **directional rewrites** — eager one-pass only sees forward implications, so a ruleset must
  be designed for *simplification*, not *exploration* (only 2 bad picks in 4M nodes); (3)
  **rematerialization blowup** (duplicating cheap consts enlarges code); (4) side-effect/CFG
  skeleton left immutable.

Lesson for us: **2B "the middle IR is an e-graph end to end … equality saturation under a
schedule" is precisely the design Cranelift measured to be too expensive and unnecessary for
pre-canonical inputs.** The defensible 2B is the *acyclic, no-saturation* variant — which is
essentially 2A with the e-graph as the in-place substrate. Full saturation-to-fixpoint over the
whole IR is a named anti-pattern with production evidence against it.

### 3.2 Extraction is NP-hard — and reaching for ILP is a trap given our cost lattice
- *Fast and Optimal Extraction for Sparse Equality Graphs*, OOPSLA 2024 (HKUST TACO Lab):
  extraction is NP-hard and **hard to approximate within any constant ratio**; ILP is slow; their
  parameterized algorithm handles **treewidth ≤ 10** and was evaluated on **Cranelift** benchmarks.
- *e-boost* (ICCAD 2025, Cornell CSL): heuristic + exact hybrid extraction for logic synthesis.

Relevance: optimal extraction matters **only when the cost model is sharing-sensitive**. Our
cost models (`AstSize`, `GrammarCostModel`, `tape_push`, `inline_body_size_penalty`) are
**additive/monotone** — greedy bottom-up (`extract.rs`) is exact for them, exactly as Cranelift's
DP is. **Trap: importing ILP/treewidth extraction machinery is over-engineering** against our
lattice (violates kiss-perf-bias, extreme parsimony). Cite it as the reason NOT to, unless a
family introduces a genuinely sharing-sensitive objective (e.g. code-size with CSE across sites —
the generated-size budget is the one candidate, §3.4).

### 3.3 egglog — Datalog + eqsat unification (the LOCK-1 interlock, and 2D's honest form)
*Better Together: Unifying Datalog and Equality Saturation*, Zhang et al., PLDI 2023. egglog =
Datalog engine + built-in equality + **lattice-based analyses** + relational e-matching (e-matching
as a relational query, far faster than naive). Two lessons:

- It is the natural home for **LOCK-1D "types-as-eclasses"** and any analysis-as-lattice
  (our `DirtyDomain`, `CostFacts` provenance) — a *single* congruence+lattice substrate serving
  both the type plane and the optimizer. This is the strongest argument for **one e-graph home**
  shared by LOCK-1/2 (the constitution's "one e-graph home?" gestalt question).
- Its lattice-analysis story is how 2D's "cost coefficients as committed facts" stays
  **deterministic**: facts are monotone lattice values in the DB, recomputed by fixpoint, not
  floating PMU noise threaded through search.

### 3.4 Unison — CP for combinatorial codegen: the 2C precedent AND its scaling wall
Lozano, Blindell, Castañeda Lozano et al., *Combinatorial Register Allocation and Instruction
Scheduling*, ACM TOPLAS 2019 (unison-code.github.io). Integrated regalloc + scheduling as **one
constraint program**, provably optimal — **up to ~946–1000 instructions**, trading compile time
for code quality beyond -O3.

Relevance to 2C: CP/CSP as the *central* decision driver is real and wins on *coupled*
combinatorial subproblems (spill/coalesce/schedule interact). BUT: (1) it caps at medium function
size and **buys compile-time-for-quality** — acceptable for a generator (compile-time), fatal for
per-parse. (2) The decisive question for us: **are our per-site lowering choices actually
coupled?** If kernel/capacity/prefilter/residualization-depth are independent per site, "the
decision driver IS a CSP" collapses to *N independent arg-min* — no constraint network, no
solver needed, and dressing it as a CSP is ceremony. **The one genuine global coupling is the
generated-size budget** (a knapsack over per-site inline/dispatch choices) — that, and only that,
is where a real CSP/B&B earns its seat (our `csp-solver` B&B `optimize.rs` is already the tool).
2C is defensible *scoped to the size-budget knapsack*, indefensible as "everything is a CSP."

### 3.5 Scheduling & rebuild — the eqsat operational traps (Willsey thesis; effect.systems notes)
- **Non-termination**: saturation may never terminate (assoc/comm rules); needs iter/node limits
  — our `CspScheduler` already carries `iter_limit`/`node_limit` (`csp_scheduler.rs:178`).
- **Rebuild is the dominant cost** (egg's deferred `rebuild()` is the headline optimization).
  **Our `csp_scheduler.rs` rebuilds a *fresh CSP every iteration* (`build_csp` per iter,
  `:229`)** — this re-pays graph-construction cost egg's native worklist avoids, and duplicates
  the deferred-rebuild machinery already in `egraph.rs`. Whether CSP-over-dirty-set beats egg's
  native worklist is **unmeasured** — a named gap and a candidate "orthogonal subsystem" to fold
  (no-orthogonal-codepaths, system-cohesion). The BackOff scheduler is the baseline it must beat.
- Scheduling ≠ extraction: both are hard and *separate* (effect.systems "schedule/extract notes").
  A family conflating them is imprecise.

### 3.6 Learned/profile cost models — 2D's determinism hazard
MLGO / Ithemal-lineage learned cost models are known to trade **reproducibility and CI stability**
for fit. SPEC §0 is explicit: **ratios certified, absolutes directional; warm benches are
disinformation.** 2D's "cost coefficients fitted from PMU captures" collides head-on with the
**bit-for-bit regen** law (PROVE-12 proved regen bit-identical) unless — as 2D itself states —
the fitted coefficients are **frozen into committed static fact tables**. The determinism is
preservable only if fitting is an *offline, committed* step, never a regen-time re-fit. And R-A
says: with today's rewrite set, **no coefficient value changes any output** — a closed loop has
nothing to close until the rewrite set first manufactures optionality (§3.2 caveat).

## 4. Family-by-family adjudication (evidence, not preference)

- **2A EGRAPH-ISLANDS — best-supported by measurement.** Matches the in-tree reality (local
  detections + per-site cost, R1 §8), Cranelift's converged posture (§3.1), and the constitution's
  Y.13 "island has a same-wave consumer + cost model." Risk it must still clear: R-A — each island
  must produce real optionality or it is a consumer-less cost model (the CALIBRATED null is exactly
  an island with no choice to make).
- **2B IR-IS-EGRAPH — only the *acyclic, no-saturation* form survives.** Full equality saturation
  end-to-end is refuted by Cranelift's 23%-rebuild-vs-10%-budget and 1.13-enode measurements and by
  our own R-A. The lawful 2B is "e-graph as the in-place IR substrate, eager rewrite, greedy/DP
  extract" — which converges onto 2A. Trap: proposing saturation-to-fixpoint over the whole IR.
- **2C CSP-CENTRAL — defensible ONLY scoped to the generated-size-budget knapsack.** Unison proves
  CP wins on *coupled* combinatorial codegen and warns of the scaling/compile-time wall. Our
  per-site choices are largely *independent* → "central CSP" is ceremony except where the global
  size budget genuinely couples inline/dispatch decisions. Name that scope or the family is
  over-generalized. B&B (`optimize.rs`) is the right tool for the knapsack.
- **2D PROFILE-CLOSED-LOOP — architecturally hazardous, salvageable only as offline-committed
  facts.** The loop threatens determinism (regen bit-identical law) and has nothing to fit until
  R-A is broken. Lawful form: fit offline → commit static fact table → regen reads facts (egglog
  lattice-analysis discipline, §3.3). "The loop is the architecture" as stated risks the profiling
  standing law; "the committed fact table is the architecture" is safe.

## 5. Traps named in comparable systems (portable checklist)

1. Full saturation-to-fixpoint over a whole compiler IR — rebuild cost > opt budget (Cranelift 23%).
2. Building the IR as an e-graph when avg eclass size ≈ 1 — no multi-representation to exploit
   (Cranelift 1.13; our R-A null). Substrate without a choice to make = consumer-less (viii).
3. Reaching for ILP/treewidth-optimal extraction when the cost lattice is monotone/additive —
   greedy is already exact (over-engineering vs extreme parsimony).
4. Dressing N independent per-site arg-mins as a "CSP" — no constraint network, no solver earned.
5. Re-fitting cost coefficients at regen time from PMU noise — breaks bit-for-bit regen; commit
   offline facts instead (2D's own escape clause).
6. A fresh CSP rebuilt each saturation iteration (our `csp_scheduler.rs:229`) re-paying the
   construction cost egg's deferred worklist avoids — measure it against BackOff or fold it.
7. Directional eager rewrites picking suboptimal forms (Cranelift trap #2) — design rulesets for
   simplification, not exploration.
8. Dominance/scope violations from merging differently-scoped values (Cranelift trap #1) — a
   parser IR with capture/materialization scopes has the analogue.

## 6. Sources
- Fallin, ægraphs, EGRAPHS 2023 — https://pldi23.sigplan.org/details/egraphs-2023-papers/2/ ;
  blog https://cfallin.org/blog/2026/04/09/aegraph/ ; RFC https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md
- Fast & Optimal Extraction for Sparse Equality Graphs, OOPSLA 2024 — https://cse.hkust.edu.hk/~parreaux/publication/oopsla24a/
- e-boost, ICCAD 2025 — https://www.csl.cornell.edu/~zhiruz/pdfs/eboost-iccad2025.pdf
- Zhang et al., Better Together: Datalog + Equality Saturation (egglog), PLDI 2023 — https://arxiv.org/abs/2304.04332
- Lozano et al., Combinatorial Register Allocation & Instruction Scheduling (Unison), TOPLAS 2019 — https://dl.acm.org/doi/fullHtml/10.1145/3332373 ; https://unison-code.github.io/
- Willsey et al., egg: Fast & Extensible Equality Saturation, POPL 2021 — https://arxiv.org/pdf/2004.03082 ; Willsey thesis — https://www.mwillsey.com/thesis/thesis.pdf
- eqsat schedule/extract notes — https://effect.systems/blog/eqsat-schedule-extract-notes.html
- In-tree: `crates/egraph/src/{cost_weights.rs:134,extract.rs,csp_scheduler.rs,egraph.rs}`,
  `crates/csp-solver/src/solver/optimize.rs`, `ir/src/{lib.rs:361,cost.rs}`,
  PROVE-12 REPORT, SPEC §0, GN-2, R1 §8.
