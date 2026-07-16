# LOCK-2 — THE GRAPH ENGINE — PRIOR-ART SWEEP (pass 1)

Author: prior-art researcher, pass 1. Constitution-bound (PORTFOLIO head): no family
allegiance; concrete artifacts or SPEC-ONLY; consumer-less substrate forbidden (Y.13);
determinism/regen-bit-identical is law. Registry/favored-approach withheld by protocol.

The problem class: **where and how graph optimization governs the generator pipeline** —
HIR/IR canonicalization, kernel/backend selection, capacity + schedule decisions, cost
models fed by profile facts, deterministic reproducible regen. Two engines are in-tree and
are the seeds: an egg-shaped e-graph (`crates/egraph`) and a general CSP solver
(`crates/csp-solver`, vendored csc411). The four family centers under adjudication: 2A
e-graph islands at named decision points; 2B the middle IR *is* an e-graph end-to-end; 2C
the decision driver *is* a CSP (e-graphs only generate candidates); 2D profile-closed-loop
(PMU-fitted cost coefficients, committed fact tables).

---

## 1. WHAT EXISTS IN-TREE (the seeds, cited)

- **`crates/egraph`** — a from-scratch egg-like engine (~1885 LOC): `EGraph` + `unionfind`
  congruence closure, `rewrite`, `scheduler` (BackoffScheduler shape), **`csp_scheduler.rs`**
  (CSP-driven eq-sat), **`extract.rs`** (greedy bottom-up), `cost_config` / `cost_weights`
  (shared `CostWeights` + `Lattice`/`Scalar`/Pareto marker), `analysis` (e-class analyses).
- **`csp_scheduler.rs`** models "which e-classes need re-probing this iteration" as a CSP: a
  monotone `DirtyDomain` lattice per class, `ParentDirtyProp` constraints per parent edge, one
  `csp.propagate()` (AC-3) computes the transitive dirty closure, `run_on_dirty` re-probes only
  dirty classes (`csp_scheduler.rs:1-42, 204-321`). Explicitly *isomorphic* to the grammar
  tier's `passes/csp_domains.rs` (same `LatticeDomain`/`revise`). Builds a **fresh CSP each
  iteration** (`build_csp`, `:328`) — a real cost.
- **`extract.rs`** — the "egg greedy bottom-up" extractor (its own doc-comment, `:1-11`):
  fixed-point over classes, each class assigned its cheapest e-node from child bests. The
  comment concedes it is **"not optimal for cost models that depend on sharing"** (`:9-11`).
  Cost is `Lattice`-typed (`Scalar<T>` + Pareto `dominated_by`), but every production cost
  model returns a scalar today (`:44-49`).
- **`cost_weights.rs` — THE IN-TREE REFUTATION (measured null result).** The grid-sweep
  calibration (`CALIBRATED_WEIGHTS`, `:134-191`) over 54 configs × 4 grammars (JSON/BBNF/CSS
  L4/Sheets) returned **identical DTA state counts to baseline for every grammar** (JSON 51,
  BBNF 476, CSS L4 2875, Sheets 191 — zero variance). The recorded cause: *"the current
  e-graph rewrite set produces e-classes with at most one semantically-distinct canonical
  form … so the cost model has no choice to exercise."* This is our own evidence base saying
  **the e-graph provides zero extraction leverage today** — there are no competing candidates
  for the cost model to choose between. Any LOCK-2 route that leans on cost-driven extraction
  must first manufacture real alternatives, or it optimizes an empty choice set.
- **`crates/csp-solver`** (csc411 vendored) — full CSP: `domain`, `constraint`, `ordering`,
  `solver`, `bitscan`, `builder`, `puzzles`, `py` bindings. AC-3 propagation + search; the
  same engine LOCK-1 eyes for width/overload solving.
- **PROVE-12 (residualization, GO/CONFIRMED)** — the one measured graph-adjacent win:
  generation-time const-eval + beta-reduction folds the CSS delimiter test to a straight-line
  `u128` bitmask immediate; every CSS corpus/lane fewer ins/B (3.7–6.4%), cyc/B 0.81–0.98×,
  +18 gen LOC, byte-identical parity (`PROVE-12/REPORT.md`). This is *extraction emitting
  specialized code* — the concrete face of what 2A/2B/2C must produce, but achieved by
  monomorphization, **not** by the e-graph.
- **The cost/BackendShape seam (R1 §8)** — `ir/cost.rs` (`CostFacts`, `ActiveCostFacts` with
  egraph/CSP selection provenance, `DecisionCspFacts`, `PriorityStep`), `BackendShape`
  (5 variants), `passes::derive_backend_shape`, `passes::derive_recognizers`
  (`SimdMode`/`SimdSite` extension enums). This is where a graph-selected decision plugs in.

**In-tree gestalt:** the machinery (e-graph + CSP scheduler + cost lattice + calibration
harness) is fully built and consumer-thin. Its single measured effect on output is null
(the sweep); the only measured pipeline win (PROVE-12) bypasses it. Per Y.13 (consumer-less
substrate forbidden), the standing question LOCK-2 must answer is not "build a graph engine"
— it exists — but **"name the decision that has ≥2 real candidates a cost model discriminates,
and wire it to a measured consumer,"** or retire the engine.

---

## 2. WHAT WON / LOST ELSEWHERE FOR THIS PROBLEM CLASS

### 2.1 Equality saturation core (the 2B substrate)
- **egg (POPL 2021)** [1] — the canonical library our crate clones. Contributions: *rebuilding*
  (deferred, amortized congruence-invariant restoration — asymptotic speedup), *e-class
  analyses* (domain lattice attached to classes), and the *backoff scheduler* (bans rules
  matching in exponentially-growing locations). Our engine has all three shapes.
- **egglog (PLDI 2023, "Better Together")** [2] — unifies Datalog + eqsat: semi-naive
  evaluation, relational e-matching, lattice values as first-class. The state of the art for
  *scaling* saturation and for expressing analyses declaratively. Relevant if 2B pursues a
  rule DSL; heavier than a hand-rolled rewrite set.
- **eqsat MLIR dialect (2025)** [3] — a non-destructive rewriting dialect embedding eqsat in a
  production compiler IR. Direct prior art for 2B's "the middle IR *is* an e-graph."

### 2.2 The production winner: acyclic e-graphs, NO fixpoint
- **Cranelift ægraph (EGRAPHS 2023; bytecodealliance RFC #27; cfallin blog)** [4][5] — the most
  load-bearing prior art. A real code generator (compiles untrusted Wasm in production)
  rearchitected its mid-end onto e-graphs to kill pass-ordering, then deliberately chose an
  **acyclic e-graph + eager (non-batched) rule application + single bottom-up pass, no
  fixpoint**. Rationale: in an acyclic egraph the nodes visible from an eclass id never change,
  so each rule fires at each node **once** — giving an efficient single-pass algorithm and
  **determinism** without saturation loops. Extraction is fused into *scoped elaboration*
  (egraph → scheduled CFG, subsuming GVN/LICM). **Lesson for LOCK-2:** the production system
  that won did **not** run equality saturation to fixpoint and did **not** solve NP-hard DAG
  extraction; it made the graph acyclic and extracted in one deterministic pass. This directly
  pressures 2B (full eqsat IR) and validates a bounded/scoped 2A. It also satisfies our regen-
  bit-identical law by construction (no scheduler nondeterminism).

### 2.3 Extraction is the hard part (2A/2B/2C all depend on it)
- **NP-completeness (effect.systems; OOPSLA 2024 sparse extraction)** [6][7] — DAG-cost
  extraction (which credits shared subexpressions) is **NP-hard and hard to approximate within
  any constant ratio**. The acyclicity constraint is the main bottleneck for ILP extraction.
- **Greedy bottom-up is unsound under sharing** [6][8] — the exact algorithm our `extract.rs`
  uses: it *overestimates* cost by ignoring common-subexpression reuse and can pick
  **cyclic/invalid** terms. Fine for tree/monotone cost (our doc-comment says as much), wrong
  the moment sharing or DAG cost matters.
- **Tractable frontiers** — OOPSLA 2024 [7]: optimal extraction parameterized by **treewidth**
  (≤10 beats ILP by orders of magnitude). **e-boost (ICCAD 2025)** [8]: adaptive heuristics +
  exact solving on a reduced core. **SmoothE (ASPLOS 2025)** [9]: differentiable/relaxed
  extraction. **Takeaway:** if LOCK-2 needs *optimal* extraction it must either keep the
  e-graph sparse/low-treewidth or accept greedy's unsoundness explicitly (and prove the cost
  model is sharing-independent).

### 2.4 The decision-driver-as-CSP prior art (2C)
- **Unison (TOPLAS 2019, "Combinatorial Register Allocation and Instruction Scheduling")** [10]
  — the canonical "compiler back-end decision *is* a CSP" result. Constraint programming models
  global regalloc + scheduling (spilling, coalescing, rematerialization, packing) exactly,
  exploits processor-specific features, **scales to ~1000-instruction functions**, ships in
  industry with LLVM. Measured wins: **1.1–10% speedup, 1.3–3.8% code-size reduction.**
  **Two lessons for 2C:** (a) CSP-as-driver is proven and productionizable at bounded scale;
  (b) the *magnitude* of a pure combinatorial-selection win is single-digit-percent — modest
  next to PROVE-12's 4.8% ins/B or the §0 kernel multiples. 2C must justify the solver cost
  against a candidate lane whose win is not already captured by monomorphization.

### 2.5 Phase-ordering motivation + when saturation blows up
- **Phase ordering / eqsat rationale (Tate et al. LMCS 2011; UW PLSE 2025)** [11][12] —
  destructive rewriting is order-sensitive; eqsat applies all rules non-destructively so
  ordering can't disable a later rewrite. This is the *only* first-principles justification for
  an e-graph over a fixed pass pipeline — and it only bites when rules genuinely conflict on
  ordering. (Our null sweep suggests the current rule set has no such conflict.)
- **Guided / Sketch-guided equality saturation (POPL 2024)** [13] — pure saturation explodes on
  large/rich rule sets; guidance (sketches, staged goals) is needed to reach targets. A named
  trap for any "just saturate the IR" (2B) ambition.

---

## 3. CONSTRAINTS OUR EVIDENCE BASE ALREADY IMPOSES

From SPEC §0 (certified/refuted) + in-tree measurement:
1. **Determinism / regen bit-identical is law** (SPEC §2 GN-1 gate; PROVE-12 §7). This
   *forbids* nondeterministic saturation scheduling in the emit path — favoring Cranelift-style
   single-pass/acyclic or a fully-ordered CSP. Any e-graph on the generator path must extract
   deterministically.
2. **Warm benches are disinformation; cold paired PMU cyc/B only** (SPEC §0 refutations;
   memory `no-warm-benches`). Any 2D cost-coefficient fit is captured cold/paired or it is not
   evidence. Compile-time itself is a budget (generated-size budget, memory).
3. **The measured null (`cost_weights.rs:134-191`)** — cost-driven extraction changes *nothing*
   today because the rewrite set yields one canonical form per class. This is the sharpest
   constraint: **no route may claim graph-optimization value without first exhibiting a
   decision with ≥2 real candidates and a cost model that discriminates them, benched.**
4. **Consumer-less substrate forbidden (Y.13 / constitution viii)** — the engine exists; a
   family that merely "keeps the e-graph" without a same-wave measured consumer is dead on
   arrival. PROVE-12's pattern (const-fold + monomorphize a grammar-derived stop-set) is the
   proven consumer shape; §0's kernel/backend selection (`SimdMode::Prefilter`, `BackendShape`)
   are the candidate decision sites.
5. **x86 dead; single generator; no god modules; colocation** (SPEC §4, constitution vi) —
   any solver/graph home is one crate, colocated with consumers, aarch64-only in kernels.
6. **The proven wins are kernel/residualization multiples (§0: 03, 05, 12), not selection
   deltas.** LOCK-2 must locate a decision whose *outcome* moves one of those lanes, not invent
   an orthogonal optimizer subsystem (memory `system-cohesion`, `no-orthogonal-codepaths`).

---

## 4. TRAPS NAMED IN COMPARABLE SYSTEMS (adversarial checklist)

- **T1 — the empty choice set.** (In-tree null sweep + eqsat literature.) An e-graph with no
  competing extractions is a cost model optimizing nothing. **Verify ≥2 candidates before
  claiming a win.**
- **T2 — greedy extraction unsoundness under sharing / cycles.** Our `extract.rs` can return
  cyclic or cost-overestimating terms once DAG cost matters [6][8]. Either keep cost
  sharing-independent (prove it) or move to acyclic/treewidth/ILP extraction — but the latter
  is NP-hard [7].
- **T3 — saturation nondeterminism vs. the regen law.** Batched fixpoint saturation with a
  backoff/random scheduler is not obviously bit-reproducible; Cranelift chose acyclic+single-
  pass *because of this* [4][5]. A 2B "IR-is-egraph end-to-end" must show byte-identical regen
  under a fixed schedule.
- **T4 — saturation blow-up on rich rule sets** [13] — "just saturate" does not scale; needs
  guidance or a node/iter cap (we have `egraph_node_limit=100_000`, `iter_limit=64`).
- **T5 — the modest-magnitude trap.** Combinatorial selection (Unison) buys single-digit % [10];
  our proven levers are kernel/residual multiples. A CSP/e-graph that costs compile-time must
  clear its own overhead against a lane the monomorphizer didn't already capture.
- **T6 — fresh-CSP-per-iteration overhead** (`csp_scheduler.rs:328`) rebuilds the propagation
  graph every saturation step; on grammars where the dirty frontier is most classes this is
  pure loss vs. a plain worklist. Bench the scheduler against BackoffScheduler on the 4-grammar
  corpus before crowning CSP-scheduling.
- **T7 — double-homed solver substrate.** LOCK-1 (1B/1D) also wants "one constraint store" /
  "types as e-classes." A LOCK-2 route that forks a second CSP/e-graph home violates cohesion;
  the gestalt will collapse it. Design for *one* solver home and *one* e-graph home shared with
  LOCK-1/5.

---

## 5. MAPPING PRIOR ART → THE FOUR FAMILY CENTERS (no allegiance; evidence only)

- **2A EGRAPH-ISLANDS** — best-supported by production evidence: bounded e-graphs at named
  decision points, each with a same-wave consumer + cost model, is exactly the Cranelift
  posture (scoped, acyclic, single-pass) [4] and satisfies Y.13 + the regen law. The open gap
  is T1: each island must exhibit ≥2 real candidates (the null sweep shows today's grammar-tier
  rewrites do not).
- **2B IR-IS-EGRAPH** — has literature backing (eqsat MLIR dialect [3], egglog [2]) but runs
  straight into T3 (determinism), T4 (blow-up), and the NP-hard extraction [6][7]. Cranelift is
  the cautionary precedent: it adopted e-graphs for the *whole* mid-end but retreated from
  fixpoint saturation to acyclic single-pass. A 2B route must inherit that retreat or defend
  against T3/T4 with measured bit-identical regen.
- **2C CSP-CENTRAL** — Unison [10] is the proof of existence and the ceiling: proven, bounded
  (~1000 vars), single-digit-% wins, e-graphs demoted to candidate generators. Fits our CSP
  substrate and the `DecisionCspFacts` seam (R1 §8). Must clear T5 (magnitude vs. compile cost)
  and name the per-site variable set (kernel / capacity / prefilter-tier / residual-depth — all
  already enum'd in `ir/`).
- **2D PROFILE-CLOSED-LOOP** — the cost-coefficient-fitting idea; blocked by SPEC §0's bench law
  (cold paired PMU only) and by the null sweep (fitted weights change nothing until real
  candidates exist). Prior art: PGO cost models are standard, but here the loop is only
  architecture if extraction has choices to re-rank. Depends on 2A/2C first exhibiting a live
  decision.

**Synthesis pointer (unowned):** the evidence tilts toward a *bounded, deterministic,
consumer-anchored* graph engine — 2A's islands or 2C's driver over the existing CSP — with the
e-graph demoted to candidate generation, extraction kept sharing-independent or acyclic, and
every claim gated on exhibiting ≥2 candidates + a benched consumer (the PROVE-12 shape). Full
fixpoint-saturated IR (2B) carries the heaviest unpaid debts (T3/T4 + NP-hard extraction) and
the least in-tree evidence (the null sweep). None of this is a verdict — it is the constraint
field the design passes must clear.

---

## SOURCES
[1] egg: Fast and Extensible Equality Saturation, POPL 2021 — https://arxiv.org/pdf/2004.03082
[2] Better Together: Unifying Datalog and Equality Saturation (egglog), PLDI 2023 — https://ztatlock.net/pubs/2023-pldi-egglog/2023-pldi-egglog.pdf
[3] eqsat: An Equality Saturation Dialect for Non-destructive Rewriting, 2025 — https://arxiv.org/pdf/2505.09363
[4] ægraphs: Acyclic E-graphs for Efficient Optimization in a Production Compiler, EGRAPHS 2023 — https://cfallin.org/pubs/egraphs2023_aegraphs_slides.pdf ; blog https://cfallin.org/blog/2026/04/09/aegraph/
[5] Cranelift e-graph RFC — https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md
[6] The E-graph extraction problem is NP-complete (effect.systems) — https://effect.systems/blog/egraph-extraction.html
[7] Fast and Optimal Extraction for Sparse Equality Graphs, OOPSLA 2024 — https://2024.splashcon.org/details/splash-2024-oopsla/144/
[8] e-boost: Boosted E-Graph Extraction with Adaptive Heuristics and Exact Solving, ICCAD 2025 — https://arxiv.org/pdf/2508.13020
[9] SmoothE: Differentiable E-Graph Extraction, ASPLOS 2025 — https://www.csl.cornell.edu/~zhiruz/pdfs/smoothe-asplos2025.pdf
[10] Combinatorial Register Allocation and Instruction Scheduling (Unison), TOPLAS 2019 — https://arxiv.org/pdf/1804.02452
[11] Equality Saturation: A New Approach to Optimization, LMCS 2011 — https://www.cs.cornell.edu/~lerner/papers/lmcs11-eqsat.pdf
[12] What is the phase ordering problem and can equality saturation help?, UW PLSE 2025 — https://uwplse.org/2025/04/14/Phase-ordering.html
[13] Guided Equality Saturation, POPL 2024 — https://steuwer.info/files/publications/2024/POPL-Guided-Equality-Saturation.pdf
