# LOCK-2 — THE GRAPH ENGINE — pass-2 PRIOR-ART

Researcher: prior-art (no family allegiance). Scope: e-graph + CSP optimization governing the
generator pipeline — HIR canon, IR rewriting, kernel/backend selection, capacity/schedule
decisions, profile-fed cost models, deterministic regen. This sweep answers: what exists here,
what won/lost elsewhere for THIS problem class, which constraints our evidence base imposes, and
the traps named in comparable systems.

---

## 0. What already exists IN-TREE (the seed the families extend, not invent)

The graph engine is not greenfield — it is an egg-shaped substrate already wired to the compiler:

- **`crates/egraph/`** (1885 LOC, edition 2024) — a domain-agnostic equality-saturation library.
  `Language` trait (derivable via `egraph-derive`) is the only consumer obligation; BOTH grammar
  IR nodes and regex HIR nodes implement it, so **both pipelines share one e-graph infrastructure**
  (`lib.rs:1-30`) — this is constitution-clause-ii isomorphism already realized in code.
  - Extraction (`extract.rs`) is the **egg greedy bottom-up** algorithm, explicitly documented as
    "not optimal for cost models that depend on sharing" (`extract.rs:6-11`). A `Lattice`/`Scalar`
    marker (`extract.rs:AA.3`) is present so multi-objective Pareto extraction can land WITHOUT
    churning `CostModel` — the seam for a richer objective is already cut but unused.
  - Two schedulers: `BackoffScheduler` (egg's rule back-off) and **`CspScheduler`** (`csp_scheduler.rs`,
    368 LOC) — models "which e-classes need re-probing" as a CSP over monotone `DirtyDomain` lattices
    with `ParentDirtyProp` constraints; one `csp.propagate()` computes the transitive dirty closure.
    This is an **LLVM-style Changed-bool convergence** (memory: changed-flag-convergence), NOT a
    structural-hash fixpoint — already the disciplined form.
  - `cost_weights.rs` — `CALIBRATED_WEIGHTS` / `CostWeights` is a single-source-of-truth knob struct
    (`structural`, `alt_per_branch`, `dispatch_bonus`) embedded by `GrammarCostModel` and
    `RegexExtractionCost`; domain knobs layer on top (pluggable-components law honored).
- **`crates/csp-solver/`** (vendored from csc411, sync-gated) — AC-3, GAC all-different (Régin 1994),
  forward checking, **lattice domains for monotonic fixed-point propagation**, and `solver/optimize.rs`
  (`OptimizationMode` — COP, not just CSP). This is the engine LOCK-1B/2C would drive.
- **`passes/` pipeline** (R1 §2): `infer → shape → CSP → egraph`; `derive_backend_shape`
  (`passes/src/lib.rs:398`) reads `BackendShapePlan.cost_facts`. `ir/src/cost.rs` already carries
  `ActiveCostFacts` with **egraph/CSP selection provenance** (`selected_shape`, cost-freshness,
  rewrite counts), `DecisionCspFacts`, and an 8-step `PriorityStep` ladder — the decision driver's
  bookkeeping exists; the families dispute how much of the pipeline it should govern.
- **PROVE-12 (GO/CONFIRMED)** — emit-spine residualization: the generator const-folds + beta-reduces
  the CSS delimiter test **at generation time** (const-generic `u128` mask + monomorphization),
  disasm-verified, regen bit-for-bit, +18 gen LOC. This IS "residualization depth" as a concrete
  lowering choice — the exact per-site variable 2C proposes to solve.

**Read:** we have egg + a CSP/COP solver + a cost-facts spine + a proven residualization lowering.
The prior-art question is which discipline (2A/2B/2C/2D) these should compose into.

---

## 1. Equality saturation for compiler optimization — the field, and where it won/lost

**egg (Willsey et al., POPL 2021)** is the reference — fast, extensible equality saturation; e-graphs
represent many equivalent programs compactly; rewrites are **additive, never destructive** (the initial
term survives), and a separate cost-guided **extraction** picks the result [egg]. Our crate is an
egg-shape. The field's headline claim is **phase-ordering avoidance**: EqSat encodes all optimizations
in the e-graph so no rule-application order need be chosen [egg; Tensat].

Where equality saturation LOST / must be bounded:
- **Termination is NOT guaranteed** — rules can introduce opportunities indefinitely; the practical fix
  is **timeout + a back-off scheduler** [Sketch-Guided EqSat, arXiv 2111.13040; egg]. A timeout-driven
  optimizer is a **determinism hazard** for a compiler that must regen bit-for-bit (our clean-regen +
  reproducible-regen law). Any saturation-loop family (2B especially) must pin a **deterministic
  saturation budget**, not a wall-clock timeout.
- **Blowup** — "millions of e-nodes and e-classes"; richer rule sets "exhaust memory faster" [Guided
  Equality Saturation, ACM 3632900]. Grammar/regex e-graphs are small today, but 2B ("IR IS an
  e-graph end to end") inherits this risk at scale.

**The production counter-datapoint — Cranelift's ægraph (Fallin, EGRAPHS 2023; RFC-27 accepted).** A
shipping compiler (Wasmtime, compiles SpiderMonkey.wasm) rearchitected its mid-end to an **ACYCLIC
e-graph**: rewrites (ISLE DSL rules) apply **eagerly and immediately during a single forward
construction pass** — acyclicity means "no need to revisit a node" — then **one cost-based extraction**
during a domtree-preorder placement pass (also doing LICM + remat). This **solved the pass-ordering
problem in production** WITHOUT an unbounded saturation loop, and is deterministic and scalable
[Fallin, aegraph slides + blog cfallin.org/blog/2026/04/09/aegraph]. **This is the strongest signal
for LOCK-2**: the family that mirrors ægraph (bounded eager rewrite + single extraction, island-scoped)
inherits determinism and production-proven scaling; the family that pursues unbounded saturation
(2B literal) inherits the termination/blowup traps and must defeat them with measured budgets.

---

## 2. Extraction — the NP-hardness cliff (binds 2B and 2C directly)

The single most-cited trap in this problem class: **optimal extraction from an e-graph is NP-hard in
general** [Cornell CS6120 tech-mapping notes; e-boost, arXiv 2508.13020]. Greedy bottom-up (our
`extract.rs`) is correct ONLY for **local / monotone** cost functions — cost of a class = min over its
nodes of (node cost + Σ child-class costs), extractable by a Knuth/Dijkstra-style traversal [egg;
juliasymbolics Metatheory]. The moment cost depends on **sharing** (a subexpression reused by two
parents counted once) or on a **global budget**, greedy is silently suboptimal and can even extract a
**cyclic** selection.

Who paid the price and how:
- **Tensat (MLSys 2021)** — tensor-graph superoptimization; uses **ILP for extraction** to guarantee an
  acyclic valid graph at lowest cost, but this "**restricts its application to smaller e-graphs**"
  [Tensat, arXiv 2101.01332]. ILP extraction is optimal and does not scale.
- **ROVER** — RTL datapath optimization; ILP extraction guarantees acyclic + minimal, achieving up to
  **63% area** savings [cited in CS6120 notes]. Same optimal-but-bounded story.
- **e-boost (2025)** — the modern answer: **adaptive heuristics + exact solving** hybrid — heuristic
  prune, exact-solve the residual [arXiv 2508.13020]. This is the shape 2C should adopt if it insists
  on a global objective: e-graph GENERATES candidates, a bounded exact CSP/ILP SELECTS over the
  extracted frontier — not exact-solve the whole graph.

**Direct consequence for our families.** 2A/2D can live on the existing greedy extractor **iff** every
island's cost stays local/monotone (`dispatch_bonus` per-node is fine; a **generated-size budget**
across the whole emit is NOT local — it is exactly the global constraint that makes extraction
NP-hard). 2C names "the generated-size budget" as a CSP constraint (correctly a global constraint) and
"e-graphs only generate candidates" (correctly the e-boost split) — that is the prior-art-sanctioned
architecture. The `Lattice`/`Scalar` seam in `extract.rs` is the hook for Pareto/multi-objective, but
**greedy Pareto extraction is still only heuristic under sharing** — this must be stated, not assumed.

---

## 3. CSP / combinatorial optimization for code generation (binds 2C)

The "the decision driver IS a CSP" family (2C) has heavyweight precedent AND a scaling ceiling:

- **Unison (Lozano, Carlsson, Blindell, Schulte; TOPLAS 2019, arXiv 1804.02452)** — integrated
  **register allocation + instruction scheduling as one constraint program**. Captures the full
  transformation set (coalescing, spilling, live-range splitting, rematerialization, packing);
  **optimal up to ~946–1000 instructions** (nearly an order of magnitude past prior CP approaches);
  mean speedup 1.1–10%, integrated with LLVM. Lesson: CP over code-gen decisions is **potent but
  medium-scale** — it is a *bounded* solver, not a whole-program one. 2C over "per-site lowering
  choices" is Unison-shaped; keep the variable count bounded (per grammar, per island) or it hits the
  same wall.
- **PBQP (Partitioned Boolean Quadratic Problem; Scholz/Eckstein)** — instruction selection over SSA
  graphs and register allocation modeled as a QAP: **node cost-vectors** (the options, exactly one
  chosen) + **edge cost-matrices** (interdependencies). NP-complete, but a **linear-time heuristic
  solver is near-optimal for sparse graphs, optimal for trees**, and beats tree-pattern matchers by up
  to 82% [complang.tuwien PBQP; SCOPES'03; used in LLVM]. **PBQP is the precise formal model for 2C's
  "per-site choice with legality edges + a cost lattice"** — and its sparse/near-optimal solver is the
  scaling escape Unison lacks. If 2C is built, PBQP (not full ILP) is the prior-art-blessed encoding.
- Reinforcement of the **candidate/selection split**: PBQP and e-boost both separate structural
  generation from a bounded selection solve — matching 2C's "e-graphs only generate candidates" and
  LOCK-1B's "local propagators + global search," and LOCK-1A's "CSP only where unification is
  undecidable, as a bounded post-solve." The whole lock family converges on **bounded post-solve**, not
  a monolithic global CSP.

Our `csp-solver` already has the machinery (AC-3, GAC, lattice domains, `OptimizationMode`); what it
lacks vs Unison/PBQP is an **edge-cost (binary-constraint) COP encoding** — the QAP shape. That is a
named gap, not a missing engine.

---

## 4. Profile-fed / learned cost models (binds 2D — and collides with our determinism law)

2D ("optimization as measured feedback; cost coefficients fitted from PMU, committed as fact tables;
solver re-runs at regen") has direct precedent and a sharp reproducibility trap:

- **MLGO (Trofin et al., Google/LLVM, arXiv 2101.04808)** — first full ML integration in a production
  compiler pass (inlining-for-size, then regalloc), RL-trained policies beating decades-tuned heuristics
  by up to 7% size. Critically for us: **MLGO ships FROZEN released models precisely so builds stay
  deterministic and reproducible** — the model is a committed artifact, retrained offline. This is
  exactly our "committed as fact tables (determinism preserved)" clause validated by the largest
  production instance. 2D is prior-art-sound **only if the fit is offline and the committed table is the
  regen input** — never fit-at-regen.
- **Cost-model accuracy is itself unreliable**: Ithemal (PMLR v97, 2019; arXiv 1808.07412) shows LLVM's
  analytical **llvm-mca is off by 2×+ the error** of a learned model on some patterns because it doesn't
  model certain microarch effects; uiCA (arXiv 2107.14210) and AnICA further document analyzer
  inconsistency. Lesson for 2D: **a mis-fit cost model will "select" a refuted lever back into life.**
  Our SPEC §0 is a graveyard of levers a naive throughput model WOULD pick (NEON digit-scan: −37/−48%
  instructions but CPI inverts 0.13→0.30; block-scheduling: fewer-cycles illusion). An
  instruction-count objective is actively dangerous here.

---

## 5. Constraints OUR evidence base imposes (the negative space the families must fit)

These are not soft preferences — they are certified numbers and refutation LAWS (constitution iv,
SPEC §0) that a graph-engine design violates at rejection:

1. **The cost objective may NOT be instruction-count.** SPEC §0 refutation 02 (digit-scan SIMD NO_GO)
   and 04 (block-scheduling refuted) both show **fewer instructions that REGRESS the product lane**
   (CPI/latency inversion; OoO already pipelines). A cost model whose objective is ins/B would
   re-select both. The lawful objective is **paired cold cyc/B**, with **ins/B only as the
   load-invariant CHECK** (PROVE-12 §4; GN-2 §1.2 precept 43–49). 2D's fit MUST target cyc/B-under-load
   or paired ratios, never warm/ins-only.
2. **Warm benches are disinformation** (SPEC §0; constitution iv). Any 2D coefficient fitted from warm
   or cached measurement voids the design — cold per-parse only (memory: no-warm-benches).
3. **Determinism / reproducible regen is a hard invariant** (clean-regen-discipline;
   changed-flag-convergence). This forbids: wall-clock-timeout saturation (§1), fit-at-regen ML (§4),
   and any nondeterministic tie-break in extraction. The Cranelift-ægraph bounded-eager shape (§1) and
   MLGO's frozen-table shape (§4) are the two production patterns that satisfy it.
4. **Consumer-less substrate is forbidden (Y.13, constitution viii).** Every e-graph "island" must have
   a **same-wave consumer + a cost model** — 2A states this as its own law, and GN-2 §1.2 (precepts
   8/37/40; the master pathology "substrate-without-consumer, 14/18 dead") makes it the governing rule
   of the kernel plane. An e-graph inserted "for future use" is a rejected design, not a deferred one
   (no-deferrals).
5. **Generated-size budget is a real global constraint** (memory: generated-size-budget; PROVE-12 +18
   LOC, GN-2 per-wave budgets). This is the one constraint that is genuinely non-local → it is exactly
   what pushes 2C toward a bounded post-solve rather than greedy extraction (§2).
6. **PROVE-12 residualization is a proven, GO lowering** — const-fold at GENERATION time via
   const-generics + monomorphization, "generalizes to any grammar-derived delimiter/STOP-set scan." The
   graph engine's job w.r.t. residualization is to DECIDE per-site depth (a candidate/selection
   problem), not to invent the mechanism. LOCK-3D ("staged-residual; PROVE-12 becomes a corollary")
   and 2C ("residualization depth" as a CSP variable) are the two homes; the graph engine supplies the
   selection.

---

## 6. Traps named in comparable systems (checklist for pass-3 synthesis)

- **T1 — Global cost + greedy extraction = silent suboptimality / cycles.** If any island's objective is
  non-local (sharing, generated-size budget), the existing greedy `Extractor` is wrong. Fix per prior
  art: keep island costs local (greedy stays valid) OR adopt the e-boost/Tensat/ROVER split — e-graph
  generates, a **bounded** exact solve (PBQP-sparse, not full ILP) selects. Do NOT extend greedy to a
  global objective and call it optimal.
- **T2 — Unbounded saturation breaks determinism + blows up.** egg's termination is not guaranteed;
  timeouts are nondeterministic. Prefer ægraph-style bounded eager rewrite / a fixed deterministic
  saturation budget. This is the primary risk of the literal 2B ("IR is an e-graph end to end").
- **T3 — CSP scaling wall.** Unison is optimal only to ~1000 vars; a monolithic whole-program CSP (naive
  2C) will not scale. Encode as sparse PBQP (per-site cost-vectors + legality edge-matrices) and bound
  the variable count per island; use the solver as a **bounded post-solve** (converges with LOCK-1A/1B).
- **T4 — Cost-model mis-fit re-selects refuted levers.** llvm-mca/analytical inaccuracy is documented;
  our §0 refutations are precisely the levers a throughput model would wrongly pick. 2D's objective must
  be the certified metric (paired cold cyc/B), the fit offline, the table committed — MLGO-shaped.
- **T5 — Consumer-less island.** Y.13 / no-deferrals: an e-graph decision point with no same-wave
  consumer and cost model is dead substrate. Every family must name the consumer.
- **T6 — Two cost-model homes drift.** `cost_weights.rs` already centralizes shared knobs to defeat this
  (memory: system-cohesion; the doc-comment explicitly forbids splitting weights across domains). Any
  new cost dimension (kernel selection, capacity plan) lands in the ONE `CostWeights` struct, not a
  parallel table — else the grammar tier and regex tier diverge silently.
- **T7 — E-graph/CSP as orthogonal subsystems.** Memory laws (no-orthogonal-codepaths, system-cohesion,
  general-infra-crates) + the in-tree `CspScheduler` (e-graph saturation DRIVEN by the CSP solver)
  already fuse the two. The trap is a design that runs e-graph and CSP as separate passes; the
  prior-art-and-in-tree answer is one substrate — CSP schedules saturation, CSP/PBQP selects over
  extracted candidates, one `CostWeights` feeds both.

---

## 7. Verdict for the families (prior-art lens only)

- **2A (EGRAPH-ISLANDS)** — best supported by production prior art (Cranelift ægraph is islands-in-a-CFG
  with per-node extraction) AND by the in-tree seed (islands already at regex-HIR-canon, backend-shape).
  Lowest determinism/blowup risk. Its stated same-wave-consumer + cost-model law is the Y.13 discipline.
- **2B (IR-IS-EGRAPH)** — the ambitious literal; inherits T2 (termination/blowup) and must defeat them
  with a deterministic budget. Only sound in the bounded-eager (ægraph) reading, not the unbounded-egg
  reading.
- **2C (CSP-CENTRAL)** — well-founded IF encoded as sparse **PBQP + bounded post-solve** (Unison/e-boost
  precedent), with e-graphs as candidate generators and the generated-size budget as the global
  constraint. Full-graph ILP (Tensat/ROVER) does not scale; avoid.
- **2D (PROFILE-CLOSED-LOOP)** — MLGO-validated ONLY as offline-fit + committed-fact-table + regen-input,
  objective = paired cold cyc/B. Fit-at-regen or ins-count objective is refuted by §0 before it starts.

The four families are not exclusive: prior art points at a **composite** — ægraph-style bounded islands
(2A) that generate candidates, a sparse PBQP/COP bounded post-solve (2C) over a global generated-size
budget, fed by an MLGO-shaped offline-fitted committed cost table (2D), with 2B's "one substrate" as the
cohesion constraint (T7) rather than a literal whole-IR saturation. This mirrors LOCK-1's own convergence
(unification-first + CSP-only-where-undecidable + bounded post-solve).

---

## Sources

- egg: Fast and Extensible Equality Saturation — arXiv 2004.03082 / CACM 3434304.
- Equality Saturation for Tensor Graph Superoptimization (Tensat) — arXiv 2101.01332 / MLSys 2021; uwplse/tensat.
- e-boost: Boosted E-Graph Extraction with Adaptive Heuristics and Exact Solving — arXiv 2508.13020.
- Cornell CS6120 2025sp — Technology Mapping with Egraphs (extraction NP-hardness; ROVER ILP 63% area).
- Chris Fallin — ægraphs: Acyclic E-graphs for Efficient Optimization in a Production Compiler, EGRAPHS 2023 (cfallin.org/pubs/egraphs2023_aegraphs_slides.pdf); blog cfallin.org/blog/2026/04/09/aegraph; bytecodealliance/rfcs #27 (accepted/cranelift-egraph.md).
- Sketch-Guided / Guided Equality Saturation — arXiv 2111.13040; ACM 3632900 (termination, blowup).
- Combinatorial Register Allocation and Instruction Scheduling (Unison) — Lozano et al., arXiv 1804.02452 / TOPLAS 3332373; unison-code.github.io.
- Partitioned Boolean Quadratic Problem (PBQP) — complang.tuwien.ac.at/scholz/pbqp; SCOPES'03 SSA-graph instruction selection; SSA-based register allocation with PBQP.
- MLGO: a Machine Learning Guided Compiler Optimizations Framework — Trofin et al., arXiv 2101.04808; llvm.org/docs/MLGO.html.
- Ithemal: Accurate, Portable and Fast Basic Block Throughput Estimation — Mendis et al., arXiv 1808.07412 / PMLR v97; uiCA arXiv 2107.14210 (cost-model inaccuracy).
- In-tree: `crates/egraph/{extract,csp_scheduler,cost_weights,scheduler}.rs`, `crates/csp-solver/src/solver/optimize.rs`, `ir/src/cost.rs`, PROVE-12 REPORT.md, R1 cartography §2/§8, SPEC §0 refutation laws, GN-2 §1.2–1.4.
