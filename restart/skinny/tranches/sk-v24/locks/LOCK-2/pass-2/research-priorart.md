# LOCK-2 — THE GRAPH ENGINE — PASS-2 PRIOR-ART RESEARCH

*Prior art for the problem class: e-graph canonicalization/extraction, CSP selection over
per-site lowering choices, profile-fed cost calibration, deterministic regen. What exists in
tree; what won and lost elsewhere; the constraints our own evidence base already imposes; the
named traps. Sweep is web + codebase, no conclusion presumed — the families own the verdict.*

Evidence standard (CONSTITUTION vii, XL-H): every claim is a concrete artifact (in-tree file
`path:line`, or a cited paper) or is labeled SPEC-ONLY. HEAD named on every measurement.

---

## 0. What already exists IN TREE (the seed engines — measure before you build)

The graph engine is not a greenfield. Both crates are vendored, wired, and tuned:

- **`crates/egraph/`** — a hand-rolled egg-shape engine (13 modules):
  - `extract.rs:1-11` — **greedy bottom-up extraction, explicitly the "egg greedy bottom-up
    algorithm."** Its own header states the limitation verbatim: *"not optimal for all cost
    models (e.g., cost models that depend on sharing require a more sophisticated
    extractor), but it works for the monotone cost models used by grammar/regex
    optimization."* This is the crux fact for family 2A/2B: the in-tree extractor is
    already the tree-cost greedy, and it already declares where it is optimal.
  - `extract.rs:35+` `trait Lattice` — a **multi-objective / Pareto scaffold already
    present** ("future multi-objective extraction can reason about dominance without
    collapsing to a scalar first"). The scalar case is `Scalar<T>`; Pareto fronts are a
    landing seam, not a research gap.
  - `csp_scheduler.rs:1-24` — **the CSP-backed rewrite scheduler already exists**: e-classes
    are CSP variables carrying a monotone `DirtyDomain` lattice, parent edges are
    `ParentDirtyProp` constraints, one `csp.propagate()` computes the transitive dirty
    closure. Explicitly *"isomorphic to the grammar tier's existing csp-solver usage."* The
    "CSP drives saturation" idea (a face of 2C) is BUILT.
  - `scheduler.rs:14-48` `RunReport` — iteration caps, per-rule match limits, **growth
    limits on total e-graph size** (`growth_limit_hit`), saturation fixpoint detection. The
    divergence guard the termination literature demands (§3 below) is already wired.
  - `cost_config.rs` — one shared `CostConfig` per compile: cross-tier `CostWeights`,
    scheduler caps, and a rayon parallelism threshold gated on grammar size (the
    `LockLatch::wait_and_reset` overhead measured at ~25% of `compile_bbnf` on small
    grammars). `@cost` grammar directive named as a future landing on the same struct.
  - `analysis.rs` monotone-lattice `Analysis`; `rewrite.rs` imperative (non-pattern) rules;
    `egraph.rs`/`unionfind.rs`/`eclass.rs` the union-find core.
- **`crates/csp-solver/`** (vendored csc411 @02965a56) — `solver/search.rs` a **unified
  monomorphized branch-and-bound kernel** (one `search` skeleton, ZST `SearchPolicy`:
  `Feasibility` | `BranchBound`, optimistic-bound cutoff, cost-sorted value order, Trail-based
  undo); `solver/optimize.rs` `DomainCostEval` cost abstraction; `domain/cost_finite.rs` +
  `domain/lattice.rs` cost/lattice domains; `builder/kuhn_munkres.rs` Hungarian assignment;
  `solver/gac/` GAC. **A discrete-choice cost-minimizing solver is present and tuned** — the
  2C "decision driver IS a CSP" substrate is largely instantiated.

**Consequence for every family:** the pass-2 question is not "build an e-graph / build a CSP"
— it is *at which named decision point does either engine earn its consumer* (XL-B, Y.13), and
*does the tree-cost greedy already suffice where saturated multiplicity ≈ 1* (directive Q1).

---

## 1. Extraction as optimization — the settled external landscape (Q1, Q3, Q6)

**Extraction is NP-hard, and the reason is DAG sharing, not tree depth.** Greedy bottom-up
gives the optimal tree cost, but "the presence of common subexpressions breaks this
assumption" — local choices change the reusability/cost of shared subterms, so tree-cost
intuition does not transfer to exact DAG extraction (egg / SPORES / EGRAPHS lineage). The
in-tree `extract.rs` header states exactly this boundary. **The decisive question is therefore
empirical (directive Q1): if after saturation the average e-class has multiplicity ≈ 1 and the
few multi-survivor classes are childless (no shared-subexpr contention), greedy bottom-up IS
optimal and extraction-as-decision (ILP/PBQP) is over-engineering** — this is measurable on the
fleet and must be measured, per PROVE-12's own shape (below).

The external toolbox, ranked by what shipped and what it cost:

| Approach | Optimality | Cost / scaling | Cite |
|---|---|---|---|
| **Greedy bottom-up** (in-tree today) | optimal iff monotone tree cost, no shared-subexpr contention | fast, linear-ish; scales to large e-graphs | egg (arXiv 2004.03082); in-tree `extract.rs` |
| **ILP** | exact/optimal | "scales poorly… prohibitively expensive," the classic bottleneck | egg §; e-boost |
| **Treewidth-parameterized** (weighted-MaxSAT on acyclic monotone circuits) | exact | `2^O(w²)·poly(w,n)` — tractable only for low-treewidth e-graphs | *E-Graphs as Circuits, and Optimal Extraction via Treewidth*, arXiv 2408.17042 |
| **ASP (Answer Set Programming)** | exact + nonmonotone choice/constraints | Datalog-fixpoint search space + ASP for the choice; competitive with ILP | arXiv 2606.10644 |
| **SmoothE** (differentiable relaxation) | near-optimal, GPU | ASPLOS 2025; the prior SOTA on quality | csl.cornell.edu smoothe_asplos2025 |
| **e-boost** (parallel heuristic + adaptive pruning + **warm-started ILP**) | near-exact | **558× over ILP, +19.04% over SmoothE**; ICCAD 2025 | arXiv 2508.13020 |

**What this says for us:** nobody at a **per-parse ~230 µs compile budget** (directive Q3, our
PROVE-08 number) reaches for raw ILP — it is the named scaling cliff. The shipping-fast answers
are (a) greedy where multiplicity is low, (b) e-boost's pattern (fast heuristic + *reduced*
exact solve with warm start) only where a genuine multi-survivor contention exists, and (c)
treewidth extraction only if the fleet e-graphs are provably low-treewidth. Our vendored B&B
kernel is the natural "reduced exact solve" home; it does not need ILP.

**PROVE-12 is the in-tree ground truth on multiplicity.** The residualization prover
(`prototypes/PROVE-12/REPORT.md`) did NOT do multi-survivor extraction: it const-folded the CSS
delimiter set to a `u128` bitmask and **monomorphized** — one canonical folded form per site,
multiplicity 1, byte-identical parity, +18 generated LOC. The win was *straight-line residual
via const-eval + beta-reduction*, not *choosing among survivors*. This is direct evidence that
at least one certified graph-opt win on this substrate lives at multiplicity 1, where greedy /
canonicalization suffices and extraction-as-search buys nothing.

---

## 2. PBQP — the register-allocation lineage is the exact match for a near-tree decision graph (Q6)

Directive Q6 asks whether **PBQP** (per-node discrete choice, pairwise cost matrices) is the
right solver for a near-tree decision graph — independent per-site lowering choices + budget
coupling + few coherence edges — vs generic B&B or e-graph extraction. The prior art is
unusually clean here:

- **PBQP = Partitioned Boolean Quadratic Problem**, a QAP specialization, introduced by
  **Scholz & Eckstein 2002** for register allocation, then **instruction selection on SSA
  graphs** (Eckstein/Scholz SCOPES 2003, `b-scholz.github.io/publications/scopes03.pdf`). The
  model is *exactly* "multiple interdependent discrete choices, each with a per-choice cost
  vector and pairwise (edge) cost matrices" — a per-site lowering choice with legality/coherence
  edges is a textbook PBQP instance.
- **NP-complete in general, but the solver runs a linear-time heuristic** via graph reduction
  rules (R0/R1/R2 degree elimination); only irreducible nodes fall to enumeration. **Hames &
  Scholz, *Nearly Optimal Register Allocation with PBQP* (2006)**: a B&B PBQP solver solved
  **97.4% of SPEC2000 functions optimally**. **Buchwald & Zwinkau, *SSA-Based Register
  Allocation with PBQP* (CC 2011)** productionized it (libFirm). LLVM ships a PBQP allocator.
- **Fit to our graph class:** where sites are largely independent (the "near-tree" the
  directive posits), most PBQP nodes are R0/R1-reducible → the solve is effectively linear, and
  the "few coherence edges" are the only quadratic terms. This is the regime where PBQP
  **dominates** both generic B&B (no structural reduction) and e-graph extraction (which models
  equivalence, not pairwise choice-coupling). The budget knapsack (directive Q7) is a *global*
  constraint, not pairwise — PBQP handles per-pair coupling but a cardinality/knapsack budget is
  awkward as pairwise cost; that seam wants the CSP store, not PBQP edges. **The honest read:
  PBQP for the per-site coherence, the csp-solver global store for the budget — and our
  `solver/search.rs` B&B already spans both if the coherence graph is small.**

**Trap named by this lineage:** PBQP's optimality guarantee is *per the cost model*; a
mis-specified pairwise matrix silently ships a wrong-but-optimal solution. The RA literature
pairs it with a spill-cost oracle; our analogue is the profile-fed cost (§4) — the solver is
only as good as the calibrated coefficients.

**Counterpoint — the shipping alternative that is NOT a solver at all: Cranelift ISLE.**
Cranelift lowers CLIF→MachInst with **ISLE, a statically-typed term-rewriting DSL** whose rules
compile to a **single-pass match-tree decision** with **user-configurable per-rule priorities**
— "as good as or better than hand-written," merging all rules, sharing work
(cfallin.org/blog/2023/01/20). It deliberately chose **priority-ordered rewrite → decision
tree, NOT equality saturation and NOT a cost solver**, for instruction selection. Cranelift's
mid-end is an **acyclic e-graph (ægraph)** — a middle path: e-graph for the machine-independent
rewrites, priority-tree for lowering. This is load-bearing for family adjudication: **a
production compiler with our exact problem shape (per-site lowering choice) chose a compiled
priority decision tree over a cost solver for lowering, reserving the e-graph for
canonicalization only.** 2A (islands at named points) has a shipping precedent; 2B (IR-is-egraph
end to end) does not, in this domain.

---

## 3. Widening rewrites, termination, confluence (Q2)

Directive Q2 asks what establishes confluence + termination + semantics-preservation for a
rewrite that *manufactures/widens* rather than narrows, coexisting with an all-narrowing set.

- **EqSat can diverge even for a terminating (even canonical) term-rewriting system** — the
  associativity rule is the canonical counterexample. *Ensuring the Termination of Equality
  Saturation for Terminating Term Rewriting Systems* (EGRAPHS 2023, effect.systems) is the
  reference result: termination of the TRS does **not** imply termination of EqSat.
- **egg's saturation itself terminates by construction only for rebuild**: the rebuild
  invariant restores congruence and terminates because `(|I|,|W|)` decreases lexicographically
  (egg, arXiv 2004.03082) — but that is congruence maintenance, not rule-set fixpoint. A
  widening rule (e.g. `x → x+0`, distributivity expansion) has no such measure and **must be
  bounded by the scheduler**, which is exactly what in-tree `scheduler.rs` `RunReport` does
  (iter_limit, node growth limit). **The termination story for widening is: no confluence
  proof, a saturation cap.** This is the shipped answer everywhere (egg, egglog, SPORES).
- **Confluence of non-terminating TRS** is separately studiable — *A Reduction-Preserving
  Completion for Proving Confluence of Non-Terminating Term Rewriting Systems* (LMCS #667) — but
  it is a proof technique, not a runtime guarantee, and is overkill for a bounded-saturation
  engine. Semantics-preservation is the tractable obligation: each rewrite is an equational
  axiom sound w.r.t. the language's denotation (*Semantic Foundations of Equality Saturation*,
  arXiv 2501.02413), verified by a parity oracle on extracted terms — which is exactly PROVE-12's
  XOR-fold checksum method.

**Verdict for families:** widening+narrowing coexist safely in shipping engines **only under a
node/iteration cap + a post-hoc parity oracle**, never a confluence proof. Our engine already
has the cap (`growth_limit_hit`) and the oracle pattern (PROVE-12 cksum). A family that proposes
widening rules must (a) declare the cap, (b) declare the parity oracle, and (c) show the widening
*earns* a narrowing that the all-narrowing set could not reach — else it is dead surface (Y.13).

---

## 4. Profile-fed cost calibration — the AutoFDO/SPGO pattern + our determinism law (Q8)

Directive Q8: can a coefficient fitted from measured cyc/B flip a real per-site decision, win
cold-paired, and drive a freshness gate RED on mutation — with regen determinism preserved and
env-source fenced?

- **The shipping pattern is Sample-PGO / AutoFDO**: profile a release binary via **PMU sample
  counters** (no instrumented build), then feed the profile back as a committed input. Modern
  PMUs sample HW events at low overhead; **"static sampling is deterministic, sampling points
  known at compile time"**; **"profile data can be archived with version information for
  reproducibility"** (survey + PGO literature). BOLT/Propeller do basic-block reordering off the
  same PMU profiles. This is precisely our SPEC §0 protocol (per-process `ri_cycles`, cold
  paired, N≥200) turned into a *committed fact table*.
- **The determinism law is ours and it is strict** (directive Q3, XL-H, memory
  `changed-flag-convergence` + `clean-regen-discipline`): `regen∘regen` must be byte-identical.
  Therefore **(a) the fitted coefficients commit as a versioned fact table** (the AutoFDO
  archived-profile pattern), **(b) the fit runs OUT of band and env-source is fenced from regen**
  — a `BBNF_COST_*` env var may steer a *bench*, never a committed regen (cf. `cost_config.rs`
  `from_env`, which is explicitly benchmarking-only), and **(c) extraction tie-break must be a
  total order, NOT HashMap iteration.** This last is a **named in-tree trap**: `extract.rs` and
  `csp_scheduler.rs` both use `FxHashMap`/`FxHashSet`; iteration order over those is
  deterministic for a fixed insertion sequence but is NOT a semantic total order — any extraction
  that breaks a cost tie by "first in map" risks a regen diff if insertion order shifts. Directive
  Q3's "HashMap iteration inadmissible" is correct and the fix is a total-order tie-break (e.g.
  by `Id` then structural key), the same discipline LLVM's PBQP uses (deterministic node order).
- **A/B differential attribution** (directive Q8's underdetermination worry): whole-grammar cyc/B
  cannot separate per-feature coefficients — the answer in the autotuning/PGO literature is
  paired differential measurement (change one feature, hold the rest), which is exactly the
  PROVE fleet's cold-paired interleave. The refutation law bites here: **warm/cached calibration
  is disinformation (SPEC §0); a coefficient fitted from a warm bench is inadmissible.**

---

## 5. Constraints our own evidence base imposes (binding, measured)

- **~230 µs per-parse compile budget** (PROVE-08 HM core) — the whole build+saturate+extract+
  **teardown** organ must fit (directive Q3). This alone rules out raw ILP and bounds saturation
  iterations; it is why the scheduler caps exist.
- **`regen∘regen` byte-identity** (GN-1 gate, standing law) — extraction must be deterministic;
  the FxHashMap tie-break trap above is real.
- **XL-B (tower ownership):** engines operate *within* the middle-IR tower at *named decision
  points*; the tower shape is not ours to redraw. This is measured pressure **against 2B
  (IR-is-egraph end to end)** and **for 2A (islands)** — and Cranelift's ægraph choice (§2) is
  the external corroboration.
- **XL-C (regex axis):** grammar tier keeps `Regex(StringId)` opaque; rich `Hir` + cheap bridge
  at the engine tier; canon home = parse-that. Any regex-HIR canonicalization island lives at the
  engine tier, not the grammar tier.
- **SPEC §0 refutation laws** — warm benches disinformation (cost calibration must be cold
  paired); x86 dead; no stable-port perf claims; no fabricated numbers. A cost model calibrated
  off anything but cold-paired PMU is void.
- **Multiplicity ≈ 1 evidence:** PROVE-12's certified win was const-fold + monomorphization at
  multiplicity 1 — direct in-tree evidence that extraction-as-search is not yet earned on the
  substrate; the fleet-wide multiplicity census (directive Q1) is the gating measurement before
  any extraction-solver family is built.

---

## 6. The traps, named (from comparable systems + our evidence)

1. **ILP scaling cliff** — optimal but "prohibitively expensive" (egg, e-boost). Never at a
   230 µs budget. If exact solving is needed, it is e-boost-style *reduced + warm-started*, on
   our B&B kernel, not raw ILP.
2. **Greedy suboptimality is a shared-subexpression phenomenon only** — it does not bite at
   multiplicity 1. Building an extraction solver before measuring multiplicity is
   consumer-less substrate (Y.13). Measure first (Q1).
3. **EqSat divergence under widening/associativity** — terminating TRS ≠ terminating EqSat
   (EGRAPHS 2023). The only shipped guard is a node/iter cap (present in `scheduler.rs`), plus a
   parity oracle. No confluence proof is owed or achievable.
4. **Non-deterministic extraction from hash iteration** — the in-tree `FxHashMap` tie-break is a
   latent regen-diff hazard; a total-order tie-break is mandatory (Q3). LLVM PBQP's determinism
   discipline is the reference.
5. **Profile env-source leaking into regen** — a `BBNF_COST_*` env fit that reaches a committed
   file breaks byte-identity. Fence env to benches; commit coefficients as a versioned fact table
   (AutoFDO pattern).
6. **PBQP with a mis-specified cost matrix** — optimal-but-wrong. The RA lineage pairs PBQP with
   a spill-cost oracle; our analogue is the cold-paired profile fit — and a global budget knapsack
   does NOT belong in pairwise PBQP edges (put it in the CSP store, Q7).
7. **Over-reaching to "IR-is-egraph end to end" (2B)** — no shipping compiler in this domain does
   it; Cranelift explicitly split canonicalization (ægraph) from lowering (ISLE priority tree).
   XL-B independently forbids redrawing the tower. The precedent and the constitution agree.

---

## Sources

- egg: Fast and Extensible Equality Saturation — https://arxiv.org/pdf/2004.03082 ; ACM https://dl.acm.org/doi/pdf/10.1145/3434304
- e-boost: Boosted E-Graph Extraction with Adaptive Heuristics and Exact Solving (ICCAD 2025) — https://arxiv.org/abs/2508.13020 ; https://www.csl.cornell.edu/~zhiruz/pdfs/eboost-iccad2025.pdf
- SmoothE: Differentiable E-Graph Extraction (ASPLOS 2025) — https://www.csl.cornell.edu/~yc2632/data/smoothe_asplos2025_final.pdf
- Answer Set Programming for Egg Extraction — https://arxiv.org/pdf/2606.10644
- E-Graphs as Circuits, and Optimal Extraction via Treewidth — https://arxiv.org/pdf/2408.17042
- SPORES: Sum-Product Optimization via Relational Equality Saturation — https://arxiv.org/pdf/2002.07951
- SSA-Based Register Allocation with PBQP (Buchwald & Zwinkau, CC 2011) — https://pp.ipd.kit.edu/uploads/publikationen/buchwald11cc.pdf
- Nearly Optimal Register Allocation with PBQP (Hames & Scholz 2006) — https://link.springer.com/chapter/10.1007/11860990_21
- Code Instruction Selection based on SSA-Graphs (Eckstein/Scholz, SCOPES 2003) — http://b-scholz.github.io/publications/scopes03.pdf
- Survey on Instruction Selection — https://arxiv.org/pdf/1306.4898
- Cranelift's Instruction Selector DSL, ISLE — https://cfallin.org/blog/2023/01/20/cranelift-isle/ ; language ref https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/isle/docs/language-reference.md
- Ensuring the Termination of Equality Saturation for Terminating TRS (EGRAPHS 2023) — https://effect.systems/doc/egraphs-2023-theory/paper.pdf
- Semantic Foundations of Equality Saturation — https://arxiv.org/pdf/2501.02413
- A Reduction-Preserving Completion for Confluence of Non-Terminating TRS (LMCS) — https://lmcs.episciences.org/667
- Profile-Guided Optimization / Sample-PGO survey — https://arxiv.org/html/2507.16649v1 ; Improved Basic Block Reordering (BOLT) — https://arxiv.org/pdf/1809.04676
- A Survey on Compiler Autotuning using Machine Learning — https://arxiv.org/pdf/1801.04405

### In-tree artifacts cited
- `crates/egraph/src/extract.rs:1-11,35+` (greedy bottom-up + Lattice/Pareto scaffold)
- `crates/egraph/src/csp_scheduler.rs:1-24` (CSP-backed saturation scheduler)
- `crates/egraph/src/scheduler.rs:14-48` (RunReport caps: iter/node/growth limits)
- `crates/egraph/src/cost_config.rs` (shared CostConfig, `from_env` bench-only)
- `crates/csp-solver/src/solver/search.rs` (unified B&B kernel), `solver/optimize.rs`, `domain/cost_finite.rs`, `builder/kuhn_munkres.rs`
- `restart/skinny/tranches/sk-v24/prototypes/PROVE-12/REPORT.md` (residualization: const-fold + monomorphize, multiplicity 1, +18 gen LOC, byte-identical parity)
- SPEC §0 refutation laws; PROVE-08 ~230 µs compile budget; R1 cartography `ir/src/cost.rs`, `ir/src/lib.rs:361` BackendShape, `passes/src/lib.rs:398` derive_backend_shape
