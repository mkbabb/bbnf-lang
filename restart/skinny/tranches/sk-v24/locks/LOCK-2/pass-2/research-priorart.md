# LOCK-2 — THE GRAPH ENGINE — PRIOR-ART (pass 2)

Prior art for *where and how graph optimization governs the generator pipeline*: e-graph
islands vs. whole-IR saturation, CSP/ILP for per-site lowering choice, profile-fitted cost
models, and deterministic regen. Adjudicated against the SK-V24 evidence base (SPEC §0
certified numbers + refutation laws, GN-2 spec, R1 cartography) and the in-tree seeds. No
family allegiance; this is what the world has already settled for THIS problem class.

---

## 0. What already exists in-tree (the seed reality)

The lock is NOT greenfield. `crates/egraph/` (1885 LOC) already ships the machinery the four
families argue over:

- **Greedy bottom-up extraction** (`extract.rs`): "the Egg greedy bottom-up algorithm … not
  optimal for all cost models (e.g., cost models that depend on sharing require a more
  sophisticated extractor)." The header *already names the deep trap* (§4). A `Lattice`/`Scalar`
  cost trait anticipates Pareto/multi-objective without churn (`extract.rs:36-60`).
- **A CSP-backed rewrite scheduler** (`csp_scheduler.rs`, 368 LOC): each e-class is a CSP
  variable with a monotone `DirtyDomain` lattice, each parent edge a `ParentDirtyProp`
  constraint; one `csp.propagate()` computes the transitive dirty closure — the in-tree answer
  to e-graph blowup / scheduling, "isomorphic to the grammar tier's `csp-solver` usage in
  `passes/csp_domains.rs`."
- **`csp-solver/`** (6987 LOC): GAC solver (`solver/gac/`), Kuhn–Munkres assignment
  (`builder/kuhn_munkres.rs`, 265 LOC), `CostFinite`/lattice domains (`domain/cost_finite.rs`,
  `domain/lattice.rs`) — i.e. an *assignment-with-cost* engine already exists, which is exactly
  the PBQP/CSP substrate 2C needs (§3).
- **The cost seam** (R1 §8.3): `ir/src/cost.rs` `ActiveCostFacts` already records egraph/CSP
  selection provenance (`selected_shape`, rewrite counts, cost-freshness); `DecisionCspFacts`,
  `PriorityStep` (8-step). `BackendShape` (5 variants) is the extraction target. **The engine is
  wired but under-consumed** — the Y.13 pathology (substrate without consumer) is the standing
  risk, not absence of substrate.

PROVE-12 is the one *landed, measured* graph-engine win: generation-time const-fold +
beta-reduction of the CSS delimiter test (`css_canon_bench` 0.81–0.98× cyc/B, +18 gen LOC,
byte-identical). It is residualization-as-monomorphization, not saturation — a data point that
**cheap, directed, generation-time rewrites already pay; the open question is whether a heavier
engine pays MORE.**

---

## 1. The dominant production analog — Cranelift's acyclic e-graph (bears on 2A vs 2B)

Chris Fallin, *The acyclic e-graph: Cranelift's mid-end optimizer* (2026-04-09) and *ægraphs*
(EGRAPHS 2023). The single most relevant settled system: a **production compiler that chose
bounded e-graph islands over full equality saturation**, and measured why.

- **Motivation = the pass-ordering disease** e-graphs cure: GVN, LICM, rematerialization fall
  out "for free" during translation to/from the e-graph; no separate machine-independent passes.
- **Eager rewrites at construction ("cascades", à la database query optimizers)**: a rule's RHS
  is itself rewritten before its uses are established — single-pass, exploiting SSA
  def-before-use. No batched rebuild loop, acyclic by construction.
- **Extraction = DP bottom-up in topological order, ignoring shared substructure** — the same
  tree-cost simplification our `extract.rs` uses, chosen precisely to dodge NP-hardness (§4).
- **The load-bearing measurement**: average e-class size **1.13 nodes**; multi-representation
  "may not (yet?) be pulling its weight." Whole-graph saturation buys **~0.1% runtime** at
  **~7–8% compile-time**. Suboptimal eager-elaboration events: "approximately twice across
  millions of nodes."
- **Ruleset discipline**: *deliberately excludes* associativity/commutativity identities "that
  do not simplify" — directional rewrites only, to keep the graph from exploding.

**Verdict for LOCK-2**: for a *generation-time* engine under a generated-LOC budget, Cranelift is
direct evidence that **2A (directed pipeline, small e-graph islands, eager directional rewrites,
cost-model extraction at named decision points) is the proven shape, and 2B (whole IR as a
saturating e-graph) must clear a Cranelift-style measurement bar or it is over-investment.** Cite:
cfallin.org/blog/2026/04/09/aegraph/; pldi23 ægraphs.

---

## 2. Whole-IR saturation — egg / egglog (bears on 2B)

- **egg** (Willsey et al., POPL 2021, arXiv 2004.03082): the *rebuilding* technique —
  amortized invariant restoration exploiting saturation's batch workload, asymptotic speedups.
  Our `csp_scheduler.rs` dirty-closure is a CSP re-cast of egg's `BackoffScheduler`.
- **egglog** ("Better Together: Unifying Datalog and Equality Saturation", Zhang et al., PLDI
  2023): e-graph = Datalog with congruence; **semi-naïve evaluation** gives determinism +
  incrementality; lattice-based analyses cooperate. Explores a larger space than egg at less
  time. This is the strongest 2B-shaped substrate, and it *already matches our lattice idiom*
  (the csp-solver `LatticeDomain` shape).
- **Counterweight**: Cranelift (§1) is a production refutation of "saturate everything." 2B is
  defensible ONLY where cross-island equalities genuinely interact and the compile-budget is
  paid — which the SPEC's generated-size budget (precept 53) + regen-determinism gate makes an
  *empirical* question, not an architectural default.

---

## 3. CSP / ILP as the decision driver (bears on 2C — the strongest external match)

The lowering-choice problem 2C names — "variables are per-site lowering choices (kernel,
capacity, prefilter tier, residualization depth), constraints are legality + the generated-size
budget, objective a cost lattice fed by PMU facts" — is **literally the optimal instruction-
selection problem**, which the compiler world has settled:

- **Instruction selection on DAGs is NP-complete** (tree covering is linear-time DP; DAGs and
  full graphs are NP-complete) — Wikipedia *Instruction selection*; Koes & Goldstein, *Near-
  Optimal Instruction Selection on DAGs* (CGO 2008).
- **PBQP is the proven engine** for exactly 2C's shape: Eckstein/Scholz/Ebner, *Generalized
  Instruction Selection using SSA-Graphs* (LCTES 2008) and *Code Instruction Selection based on
  SSA-Graphs* (SCOPES 2003) reduce optimal covering under legality + cost to **Partitioned
  Boolean Quadratic Programming** — a linear-time solver yields provably optimal solutions for
  "almost all" SSA-graph nodes, heuristic fallback for a negligible remainder. This is the
  template: per-site choice variables, pairwise legality/cost matrices, near-optimal in practice.
- **Combinatorial register allocation + instruction scheduling** (Castañeda Lozano & Schulte,
  ACM CSUR 2018): full constraint-programming formulation of joint back-end decisions — evidence
  that CP scales to real back-ends.
- **In-tree fit**: `csp-solver`'s Kuhn–Munkres (`builder/kuhn_munkres.rs`) + `CostFinite`
  domains are already an assignment-with-cost engine; 2C's PBQP-shaped driver is a *specialization
  of machinery that exists*, not new infrastructure. The `generated_loc_budget` is already a hard
  constraint in the SPEC (GN-2 ledger) — it *is* a CSP constraint, not a metaphor.

**Verdict**: 2C has the deepest external precedent and the readiest in-tree substrate. Its sharp
claim — "e-graphs only generate candidates; the CSP decides" — is the PBQP posture, and it
**dissolves the extraction NP-hardness (§4) by making DAG-cost the CSP objective rather than a
separate greedy pass.**

---

## 4. THE deep trap — extraction under shared substructure (cross-cutting, binds every family)

The universally-reported pitfall, and the one our own `extract.rs` header confesses:

- **Greedy bottom-up (tree cost) double-counts shared subexpressions.** "The presence of common
  subexpressions breaks the assumption that the best plan for any expression contains the best
  plan for its subexpressions." DAG cost (count each subexpr once) is the correct model but
  **DAG-optimal extraction is NP-hard and inapproximable within any constant ratio.**
- **The settled solution menu** (choose per island by sparsity/budget):
  - **ILP** — optimal, scales poorly (the baseline everyone beats).
  - **Fast & Optimal Extraction for Sparse Equality Graphs** (Sun/Parreaux et al., OOPSLA 2024):
    parameterized by **treewidth ≤ 10**, optimally extracts in a fraction of ILP time — the
    "our e-graphs are sparse/tree-like" escape hatch.
  - **e-boost** (Cornell, ICCAD 2025): adaptive heuristics + exact solving hybrid.
  - **SmoothE** (Cornell, ASPLOS 2025): differentiable relaxation of extraction.
  - **ASP for E-graph DAG extraction** (Zucker): answer-set-programming encoding.
- **The Cranelift escape (§1)**: keep e-classes tiny (1.13 avg) so sharing rarely bites, and
  accept the ~2-in-millions suboptimality. Valid ONLY when islands are small and directional.

**Binding on LOCK-2**: `extract.rs`'s greedy pass is **correct only for monotone tree-cost
islands** (regex/grammar canon, where the header claims it "works"). For any island where sharing
is load-bearing — residualization depth reused across sites (PROVE-12 monomorph sharing), a
kernel selected at many scan sites (GN-2.W1 `find_member`) — greedy tree-cost **will mis-pick**.
Two lawful answers: (a) prove the island sparse/small (Cranelift or treewidth-≤10 route), or
(b) fold extraction INTO the CSP (2C), where DAG cost = the assignment objective. **This is the
central 2A/2B-vs-2C adjudication and it is decidable by measurement.**

---

## 5. Profile-fed cost models + determinism (bears on 2D)

- **MLGO** (Trofin et al., 2021, arXiv 2101.04808): ML cost models integrated into production
  LLVM (inlining). The load-bearing lesson for us is **determinism**: "users care about
  compilation determinism"; random perturbation exists **only in dev/training**, the *deployed*
  model is frozen and deterministic. **ML-driven Hardware Cost Model for MLIR** (arXiv
  2302.11405) confirms the fit-offline / deploy-frozen split.
- **Direct mapping to 2D + SPEC law**: 2D's "cost coefficients fitted from canon-bench PMU
  captures, committed as fact tables (determinism preserved); solver re-runs at regen" is exactly
  MLGO's deploy-frozen posture. The GN-1 gate "full regen bit-for-bit" is a *hard* constraint: an
  online/adaptive cost model is illegal; coefficients must be **offline-fit, committed, and the
  regen must be idempotent** (precept 53, PROVE-12 proved regen bit-for-bit is achievable). 2D is
  well-precedented but its whole value is gated on the fitted table not perturbing determinism.

---

## 6. Constraints our own evidence base already imposes (the guardrails)

The cost model is not free to select whatever it likes — SPEC §0 pre-prunes the candidate space:

- **Refuted levers cannot be candidates.** No digit-scan SIMD (02 NO_GO, latency-bound), no
  block-scheduling pursuit (04, 1.05–1.06× only — the OoO core already pipelines), no shim perf
  claims (09 — 2× the movemask reduces, did not reproduce), fold-over-cursor dead, x86 dead. The
  *candidate generators* (e-graph rewrites) must not emit these — the cost model must never be in
  a position to "discover" a refuted lever. Cost inputs are **PMU cyc/B, paired, cold N≥200,
  multi-window** (precepts 43–49); **warm benches are disinformation** (voids any coefficient).
- **Determinism / regen bit-for-bit** (GN-1 gate) forbids nondeterministic saturation order or
  floating coefficients — mandates a fixed schedule (our `csp_scheduler` gives one) and committed
  fact tables (§5).
- **`generated_loc_budget`** (precept 53) is a *first-class CSP constraint* (2C names it), with
  teeth: overflow blocks the wave until traced to an O(N) generator regression.
- **Y.13 (consumer-less substrate forbidden)** = 2A's own law ("every island has a same-wave
  consumer and a cost model"). The seed reality (§0) shows the engine is *already* under-consumed
  — the risk is not building more graph engine, it is wiring what exists to a real decision.
- **The engine runs at GENERATION time** (type plane costs zero at runtime, verified invariant):
  the budget being optimized against is compile/generation time — reinforcing Cranelift's
  small-island verdict (§1) over unbounded saturation.

---

## 7. Named traps carried from comparable systems (checklist for every family)

1. **Blowup / scheduling** — full saturation explodes without a schedule (egg backoff; our
   `csp_scheduler` dirty-closure). 2B inherits this; 2A/2C bound it structurally.
2. **Greedy extraction mis-picks under sharing** (§4) — the deep one; already confessed in
   `extract.rs`. Non-monotone cost models silently break bottom-up.
3. **Saturation buys little in production** (Cranelift 0.1% at 7–8% compile) — over-investment
   risk for 2B; demand a Cranelift-style measurement bar.
4. **Cost-model non-determinism** (MLGO) — online/adaptive fitting breaks regen; must be
   offline-fit + committed (2D / §5).
5. **Phase ordering only cured where islands interact** — isolated 2A islands do NOT cure
   *cross-island* ordering; if two decisions are coupled (prefilter tier ⇄ kernel choice ⇄
   residualization depth) they must share one graph/CSP, not sit in separate islands (the
   GN-2.W1 ⇄ PT-1.W5 single-dispatch interlock is exactly this).
6. **Candidate space must exclude refuted levers** (§6) — a cost model that *can* pick a NO_GO
   lever is a defect, not a feature.

---

## 8. Synthesis for the design loop (inputs, not a verdict)

- **2A is the proven default shape** for a generation-time, budget-bound engine (Cranelift is
  the production proof); its risk is Y.13 under-consumption, which the seed reality confirms is
  the *actual* standing bug.
- **2C has the deepest external precedent** (PBQP instruction selection) and the readiest in-tree
  substrate (Kuhn–Munkres + CostFinite), and it *dissolves* the §4 extraction NP-hardness by
  making DAG-cost the CSP objective. 2A and 2C are likely **complementary, not rival**: e-graph
  islands generate candidates, one CSP decides across coupled sites (the ægraph "extraction as
  optimization" posture, made explicit).
- **2B must clear a Cranelift bar** — whole-IR saturation is defensible only where measured to
  pay past the compile-budget + determinism cost.
- **2D is well-precedented (MLGO) but conditional** — its entire value rests on the fitted table
  being offline-fit, committed, and regen-idempotent (PROVE-12 proves that is achievable).

**Proto proposal**: model one live coupled decision site — residualization depth (PROVE-12
sharing) OR prefilter-tier/kernel selection at N scan sites (GN-2.W1) — as a PBQP/CSP over the
in-tree `csp-solver` with a **DAG-cost** objective, benched head-to-head against `extract.rs`
greedy tree-cost extraction on the *same* egraph island. Measure whether greedy mis-picks under
shared kernel reuse and whether the CSP recovers the optimum within the generation-time budget.
This decides 2A/2B-vs-2C on a real seam with running code, per constitution clause vii.

---

## Citations

- Fallin, *The acyclic e-graph: Cranelift's mid-end optimizer*, cfallin.org/blog/2026/04/09/aegraph/;
  *ægraphs* (EGRAPHS 2023, pldi23.sigplan.org); *Cranelift ISLE*, cfallin.org/blog/2023/01/20/cranelift-isle/.
- Willsey et al., *egg: Fast and Extensible Equality Saturation*, POPL 2021 (arXiv 2004.03082).
- Zhang et al., *Better Together: Unifying Datalog and Equality Saturation* (egglog), PLDI 2023
  (doi 10.1145/3591239).
- Sun/Parreaux et al., *Fast and Optimal Extraction for Sparse Equality Graphs*, OOPSLA 2024
  (cse.hkust.edu.hk/~parreaux/publication/oopsla24a/).
- *e-boost: Boosted E-Graph Extraction*, ICCAD 2025 (csl.cornell.edu/~zhiruz/pdfs/eboost-iccad2025.pdf);
  *SmoothE: Differentiable E-Graph Extraction*, ASPLOS 2025.
- Zucker, *Answer Set Programming for E-Graph DAG Extraction*, philipzucker.com/asp-for-egraph-extraction/.
- Eckstein/Scholz/Ebner, *Generalized Instruction Selection using SSA-Graphs*, LCTES 2008;
  *Code Instruction Selection based on SSA-Graphs*, SCOPES 2003 (PBQP).
- Koes & Goldstein, *Near-Optimal Instruction Selection on DAGs*, CGO 2008; Wikipedia,
  *Instruction selection* (DAG covering NP-completeness).
- Castañeda Lozano & Schulte, *Survey on Combinatorial Register Allocation and Instruction
  Scheduling*, ACM CSUR 2018.
- Trofin et al., *MLGO: a Machine Learning Guided Compiler Optimizations Framework*, 2021
  (arXiv 2101.04808); *ML-driven Hardware Cost Model for MLIR* (arXiv 2302.11405).
- In-tree: `crates/egraph/src/{extract.rs,csp_scheduler.rs,cost_config.rs,cost_weights.rs}`,
  `crates/csp-solver/src/{builder/kuhn_munkres.rs,domain/cost_finite.rs,solver/gac/}`,
  SPEC §0 (PROVE-12 GO; refutations 02/04/09 as law), GN-2.md, R1 §8 (cost/BackendShape seam).
