# LOCK-2 — THE GRAPH ENGINE — PRIOR-ART (pass 2)

Prior art for *where and how graph optimization governs the generator pipeline*: e-graph
islands vs. whole-IR saturation, CSP/ILP for per-site lowering choice, profile-fitted cost
models, and deterministic regen. Adjudicated against the SK-V24 evidence base (SPEC §0
certified numbers + refutation laws, GN-2 spec, R1 cartography) and the in-tree seeds. No
family allegiance; this is what the world — and this corpus — has already settled for THIS
problem class.

---

## 0. THE BINDING PRIOR ADJUDICATION (this corpus, already decided)

`restart/skinny/tranches/sk-v21/research/27-sota-egraph-csp.md` is a full, sourced SOTA sweep
of **exactly this problem class**, and its verdict is a settled corpus fact the gestalt must
not silently contradict (Constitution ix). Its load-bearing thesis:

> **The solver is not the deliverable — the cost table and the constraints are. Where the
> input carries no measured discrimination, every solver (greedy, ILP, CSP, saturation)
> degenerates to the same answer, and elaboration over it is theatre.**

Two data points, arrived at from opposite directions, anchor it:

- **Cranelift's production acyclic e-graph averages 1.13 e-nodes/e-class** after rewriting —
  a shipping optimizer's extractor almost never has more than one candidate, and it
  deliberately uses tree-cost single-pass DP, *not* ILP/CSP/saturation.
- **This repo's own 54-config cost-weight grid sweep returned a documented NULL** across
  JSON/BBNF/CSS-L4/Sheets: "the current e-graph rewrite set produces e-classes with at most one
  canonical form for these grammars, so the cost model has no choice to exercise"
  (`crates/egraph/src/cost_weights.rs:134-167`).

The **three theatre-detectors** (each with a frontier witness) — any one tripping voids the
machinery no matter how sophisticated it looks:

1. **Cost is constant/zero** → flat objective → any solver returns its input ranking. (Skinny
   witness: `perf_cost ≡ 0` at all 8 sites, sk-v20 research/24.)
2. **The e-class is a singleton** → nothing to rank. (Cranelift 1.13; the sweep null.)
3. **The CSP is separable** → optimum = per-variable argmin → CP/ILP is redundant over greedy.
   (Unison wins *only because* reg-alloc+scheduling is non-separable.)

**All three currently trip for our grammars.** This is the floor every family below stands on:
the net-new deliverable is the *measured cost table* + the *rewrites that create alternatives*,
not the solver. Any family leading with solver sophistication over a flat/un-measured input is
theatre by this corpus's own settled standard.

---

## 1. What already exists in-tree (the seed reality)

The lock is NOT greenfield. `crates/egraph/` (1885 LOC) already ships the machinery the four
families argue over:

- **Greedy bottom-up extraction** (`extract.rs`): "the Egg greedy bottom-up algorithm … not
  optimal for all cost models (e.g., cost models that depend on sharing require a more
  sophisticated extractor)." The header *already names the deep trap* (§5). A `Lattice`/`Scalar`
  cost trait anticipates Pareto/multi-objective without churn (`extract.rs:36-60`) — **staged,
  not yet exercised.**
- **A CSP-backed rewrite scheduler** (`csp_scheduler.rs`, 368 LOC): each e-class is a CSP
  variable with a monotone `DirtyDomain` lattice, each parent edge a `ParentDirtyProp`
  constraint; one `csp.propagate()` computes the transitive dirty closure — a **dirty-frontier
  saturation loop with `rebuild()` each iter** (iter_limit 64, node_limit 100k). Note this is
  *heavier* than Cranelift's apply-once acyclic design (§2): it keeps the classical rebuild/
  re-canon loop the ægraph deletes by construction.
- **`csp-solver/`** (6987 LOC): AC-3/GAC + backtracking, Kuhn–Munkres assignment
  (`builder/kuhn_munkres.rs`, 265 LOC), `CostFinite`/lattice domains — an *assignment-with-cost*
  engine exists. It is classical CP, **a generation behind lazy-clause-generation CP-SAT** (§3).
- **The cost seam** (R1 §8.3): `ir/src/cost.rs` `ActiveCostFacts` records egraph/CSP selection
  provenance; `DecisionCspFacts` (40-field String bag), `PriorityStep` (8-step). `BackendShape`
  (5 variants, `ir/lib.rs:361`) is the extraction target; `derive_backend_shape`
  (`passes/lib.rs:398`) the seam; **N\*=3 kernel-crossover** the empirical cost threshold it
  would encode. SIMD kernels are still selected by `cfg!(target_arch)` + a boolean predicate —
  **disjoint from the decision engine.** **The engine is wired but under-consumed** — the Y.13
  pathology (substrate without consumer) is the standing risk, not absence of substrate.

PROVE-12 is the one *landed, measured* graph-engine-adjacent win: generation-time const-fold +
beta-reduction of the CSS delimiter test to a `u128` mask immediate (`css_canon_bench`
0.81–0.98× cyc/B, +18 gen LOC, byte-identical). It is residualization-as-monomorphization and
**uses no e-graph at all** — a data point that **cheap, directed, generation-time rewrites
already pay; the open question is whether a heavier engine pays MORE.**

---

## 2. The dominant production analog — Cranelift's acyclic e-graph (bears on 2A vs 2B)

Chris Fallin, *The acyclic e-graph: Cranelift's mid-end optimizer* (2026-04-09) and *ægraphs*
(EGRAPHS 2023). The single most relevant settled system: a **production compiler that chose
bounded, apply-once rewriting over full equality saturation**, and measured why.

- **Motivation = the pass-ordering disease** e-graphs cure: GVN, LICM, rematerialization fall
  out "for free" during translation to/from the e-graph; no separate machine-independent passes.
- **Eager rewrites at construction, single-pass, acyclic** (append-only union nodes ⇒ a rule
  fires exactly once per node): **no batched rebuild loop.** The classical fixpoint the seed
  `csp_scheduler` still runs is eliminated *by construction*.
- **Extraction = DP bottom-up in topological order, ignoring shared substructure** — the same
  tree-cost simplification our `extract.rs` uses, chosen precisely to dodge NP-hardness (§5).
- **The load-bearing measurement**: average e-class size **1.13 nodes**; whole-graph saturation
  buys **~0.1–2% runtime** at **~7–8% compile-time**; naive classical rewriting (assoc/comm on)
  inflated compile time **~5×**. Suboptimal eager events: "~twice across millions of nodes."
- **Ruleset discipline**: *deliberately excludes* associativity/commutativity identities — the
  explosion-avoidance that egglog formalizes as scheduled rulesets (§3).

**Verdict**: for a *generation-time* engine under a generated-LOC budget, Cranelift is direct
evidence that **2A/2B are only defensible in the acyclic apply-once shape**, and that **whole-IR
saturation with a rebuild loop (2B as literally worded, and the seed scheduler) is the 5×-blowup
shape a production compiler deleted.** Cite: cfallin.org/blog/2026/04/09/aegraph/; pldi23 ægraphs.

---

## 3. Whole-IR saturation & the rewrite layer — egg / egglog (bears on 2B)

- **egg** (Willsey et al., POPL 2021, arXiv 2004.03082): the *rebuilding* technique — amortized
  invariant restoration exploiting saturation's batch workload. Our `csp_scheduler.rs`
  dirty-closure is a CSP re-cast of egg's `BackoffScheduler`.
- **egglog** ("Better Together", Zhang et al., PLDI 2023, arXiv 2304.04332): e-graph = Datalog
  with congruence; **semi-naïve evaluation** gives determinism + incrementality; **rulesets +
  per-ruleset scheduling** (backoff/ban) tame explosive rules; lattice analyses cooperate. This
  is the **SOTA shape of the rewrite/schedule layer** and already matches our `LatticeDomain`
  idiom — *but its value is realized only once the rewrite set is expanded enough to create the
  alternatives the null sweep (§0) says do not yet exist.* Rewrites that create real choices, not
  scheduler sophistication, are the actual lever.
- **Counterweight**: Cranelift (§2) is a production refutation of "saturate everything." 2B is
  defensible ONLY in the acyclic apply-once form, and ONLY where cross-island equalities genuinely
  interact and the compile-budget + regen-determinism gate is paid — an *empirical* question.

---

## 4. CSP / ILP as the decision driver (bears on 2C — precedented, but tightly conditional)

The lowering-choice problem 2C names — per-site variables (kernel, capacity, prefilter tier,
residualization depth), constraints legality + generated-size budget, objective a PMU-fed cost
lattice — maps onto the **optimal instruction-selection problem**, which the compiler world has
settled machinery for:

- **Instruction selection on DAGs is NP-complete** (tree covering is linear-time DP) — Koes &
  Goldstein, *Near-Optimal Instruction Selection on DAGs*, CGO 2008; Wikipedia *Instruction selection*.
- **PBQP** — Eckstein/Scholz/Ebner, *Generalized Instruction Selection using SSA-Graphs* (LCTES
  2008), *Code Instruction Selection based on SSA-Graphs* (SCOPES 2003): optimal covering under
  legality + cost as Partitioned Boolean Quadratic Programming, near-optimal for "almost all" nodes.
- **Unison** — Castañeda Lozano & Schulte (TOPLAS 2019, ACM CSUR 2018 survey; arXiv 1804.02452):
  integrated register allocation + instruction scheduling as one constraint program, beats LLVM,
  optimal to ~946-instruction functions. **CP wins there ONLY because the subproblems are
  non-separable** (schedule ↔ live-range ↔ spill ↔ schedule). This is the discriminating criterion.
- **CP-SAT / lazy clause generation** (Feydy–Stuckey) is the CP SOTA — propagators emit
  *explanation clauses*, not domain mutations; our AC-3 csp-solver is a generation behind it.
- **In-tree fit**: `csp-solver`'s Kuhn–Munkres + `CostFinite` domains are already an
  assignment-with-cost engine; a PBQP-shaped driver is a *specialization of extant machinery*.

**Verdict — the corrective (§0 binding):** 2C's sharp claim, "e-graphs only generate candidates;
the CSP decides," is the **two-engine inversion the frontier never ships**: e-boost, Unison, and
the MLIR eqsat dialect all fold cost + constraints *inside* the single extraction (a 0/1
selection = one e-node per class, min cost — literally the ILP/CSP). A downstream CSP that
recovers the extractor's answer is the documented tautology skinny shipped
(`csp_strategy` "solves to recover the index it was handed", sk-v20 research/24). 2C is
justified **only** if it (a) makes DAG-cost/legality the objective *within* extraction, and
(b) exhibits a **genuine coupling constraint** that renders the decision non-separable — and the
**one live candidate for that coupling is the `generated_loc_budget`** (Σ per-site LOC ≤ budget),
which no per-leaf argmin can honor. That must be *demonstrated by the mandatory ablation* (§8),
not assumed. `cfg!(target_arch)` survives only as an admissibility filter, never as the selector.

---

## 5. THE deep trap — extraction under shared substructure (cross-cutting, binds every family)

The universally-reported pitfall, confessed in our own `extract.rs` header:

- **Greedy bottom-up (tree cost) double-counts shared subexpressions.** DAG cost (count each
  subexpr once) is correct but **DAG-optimal extraction is NP-hard.** It only bites when a kernel
  is *amortized/shared across leaves* (DAG-shaped cost); for a sparse per-grammar island, greedy
  tree-cost **is** optimal and Cranelift-cheap.
- **The settled escalation menu** (choose per island by measured need):
  - **ILP** — optimal, "prohibitively expensive" past small graphs (the baseline everyone beats).
  - **Fast & Optimal Extraction for Sparse Equality Graphs** (Sun/Parreaux et al., OOPSLA 2024):
    treewidth-parameterized, optimal for **treewidth ≤ 10**, a fraction of ILP time — the
    "our e-graphs are sparse/tree-like" exact route.
  - **e-boost** (Cornell, ICCAD 2025): heuristic-prune → **warm-start ILP**, **558× faster than
    plain ILP**, +19% quality vs SmoothE — SOTA when near-optimal DAG extraction is genuinely needed.
  - **SmoothE** (Cornell, ASPLOS 2025): differentiable GPU-batched relaxation.
  - **ASP for E-graph DAG extraction** (Zucker): answer-set-programming encoding.

**Binding on LOCK-2**: `extract.rs`'s greedy pass is **correct only for monotone tree-cost
islands**. Escalate **only if measurement proves both** (i) the cost is genuinely DAG-shaped
(shared kernels amortized across leaves — e.g. PROVE-12 monomorph sharing, a `find_member` kernel
selected at many GN-2.W1 scan sites) *and* (ii) the greedy answer differs from the exact optimum
on the real corpus. Absent that proof, escalation is theatre by the frontier's own standard.

---

## 6. Profile-fed cost models + determinism (bears on 2D — the most evidence-aligned family)

- **The missing input, named (sk-v21/27):** the KernelCostTable in the **dav1d-checkasm
  discipline** — `(KernelId, Uarch) → cycles/64B`, versioned data regenerated on the M-series
  target, never estimated (per-function speedups span "a few percent to 20+"; instruction
  estimates cannot rank a NEON `vqtbl1q` lane against scalar without modeling port pressure).
  **This is the one genuinely net-new, load-bearing artifact; every "cost model" complaint reduces
  to its absence.** It is what makes `perf_cost` non-zero and thus any solver non-tautological.
- **MLGO** (Trofin et al., 2021, arXiv 2101.04808) + *ML-driven Hardware Cost Model for MLIR*
  (arXiv 2302.11405): the **fit-offline / deploy-frozen** split — "users care about compilation
  determinism"; perturbation exists only in training, the deployed model is frozen. **Target-Aware
  real-expression implementation** (Herbie lineage, arXiv 2410.14025) is the closest analogue to
  cost-model-gated per-target selection: cost from the target's actual instruction costs, applied
  *in* extraction.
- **Direct mapping to 2D + SPEC law**: 2D's "coefficients fitted from canon-bench PMU captures,
  committed as fact tables (determinism preserved); solver re-runs at regen" **is** the
  deploy-frozen posture. The GN-1 gate "full regen bit-for-bit" makes an online/adaptive cost
  model **illegal**; coefficients must be **offline-fit, committed, regen-idempotent**, and drawn
  from **paired cold PMU** (warm benches void any coefficient — the 09 self-deception lesson).
  2D is well-precedented; its whole value rests on the fitted table not perturbing determinism.

---

## 7. Constraints our evidence base already imposes + named traps (the guardrails)

- **Refuted levers cannot be candidates** (SPEC §0 / Constitution iv). No digit-scan SIMD (02
  NO_GO), no block-scheduling pursuit (04, 1.05–1.06× — the OoO core already pipelines; there is
  *no measured lever* for a scheduling-decision e-graph on this axis), no shim perf claim (09),
  fold-over-cursor dead, x86 dead. The *candidate generators* must never emit a NO_GO lever — a
  cost model that *can* "discover" a refuted lever is a defect, not a feature.
- **Determinism / regen bit-for-bit** (GN-1 gate) forbids nondeterministic saturation order or
  floating coefficients — mandates a fixed schedule + committed fact tables (§6).
- **`generated_loc_budget`** (precept 53) is a first-class *global coupling* CSP constraint — the
  one candidate that could lift 2C above per-leaf-argmin theatre — with teeth: overflow blocks the
  wave until traced to an O(N) generator regression.
- **Y.13 (consumer-less substrate forbidden)** = 2A's own law ("every island has a same-wave
  consumer and a cost model"). The seed reality (§1) shows the engine is *already* under-consumed;
  the risk is not building more graph engine, it is wiring what exists to a real, measured decision.
- **The engine runs at GENERATION time** (type plane costs zero at runtime, verified invariant):
  the optimized budget is compile/generation time — reinforcing the small-island verdict (§2).
- **String-bag telemetry** (`DecisionCspFacts`, 40 fields of "pass"/"evidence-only") — replace
  with a falsifiable, diffable per-leaf manifest (`rule_id, shape, kernel_id, uarch, cost_cell,
  table_version, rejected_alternatives_with_costs`) that re-derives byte-for-byte at regen.

---

## 8. Synthesis for the design loop (inputs, not a verdict)

- **2D is the prerequisite for all others.** Build the committed, checkasm/PMU-fitted
  KernelCostTable **first** (versioned data, offline-fit, regen-idempotent — MLGO posture,
  PROVE-12 proves bit-for-bit regen is achievable). Without it, every other family trips
  theatre-detector #1 and no solver upgrade means anything.
- **2A is the right structural default** for a generation-time, budget-bound engine — but in the
  **Cranelift acyclic apply-once shape**, and with its "CSP selects over extracted candidates"
  clause demoted to a legality filter *inside* extraction (the two-stage extract→CSP has no
  frontier precedent, §4). Its live bug is Y.13 under-consumption (§1), not missing substrate.
- **2B must clear the Cranelift bar** and shed the seed scheduler's rebuild loop — whole-IR
  saturation is defensible only in the acyclic form and only where measured to pay past the
  compile-budget + determinism cost, once the rewrite set actually creates alternatives (egglog).
- **2C is precedented (PBQP/Unison) but conditional**: legitimate only if it folds cost+legality
  *into* one extraction AND the `generated_loc_budget` (or another proven coupling) makes the
  decision non-separable. Otherwise it is redundant over per-leaf argmin.

**The mandatory anti-theatre gate (binding, from §0):** before shipping *any* solver above greedy
per-leaf argmin, run the ablation — **does the solver's chosen configuration differ from
independent per-leaf argmin over the measured table?** If not (as the sweep null and Cranelift 1.13
both predict absent real coupling), the CSP/ILP is theatre and the correct engine is argmin +
legality filters. Escalation ladder, each rung gated on that ablation passing: greedy tree-cost →
treewidth-FPT → e-boost warm-start ILP (proven DAG cost) OR CP-SAT/LCG (proven coupling).

**Proto proposal**: build the KernelCostTable cell for ONE live coupled seam — prefilter-tier/
kernel selection at N scan sites (GN-2.W1 `find_member`) or residualization depth (PROVE-12
sharing) — then run the ablation: per-leaf argmin over the measured table vs. a PBQP/CSP over the
in-tree `csp-solver` with the `generated_loc_budget` as a *global* constraint, on the *same*
egraph island, benched paired-cold. This decides 2A-vs-2C on a real seam with running code, and
its result — argmin suffices, or the budget-coupling makes CSP strictly better — is the verdict,
per Constitution clause vii. Whichever way it falls, the deliverable that ships is the committed
cost table + the rewrites that create alternatives, never the solver.

---

## Citations

- **sk-v21 research/27-sota-egraph-csp.md** (the binding in-corpus adjudication: the null sweep,
  the three theatre-detectors, the KernelCostTable-first mandate, the ablation gate) — this repo.
- Fallin, *The acyclic e-graph: Cranelift's mid-end optimizer*, cfallin.org/blog/2026/04/09/aegraph/;
  *ægraphs* (EGRAPHS 2023, pldi23.sigplan.org); RFC bytecodealliance/rfcs cranelift-egraph.md.
- Willsey et al., *egg: Fast and Extensible Equality Saturation*, POPL 2021 (arXiv 2004.03082).
- Zhang et al., *Better Together: Unifying Datalog and Equality Saturation* (egglog), PLDI 2023
  (arXiv 2304.04332); egglog scheduling — egraphs-good.github.io/egglog-tutorial/04-scheduling.html.
- Sun/Parreaux et al., *Fast and Optimal Extraction for Sparse Equality Graphs*, OOPSLA 2024
  (cse.hkust.edu.hk/~parreaux/publication/oopsla24a/).
- *e-boost: Boosted E-Graph Extraction*, ICCAD 2025 (arXiv 2508.13020);
  *SmoothE: Differentiable E-Graph Extraction*, ASPLOS 2025.
- Zucker, *Answer Set Programming for E-Graph DAG Extraction*, philipzucker.com.
- Eckstein/Scholz/Ebner, *Generalized Instruction Selection using SSA-Graphs*, LCTES 2008;
  *Code Instruction Selection based on SSA-Graphs*, SCOPES 2003 (PBQP).
- Koes & Goldstein, *Near-Optimal Instruction Selection on DAGs*, CGO 2008; Wikipedia,
  *Instruction selection* (DAG covering NP-completeness).
- Castañeda Lozano & Schulte, *Combinatorial Register Allocation and Instruction Scheduling*,
  TOPLAS 2019 (arXiv 1804.02452) + ACM CSUR 2018 survey; unison-code.github.io.
- Feydy & Stuckey, lazy clause generation / CP-SAT — people.eng.unimelb.edu.au/pstuckey.
- Trofin et al., *MLGO*, 2021 (arXiv 2101.04808); *ML-driven Hardware Cost Model for MLIR*
  (arXiv 2302.11405); *Target-Aware Implementation of Real Expressions* (arXiv 2410.14025).
- MLIR eqsat dialect (arXiv 2505.09363); DIOSPYROS eqsat-for-DSP (ASPLOS 2021).
- dav1d / checkasm NEON discipline — ewoutterhoeven, *How ARM's NEON enables efficient AV1 decoding*.
- In-tree: `crates/egraph/src/{extract.rs,csp_scheduler.rs,cost_config.rs,cost_weights.rs:134-167}`,
  `crates/csp-solver/src/{builder/kuhn_munkres.rs,domain/cost_finite.rs}`, `prototypes/PROVE-12`,
  SPEC §0 (PROVE-12 GO; refutations 02/04/09 as law), GN-2.md, R1 §8 (cost/BackendShape seam).
