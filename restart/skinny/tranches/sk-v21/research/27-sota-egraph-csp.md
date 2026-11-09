# 27 — SOTA of the e-graph + CSP frontier for codegen decisions (2025–2026)

## Thesis

The e-graph and constraint-programming machinery in this repository is real, integrated, and
sound *as libraries* — a greedy bottom-up extractor with a `Lattice`/`CostModel` trait surface
(`crates/egraph/src/extract.rs:36-215`), a consolidated `CostWeights` source of truth
(`crates/egraph/src/cost_weights.rs:40-110`), and a general CSP solver crate. What is missing is
the same thing the 2025–2026 frontier has converged on as the *only* part that matters: **a
measured cost that gives the solver a real choice to make.** The frontier's clearest lesson,
delivered independently by a production compiler and by this repo's own calibration sweep, is that
the solver is not the deliverable — the cost table and the constraints are. Where the input carries
no measured discrimination, every solver (greedy, ILP, CSP, e-graph saturation) degenerates to the
same answer, and any elaboration over it is theatre.

Two numbers anchor this entire report:

- **Cranelift's production acyclic e-graph has an average e-class size of 1.13 e-nodes** after
  rewriting across its benchmark corpus (https://cfallin.org/blog/2026/04/09/aegraph/). In a
  shipping optimizing compiler, the extractor almost never has more than one candidate to choose
  between — and it deliberately uses tree-cost single-pass dynamic programming, *not* ILP, *not*
  CSP, *not* a saturation loop, precisely because the combinatorics do not warrant it.
- **This repo's own 54-configuration cost-weight grid sweep returned a documented null** across all
  four primary grammars (JSON 51 states, BBNF 476, CSS L4 2875, Sheets 191 — sweep variance = the
  singleton set in every case), because "the current e-graph rewrite set produces e-classes with at
  most one semantically-distinct canonical form for these grammars, so the cost model has no choice
  to exercise" (`crates/egraph/src/cost_weights.rs:134-167`).

These are the *same finding* arrived at from opposite directions. Cranelift confirms it is not a
defect peculiar to this project: for structured, sparse rewrite systems the extraction problem is
usually trivial, and the engineering value lives in the *rewrites that create alternatives* and the
*cost that discriminates them* — never in the extraction algorithm's sophistication.

---

## (a) egg / egglog current state; extraction with real cost models

### egglog is the mature successor; egg is the stable predecessor

`egglog` (PLDI 2023, "Better Together: Unifying Datalog and Equality Saturation",
https://arxiv.org/pdf/2304.04332) has matured through 2025 into the reference system: Datalog +
equality saturation in one fixpoint engine, with **rulesets** (named rule groups, `:ruleset`
annotations), **customizable scheduling** (per-ruleset saturation control, backoff/ban to tame
explosive rules — https://egraphs-good.github.io/egglog-tutorial/04-scheduling.html), proofs,
seminaïve evaluation for e-matching via database query planning, lattice-valued analyses, a plugin
system, and Python bindings (PLDI 2025 tutorial "Unlocking Optimizations with egglog",
https://pldi25.sigplan.org/details/pldi-2025-tutorials/4/). Formal semantics were pinned down in
2025 ("Semantic foundations of equality saturation", https://arxiv.org/pdf/2501.02413). The
relevance here: **egglog's ruleset + scheduling model is exactly the pluggability the repo's
`CostWeights` doc gestures at but does not have** — the ability to gate which rewrites fire, and to
bound saturation so it terminates without exploding. The repo currently runs a hand-rolled fixed
rule set with no scheduling discipline; egglog's ruleset/scheduling is the SOTA shape of that.

### Extraction: tree cost is polynomial, DAG cost is NP-hard — and this is the whole game

The universal 2024–2025 framing (e-boost https://arxiv.org/abs/2508.13020; E-Graphs as Circuits
https://arxiv.org/abs/2408.17042; SmoothE ASPLOS 2025
https://www.csl.cornell.edu/~yc2632/data/smoothe_asplos2025_final.pdf):

- **Tree cost** counts each shared subexpression once *per use* — solvable greedily bottom-up in
  polynomial time (this is exactly `crates/egraph/src/extract.rs:96-111` `AstSize` and the
  fixed-point in `compute_best`, `:134-171`). It over-counts sharing but is fast and is what
  Cranelift ships.
- **DAG cost** counts each unique subexpression once regardless of reuse — the accurate model for
  code size / logic area — and is **NP-hard**, because the choice at one class now interacts with
  sharing decisions elsewhere. This is where ILP/CSP/heuristics earn their keep.

The 2024–2025 extraction frontier, in ascending sophistication:

1. **Greedy bottom-up** (egg default; this repo's `Extractor`): fast, optimal only for monotone
   tree-cost models. The repo's own extractor doc concedes this: "not optimal for all cost models
   (e.g., cost models that depend on sharing require a more sophisticated extractor), but it works
   for the monotone cost models used by grammar/regex optimization"
   (`crates/egraph/src/extract.rs:8-11`). That caveat is correct and is the right call *for a sparse
   grammar e-graph*.
2. **ILP extraction**: optimal for DAG cost, scales poorly — "prohibitively expensive" past small
   graphs (e-boost §1).
3. **Treewidth-parameterized exact extraction** ("E-Graphs as Circuits, and Optimal Extraction via
   Treewidth", OOPSLA 2024, https://arxiv.org/abs/2408.17042): recasts an e-graph as a Boolean
   circuit; gives an FPT algorithm in `2^O(w²)·poly(w,n)` where `w` is treewidth; circuit
   simplifications shrink size and treewidth by **40–80%**; optimally extracts graphs of treewidth
   ≤ 10 in a fraction of ILP's time. This is the *right* tool when exactness matters and the graph
   is sparse — which grammar e-graphs are.
4. **SmoothE** (ASPLOS 2025): differentiable relaxation of extraction, GPU-batched.
5. **e-boost** (Aug 2025, https://arxiv.org/abs/2508.13020): parallelized heuristic DAG-cost
   extraction + adaptive threshold pruning + **warm-start ILP** (heuristic solution seeds the exact
   solver). **558× faster than plain ILP, 19.04% better quality than SmoothE, 7.6–8.1% area
   improvement in logic synthesis.** This "heuristic-prunes-then-exact-solves-the-remainder" is the
   current SOTA pattern for when you genuinely need near-optimal DAG extraction.
6. **Answer-Set Programming for extraction** (2606.10644) — declarative alternative to ILP.

**Load-bearing takeaway for the repo:** the extractor already in `crates/egraph` is the right
*class* of algorithm (greedy bottom-up, tree cost) for a sparse per-grammar decision. Upgrading to
ILP/CSP is only justified if (i) the cost is genuinely DAG-shaped (shared kernels amortized across
leaves) *and* (ii) measurement shows the greedy answer differs from the optimum. Neither is
established today; the sweep null (`cost_weights.rs:134-167`) says the opposite.

---

## (b) Equality saturation in production compilers: Cranelift's ægraph

Cranelift is the load-bearing precedent because it is the one place eqsat actually ships in a
production optimizing compiler, and it made every choice *against* sophistication —
https://cfallin.org/blog/2026/04/09/aegraph/, ægraphs PLDI/EGRAPHS 2023, RFC
https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md, with updated data
presented at the January 2026 Dagstuhl e-graphs seminar.

How it **bounds rewrites** (the anti-explosion design):

- **Eager, single-pass, apply-once.** Rewrites are applied immediately when a node is created;
  because the graph is acyclic and append-only (union nodes form an immutable binary tree joining
  original and rewritten forms), "nodes visible from a given id for an eclass never change," so a
  rule is applied exactly once per node. There is **no rebuild step** and no re-canonicalization
  loop — the classical e-graph's expensive fixpoint is eliminated by construction.
- **Ruleset curated to avoid blowup.** It "deliberately excludes identity rules (associativity,
  commutativity) that explode the solution space." Naive classical rewriting inflated compile time
  ~**5×** once rewrites turned on; the acyclic redesign brought overhead to **~7–8% compile-time**
  for a **~2% runtime** improvement.
- **Average e-class size 1.13.** After rewriting, most e-classes carry exactly one option. The
  entire extraction apparatus is doing near-trivial work in the common case.

How it **extracts / costs**:

- **Toposort + bottom-up dynamic programming**, "picking minima," with a per-node cost equal to the
  sum of argument costs plus own cost — i.e. **tree cost that ignores shared substructure**
  ("we'll ignore shared substructure, pretending that each use of a subtree counts that subtree's
  cost anew"). No backtracking, no ILP, no CSP. Elaboration back to CFG uses scoped memoization over
  the dominator tree to keep SSA valid.

Adjacent production/MLIR precedents: the **eqsat MLIR dialect** for non-destructive rewriting
(https://arxiv.org/pdf/2505.09363) and **DialEgg** (dialect-agnostic MLIR optimizer over egglog).
Both keep extraction cost-model-driven but structurally simple.

**Load-bearing takeaway:** a shipping compiler with a mature eqsat mid-end uses tree-cost single-pass
DP and curates its rules to keep e-classes near-singleton. If the project's kernel/layout decision
is *simpler* than Cranelift's whole mid-end (it is — dozens of variables, not a whole function's
value graph), then anything heavier than tree-cost greedy extraction must justify itself against
this datum, not against the aspiration of "equality saturation."

---

## (c) CSP / optimization SOTA relevant to kernel selection

### or-tools CP-SAT = lazy clause generation (the current CP SOTA)

CP-SAT is a **lazy clause generation (LCG)** solver (Feydy/Stuckey lineage,
https://people.eng.unimelb.edu.au/pstuckey/papers/cp09-lc.pdf; overview
https://medium.com/@jonaspoelmans120/understanding-cp-sat-...): search is driven by a SAT/CDCL
engine; finite-domain **propagators do not mutate domains directly — they lazily emit the clauses
that *explain* each domain change**, feeding them into the SAT core so it can do unit propagation,
conflict analysis, and clause learning. This fuses CP's rich global constraints with SAT's
nogood-learning and MIP-style bounding. It is the reference design if the project ever needs a real
CSP: **propagators as explanation generators, not domain mutators.** The repo's vendored
`crates/csp-solver` (AC-3/backtracking with `adjacency`/`ordering`/`domain` modules) is a classical
CP solver, a generation behind LCG; that gap is only relevant if a genuinely combinatorial coupling
constraint materializes.

### Unison — the canonical proof that CP belongs in a compiler backend, and its scale threshold

"Combinatorial Register Allocation and Instruction Scheduling" (TOPLAS 2019,
https://dl.acm.org/doi/10.1145/3332373; https://arxiv.org/abs/1804.02452; https://unison-code.github.io/):
Unison models **integrated register allocation + instruction scheduling** as one constraint program,
LLVM-integrated, industry-used, and **generates better code than LLVM while solving optimally for
functions up to ~946 instructions** (nearly 10× larger than prior combinatorial approaches). The
reason CP wins there and greedy/heuristic LLVM passes lose: the two subproblems are **deeply
interdependent** (a scheduling choice changes live ranges, which changes allocation, which changes
spills, which changes scheduling) — a real coupling that greedy phase-ordering cannot see. CP's
edge is exactly the presence of *cross-variable constraints that make the problem non-separable.*

This is the discriminating criterion for the sober question below: **CP earns its keep when the
decision is non-separable.** Unison's ~1000-instruction ceiling also sets the scale: a
dozens-of-variables per-grammar decision is two orders of magnitude smaller, so if it *were*
non-separable, CP would solve it instantly — but the far more likely case is that it is separable,
and then CP is redundant over per-leaf argmin.

### MiniZinc, and "when CP beats ILP / greedy"

MiniZinc (solver-independent modeling, https://icaps25.icaps-conference.org/program/tutorials/minizinc/)
compiles one model to CP, MIP, or SAT backends; the annual MiniZinc Challenge is "consistently won
by CP solvers" on scheduling/selection. The instruction-selection literature (Blindell survey,
https://arxiv.org/pdf/1306.4898) makes the practitioner's argument for CP over ILP directly:
**machine constraints are easier to state as CP constraints than to linearize as ILP inequalities**
(non-overlap of covered DFG nodes, resource exclusion, ISA legality). Constraint-driven kernel
auto-tuning is live in HPC too — "Constraint-Driven Auto-Tuning of GEMM-like Operators" (SC'25,
https://dl.acm.org/doi/10.1145/3712285.3759898) uses constraint dependency chains to prune infeasible
kernel-parameter configurations. The pattern: **CP as a legality/feasibility pruner over a
parameterized kernel space**, which is precisely the ISA/layout-legality role a right-sized
formulation would give it.

**When CP does *not* beat greedy:** when the variables are independent (each leaf's best kernel does
not depend on any other leaf's choice), the optimum *is* the per-leaf argmin, and CP/ILP recover it
at strictly higher engineering cost. This is the case the repo is almost certainly in.

---

## (d) Hybrid e-graph + CSP precedents

The honest frontier finding: **the mainstream does not run an e-graph and *then* a separate CSP.
It folds the cost/constraints *into* extraction, because extraction already *is* the optimization
problem** (a 0/1 selection: one e-node per reachable class, minimizing cost — literally an ILP/CSP).
The precedents:

- **e-boost** (https://arxiv.org/abs/2508.13020) is the clearest hybrid: heuristic pruning produces
  a reduced e-graph, then a **warm-started ILP** solves extraction exactly on the remainder. E-graph
  and constraint-solver are one pipeline, not two engines with a handoff.
- **Instruction selection modulo equivalence**: e-graphs represent the equivalence closure, and
  extraction "selects a well-typed program minimal according to the target description's cost model"
  (Blindell survey framing; MLIR eqsat dialect). The target cost *is* the extraction objective.
- **Target-Aware Implementation of Real Expressions** (https://arxiv.org/pdf/2410.14025, Herbie
  lineage): e-graph rewriting to platform-specific operations under a **cost model derived from the
  target's actual instruction costs** — the closest published analogue to "cost-model-gated hot-leaf
  selection per target ISA." Again: cost lives inside extraction.

**Load-bearing takeaway:** a two-stage "e-graph extract → CSP finalize" pipeline that hands the CSP
the answer the extractor already chose (the tautology documented at
`restart/skinny/tranches/sk-v20/research/24-decision-engine.md:63-73`) has **no precedent in the
frontier** because it inverts the design: the constraints and cost belong *inside* the single
extraction, as e-boost, Unison-style CP, and MLIR eqsat all do. If constraints matter, they become
extraction constraints (hard-prune illegal nodes, add resource coupling); if they don't, there is no
second stage.

---

## (e) The sober question: what is right-sized vs theatre for kernel + tape-layout selection

### What the audit found (the theatre baseline)

Two independent decision subsystems, both cost-theatre:

- Skinny: `perf_cost ≡ 0` at all eight assignment sites; selection collapses to a lexicographic
  tie-break over two hardcoded size/rank constants; one cosmetic rewrite exists only to satisfy a
  "≥1 rewrite" lock; the CSP "solves to recover the index it was handed" and its own test asserts it
  "preserves the active shape" (`restart/skinny/tranches/sk-v20/research/24-decision-engine.md:8-15,
  46-73`). The SIMD kernels that actually deliver the speed are selected by `cfg!(target_arch)` + a
  boolean admissibility predicate, **entirely disjoint from the decision engine** (`24:89-107`).
- Core: a second, larger `csp_strategy` solver (1361 lines) with the *same* pathology in a different
  file — `kernel_shape.rs` documents "thresholds are pluggable via the cost model" but hardcodes
  `NIBBLE_LUT_MAX = 8` / `WIDE_LUT_MAX = 16` (`24:126-132`).
- The e-graph library's calibrated weights are the default weights, justified by a null sweep
  (`crates/egraph/src/cost_weights.rs:134-191`).

Every one of these is the same failure the frontier predicts: **a solver over an input with no
measured discrimination.** When the cost is zero or the e-class is a singleton, the solver *must* be
tautological — there is nothing to solve.

### The three theatre-detectors (each has a frontier witness)

1. **Cost is constant / zero** → the objective is flat → any solver returns its input ranking.
   (Witness: `perf_cost ≡ 0`, `24:46-52`.)
2. **The e-class is a singleton** → there is no alternative to rank. (Witnesses: Cranelift's 1.13
   average; the repo's sweep null "at most one canonical form", `cost_weights.rs:159-161`.)
3. **The CSP is separable** → the optimum is per-variable argmin → CP/ILP is redundant over a greedy
   pass. (Witness: Unison wins *only because* reg-alloc+scheduling is non-separable; a separable
   per-leaf kernel choice is not that.)

If a formulation trips any of the three, the machinery is theatre regardless of how sophisticated it
looks.

### What a real, measured formulation looks like

The decision is: for each grammar, per hot leaf, choose `(tape_layout_shape, simd_kernel)` from an
admissible set, minimizing runtime. A dozens-of-variables problem. Right-sized design:

1. **The missing input is the cost, not the solver — build `KernelCostTable` first.**
   `(KernelId, Uarch) → cycles_per_64B`, populated by a **checkasm/PMU microbench harness** in the
   dav1d discipline — the exact tool dav1d uses to cycle-count each NEON function and gate merges on
   measured speedup (per-function speedups span "a few percent to factors of 20+" across 146
   functions; https://medium.com/@ewoutterhoeven/how-arms-neon-enables-efficient-av1-decoding-on-mobile-5fcb3a4f6e7f).
   Checked in as versioned data, regenerated on the M-series target, never estimated. Instruction
   estimates cannot rank a NEON `vqtbl1q` lane against a scalar fallback without modeling port
   pressure — only measurement can. This single artifact is what makes `perf_cost` non-zero and thus
   makes any solver non-trivial. **Without it, no solver upgrade means anything.**

2. **Constraints = ISA/layout legality, as hard admissibility filters (this is CP's real role).**
   A NEON kernel node is *hard-pruned* on a non-NEON uarch; a layout that a consumer cannot decode is
   pruned; `avx512bw`-gated shapes are struck on aarch64 (dead today,
   `24:30-33`). These are exactly the "constraints easier to state in CP than to linearize in ILP"
   from the instruction-selection literature — but they are *filters on the candidate set*, applied
   before/within extraction, not a downstream tautological solver. `cfg!(target_arch)` survives
   **only** as this admissibility filter, never as the selector (`24:170-178`).

3. **Objective = frequency-weighted measured cost.** `Σ freq(leaf) · table_cost(kernel(leaf), uarch)`,
   with frequency from corpus profiling (`crates/ir/src/passes/profile.rs`) on the *frequency* axis
   and the checkasm table on the *per-kernel* axis. This is the iburg/BURG dynamic-programming form —
   with real cells, not zeros.

4. **Solver = greedy bottom-up extraction by default; escalate only on measured non-separability.**
   The existing `Extractor` (`crates/egraph/src/extract.rs`) is the right default: if per-leaf kernel
   choices are independent, tree-cost greedy *is* optimal and Cranelift-cheap. Escalate to
   **treewidth-FPT extraction** (https://arxiv.org/abs/2408.17042, optimal ≤ treewidth 10, beats ILP)
   or **e-boost-style warm-start ILP** (https://arxiv.org/abs/2508.13020) *only if* two conditions
   are measured true: (i) kernels are shared/amortized across leaves so the cost is genuinely
   DAG-shaped, and (ii) an ablation shows the greedy answer differs from the exact optimum on the real
   corpus. Escalate to **CP-SAT / lazy clause generation** *only if* a genuine coupling constraint
   appears (a shared tape/register budget forcing a global tradeoff, Unison-style non-separability) —
   which for a dozens-of-variables per-grammar decision is unlikely and must be demonstrated, not
   assumed.

5. **One engine, not two.** Unify the skinny `BackendShape` engine and core `csp_strategy` into a
   single extraction over the product language `(BackendShape × KernelStrategy)` with kernels as
   cost-selectable alternatives in one e-class (`24:168-178`). Two solvers over the same decision
   violate the single-path discipline and double the theatre surface.

6. **The falsifiable manifest replaces the String-status telemetry.** Emit, per selected leaf,
   `(rule_id, shape, kernel_id, uarch, table_cost_cell, table_version, scalar_oracle, checkasm_cmd,
   rejected_alternatives_with_costs)` — generated by the extractor, diffable across regens, byte-for-byte
   reproducible, every `kernel_id` traceable to a checkasm-gated cell (`24:181-192`). This is what
   makes the decision *auditable*: you can re-derive it and see the rejected alternatives and their
   measured costs. The current 40-field `DecisionCspFacts`/`BenchFacts` String bags record
   "pass"/"evidence-only" and are deleted.

### The mandatory ablation (the anti-theatre gate)

Before shipping *any* solver above greedy argmin, run the ablation the whole frontier implies: **does
the solver's chosen configuration differ from independent per-leaf argmin over the measured table?**
If not — as the sweep null and Cranelift's 1.13 both predict it will not, absent real coupling — then
the CSP/ILP is theatre and the correct engine is per-leaf argmin with legality filters. The solver
must *earn* its complexity by producing a measurably different, measurably better selection on the
real corpus. This single test is the difference between the frontier's discipline and the audit's
finding.

---

## Bottom line for the tranche plan

- **Build the `KernelCostTable` (checkasm/PMU, versioned data) before touching any solver.** It is
  the one genuinely net-new, load-bearing artifact; every "cost model" complaint in the audit
  reduces to its absence. dav1d's checkasm is the exact prior art.
- **Keep the existing greedy tree-cost extractor as the default engine.** Cranelift ships eqsat at
  1.13 average e-class size with tree-cost single-pass DP; a per-grammar dozens-of-variables decision
  is smaller, not larger. Match the production discipline, not the ILP aspiration.
- **Demote `cfg!(target_arch)` and every hardcoded threshold to admissibility constraints / table
  cells.** Legality is CP's real job; selection is the table's.
- **Retire the tautological second stage entirely.** No frontier design has an extractor hand its
  answer to a downstream CSP; constraints live inside the single extraction (e-boost, Unison, MLIR
  eqsat all confirm this).
- **Gate solver escalation (treewidth-FPT → warm-start ILP → CP-SAT/LCG) behind a measured ablation**
  proving the greedy answer is suboptimal on the real corpus. Absent that proof, escalation is
  theatre by the frontier's own standard.
- **egglog's rulesets + scheduling are the SOTA shape of the rewrite layer**, if and when the rewrite
  set is expanded enough to create the alternatives the null sweep says do not yet exist — because
  *rewrites that create real alternatives*, not solver sophistication, are the actual lever.

## Sources

- Cranelift ægraph — https://cfallin.org/blog/2026/04/09/aegraph/ ; RFC https://github.com/bytecodealliance/rfcs/blob/main/accepted/cranelift-egraph.md ; https://news.ycombinator.com/item?id=47717192
- egglog — https://arxiv.org/pdf/2304.04332 ; scheduling https://egraphs-good.github.io/egglog-tutorial/04-scheduling.html ; PLDI 2025 tutorial https://pldi25.sigplan.org/details/pldi-2025-tutorials/4/ ; semantics https://arxiv.org/pdf/2501.02413
- e-boost — https://arxiv.org/abs/2508.13020 ; https://github.com/Yu-Maryland/e-boost
- SmoothE (ASPLOS 2025) — https://www.csl.cornell.edu/~yc2632/data/smoothe_asplos2025_final.pdf
- Treewidth extraction — https://arxiv.org/abs/2408.17042 (OOPSLA 2024)
- ASP extraction — https://arxiv.org/pdf/2606.10644
- eqsat MLIR dialect — https://arxiv.org/pdf/2505.09363 ; DialEgg — https://www.researchgate.net/publication/389479728
- Lazy clause generation / CP-SAT — https://people.eng.unimelb.edu.au/pstuckey/papers/cp09-lc.pdf ; https://people.eng.unimelb.edu.au/pstuckey/papers/lazy.pdf
- Unison — https://dl.acm.org/doi/10.1145/3332373 ; https://arxiv.org/abs/1804.02452 ; https://unison-code.github.io/
- MiniZinc — https://icaps25.icaps-conference.org/program/tutorials/minizinc/ ; instruction-selection survey https://arxiv.org/pdf/1306.4898
- Constraint-driven GEMM auto-tuning (SC'25) — https://dl.acm.org/doi/10.1145/3712285.3759898
- Target-aware real-expression implementation — https://arxiv.org/pdf/2410.14025
- dav1d / checkasm NEON — https://medium.com/@ewoutterhoeven/how-arms-neon-enables-efficient-av1-decoding-on-mobile-5fcb3a4f6e7f ; https://jbkempf.com/blog/2023/dav1d-1.2.0/
</content>
</invoke>
