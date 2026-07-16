# LOCK-2 — THE GRAPH ENGINE — pass-1 PRIOR-ART SWEEP

*e-graph + CSP optimization: HIR canon, IR rewriting, kernel/backend selection, capacity/schedule
decisions, profile-fed cost, deterministic regen. Families 2A EGRAPH-ISLANDS · 2B IR-IS-EGRAPH ·
2C CSP-CENTRAL · 2D PROFILE-CLOSED-LOOP.*

Constitution binds: consumer-less substrate forbidden (viii/Y.13); type plane costs zero at runtime
(v); concrete artifacts or SPEC-ONLY (vii); refutation laws (iv); extant GN/PT codex is the baseline
(ix). No family allegiance — this is a class-wide sweep.

---

## 1. WHAT EXISTS IN-TREE (the seed, measured by `wc`/read)

| Component | Path | LOC | What it is |
|---|---|--:|---|
| e-graph core | `crates/egraph/src/egraph.rs` | 297 | egg-style: union-find + hashcons `memo` + Willsey **rebuild** (worklist of pending classes, deferred congruence, upward parent merge); `union_count` work-metric |
| Language trait + derive | `egraph/src/language.rs`, `egraph-derive/src/lib.rs` | 114+343 | e-nodes as `children()->&[Id]`; derive projects `Vec/Box/Self` fields → `Id` (memory: derive-language-macro) |
| Extractor | `egraph/src/extract.rs` | 215 | **greedy bottom-up, TREE cost** — fixed-point `compute_best`; `Lattice`/`CostModel` traits with `Scalar<T>` Pareto-ready marker but **only scalar used**; `AstSize` default |
| CSP scheduler | `egraph/src/csp_scheduler.rs` | 368 | models "which classes dirty this iter" as CSP: monotone `DirtyDomain` lattice + `ParentDirtyProp` AC-3 closure; **rebuilds a fresh `Csp` every iteration** (:229) |
| BackoffScheduler | `egraph/src/scheduler.rs` | 116 | the egg backoff baseline the CSP one competes against |
| Cost config/weights | `egraph/src/cost_config.rs`, `cost_weights.rs` | 119+191 | single source of truth; 13 dims (structural, alt_per_branch, dispatch_bonus, call/inline, tape_push, dispatch_branch/table, prettify, cross_module); `CALIBRATED_WEIGHTS` |
| CSP solver engine | `crates/csp-solver/src/solver/` | ~1400 | AC-3 propagate, unified `branch_and_bound` kernel (`Feasibility`\|`BranchBound` zero-sized policy, :78), `DomainCostEval` (optimize.rs), `LatticeDomain`, monotonic solver; vendored from csc411 |
| Decision-driver seam | `ir/src/cost.rs`, `passes/src/lib.rs` | — | `ActiveCostFacts` (egraph/CSP selection provenance), `DecisionCspFacts`, `BackendShape` (5 shapes), `derive_backend_shape` (:398), `derive_recognizers` (:338) — per R1 §8 |

The engine is real and dual-tier (grammar IR + regex HIR share `CostWeights`). The decision driver
already exists as a seam: `passes::derive_backend_shape` selects over `BackendShape` via
`BackendShapePlan.cost_facts`; `ActiveCostFacts` records e-graph/CSP provenance. This is the
substrate 2A/2C would formalize, not invent.

---

## 2. THE EVIDENCE BASE ALREADY IMPOSES (internal refutations — binding)

**R-1 · THE INERT COST MODEL (the null calibration, in-tree, `cost_weights.rs:134-191`).**
A 54-configuration grid sweep over {structural, alt_per_branch, dispatch_bonus, ref_cost} on the
4-grammar corpus produced a **DTA state count IDENTICAL to baseline for every grammar** (JSON 51,
BBNF 476, CSS-L4 2875, Sheets 191 — zero variance across all 54). Documented root cause: the current
rewrite set (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor`) produces e-classes with **"at most one semantically-distinct canonical form"**,
so *the cost model has no choice to exercise*. **Extraction is inert today.** This is decisive for
LOCK-2: any 2A/2C/2D route claiming cost-model-governed selection is a **consumer-less substrate
(viii/Y.13 violation) until a rewrite manufactures a real e-class branch.** Richer extraction (ILP,
Pareto, differentiable) solves a problem the tree has measured it does not yet have.

**R-2 · profiling law (SPEC §0 / §4).** Ratios certified, absolutes directional; PMU cyc/B, paired,
cold N≥200, multi-window; warm benches are disinformation. 2D's "cost coefficients fitted from PMU"
must land as **committed fact tables** — SPEC §2C/2D already says "committed as fact tables
(determinism preserved)".

**R-3 · determinism (constitution v + regen-only law + memory: changed-flag-convergence).** Fixed-point
loops use an LLVM `Changed` bool (the CSP scheduler's `total_delta`/`union_count` metric already does,
`csp_scheduler.rs:273`, `rewrite.rs:91`), content hash as `debug_assert` only. Profile-fed weights
that vary regen output break bit-for-bit reproducibility — the hard GN-1 gate. Frozen fact tables are
the only lawful form.

**R-4 · consumer-in-wave (Y.13, GN-2 charter).** GN-2.W1 already names the concrete first island +
consumer: `SimdMode::Prefilter` recognizer (`ir/src/lib.rs:425`) → `find_member` lowering
(`codegen/src/lower/recognizer/`) → benched grammar scan site. LOCK-2 must route through this seam,
not orthogonal machinery (memory: no-orthogonal-codepaths, system-cohesion).

---

## 3. WHAT WON / LOST ELSEWHERE (external prior art, cited)

**Cranelift ægraph — the production verdict against full saturation (most relevant).**
Fallin (EGRAPHS 2023; blog 2026-04-09) rebuilt Cranelift's mid-end on an **acyclic e-graph** that
**deliberately rejects equality saturation**: eager rewriting at node construction + immutable union
nodes + acyclicity (FLAX) → **no rebuild, no parent pointers, single pass**. Extraction is
single-pass bottom-up DP that **ignores sharing to dodge NP-hardness**. The measured facts corroborate
our R-1 exactly: **average e-class size after rewriting = 1.13 e-nodes**; the multi-option layer
contributes **~0.1% execution improvement at ~0.005% compile cost** — *"the ægraph itself may not
(yet?) be pulling its weight"*; the **sea-of-nodes canonicalization (GVN/LICM/CSE/DCE "for free")
does the heavy lifting.** Full saturation "needs a meta/strategy driver with fuel to bound effort."
→ **Trap for 2B (IR-IS-EGRAPH):** the win is canonicalization + hash-consing, NOT extraction search;
budget a fuel/strategy layer or blowup kills the compile budget.
[cfallin.org/blog aegraph](https://cfallin.org/blog/2026/04/09/aegraph/) ·
[EGRAPHS 2023 slides](https://cfallin.org/pubs/egraphs2023_aegraphs_slides.pdf)

**egg (Willsey et al., POPL 2021).** The rebuild/amortized-invariant-restoration + deferred congruence
our `egraph.rs` mirrors; the **BackoffScheduler** (ban rules that explode match counts) is the baseline
our `CspScheduler` must beat. [egg POPL21](https://arxiv.org/abs/2004.03082)

**Extraction is NP-hard/NP-complete (DAG cost).** Confirmed everywhere. Prior art ladder: ILP (optimal,
doesn't scale) → **treewidth-parameterized** (Goldstein et al., "E-graphs as Circuits", OOPSLA/2024 —
optimal up to treewidth ~10, evaluated on the *Cranelift* suite) → **e-boost** (2025, 558× over ILP via
warm-start exact + parallel heuristics) → **SmoothE** (ASPLOS 2025, differentiable/gradient extraction);
**Extraction Gym** is the 2024 benchmark. Our `Extractor` is *greedy tree-cost* — below even DAG-greedy,
matching Cranelift's pragmatic choice.
[Fast&Optimal Extraction OOPSLA24](https://cse.hkust.edu.hk/~parreaux/publication/oopsla24a/) ·
[e-boost](https://arxiv.org/abs/2508.13020) · [SmoothE](https://www.csl.cornell.edu/~yc2632/data/smoothe_asplos2025_final.pdf)

**Constraint-based codegen (2C prior art).** Unison / "Modeling universal instruction selection"
(Castañeda Lozano et al., CP 2015) unifies instruction selection + register allocation + scheduling as
one constraint model — the strongest 2C precedent, but **notoriously slow** (seconds–minutes/function).
Phase-ordering-as-constraints (Zucker, "Compiling with constraints"); MileStone (2026, multi-objective
phase ordering over graph IR). → **Trap for 2C:** full constraint-model codegen can be orders too slow
for a per-compile budget; LOCK-1 already scopes CSP to "bounded post-solve where unification is
undecidable" — LOCK-2's CSP must inherit that bound.
[Universal instr-sel CP15](https://dl.acm.org/doi/10.5555/3102787.3102831)

**Profile-guided cost (2D prior art).** HW-counted PGO (PMU sampling → frequency profiles) is standard;
the reproducibility literature is explicit that profile-fed codegen breaks deterministic disassembly
unless the profile is frozen. → confirms R-3: **commit PMU-fitted coefficients as static fact tables.**

---

## 4. TRAPS (named, for the synthesis)

1. **Inert-cost-model trap (R-1, corroborated by aegraph 1.13/0.1%).** Building richer extraction
   before rewrites manufacture real alternatives is a Y.13 substrate-without-consumer. *First prove
   an e-class holds >1 candidate; then a cost model matters.*
2. **Saturation-blowup trap (aegraph).** Full IR-as-egraph needs fuel. The `CspScheduler` allocates a
   **fresh `Csp` per iteration** (O(classes+edges), `csp_scheduler.rs:229`) — the opposite of aegraph's
   no-rebuild lesson; a scaling risk at CSS-L4 (2875 states). Measure it, or adopt eager-rewrite.
3. **Extraction-NP trap.** Don't promise DAG-optimal extraction; greedy tree-cost is what ships and what
   Cranelift ships. ILP/CSP extraction is unjustified until classes branch and treewidth is bounded.
4. **Determinism trap (R-3).** Profile-fed weights break regen bit-reproducibility unless frozen into
   committed fact tables. This is a hard GN gate, not a footnote.
5. **Unison trap (2C).** Constraint-model codegen can blow the compile-time budget; CSP must be a
   bounded post-solve, not the primary driver.
6. **Elegance-over-measurement trap (aegraph 0.1%).** A correct e-graph layer can still be negligible.
   Every island must close on a *measured same-wave consumer win* (GN-2.W1 born-RED style), not
   architectural symmetry.

---

## 5. BEARING ON THE FOUR FAMILIES

- **2A EGRAPH-ISLANDS** — best-aligned with the evidence: directed pipeline, e-graph only at named
  decision points, each with a same-wave consumer + cost model. The GN-2.W1 prefilter seam is a
  ready first island. Survives R-1 *only if the island's e-classes actually branch*.
- **2B IR-IS-EGRAPH** — the aegraph verdict is a direct refutation of "full-saturation IR pays";
  salvageable ONLY as the aegraph shape (eager rewrite + canonicalization for free), not classical
  saturation. Carries the blowup + rebuild-cost risk.
- **2C CSP-CENTRAL** — strong prior art (Unison) but the speed trap + the inert-cost-model finding
  bound it to a *bounded post-solve over tiny candidate sets*, cohered with LOCK-1's CSP scoping.
- **2D PROFILE-CLOSED-LOOP** — lawful only as committed fact tables (R-2/R-3); the loop must be
  offline-fit, frozen-at-regen, determinism-preserving.

---

## 6. THE ONE PROTOTYPE THAT SETTLES PASS-1

Author **one rewrite rule that manufactures a genuine e-class alternative** on a real grammar (the
canonical case: dispatch-table vs sequential-trial for a JSON/CSS `Alt`, the two forms `CostWeights`
already prices via `dispatch_branch`/`dispatch_table`/`alt_per_branch`), then show the `Extractor`
picks **differently** under two weight vectors AND that the two picks produce a **measured cyc/B delta**
on `json_canon_bench`/`css_canon_bench`. That single artifact either (a) refutes R-1's inertness for our
workload and de-risks all of 2A/2C, or (b) confirms the aegraph "0.1%" warning holds here too — in which
case LOCK-2 collapses to canonicalization + the existing `BackendShape` driver, and the extraction/CSP
machinery is retired to SPEC-ONLY. Either outcome is decision-grade.
