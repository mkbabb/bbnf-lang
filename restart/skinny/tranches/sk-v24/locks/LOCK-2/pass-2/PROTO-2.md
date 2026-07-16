# PROTO-2 — PROVE-2-PBQP-vs-BB: is PBQP the right solver shape vs generic B&B on the near-tree scan-plan graph?

**Lock/pass/probe:** LOCK-2 (THE GRAPH ENGINE) · pass-2 · probe 2 · family 2C / 2A Island B
**Base-pin:** HEAD `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` — verified descends from `ce51c36fc`.
Worktree opened STALE at `d2b32f6e3`; fast-forwarded with `git merge --ff-only ce51c36fc` (clean). All numbers measured on `ce51c36fc`. **No wrong-artifact-risk.**
**Machine:** darwin arm64, loadavg ~9.2–9.9 at measure time (recorded in each run log).
**Verdict:** RAN, MEASURED. Prototype RUNS (not SPEC_ONLY).

---

## 1. Question

On the near-tree scan-plan selection graph — independent per-site tier choice, a few pairwise coherence edges, one global size budget — is PBQP (R0/R1/R2 reductions + Lagrangian dual of the budget) the right solver *shape* versus the crate's existing generic branch-and-bound (`solver/search.rs` + `solver/optimize.rs`) carrying the budget as `bounded_sum`? Numbers, not citations.

## 2. Method (what actually ran)

**Merge (as briefed):** the BANKED `bounded_sum.rs` (knapsack constraint, `Σ weight(var=val) ≤ capacity`, bound-consistency `revise`) was merged from the PROTO-2 bank worktree `wf_63419230-36d-30` into `crates/csp-solver/src/constraint/`. The bank was cut against a *divergent* tree (it carries `soft.rs`/`cardinality.rs`/a `Soft` enum variant that the base-pin tree does **not**); the constraint itself dropped in cleanly via the base tree's `ConstraintEnum::Custom(Box<dyn Constraint>)` escape hatch (`add_constraint(impl Constraint)`), and the banked test's `SolveConfig` was adapted to the base shape (drop `backjumping`, add `cancel: None`). Regression floor: `cargo test -p csp-solver --test bounded_sum` → **6/6 passing.**

**Probe:** `crates/csp-solver/examples/scan_plan_solve.rs` (self-contained, links the real crate). One node per site, domain `{Exact=0, Prefilter=1}`; unary = runtime cost (Exact 10, Prefilter 1) with a tier-dependent ε tie-break (<0.02 total) so the global optimum is **unique**; a coherence edge = **hard "sites sharing a dispatch table emit the same tier"** (the one coherence semantics representable *identically* in both solvers — PBQP ∞ off-diagonal, B&B a binary `SameTier` constraint); budget = `Σ static_size_cost ≤ B` (Exact 100 LOC, Prefilter 300 LOC). Synthetic per-site costs are admissible here (real-cost budget-binding is XL-D-blocked two locks deep). Three solvers on each instance:

- **Route A (PBQP):** R0/R1/R2 graph reductions + back-substitution; the global budget relaxed into the unary via a Lagrangian multiplier λ, swept (250 001 steps over [0, 0.125] — step 5·10⁻⁷, fine enough to trace every per-component relaxation breakpoint) with feasibility filtering.
- **Route B (B&B):** the crate's real `Csp<CostFiniteDomain>::solve_optimized` (`search.rs` branch-and-bound, `MinimizeCost`) with budget = `BoundedSumConstraint` and coherence = hard `SameTier`; `stats()` reports `nodes_explored`/`backtracks`.
- **Reference:** brute force over all 2ᴺ feasible tier tuples — the optimality oracle both routes are checked against.

Instances: JSON (3 self-loop sites, coherence `0-1, 0-2`) at slack/tight budgets; CSS-L4 (12 sites, coherence = a forest: path `0-1-2`, edge `3-4`, degree-3-hub tree `5-6-7,6-8`, edge `9-10`, isolated `11`) at slack/tight; plus a **K4 clique** dense control (treewidth-3, the shape that *forces* an RN fallback).

## 3. The numbers (base-pin `ce51c36fc`, release, cold, loadavg ~9.9)

| instance | budget | brute optimum (obj) | PBQP result (obj) | B&B result | PBQP reductions | PBQP µs / (per-λ) | B&B µs | nodes/backtr | ID? |
|---|---|---|---|---|---|---|---|---|---|
| json | slack 900 | `[1,1,1]` 3.000 | `[1,1,1]` 3.000 | `[1,1,1]` | R0=1 R1=2 R2=0 **RN=0** | 245 518 / **0.98** | 47.2 | 4/4 | **✓** |
| json | tight 500 | `[0,0,0]` 30.003 | `[0,0,0]` 30.003 | `[0,0,0]` | R0=1 R1=2 R2=0 **RN=0** | 247 306 / **0.99** | 20.1 | 3/4 | **✓** |
| css-l4 | tight 1800 | `[0×9,1,1,1]` 93.036 | `[0×9,1,1,1]` 93.036 | `[0×9,1,1,1]` | R0=5 R1=7 R2=0 **RN=0** | 1 139 409 / **4.56** | 90.8 | 35/39 | **✓** |
| css-l4 | slack 3400 | `[1×11,0]` **21.011** | `[0,0,0,1×9]` **39.003** | `[1×11,0]` **21.011** | R0=5 R1=7 R2=0 **RN=0** | 1 170 700 / **4.68** | 83.5 | 29/32 | **✗** |
| K4 (control) | 900 | `[0,0,0,0]` 40.006 | `[0,0,0,0]` 40.006 | `[0,0,0,0]` | R0=1 R1=1 R2=1 **RN=1** | 674 018 / **2.70** | 39.9 | 4/5 | ✓* |

`ID?` = PBQP assignment byte-identical to B&B **and** to the brute reference. `*` K4 match is incidental (RN is heuristic). `per-λ` = PBQP single-shot reduction time (µs) = total ÷ 250 001.

Two facts the fine λ-grid isolated (the coarse first pass, step 10⁻³, jumped the transition band and reported css-l4 obj 120 — that was a grid artifact, now removed):

1. **css-l4 *tight* disagreement was grid-coarseness — removable.** With a grid fine enough to trace each breakpoint the Lagrangian dual recovers the true optimum (`93.036`, identical to B&B/brute).
2. **css-l4 *slack* disagreement is the genuine Lagrangian integrality gap — irremovable.** The λ-sweep flips coherence-components to Exact in ratio/ε order, producing a *nested* family: ∅ ⊂ {0,1,2} ⊂ {0,1,2}+… . It reaches feasibility by flipping the low-ε 3-node component `{0,1,2}` (obj 39.003) and **never visits the budget-optimal singleton flip `{11}` (obj 21.011)**. No λ resolution recovers it — the 0/1-knapsack optimum is not on the Lagrangian path. PBQP is suboptimal here by 39 vs 21.

## 4. What the numbers say

- **The graph reduction is exactly the right shape — and it is trivially cheap.** Every real scan-plan instance is a forest (treewidth ≤ 2): R0/R1/R2 fully collapse it, **RN=0, zero fallbacks**, in **~1–5 µs** single-shot. The K4 control (treewidth 3) is the only instance that forces **RN=1** — confirming the reducible/dense boundary is real and that scan-plan graphs sit firmly on the reducible side. The near-tree premise is **CONFIRMED** (the "irreducibly dense" FAIL branch is *refuted*).
- **But B&B never needed help on this graph.** B&B explores **3–35 nodes** total; the near-tree structure is so slight that generic branch-and-bound with `bounded_sum` knapsack propagation dispatches it in **20–91 µs** and returns the true optimum (matching brute force) on **all five** instances, including the two the Lagrangian-PBQP got wrong or needed hair-fine λ to get right. PBQP's reduction produces **no search-space win** because B&B had no search-space problem to begin with.
- **The global budget is where PBQP's shape breaks.** PBQP is a pairwise-energy minimizer; a `Σcost ≤ B` knapsack is not a pairwise term, so it must be dualized, and the Lagrangian dual of a 0/1 budget carries an integrality gap (css-l4 slack: 39.003 vs 21.011). B&B carries the budget *natively and exactly* as `bounded_sum` bound-consistency inside the search — no relaxation, no gap. The dual wrapper is also where all of PBQP's wall-clock goes: the reduction is µs, but resolving the dual to even approach the optimum cost **0.25–1.17 s** (250 k reduction passes) — **~10⁴× slower than B&B, and still wrong on css-l4 slack.**

## 5. Verdict

**FAIL on the literal PASS clause (not byte-identical on every instance: css-l4 slack disagrees) — and the disagreement is decision-grade, resolving cleanly in B&B's favor.**

The disagreement is **not** a B&B optimality bug (B&B matches brute force on all 5); it is PBQP's Lagrangian budget-dual missing the knapsack optimum. Combined with **RN=0 / 3–35 B&B nodes** on every real instance, this trips two of the brief's three FAIL conditions at once: *"PBQP shows no reduction win on the near-tree graph (B&B suffices, PBQP is ceremony)"* and *"disagree on assignment (an optimality bug in one route)"*.

> **Route B is the solver home for the budget-coupled scan-plan problem. Route A (PBQP) is not the optimality oracle here — it is ceremony.** PBQP's R0/R1/R2 reduction is the correct *shape* for the pairwise coherence sub-structure (fully reduces the near-tree, RN=0, ~1–5 µs), but (a) the near-tree is so thin that B&B already solves it exactly in 3–35 nodes / <100 µs, and (b) the global budget — the one genuinely coupling constraint — has no pairwise encoding, so PBQP must dualize it and inherits a Lagrangian integrality gap that returns a suboptimal plan (39.0 vs 21.0). B&B carries the budget exactly as `bounded_sum` and is optimal on every instance. **2C / Island-B solver home = the existing B&B + `bounded_sum`; do not adopt PBQP-plug-in-propagator as the shared csp-solver optimizer for this problem class.** For GESTALT-B's "one solver" coordination (XL-G), this favors **B&B-native-knapsack** over **PBQP-plug-in-propagator**.

**Scope honesty.** The model uses *hard* coherence (the only coherence semantics representable identically in the base-pin tree, whose `ConstraintEnum` lacks the `Soft` variant the divergent bank carries). If coherence is instead a *finite* preference penalty (two sites *prefer* but aren't *required* to share a table), that term is native to PBQP's pairwise matrix but has **no representation in the base B&B** — B&B would need a `Soft`/`SoftLambdaConstraint` extension (which the divergent bank tree already prototypes). That is the one residual point in PBQP's favor and is the correct follow-up if finite coherence penalties turn out to be load-bearing; it does not change this verdict, because the *budget* — not coherence — is the coupling that actually stresses the solver, and the budget belongs in B&B.

## 6. Gates & deliverables

- `cargo test -p csp-solver --test bounded_sum` → **6/6** (regression floor held after merge).
- `cargo run -p csp-solver --example scan_plan_solve --release` → ran to completion (exit 0), numbers in §3.
- `xtask check-json` (regen∘regen): **not run** — changes are csp-solver-only and touch **zero** grammar/generated files, so the JSON regen surface is unaffected by construction; flagged honestly rather than asserted.
- Artifacts: this `PROTO-2.md` + `PROTO-2.patch` (merged `bounded_sum.rs`, `constraint/mod.rs` wiring, adapted `tests/bounded_sum.rs`, new `examples/scan_plan_solve.rs`).
