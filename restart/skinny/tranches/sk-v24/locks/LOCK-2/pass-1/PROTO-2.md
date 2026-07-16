# PROTO-2 — PROVE-2-BUDGET — does the generated-size budget ever bind over ≥2 competing scan sites?

LOCK-2 (THE GRAPH ENGINE) · pass-1 · probe 2 · family 2C CSP-CENTRAL + 2A Island B
Verdict: **PASS** (RAN, decision-grade). Runs in worktree `wf_63419230-36d-28` (csp-solver
@ d2b32f6e3). loadavg at run ≈ 2.4. No perf claim — the cyc/B are synthetic cost *inputs*, not
measurements; the reported numbers are solver-decision outputs (plans, LOC sums, node counts).

## Question

Does a fleet grammar present ≥2 *competing* self-loop scan sites under a binding
`generated_loc_budget`, such that a real N-variable CSP forces `Exact` on a legal-but-unaffordable
site — the decision the rigged 1-variable CSP (`decision_csp.rs:29-189`, forces `value ==
selected_index`) structurally cannot make? Settles 2C's teeth and 2A's Island B, orthogonal to
extraction (Brief 1).

## Method

- **Step 0 (cheap falsifier).** Audit the fleet grammars for negated-class self-loop scan sites —
  a star `[^D]*` whose delimiter set `D` (the scanned class's complement) has cardinality ∈[1,16],
  i.e. a shufti-able "scan-to-delimiter" loop. `<2` competing legal sites anywhere → 2C
  refuted-for-corpus, stop.
- **Step 1.** Net-new `bounded_sum` knapsack propagator at
  `crates/csp-solver/src/constraint/bounded_sum.rs`; lower bound = `Σ_k min_live_weight(k)`.
- **Step 2.** ONE global `Csp<CostFiniteDomain>` over ≥2 sites; each site's `{Prefilter, Exact}` a
  variable; `Σ static_size ≤ generated_loc_budget` a live `BoundedSumConstraint`; solve with
  `OptimizationMode::MinimizeCost` + `Pruning::Ac3` (invokes the propagator's `revise`) +
  `Ordering::FailFirst`.

## What ran

```
cargo test -p csp-solver --test bounded_sum   → 7 passed / 0 failed
cargo test -p csp-solver                      → 111 passed / 0 failed (no regression; CARGO_EXIT=0)
cargo run  -p csp-solver --example scan_plan_budget   → plans below
```

## The numbers

### Step 0 — self-loop scan-site audit (grammar source, `[^D]*`, |complement| ∈[1,16])

| Grammar | Competing self-loop sites (delimiter set, cardinality) | Count |
|---|---|---|
| **JSON** | string `[^"\\]*` {`"` `\`}=2. (ws `[ \t\n\r]*` & digit runs are *positive-class* membership scans, complement 252/246 ∉[1,16] — not shufti delimiter sites.) | **1** |
| **Google-Sheets** | string `[^"]*` {`"`}=1 | **1** |
| **BBNF** | comment `[^*]*` {`*`}=1 · string `[^"\\]*` {`" \`}=2 · hint `[^)]*` {`)`}=1 | **≥3** |
| **CSS-L4** | string `[^"\\]*`=2 · genericDecl `[^;!}]*`=3 · selectorSpan `[^{};]+`=3 · valueSpan `[^;{}!,]+`=5 · comment `[^*]*`=1 · mediaFeatureVal `[^)]+`=1 · funcBody `[^()"']+`=4 · urlFunction `[^)"'\s]+`≈7 · gradients `[^,)]+`=2 · genericAtRule `[^;{}]*`=3 … | **≥10** |

The §0 counterexample ("JSON's sole self-loop is whitespace-skip, no contest") holds *only for
JSON* (which has exactly 1 qualifying site — the string interior, not whitespace). **CSS-L4 and
BBNF each present ≥2 competing legal sites.** Step 0 does **not** refute 2C-for-corpus.

Honest asterisk: today's `passes::recognizers::derive_recognizers` (`skinny/crates/passes/src/lib.rs:338`)
collapses every structural byte into **one** `SimdScan{ mode: Exact, site: PreEntry }` per grammar
— `ir::SimdSite` has only `PreEntry`, `ir::SimdMode` only `Exact` (`ir/src/lib.rs:424-432`). So the
sites above are *latent in grammar source but unmodeled by the IR*. The per-loop `SelfLoop`
recognizer-derivation that would expose them is exactly the GN-2.W1 consumer Island B / 2C drive;
the CSP was therefore built over the audited real sites (brief-sanctioned: "construct/target a real
`.bbnf`").

### Step 2 — global scan-plan CSP (5 CSS-L4 sites; Prefilter=40 LOC/fast, Exact=8 LOC/slow)

```
per-rule greedy pick (each site min-runtime independently) = [Prefilter×5] = 200 LOC

TIGHT budget = 120 LOC   nodes_explored=12
  string          Prefilter | genericDecl     Prefilter | selectorSpan    Exact
  comment-body    Exact     | mediaFeatureVal Exact
  Σ static_size = 104 ≤ 120   Σ runtime = 590

SLACK budget = 220 LOC   nodes_explored=9
  string Prefilter · genericDecl Prefilter · selectorSpan Prefilter · comment Prefilter · media Prefilter
  Σ static_size = 200   Σ runtime = 500
```

| Gate (2C / Island B) | Result |
|---|---|
| Tight budget forces `Exact` on ≥1 legal site (HARD solve-time constraint) | **3 sites forced** (the 3 coolest); Prefilter kept on the 2 hottest |
| Tight plan honors budget | 104 ≤ 120 ✓ |
| Greedy per-rule busts tight budget → global CSP strictly dominates | 200 > 120 ✓ (global emits the only legal plan) |
| Slack plan byte-identical to per-rule pick (parity floor) | `[Prefilter×5] == greedy` ✓ |
| Solve determinism (regen∘regen proxy) | solve twice ⇒ identical at budgets 120/150/220 ✓ |
| Budget binds monotonically | Exact-count 0→5 as budget 220→40, non-decreasing ✓ |

## Verdict

**PASS — decision-grade.** ≥2 competing self-loop scan sites (|complement| ∈[1,16]) exist in the
real fleet (CSS-L4 ≥10, BBNF ≥3; JSON/Sheets =1). A single global `Csp<CostFiniteDomain>` with the
net-new `bounded_sum` knapsack as a live constraint **forces `Exact` on 3 legal-but-unaffordable
sites under a tight budget** (the decision the 1-var CSP cannot make) and is **byte-identical to the
per-rule pick under a slack budget**; the solve is deterministic. **2C's teeth exist on the corpus;
2A's Island B stands on CSP+budget with no e-class branching** (orthogonal to Brief 1).

Scope carried forward (not this probe's to close): the per-loop `SimdSite::SelfLoop` /
`SimdMode::Prefilter` recognizer-derivation and its codegen consumer are unbuilt (GN-2.W1); full
`cargo xtask regen` is therefore not exercised (no consumer reads the plan yet) — determinism is
shown at the CSP decision layer, where regen∘regen would inherit it. Real static-size weights and
per-site cyc/B must come from 2D's fitted table, not the synthetic constants used here.

## Deliverables / patch

`crates/csp-solver/src/constraint/bounded_sum.rs` (+168, the propagator) ·
`constraint/mod.rs` (+2, module wiring) · `tests/bounded_sum.rs` (+183, 7 tests) ·
`examples/scan_plan_budget.rs` (+114, the printing bin). +467 LOC, 0 deletions, additive; wired via
the existing `Custom` constraint seam (no dispatch edit). Full diff: `PROTO-2.patch`.
