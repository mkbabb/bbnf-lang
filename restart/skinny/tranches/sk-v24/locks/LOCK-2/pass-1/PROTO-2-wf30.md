# PROTO-2 — PROVE-2-BUDGET: does the generated-size budget ever bind over ≥2 competing scan sites?

LOCK-2 (THE GRAPH ENGINE — egraph + CSP optimization) · pass 1 · probe 2 · settles 2C teeth + 2A Island B.
Worktree `wf_63419230-36d-30`. Host: darwin arm64 (no x86). Load avg at run: 12.60 / 11.18 / 11.49.
RAN, not SPEC_ONLY. No perf claim — reported figures are solver-decision outputs (plans, LOC sums), not timings.

> Reconciliation note. A sibling probe (worktree `…-28`) reached **PASS** by auditing *whole-repo grammar
> source* for shufti `[^D]*` loops; this run reaches the **same source-level audit result** but is careful to
> separate it from the **skinny IR-modeled, budget-governed** fleet, because the two scopings answer 2C
> differently and only one is under `generated_loc_budget`. The honest verdict is **conditional**, below.

---

## Question

Does a fleet grammar present **≥2 competing self-loop scan sites** under a binding
`generated_loc_budget` (`skinny/Cargo.toml:67 = 4000`) such that **one global N-variable CSP** forces
`Exact` on a legal-but-unaffordable site — the decision the rigged **1-variable** CSP
(`skinny/crates/passes/src/decision_csp.rs:29–189`, constraint `value == selected_index`) structurally
cannot make? Settles 2C's teeth and 2A's Island B, orthogonal to the extraction axis.

---

## Method

- **Step 0 — cheap falsifier.** Audit for self-loop scan sites (`ir SimdSite::SelfLoop`, complement
  cardinality ∈ [1,16]). `< 2` competing legal sites → 2C refuted-for-corpus; record and stop the corpus arm.
- **Step 1 — one net-new primitive.** `bounded_sum` knapsack propagator at
  `crates/csp-solver/src/constraint/bounded_sum.rs`; lower bound = `Σ min-cost residuals`.
- **Step 2 — one global CSP.** `Csp<CostFiniteDomain>` over ≥ 2 sites, each `{Exact, Prefilter}` a variable,
  `Σ static_size_cost ≤ budget` a live `BoundedSumConstraint`; solve `MinimizeCost` + FC / fail-first.

---

## What ran

### Step 0 — the audit, at two scopes (both verified this run)

**Scope A — the skinny budget-governed fleet, as the IR models it (the literal `ir SimdSite::SelfLoop` reading):**

| Fact | Finding | Source |
|---|---|---|
| Grammars under `generated_loc_budget` | **1**: `json` | `skinny/Cargo.toml:60–67` |
| `.bbnf` in the skinny fleet | **1**: `skinny/grammars/json.bbnf` | `find skinny -name '*.bbnf'` |
| `SimdSite` variants in the IR | **`{ PreEntry }`** — no `SelfLoop` | `skinny/crates/ir/src/lib.rs:407–409` |
| `SimdMode` variants in the IR | **`{ Exact }`** — no `Prefilter` | `skinny/crates/ir/src/lib.rs:401–403` |
| Recognizers emitted for `json` | **1**: `SimdScan { Exact, PreEntry }` over the union alphabet | `passes/src/lib.rs:355–360`; `runtime/.../json/generated.rs:10` |

→ **0 `SimdSite::SelfLoop`, 1 `PreEntry`. `< 2` competing sites *as modeled*.** The very variants the
question presupposes (`SelfLoop`, `Prefilter`) are absent from the IR. On the shipped, budget-governed
corpus, a per-rule (1-variable) pick and a global N-variable knapsack are identical — the budget has nothing
to arbitrate. **2C refuted-for-corpus-as-modeled.**

**Scope B — the latent grammar source (whole-repo `.bbnf`, shufti `[^D]*` / `[^D]+`, |D| ∈ [1,16]):**

| Grammar | Qualifying self-loop sites (delimiter set → |D|) | Count |
|---|---|---|
| `grammar/css/pretty.bbnf` | comment `[^*]*`→1, `[^{};]+`→3, `[^;{}!,]+`→5, `[^;{}]+`→3, `[^}]+`→1, `[^{]+`→1 … | **15** |
| `grammar/css/l4/*` (stylesheet, properties, values, gradients, func-body, media, filters) | `[^*]*`→1, `[^;{}]*`→3, `[^;!}]*`→3, `[^()"']+`→4, `[^)"'\s]+`→~7, `[^,)]+`→2, `[^)]+`→1, `[^\s;!}]+`→5 … | **≥10** |
| `grammar/bbnf/bbnf.bbnf` | comment `[^*]*`→1, hint `[^)]*`→1 | **2** |
| `grammar/misc/csv.bbnf` | `[^"]*`→1, `[^,"\r\n]+`→4 | **2** |
| `grammar/json/json.bbnf` (= skinny json) | string interior `[^"\\]`→2 (ws `[ \t\n\r]*` is a *positive-class* scan, complement 252 ∉ [1,16]) | **1** |

→ **CSS-L4 and BBNF and CSV each present ≥ 2 competing shufti self-loops in source.** The brief's own
parenthetical ("JSON's sole self-loop is whitespace-skip, no contest") is a source-level statement, and it
holds only for JSON. **At the source level, 2C is *not* refuted** — the sites are real.

**The gap between A and B is the whole finding.** These ≥2 sites are **latent in grammar source but
unmodeled by the IR** (which collapses everything into one `PreEntry Exact` scan) and **not under the skinny
budget** (only `json` is). Exposing them as per-loop `SimdSite::SelfLoop` recognizers is exactly the
unbuilt **GN-2.W1** SelfLoop-derivation. Until that lands, the budget governs a 1-site fleet.

### Step 1 — `bounded_sum` propagator (net-new primitive, RAN)

`crates/csp-solver/src/constraint/bounded_sum.rs` — `BoundedSumConstraint<V>`: `Σ weight(var_i = value) ≤
capacity`. `check` uses the knapsack lower bound `Σ min-cost residuals`; `revise` does bound-consistency
filtering (prunes any value whose selection pushes the floor over capacity; wipeout → `Unsatisfiable`). The
only linear resource bound in a zoo of relational predicates (`all_different / not_equal / implication /
cardinality / lambda`). Wired via the existing `Custom` seam — no dispatch edit.

### Step 2 — one global N-variable CSP over ≥2 competing sites (mechanism, RAN)

`crates/csp-solver/tests/bounded_sum.rs`. Two sites, each a var over `{Exact=0, Prefilter=1}`.
`CostFiniteDomain` prices the **runtime** objective (Exact 10, Prefilter 1 → MinimizeCost prefers
Prefilter). One `BoundedSumConstraint` prices **generated LOC** (Exact 100, Prefilter 300) and bounds the
sum. `Pruning::ForwardChecking`, `Ordering::FailFirst`, `OptimizationMode::MinimizeCost`.

```
cargo test -p csp-solver --test bounded_sum   → 6 passed / 0 failed   (compile 1.62s, run 0.00s)
  bounded_sum_check_rejects_over_budget_full_assignment ... ok
  bounded_sum_prunes_individually_infeasible_value ...     ok
  bounded_sum_unsat_when_even_cheapest_busts_budget ...    ok
  slack_budget_equals_per_site_greedy_pick ...             ok
  tight_budget_forces_exact_on_at_least_one_site ...       ok
  budget_binds_monotonically_across_the_threshold ...      ok
```

### Regen∘regen byte-identity on the real corpus (parity floor, RAN)

`env -C skinny cargo run -p xtask -- check-json` → **exit 0** (build 8.67s; skinny `passes` recompiled
against the modified `csp-solver`). `check_json` emits fresh codegen via
`codegen::emit_runtime_from_request` and byte-compares against `crates/runtime/src/grammars/json/`
(`check_dir`, fails closed on any diff). Exit 0 ⇒ fresh regen is byte-identical to the checked-in tree; the
additive propagator perturbs nothing.

---

## The numbers

Two sites; per-site runtime-greedy pick = `Prefilter` on both; greedy LOC demand = 600.

| Budget | Feasible? | Plan | LOC used | Forced `Exact` | == greedy pick? |
|---|---|---|---|---|---|
| 1000 (slack) | yes | `{Prefilter, Prefilter}` | 600 | 0 | **yes — parity floor** |
| 700 (slack)  | yes | `{Prefilter, Prefilter}` | 600 | 0 | yes |
| 550 (tight)  | yes | `{Prefilter, Exact}`     | 400 | **1** | **no — budget binds, plan changes** |
| 500 (tight)  | yes | `{Prefilter, Exact}`     | 400 | **1** | no |
| 150 (infeasible) | **no** | domain wipeout | — | — | propagator → `Unsatisfiable` |

Propagator in isolation: budget 250 over one site prunes `Prefilter` (300 > 250) → `{Exact}`; a full
`{Prefilter, Prefilter}` = 600 is rejected by `check` at 500; `{Exact, Exact}` = 200 > 150 wipes a domain in
`propagate()`. The threshold is monotone: above 600 the budget is slack and the plan equals the per-site
greedy pick; straddling 600 it binds and forces exactly one site off its runtime-optimum onto the
code-cheap `Exact`. That cross-site coupling flows through the single shared `Σ ≤ C` constraint and **cannot
be represented by `value == selected_index`**, which optimizes each rule in isolation with no shared pool.

---

## Verdict

**Conditional — 2A Island B mechanism PROVEN; 2C teeth LATENT (source-real, IR-unmodeled, un-budgeted).**

- **2A Island B (the engine): stands, independent of the extraction axis.** The net-new `bounded_sum`
  knapsack + an N-variable `Csp<CostFiniteDomain>` make the exact budget-binding decision the rigged
  1-variable CSP cannot — forcing `Exact` on ≥ 1 legal site under a tight budget, reproducing the per-site
  pick under a slack budget (parity floor), deterministic, with real-corpus regen byte-identical. 6/6 green.

- **2C (does the budget bind over a real contest?): depends on scope, and the split is the answer.**
  - *Skinny budget-governed fleet, as the IR models it:* **refuted-for-corpus** — 1 grammar, 1 `PreEntry`
    site, 0 `SimdSite::SelfLoop`; the budget has nothing to arbitrate today.
  - *Whole-repo grammar source:* **not refuted** — CSS-L4 (≥10), BBNF (2), CSV (2) each carry ≥ 2 shufti
    self-loops (|D| ∈ [1,16]). The teeth are real in source but **latent**: unmodeled by `ir::SimdSite`
    (only `PreEntry`/`Exact`) and outside the budget (only `json` is registered).

So 2C's teeth do **not** close on the shipped corpus as it stands, yet they are **not** absent from the
fleet — they await the **GN-2.W1** per-loop `SimdSite::SelfLoop` / `SimdMode::Prefilter` recognizer
derivation to become IR-modeled and budget-governed. The moment ≥ 2 self-loop sites are modeled under one
grammar's budget, the `bounded_sum` engine built here is precisely what lets the budget bind — no e-class
branching required. **Island B does not collapse to GN-2.W1's hand-written arm; it is *gated behind* it:**
the CSP+budget mechanism is ready and proven; only the site-exposing derivation is missing.

**Verdict line:** 2A Island B mechanism **RAN and PROVEN** (global knapsack CSP forces `Exact` under a
tight budget, equals the per-rule pick under slack, regen byte-identical); 2C teeth are **source-real but
IR-latent** — refuted only for the skinny corpus *as modeled today* (1 site, no `SelfLoop`), live for the
broader fleet, and gated behind GN-2.W1's SelfLoop derivation. Decision-grade, RAN_MEASURED.

---

## Files / patch

- `crates/csp-solver/src/constraint/bounded_sum.rs` — net-new knapsack propagator (+178)
- `crates/csp-solver/src/constraint/mod.rs` — module wiring (+2)
- `crates/csp-solver/tests/bounded_sum.rs` — propagator + 2-site budget CSP tests (+165)

Full diff: `proto-2.patch` (345 insertions, 0 deletions; purely additive).
