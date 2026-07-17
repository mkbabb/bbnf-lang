# PROTO-1 — PROVE-2-ALTCOST-CROSSOVER (LOCK-2 pass-2, probe 1)

Family 2A EGRAPH-ISLANDS master gate. Resolves extraction-as-decider
for 2A-I2/I4, 2B, and 2D.

- **BASE-PIN:** worktree HEAD = `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`,
  verified `git merge-base --is-ancestor ce51c36fc HEAD` → OK (HEAD *is*
  the base commit; no fast-forward needed). All numbers measured on this
  HEAD.
- **Tree:** `crates/ir/src/egraph/cost.rs` (the only `GrammarCostModel`
  in the tree — three-tree caveat checked: `find . -path '*egraph/cost.rs'`
  returns exactly one).
- **Profile:** `--profile ax-iter`.
- **loadavg (recorded):** 17.37 at open → 6.98 at close. High throughout;
  bears on the PMU half (below).

---

## QUESTION

PROTO-1's widening rewrite (`ManufactureDispatchAlt`) manufactures ≥2
confluent, terminating survivors at a child-bearing `Alt` through
`CspScheduler`, but the extractor picked identically across the whole
calibrated weight space — only the *forbidden* `dispatch_bonus` SIGN flip
changed the pick. Root cause (verified on disk, `cost.rs:109`): the `Alt`
arm was `base + dispatch_bonus if Some else base` — a scalar constant in
N. The three coefficients a real crossover needs already ship calibrated
(`cost_weights.rs`: `alt_per_branch=1.5`, `dispatch_branch=0.5`,
`dispatch_table=3.0`) but the arm never read them.

Does rewiring the arm to net **DISPATCH = dispatch_table + N·dispatch_branch**
vs **SEQUENTIAL = N·alt_per_branch** make a SINGLE fixed calibrated vector
pick DISPATCH at large N and SEQUENTIAL at small N, crossing at
**N\* = dispatch_table / (alt_per_branch − dispatch_branch) = 3.0**?

---

## METHOD

Two-line rewire of the `Alt` arm (`cost.rs:109`), reading the shipping
coefficients instead of the flat bonus:

```rust
GrammarENode::Alt(children, dispatch) => {
    let child_sum = children.iter().map(|&id| child_cost(id)).sum();
    let n = children.len() as f64;
    let shared = structural + child_sum;          // identical for both forms
    if dispatch.is_some() {
        shared + self.weights.dispatch_table + n * self.weights.dispatch_branch
    } else {
        shared + n * self.weights.alt_per_branch
    }
}
```

Because `shared = structural + child_sum` is identical for the two forms
(they carry the same children), the extractor's pick reduces to the sign
of

    Δ(N) = dispatch_table + N·(dispatch_branch − alt_per_branch)
         = 3.0 + N·(0.5 − 1.5) = 3.0 − N        →  N* = 3.0

New test `crates/ir/tests/egraph/alt_cost_crossover.rs` (registered in
`tests/egraph/main.rs`) reuses PROTO-1's `ManufactureDispatchAlt` + the
shipping all-narrowing set (`DeduplicateAltBranches`, `SupersetAbsorbAlt`,
`UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`) through
`CspScheduler::default()`. It builds N-branch child-bearing Alts
(`Seq([Literal(sentinel), Ref(distinct)])`, pairwise-disjoint FIRST bytes),
saturates, asserts both forms survive (confluence), then extracts under
**one unmodified `CALIBRATED_WEIGHTS`** vector (the test asserts
`weights == CALIBRATED_WEIGHTS` — no per-test tuning permitted).

Command: `cargo test -p bbnf-ir --test egraph --profile ax-iter alt_cost_crossover -- --nocapture`

---

## WHAT RAN — THE NUMBERS

Extraction under ONE fixed `CALIBRATED_WEIGHTS` vector (both tests GREEN,
`2 passed; 0 failed`):

| N | Δ(N) | dispatch_cost | sequential_cost | extractor pick |
|---|-----:|--------------:|----------------:|----------------|
| 2 | +1.0 | 14.00 | **13.00** | SEQUENTIAL |
| 3 |  0.0 | 19.00 | 19.00 | SEQUENTIAL (tie → min) |
| 4 | −1.0 | **24.00** | 25.00 | DISPATCH |
| 5 | −2.0 | **29.00** | 31.00 | DISPATCH |
| 6 | −3.0 | **34.00** | 37.00 | DISPATCH |
| 7 | −4.0 | **39.00** | 43.00 | DISPATCH |
| 8 | −5.0 | **44.00** | 49.00 | DISPATCH |

- **2-branch → SEQUENTIAL, 8-branch → DISPATCH under the SAME vector.**
- Monotone single flip across N=2..8; crossover sits exactly at
  **N\* = 3.0** (tie at N=3, DISPATCH from N=4 on).
- The pick flips **without touching `dispatch_bonus`** — PROTO-1's
  sign-only refutation is itself refuted. The calibrated
  `dispatch_table`/`dispatch_branch`/`alt_per_branch` coefficients now
  carry a genuine structural N-crossover.

`tests/egraph/widen_confluent.rs` (PROTO-1) remains alongside — the
manufactured twin stays confluent and terminating; the rewire only
re-prices it.

### Regen∘regen (regen ×2) — my change is regen-NEUTRAL

`cargo run -p xtask --features grammar-regen -- regen --check`:

- **With my rewire:** 3 of 9 grammars drift (css_l4, google_sheets, bnf).
- **cost.rs reverted to HEAD, same check:** the **same 3** grammars drift.

Identical drift set with and without the change ⇒ the drift is
**pre-existing worktree staleness** (committed generated files lag a fresh
regen), NOT introduced by this probe. Mechanism confirms it: the
SEQUENTIAL (`None`) form's cost is **byte-identical** to the old `base`
(`structural + N·alt_per_branch + child_sum`); only the `Some` form's
price changed; production ships no manufactured dispatch twin
(`ManufactureDispatchAlt` appears nowhere in `crates/*/src` outside its
own definition file), so production extraction has no `None`/`Some` choice
point and its picks — hence generated output — are unchanged by the
rewire. The 6 non-drifting grammars (json, bbnf, css_pretty, csv, math,
ebnf) confirm byte-stability where the checked-in files were already fresh.

---

## THE PMU HALF — UNMEASURED (blocked)

The gate's second clause ("both lowerings' cyc/B 95% CI excludes 1.00×
same-direction") was **not measured**. Blocks, all recorded honestly:

1. **`css_canon_bench` does not exist** in this tree (`find crates -name
   'css_canon_bench*'` → empty; benches present are compile_pipeline,
   json_callgrind, css/*, google_sheets/*, json/*). The brief's bench
   command is a stale reference.
2. **No scriptable PMU on this host:** `perf` absent (macOS), `instruments`
   absent, only SIP-restricted `dtrace`. cyc/B and load-invariant ins/B
   counters are not obtainable from bash without entitlements.
3. **loadavg 6.98–17.37** throughout — cold-paired *timing* would be
   noise-dominated; recording the load would caveat any cyc/B claim to
   worthlessness.
4. **No direct synthetic-EGraph → runnable-parser lowering** exists; real
   lowering runs the full grammar compile pipeline, not a hand-built
   `EGraph`. Building a bespoke cold-paired PMU harness for a synthetic
   8-branch Alt was not parsimonious inside the 50-min cap.

No PMU numbers were fabricated. The runtime superiority of a 128-entry
table dispatch over sequential trial at N=8 is a well-established
microarchitectural fact, but it is **not** the novel claim under test and
is not asserted here as measured.

---

## VERDICT

**The structural crossover — the master question — is PROVEN and RAN
deterministically.** One fixed, unmodified `CALIBRATED_WEIGHTS` vector
yields 2-branch → SEQUENTIAL and 8-branch → DISPATCH, crossing at
N\* = 3.0, driven by the calibrated coefficients and **not** by the
forbidden `dispatch_bonus` sign. PROTO-1's sign-only finding is refuted;
the fix is exactly the documented two-line rewire to existing weights, and
it is regen-neutral (no generated-file change attributable to it).

**The runtime-confirmation half (cyc/B CI excludes 1.00×) is UNMEASURED**,
blocked by a nonexistent bench target, absent PMU tooling on darwin-ARM,
pathological loadavg, and no synthetic→runnable lowering path.

**Bottom line for the parent:** the *structural precondition* for
extraction-as-decider is EARNED with a green deterministic test — the cost
model now discriminates the two Alt forms on N, which is exactly what
2A/2B/2D were blocked on. The full PASS gate is not closed only because
its PMU clause was not measurable in this environment; that clause needs a
real lowering + PMU host (perf/instruments, quiet machine) to close.
Decision recommendation: treat the structural half as EARNED and route the
PMU clause to a probe with PMU access, rather than declaring the gate
REFUTED — refutation would require a *failed* crossover or a *sign-only*
result, and **neither occurred**.

**Verdict line: RAN_UNMEASURED** — structural crossover PROVEN (green,
deterministic, single fixed calibrated vector, N\*=3.0); PMU cyc/B clause
blocked/unmeasured.

---

## ARTIFACTS

- `PROTO-1.patch` — the two-line `cost.rs` rewire + the new
  `alt_cost_crossover.rs` test + the `main.rs` mod registration. (Reuses
  PROTO-1's `widen.rs` / `widen_confluent.rs`, already present in the
  worktree; not re-attributed here.)
- Test output verbatim (both green):
  ```
  [CROSSOVER] N=2 Δ=+1.0 dispatch=14.00 sequential=13.00 pick=SEQUENTIAL
  [CROSSOVER] N=4 Δ=-1.0 dispatch=24.00 sequential=25.00 pick=DISPATCH
  [CROSSOVER] N=8 Δ=-5.0 dispatch=44.00 sequential=49.00 pick=DISPATCH
  test alt_cost_crossover::alt_cost_crossover_single_vector_crosses ... ok
  test alt_cost_crossover::alt_cost_crossover_brackets_n_star_at_three ... ok
  test result: ok. 2 passed; 0 failed
  ```
