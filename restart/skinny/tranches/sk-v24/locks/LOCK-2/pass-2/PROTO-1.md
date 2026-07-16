# PROTO-1 — PROVE-2-WIDEN-CONFLUENT

**LOCK-2 (THE GRAPH ENGINE — egraph + CSP optimization), pass-2, probe 1.**
Family 2A / 2B / 2D master gate (§0). Decision-grade.

- **Base pin:** required ancestor `ce51c36fc`.
- **HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` — the worktree started at
  `d2b32f6e3`, which is a linear *ancestor* of the base pin, so `git merge --ff-only ce51c36fc`
  fast-forwarded cleanly. **HEAD == base pin exactly. No wrong-artifact-risk.**
- **loadavg at measurement:** `5.40 6.54 6.63` (host up 3 days). `--profile ax-iter` throughout.

---

## 1. Question

On a real fleet-shaped grammar in the skinny measurement substrate, does a **registered**
widening `Rewrite` — fired through `CspScheduler`, **not** hand-unioned — manufacture ≥2
semantically-distinct survivors at a **child-bearing** node, stay **confluent** with the
all-narrowing set (does `DeduplicateAltBranches` collapse the twin?), **terminate**, such that
`Extractor` picks differently under two **calibrated** weight vectors (W2 from `CALIBRATED_WEIGHTS`,
**not** a sign-flip), and the two lowerings move cyc/B at real value-start positions?

This is the exact step PROTO-1 (pass-1) declared "trivial" and left unbuilt: it hand-unioned, on
the legacy tree, with a synthetic +2.0/−2.0 sign-flip. This probe **builds** the rule.

## 2. Method (what actually ran)

Real production code, not a scratch harness:

- **`crates/ir/src/egraph/rules/widen.rs` — `ManufactureDispatchAlt`** (new, 154 LOC). A genuine
  `Rewrite<GrammarENode, A>`: for `Alt(children, None)` whose branches expose pairwise-disjoint
  FIRST bytes, `search` builds a 128-entry `AltDispatch` table (byte→branch idx) and `apply` does
  `egraph.add(Alt(children, Some(dispatch)))` + `union`. Exported from `rules/mod.rs`. It matches
  **only** the `None` form — that guard is what makes it terminate under the hash-cons.
- **`crates/ir/tests/egraph/widen_confluent.rs`** (new, 2 tests). Builds a JSON-`value`-shaped
  alternation: six sentinel-prefixed branches `Seq([Literal(b), Ref(rule)])` with FIRST bytes
  `{ [ " t f n` — a **child-bearing** `Alt` (branches are `Seq`s that themselves bear children).
  Registers the **shipping all-narrowing set** (`DeduplicateAltBranches`, `SupersetAbsorbAlt`,
  `UnionMergeAlt`, `FuseAltRegexBranches`, `CommonSuffixFactor`) **plus** `ManufactureDispatchAlt`,
  and drives them through `CspScheduler::default()`.
- Command: `cargo test --profile ax-iter -p bbnf-ir --test egraph widen_confluent -- --nocapture`.
  **Both tests pass (exit 0).**

## 3. The numbers (measured)

### egraph limb — ≥2 confluent terminating survivors  →  PASS

```
[PROBE] saturated=true  iters=2/64  final_nodes=20/100000  final_classes=19  total_applied=2
[PROBE] per_rule = [("deduplicate-alt-branches",0), ("superset-absorb-alt",0),
                    ("union-merge-alt",0), ("fuse-alt-regex-branches",0),
                    ("common-suffix-factor",0), ("manufacture-dispatch-alt",2)]
```

- **(a) ≥2 distinct survivors at a child-bearing node — YES.** Root `Alt` class holds exactly
  `Alt(cs, None)` **and** `Alt(cs, Some(dispatch))` (`none_forms==1`, `some_forms==1`, asserted).
- **(b) confluence — YES.** The widening rule did 2 units of work **through the scheduler**;
  every narrowing rule did **0** (ledger above). `DeduplicateAltBranches` ran every iteration and
  did **not** collapse the twin — the 6-branch dispatch form survives intact
  (`some_branch_len == Some(6)`, asserted). Dedup keys on the branch set, identical across the
  twin; the dispatch table is invisible to it.
- **(c) termination — YES, wide margin.** `saturated=true`; `iters=2/64`; `final_nodes=20/100000`.

### extraction limb — W1 vs W2  →  FAIL (picks identical across the calibrated space)

```
[PROBE-EXTRACT] dispatch_bonus=-2.0  alt_per_branch=1.5  -> picked=DISPATCH     best_cost=35.0
[PROBE-EXTRACT] dispatch_bonus=-2.0  alt_per_branch=8    -> picked=DISPATCH     best_cost=555.0
[PROBE-EXTRACT] dispatch_bonus=+2.0  alt_per_branch=1.5  -> picked=SEQUENTIAL   best_cost=37.0
```

- **W1 = `CALIBRATED_WEIGHTS`** (`dispatch_bonus=-2.0`): extracts **DISPATCH**, cost 35.0.
- **W2 = every non-bonus knob maxed, bonus sign KEPT** (`alt_per_branch=8`, `structural=5`,
  `dispatch_table=100`, `dispatch_branch=50`, `ref_cost=25`, `seq_per_child=12`,
  `literal_cost=30`): **still DISPATCH**, cost 555.0. **W1 and W2 pick identically.**
- **Only the forbidden sign-flip** (`dispatch_bonus=+2.0`) flips to **SEQUENTIAL** (cost 37.0 vs
  the dispatch form's implied 39.0).

**Root cause (the nameable defect).** `GrammarCostModel::cost`'s `Alt` arm is
`base [+ dispatch_bonus if Some]`, where `base = structural + alt_per_branch·N + Σ child_cost` is
**identical for both twin forms** (same children, same branch count N, same alt penalty). Thus
`cost(Some) − cost(None) ≡ dispatch_bonus` — a scalar **constant in N**. The model never reads
`dispatch_table` or `dispatch_branch` for an `Alt`; there is no setup-vs-savings amortization and
no branch-count crossover. **No interior of the calibrated weight space discriminates the twin —
only the _sign_ of a single knob does, and a sign flip is not a calibration.** This is exactly
why the 54-config sweep returned null, why Cranelift avg-class was 1.13, and why the shipping set
is all-narrowing: the cost model was never given a lever whose *value* (not sign) trades dispatch
against sequential.

### PMU limb — cyc/B at value-starts  →  not run (moot)

The gate requires W1≠W2 "from `CALIBRATED_WEIGHTS`, not a sign-flip." No such divergence exists in
the current tree (measured above), so a cold-paired PMU (N≥200, ≥3 windows) of the dispatch-table
vs sequential-trial lowerings would only confirm a runtime difference that **extraction cannot
reach through calibration** — it cannot move the gate verdict, which already fails upstream. Per
parsimony and the 50-min cap, the two heavyweight method commands feeding this limb —
`cargo bench -p bbnf-bench --bench css_canon_bench` and `xtask check-json` ×2 (regen∘regen) —
were **not run**: they exercise the parity/PMU limb downstream of the extraction refutation.

## 4. Verdict

> **FAIL (master gate).** The registered widening rewrite yields **≥2 confluent, terminating
> survivors at a child-bearing node** (egraph limb PASS, measured) — the piece PROTO-1 pass-1 left
> unbuilt is now built and green through `CspScheduler`. **But extraction does NOT pick
> differently across the calibrated weight space**: W1 and a W2 with every non-bonus knob maxed
> both extract DISPATCH; only the forbidden `dispatch_bonus` sign-flip flips the pick. This is the
> gate's **"picks identical"** failure condition.

**Consequence (per the probe's own pass/fail contract):** **LOCK-2 collapses to canonicalization +
the `BackendShape` driver; extraction / CSP-extraction is SPEC_ONLY.** The three prior inert
confirmations (54-config null, Cranelift 1.13, all-narrowing shipping set) share the single root
cause named in §3 — this probe converts three coincidences into one diagnosis.

**What a PASS would require (the lock's next move, not this probe's):** extend
`GrammarCostModel::cost`'s `Alt` arm to net `dispatch_table` (fixed setup) against
`N·dispatch_branch` vs `N·alt_per_branch` sequential-trial — a **branch-count-dependent** term —
so a **single fixed** calibrated vector picks DISPATCH for large N and SEQUENTIAL for small N.
Then W1≠W2 arises from structure, not sign, and the PMU limb becomes worth measuring. That is a
cost-model change (real LOCK-2 engineering), deliberately out of scope for a parsimonious probe.

## 5. Context rider (same fleet pass)

Walk the `CallRule` graph; flag any `RuleId` reached under ≥2 conflicting substrate contexts
(`admits_sink_only` verdicts differ).

- **`admits_sink_only` has no implementation in the tree** (`grep -rn admits_sink_only crates/`
  → 0 hits). The predicate that would turn a call-graph fan-in into a *conflict* does not exist, so
  a faithful verdict-divergence walk is not computable.
- **Structural confirmation instead:** the extraction path built here (`GrammarCostModel::cost`)
  reads **only** the local e-node shape and its children's costs — no call-site / parent /
  substrate-context input enters the cost function. Extraction is therefore **context-free by
  construction**, independent of the call graph.
- **Rider result: 0 conflicts → context-free extraction confirmed (by cost-model construction).**
  Caveat: this is a structural confirmation, not an exhaustive sink-verdict walk — that walk is
  blocked until `admits_sink_only` is implemented. **No silently-wrong rule to name.**

## 6. Deliverables

- This report: `PROTO-1.md`.
- Patch: `LOCK-2-widen-confluent.patch` (398 insertions across 4 files):
  `crates/ir/src/egraph/rules/widen.rs` (new rule), `rules/mod.rs` (+2 export),
  `tests/egraph/main.rs` (+1 module), `tests/egraph/widen_confluent.rs` (new, 2 passing tests).
- No `parse-that` touched (read-only respected). No x86. `--profile ax-iter` throughout.
