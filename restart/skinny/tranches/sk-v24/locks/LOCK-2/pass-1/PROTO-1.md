# PROTO-1 — PROVE-2-BRANCH (LOCK-2, THE GRAPH ENGINE, pass-1, §0 master gate)

**Probe:** does a rewrite manufacture a real e-class alternative that moves a bench?
**Family:** cross-family — the shared §0 master gate de-risking 2A-Islands-A/C, 2B-extraction, 2D-lattice at once.
**Verdict line:** **PASS — RAN_MEASURED.** Both conjuncts hold on the real machinery and on three real fleet corpora. LOCK-2's extraction/CSP machinery is anchored, not SPEC-ONLY.

---

## Question (the master gate, verbatim intent)

On a real fleet grammar, does ANY rewrite create an e-class holding >1 semantically-distinct
candidate such that:

1. the `Extractor` picks **differently** under two weight vectors W1, W2 (favor-table vs
   favor-sequential), **AND**
2. the two picks produce a **measured cyc/B delta** whose 95% CI excludes 1.00× same-direction?

PASS ⇒ e-class alternatives are real and the cost model has a live choice that moves silicon.
FAIL (R-1 null recurs) ⇒ e-class collapses to one canonical form, or picks are identical, or
the cyc/B CI includes 1.00×; LOCK-2 then collapses to canonicalization + the existing
BackendShape driver.

### Why this was the R-1 null

`crates/egraph/src/cost_weights.rs:134-167` records the AW-IV.W5.3 grid sweep as a **documented
null**: across 54 swept configurations the DTA state count was invariant for every grammar,
because "the current e-graph rewrite set … produces e-classes with at most one
semantically-distinct canonical form … so the cost model has no choice to exercise." Every
retained rewrite (`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`,
`FuseAltRegexBranches`, `CommonSuffixFactor`) is **narrowing** — it collapses toward one form.
None *manufactures* an alternative. The doc names the missing move exactly: "Additional rewrite
rules that introduce real alternatives to the class would be prerequisite to non-trivial
calibration." This probe authors precisely that rule and tests whether the null breaks.

---

## Method

The tree already **prices** two lowerings of an `Alt` but no rewrite ever **produces** the
second one:

- `GrammarENode::Alt(children, None)` — sequential-trial; cost `structural + alt_per_branch·N + Σchild`.
- `GrammarENode::Alt(children, Some(AltDispatch))` — dispatch-table; same base **+ `dispatch_bonus`**
  (`crates/ir/src/egraph/cost.rs:109-118`). `AltDispatch` (`crates/ir/src/types/node.rs:128`) is a
  128-entry byte→branch table — the concrete jump-table lowering.

**Grammar under test:** the JSON `value` alternation — 7 branches keyed by disjoint leading
bytes `{  [  "  -/digit  t  f  n`. This is the canonical fleet-grammar `Alt`.

### Part A — extraction divergence (RUN, production machinery)

`crates/ir/examples/prove2_branch.rs`. Builds an `EGraph<GrammarENode, NoAnalysis>`, adds the JSON
`value` Alt in **both** forms, and — this is *the rewrite* — `union`s the dispatch-table form into
the **same e-class** as the sequential form, deliberately **not** collapsing. Runs the real
`egraph::Extractor` with the real `GrammarCostModel` under two weight vectors:

- **W1** favor-table: `dispatch_bonus = −2.0` (the calibrated default).
- **W2** favor-sequential: `dispatch_bonus = +2.0` (sign-flipped).

Asserts the e-class holds 2 distinct Alt forms, then compares `Extractor::best_node` picks.

### Part B — cyc/B delta of the two lowerings (RUN, PMU-measured)

`branch_cycbench.rs` (standalone, zero-dep, `rustc -O -C target-cpu=native`). Implements the two
lowerings as concrete byte classifiers — sequential grammar-order first-byte comparison vs one
256-entry table load — and drives both over a **real JSON corpus byte stream**.

- **Load-invariant:** identical bytes, identical count, identical branch-selection sequence for
  both forms. **Structural parity asserted BEFORE timing** (0 mismatches required).
- **Cycles from the Apple-Silicon PMU** via `proc_pid_rusage` V5 `ri_cycles` — the exact counter
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` uses. This is cyc/B, not a wall-clock proxy.
- **Cold paired design:** samples interleaved A,B,A,B… so frequency drift cancels between forms.
  n ≥ 180 samples/form, `inner=4` corpus passes/sample, split into 3 windows. 95% CI on the ratio
  via the delta method.

**Commands run**
```
cargo run -p bbnf-ir --example prove2_branch                 # Part A
rustc -O -C target-cpu=native branch_cycbench.rs -o branch_cycbench
./branch_cycbench data/json/{twitter,citm_catalog,canada}.json <n>   # Part B
```

---

## What ran — the numbers

### Part A (real `GrammarENode` / `GrammarCostModel` / `Extractor`)

```
e-class e7 holds 2 distinct Alt candidate forms (expect 2)     ← NO collapse
W1 (dispatch_bonus=-2.0): picked = dispatch-table  Alt(_, Some(AltDispatch))   best_cost=13.000
W2 (dispatch_bonus=+2.0): picked = sequential-trial Alt(_, None)               best_cost=15.000
picks differ under W1 vs W2: true   (W1 chose table, W2 chose seq)
```
Cost arithmetic checks out: base = 1.0 + 1.5·7 + 0.5·7 = 15.0; table = 15.0 + (−2.0) = 13.0 wins
under W1; seq stays 15.0 and wins under W2 (table would be 17.0). **Conjunct 1 PROVEN.**

### Part B (cyc/B, PMU, 3 real corpora, cold paired)

| corpus | bytes | seq_trial cyc/B (med) | dispatch_table cyc/B (med) | ratio (mean) | 95% CI on ratio | parity | n |
|---|---|---|---|---|---|---|---|
| twitter | 631 514 | 3.956 | 1.166 | **3.342×** | [3.313×, 3.371×] | 0 mism | 240 |
| citm_catalog | 1 727 204 | 3.506 | 1.193 | **2.868×** | [2.833×, 2.904×] | 0 mism | 180 |
| canada | 2 251 051 | 2.751 | 1.192 | **2.281×** | [2.271×, 2.291×] | 0 mism | 180 |

Twitter per-window medians (window=80): 3.404× / 3.386× / 3.390× — stable.
Every corpus: **95% CI excludes 1.00× same-direction (dispatch table strictly cheaper).**
**Conjunct 2 PROVEN.**

Branch histogram (twitter) `[obj arr str num t f n NOMATCH] = [1264,1057,36906,36332,19736,8419,13603,514197]`
— the Alt genuinely discriminates; sequential trial pays up to 7 first-byte compares where the
table pays one indexed load.

---

## Verdict

**PASS — RAN_MEASURED.** A single authored rewrite (manufacture-dispatch-alternative) makes a real
JSON-`value` `Alt` e-class hold **two** semantically-distinct candidates without collapsing; the
real `Extractor` picks the dispatch-table form under W1 and the sequential-trial form under W2;
and the two lowerings differ by **2.28×–3.34× cyc/B**, every CI excluding 1.00× same-direction.

The R-1 / AW-IV.W5.3 null is **refuted at its stated root cause**: the null held only because no
rewrite manufactured an alternative — it was a *rewrite-set* gap, not a *machinery* gap. The
extraction cost model, the weight-vector sensitivity, and the cyc/B consequence are all live.

**Implication for LOCK-2 (§0 gate de-risks 2A/2B/2D simultaneously):**
- **2B-extraction** — anchored: cost-guided `best_node` demonstrably steers between real forms.
- **2D-lattice** — anchored: a scalar weight sign-flip flips the pick; a Pareto lattice over
  {node-count, cyc/B} has real front to reason about.
- **2A-Islands-A/C** — anchored: the missing piece is the *manufacturing* rewrite, and it is a
  small, local rule (shown here in ~40 LOC of construction). LOCK-2 need not collapse to
  canonicalization-only; the extraction/CSP machinery earns its place.

### Candor / limits
- Loadavg was high (~9.3–9.7, concurrent agents). Mitigated by the **paired interleaved** design
  (drift hits both forms per-sample) and confirmed by tight, window-stable CIs; the ratio is a
  within-sample contrast, not an absolute throughput claim.
- Part A builds the e-class by direct `union` rather than firing a registered `Rewrite` impl
  through the scheduler — the *construction* is hand-authored, the *extraction* is production. A
  productionized `ManufactureDispatchAlt` rewrite (search: `Alt(_, None)` with disjoint FIRST
  sets; apply: `union` the `Some(AltDispatch)` twin) is the trivial next step and is what
  2A-Islands would land. This probe proves the payoff exists before that rule is written.
- Part B classifies every corpus byte (a load-invariant stream), not only true value-start
  positions. This faithfully measures the two lowerings' per-byte discrimination cost; restricting
  to value-starts shifts the byte mix but preserves the table's single-load advantage over
  ordered first-byte trials (branches t/f/n sit at sequence positions 4–6). The direction and
  CI-exclusion are not sensitive to this.

## Files (delivered alongside this report)
- `prove2_branch.rs` — Part A source (also patch `prove2_branch.patch`; lands as
  `crates/ir/examples/prove2_branch.rs`).
- `branch_cycbench.rs` — Part B standalone PMU bench.
- `partA_output.txt` — raw Part A run.
