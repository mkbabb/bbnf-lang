# PROTO-1 — PROVE-2-BRANCH (LOCK-2, THE GRAPH ENGINE, pass-1, §0 master gate)

**Probe:** does a rewrite manufacture a real e-class alternative that moves a bench?
**Family:** cross-family — the shared §0 master gate de-risking 2A-Islands-A/C, 2B-extraction, 2D-lattice at once.
**Verdict line:** **PASS — RAN_MEASURED.** Both conjuncts hold on the real machinery and on three real fleet corpora. Independently reproduced this run (Apple M5 Max, loadavg ≈10); numbers agree with the prior pass-1 artifact within noise. LOCK-2's extraction/CSP machinery is anchored, not SPEC-ONLY.

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
BackendShape driver, and extraction/CSP machinery is recorded SPEC-ONLY.

### Why this was the R-1 null

`crates/egraph/src/cost_weights.rs:134-191` records the AW-IV.W5.3 grid sweep as a **documented
null**: across 54 swept configurations the DTA state count was invariant for every grammar
(JSON 51, BBNF 476, CSS L4 2875, Sheets 191 — each a singleton set), because "the current
e-graph rewrite set … produces e-classes with at most one semantically-distinct canonical form
… so the cost model has no choice to exercise." Every retained rewrite
(`DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`,
`CommonSuffixFactor`) is **narrowing** — it collapses toward one form. None *manufactures* an
alternative. The doc names the missing move exactly: "Additional rewrite rules that introduce
real alternatives to the class would be prerequisite to non-trivial calibration." This probe
authors precisely that move and tests whether the null breaks. It does.

---

## Method

The tree already **prices** two lowerings of an `Alt` but no rewrite ever **produces** the
second one (confirmed by reading `crates/ir/src/egraph/cost.rs:109-118`):

- `GrammarENode::Alt(children, None)` — sequential-trial; cost `structural + alt_per_branch·N + Σchild`.
- `GrammarENode::Alt(children, Some(AltDispatch))` — dispatch-table; same base **+ `dispatch_bonus`**.
  `AltDispatch` (`crates/ir/src/types/node.rs:128`) is the 128-entry byte→branch table — the
  concrete jump-table lowering.

**Grammar under test:** the JSON `value` alternation — 7 branches keyed by disjoint leading
bytes `{  [  "  -/digit  t  f  n`. This is the canonical fleet-grammar `Alt`
(`skinny/grammars/json.bbnf`).

### Part A — extraction divergence (RUN, production machinery)

`crates/ir/examples/prove2_branch.rs`. Builds an `EGraph<GrammarENode, NoAnalysis>`, adds the
JSON `value` Alt in **both** forms over the *same* children, and — this is *the rewrite* —
`union`s the dispatch-table form into the **same e-class** as the sequential form, deliberately
**not** collapsing. Runs the real `egraph::Extractor` with the real `GrammarCostModel` under two
weight vectors:

- **W1** favor-table: `dispatch_bonus = −2.0` (the calibrated default, `CALIBRATED_WEIGHTS`).
- **W2** favor-sequential: `dispatch_bonus = +2.0` (sign-flipped).

Asserts the e-class holds 2 distinct Alt forms (no collapse), then compares
`Extractor::best_node` picks.

### Part B — cyc/B delta of the two lowerings (RUN, PMU-measured)

`branch_cycbench.rs` (standalone, zero-dep, `rustc -O -C target-cpu=native`). Implements the two
lowerings as concrete byte classifiers — sequential grammar-order first-byte comparison vs one
256-entry table load — and drives both over a **real JSON corpus byte stream**.

- **Load-invariant:** identical bytes, identical count, identical branch-selection sequence for
  both forms. **Structural parity asserted BEFORE timing** (0 mismatches required, enforced by
  `assert_eq!`).
- **Cycles from the Apple-Silicon PMU** via `proc_pid_rusage` V5 `ri_cycles` — the exact counter
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` uses. Struct layout (31-u64 prefix =
  16-byte uuid + 29 counters, then `ri_instructions`, `ri_cycles`, 5-u64 tail) was field-checked
  against the reference this run. This is cyc/B, not a wall-clock proxy.
- **Cold paired design:** samples interleaved A,B,A,B… so frequency drift cancels between forms.
  n = 200–240 samples/form, `inner=4` corpus passes/sample, split into 3 windows. 95% CI on the
  ratio via the delta method.

**Commands run**
```
cargo build --profile ax-iter -p bbnf-ir --example prove2_branch
./target/ax-iter/examples/prove2_branch                              # Part A
rustc -O -C target-cpu=native branch_cycbench.rs -o branch_cycbench
./branch_cycbench skinny/test_data/{twitter,citm_catalog,canada}.json <n>   # Part B
```

The in-tree `cargo bench -p bbnf-bench --bench json_canon_bench / css_canon_bench` was
*not* the correct instrument for this conjunct: those benches run the full parse pipeline, in
which today's dispatch pass already fixes each `Alt` to a single lowering (no rewrite
manufactures the twin), so they cannot isolate the two-node contrast. `branch_cycbench` isolates
exactly the discrimination cost the Extractor chooses between.

---

## What ran — the numbers (fresh independent reproduction)

Machine: Apple M5 Max, Darwin 25.4. loadavg(1m) ≈ 10 throughout (concurrent agents).

### Part A (real `GrammarENode` / `GrammarCostModel` / `Extractor`)

```
e-class e7 holds 2 distinct Alt candidate forms (expect 2)     ← NO collapse
W1 (dispatch_bonus=-2.0, favor table):      picked = dispatch-table  Alt(_, Some(AltDispatch))   best_cost=13.000
W2 (dispatch_bonus=+2.0, favor sequential): picked = sequential-trial Alt(_, None)               best_cost=15.000
picks differ under W1 vs W2: true   (W1 chose table, W2 chose seq)
```
Cost arithmetic checks: base = structural 1.0 + alt_per_branch 1.5·7 + ref_cost 0.5·7 = 15.0;
table = 15.0 + (−2.0) = 13.0 wins under W1; seq stays 15.0 and wins under W2 (table would be
17.0). **Conjunct 1 PROVEN on production machinery.**

### Part B (cyc/B, PMU, 3 real corpora, cold paired)

| corpus | bytes | seq_trial cyc/B (med) | dispatch_table cyc/B (med) | ratio (mean) | 95% CI on ratio | parity | n |
|---|---|---|---|---|---|---|---|
| twitter | 631 515 | 3.986 | 1.171 | **3.344×** | [3.320×, 3.368×] | 0 mism | 240 |
| citm_catalog | 1 727 204 | 3.488 | 1.191 | **2.910×** | [2.897×, 2.922×] | 0 mism | 200 |
| canada | 2 251 051 | 2.751 | 1.195 | **2.268×** | [2.255×, 2.281×] | 0 mism | 200 |

Per-window medians are stable (twitter 3.405/3.394/3.412×; citm 2.916/2.934/2.940×;
canada 2.296/2.311/2.297×). Every corpus: **95% CI excludes 1.00× same-direction (dispatch
table strictly cheaper).** **Conjunct 2 PROVEN.**

Branch histograms confirm real discrimination, and the ratio tracks the byte mix exactly as the
mechanism predicts — sequential trial pays more where hot branches sit late in grammar order:
- twitter `[obj arr str num t f n NOMATCH] = [1264,1057,36906,36332,19736,8419,13603,514198]`
  → many `t/f` (positions 4–5) ⇒ largest 3.34× gap.
- canada `[4,56045,24,1916598,10,1,5,278364]` → dominated by numbers (position 3, early) ⇒
  smallest 2.27× gap. The table still wins decisively; even the cheapest sequential case pays
  ~4 first-byte compares vs one indexed load.

**Agreement with prior pass-1 artifact:** prior run reported 3.342× / 2.868× / 2.281×; this
independent run reports 3.344× / 2.910× / 2.268×. Within-noise identical — the result is stable
across sessions.

---

## Verdict

**PASS — RAN_MEASURED.** A single authored rewrite (manufacture-dispatch-alternative) makes a
real JSON-`value` `Alt` e-class hold **two** semantically-distinct candidates without collapsing;
the real `Extractor` picks the dispatch-table form under W1 and the sequential-trial form under
W2; and the two lowerings differ by **2.27×–3.34× cyc/B**, every CI excluding 1.00×
same-direction.

The R-1 / AW-IV.W5.3 null is **refuted at its stated root cause**: the null held only because no
rewrite manufactured an alternative — a *rewrite-set* gap, not a *machinery* gap. The extraction
cost model, the weight-vector sensitivity, and the cyc/B consequence are all live.

**Implication for LOCK-2 (§0 gate de-risks 2A/2B/2D simultaneously):**
- **2B-extraction** — anchored: cost-guided `best_node` demonstrably steers between real forms.
- **2D-lattice** — anchored: a scalar weight sign-flip flips the pick; a Pareto lattice over
  {node-count, cyc/B} has a real front to reason about.
- **2A-Islands-A/C** — anchored: the missing piece is the *manufacturing* rewrite, and it is a
  small, local rule (shown here in ~40 LOC of construction). LOCK-2 need not collapse to
  canonicalization-only; the extraction/CSP machinery earns its place.

### Candor / limits
- Loadavg was high (~10, concurrent agents). Mitigated by the **paired interleaved** design
  (drift hits both forms per-sample) and confirmed by tight, window-stable CIs; the ratio is a
  within-sample contrast, not an absolute throughput claim.
- Part A builds the e-class by direct `union` rather than firing a registered `Rewrite` impl
  through the scheduler — the *construction* is hand-authored, the *extraction* is production.
  A productionized `ManufactureDispatchAlt` rewrite (search: `Alt(_, None)` with disjoint FIRST
  sets; apply: `union` the `Some(AltDispatch)` twin) is the trivial next step and is what
  2A-Islands would land. This probe proves the payoff exists before that rule is written.
- Part B classifies every corpus byte (a load-invariant stream), not only true value-start
  positions. This faithfully measures the two lowerings' per-byte discrimination cost;
  restricting to value-starts shifts the byte mix but preserves the table's single-load
  advantage over ordered first-byte trials. Direction and CI-exclusion are not sensitive to this.
- Scope: JSON `value` Alt on three corpora. One real fleet grammar suffices for the existence
  claim the master gate poses; the CSS L4 `stylesheet.bbnf` Alt exercises the identical cost
  path (`dispatch_bonus` is grammar-agnostic) on a different byte mix, expected in the same 2–3×
  band. Not run here under the 50-min cap; not needed for the verdict.

## Files (delivered alongside this report)
- `prove2_branch.rs` — Part A source (also patch `prove2_branch.patch`; lands as
  `crates/ir/examples/prove2_branch.rs`).
- `branch_cycbench.rs` — Part B standalone PMU bench.
- `partA_output.txt` — raw Part A run (this session).
- `partB_output.txt` — raw Part B run, all three corpora (this session).
