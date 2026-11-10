# Contest pre-registration (skeleton)

**Unit:** sk-v27 LW1.CON-5. **Status:** SKELETON. Nothing here is frozen yet, and no claim may cite it.

- **Floors** are proposed and frozen at the feasibility seam, LW2.CON-2, in a hashed decision-plan commit.
- **The cell list** in each family is filled at LW2.CON-1, from the per-plane semantic specs.
- **This file** is frozen in a hashed commit before the first certification run, LW5.CON-1.

After the freeze, the file is never edited. Any change is an addendum with its own digest, and a cell measured before its addendum is re-measured.

The digest of this file is in `PREREG.md.sha256`. `bench/test_prereg.py` fails if the two disagree.

## 1. What a claim is

A claim compares one BBNF contestant against one opponent on one cell. A cell is fixed by five things:

- plane;
- input;
- build matrix;
- track (`generated` or `hand`);
- output-parity class.

The quantity measured is the per-pair log ratio `r = ln(t_bbnf / t_opp)`, where `t` is wall time per input byte. Each pair is two fresh processes, run back to back in alternating order (ABBA blocks), under the CB-B0 harness (`bench/cb-b0/cbb0.py`).

Every contestant walks a checksum of its output inside the timed region, and the two checksums must agree. When they disagree, the pair is discarded and counted. If more than 1 % of a cell's pairs are discarded, the cell is NOT-COMPARABLE.

## 2. Decision rule

- **Estimate:** the Hodges–Lehmann estimate of `r`, with a BCa bootstrap confidence interval (10,000 resamples). The p-value comes from a one-sided Wilcoxon signed-rank test.
- **Multiplicity:** Holm–Bonferroni within each family (§4), at family-wise α = 0.01.
- **Minimum effect:** 3 %, so δ = ln(0.97).

Each cell gets one of four verdicts:

| Verdict | Condition |
|---|---|
| WIN | Holm-adjusted p < 0.01, and the CI's upper bound for `r` is < δ. |
| LOSS | The mirror image of WIN: the CI's lower bound is > −δ, at the same adjusted level. |
| TIE | The whole CI lies inside (δ, −δ). |
| UNDECIDED | Anything else. It is reported as UNDECIDED and never rounded to a win. |

## 3. Sample sizes and conditions

- **Exploratory cells:** ≥ 10 reps per side, reported as median, IQR and MAD. They are never certifying.
- **Certification cells:** ≥ 200 pairs, spread over ≥ 30 separate harness invocations, within the 72 h budget per certification unit (X-7). A cell is re-queued once at most.
- **Conditions:** only a CB-B0 record with status QUIET can certify. QUIET means:
  - 1-minute load below 8 before every rep;
  - no thermal warning;
  - the `guarded --exclusive bench` lock held;
  - user-interactive QoS on ≥ 95 % of each rep's CPU time.

  A NOISY record never certifies, whatever its numbers.
- **Matrices:** PORTABLE is the default codegen for the target triple and is the headline. NATIVE (`-C target-cpu=native`) is reported beside it and labelled. Claims read "on Apple M-series (aarch64)". x86 numbers are observational only (X-4).
- **Opponents:** each opponent is built from the release pinned in `bench/W0-BENCH-PINS.tsv` (v2, checked by `bench/pins/pins.py check`), with that project's recommended release flags. An opponent bump re-measures every claim that cites it (LW6.CON-2).

## 4. Families

Each family is one Holm group. Cells are added by addendum before the freeze.

| Family | Plane | Opponents (pinned) | Cells | State |
|---|---|---|---|---|
| F-JSON-P1 | JSON to a typed value | simd-json, sonic-rs, serde_json, jiter | OPEN (LW2.CON-1) | skeleton |
| F-JSON-P4 | JSON lossless CST | tree-sitter + tree-sitter-json (CST class only) | OPEN (LW2.CON-1) | skeleton |
| F-CSS-P2 | CSS tokenize and parse | cssparser (G-C1a); lightningcss on the typed-coverage manifest only (G-C1b) | OPEN (LW3.CON-1) | skeleton |
| F-REGEX | regex match, sticky and leftmost-first | regex, regex-automata | OPEN (LW3.CON-1) | skeleton |
| F-COMB | combinator and generator parsers on the shared grammars | winnow, nom, chumsky, pest, logos | OPEN (LW2.CON-1) | skeleton |
| F-JS | TS/JS planes | @lezer/*, @csstools/*, css-tree, postcss, peggy, chevrotain, ohm-js, parsimmon | OPEN (LW2.CON-1) | skeleton |
| F-PP | pretty-printing and formatting throughput | OPEN (the pprint design's opponents) | OPEN (LW4.PP) | skeleton |
| F-TRIAL-EG | root trial: e-graph (P5) on against off, on the CB-5.5 slice | the same slice with the e-graph off | §6 (LW2.COMP-1) | registered |
| F-TRIAL-CSP | root trial: CSP strategy against greedy, on the CB-5.5 slice | the same slice with the greedy strategy | §6 (LW2.COMP-1) | registered |

The planes P3, P5 and P6 (Wave F, LW6.CON-1) are named in the competition design past its dispatch cut. They are added as families once LW1.TOPO-0 files the full text.

## 5. What is reported for every cell

Each cell reports:

- the plane, input (with its content hash), matrix and track;
- the opponent pin row;
- n pairs and n discarded;
- the Hodges–Lehmann estimate and the BCa CI, as percentages;
- the raw p and the Holm-adjusted p;
- the verdict;
- the CB-B0 record keys behind it.

Cells reported as OBSERVATIONAL, NOISY, NOT-COMPARABLE or DESCRIPTIVE are labelled as such in the competition table (LG-e).

## 6. Root trials (LW2.COMP-1), registered by S5 COMP

This section is an addendum. It is written before Phase 3 and hashed with the rest of the file. S11 BENCH runs these cells and applies the rule below mechanically; it adds nothing and reads nothing into a record.

**Harness.** Both trials run on the CB-5.5 JSON emitter slice (LW2.COMP-4) for `grammar/json/json.bbnf`. The slice exposes each organ as one arm switch, and the two arms of a trial differ in that switch only: same tree hash, same toolchain, same matrix, same input.

| Trial | Organ arm (complex) | Simpler arm | Loser tag |
|---|---|---|---|
| EG | e-graph saturation and extraction on (`bbnf_ir::egraph`, P5) | e-graph off: the plan is built from the unrewritten program | `trial/lw2-egraph-loser` or `trial/lw2-direct-loser` |
| CSP | per-node strategy chosen by the CSP (`bbnf_ir::passes::csp_strategy`) | greedy: each node takes the first admissible strategy in the solver's own priority order | `trial/lw2-csp-loser` or `trial/lw2-greedy-loser` |

**Quantity.** For a timing cell, `r = ln(t_complex / t_simple)` per ABBA pair, under §1 and §2 (output checksums must agree, or the pair is discarded). A WIN for the organ is `r` below δ = ln(0.97).

**Inputs.** The corpus-v1 JSON files, each fixed by its sha256 in `corpus/MANIFEST.sha256` at the S11 HEAD: `twitter.json` (the upstream simdjson file, LW2.TOPO-6), `canada.json`, `citm_catalog.json`.

**Registered cells.** No other cell may be run for these trials. S11's harness refuses an unregistered cell id (`cbb0.py run --cell`).

| Cell | Family | Kind | Measure | Input | Matrix |
|---|---|---|---|---|---|
| C-EG-PARSE-TWITTER-P | F-TRIAL-EG | microbench | parse time per byte, slice parser | twitter.json | PORTABLE |
| C-EG-PARSE-CANADA-P | F-TRIAL-EG | microbench | parse time per byte, slice parser | canada.json | PORTABLE |
| C-EG-PARSE-CITM-P | F-TRIAL-EG | microbench | parse time per byte, slice parser | citm_catalog.json | PORTABLE |
| C-EG-PARSE-TWITTER-N | F-TRIAL-EG | microbench (labelled) | parse time per byte, slice parser | twitter.json | NATIVE |
| C-EG-EMIT-TIME | F-TRIAL-EG | compile artifact | wall time of one slice emission of json.bbnf | json.bbnf | PORTABLE |
| C-EG-EMIT-BYTES | F-TRIAL-EG | compile artifact (exact) | bytes of the slice's emitted Rust | json.bbnf | — |
| C-EG-REGEN-BYTES | F-TRIAL-EG | compile artifact (exact) | bytes `cargo xtask regen` writes under the arm | in-tree grammars | — |
| C-CSP-PARSE-TWITTER-P | F-TRIAL-CSP | microbench | parse time per byte, slice parser | twitter.json | PORTABLE |
| C-CSP-PARSE-CANADA-P | F-TRIAL-CSP | microbench | parse time per byte, slice parser | canada.json | PORTABLE |
| C-CSP-PARSE-CITM-P | F-TRIAL-CSP | microbench | parse time per byte, slice parser | citm_catalog.json | PORTABLE |
| C-CSP-PARSE-TWITTER-N | F-TRIAL-CSP | microbench (labelled) | parse time per byte, slice parser | twitter.json | NATIVE |
| C-CSP-EMIT-TIME | F-TRIAL-CSP | compile artifact | wall time of one slice emission of json.bbnf | json.bbnf | PORTABLE |
| C-CSP-EMIT-BYTES | F-TRIAL-CSP | compile artifact (exact) | bytes of the slice's emitted Rust | json.bbnf | — |
| C-CSP-REGEN-BYTES | F-TRIAL-CSP | compile artifact (exact) | bytes `cargo xtask regen` writes under the arm | in-tree grammars | — |

The Holm group of each family is its timing cells (four microbench cells and the emit-time cell). An exact cell is read twice per arm, must give the same number both times, and is reported, not tested: it can only veto (below).

**Sample sizes.** §3 applies unchanged: a timing cell certifies with ≥ 200 pairs over ≥ 30 harness invocations, QUIET records only; fewer pairs are exploratory and never decide a trial.

**Deadline.** 72 h per trial, from S11's first registered run of that trial, re-queued once (X-7). A record waits at most 60 min for a 1-minute load below 8 and otherwise runs labelled NOISY; a NOISY record never certifies.

**Verdict rule, per trial** (applied in this order):

1. **UNDECIDED** if, at the deadline, any PORTABLE microbench cell or the emit-time cell lacks a certifying verdict (too few pairs, NOT-COMPARABLE, or NOISY only).
2. **WIN** for the organ if at least one PORTABLE microbench cell is WIN, none of the four microbench cells is LOSS, the emit-time cell is not LOSS, and neither exact cell grows by more than 3 % over the simpler arm.
3. **LOSS** for the organ if at least one microbench cell is LOSS and none is WIN.
4. **UNDECIDED** otherwise.

WIN keeps the organ, and the simpler arm is tagged the loser. LOSS and UNDECIDED keep the simpler arm (e-graph off; greedy), and the organ is tagged the loser. NATIVE cells are reported beside the PORTABLE ones and can block a WIN only through rule 2's "none is LOSS". Each verdict is recorded with the CB-B0 record ids behind it. Porting the winner is LW3.COMP-1.
