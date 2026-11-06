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
