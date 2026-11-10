# Contest pre-registration (skeleton)

**Unit:** sk-v27 LW1.CON-5; families and LW2 cells registered at LW2.CON-1 (§6). **Status:** OPEN. Nothing here is frozen yet, and no claim may cite it.

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
| F-JSON-P1 | P1 JSON check (`bench/planes/P1.md`) | sonic-rs (primary, Rust); serde_json, simd-json, jiter; simdjson (C++, observational) | §6 (LW2.CON-1) | open |
| F-JSON-P4 | P4 JSON DOM (`P4.md`); the CST class only against CST arms | sonic-rs (primary); serde_json, simd-json, jiter; yyjson (C, observational); tree-sitter-json (CST class only) | §6 (LW2.CON-1) | open |
| F-CSS-P2 | P2 CSS token and event stream (`P2.md`); G-C1b cells read P7 (`P7.md`) on the typed-coverage manifest only | cssparser (G-C1a, primary); lightningcss (G-C1b) | §6 (LW2.CON-1); G-C1b cells wait for the manifest | open |
| F-REGEX | regex match, sticky and leftmost-first (no plane spec in LW2) | regex, regex-automata | OPEN (LW3.CON-1, after OR-7) | skeleton |
| F-COMB | combinator and generator parsers on the shared grammars: JSON at P4, CSS at P2 | winnow, nom, chumsky, pest, lalrpop, peg (logos: no comparator) | §6 (LW2.CON-1) | open |
| F-JS | the TS face: JSON at P4, CSS at P2 | JSON.parse (tracked, never gated), peggy, chevrotain, ohm-js, parsimmon, antlr4, @lezer/* (CST class), @csstools/*, css-tree (postcss: no comparator) | §6 (LW2.CON-1) | open |
| F-PP | pretty-printing and formatting throughput | OPEN (the pprint design's opponents) | OPEN (LW4.PP, under X-10) | skeleton |

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

## 6. Registered cells (LW2.CON-1)

A cell is measured only if it is registered here and `bench/contest/admit.py cell <id>` admits it. Admission refuses an unregistered cell, a PREREG.md that no longer matches its digest, an arm with no `bench/contest/ROWS.tsv` row or a "no comparator" row, arms of different planes or classes, arms that disagree on any witness input of the plane, and a missing or mismatched build plane (I-8). An arm whose row is OWED (no witness mode yet) is refused until its adapter lands, and the refusal is the cell's LW2 record. Inputs are named by path and the leading digits of their sha256 in `corpus/MANIFEST.sha256`.

### 6.1 Kinds

| Kind | Purpose | Verdict rule |
|---|---|---|
| BASE | a baseline per goal, both arms as their rows say | OBSERVATIONAL: median, IQR and MAD of `r` over ≥ 10 pairs; never certifying; no verdict |
| AA | self against self: both arms are the same row and the same build | competition U4 (a): the decision rule of §2 must report 0 cells beyond the floor; the HL estimate and its CI give the family's A/A noise for the floor `max(10 %, 3 × A/A noise)` (LW2.CON-2) |
| SLOW | the same row against itself with an injected 15 % slowdown on one side | competition U4 (b): the rule must detect it (LOSS for the slowed side, or a CI whose lower bound exceeds −ln(0.97)); an undetected slowdown makes the family's floor NOISY-UNFROZEN |
| MISMATCH | a pair that is not plane-equivalent | competition U4 (c) and (d): admission must refuse it; a MISMATCH cell that is admitted, or measured, falsifies the harness |

The slowdown is injected by the runner, never by the measured binary: the slowed arm's process is spawned under the same command, and the runner busy-waits inside that arm's timed window for 15 % of the unslowed arm's A/A median, on the same core class. Pairs are ABBA blocks as §1; every record is a CB-B0 record (`bench/cb-b0/cbb0.py`) with its load, thermal state and QoS.

### 6.2 Cells

| Cell | Family | Kind | Plane | Class | Input | Matrix | Track | Arm (bbnf) | Arm (opp) | Pairs |
|---|---|---|---|---|---|---|---|---|---|---|
| C-J1-BASE-TWITTER | F-JSON-P1 | BASE | P1 | CHK | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P1-bbnf-gen | R-JSON-P1-sonic-rs | ≥ 10 |
| C-J1-BASE-CITM | F-JSON-P1 | BASE | P1 | CHK | corpus/json/citm_catalog.json@a73e7a88 | PORTABLE | generated | R-JSON-P1-bbnf-gen | R-JSON-P1-sonic-rs | ≥ 10 |
| C-J1-BASE-CANADA | F-JSON-P1 | BASE | P1 | CHK | corpus/json/canada.json@f83b3b35 | PORTABLE | generated | R-JSON-P1-bbnf-gen | R-JSON-P1-sonic-rs | ≥ 10 |
| C-J1-AA | F-JSON-P1 | AA | P1 | CHK | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P1-bbnf-gen | R-JSON-P1-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-J1-SLOW | F-JSON-P1 | SLOW | P1 | CHK | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P1-bbnf-gen | R-JSON-P1-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-J3-BASE-TWITTER | F-JSON-P4 | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-sonic-rs | ≥ 10 |
| C-J3-BASE-CITM | F-JSON-P4 | BASE | P4 | VAL | corpus/json/citm_catalog.json@a73e7a88 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-sonic-rs | ≥ 10 |
| C-J3-BASE-CANADA | F-JSON-P4 | BASE | P4 | VAL | corpus/json/canada.json@f83b3b35 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-sonic-rs | ≥ 10 |
| C-J3-BASE-SERDE-TWITTER | F-JSON-P4 | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-serde_json | ≥ 10 |
| C-J3-AA | F-JSON-P4 | AA | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-J3-SLOW | F-JSON-P4 | SLOW | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-C1-BASE-BOOTSTRAP | F-CSS-P2 | BASE | P2 | TOK | corpus/css/bootstrap.css@4a50207b | PORTABLE | generated | R-CSS-P2-bbnf-gen | R-CSS-P2-cssparser | ≥ 10 |
| C-C1-BASE-TAILWIND | F-CSS-P2 | BASE | P2 | TOK | corpus/css/tailwind.css@de305d92 | PORTABLE | generated | R-CSS-P2-bbnf-gen | R-CSS-P2-cssparser | ≥ 10 |
| C-C1-AA | F-CSS-P2 | AA | P2 | TOK | corpus/css/bootstrap.css@4a50207b | PORTABLE | generated | R-CSS-P2-bbnf-gen | R-CSS-P2-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-C1-SLOW | F-CSS-P2 | SLOW | P2 | TOK | corpus/css/bootstrap.css@4a50207b | PORTABLE | generated | R-CSS-P2-bbnf-gen | R-CSS-P2-bbnf-gen | ≥ 30 over ≥ 3 invocations |
| C-C1B-BASE-BOOTSTRAP | F-CSS-P2 | BASE | P7 | TYPED | corpus/css/bootstrap.css@4a50207b (typed-coverage manifest) | PORTABLE | generated | R-CSS-P7-bbnf-css_l4 | R-CSS-P7-lightningcss | ≥ 10 |
| C-G1-BASE-PEST | F-COMB | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-COMB-P4-pest | ≥ 10 |
| C-G1-BASE-NOM | F-COMB | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-COMB-P4-nom | ≥ 10 |
| C-G1-BASE-WINNOW | F-COMB | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-COMB-P4-winnow | ≥ 10 |
| C-G1-BASE-CHUMSKY | F-COMB | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-COMB-P4-chumsky | ≥ 10 |
| C-G1-AA | F-COMB | AA | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-COMB-P4-winnow | R-COMB-P4-winnow | ≥ 30 over ≥ 3 invocations |
| C-G2-BASE-JSONPARSE | F-JS | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JS-P4-bbnf-ts | R-JS-P4-json-parse | ≥ 10 |
| C-G2-BASE-PEGGY | F-JS | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JS-P4-bbnf-ts | R-JS-P4-peggy | ≥ 10 |
| C-G2-BASE-CHEVROTAIN | F-JS | BASE | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JS-P4-bbnf-ts | R-JS-P4-chevrotain | ≥ 10 |
| C-G2-AA | F-JS | AA | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | hand | R-JS-P4-json-parse | R-JS-P4-json-parse | ≥ 30 over ≥ 3 invocations |
| C-G2-SLOW | F-JS | SLOW | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | hand | R-JS-P4-json-parse | R-JS-P4-json-parse | ≥ 30 over ≥ 3 invocations |
| C-MM-PLANE | F-JSON-P4 | MISMATCH | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | L-CON4-skinny.parse_only_track1 | L-CON4-skinny.parse_only_serde | refused |
| C-MM-CLASS | F-JSON-P4 | MISMATCH | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | generated | R-JSON-P4-bbnf-gen | R-JSON-P4-tree-sitter-json | refused |
| C-MM-NOCOMP | F-JS | MISMATCH | P2 | TOK | corpus/css/bootstrap.css@4a50207b | PORTABLE | generated | R-JS-P2-csstools | R-JS-P2-postcss | refused |
| C-MM-BUILD | F-JS | MISMATCH | P4 | VAL | corpus/json/twitter.json@30721e49 | PORTABLE | hand | R-JS-P4-json-parse | R-JS-P4-json-parse | refused: build planes `bench/contest/build-planes/node-default.json` against `node-jitless.json` |

`C-G2-AA` and `C-G2-SLOW` use JSON.parse because it is the one arm with a witness mode at LW2.CON-1; every other AA and SLOW cell waits for its bbnf arm's witness mode (U5). F-REGEX and F-PP have no LW2 cells: their rows are "no comparator" until LW3.CON-1 and LW4.PP. The LW1.CON-4 records stay OBSERVATIONAL: their rows are reclassified in ROWS.tsv (`L-CON4-*`), and `C-MM-PLANE` registers the pair that LW1 read as one "parse_only" column, P1 against P4, as refused.
