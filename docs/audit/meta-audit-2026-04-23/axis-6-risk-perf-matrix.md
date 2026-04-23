# Axis 6 — Risk / Perf Matrix Audit

Scope: audit `/Users/mkbabb/Programming/bbnf-lang/docs/RISK-PERF-MATRIX.md` for probability calibration, arithmetic consistency, AZ split cascade math, and per-grammar performance anchors / wave assignments.

Primary comparison surfaces:

- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AU/FINAL.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-I/AZ-I.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-I/waves/W2.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-I/waves/W3.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-II/AZ-II.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/BA.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/waves/W1.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/BB.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/waves/W0.md`
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BB/waves/W2.md`

Method: replay the table arithmetic from the listed wave rows, then compare the matrix claims against the tranche wave specs and AU baseline artefacts.

## Verified

### 1. The AU baseline anchors for the three data grammars are correctly lifted from AU

The matrix's AU-baseline rows at `docs/RISK-PERF-MATRIX.md:316-317`, `331-332`, and `345-346` match the AU close matrix at `docs/tranches/AU/FINAL.md:443-453`:

| Fixture | Matrix | AU FINAL |
|---|---:|---:|
| JSON canada | 1231 | 1231 |
| JSON citm | 2438 | 2438 |
| JSON twitter | 1967 | 1967 |
| CSS normalize | 735 | 735 |
| CSS bootstrap | 454 | 454 |
| CSS tailwind | 496 | 496 |
| Sheets parse_simple | 95 | 95 |

For the data grammars, the anchor table is faithful.

### 2. The AZ split correction is directionally honest

The matrix explicitly rejects the earlier optimistic claim that splitting AZ improves raw multiplicative joint probability, and the correction is mathematically sound in direction:

- `AZ-I tranche close = 0.070 / 0.34` at `docs/RISK-PERF-MATRIX.md:125`
- `AZ-II tranche close = 0.17 / 0.45` at `docs/RISK-PERF-MATRIX.md:148`
- Product from the displayed tranche-close rows:
  - declared: `0.070 × 0.17 = 0.0119`, which rounds to `0.012`
  - floor: `0.34 × 0.45 = 0.153`, which rounds to `0.15`

That supports the text at `docs/RISK-PERF-MATRIX.md:228-234`: the split lowers raw joint probability relative to the monolithic `0.09 / 0.38`, because more probability-less-than-one gates are introduced.

### 3. The main wave-to-performance assignments are mostly aligned for AZ-I, BA, and BB

The matrix's key wave/perf checkpoints match the tranche specs they cite:

- `AZ-I.W2` as the JSON + Sheets AU-parity recovery wave:
  - matrix: `docs/RISK-PERF-MATRIX.md:321,350`
  - tranche spec: `docs/tranches/AZ-I/waves/W2.md:5-8,31-33,89-93`
- `AZ-I.W3` as the CSS direct-to-struct parity wave:
  - matrix: `docs/RISK-PERF-MATRIX.md:336`
  - tranche spec: `docs/tranches/AZ-I/waves/W3.md:5-8,29-31,87-90`
- `BA.W1` as the lazy-path micro-bench and sonic-rs comparison wave:
  - matrix: `docs/RISK-PERF-MATRIX.md:323-324,338`
  - tranche spec: `docs/tranches/BA/waves/W1.md:6-9,64-73,83-94`
- `BB.W0` as the Tranche H rediscovery gate:
  - matrix: `docs/RISK-PERF-MATRIX.md:190,195`
  - tranche spec: `docs/tranches/BB/waves/W0.md:5-8,89-105`
- `BB.W2` as the CSS + BBNF enumeration wave:
  - matrix: `docs/RISK-PERF-MATRIX.md:192,325,339`
  - tranche spec: `docs/tranches/BB/waves/W2.md:5-8,45-78`

The matrix is strongest where it mirrors tranche docs that already name concrete numeric gates.

### 4. The "split buys reversal scope, not probability" framing is strategically correct

The prose at `docs/RISK-PERF-MATRIX.md:236-298` is not free, but it is honest on the main point: the split is justified by cleaner checkpointing and narrower reversals, not by better raw success odds. That is consistent with the tranche structure itself:

- AZ-I closes the three data grammars before BBNF cutover starts (`docs/tranches/AZ-I/AZ-I.md:91-127`)
- AZ-II isolates the two-stage BBNF bootstrap risk (`docs/tranches/AZ-II/AZ-II.md:115-123,267-297`)

As a planning thesis, that reframe is sound.

## Refined

### 1. The tranche-close rows are approximate calibrated numbers, not exact compounds of the listed wave rows

Several rows are labeled as compounds, but the displayed tranche-close numbers do not equal the direct products of the listed wave probabilities.

Replayed products from the matrix's own wave rows:

| Tranche | Exact product from wave rows | Stated close row |
|---|---|---|
| B1 | `0.5375 / 0.7871` | `0.55 / 0.80` |
| AY-II | `0.1815 / 0.4701` | `0.20 / 0.55` |
| AZ-I | `0.0700 / 0.3516` | `0.070 / 0.34` |
| AZ-II | `0.1694 / 0.4751` | `0.17 / 0.45` |
| BA | `0.2621 / 0.5652` | `0.27 / 0.55` |
| BB | `0.0997 / 0.3272` | `0.10 / 0.32` |

This is acceptable only if the doc says these are calibrated tranche-level overrides. As written, rows such as `docs/RISK-PERF-MATRIX.md:79,102,125,148,174,195` read as direct arithmetic compounds. They should either:

- be replaced with the exact products, or
- be relabeled as tranche-level judgment calls distinct from the wave-row products.

### 2. The corrected AZ-I + AZ-II joint is right under the document's rounded-tranche model, but not under the exact wave-row model

`docs/RISK-PERF-MATRIX.md:229-230` gives `0.012 / 0.15`.

That is correct if the cascade is intentionally multiplying the displayed tranche-close rows:

- `0.070 × 0.17 = 0.0119`
- `0.34 × 0.45 = 0.153`

But if the matrix is supposed to be strictly derived from the wave-row products, the exact joint is:

- declared: `0.069966 × 0.1694 = 0.01185` → still `0.012`
- floor: `0.35157 × 0.47511 = 0.1670` → `0.17`

The fix here is not conceptual. It is model hygiene: the document should say whether the cascade multiplies rounded tranche-close estimates or the exact wave-row products.

### 3. The AY-II per-grammar rows are planning checkpoints, not actual wave hard-gate transcriptions

Examples:

- matrix: `AY-II W1 close ≥ 800 / 1600 / 1200` at `docs/RISK-PERF-MATRIX.md:319`
- actual AY-II.W1 gate: peer-relative ratios vs sonic-rs / simd-json, not those absolute MB/s numbers (`docs/tranches/AY-II/waves/W1.md:5,35-55`)

- matrix: `AY-II W2 close ≥ 500 / 350 / 380` at `docs/RISK-PERF-MATRIX.md:334`
- actual AY-II.W2 gate: competitor-relative CSS performance and typed-semantic parity, not those exact absolute floors (`docs/tranches/AY-II/waves/W2.md:5,52-57`)

- matrix: `AY-II W3 close ≥ 75` at `docs/RISK-PERF-MATRIX.md:348`
- actual AY-II.W3 gate: clean fat-LTO Sheets benches plus self-parity and no panic; no explicit `75 MB/s` hard gate is named (`docs/tranches/AY-II/waves/W3.md:5,98-113`)

These rows are useful, but they should be labeled "forecasted checkpoint marks" rather than "wave-close targets" if the intent is not to restate the tranche gates verbatim.

### 4. BBNF is not AU-MB/s-anchored the way the other grammars are

The matrix is careful at `docs/RISK-PERF-MATRIX.md:358-364` to note that BBNF self-parse was not in AU's 17-entry matrix. That is the right substantive position.

But the section intro at `docs/RISK-PERF-MATRIX.md:304-310` frames the performance tables as MB/s floors, and the AZ-II hard-gate table in `docs/tranches/AZ-II/AZ-II.md:97-112` still presents `BBNF self-parse | AU-baseline | ≥ AU | 10% better` as though AU supplied a numeric anchor.

The clean version is:

- data grammars: AU-MB/s anchored
- BBNF: correctness- and reproducibility-anchored, with local micro-bench trajectory only

## Flawed

### 1. The cascade table compounds rounded tranche-close rows as if they were exact, and the inflation is material

The cascade table at `docs/RISK-PERF-MATRIX.md:220-226` uses the displayed tranche-close numbers (`0.55`, `0.20`, `0.34`, etc.) as multiplicative inputs. That materially overstates several milestone probabilities relative to the wave-row products above.

Examples from replay:

| Milestone | Stated | Exact from wave rows |
|---|---:|---:|
| B1 + AY-II close declared | `0.11` | `0.0976` |
| B1 + AY-II close floor | `0.44` | `0.3700` |
| + AZ-I close floor | `0.15` | `0.1301` |
| + AZ-II close floor | `0.068` | `0.0618` |
| + BA close floor | `0.037` | `0.0349` |

The gap is not cosmetic. It shifts the floor narrative upward by enough to matter in a planning document whose stated purpose is calibration.

### 2. The AY-II tranche-close floor is unsupported by the matrix's own wave rows

`docs/RISK-PERF-MATRIX.md:102` states `AY-II tranche close = 0.20 / 0.55`.

From the listed wave-row floors at `docs/RISK-PERF-MATRIX.md:96-101`, the exact product is:

`0.92 × 0.85 × 0.82 × 0.88 × 0.85 × 0.98 = 0.4701`

That is not a rounding path to `0.55`. If the intent was a calibrated uplift due to correlated floors or non-independence, the document does not say so. As written, the row is mathematically unsupported and then feeds the inflated `0.44` and `0.15` cascade numbers.

### 3. The BA-opening logic in the matrix contradicts BA's own opening contract

The matrix says:

- `docs/RISK-PERF-MATRIX.md:162-166`: if AZ-II invokes `bbnf-tape-mini`, BA still opens

BA's tranche doc says the opposite:

- `docs/tranches/BA/BA.md:63-80`: BA requires full `StructRegistry` coverage, full tape deletion, and does **not** open on a partial substrate

This contradiction is load-bearing for Axis 6 because it invalidates the downstream floor cascade interpretation:

- if AZ-II closes on `bbnf-tape-mini`, BA floor should not be treated as open and multiplicatively available
- the prose at `docs/RISK-PERF-MATRIX.md:241-247` overstates what AZ-I checkpointing unlocks when it says host-binding work can start against AZ-I output without waiting for BBNF's outcome

Checkpointing and reversal isolation are real benefits. BA-open-under-escape is not.

## Open

### 1. The base-rate calibration is plausible, but not replayable from the matrix itself

`docs/RISK-PERF-MATRIX.md:56-60` gives the key priors:

- Era V declared perf gate without revert: `~35%`
- Eras III-IV: `~70%`

Those numbers are directionally consistent with the archaeology narrative, but the matrix does not show the numerator / denominator derivation, and the archaeology docs do not expose a simple tally table that lets an independent reader recompute them quickly.

This leaves the calibration believable but not audit-replayable. A one-table appendix should name the exact tranche/gate sample that yields 35% and 70%.

### 2. The sensitivity deltas are asserted, not derived

The levers at `docs/RISK-PERF-MATRIX.md:372-392` claim cascade drops of `~20%`, `~10%`, and `~25%`, but no arithmetic or scenario table is provided. They read as sensible expert judgment, not as verifiable calculations.

That is acceptable for heuristic planning, but not yet for an audit-grade quantitative appendix.

### 3. The BB dependency story still needs one normalization pass

The matrix at `docs/RISK-PERF-MATRIX.md:183-186` says BB opens on `AZ-I + AY-II` and is not blocked on AZ-II. `docs/tranches/BB/BB.md:262-265` says the same. But `docs/tranches/BB/BB.md:269-273` also contains language implying tape abrogation is already complete at AZ-I close, which is not true under the current AZ split.

That inconsistency does not break the matrix's BB wave assignments, but it does leave the downstream dependency narrative less crisp than it should be.

## Verdict

`docs/RISK-PERF-MATRIX.md` is strongest as a strategic planning instrument and weakest as a strict quantitative appendix.

What checks out:

- the AU performance anchors for JSON/CSS/Sheets
- the directional correction on AZ split arithmetic
- the main AZ-I, BA, and BB wave-to-performance assignments
- the core reframe that the split buys reversal scope and checkpoint quality, not raw probability

What needs redress before this document should be treated as mathematically authoritative:

1. Normalize tranche-close rows to either exact products or explicitly calibrated overrides.
2. Recompute the cascade from one declared model only.
3. Remove the BA-opens-on-`bbnf-tape-mini` claim unless BA itself is rewritten to permit it.
4. Separate BBNF correctness/reproducibility marks from AU-MB/s-anchored performance rows.

As written, the plan logic is mostly sound; the arithmetic presentation is not yet tight enough to serve as the repo's final quantitative source of truth.
