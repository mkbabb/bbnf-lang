# SK-V9 S-P1 CONVERGED

Date: 2026-05-18.
Verdict: S-P1 Profile fully converges per `restart/prompts/ORCHESTRATOR.md` §3Z.
Authority: per-lens two-consecutive-cycle audit below.

## §3Z convergence audit

| Lens | V4 | V5 | V6 | First qual | Second qual |
|---|---:|---:|---:|---|---|
| CH1 CORRECTNESS | 96.2% | 100% | — | V4 | V5 ✓ |
| CH2 GENERALITY | 97.2% | 97.4% | — | V4 | V5 ✓ |
| CH3 REGRESSION | 100% | 100% | — | V4 | V5 ✓ |
| CH4 COST | 93.3% | 100% | 100% | V5 | V6 ✓ |
| CH5 HIDDEN COUPLING | 100% | 100% | — | V4 | V5 ✓ |
| CH6 ANTI-PAPER-CLOSE | 97.0% | ACCEPT | — | V4 | V5 ✓ |

All six lenses have two consecutive qualifying cycles. S-P1 advances
to S-P2 Research per `restart/prompts/skinny/PASS-2-RESEARCH.md`.

## Cycle trajectory

V1 (2/6 ACCEPT): V1 baseline; absence-ledger, PMU unblocked but agents
shallow.

V2 (4/6 ACCEPT BLOCKED): post-W0 samply rerun; CH4 blocked on PMU/cycles
(perf absent, xctrace required full Xcode, powermetrics needed sudo).

V3 (failed): PMU unblocked via xctrace + Xcode license accept; six
agents landed real PMU data (17×2 corpora) + Time Profiler captures.
CHALLENGE V3 returned 4 of 6 lenses below the 95% bar (CH4 14%, CH2
33%, CH6 89%, CH1 67% strict). Load-bearing defects: D's S-P1
overreach with uncosted wave proposals; C's sequenced paper-close
(ran before A/B landed); JSON-role symbol leaks across the cohort.

V4 (5/6 converged, CH4 93.3%): six V3 reports edited in place per F1-F6
fold. The regression-script commit surfaced the load-bearing honesty
correction: V3's OLS coefficients were ~8× over-stated. Real fit
`ns_per_byte ≈ 1.079·(q/B) + 0.184·(n/B) + 0.051`, R²=0.371. The "10%
cut clears 7/11" forecast superseded; four LOSS rows (unicode_mixed,
unicode_escapes, y_string_unicode, gsoc-2018) cannot be closed by a
delimiter-only intervention. CH4 lone outlier with 5 named gaps.

V5 (6/6 ≥95%, 5 of 6 fully converged): six surgical edits closing the
V4 CH4 gaps + the two V4 CH1 REVISEs + the V4 CH6 residual MEDIUM. No
new measurement.

V6 (CH4 confirmation): unchanged V5 substantive; CH4 V6 = 100%. Second
consecutive qualifying cycle on CH4 achieved; pass converges.

## What S-P1 hands to S-P2

The S-P2 Research dispatch consumes:

1. **Real PMU table** at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` — 34 rows
   of cycles, instructions, CPI, cycles/byte across all 17 corpora ×
   {Track 1 generated, Track 2 hand-coded}.
2. **Per-symbol Time Profiler exports** at
   `/tmp/skv9-xctrace-v3/p1b-tp/exports/<corpus>__<track>.symbols.json`
   — top-15 self-time per row, ~1700-2000 in-process samples.
3. **Deep hot-leaf attribution** (P1-V3-C) — per-corpus per-track
   structural breakdown by primitive class.
4. **Structural correlation** (P1-V3-D) — OLS coefficients (real, not
   V3's inflated), R², residuals, the four uncloseable rows.
5. **Primitive-class vocabulary** (P1-V3-B §1.5) — substrate-neutral
   canonical 7-class set (per-string-span scanner, escape_codec_hex_unit,
   structural-element walker, etc.) with cross-grammar parameterisation
   for JSON / CSS L4 / Sheets / JS / TOML.
6. **Cleanup manifest** (P1-V3-E) — 524 doc ARCHIVE-MOVE, ~700 LOC
   SAFE-TO-DELETE, split into E1 (≤30 min LOW) + E2 (≤45 min MEDIUM +
   cargo test gate).
7. **REDRESS reconciliation** (P1-V3-F) — ~60 entries STILL-LOAD-BEARING,
   7 SUPERSEDED, 19 surgical SPEC/HANDOFF/DISPATCH-PROMPT edits queued
   under ≤30 min batch cap.

## Load-bearing diagnoses

The convergent S-P1 V3-V6 verdict:

1. **Dead SIMD structural scanner**: `scan_structurals` is 0.00%
   self-time on every (corpus, track) row. The stage-1 SIMD index
   bbnf produces is discarded; the recursive-descent parser
   re-discovers structural bytes in a scalar pass.
2. **String-scanner pair dominates loss corpora**: `match_tiny_plain_string`
   + `match_string_at_quote` reach 47-67% self-time on dense-key losses
   (lower than SC-4's 75% upper bound).
3. **Unicode-escape codec dominates y_string_unicode**: `read_hex_unit_scalar`
   + `hex_nibble` = 38-44% — a class SC-4 missed entirely.
4. **Quote density is a step function, but not the whole story**: the
   regression's per-quote coefficient is 1.079 ns/B, the per-number is
   0.184 ns/B, the intercept is 0.051. R²=0.371. Four LOSS rows
   (unicode_mixed, escapes, y_string_unicode, gsoc-2018) have throughput
   gaps exceeding 130-460% of the regression's full per-byte budget —
   delimiter-only intervention is insufficient.

## Next move

`restart/prompts/skinny/PASS-2-RESEARCH.md` S-P2 Research dispatches
six sub-agents on:
- W3 union event-model fit-gate rejection diagnosis + alternate design.
- Retained class/event grammar + `ValueRef` cursor proof shape.
- Apache/CITM measured-row admission methodology.
- Host-targeted aarch64 ASM/SIMD intrinsic opportunities.
- Unicode-escape codec design (the four uncloseable rows).
- Updated SOTA competitor teardown for parse + node speed on Apple M5 Max.

S-P2 dispatch unblocked. The framework continues.
