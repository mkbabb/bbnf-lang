# SK-V8 W4 Hardening V2 CH2

Verdict: ACCEPT.

Confidence: 95%.

## Findings

- Numeric evidence is sufficient to reject the three-row source plan. Under
  `time <= sonic * 1.10`, Apache passes: `95.347us <= 101.907us`.
- `random` fails: `569.57us > 509.586us`.
- `numbers` fails: `106.43us > 102.532us`, and the recorded `+6.3287%`
  Track 2 regression is enough to reject the candidate even before report
  admission.
- REDRESS Item 93 now fail-closes the route: source patch rejected, no Lock 14
  allowance, `skinny/RESULTS.md` unchanged.
- No surviving W4 row-table admission or source-admission claim was found.
  Remaining W4 source language is candidate/rejected/reverted language.

## Required Folds

None required.
