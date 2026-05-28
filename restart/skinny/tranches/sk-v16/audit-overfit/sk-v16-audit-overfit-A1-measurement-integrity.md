# SK-V16 S-P0 A1 - Measurement Integrity

Date: 2026-05-28.
HEAD: `fc16919d4`.
Axis: A1 measurement integrity.
Disposition: ACCEPT.

## Finding Summary

No CRITICAL or HIGH measurement-integrity blocker was found in live admitted
rows. Current admission is JSON-only:

- `restart/skinny/ROLLING-SOTA-DELTA.md` contains 51 JSON `ADMITTED` rows.
- `restart/skinny/ROLLING-SOTA-DELTA.md` contains 24 CSS L4 rows, all `OPEN`.
- The CSS 24-row duplicate throughput cluster is retained diagnostic state, not
  live admission.
- Dirty generated state is a routed blocker for future broad proof, not a
  contaminant of current JSON row admission.

## Evidence

Commands:

```sh
rg -c '\| json/.+\| .*\| ADMITTED \|' restart/skinny/ROLLING-SOTA-DELTA.md
# 51

rg -c '\| css_l4/.+\| .*\| OPEN \|' restart/skinny/ROLLING-SOTA-DELTA.md
# 24
```

`restart/skinny/ROLLING-SOTA-DELTA.md` records the CSS table values as retained
W8R diagnostic evidence only and states that CSS remains `OPEN` until fresh
typed equality and same-workload cssparser proof land.

PASS-IMPL V2 and W11 record the current CSS same-workload rejection: Track 1
`2/4`, cssparser `4/4`, typed summaries unequal, Track 1 `3.426 Mbps`,
cssparser `1995.168 Mbps`, and `admitted_rows=0`.

## Prune Candidates

1. Retire or quarantine dirty generated state before any broad SK-V16
   measurement close proof.
2. Collapse or supersede the old CSS W8R diagnostic broadcast cluster so it
   cannot be mistaken for per-row admission evidence.
3. Require fresh SK-V16 CSS typed report gates before any CSS SOTA admission:
   typed equality first, same-workload cssparser comparator, distinct per-row
   measurement, and explicit admitted-row count.
