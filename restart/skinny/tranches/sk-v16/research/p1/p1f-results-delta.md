# SK-V16 P1-F: RESULTS Extraction And Delta

Pass: S-P1 Profile. Cycle: V16.
Date: 2026-05-28.
Scope: extraction of `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md`.
Output: this file.
Baseline: SK-V16-open (`5ed43f8e1`).
Host triple: `aarch64-apple-darwin`.
Build flags: n/a for document extraction.
Profile tool: S-P1 P1-A/B/C artifacts.
Corpus coverage: 51 JSON admitted rows and 24 CSS open rows.

## Section 1 - Method

```sh
rg -c '\| json/.+\| .*\| ADMITTED \|' restart/skinny/ROLLING-SOTA-DELTA.md
rg -c '\| css_l4/.+\| .*\| OPEN \|' restart/skinny/ROLLING-SOTA-DELTA.md
rg -n 'AUDIT-SUSTAINED|AUDIT-FALSIFIED|not_admitted' skinny/RESULTS.md restart/skinny/ROLLING-SOTA-DELTA.md
```

## Section 2 - Findings

Admission surface:

| Surface | Count | Status |
|---|---:|---|
| JSON parse_only | 17 | ADMITTED / AUDIT-SUSTAINED |
| JSON direct_to_struct | 17 | ADMITTED / AUDIT-SUSTAINED |
| JSON real_typed_struct | 17 | ADMITTED / AUDIT-SUSTAINED |
| CSS L4 | 24 | OPEN / diagnostic-only |

P1-F is observational. This artifact records extraction, profiling references,
and delta telemetry only. It does not admit rows, change thresholds, or convert
`OPEN` to `ADMITTED`.

CSS rows remain `OPEN`. W8R full-parse, `CSS_GENERATED_RS`, fact-stream,
brace/delimiter summary, FNV metadata, wrong-plane/cross-plane comparator,
sidecar, historical, stale, or broadcast evidence is diagnostic-only and cannot
update rolling admission status.

## Section 3 - Delta Vs SK-V15

No row changes. P1 attached fresh S-P1 profile references to the JSON admitted
baseline but did not change row status:

| Row family | Prior | SK-V16 P1 |
|---|---|---|
| JSON parse_only 17/17 | ADMITTED | ADMITTED, re-profiled |
| JSON direct 17/17 | ADMITTED | ADMITTED, re-profiled |
| JSON typed 17/17 | ADMITTED | ADMITTED, re-profiled |
| CSS L4 24/24 | OPEN | OPEN, no typed equality proof |

## Section 4 - Anomalies And Masking Signals

Do not broadcast one CSS measurement across 24 rows as admission. Every
profiled row must cite a row-local run/artifact, or say `unprofiled`/`absent`
with cause. Positive diagnostic parse margins remain non-admission until the
SK-V16 typed report gate proves grammar-derived provider, same-workload
typed-summary equality, and cssparser SOTA.

## Section 5 - Sources

- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v16/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v16/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
