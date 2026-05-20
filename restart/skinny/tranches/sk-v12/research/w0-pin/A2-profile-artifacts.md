# SK-V12 W0 PIN Research A2 - Profile Artifacts

Date: 2026-05-20.
Scope: read-only pin S-P1 profile artifact completeness.
Verdict: PASS.

## Findings

The pin S-P1 profile root `/tmp/skv12-pin-p1` is present and complete enough
for W0.

| Artifact | Finding |
|---|---|
| `/tmp/skv12-pin-p1/pmu/capture_status.tsv` | 82/82 PASS rows |
| `/tmp/skv12-pin-p1/pmu/parse_pmu_rows.tsv` | 34 data rows, 17 JSON corpora x 2 tracks |
| `/tmp/skv12-pin-p1/pmu/product_pmu_rows.tsv` | 48 data rows, 17 direct x 2 tracks plus 7 real-typed x 2 tracks |
| `/tmp/skv12-pin-p1/samply/capture_status.tsv` | 82/82 PASS rows; referenced artifacts exist |
| `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` | 212/212 PASS rows; referenced artifacts exist |
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv` | 82 data rows, 0 unresolved source anchors |
| `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` | 410 data rows, 0 unresolved source anchors |

Done markers exist:

- `/tmp/skv12-pin-p1/pmu/done.txt`
- `/tmp/skv12-pin-p1/samply/done.txt`
- `/tmp/skv12-pin-p1/xctrace/done.txt`

CSS target artifacts are intentionally absent in W0. The generated CSS runtime,
lightningcss same-plane comparator, and strict equality oracle do not exist
yet; W1b-1/W1b-2 own that work.

## Sources

- `/tmp/skv12-pin-p1`
- `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
