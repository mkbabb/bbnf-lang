# SK-V12 W0-A1: Profile Authority

Date: 2026-05-20.
Scope: SK-V12 W0 read-only audit of the opening profile authority, replay
manifest, PMU tables, and self-time exports.
Output: this file.

## Section 1 - Findings

The W0 profile authority is present and internally coherent. The SK-V12 S-P1
packet binds the opening baseline to source commit `50bd1648`, profile root
`/tmp/skv12-p1`, target root `/tmp/skv12-profile-target-50bd1648`, replay TSV
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`, and the
self-time summary/detail exports under `/tmp/skv12-p1/xctrace-time-profiler/`.

The S-P1 hardening record converged after V4 and V5 six-lens review. The replay
ledger has 506 data rows. Lane counts are:

| Lane | Rows |
|---|---:|
| `pmu` | 82 |
| `samply` | 82 |
| `xctrace-cpu-counters` | 82 |
| `xctrace-time-profiler-primary` | 82 |
| `xctrace-time-profiler-export` | 34 |
| `xctrace-time-profiler-export-primary` | 48 |
| `xctrace-time-profiler-product-v2` | 48 |
| `xctrace-time-profiler-product-v2-export` | 48 |

The `/tmp/skv12-p1` artefacts exist on this host. The PMU capture status file
records 328 PASS rows. The parse/product PMU TSVs contain 34 and 48 data rows,
respectively. The self-time summary contains 82 rows; the detail export contains
410 rows. No self-time row resolves to `:0` or `UNRESOLVED_LINE_ZERO`.

## Section 2 - Recommendations

W0 should consume the exact S-P1 artefacts named above rather than a broad
`/tmp` glob. The xctrace Time Profiler XML-derived TSVs are the self-time
authority. The samply artefacts remain retained evidence, not a replacement for
the xctrace authority.

## Section 3 - Risks

The profile root is under `/tmp`, so later host cleanup can invalidate replay.
W0 should preserve the manifest path references and not silently fall back to a
default Criterion or local target cache.

## Section 4 - Sources

- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- `/tmp/skv12-p1/pmu/capture_status.tsv`
- `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
- `/tmp/skv12-p1/pmu/product_pmu_rows.tsv`
- `/tmp/skv12-p1/xctrace-time-profiler/self_time_summary.tsv`
- `/tmp/skv12-p1/xctrace-time-profiler/self_time_details.tsv`
