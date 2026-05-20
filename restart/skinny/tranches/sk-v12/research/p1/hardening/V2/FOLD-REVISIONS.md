# SK-V12 S-P1 Hardening V2 Fold Revisions

Pass: S-P1 Profile. Cycle: V2 CHALLENGE -> V3 fold.
Date: 2026-05-20.
Scope: fold the V2 CH1 and CH4 REVISE dispositions without behavior-source
changes.

## CH1 Fold - Line-Zero Source Anchors

The V2 CH1 blocker was limited to xctrace-derived source anchors ending in
`:0` inside the retained self-time TSVs. The fold re-parsed the existing
Time Profiler XML under `/tmp/skv12-p1`; it did not record fresh benchmark or
profile runs.

Derived artifacts regenerated:

- `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
- `/tmp/skv12-p1/time_profile_parse_table.md`
- `/tmp/skv12-p1/time_profile_direct_table.md`
- `/tmp/skv12-p1/time_profile_typed_table.md`

Validation:

```bash
awk -F '\t' 'NR>1 {n++; if ($16 ~ /:0$/) z++} END {print n, z+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if ($9 ~ /:0$/) z++} END {print n, z+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 410 0

awk -F '\t' 'NR>1 {n++; if ($16 ~ /UNRESOLVED/) z++} END {print n, z+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 82 0

awk -F '\t' 'NR>1 {n++; if ($9 ~ /UNRESOLVED/) z++} END {print n, z+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 410 0
```

The manifest now states that line-zero frames were normalized to current source
anchors in the derived TSVs.

## CH4 Fold - Replay Surface And Samply Policy

The fold adds the repo-tracked replay ledger
`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`.

Replay row counts:

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

Every replay row names lane, family, plane, corpus, mode, launch alias,
iteration count, cwd, binary path, expected return-code policy, output artifact,
status artifact, full command, and notes.

The manifest and P1-A/P1-B now label samply `--save-only` rows as retained
artifact-only evidence. Exported xctrace Time Profiler XML remains the self-time
authority for SK-V12 S-P1.

## Unchanged Boundaries

- PMU aggregate values remain parse `2.920217 / 0.204887`, direct
  `4.290305 / 0.183717`, and typed `3.123172 / 0.185056`.
- Mode III remains an explicit absence boundary.
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, and behavior source remain
  unchanged by this fold.
