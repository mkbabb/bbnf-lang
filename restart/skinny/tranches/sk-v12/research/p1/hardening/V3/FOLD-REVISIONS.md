# SK-V12 S-P1 Hardening V3 Fold Revisions

Pass: S-P1 Profile. Cycle: V3 CHALLENGE -> V4 fold.
Date: 2026-05-20.
Scope: fold the V3 CH1 REVISE disposition without behavior-source changes.

## CH1 Fold - Line-Zero Symbol Labels

The V3 CH1 blocker was limited to the displayed `top_leaf` and `symbol` fields
inside the retained self-time TSVs. The source columns were already concrete
after the V2 fold. This fold re-parsed the existing xctrace Time Profiler XML
under `/tmp/skv12-p1`; it did not record fresh benchmark or profile runs.

Derived artifacts regenerated:

- `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`
- `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`
- `/tmp/skv12-p1/time_profile_parse_table.md`
- `/tmp/skv12-p1/time_profile_direct_table.md`
- `/tmp/skv12-p1/time_profile_typed_table.md`

Validation:

```bash
awk -F '\t' 'NR>1 {
  n++;
  if ($16 ~ /:0([^0-9]|$)/) src++;
  if ($15 ~ /:0([^0-9]|$)/) sym++;
  for (i=1;i<=NF;i++) if ($i ~ /:0([^0-9]|$)/) any++;
} END {print n, src+0, sym+0, any+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 82 0 0 0

awk -F '\t' 'NR>1 {
  n++;
  if ($9 ~ /:0([^0-9]|$)/) src++;
  if ($8 ~ /:0([^0-9]|$)/) sym++;
  for (i=1;i<=NF;i++) if ($i ~ /:0([^0-9]|$)/) any++;
} END {print n, src+0, sym+0, any+0}' \
  /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 410 0 0 0

awk -F '\t' 'NR>1 {for (i=1;i<=NF;i++) if ($i ~ /UNRESOLVED/) {u++; break}}
  END {print u+0}' /tmp/skv12-p1/time_profile_hot_leaf_summary.tsv
# 0

awk -F '\t' 'NR>1 {for (i=1;i<=NF;i++) if ($i ~ /UNRESOLVED/) {u++; break}}
  END {print u+0}' /tmp/skv12-p1/time_profile_hot_leaf_details.tsv
# 0
```

The capture manifest now states the stricter invariant: no `:0` remains in the
summary `top_leaf` / `top_leaf_source` fields or the detail `symbol` / `source`
fields, and no unresolved line-zero markers remain.

## Unchanged Boundaries

- The replay TSV remains the exact command surface for independent replay.
- PMU aggregate values remain parse `2.920217 / 0.204887`, direct
  `4.290305 / 0.183717`, and typed `3.123172 / 0.185056`.
- Mode III remains an explicit absence boundary.
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, and behavior source remain
  unchanged by this fold.
