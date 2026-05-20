# SK-V12 S-P1 Hardening V5 - CH1 Correctness

Verdict: ACCEPT.

Lens: CH1 correctness confirmation for repo commit
`fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb` plus retained artifacts under
`/tmp/skv12-p1`.

## Evidence

1. Self-time source and symbol fields are clean.

   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` has 82 data rows, 16
   fields per row, no empty `top_leaf` / `top_leaf_source` cells, and zero
   `:0`, `unresolved`, `unknown`, `unprofiled`, or `??` hits in those fields.
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` has 410 data rows, 9
   fields per row, no empty `symbol` / `source` cells, and the same zero-marker
   result. A direct `rg` scan over both TSVs also found no `:0` or unresolved
   marker anywhere in either file. This confirms the V3 fold invariant now
   recorded in `skv12-p1-capture-manifest.md`.

2. Replay ledger validity holds.

   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` has 506
   data rows, 14 fields per row, and no duplicate
   lane/family/plane/corpus/mode keys. Lane counts match the manifest: 82
   `pmu`, 82 `samply`, 82 `xctrace-cpu-counters`, 82
   `xctrace-time-profiler-primary`, 34 `xctrace-time-profiler-export`, 48
   `xctrace-time-profiler-export-primary`, 48
   `xctrace-time-profiler-product-v2`, and 48
   `xctrace-time-profiler-product-v2-export`. All referenced CWDs, output
   artifacts, and status artifacts exist; the 82 absolute probe binary rows
   exist, while the non-absolute `samply` and `xctrace` rows are tool-command
   lanes as described by the ledger.

3. PMU aggregates are internally consistent.

   Recomputing weighted aggregates from `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv`
   and `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` yields the documented values:
   parse 34 rows at `12274.872 Mbps`, `2.920217 c/B`, `0.204887 CPI`; direct
   34 rows at `8278.039 Mbps`, `4.290305 c/B`, `0.183717 CPI`; typed guards 14
   rows at `11338.859 Mbps`, `3.123172 c/B`, `0.185056 CPI`. These match P1-D,
   P1-E, and the capture manifest. PMU row authority remains
   cycles/instructions/c/B/CPI only; branch, L1, and LLC counters are not
   inferred.

4. Mode III remains an absence boundary.

   P1-C and the manifest state that `/tmp/skv12-p1` contains parse, direct, and
   typed lanes but no fresh Mode III samply call stacks for
   `host_call_eager_decode`, `alternate_scalar_plan`, `cold_first_parse`, and
   no fresh structural-scan-only xctrace lane. A retained artifact scan found
   no `/tmp/skv12-p1/samply/probes`, `json_probes_*`, or structural-scan paths.
   The Mode III material is therefore diagnostic W0 Criterion evidence only,
   not fresh SK-V12 hot-leaf authority.

5. No unsupported row movement is present.

   `git diff --name-status db2c999b..HEAD -- skinny/RESULTS.md
   skinny/REDRESS.md` produced no output. P1-F records the unchanged live
   surface: 16 `parse_only S / NO-GO`, 1 `parse_only L / NO-GO`, 4
   `direct_to_struct A / GO`, 13 `direct_to_struct N-direct / NO-GO`, 7
   `real_typed_struct A / GO`, and overall `N-direct / NoGo`. REDRESS 120
   remains the close authority: W9 admits no direct row and no W0-clamped row.

## Required Fold

None. The V5 CH1 confirmation accepts the V4 all-ACCEPT packet for
PASS-1-PROFILE correctness.
