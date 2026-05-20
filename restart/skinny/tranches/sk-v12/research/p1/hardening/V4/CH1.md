# SK-V12 S-P1 Hardening V4 - CH1 Correctness

Verdict: ACCEPT.

Lens: CH1 correctness for current repo commit
`6d19429f2c0afd25d8746658b4bdb458226402fe` plus retained artifacts under
`/tmp/skv12-p1`.

Scope: `restart/prompts/skinny/PASS-1-PROFILE.md`, all six S-P1 artifacts,
`skv12-p1-capture-manifest.md`, `skv12-p1-replay.tsv`, V1/V2/V3 hardening and
fold files, `skinny/RESULTS.md`, `skinny/REDRESS.md`, and the retained
`/tmp/skv12-p1` profile artifacts.

## Findings

1. PASS - The V3 line-zero self-time blocker is folded.

   CH1 requires every hot-leaf claim to cite symbol path, percent self-time, and
   source file:line, and to resolve `unprofiled` cells
   (`restart/prompts/skinny/PASS-1-PROFILE.md:123`). V3 left one blocker:
   self-time TSV `top_leaf` and `symbol` fields still contained line-zero
   pseudo-symbols. The V3 fold states those displayed fields were regenerated
   from existing xctrace Time Profiler XML without fresh benchmark/profile runs
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:7`).

   Direct validation of the retained TSVs now matches the stricter manifest
   invariant (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:165`):
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` has 82 data rows and
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv` has 410 data rows. Summary
   `top_leaf` / `top_leaf_source` fields have `0` `:0` hits and `0` unresolved
   marker hits; detail `symbol` / `source` fields have `0` `:0` hits and `0`
   unresolved marker hits. A whole-row scan of both TSVs also found `0` `:0`
   hits and `0` unresolved marker hits.

2. PASS - The replay ledger still validates.

   The manifest defines `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
   as the authoritative replay surface and lists the expected lane counts
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:40`,
   `:45`). The current ledger has 506 data rows, 14 fields per row, no duplicate
   lane/family/plane/corpus/mode keys, and the same lane counts: 82 `pmu`, 82
   `samply`, 82 `xctrace-cpu-counters`, 82 `xctrace-time-profiler-primary`, 34
   `xctrace-time-profiler-export`, 48 `xctrace-time-profiler-export-primary`,
   48 `xctrace-time-profiler-product-v2`, and 48
   `xctrace-time-profiler-product-v2-export`. Every referenced output artifact,
   status artifact, CWD, and absolute binary path exists. The samply rows remain
   artifact-only and do not source self-time percentages
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:58`).

3. PASS - PMU aggregate arithmetic is consistent.

   Recomputing weighted values from `/tmp/skv12-p1/pmu/parse_pmu_rows.tsv` and
   `/tmp/skv12-p1/pmu/product_pmu_rows.tsv` yields parse 34 rows at
   `12274.872 Mbps`, `2.920217 c/B`, `0.204887 CPI`; direct 34 rows at
   `8278.039 Mbps`, `4.290305 c/B`, `0.183717 CPI`; and typed 14 rows at
   `11338.859 Mbps`, `3.123172 c/B`, `0.185056 CPI`. These match P1-D, P1-E,
   and the manifest (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:99`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:112`,
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:179`).
   P1-D and P1-E both keep branch/L1/LLC counters absent rather than inferred.

4. PASS - Mode III remains an explicit absence boundary.

   P1-C records 17/17 corpus coverage for W0 masking and structural-scan facts,
   17/17 fresh parse PMU rows, and 0/17 fresh Mode III samply call-stack probe
   rows (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:17`).
   It also states there is no `/tmp/skv12-p1/samply/probes`, no
   `json_probes_*` capture, and no structural-scan capture under `/tmp/skv12-p1`
   (`restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:54`).
   A retained artifact scan found no matching probe or structural-scan paths.
   The manifest preserves the boundary: no S-P2/S-P3 wave may use Mode III
   symbols as fresh SK-V12 hot-leaf authority without a later capture
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:193`).

5. PASS - No unsupported row movement is present.

   Live `skinny/RESULTS.md` extraction still has 41 JSON rows: 17
   `parse_only`, 17 `direct_to_struct`, and 7 `real_typed_struct`. Outcomes are
   unchanged: 16 `parse_only S / NO-GO`, 1 `parse_only L / NO-GO`, 4
   `direct_to_struct A / GO`, 13 `direct_to_struct N-direct / NO-GO`, and 7
   `real_typed_struct A / GO`. P1-F records zero delta from SK-V11 close and an
   unchanged overall `N-direct / NoGo` seed surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:66`,
   `:77`). P1-E independently says no `skinny/RESULTS.md` row moved
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:340`).
   REDRESS 120 remains the governing close record: no direct row and no
   W0-clamped row was admitted (`skinny/REDRESS.md:3531`, `:3542`).

## Required Fold

None. The V4 CH1 audit accepts the V3 fold: the self-time source and symbol
fields are clean, replay remains independently checkable against retained
artifacts, PMU aggregates match, Mode III absence is bounded, and the packet
does not move or imply movement of any unsupported row.
