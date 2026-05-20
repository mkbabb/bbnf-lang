Verdict: ACCEPT

# SK-V12 S-P1 Hardening V4 CH5: Hidden Coupling

Date: 2026-05-20.
Lens: CH5 hidden coupling.
Scope: audit commit `6d19429f` and the SK-V12 S-P1 packet for hidden coupling
after the V3 symbol-label fold.

## Evidence

1. Commit `6d19429f` is confined to the profile packet. Its changed paths are
   exactly `restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md`
   and `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`.
   A targeted diff from the capture source baseline to `6d19429f` over
   `skinny/crates`, `skinny/Cargo.toml`, `skinny/Cargo.lock`,
   `skinny/RESULTS.md`, and `skinny/REDRESS.md` is empty. The audited commit
   therefore does not hide a behavior, result-row, REDRESS, or gate mutation
   behind a documentation fold.

2. Artifact paths are explicit evidence paths, not repo-tracked authority. The
   manifest pins source baseline `50bd1648`, capture root `/tmp/skv12-p1`, build
   root `/tmp/skv12-profile-target-50bd1648`, and says result authority remains
   `skinny/RESULTS.md` while the manifest records profile evidence only and
   moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11`-`:16`).
   It names the repo-tracked replay TSV as the authoritative command surface for
   independent replay, while command blocks are readable recipes only (`:38`-`:43`).
   The V3 fold says it re-parsed existing xctrace Time Profiler XML under
   `/tmp/skv12-p1`, recorded no fresh benchmark or profile runs, and regenerated
   only the retained derived self-time artifacts
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:9`-`:20`).

3. The replay ledger still matches the `/tmp` status surface. I verified
   `skv12-p1-replay.tsv` has 506 data rows, 14 columns, and zero bad field
   counts. Lane counts remain 82 `pmu`, 82 `samply`, 82
   `xctrace-cpu-counters`, 82 `xctrace-time-profiler-primary`, 34
   `xctrace-time-profiler-export`, 48 `xctrace-time-profiler-export-primary`,
   48 `xctrace-time-profiler-product-v2`, and 48
   `xctrace-time-profiler-product-v2-export`, matching the manifest (`:45`-`:62`).
   Every replay `output_artifact` and `status_artifact` path exists. The primary
   status file has 328 PASS rows: 166 `rc=0` and 162 `rc=54`; the original
   Time Profiler export status has 82 PASS exports and 9,327,356 XML bytes; the
   product-v2 status has 48 PASS exports and 23,383,417 XML bytes. The known
   initial product cwd failure remains isolated in
   `/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv` with 34 parse
   PASS rows and 48 product `rc=134` failures, matching the manifest's explicit
   cwd boundary (`:84`-`:88`).

4. Source and symbol normalization is bounded to attribution fields. The V3 fold
   validates 82 summary rows and 410 detail rows with zero source `:0`, zero
   symbol `:0`, zero any-field `:0`, and zero unresolved markers
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:22`-`:54`).
   I rechecked all five regenerated retained artifacts:
   `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv`,
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`,
   `/tmp/skv12-p1/time_profile_parse_table.md`,
   `/tmp/skv12-p1/time_profile_direct_table.md`, and
   `/tmp/skv12-p1/time_profile_typed_table.md`; each has zero line-zero labels
   and zero `UNRESOLVED` markers. The manifest's strengthened invariant is
   therefore true for the cited `/tmp` artifacts and remains an attribution
   repair, not implementation authority (`skv12-p1-capture-manifest.md:157`-`:169`).

5. Cwd, alias, and target-binary handling are explicit. The manifest separates
   the failed repository-root product PMU run from the final `skinny/` product
   run and records the `update_center` row key versus `update-center` launch
   alias rule (`skv12-p1-capture-manifest.md:84`-`:94`). I verified the replay
   ledger has 274 rows with cwd `/Users/mkbabb/Programming/bbnf-lang/skinny`
   and 232 rows with cwd `/Users/mkbabb/Programming/bbnf-lang`, and all 38
   `update_center` rows use the expected parse filename or product launch alias;
   direct `update_center` alias mismatches are zero. The target binaries named
   by the packet exist as executable arm64 Mach-O files at
   `/tmp/skv12-profile-target-50bd1648/release/profile_direct` and
   `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe`.

6. Profile-only evidence does not leak into implementation authority. The
   manifest labels samply rows artifact-only and sources self-time percentages
   from exported xctrace Time Profiler XML (`skv12-p1-capture-manifest.md:58`-`:62`).
   P1-F says fresh `/tmp/skv12-p1` artifacts are profiling evidence only and are
   not consumed by `skinny/RESULTS.md` as row movement, hot-leaf symbol
   resolution, or admission evidence
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:21`-`:33`).
   `SYNTHESIS.md` says SK-V12 synthesis is not behavior implementation
   authority and pre-blocks PMU, cycles, structural-scan, masking-probe,
   Criterion-slope, sidecar-freshness, and parser inventory evidence as behavior
   producers (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-`:9`, `:228`-`:244`).

## Required Fold

None. Commit `6d19429f` and the folded S-P1 packet require or imply no behavior
source, `skinny/RESULTS.md`, or `skinny/REDRESS.md` change.
