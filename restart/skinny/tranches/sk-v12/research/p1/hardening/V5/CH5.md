Verdict: ACCEPT

# SK-V12 S-P1 Hardening V5 CH5: Hidden Coupling

Date: 2026-05-20.
Lens: CH5 hidden coupling confirmation after V4 all-ACCEPT.
Scope: audit current repo commit `fe7ae2ab` and the SK-V12 S-P1 packet for
artifact, replay, cwd/alias, source/symbol, and target-binary coupling.

## Evidence

1. Commit `fe7ae2ab40d3ba205445f07bc4cd870d68cdb1cb` is the V4 hardening
   archive, not an implementation or packet mutation. Its changed paths are
   exactly `V4/CH1.md` through `V4/CH6.md` and `V4/CONSOLIDATED.md`. A targeted
   diff from the capture source baseline `50bd1648` to `fe7ae2ab` over
   `skinny/crates`, `skinny/Cargo.toml`, `skinny/Cargo.lock`,
   `skinny/RESULTS.md`, and `skinny/REDRESS.md` is empty. A targeted diff from
   `6d19429f` to `fe7ae2ab` over the S-P1 packet, `SYNTHESIS.md`, `HANDOFF.md`,
   source, `RESULTS`, and `REDRESS` shows only the V4 challenge files. The V4
   consolidation records six-of-six ACCEPT, zero open REVISE findings, no row or
   gate movement, and routes this V5 confirmation cycle only
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V4/CONSOLIDATED.md:19`-`:28`).

2. Artifact paths remain evidence paths, not repo authority. The capture
   manifest pins source baseline `50bd1648`, capture root `/tmp/skv12-p1`, build
   root `/tmp/skv12-profile-target-50bd1648`, and says result authority remains
   `skinny/RESULTS.md` while the manifest records profile evidence only and
   moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11`-`:16`).
   The same manifest makes `skv12-p1-replay.tsv` the authoritative command
   surface and says the command blocks are recipes only (`:38`-`:43`). It also
   labels samply rows artifact-only, with self-time percentages sourced from
   exported xctrace Time Profiler XML (`:58`-`:62`).

3. Replay ledger validation still closes the path-coupling surface. Direct
   validation of
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv` found 506
   data rows, 14 fields per row, zero bad field counts, zero missing
   output/status artifacts, and lane counts matching the manifest: 82 `pmu`, 82
   `samply`, 82 `xctrace-cpu-counters`, 82 `xctrace-time-profiler-primary`, 34
   `xctrace-time-profiler-export`, 48 `xctrace-time-profiler-export-primary`,
   48 `xctrace-time-profiler-product-v2`, and 48
   `xctrace-time-profiler-product-v2-export`. The status files also match the
   manifest: primary capture has 328 PASS rows split as 166 `rc=0` and 162
   `rc=54`; original Time Profiler export has 82 PASS exports and 9,327,356 XML
   bytes; product-v2 export has 48 PASS exports and 23,383,417 XML bytes. These
   are replay/profiling facts, not implementation authority.

4. Cwd and alias handling are explicit rather than hidden coupling. The manifest
   separates the initial repository-root product PMU failure from the final
   `skinny/` product run and records the `update_center` row key versus
   `update-center` launch alias rule
   (`skv12-p1-capture-manifest.md:84`-`:94`). The replay TSV has 274 rows with
   cwd `/Users/mkbabb/Programming/bbnf-lang/skinny` and 232 rows with cwd
   `/Users/mkbabb/Programming/bbnf-lang`; all 38 `update_center` rows use the
   expected parse filename or product launch alias, with zero alias mismatches.
   The preserved initial cwd-failure file has 34 parse PASS rows and 48 product
   `rc=134` failures, matching the documented boundary rather than silently
   feeding failed product evidence into the packet.

5. Source and symbol normalization remains bounded to attribution. The V3 fold
   states it re-parsed existing xctrace Time Profiler XML under `/tmp/skv12-p1`,
   recorded no fresh benchmark or profile runs, regenerated only retained
   derived self-time artifacts, and left replay, Mode III, `RESULTS`, `REDRESS`,
   and behavior source unchanged
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/V3/FOLD-REVISIONS.md:9`-`:20`,
   `:56`-`:63`). The manifest invariant now states 82/82 summary rows and
   410/410 detail rows with no `:0` in symbol/source fields and no
   `UNRESOLVED_LINE_ZERO` markers (`skv12-p1-capture-manifest.md:165`-`:169`).
   Direct validation for this audit found zero bad labels in the summary and
   detail TSVs and zero `:0` or unresolved hits across the retained summary,
   details, parse table, direct table, and typed table.

6. Target binaries are profile targets only. The replay ledger and manifest name
   the build root as `/tmp/skv12-profile-target-50bd1648`; the two target
   binaries used by replay,
   `/tmp/skv12-profile-target-50bd1648/release/profile_direct` and
   `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe`, exist as
   executable arm64 Mach-O files. Because the guarded source/result/redress diff
   from `50bd1648` to `fe7ae2ab` is empty, these binaries are retained profile
   artifacts for the pinned baseline, not a side channel for newer implementation
   authority.

7. Profile-only evidence is consistently barred from implementation authority.
   P1-D says PMU values are profile evidence only and do not move `RESULTS`,
   admit direct/typed rows, or change the SK-V12 opening surface
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:84`-`:92`).
   P1-E says top-leaf percentages are xctrace Time Profiler attribution and row
   admission still belongs to Criterion/`skinny/RESULTS.md`
   (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:351`-`:365`).
   P1-F says fresh `/tmp/skv12-p1` artifacts are profiling evidence only and are
   not consumed by `skinny/RESULTS.md` as row movement, hot-leaf symbol
   resolution, or direct/typed admission evidence
   (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:21`-`:33`).
   `SYNTHESIS.md` is not behavior implementation authority and pre-blocks PMU,
   cycles, structural-scan, masking-probe, Criterion-slope, sidecar-freshness,
   and parser inventory evidence as behavior producers
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5`-`:9`, `:228`-`:244`).

## Required Fold

None. The V5 CH5 confirmation finds no hidden coupling path from profile-only
artifact, replay, cwd/alias, source/symbol, or target-binary evidence into
implementation authority.
