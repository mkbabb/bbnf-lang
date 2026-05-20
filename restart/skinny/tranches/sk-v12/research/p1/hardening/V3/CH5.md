# SK-V12 S-P1 Hardening V3 CH5 Hidden Coupling

Pass: S-P1 Profile. Cycle: V3.
Date: 2026-05-20.
Lens: CH5 hidden coupling.
Scope: current repo commit `ffe5553d` and the folded S-P1 packet.
Output: this file only.

## Verdict

Verdict: ACCEPT

No hidden coupling defect remains in the V3 fold. The packet keeps profile
artifacts, replay metadata, source-line attribution, target binaries, and row
authority in separate lanes. No behavior source, `skinny/RESULTS.md`, or
`skinny/REDRESS.md` change is required or implied.

## Evidence

1. **Artifact paths stay fenced from repo-tracked authority.** The S-P1 prompt
   permits flame/profile artifacts outside the doc tree under `/tmp/skv{N}-p1/`
   while committing only the P1 artifacts that cite them
   (`restart/prompts/skinny/PASS-1-PROFILE.md:204-206`). The V3 manifest follows
   that contract: it declares `/tmp/skv12-p1` and
   `/tmp/skv12-profile-target-50bd1648` as capture/build roots, says result
   authority remains `skinny/RESULTS.md`, and says the manifest records profile
   evidence only and moves no rows
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11-16`).
   The repo-tracked replay ledger is the command surface, not a committed copy of
   the `/tmp` artifacts (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:38-62`).

2. **The replay ledger is concrete and matches the capture-status shape.** The
   folded ledger `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv`
   validates as 14 columns across 506 data rows. Its lane counts are 82 PMU, 82
   samply, 82 xctrace CPU Counter, 82 primary xctrace Time Profiler, 34 parse
   exports, 48 primary product exports, 48 product-v2 Time Profiler recaptures,
   and 48 product-v2 exports. I checked every `output_artifact` and
   `status_artifact` path named in the ledger exists under `/tmp`. The primary
   status file has 328 PASS rows: 82 PMU, 82 samply, and 164 xctrace rows, with
   xctrace `rc=54` retained only where the status/log policy accepts a saved
   time-limit or target-exit trace. Product-v2 status has 48 PASS exports and
   23,383,417 XML bytes. This closes the V2 CH4 placeholder replay risk without
   turning `/tmp` capture files into behavior authority.

3. **Samply is artifact-only; xctrace XML owns self-time.** The manifest says
   samply rows are retained artifact-only because they use `--save-only`, and
   self-time percentages are sourced from exported xctrace Time Profiler XML
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:58-62`).
   P1-A and P1-B carry the same policy for parse and product rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:101-110`,
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:120-136`).
   P1-E then treats the derived self-time tables as xctrace leaf attribution, not
   PMU counters or row admission (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:72-80`,
   `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:360-365`).

4. **Source-line normalization is bounded to attribution.** The V2 fold
   required replacing line-zero self-time anchors. The V3 fold records that the
   summary table has 82/82 rows with no `top_leaf_source` ending in `:0`, and
   the detail table has 410/410 rows with no `source` ending in `:0` and no
   `UNRESOLVED_LINE_ZERO` markers
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:145-168`,
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md:8-44`).
   I rechecked `/tmp/skv12-p1/time_profile_hot_leaf_summary.tsv` and
   `/tmp/skv12-p1/time_profile_hot_leaf_details.tsv`: 82 summary rows and 410
   detail rows, zero `:0`, zero unresolved markers. Because
   `git diff --name-status 50bd1648..ffe5553d -- skinny/crates skinny/Cargo.toml
   skinny/Cargo.lock skinny/RESULTS.md skinny/REDRESS.md` returns no paths, the
   current source anchors do not hide a changed implementation.

5. **CWD, alias, and target-binary handling are explicit.** The manifest records
   the failed initial product run from the repository root separately at
   `/tmp/skv12-p1/pmu/capture_status.initial-product-cwd-fail.tsv`, and states
   the final product PMU run is from `skinny/`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:84-94`).
   P1-D repeats that the initial product rows failed fixture lookup at `rc=134`
   and that the final rerun from `skinny/` passed parse/direct/typed PMU rows
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:72-82`).
   The replay ledger records the `update_center` row key versus `update-center`
   launch alias for product direct/typed rows and uses absolute parse fixture
   paths for `update-center.json`. Product direct/typed replay rows run from
   `/Users/mkbabb/Programming/bbnf-lang/skinny`; export rows run from the repo
   root. The target binaries named by the packet exist as arm64 Mach-O
   executables at `/tmp/skv12-profile-target-50bd1648/release/profile_direct` and
   `/tmp/skv12-profile-target-50bd1648/release/xctrace_probe`.

6. **Profile-only evidence does not leak into implementation authority.** SK-V12
   synthesis says it is not behavior implementation authority, creates no
   `SPEC.md` or `DISPATCH-PROMPT.md`, and requires S-P3 to own the later wave
   plan (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5-9`). It also makes the
   generated non-JSON baseline the first material target and blocks PMU, cycles,
   structural-scan, masking-probe, Criterion-slope, sidecar-freshness, and parser
   inventory evidence as behavior producers
   (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38-49`,
   `restart/skinny/tranches/sk-v12/SYNTHESIS.md:228-246`). P1-B keeps JSON
   direct/typed product profiling from substituting for a non-JSON baseline
   (`restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:256-268`),
   P1-D says PMU values move no rows and admit no direct or typed row
   (`restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:84-92`), and
   P1-F says fresh `/tmp/skv12-p1` captures are not consumed by
   `skinny/RESULTS.md` as row movement, hot-leaf symbol resolution, or admission
   evidence (`restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:21-33`).

7. **Commit `ffe5553d` is doc-only for the S-P1 fold.** `git show
   --name-status ffe5553d` changes only:
   `restart/skinny/tranches/sk-v12/research/p1/hardening/V2/FOLD-REVISIONS.md`,
   P1-A, P1-B, P1-D, P1-E, the capture manifest, and the new replay TSV. It does
   not touch behavior source, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.
   Additional diff filters from SK-V12-open and SK-V11 close also return no
   paths under behavior source, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.

## Required Fold

None.
