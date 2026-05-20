# PIN-V1 CH1 - Correctness

Verdict: REVISE

Score: 88%

## Blocking Findings

1. Stale pre-pin replay authority remains load-bearing.

   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:70-73`
   declares `skv12-p1-replay.tsv` the authoritative independent replay surface.
   That TSV is not pin-aware: all 506 data rows still point at `/tmp/skv12-p1`
   and `/tmp/skv12-profile-target-50bd1648`, with 0 rows pointing at
   `/tmp/skv12-pin-p1` or `/tmp/skv12-pin-profile-target-cf7848b2`
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv:2` is
   representative). This conflicts with the manifest's pin authority at
   `skv12-p1-capture-manifest.md:11-20` and with the task requirement to validate
   the pinned fold against `/tmp/skv12-pin-p1`, not pre-pin prose memory.

   Required fold edit: either regenerate/track a pin replay ledger whose command,
   binary, output, and status paths use `/tmp/skv12-pin-p1` and
   `/tmp/skv12-pin-profile-target-cf7848b2`, or mark `skv12-p1-replay.tsv` as
   pre-pin historical only and name the pin replay authorities explicitly
   (`/tmp/skv12-pin-p1/pmu/pmu-commands.sh`,
   `/tmp/skv12-pin-p1/samply/samply-commands.sh`, and
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`). Update manifest sections
   `Run Identity`, `Exact Replay Surface`, `Primary Capture`, and
   `Self-Time Export` so no `/tmp/skv12-p1` path is presented as current
   replay authority.

2. Time Profiler export PASS claims do not match the pin status artifact.

   The manifest reports `Time Profiler XML exports | 82 | PASS` at
   `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:31`,
   and P1-B reports `product-v2 XML exports | 48/48 PASS` at
   `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:29`. The
   cited pin artifact `/tmp/skv12-pin-p1/time_profile_export_status.tsv` has 82
   data rows, but every row's status is `SKIP`, not `PASS`; the export files do
   exist and have rc=0. The xctrace capture rows themselves are PASS in
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`, but the export-status claim is
   not artifact-true.

   Required fold edit: either rerun/record export rows with status `PASS`, or
   change the fold wording to `82 exports present, rc=0, status=SKIP because the
   XML files already existed`, reserving `PASS` for the actual xctrace capture
   status rows.

## Nonblocking Notes

- JSON corpus coverage is present in the pin artifacts: PMU and samply each have
  82/82 PASS rows; xctrace has 212/212 PASS capture rows. PMU row split is
  34 parse, 34 direct, and 14 typed.
- Product-plane coverage is present: direct covers 17/17 corpora x Track 1/2,
  and typed covers the 7 real-typed guard corpora x Track 1/2.
- Hot-leaf attribution is artifact-backed: `/tmp/skv12-pin-p1/time_profile_hot_leaf_summary.tsv`
  has 82 data rows and `/tmp/skv12-pin-p1/time_profile_hot_leaf_details.tsv` has
  410 data rows; both have 0 unresolved source anchors in the load-bearing
  symbol/source fields.
- CSS L4 absence is stated honestly as an unprofiled boundary, not a fallback
  authorization: see P1-C `p1c-samply-mode-3.md:96-110`, P1-D
  `p1d-pmu-cycles.md:65-75`, and P1-F `p1f-results-delta.md:22-33`.
- P1-A, P1-B, and P1-E still contain stale partial-capture blocker sections, but
  each has a top fold that explicitly supersedes those sections (`p1a:23-27`,
  `p1b:18-21`, `p1e:17-20`). On the next fold, delete those stale sections or
  move them under a clearly historical appendix to remove ambiguity.
