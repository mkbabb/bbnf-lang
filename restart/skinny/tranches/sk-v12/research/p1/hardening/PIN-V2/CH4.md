# SK-V12 S-P1 PIN-V2 CH4 - Cost / Replayability

Verdict: REVISE
Score: 91%

## Blocking Findings

1. The xctrace `rc=54` acceptance policy is false as written. The revised
   manifest says `rc=54` is accepted only when xctrace stderr records an
   accepted stop condition and "Output file saved as"
   (`restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:107-110`).
   The actual 185 `rc=54` rows in
   `/tmp/skv12-pin-p1/xctrace/capture_status.tsv` record those strings in the
   stdout logs, not stderr: stdout saved-output count is 185, stderr
   saved-output count is 0. The traces themselves exist, so this is a policy
   wording/replay-honesty defect, not an artifact-loss defect.

2. Several P1 files still label the capture commit as "current HEAD" even
   though this PIN-V2 review head is `d4ef80b217691c7e7a9603a2f9f178b3dc19d00b`.
   Examples: P1-A scope says current HEAD `cf7848b2`
   (`p1a-samply-mode-1.md:5`), P1-B says current HEAD / G-Alpha current HEAD
   (`p1b-samply-mode-2.md:5-8`), P1-D says current HEAD at scope and delta
   (`p1d-pmu-cycles.md:5`, `:215-217`), and P1-F says live current HEAD is
   `cf7848b227...` (`p1f-results-delta.md:5-6`). The manifest correctly names
   capture source `cf7848b2` and initial S-P1 fold `b1043383`
   (`skv12-p1-capture-manifest.md:10-13`), but the folded packet should
   distinguish capture source commit, initial fold commit, and current review
   head instead of overloading "current HEAD."

## Validated Evidence

- Exact target and stamps validate: capture root `/tmp/skv12-pin-p1`, build
  root `/tmp/skv12-pin-profile-target-cf7848b2`, both release binaries present,
  PMU done `2026-05-20T18:05:34Z`, samply done `2026-05-20T18:15:35Z`,
  xctrace done `2026-05-20T18:40:17Z`.
- The tracked pin replay ledger has 458 data rows:
  82 PMU, 82 samply, 82 xctrace primary Time Profiler, 82 xctrace CPU Counters,
  48 xctrace product-v2 Time Profiler, and 82 xctrace export rows.
  `/tmp/skv12-pin-p1/pmu/pmu-commands.sh` and
  `/tmp/skv12-pin-p1/samply/samply-commands.sh` each have 82 command rows.
- Capture status counts match the manifest: PMU 82/82 `PASS rc=0`, samply
  82/82 `PASS rc=0`, xctrace 212/212 `PASS` with 185 `rc=54` and 27 `rc=0`.
  All 212 xctrace trace directories exist.
- Export SKIP semantics are honest after PIN-V1: `/tmp/skv12-pin-p1/time_profile_export_status.tsv`
  has 82 `SKIP rc=0` rows, and all 82 export XML files are present and nonzero.
- Derived table checks pass: summary is 82 data rows split parse 34 / direct 34
  / typed 14; details is 410 data rows split parse 170 / direct 170 / typed 70.
  The readable tables contain 34 parse, 34 direct, and 14 typed data rows. The
  load-bearing source fields have zero `:0`, `unknown`, or `none` failures.
- CSS L4 is correctly missing as an S-P2/S-P3 prerequisite, not silently
  substituted by JSON, Sheets, or report fixtures. The pin requires generated
  CSS L4 to beat lightningcss (`USER-PIN-W1-CSS-L4-SOTA.md:18-35`), and P1-C /
  P1-E record no generated CSS L4 Track 1 runtime, no lightningcss same-plane
  comparator, and no strict equality oracle under the pin root.

## Nonblocking Notes

- PMU cost honesty is acceptable: P1-D does not infer branch misses, L1 misses,
  or LLC misses from absent TSV columns (`p1d-pmu-cycles.md:65-74`).
- Result movement is honestly separated from profile evidence: the manifest
  keeps `skinny/RESULTS.md` as result authority, while P1-F records that JSON
  rows do not populate the CSS L4 lightningcss close bar.
- Mode III absence is explicit and should remain a downstream prerequisite if a
  later route wants to cite masking-probe call stacks.

## Exact Fold Edits Required

1. In `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md`,
   replace the `rc=54` policy at lines 107-110 with: "`rc=54` is accepted only
   when the captured xctrace log stream records an accepted stop condition and
   `Output file saved as`; in the current pin root those strings are in the
   stdout path recorded by `capture_status.tsv`." Add the validation command
   that checks `$9` for the 185 stdout hits and does not require `$10`.
2. In P1-A, P1-B, P1-D, and P1-F, replace "current HEAD `cf7848b2`" language
   with "capture source commit `cf7848b2`"; where the document needs the
   reviewed doc head, name `d4ef80b217691c7e7a9603a2f9f178b3dc19d00b` separately.
3. In the manifest Pin Run Identity block, add the final folded review head:
   `d4ef80b2` (`docs(sk-v12-p1-hardening): fold pin replay challenge PIN-V1`),
   while preserving capture source `cf7848b2`, initial S-P1 fold `b1043383`,
   target dir `/tmp/skv12-pin-profile-target-cf7848b2`, capture root
   `/tmp/skv12-pin-p1`, binary paths, host/tool versions, and completion stamps.
4. Keep the CSS L4 absence wording as a hard prerequisite for S-P2/S-P3:
   generated CSS L4 Track 1 runtime, lightningcss same-plane comparator, and
   strict equality oracle must exist before any CSS L4 profile or >SOTA claim.
