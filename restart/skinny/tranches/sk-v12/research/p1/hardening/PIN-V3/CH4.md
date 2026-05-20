# SK-V12 S-P1 PIN-V3 CH4 - Cost / Replayability

Verdict: ACCEPT
Score: 96%

## Blockers

None.

## Validated Evidence

- Review base is the requested current head
  `9559a2c4b48076e1b98711d33110392cc3ee521f`
  (`docs(sk-v12-p1-hardening): fold pin replay challenge PIN-V2`). The run
  identity is now separated from review head: capture source `cf7848b2`,
  initial S-P1 fold `b1043383`, prior PIN-V2 review base `d4ef80b2`, capture
  root `/tmp/skv12-pin-p1`, and build root
  `/tmp/skv12-pin-profile-target-cf7848b2`.
- Completion stamps match the manifest and exist on disk:
  PMU `done 2026-05-20T18:05:34Z`, samply
  `done 2026-05-20T18:15:35Z`, and xctrace
  `done 2026-05-20T18:40:17Z`.
- The tracked pin replay ledger has the expected 10-column shape and 458 data
  rows with no empty required fields, no placeholder commands, existing cwd /
  artifact / status paths, and lane counts of 82 PMU, 82 samply, 82 xctrace
  primary Time Profiler, 82 xctrace CPU Counters, 48 xctrace product-v2 Time
  Profiler, and 82 xctrace export rows.
- The canonical-mode check returns zero bad rows:
  all replay modes are `track1`, `track2`, `real_typed_track1`, or
  `real_typed_track2`. The prior malformed `samply-parse` shape is repaired;
  `samply` rows are 34 parse, 34 direct, and 14 typed, with zero
  `update-center` corpus aliases.
- PMU and samply replay counts match authority: both command ledgers have 82
  rows; `/tmp/skv12-pin-p1/pmu/capture_status.tsv` is 82/82 `PASS rc=0`;
  `/tmp/skv12-pin-p1/samply/capture_status.tsv` is 82/82 `PASS rc=0`; all 82
  samply `.json.gz` artifacts are nonempty.
- xctrace counts match authority: `/tmp/skv12-pin-p1/xctrace/capture_status.tsv`
  has 212/212 `PASS`, split 82 primary Time Profiler, 82 CPU Counters, and 48
  product-v2 Time Profiler rows. Return codes are 27 `rc=0` and 185 `rc=54`.
- The `rc=54` stdout validation returns 185 good rows and zero bad rows. For
  every `rc=54` row, the stdout path in field 9 contains both `Output file
  saved as` and an accepted stop condition (`Reached specified time limit` or
  `Target app exited`), matching the revised manifest wording.
- Export SKIP semantics are honest: `/tmp/skv12-pin-p1/time_profile_export_status.tsv`
  has 82 data rows, all `SKIP rc=0`, and all 82 managed export XML paths are
  present and nonzero. These are reused exports, not relabeled PASS rows.
- Derived table checks pass: hot-leaf summary has 82 data rows split parse 34 /
  direct 34 / typed 14; details has 410 data rows split parse 170 / direct 170
  / typed 70; both checks report zero unresolved source anchors (`:0`,
  `unknown`, or `none` in load-bearing fields). The readable parse/direct/typed
  tables are derived from the same xctrace Time Profiler XML surface.
- CSS L4 absence is correctly preserved as an S-P2/S-P3 prerequisite. The pin
  root has zero CSS/lightningcss artifacts; P1-C/P1-E/P1-F and the manifest
  state that generated CSS L4 Track 1 runtime, same-plane lightningcss
  comparator, and strict equality oracle must exist before any CSS L4 profile
  or `>SOTA` claim.

## Nonblocking Notes

- There is one extra `/tmp/skv12-pin-p1/inspect/*.time-profile.xml` file outside
  the 82-row export-status ledger. It is not cited as replay authority and does
  not affect the managed export count.
- P1-C still says `documentation head cf7848b2`; read in context, that is the
  capture/boundary baseline, while this CH4 review base is the folded PIN-V2
  head `9559a2c4`. No replay command depends on that phrase.
- PMU branch-miss, L1, and LLC columns remain absent and are not inferred. That
  is cost-honest for this profile root.
- Mode III remains absent from the pin root. The absence is explicit and remains
  a downstream prerequisite for any masking-probe claim.

## Exact Fold Edits Required

None.
